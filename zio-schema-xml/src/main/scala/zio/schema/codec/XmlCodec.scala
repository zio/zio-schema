package zio.schema.codec

import java.time._
import java.util.{ Base64, UUID }

import scala.annotation.nowarn
import scala.util.control.NonFatal
import scala.xml._

import zio.schema.annotation._
import zio.schema.codec.DecodeError._
import zio.schema.{ DynamicValue, Fallback, Schema, StandardType, TypeId }
import zio.stream.ZPipeline
import zio.{ Cause, Chunk, Unsafe }

object XmlCodec {

  implicit def xmlCodec[A](implicit schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {

      override def encode(value: A): Chunk[Byte] =
        XmlEncoder.encode(schema, value)

      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] =
        if (whole.isEmpty) Left(EmptyContent("No bytes to decode"))
        else XmlDecoder.decode(schema, whole)

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
        ZPipeline.mapChunks[A, Byte] { chunk =>
          chunk.flatMap(XmlEncoder.encode(schema, _))
        }

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
        ZPipeline.mapChunksEither[Byte, A] { bytes =>
          XmlDecoder.decode(schema, bytes).map(Chunk.single)
        }
    }

  def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A] = xmlCodec[A]

  // ---------------------------------------------------------------------------
  // Encoder
  // ---------------------------------------------------------------------------
  private[codec] object XmlEncoder {

    def encode[A](schema: Schema[A], value: A): Chunk[Byte] = {
      val rootName = rootElementName(schema)
      val elem     = encodeToElem(schema, value, rootName)
      val xmlStr   = """<?xml version="1.0" encoding="UTF-8"?>""" + "\n" + elem.toString()
      Chunk.fromArray(xmlStr.getBytes("UTF-8"))
    }

    private def rootElementName[A](schema: Schema[A]): String = schema match {
      case r: Schema.Record[_] => typeIdName(r.id)
      case e: Schema.Enum[_]   => typeIdName(e.id)
      case Schema.Lazy(s)      => rootElementName(s())
      case _                   => "value"
    }

    private def typeIdName(id: TypeId): String = id match {
      case TypeId.Structural              => "value"
      case TypeId.Nominal(_, _, typeName) => typeName
    }

    private def encodeToElem[A](schema: Schema[A], value: A, name: String): Elem = {
      val children = encodeChildren(schema, value)
      children match {
        case Seq(text: Text) =>
          Elem(null, name, Null, TopScope, minimizeEmpty = false, text)
        case nodes if nodes.isEmpty =>
          Elem(null, name, Null, TopScope, minimizeEmpty = true)
        case nodes =>
          Elem(null, name, Null, TopScope, minimizeEmpty = false, nodes: _*)
      }
    }

    @nowarn
    private def encodeChildren[A](schema: Schema[A], value: A): Seq[Node] = (schema, value) match {

      case (Schema.Primitive(standardType, _), v) =>
        Seq(Text(encodePrimitive(standardType, v)))

      case (record: Schema.Record[A] @unchecked, v) =>
        encodeRecord(record, v)

      case (enum0: Schema.Enum[A] @unchecked, v) =>
        encodeEnum(enum0, v)

      case (Schema.Sequence(elementSchema, _, toChunk, _, _), v) =>
        toChunk(v).map(e => encodeToElem(elementSchema.asInstanceOf[Schema[Any]], e, "item")).toSeq

      case (Schema.NonEmptySequence(elementSchema, _, toChunk, _, _), v) =>
        toChunk(v).map(e => encodeToElem(elementSchema.asInstanceOf[Schema[Any]], e, "item")).toSeq

      case (Schema.Map(keySchema, valueSchema, _), v: scala.collection.immutable.Map[_, _] @unchecked) =>
        v.toSeq.map { case (k, vv) =>
          val keyElem   = encodeToElem(keySchema.asInstanceOf[Schema[Any]], k, "key")
          val valueElem = encodeToElem(valueSchema.asInstanceOf[Schema[Any]], vv, "value")
          Elem(null, "entry", Null, TopScope, minimizeEmpty = false, keyElem, valueElem)
        }

      case (Schema.NonEmptyMap(keySchema, valueSchema, _), v) =>
        val entries = v.asInstanceOf[zio.prelude.NonEmptyMap[Any, Any]].toList
        entries.map { case (k, vv) =>
          val keyElem   = encodeToElem(keySchema.asInstanceOf[Schema[Any]], k, "key")
          val valueElem = encodeToElem(valueSchema.asInstanceOf[Schema[Any]], vv, "value")
          Elem(null, "entry", Null, TopScope, minimizeEmpty = false, keyElem, valueElem)
        }

      case (Schema.Set(elementSchema, _), v: scala.collection.immutable.Set[_] @unchecked) =>
        v.toSeq.map(e => encodeToElem(elementSchema.asInstanceOf[Schema[Any]], e, "item"))

      case (Schema.Optional(innerSchema, _), v: Option[_] @unchecked) =>
        v match {
          case Some(inner) =>
            Seq(encodeToElem(innerSchema.asInstanceOf[Schema[Any]], inner, "some"))
          case None =>
            Seq(Elem(null, "none", Null, TopScope, minimizeEmpty = true))
        }

      case (Schema.Either(leftSchema, rightSchema, _), v: scala.util.Either[_, _] @unchecked) =>
        v match {
          case Left(l) =>
            Seq(encodeToElem(leftSchema.asInstanceOf[Schema[Any]], l, "left"))
          case Right(r) =>
            Seq(encodeToElem(rightSchema.asInstanceOf[Schema[Any]], r, "right"))
        }

      case (Schema.Fallback(leftSchema, rightSchema, _, _), v: Fallback[_, _] @unchecked) =>
        v match {
          case Fallback.Left(l) =>
            Seq(encodeToElem(leftSchema.asInstanceOf[Schema[Any]], l, "left"))
          case Fallback.Right(r) =>
            Seq(encodeToElem(rightSchema.asInstanceOf[Schema[Any]], r, "right"))
          case Fallback.Both(l, r) =>
            val leftElem  = encodeToElem(leftSchema.asInstanceOf[Schema[Any]], l, "left")
            val rightElem = encodeToElem(rightSchema.asInstanceOf[Schema[Any]], r, "right")
            Seq(Elem(null, "both", Null, TopScope, minimizeEmpty = false, leftElem, rightElem))
        }

      case (Schema.Tuple2(leftSchema, rightSchema, _), v: (_, _) @unchecked) =>
        Seq(
          encodeToElem(leftSchema.asInstanceOf[Schema[Any]], v._1, "_1"),
          encodeToElem(rightSchema.asInstanceOf[Schema[Any]], v._2, "_2")
        )

      case (Schema.Transform(innerSchema, _, g, _, _), v) =>
        g.asInstanceOf[Any => scala.util.Either[String, Any]](v) match {
          case Right(underlying) =>
            encodeChildren(innerSchema.asInstanceOf[Schema[Any]], underlying)
          case Left(error) =>
            throw new RuntimeException(s"Transform encode failed: $error")
        }

      case (Schema.Lazy(schema0), v) =>
        encodeChildren(schema0(), v)

      case (Schema.Dynamic(_), v: DynamicValue) =>
        encodeChildren(DynamicValue.schema.asInstanceOf[Schema[Any]], v)

      case (Schema.Fail(message, _), _) =>
        throw new RuntimeException(s"Cannot encode failed schema: $message")

      case _ =>
        throw new RuntimeException(s"Unsupported schema type for encoding: ${schema.getClass.getSimpleName}")
    }

    private def encodeRecord[R](record: Schema.Record[R], value: R): Seq[Node] =
      record.nonTransientFields.flatMap { field =>
        val fieldValue = field.asInstanceOf[Schema.Field[R, Any]].get(value)
        val fieldSchema = field.schema.asInstanceOf[Schema[Any]]
        val name = field.fieldName
        // For optional fields within records, omit the element entirely for None
        fieldSchema match {
          case Schema.Optional(_, _) =>
            fieldValue.asInstanceOf[Option[Any]] match {
              case Some(inner) =>
                val innerSchema = fieldSchema.asInstanceOf[Schema.Optional[Any]].schema
                Seq(encodeToElem(innerSchema, inner, name))
              case None =>
                Seq.empty
            }
          case _ =>
            Seq(encodeToElem(fieldSchema, fieldValue, name))
        }
      }.toSeq

    private def encodeEnum[Z](enum0: Schema.Enum[Z], value: Z): Seq[Node] = {
      val isSimple = enum0.annotations.exists(_.isInstanceOf[simpleEnum])
      enum0.nonTransientCases.find(_.isCase(value)) match {
        case Some(c) =>
          val caseName  = c.caseName
          val caseValue = c.asInstanceOf[Schema.Case[Z, Any]].deconstruct(value)
          if (isSimple) {
            Seq(Text(caseName))
          } else {
            val caseSchema = c.schema.asInstanceOf[Schema[Any]]
            // Check for @discriminatorName annotation
            enum0.discriminatorName match {
              case Some(discriminator) =>
                // Encode with discriminator element + inline fields
                val discriminatorElem =
                  Elem(null, discriminator, Null, TopScope, minimizeEmpty = false, Text(caseName))
                val innerNodes = encodeChildren(caseSchema, caseValue)
                discriminatorElem +: innerNodes
              case None if enum0.noDiscriminator =>
                // @noDiscriminator: encode case fields directly without wrapper
                encodeChildren(caseSchema, caseValue)
              case None =>
                // Default: wrap in case-name element
                val children = encodeChildren(caseSchema, caseValue)
                Seq(Elem(null, caseName, Null, TopScope, children.isEmpty, children: _*))
            }
          }
        case None =>
          throw new RuntimeException(s"No matching case found for enum value: $value")
      }
    }

    @nowarn
    private def encodePrimitive[A](standardType: StandardType[A], value: A): String = standardType match {
      case StandardType.UnitType             => ""
      case StandardType.StringType           => value.asInstanceOf[String]
      case StandardType.BoolType             => value.toString
      case StandardType.ByteType             => value.toString
      case StandardType.ShortType            => value.toString
      case StandardType.IntType              => value.toString
      case StandardType.LongType             => value.toString
      case StandardType.FloatType            => value.toString
      case StandardType.DoubleType           => value.toString
      case StandardType.BigIntegerType       => value.asInstanceOf[java.math.BigInteger].toString
      case StandardType.BigDecimalType       => value.asInstanceOf[java.math.BigDecimal].toString
      case StandardType.BinaryType           => Base64.getEncoder.encodeToString(value.asInstanceOf[Chunk[Byte]].toArray)
      case StandardType.CharType             => value.asInstanceOf[Char].toString
      case StandardType.UUIDType             => value.asInstanceOf[UUID].toString
      case StandardType.CurrencyType         => value.asInstanceOf[java.util.Currency].getCurrencyCode
      case StandardType.DayOfWeekType        => value.asInstanceOf[DayOfWeek].getValue.toString
      case StandardType.MonthType            => value.asInstanceOf[Month].getValue.toString
      case StandardType.MonthDayType         => value.asInstanceOf[MonthDay].toString
      case StandardType.PeriodType           => value.asInstanceOf[Period].toString
      case StandardType.YearType             => value.asInstanceOf[Year].getValue.toString
      case StandardType.YearMonthType        => value.asInstanceOf[YearMonth].toString
      case StandardType.ZoneIdType           => value.asInstanceOf[ZoneId].getId
      case StandardType.ZoneOffsetType       => value.asInstanceOf[ZoneOffset].toString
      case StandardType.DurationType         => value.asInstanceOf[Duration].toString
      case StandardType.InstantType          => value.asInstanceOf[Instant].toString
      case StandardType.LocalDateType        => value.asInstanceOf[LocalDate].toString
      case StandardType.LocalTimeType        => value.asInstanceOf[LocalTime].toString
      case StandardType.LocalDateTimeType    => value.asInstanceOf[LocalDateTime].toString
      case StandardType.OffsetTimeType       => value.asInstanceOf[OffsetTime].toString
      case StandardType.OffsetDateTimeType   => value.asInstanceOf[OffsetDateTime].toString
      case StandardType.ZonedDateTimeType    => value.asInstanceOf[ZonedDateTime].toString
      case _                                 => value.toString
    }
  }

  // ---------------------------------------------------------------------------
  // Decoder
  // ---------------------------------------------------------------------------
  private[codec] object XmlDecoder {

    def decode[A](schema: Schema[A], bytes: Chunk[Byte]): Either[DecodeError, A] =
      try {
        val xmlStr = new String(bytes.toArray, "UTF-8")
        // Strip XML declaration if present
        val cleaned = if (xmlStr.startsWith("<?xml")) {
          val idx = xmlStr.indexOf("?>")
          if (idx >= 0) xmlStr.substring(idx + 2).trim else xmlStr
        } else xmlStr

        val elem = XML.loadString(cleaned)
        Right(decodeFromElem(schema, elem, Chunk.empty))
      } catch {
        case e: DecodeError => Left(e)
        case NonFatal(e)    => Left(ReadError(Cause.fail(e), e.getMessage))
      }

    @nowarn
    private def decodeFromElem[A](schema: Schema[A], elem: Elem, path: Chunk[String]): A =
      (schema match {

        case Schema.Primitive(standardType, _) =>
          decodePrimitive(standardType, elem.text, path)

        case record: Schema.Record[A] @unchecked =>
          decodeRecord(record, elem, path)

        case enum0: Schema.Enum[A] @unchecked =>
          decodeEnum(enum0, elem, path)

        case Schema.Sequence(elementSchema, fromChunk, _, _, _) =>
          val items = childElems(elem, "item")
          val values = items.map(e => decodeFromElem(elementSchema.asInstanceOf[Schema[Any]], e, path :+ "item"))
          fromChunk.asInstanceOf[Chunk[Any] => Any](Chunk.fromIterable(values))

        case Schema.NonEmptySequence(elementSchema, fromChunkOption, _, _, _) =>
          val items = childElems(elem, "item")
          val values = items.map(e => decodeFromElem(elementSchema.asInstanceOf[Schema[Any]], e, path :+ "item"))
          fromChunkOption.asInstanceOf[Chunk[Any] => Option[Any]](Chunk.fromIterable(values)) match {
            case Some(v) => v
            case None    => throw MalformedFieldWithPath(path, "NonEmptySequence cannot be empty")
          }

        case Schema.Map(keySchema, valueSchema, _) =>
          val entries = childElems(elem, "entry")
          entries.map { entry =>
            val keyElem   = childElem(entry, "key", path :+ "entry")
            val valueElem = childElem(entry, "value", path :+ "entry")
            val k = decodeFromElem(keySchema.asInstanceOf[Schema[Any]], keyElem, path :+ "key")
            val v = decodeFromElem(valueSchema.asInstanceOf[Schema[Any]], valueElem, path :+ "value")
            (k, v)
          }.toMap

        case Schema.NonEmptyMap(keySchema, valueSchema, _) =>
          val entries = childElems(elem, "entry")
          val pairs = entries.map { entry =>
            val keyElem   = childElem(entry, "key", path :+ "entry")
            val valueElem = childElem(entry, "value", path :+ "entry")
            val k = decodeFromElem(keySchema.asInstanceOf[Schema[Any]], keyElem, path :+ "key")
            val v = decodeFromElem(valueSchema.asInstanceOf[Schema[Any]], valueElem, path :+ "value")
            (k, v)
          }
          val chunk = Chunk.fromIterable(pairs)
          zio.NonEmptyChunk.fromChunk(chunk) match {
            case Some(nec) => zio.prelude.NonEmptyMap.fromNonEmptyChunk(nec)
            case None      => throw MalformedFieldWithPath(path, "NonEmptyMap cannot be empty")
          }

        case Schema.Set(elementSchema, _) =>
          val items = childElems(elem, "item")
          items.map(e => decodeFromElem(elementSchema.asInstanceOf[Schema[Any]], e, path :+ "item")).toSet

        case Schema.Optional(innerSchema, _) =>
          val children = elemChildren(elem)
          if (children.exists(_.label == "none")) {
            None
          } else {
            children.find(_.label == "some") match {
              case Some(someElem) => Some(decodeFromElem(innerSchema.asInstanceOf[Schema[Any]], someElem, path :+ "some"))
              case None           => None
            }
          }

        case Schema.Either(leftSchema, rightSchema, _) =>
          val children = elemChildren(elem)
          children.find(_.label == "left") match {
            case Some(leftElem) =>
              Left(decodeFromElem(leftSchema.asInstanceOf[Schema[Any]], leftElem, path :+ "left"))
            case None =>
              children.find(_.label == "right") match {
                case Some(rightElem) =>
                  Right(decodeFromElem(rightSchema.asInstanceOf[Schema[Any]], rightElem, path :+ "right"))
                case None =>
                  throw MalformedFieldWithPath(path, "Expected <left> or <right> element in Either")
              }
          }

        case Schema.Fallback(leftSchema, rightSchema, _, _) =>
          val children = elemChildren(elem)
          children.find(_.label == "both") match {
            case Some(bothElem) =>
              val leftElem  = childElem(bothElem, "left", path :+ "both")
              val rightElem = childElem(bothElem, "right", path :+ "both")
              val l = decodeFromElem(leftSchema.asInstanceOf[Schema[Any]], leftElem, path :+ "left")
              val r = decodeFromElem(rightSchema.asInstanceOf[Schema[Any]], rightElem, path :+ "right")
              Fallback.Both(l, r)
            case None =>
              children.find(_.label == "left") match {
                case Some(leftElem) =>
                  Fallback.Left(decodeFromElem(leftSchema.asInstanceOf[Schema[Any]], leftElem, path :+ "left"))
                case None =>
                  children.find(_.label == "right") match {
                    case Some(rightElem) =>
                      Fallback.Right(
                        decodeFromElem(rightSchema.asInstanceOf[Schema[Any]], rightElem, path :+ "right")
                      )
                    case None =>
                      throw MalformedFieldWithPath(path, "Expected <left>, <right>, or <both> in Fallback")
                  }
              }
          }

        case Schema.Tuple2(leftSchema, rightSchema, _) =>
          val first  = childElem(elem, "_1", path)
          val second = childElem(elem, "_2", path)
          val l = decodeFromElem(leftSchema.asInstanceOf[Schema[Any]], first, path :+ "_1")
          val r = decodeFromElem(rightSchema.asInstanceOf[Schema[Any]], second, path :+ "_2")
          (l, r)

        case Schema.Transform(innerSchema, f, _, _, _) =>
          val inner = decodeFromElem(innerSchema.asInstanceOf[Schema[Any]], elem, path)
          f.asInstanceOf[Any => scala.util.Either[String, Any]](inner) match {
            case Right(v) => v
            case Left(err) =>
              throw MalformedFieldWithPath(path, s"Transform decode failed: $err")
          }

        case Schema.Lazy(schema0) =>
          decodeFromElem(schema0(), elem, path)

        case Schema.Dynamic(_) =>
          decodeFromElem(DynamicValue.schema, elem, path)

        case Schema.Fail(message, _) =>
          throw MalformedFieldWithPath(path, s"Cannot decode failed schema: $message")

        case _ =>
          throw MalformedFieldWithPath(path, s"Unsupported schema type: ${schema.getClass.getSimpleName}")
      }).asInstanceOf[A]

    private def decodeRecord[R](record: Schema.Record[R], elem: Elem, path: Chunk[String]): R = {
      // Reject extra fields if annotated with @rejectExtraFields
      if (record.rejectExtraFields) {
        val knownNames = record.fields.flatMap { field =>
          if (field.transient) Chunk.empty
          else Chunk.fromIterable(field.nameAndAliases)
        }.toSet
        val extraElems = elemChildren(elem).filterNot(e => knownNames.contains(e.label))
        if (extraElems.nonEmpty) {
          throw MalformedFieldWithPath(
            path,
            s"Unexpected field(s): ${extraElems.map(_.label).mkString(", ")}"
          )
        }
      }

      val fieldValues = record.fields.map { field =>
        val name    = field.fieldName
        val newPath = path :+ name

        if (field.transient) {
          field.defaultValue.getOrElse(
            field.schema.defaultValue match {
              case Right(v) => v
              case Left(_)  => throw MalformedFieldWithPath(newPath, s"Missing default for transient field '$name'")
            }
          )
        } else {
          val fieldSchema = field.schema.asInstanceOf[Schema[Any]]
          // Look up child element by fieldName first, then by aliases (@fieldNameAliases)
          val fieldElemOpt = findChildElem(elem, name).orElse {
            field.aliases.collectFirst {
              case alias if findChildElem(elem, alias).isDefined => findChildElem(elem, alias).get
            }
          }
          fieldElemOpt match {
            case Some(fieldElem) =>
              fieldSchema match {
                case Schema.Optional(innerSchema, _) =>
                  Some(decodeFromElem(innerSchema.asInstanceOf[Schema[Any]], fieldElem, newPath))
                case _ =>
                  decodeFromElem(fieldSchema, fieldElem, newPath)
              }
            case None =>
              fieldSchema match {
                case Schema.Optional(_, _) => None
                case _: Schema.Collection[_, _] =>
                  emptyCollection(fieldSchema).getOrElse(
                    throw MalformedFieldWithPath(newPath, s"Missing required field '$name'")
                  )
                case _ =>
                  field.defaultValue.getOrElse(
                    if (field.optional) {
                      field.schema.defaultValue match {
                        case Right(v) => v
                        case Left(_)  => throw MalformedFieldWithPath(newPath, s"Missing required field '$name'")
                      }
                    } else {
                      throw MalformedFieldWithPath(newPath, s"Missing required field '$name'")
                    }
                  )
              }
          }
        }
      }
      Unsafe.unsafe { implicit u =>
        record.construct(fieldValues) match {
          case Right(v)  => v
          case Left(err) => throw MalformedFieldWithPath(path, s"Failed to construct record: $err")
        }
      }
    }

    private def decodeEnum[Z](enum0: Schema.Enum[Z], elem: Elem, path: Chunk[String]): Z = {
      val isSimple = enum0.annotations.exists(_.isInstanceOf[simpleEnum])

      if (isSimple) {
        val text = elem.text.trim
        enum0.nonTransientCases.find(_.caseName == text) match {
          case Some(c) =>
            val caseSchema = c.schema.asInstanceOf[Schema[Any]]
            caseSchema.defaultValue match {
              case Right(v) => c.asInstanceOf[Schema.Case[Z, Any]].construct(v)
              case Left(err) =>
                throw MalformedFieldWithPath(path, s"Cannot construct simple enum case '$text': $err")
            }
          case None =>
            throw MalformedFieldWithPath(path, s"Unknown enum case: $text")
        }
      } else {
        // Check for @discriminatorName annotation
        enum0.discriminatorName match {
          case Some(discriminator) =>
            decodeEnumWithDiscriminator(enum0, elem, path, discriminator)
          case None if enum0.noDiscriminator =>
            decodeEnumNoDiscriminator(enum0, elem, path)
          case None =>
            decodeEnumWrapped(enum0, elem, path)
        }
      }
    }

    /** Default enum decoding: case wrapped in a named element */
    private def decodeEnumWrapped[Z](enum0: Schema.Enum[Z], elem: Elem, path: Chunk[String]): Z = {
      val children = elemChildren(elem)
      if (children.isEmpty) {
        throw MalformedFieldWithPath(path, "Expected child element for enum case")
      }
      val caseElem  = children.head
      val caseLabel = caseElem.label

      findEnumCase(enum0, caseLabel) match {
        case Some(c) =>
          val caseSchema = c.schema.asInstanceOf[Schema[Any]]
          val decoded    = decodeFromElem(caseSchema, caseElem, path :+ caseLabel)
          c.asInstanceOf[Schema.Case[Z, Any]].construct(decoded)
        case None =>
          throw MalformedFieldWithPath(path, s"Unknown enum case: $caseLabel")
      }
    }

    /** Decode enum with @discriminatorName: discriminator element + inline fields */
    private def decodeEnumWithDiscriminator[Z](
      enum0: Schema.Enum[Z],
      elem: Elem,
      path: Chunk[String],
      discriminator: String
    ): Z = {
      val discElem = findChildElem(elem, discriminator).getOrElse(
        throw MalformedFieldWithPath(path, s"Missing discriminator element <$discriminator>")
      )
      val caseLabel = discElem.text.trim

      findEnumCase(enum0, caseLabel) match {
        case Some(c) =>
          val caseSchema = c.schema.asInstanceOf[Schema[Any]]
          // Decode from the parent elem directly (fields are siblings of the discriminator)
          val decoded = decodeFromElem(caseSchema, elem, path :+ caseLabel)
          c.asInstanceOf[Schema.Case[Z, Any]].construct(decoded)
        case None =>
          throw MalformedFieldWithPath(path, s"Unknown enum case: $caseLabel")
      }
    }

    /** Decode enum with @noDiscriminator: try each case until one succeeds */
    private def decodeEnumNoDiscriminator[Z](
      enum0: Schema.Enum[Z],
      elem: Elem,
      path: Chunk[String]
    ): Z = {
      val errors = scala.collection.mutable.ListBuffer.empty[String]
      val it     = enum0.nonTransientCases.iterator
      while (it.hasNext) {
        val c = it.next()
        try {
          val caseSchema = c.schema.asInstanceOf[Schema[Any]]
          val decoded    = decodeFromElem(caseSchema, elem, path)
          return c.asInstanceOf[Schema.Case[Z, Any]].construct(decoded)
        } catch {
          case _: DecodeError =>
            errors += c.caseName
          case NonFatal(_) =>
            errors += c.caseName
        }
      }
      throw MalformedFieldWithPath(
        path,
        s"No matching enum case found. Tried: ${errors.mkString(", ")}"
      )
    }

    /** Find an enum case by caseName, caseNameAliases, or id */
    private def findEnumCase[Z](enum0: Schema.Enum[Z], label: String): Option[Schema.Case[Z, _]] =
      enum0.nonTransientCases
        .find(_.caseName == label)
        .orElse(
          enum0.nonTransientCases.find(c => c.caseNameAliases.contains(label) || c.id == label)
        )

    @nowarn
    private def decodePrimitive[A](standardType: StandardType[A], text: String, path: Chunk[String]): A =
      (try {
        standardType match {
          case StandardType.UnitType             => ()
          case StandardType.StringType           => text
          case StandardType.BoolType             => text.trim.toBoolean
          case StandardType.ByteType             => text.trim.toByte
          case StandardType.ShortType            => text.trim.toShort
          case StandardType.IntType              => text.trim.toInt
          case StandardType.LongType             => text.trim.toLong
          case StandardType.FloatType            => text.trim.toFloat
          case StandardType.DoubleType           => text.trim.toDouble
          case StandardType.BigIntegerType       => new java.math.BigInteger(text.trim)
          case StandardType.BigDecimalType       => new java.math.BigDecimal(text.trim)
          case StandardType.BinaryType           => Chunk.fromArray(Base64.getDecoder.decode(text.trim))
          case StandardType.CharType =>
            val t = text
            if (t.length == 1) t.charAt(0)
            else throw MalformedFieldWithPath(path, s"Expected single character, got: '$t'")
          case StandardType.UUIDType             => UUID.fromString(text.trim)
          case StandardType.CurrencyType         => java.util.Currency.getInstance(text.trim)
          case StandardType.DayOfWeekType        => DayOfWeek.of(text.trim.toInt)
          case StandardType.MonthType            => Month.of(text.trim.toInt)
          case StandardType.MonthDayType         => MonthDay.parse(text.trim)
          case StandardType.PeriodType           => Period.parse(text.trim)
          case StandardType.YearType             => Year.of(text.trim.toInt)
          case StandardType.YearMonthType        => YearMonth.parse(text.trim)
          case StandardType.ZoneIdType           => ZoneId.of(text.trim)
          case StandardType.ZoneOffsetType       => ZoneOffset.of(text.trim)
          case StandardType.DurationType         => Duration.parse(text.trim)
          case StandardType.InstantType          => Instant.parse(text.trim)
          case StandardType.LocalDateType        => LocalDate.parse(text.trim)
          case StandardType.LocalTimeType        => LocalTime.parse(text.trim)
          case StandardType.LocalDateTimeType    => LocalDateTime.parse(text.trim)
          case StandardType.OffsetTimeType       => OffsetTime.parse(text.trim)
          case StandardType.OffsetDateTimeType   => OffsetDateTime.parse(text.trim)
          case StandardType.ZonedDateTimeType    => ZonedDateTime.parse(text.trim)
          case _                                 => throw MalformedFieldWithPath(path, s"Unsupported primitive: $standardType")
        }
      } catch {
        case e: DecodeError => throw e
        case NonFatal(e) =>
          throw MalformedFieldWithPath(path, s"Failed to decode ${standardType}: ${e.getMessage}")
      }).asInstanceOf[A]

    // XML utility helpers

    private def elemChildren(elem: Elem): Seq[Elem] =
      elem.child.collect { case e: Elem => e }.toSeq

    private def childElems(elem: Elem, name: String): Seq[Elem] =
      elem.child.collect { case e: Elem if e.label == name => e }.toSeq

    private def findChildElem(elem: Elem, name: String): Option[Elem] =
      elem.child.collectFirst { case e: Elem if e.label == name => e }

    private def childElem(elem: Elem, name: String, path: Chunk[String]): Elem =
      findChildElem(elem, name).getOrElse(
        throw MalformedFieldWithPath(path, s"Missing required element <$name>")
      )

    private def emptyCollection[A](schema: Schema[A]): Option[A] = schema match {
      case s: Schema.Sequence[_, _, _] => Some(s.fromChunk(Chunk.empty).asInstanceOf[A])
      case _: Schema.Map[_, _]         => Some(scala.collection.immutable.Map.empty.asInstanceOf[A])
      case _: Schema.Set[_]            => Some(scala.collection.immutable.Set.empty.asInstanceOf[A])
      case Schema.Lazy(s)              => emptyCollection(s())
      case _                           => None
    }
  }
}
