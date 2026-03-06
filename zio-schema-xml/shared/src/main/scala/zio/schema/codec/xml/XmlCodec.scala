package zio.schema.codec.xml

import java.nio.charset.StandardCharsets
import java.time._
import java.util.{ Base64, UUID }

import scala.collection.immutable.ListMap
import scala.util.control.NonFatal

import zio.schema._
import zio.schema.annotation._
import zio.schema.codec._
import zio.stream.ZPipeline
import zio.{ Cause, Chunk }

object XmlCodec {

  final case class Configuration(
    writerConfig: WriterConfig = WriterConfig.default,
    readerConfig: ReaderConfig = ReaderConfig.default
  )

  object Configuration {
    val default: Configuration = Configuration()
  }

  implicit def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A] =
    schemaBasedBinaryCodec(Configuration.default)

  def schemaBasedBinaryCodec[A](config: Configuration)(implicit schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {

      override def encode(value: A): Chunk[Byte] = {
        val xml = Encoder.encode(schema, value)
        val str = XmlWriter.write(xml, config.writerConfig)
        Chunk.fromArray(str.getBytes(StandardCharsets.UTF_8))
      }

      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] = {
        val str = new String(whole.toArray, StandardCharsets.UTF_8)
        XmlReader.read(str, config.readerConfig) match {
          case Left(err) =>
            Left(DecodeError.ReadError(Cause.empty, err.toString))
          case Right(xml) =>
            Decoder.decode(schema, xml)
        }
      }

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
        ZPipeline.mapChunks(_.flatMap(encode))

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
        ZPipeline.mapChunksEither(bytes => decode(bytes).map(Chunk.single))
    }

  private object Encoder {

    def encode[A](schema: Schema[A], value: A): Xml =
      encodeSchema(schema, value, None)

    private def encodeSchema[A](schema: Schema[A], value: A, fieldName: Option[String]): Xml =
      schema match {
        case Schema.Primitive(standardType, _) =>
          val text = encodePrimitive(standardType, value)
          fieldName match {
            case Some(name) => wrapElement(name, Xml.Text(text))
            case None       => Xml.Text(text)
          }

        case record: Schema.Record[A] =>
          encodeRecord(record, value, fieldName)

        case enumSchema: Schema.Enum[A] =>
          encodeEnum(enumSchema, value, fieldName)

        case Schema.Sequence(elemSchema, _, toChunk, _, _) =>
          encodeSequence(elemSchema, toChunk(value), fieldName)

        case Schema.NonEmptySequence(elemSchema, _, toChunk, _, _) =>
          encodeSequence(elemSchema, toChunk(value), fieldName)

        case Schema.Set(elemSchema, _) =>
          encodeSequence(elemSchema, Chunk.fromIterable(value), fieldName)

        case Schema.Map(keySchema, valueSchema, _) =>
          encodeMap(keySchema, valueSchema, Chunk.fromIterable(value), fieldName)

        case Schema.NonEmptyMap(keySchema, valueSchema, _) =>
          encodeMap(keySchema, valueSchema, Chunk.fromIterable(value.toList), fieldName)

        case Schema.Optional(innerSchema, _) =>
          value.asInstanceOf[Option[Any]] match {
            case Some(v) => encodeSchema(innerSchema.asInstanceOf[Schema[Any]], v, fieldName)
            case None    => Xml.Element(XmlName(fieldName.getOrElse("none")), Chunk.empty, Chunk.empty)
          }

        case Schema.Either(leftSchema, rightSchema, _) =>
          val either = value.asInstanceOf[scala.util.Either[Any, Any]]
          val wrapped = either match {
            case scala.util.Left(l) => wrapElement("Left", encodeSchema(leftSchema.asInstanceOf[Schema[Any]], l, None))
            case scala.util.Right(r) =>
              wrapElement("Right", encodeSchema(rightSchema.asInstanceOf[Schema[Any]], r, None))
          }
          fieldName.fold(wrapped)(n => wrapElement(n, wrapped))

        case Schema.Tuple2(leftSchema, rightSchema, _) =>
          val (a, b) = value.asInstanceOf[(Any, Any)]
          val children = Chunk(
            wrapElement("_1", encodeSchema(leftSchema.asInstanceOf[Schema[Any]], a, None)),
            wrapElement("_2", encodeSchema(rightSchema.asInstanceOf[Schema[Any]], b, None))
          )
          Xml.Element(XmlName(fieldName.getOrElse("tuple")), Chunk.empty, children)

        case Schema.Fallback(leftSchema, rightSchema, _, _) =>
          val fb = value.asInstanceOf[Fallback[Any, Any]]
          val inner = fb match {
            case Fallback.Left(l) =>
              wrapElement("Left", encodeSchema(leftSchema.asInstanceOf[Schema[Any]], l, None))
            case Fallback.Right(r) =>
              wrapElement("Right", encodeSchema(rightSchema.asInstanceOf[Schema[Any]], r, None))
            case Fallback.Both(l, r) =>
              val children = Chunk[Xml](
                wrapElement("Left", encodeSchema(leftSchema.asInstanceOf[Schema[Any]], l, None)),
                wrapElement("Right", encodeSchema(rightSchema.asInstanceOf[Schema[Any]], r, None))
              )
              Xml.Element(XmlName("Both"), Chunk.empty, children)
          }
          fieldName.fold(inner)(n => wrapElement(n, inner))

        case Schema.Transform(innerSchema, _, g, _, _) =>
          g(value) match {
            case scala.util.Right(a) => encodeSchema(innerSchema.asInstanceOf[Schema[Any]], a, fieldName)
            case scala.util.Left(_)  => Xml.Text("")
          }

        case lzy @ Schema.Lazy(_) =>
          encodeSchema(lzy.schema, value, fieldName)

        case Schema.Dynamic(_) =>
          encodeDynamic(value.asInstanceOf[DynamicValue], fieldName)

        case Schema.Fail(_, _) =>
          Xml.Text("")
      }

    private def encodeRecord[A](record: Schema.Record[A], value: A, fieldName: Option[String]): Xml = {
      val recordNs = record.annotations.collectFirst { case ns: xmlNamespace => ns }
      val baseName = fieldName.getOrElse(recordName(record))
      val elementName = recordNs match {
        case Some(ns) => XmlName(baseName, ns.prefix, Some(ns.uri))
        case None     => XmlName(baseName)
      }
      var attributes = Chunk.empty[(XmlName, String)]
      val children = record.nonTransientFields.flatMap { field =>
        val fName       = field.fieldName
        val fValue      = field.get(value)
        val isAttribute = field.annotations.collectFirst { case _: xmlAttribute => () }.isDefined
        val fieldNs     = field.annotations.collectFirst { case ns: xmlNamespace => ns }
        if (isAttribute) {
          val attrName = fieldNs match {
            case Some(ns) => XmlName(fName, ns.prefix, Some(ns.uri))
            case None     => XmlName(fName)
          }
          encodePrimitiveField(field.schema.asInstanceOf[Schema[Any]], fValue) match {
            case Some(text) => attributes = attributes :+ ((attrName, text))
            case None       => ()
          }
          Chunk.empty
        } else {
          field.schema match {
            case Schema.Optional(inner, _) =>
              fValue.asInstanceOf[Option[Any]] match {
                case Some(v) =>
                  Chunk.single(encodeSchema(inner.asInstanceOf[Schema[Any]], v, Some(fName)))
                case None =>
                  Chunk.empty
              }
            case _ =>
              Chunk.single(encodeSchema(field.schema.asInstanceOf[Schema[Any]], fValue, Some(fName)))
          }
        }
      }
      Xml.Element(elementName, attributes, children)
    }

    private def encodeEnum[A](enumSchema: Schema.Enum[A], value: A, fieldName: Option[String]): Xml = {
      val theCase = enumSchema.caseOf(value)
      theCase match {
        case Some(c) =>
          val caseName0          = c.caseName
          val innerValue         = c.deconstruct(value)
          val isSimple           = enumSchema.annotations.exists(_.isInstanceOf[simpleEnum])
          val hasDiscriminator   = enumSchema.discriminatorName.isDefined
          val hasNoDiscriminator = enumSchema.noDiscriminator

          if (isSimple) {
            val text = Xml.Text(caseName0)
            fieldName.fold[Xml](text)(n => wrapElement(n, text))
          } else if (hasNoDiscriminator) {
            val inner = encodeSchema(c.schema.asInstanceOf[Schema[Any]], innerValue, Some(caseName0))
            fieldName.fold(inner)(n => wrapElement(n, inner))
          } else if (hasDiscriminator) {
            val discName = enumSchema.discriminatorName.get
            val inner    = encodeSchema(c.schema.asInstanceOf[Schema[Any]], innerValue, None)
            val enriched = inner match {
              case Xml.Element(eName, eAttrs, eChildren) =>
                val discChild = wrapElement(discName, Xml.Text(caseName0))
                Xml.Element(eName, eAttrs, discChild +: eChildren)
              case other =>
                val children = Chunk[Xml](wrapElement(discName, Xml.Text(caseName0)), other)
                Xml.Element(XmlName(caseName0), Chunk.empty, children)
            }
            fieldName.fold(enriched)(n => wrapElement(n, enriched))
          } else {
            val inner       = encodeSchema(c.schema.asInstanceOf[Schema[Any]], innerValue, Some(caseName0))
            val wrapperName = fieldName.getOrElse(enumName(enumSchema))
            wrapElement(wrapperName, inner)
          }
        case None =>
          Xml.Text("")
      }
    }

    private def encodeSequence[A](elemSchema: Schema[A], values: Chunk[A], fieldName: Option[String]): Xml = {
      val itemName = fieldName.getOrElse("item")
      val children = values.map(v => encodeSchema(elemSchema, v, Some("item")))
      Xml.Element(XmlName(itemName), Chunk.empty, children)
    }

    private def encodeMap[K, V](
      keySchema: Schema[K],
      valueSchema: Schema[V],
      entries: Chunk[(K, V)],
      fieldName: Option[String]
    ): Xml = {
      val children = entries.map {
        case (k, v) =>
          val keyXml   = encodeSchema(keySchema, k, Some("key"))
          val valueXml = encodeSchema(valueSchema, v, Some("value"))
          Xml.Element(XmlName("entry"), Chunk.empty, Chunk(keyXml, valueXml))
      }
      Xml.Element(XmlName(fieldName.getOrElse("map")), Chunk.empty, children)
    }

    private def encodeDynamic(dv: DynamicValue, fieldName: Option[String]): Xml = {
      val xml = dv match {
        case DynamicValue.Primitive(v, st) =>
          val text = encodePrimitive(st.asInstanceOf[StandardType[Any]], v)
          Xml.Text(text)
        case DynamicValue.Record(_, values) =>
          val children = values.toList.map {
            case (k, v) =>
              wrapElement(k, encodeDynamic(v, None))
          }
          Xml.Element(XmlName("record"), Chunk.empty, Chunk.fromIterable(children))
        case DynamicValue.Enumeration(_, (caseName0, v)) =>
          wrapElement(caseName0, encodeDynamic(v, None))
        case DynamicValue.Sequence(values) =>
          val children = values.map(v => wrapElement("item", encodeDynamic(v, None)))
          Xml.Element(XmlName("sequence"), Chunk.empty, children)
        case DynamicValue.Dictionary(entries) =>
          val children = entries.map {
            case (k, v) =>
              Xml.Element(
                XmlName("entry"),
                Chunk.empty,
                Chunk(wrapElement("key", encodeDynamic(k, None)), wrapElement("value", encodeDynamic(v, None)))
              )
          }
          Xml.Element(XmlName("dictionary"), Chunk.empty, children)
        case DynamicValue.SetValue(values) =>
          val children = Chunk.fromIterable(values).map(v => wrapElement("item", encodeDynamic(v, None)))
          Xml.Element(XmlName("set"), Chunk.empty, children)
        case DynamicValue.SomeValue(v) =>
          encodeDynamic(v, None)
        case DynamicValue.NoneValue =>
          Xml.Text("")
        case DynamicValue.Tuple(l, r) =>
          Xml.Element(
            XmlName("tuple"),
            Chunk.empty,
            Chunk(wrapElement("_1", encodeDynamic(l, None)), wrapElement("_2", encodeDynamic(r, None)))
          )
        case DynamicValue.LeftValue(v) =>
          wrapElement("Left", encodeDynamic(v, None))
        case DynamicValue.RightValue(v) =>
          wrapElement("Right", encodeDynamic(v, None))
        case DynamicValue.BothValue(l, r) =>
          Xml.Element(
            XmlName("Both"),
            Chunk.empty,
            Chunk(wrapElement("Left", encodeDynamic(l, None)), wrapElement("Right", encodeDynamic(r, None)))
          )
        case DynamicValue.Singleton(_) =>
          Xml.Text("")
        case DynamicValue.DynamicAst(_) =>
          Xml.Text("")
        case DynamicValue.Error(msg) =>
          Xml.Text(msg)
      }
      fieldName.fold(xml)(n => wrapElement(n, xml))
    }

    private def encodePrimitive[A](st: StandardType[A], value: A): String =
      st match {
        case StandardType.UnitType           => ""
        case StandardType.StringType         => value.asInstanceOf[String]
        case StandardType.BoolType           => value.toString
        case StandardType.ByteType           => value.toString
        case StandardType.ShortType          => value.toString
        case StandardType.IntType            => value.toString
        case StandardType.LongType           => value.toString
        case StandardType.FloatType          => value.toString
        case StandardType.DoubleType         => value.toString
        case StandardType.BinaryType         => Base64.getEncoder.encodeToString(value.asInstanceOf[Chunk[Byte]].toArray)
        case StandardType.CharType           => value.toString
        case StandardType.BigIntegerType     => value.toString
        case StandardType.BigDecimalType     => value.toString
        case StandardType.UUIDType           => value.toString
        case StandardType.DayOfWeekType      => value.asInstanceOf[DayOfWeek].name
        case StandardType.DurationType       => value.toString
        case StandardType.InstantType        => value.toString
        case StandardType.LocalDateType      => value.toString
        case StandardType.LocalDateTimeType  => value.toString
        case StandardType.LocalTimeType      => value.toString
        case StandardType.MonthType          => value.asInstanceOf[Month].name
        case StandardType.MonthDayType       => value.toString
        case StandardType.OffsetDateTimeType => value.toString
        case StandardType.OffsetTimeType     => value.toString
        case StandardType.PeriodType         => value.toString
        case StandardType.YearType           => value.asInstanceOf[Year].getValue.toString
        case StandardType.YearMonthType      => value.toString
        case StandardType.ZonedDateTimeType  => value.toString
        case StandardType.ZoneIdType         => value.asInstanceOf[ZoneId].getId
        case StandardType.ZoneOffsetType     => value.toString
        case StandardType.CurrencyType       => value.asInstanceOf[java.util.Currency].getCurrencyCode
      }

    private def wrapElement(name: String, child: Xml): Xml.Element =
      Xml.Element(XmlName(name), Chunk.empty, Chunk(child))

    private def recordName[A](record: Schema.Record[A]): String =
      record.id.name

    private def enumName[A](enumSchema: Schema.Enum[A]): String =
      enumSchema.id.name

    private def encodePrimitiveField[A](schema: Schema[A], value: A): Option[String] =
      schema match {
        case Schema.Primitive(standardType, _) =>
          Some(encodePrimitive(standardType, value))
        case Schema.Optional(inner, _) =>
          value.asInstanceOf[Option[Any]] match {
            case Some(v) => encodePrimitiveField(inner.asInstanceOf[Schema[Any]], v)
            case None    => None
          }
        case Schema.Lazy(inner0) =>
          encodePrimitiveField(inner0(), value)
        case Schema.Transform(innerSchema, _, g, _, _) =>
          g(value) match {
            case scala.util.Right(a) => encodePrimitiveField(innerSchema.asInstanceOf[Schema[Any]], a)
            case scala.util.Left(_)  => None
          }
        case _ => None
      }

  }

  private object Decoder {

    def decode[A](schema: Schema[A], xml: Xml): Either[DecodeError, A] =
      decodeSchema(schema, xml)

    private def decodeSchema[A](schema: Schema[A], xml: Xml): Either[DecodeError, A] =
      schema match {
        case Schema.Primitive(standardType, _) =>
          val text = extractText(xml)
          decodePrimitive(standardType, text).asInstanceOf[Either[DecodeError, A]]

        case record: Schema.Record[A] =>
          decodeRecord(record, xml)

        case enumSchema: Schema.Enum[A] =>
          decodeEnum(enumSchema, xml)

        case Schema.Sequence(elemSchema, fromChunk, _, _, _) =>
          decodeSequence(elemSchema, xml).map(fromChunk)

        case Schema.NonEmptySequence(elemSchema, fromChunkOption, _, _, _) =>
          decodeSequence(elemSchema, xml).flatMap { chunk =>
            fromChunkOption(chunk) match {
              case Some(col) => Right(col)
              case None      => Left(DecodeError.EmptyContent("NonEmptySequence cannot be empty"))
            }
          }

        case Schema.Set(elemSchema, _) =>
          decodeSequence(elemSchema, xml).map(_.toSet[Any]).asInstanceOf[Either[DecodeError, A]]

        case Schema.Map(keySchema, valueSchema, _) =>
          decodeMap(keySchema, valueSchema, xml).map(_.toMap).asInstanceOf[Either[DecodeError, A]]

        case Schema.NonEmptyMap(keySchema, valueSchema, _) =>
          decodeMap(keySchema, valueSchema, xml).flatMap { pairs =>
            val map = pairs.toMap
            if (map.isEmpty) Left(DecodeError.EmptyContent("NonEmptyMap cannot be empty"))
            else {
              try Right(zio.prelude.NonEmptyMap.fromMapOption(map).get.asInstanceOf[A])
              catch {
                case NonFatal(e) => Left(DecodeError.ReadError(Cause.fail(e), e.getMessage))
              }
            }
          }

        case Schema.Optional(innerSchema, _) =>
          xml match {
            case Xml.Element(_, _, children) if children.isEmpty =>
              Right(None.asInstanceOf[A])
            case Xml.Text(v) if v.isEmpty =>
              Right(None.asInstanceOf[A])
            case _ =>
              decodeSchema(innerSchema, xml).map(v => Some(v).asInstanceOf[A])
          }

        case Schema.Either(leftSchema, rightSchema, _) =>
          decodeEither(leftSchema, rightSchema, xml).asInstanceOf[Either[DecodeError, A]]

        case Schema.Tuple2(leftSchema, rightSchema, _) =>
          decodeTuple(leftSchema, rightSchema, xml).asInstanceOf[Either[DecodeError, A]]

        case Schema.Fallback(leftSchema, rightSchema, fullDecode, _) =>
          decodeFallback(leftSchema, rightSchema, fullDecode, xml).asInstanceOf[Either[DecodeError, A]]

        case Schema.Transform(innerSchema, f, _, _, _) =>
          decodeSchema(innerSchema, xml).flatMap { a =>
            f(a) match {
              case scala.util.Right(b) => Right(b.asInstanceOf[A])
              case scala.util.Left(err) =>
                Left(DecodeError.ReadError(Cause.empty, s"Transform failed: $err"))
            }
          }

        case lzy @ Schema.Lazy(_) =>
          decodeSchema(lzy.schema, xml)

        case Schema.Dynamic(_) =>
          decodeDynamic(xml).asInstanceOf[Either[DecodeError, A]]

        case Schema.Fail(message, _) =>
          Left(DecodeError.ReadError(Cause.empty, s"Failed schema: $message"))
      }

    private def decodeRecord[A](record: Schema.Record[A], xml: Xml): Either[DecodeError, A] = {
      val element = xml match {
        case e: Xml.Element => e
        case _              => return Left(DecodeError.ReadError(Cause.empty, "Expected XML element for record"))
      }
      val childrenByName = groupChildrenByName(element.children)
      val attributeMap   = groupAttributesByName(element.attributes)
      val fieldValues    = new Array[Any](record.fields.size)
      var i              = 0
      val it             = record.fields.iterator
      while (it.hasNext) {
        val field = it.next()
        if (field.transient) {
          field.defaultValue match {
            case Some(dv) => fieldValues(i) = dv
            case None =>
              field.schema.defaultValue match {
                case scala.util.Right(dv) => fieldValues(i) = dv
                case scala.util.Left(err) =>
                  return Left(
                    DecodeError.MissingField(field.schema, s"Transient field '${field.name}' has no default: $err")
                  )
              }
          }
        } else {
          val fName       = field.fieldName
          val isAttribute = field.annotations.collectFirst { case _: xmlAttribute => () }.isDefined
          if (isAttribute) {
            attributeMap.get(fName) match {
              case Some(attrValue) =>
                decodePrimitiveField(field.schema.asInstanceOf[Schema[Any]], attrValue) match {
                  case Right(v)  => fieldValues(i) = v
                  case Left(err) => return Left(err)
                }
              case None =>
                if (field.optional) {
                  field.defaultValue match {
                    case Some(dv) => fieldValues(i) = dv
                    case None =>
                      field.schema match {
                        case _: Schema.Optional[_] => fieldValues(i) = None
                        case _ =>
                          field.schema.defaultValue match {
                            case scala.util.Right(dv) => fieldValues(i) = dv
                            case scala.util.Left(_) =>
                              return Left(DecodeError.MissingField(field.schema, s"Missing attribute: '$fName'"))
                          }
                      }
                  }
                } else {
                  field.schema match {
                    case _: Schema.Optional[_] => fieldValues(i) = None
                    case _ =>
                      return Left(DecodeError.MissingField(field.schema, s"Missing required attribute: '$fName'"))
                  }
                }
            }
          } else {
            childrenByName.get(fName) match {
              case Some(childXml) =>
                field.schema match {
                  case Schema.Optional(inner, _) =>
                    decodeSchema(inner, childXml) match {
                      case Right(v)  => fieldValues(i) = Some(v)
                      case Left(err) => return Left(err)
                    }
                  case _ =>
                    decodeSchema(field.schema, childXml) match {
                      case Right(v)  => fieldValues(i) = v
                      case Left(err) => return Left(err)
                    }
                }
              case None =>
                if (field.optional) {
                  field.defaultValue match {
                    case Some(dv) => fieldValues(i) = dv
                    case None =>
                      field.schema match {
                        case _: Schema.Optional[_] => fieldValues(i) = None
                        case _ =>
                          field.schema.defaultValue match {
                            case scala.util.Right(dv) => fieldValues(i) = dv
                            case scala.util.Left(_) =>
                              return Left(DecodeError.MissingField(field.schema, s"Missing field: '$fName'"))
                          }
                      }
                  }
                } else {
                  field.schema match {
                    case _: Schema.Optional[_] => fieldValues(i) = None
                    case _ =>
                      return Left(DecodeError.MissingField(field.schema, s"Missing required field: '$fName'"))
                  }
                }
            }
          }
        }
        i += 1
      }
      zio.Unsafe.unsafe { implicit unsafe =>
        record.construct(Chunk.fromArray(fieldValues)) match {
          case scala.util.Right(v)  => Right(v)
          case scala.util.Left(err) => Left(DecodeError.ReadError(Cause.empty, s"Record construction failed: $err"))
        }
      }
    }

    private def decodeEnum[A](enumSchema: Schema.Enum[A], xml: Xml): Either[DecodeError, A] = {
      val isSimple           = enumSchema.annotations.exists(_.isInstanceOf[simpleEnum])
      val hasNoDiscriminator = enumSchema.noDiscriminator
      val hasDiscriminator   = enumSchema.discriminatorName

      if (isSimple) {
        val text = extractText(xml)
        findCaseByName(enumSchema, text) match {
          case Some(c) =>
            constructCaseDefault(c) match {
              case Right(v) => Right(v)
              case Left(err) =>
                Left(DecodeError.ReadError(Cause.empty, s"Cannot construct simple enum case '$text': ${err.message}"))
            }
          case None =>
            Left(DecodeError.ReadError(Cause.empty, s"Unknown enum case: '$text'"))
        }
      } else if (hasDiscriminator.isDefined) {
        val discName = hasDiscriminator.get
        xml match {
          case element: Xml.Element =>
            val childMap = groupChildrenByName(element.children)
            childMap.get(discName) match {
              case Some(discXml) =>
                val caseName0 = extractText(discXml)
                findCaseByName(enumSchema, caseName0) match {
                  case Some(c) =>
                    decodeCase(c, xml)
                  case None =>
                    Left(DecodeError.ReadError(Cause.empty, s"Unknown enum case from discriminator: '$caseName0'"))
                }
              case None =>
                Left(DecodeError.ReadError(Cause.empty, s"Missing discriminator field '$discName'"))
            }
          case _ =>
            Left(DecodeError.ReadError(Cause.empty, "Expected element for discriminated enum"))
        }
      } else if (hasNoDiscriminator) {
        xml match {
          case element: Xml.Element =>
            var result: Either[DecodeError, A] = Left(DecodeError.ReadError(Cause.empty, "No matching case found"))
            val casesIt                        = enumSchema.nonTransientCases.iterator
            while (casesIt.hasNext) {
              val c = casesIt.next()
              decodeCase(c, element) match {
                case Right(v) =>
                  result = Right(v)
                  return result
                case Left(_) => ()
              }
            }
            result
          case _ =>
            Left(DecodeError.ReadError(Cause.empty, "Expected element for enum"))
        }
      } else {
        xml match {
          case element: Xml.Element =>
            val children = element.children
            children.collectFirst { case e: Xml.Element => e } match {
              case Some(caseElement) =>
                val caseName0 = caseElement.name.localName
                findCaseByName(enumSchema, caseName0) match {
                  case Some(c) =>
                    decodeCase(c, caseElement)
                  case None =>
                    Left(DecodeError.ReadError(Cause.empty, s"Unknown enum case: '$caseName0'"))
                }
              case None =>
                Left(DecodeError.ReadError(Cause.empty, "Expected child element for enum case"))
            }
          case _ =>
            Left(DecodeError.ReadError(Cause.empty, "Expected element for enum"))
        }
      }
    }

    private def decodeSequence[A](elemSchema: Schema[A], xml: Xml): Either[DecodeError, Chunk[A]] =
      xml match {
        case element: Xml.Element =>
          val results = element.children.collect { case e: Xml.Element => e }
          var acc     = Chunk.empty[A]
          val it      = results.iterator
          while (it.hasNext) {
            decodeSchema(elemSchema, it.next()) match {
              case Right(v)  => acc = acc :+ v
              case Left(err) => return Left(err)
            }
          }
          Right(acc)
        case _ =>
          Right(Chunk.empty)
      }

    private def decodeMap[K, V](
      keySchema: Schema[K],
      valueSchema: Schema[V],
      xml: Xml
    ): Either[DecodeError, Chunk[(K, V)]] =
      xml match {
        case element: Xml.Element =>
          val entries = element.children.collect { case e: Xml.Element if e.name.localName == "entry" => e }
          var acc     = Chunk.empty[(K, V)]
          val it      = entries.iterator
          while (it.hasNext) {
            val entry    = it.next()
            val childMap = groupChildrenByName(entry.children)
            val keyResult = childMap.get("key") match {
              case Some(k) => decodeSchema(keySchema, k)
              case None    => Left(DecodeError.MissingField(keySchema, "Missing 'key' in map entry"))
            }
            val valueResult = childMap.get("value") match {
              case Some(v) => decodeSchema(valueSchema, v)
              case None    => Left(DecodeError.MissingField(valueSchema, "Missing 'value' in map entry"))
            }
            (keyResult, valueResult) match {
              case (Right(k), Right(v)) => acc = acc :+ ((k, v))
              case (Left(e), _)         => return Left(e)
              case (_, Left(e))         => return Left(e)
            }
          }
          Right(acc)
        case _ =>
          Right(Chunk.empty)
      }

    private def decodeEither[A, B](
      leftSchema: Schema[A],
      rightSchema: Schema[B],
      xml: Xml
    ): Either[DecodeError, scala.util.Either[A, B]] =
      xml match {
        case element: Xml.Element =>
          val children = element.children
          children.collectFirst { case e: Xml.Element => e } match {
            case Some(child) =>
              child.name.localName match {
                case "Left" =>
                  decodeSchema(leftSchema, child).map(v => scala.util.Left(v))
                case "Right" =>
                  decodeSchema(rightSchema, child).map(v => scala.util.Right(v))
                case other =>
                  Left(DecodeError.ReadError(Cause.empty, s"Expected 'Left' or 'Right', got '$other'"))
              }
            case None =>
              Left(DecodeError.ReadError(Cause.empty, "Expected child element for Either"))
          }
        case _ =>
          Left(DecodeError.ReadError(Cause.empty, "Expected element for Either"))
      }

    private def decodeTuple[A, B](
      leftSchema: Schema[A],
      rightSchema: Schema[B],
      xml: Xml
    ): Either[DecodeError, (A, B)] =
      xml match {
        case element: Xml.Element =>
          val childMap = groupChildrenByName(element.children)
          for {
            a <- childMap.get("_1") match {
                  case Some(x) => decodeSchema(leftSchema, x)
                  case None    => Left(DecodeError.MissingField(leftSchema, "Missing '_1' in tuple"))
                }
            b <- childMap.get("_2") match {
                  case Some(x) => decodeSchema(rightSchema, x)
                  case None    => Left(DecodeError.MissingField(rightSchema, "Missing '_2' in tuple"))
                }
          } yield (a, b)
        case _ =>
          Left(DecodeError.ReadError(Cause.empty, "Expected element for Tuple"))
      }

    private def decodeFallback[A, B](
      leftSchema: Schema[A],
      rightSchema: Schema[B],
      fullDecode: Boolean,
      xml: Xml
    ): Either[DecodeError, Fallback[A, B]] =
      xml match {
        case element: Xml.Element =>
          val children = element.children
          children.collectFirst { case e: Xml.Element => e } match {
            case Some(child) =>
              child.name.localName match {
                case "Both" if fullDecode =>
                  val bothChildren = groupChildrenByName(child.children)
                  for {
                    l <- bothChildren.get("Left") match {
                          case Some(x) => decodeSchema(leftSchema, x)
                          case None    => Left(DecodeError.MissingField(leftSchema, "Missing 'Left' in Both"))
                        }
                    r <- bothChildren.get("Right") match {
                          case Some(x) => decodeSchema(rightSchema, x)
                          case None    => Left(DecodeError.MissingField(rightSchema, "Missing 'Right' in Both"))
                        }
                  } yield Fallback.Both(l, r)
                case "Left" =>
                  decodeSchema(leftSchema, child).map(Fallback.Left(_))
                case "Right" =>
                  decodeSchema(rightSchema, child).map(Fallback.Right(_))
                case "Both" =>
                  val bothChildren = groupChildrenByName(child.children)
                  bothChildren.get("Left") match {
                    case Some(x) => decodeSchema(leftSchema, x).map(Fallback.Left(_))
                    case None =>
                      bothChildren.get("Right") match {
                        case Some(x) => decodeSchema(rightSchema, x).map(Fallback.Right(_))
                        case None    => Left(DecodeError.ReadError(Cause.empty, "Both element has no Left or Right child"))
                      }
                  }
                case other =>
                  Left(DecodeError.ReadError(Cause.empty, s"Expected 'Left', 'Right', or 'Both', got '$other'"))
              }
            case None =>
              Left(DecodeError.ReadError(Cause.empty, "Expected child element for Fallback"))
          }
        case _ =>
          Left(DecodeError.ReadError(Cause.empty, "Expected element for Fallback"))
      }

    private def decodeDynamic(xml: Xml): Either[DecodeError, DynamicValue] =
      xml match {
        case Xml.Text(value) =>
          Right(DynamicValue.Primitive(value, StandardType.StringType))
        case Xml.CData(value) =>
          Right(DynamicValue.Primitive(value, StandardType.StringType))
        case element: Xml.Element =>
          val children = element.children
          if (children.isEmpty)
            Right(DynamicValue.Primitive("", StandardType.StringType))
          else if (children.size == 1 && children.head.isInstanceOf[Xml.Text])
            Right(DynamicValue.Primitive(children.head.asInstanceOf[Xml.Text].value, StandardType.StringType))
          else {
            val pairs = children.collect {
              case e: Xml.Element =>
                decodeDynamic(e).map(v => e.name.localName -> v)
            }
            var result = ListMap.empty[String, DynamicValue]
            val it     = pairs.iterator
            while (it.hasNext) {
              it.next() match {
                case Right((k, v)) => result = result + (k -> v)
                case Left(err)     => return Left(err)
              }
            }
            Right(DynamicValue.Record(TypeId.Structural, result))
          }
        case Xml.Comment(_) =>
          Right(DynamicValue.Primitive("", StandardType.StringType))
        case Xml.ProcessingInstruction(_, _) =>
          Right(DynamicValue.Primitive("", StandardType.StringType))
      }

    @SuppressWarnings(Array("org.wartremover.warts.ToString"))
    private def decodePrimitive[A](st: StandardType[A], text: String): Either[DecodeError, A] = {
      val result: Either[DecodeError, Any] = st match {
        case StandardType.UnitType =>
          Right(())
        case StandardType.StringType =>
          Right(text)
        case StandardType.BoolType =>
          text.toLowerCase match {
            case "true"  => Right(true)
            case "false" => Right(false)
            case _       => Left(DecodeError.ReadError(Cause.empty, s"Invalid boolean: '$text'"))
          }
        case StandardType.ByteType =>
          tryParse(text, _.toByte, "Byte")
        case StandardType.ShortType =>
          tryParse(text, _.toShort, "Short")
        case StandardType.IntType =>
          tryParse(text, _.toInt, "Int")
        case StandardType.LongType =>
          tryParse(text, _.toLong, "Long")
        case StandardType.FloatType =>
          tryParse(text, _.toFloat, "Float")
        case StandardType.DoubleType =>
          tryParse(text, _.toDouble, "Double")
        case StandardType.BinaryType =>
          try Right(Chunk.fromArray(Base64.getDecoder.decode(text)))
          catch { case NonFatal(e) => Left(DecodeError.ReadError(Cause.fail(e), s"Invalid Base64: $text")) }
        case StandardType.CharType =>
          if (text.length == 1) Right(text.charAt(0))
          else Left(DecodeError.ReadError(Cause.empty, s"Invalid Char: '$text'"))
        case StandardType.BigIntegerType =>
          tryParse(text, s => new java.math.BigInteger(s), "BigInteger")
        case StandardType.BigDecimalType =>
          tryParse(text, s => new java.math.BigDecimal(s), "BigDecimal")
        case StandardType.UUIDType =>
          tryParse(text, UUID.fromString, "UUID")
        case StandardType.DayOfWeekType =>
          tryParse(text, s => DayOfWeek.valueOf(s.toUpperCase), "DayOfWeek")
        case StandardType.DurationType =>
          tryParse(text, Duration.parse, "Duration")
        case StandardType.InstantType =>
          tryParse(text, Instant.parse, "Instant")
        case StandardType.LocalDateType =>
          tryParse(text, LocalDate.parse, "LocalDate")
        case StandardType.LocalDateTimeType =>
          tryParse(text, LocalDateTime.parse, "LocalDateTime")
        case StandardType.LocalTimeType =>
          tryParse(text, LocalTime.parse, "LocalTime")
        case StandardType.MonthType =>
          tryParse(text, s => Month.valueOf(s.toUpperCase), "Month")
        case StandardType.MonthDayType =>
          tryParse(text, MonthDay.parse, "MonthDay")
        case StandardType.OffsetDateTimeType =>
          tryParse(text, OffsetDateTime.parse, "OffsetDateTime")
        case StandardType.OffsetTimeType =>
          tryParse(text, OffsetTime.parse, "OffsetTime")
        case StandardType.PeriodType =>
          tryParse(text, Period.parse, "Period")
        case StandardType.YearType =>
          tryParse(text, s => Year.of(s.toInt), "Year")
        case StandardType.YearMonthType =>
          tryParse(text, YearMonth.parse, "YearMonth")
        case StandardType.ZonedDateTimeType =>
          tryParse(text, ZonedDateTime.parse, "ZonedDateTime")
        case StandardType.ZoneIdType =>
          tryParse(text, ZoneId.of, "ZoneId")
        case StandardType.ZoneOffsetType =>
          tryParse(text, ZoneOffset.of, "ZoneOffset")
        case StandardType.CurrencyType =>
          tryParse(text, java.util.Currency.getInstance, "Currency")
      }
      result.asInstanceOf[Either[DecodeError, A]]
    }

    private def tryParse[A](text: String, f: String => A, typeName: String): Either[DecodeError, A] =
      try Right(f(text))
      catch {
        case NonFatal(e) =>
          Left(DecodeError.ReadError(Cause.fail(e), s"Invalid $typeName: '$text'"))
      }

    private def extractText(xml: Xml): String =
      xml match {
        case Xml.Text(v)  => v
        case Xml.CData(v) => v
        case Xml.Element(_, _, children) =>
          children.collect {
            case Xml.Text(v)  => v
            case Xml.CData(v) => v
          }.mkString
        case Xml.Comment(_)                  => ""
        case Xml.ProcessingInstruction(_, _) => ""
      }

    private def groupChildrenByName(children: Chunk[Xml]): scala.collection.immutable.Map[String, Xml] = {
      var result = scala.collection.immutable.Map.empty[String, Xml]
      val it     = children.iterator
      while (it.hasNext) {
        it.next() match {
          case e: Xml.Element =>
            result = result + (e.name.localName -> e)
          case _ => ()
        }
      }
      result
    }

    private def groupAttributesByName(
      attributes: Chunk[(XmlName, String)]
    ): scala.collection.immutable.Map[String, String] = {
      var result = scala.collection.immutable.Map.empty[String, String]
      val it     = attributes.iterator
      while (it.hasNext) {
        val (name, value) = it.next()
        result = result + (name.localName -> value)
      }
      result
    }

    private def decodePrimitiveField[A](schema: Schema[A], text: String): Either[DecodeError, A] =
      schema match {
        case Schema.Primitive(standardType, _) =>
          decodePrimitive(standardType, text).asInstanceOf[Either[DecodeError, A]]
        case Schema.Optional(inner, _) =>
          if (text.isEmpty) Right(None.asInstanceOf[A])
          else decodePrimitiveField(inner, text).map(v => Some(v).asInstanceOf[A])
        case Schema.Lazy(inner0) =>
          decodePrimitiveField(inner0(), text)
        case Schema.Transform(innerSchema, f, _, _, _) =>
          decodePrimitiveField(innerSchema, text).flatMap { a =>
            f(a) match {
              case scala.util.Right(b) => Right(b.asInstanceOf[A])
              case scala.util.Left(err) =>
                Left(DecodeError.ReadError(Cause.empty, s"Transform failed: $err"))
            }
          }
        case _ =>
          Left(DecodeError.ReadError(Cause.empty, "Attribute fields must have primitive types"))
      }

    private def findCaseByName[A](enumSchema: Schema.Enum[A], name: String): Option[Schema.Case[A, _]] =
      enumSchema.nonTransientCases.find { c =>
        c.caseName == name || c.id == name || c.caseNameAliases.contains(name)
      }

    private def decodeCase[Z, A](c: Schema.Case[Z, A], xml: Xml): Either[DecodeError, Z] =
      decodeSchema(c.schema, xml).map(c.construct)

    private def constructCaseDefault[Z, A](c: Schema.Case[Z, A]): Either[DecodeError, Z] =
      c.schema.defaultValue match {
        case scala.util.Right(v)  => Right(c.construct(v))
        case scala.util.Left(err) => Left(DecodeError.ReadError(Cause.empty, err))
      }
  }
}
