package zio.schema.codec

import scala.util.Try
import scala.xml._

import zio._
import zio.schema._
import zio.stream.{ ZChannel, ZPipeline }

object XmlCodec {

  final case class Configuration(
    attributePrefix: String = "@",
    textNodeName: String = "#text",
    ignoreEmptyCollections: Boolean = false,
    treatStreamsAsDocuments: Boolean = false
  )

  object Configuration {
    val default: Configuration = Configuration()
  }

  def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A] =
    schemaBasedBinaryCodec[A](Configuration.default)

  def schemaBasedBinaryCodec[A](cfg: Configuration)(implicit schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {
      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] =
        XmlDecoder.decode(schema, new String(whole.toArray, "UTF-8"), cfg)

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
        ZPipeline.utfDecode.mapError(cce => DecodeError.ReadError(Cause.fail(cce), cce.getMessage)) >>>
          xmlSplitter >>>
          ZPipeline.mapZIO { (s: String) =>
            ZIO.fromEither(XmlDecoder.decode(schema, s, cfg))
          }

      override def encode(value: A): Chunk[Byte] =
        XmlEncoder.encode(schema, value, cfg)

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
        if (cfg.treatStreamsAsDocuments) {
          ZPipeline.mapChunks[A, Chunk[Byte]](_.map(encode)).flattenChunks
        } else {
          ZPipeline.mapChunks[A, Chunk[Byte]](_.map(encode)).intersperse(Chunk.single('\n'.toByte)).flattenChunks
        }
    }

  private val xmlSplitter: ZPipeline[Any, Nothing, String, String] =
    ZPipeline.suspend {
      val stringBuilder = new StringBuilder
      var inTag         = false
      var depth         = 0

      def fetchChunk(chunk: Chunk[String]): Chunk[String] = {
        val chunkBuilder = ChunkBuilder.make[String]()

        for {
          string <- chunk
          c      <- string
        } {
          stringBuilder.append(c)

          c match {
            case '<' if !inTag =>
              inTag = true
              if (stringBuilder.length > 1 && stringBuilder.charAt(stringBuilder.length - 2) != '>') {
                depth += 1
              }
            case '>' if inTag =>
              inTag = false
              if (stringBuilder.length > 2 && stringBuilder.charAt(stringBuilder.length - 2) == '/') {
                // Self-closing tag
                if (depth == 1) {
                  chunkBuilder += stringBuilder.result()
                  stringBuilder.clear()
                  depth = 0
                }
              } else if (stringBuilder.length > 2 && stringBuilder.charAt(1) == '/') {
                // Closing tag
                depth -= 1
                if (depth == 0) {
                  chunkBuilder += stringBuilder.result()
                  stringBuilder.clear()
                }
              }
            case _ =>
          }
        }

        chunkBuilder.result()
      }

      lazy val loop: ZChannel[Any, ZNothing, Chunk[String], Any, Nothing, Chunk[String], Any] =
        ZChannel.readWithCause(
          in => {
            val out = fetchChunk(in)
            if (out.isEmpty) loop else ZChannel.write(out) *> loop
          },
          err =>
            if (stringBuilder.isEmpty) ZChannel.refailCause(err)
            else ZChannel.write(Chunk.single(stringBuilder.result())) *> ZChannel.refailCause(err),
          done =>
            if (stringBuilder.isEmpty) ZChannel.succeed(done)
            else ZChannel.write(Chunk.single(stringBuilder.result())) *> ZChannel.succeed(done)
        )

      ZPipeline.fromChannel(loop)
    }

  object XmlEncoder {

    def encode[A](schema: Schema[A], value: A, cfg: Configuration): Chunk[Byte] = {
      val xml = schema match {
        case Schema.Primitive(standardType, _) =>
          Elem(null, standardType.tag, Null, TopScope, false, Text(encodeValue(value, schema, None, cfg).text))
        case _ =>
          encodeValue(value, schema, None, cfg)
      }
      Chunk.fromArray(xml.toString.getBytes("UTF-8"))
    }

    private def encodeValue[A](value: A, schema: Schema[A], fieldName: Option[String], cfg: Configuration): Node =
      schema match {
        case Schema.Primitive(standardType, _) =>
          encodePrimitive(value, standardType, fieldName)

        case Schema.Sequence(elementSchema, _, toChunk, _, _) =>
          encodeSequence(toChunk(value), elementSchema, fieldName, cfg)

        case Schema.Map(_, valueSchema, _) =>
          encodeMap(value.asInstanceOf[Map[String, _]], valueSchema.asInstanceOf[Schema[Any]], fieldName, cfg)

        case Schema.Set(elementSchema, _) =>
          encodeSet(value.asInstanceOf[Set[Any]], elementSchema.asInstanceOf[Schema[Any]], fieldName, cfg)

        case record: Schema.Record[A] =>
          encodeRecord(value, record, fieldName, cfg)

        case Schema.Enum1(_, case1, _) =>
          encodeEnum1(value, case1, fieldName, cfg)

        case Schema.Enum2(_, case1, case2, _) =>
          encodeEnum2(value, case1, case2, fieldName, cfg)

        case Schema.Enum3(_, case1, case2, case3, _) =>
          encodeEnum3(value, case1, case2, case3, fieldName, cfg)

        case Schema.EnumN(_, cases, _) =>
          encodeEnumN(value, cases.toSeq, fieldName, cfg)

        case Schema.Optional(schema, _) =>
          encodeOptional(value.asInstanceOf[Option[_]], schema.asInstanceOf[Schema[Any]], fieldName, cfg)

        case Schema.Tuple2(left, right, _) =>
          encodeTuple2(
            value.asInstanceOf[(_, _)],
            left.asInstanceOf[Schema[Any]],
            right.asInstanceOf[Schema[Any]],
            fieldName,
            cfg
          )

        case Schema.Either(left, right, _) =>
          encodeEither(
            value.asInstanceOf[Either[_, _]],
            left.asInstanceOf[Schema[Any]],
            right.asInstanceOf[Schema[Any]],
            fieldName,
            cfg
          )

        case Schema.Transform(schema, _, _, _, _) =>
          encodeValue(value, schema.asInstanceOf[Schema[A]], fieldName, cfg)

        case Schema.Lazy(schema0) =>
          encodeValue(value, schema0(), fieldName, cfg)

        case _ =>
          throw new RuntimeException(s"Unsupported schema type for XML encoding: ${schema.getClass.getSimpleName}")
      }

    private def encodePrimitive[A](value: A, standardType: StandardType[A], fieldName: Option[String]): Node = {
      val text = standardType match {
        case StandardType.StringType     => value.toString
        case StandardType.BoolType       => value.toString
        case StandardType.ByteType       => value.toString
        case StandardType.ShortType      => value.toString
        case StandardType.IntType        => value.toString
        case StandardType.LongType       => value.toString
        case StandardType.FloatType      => value.toString
        case StandardType.DoubleType     => value.toString
        case StandardType.CharType       => value.toString
        case StandardType.BigIntegerType => value.toString
        case StandardType.BigDecimalType => value.toString
        case StandardType.UUIDType       => value.toString
        case StandardType.UnitType       => "()"
        case StandardType.BinaryType =>
          val chunk = value.asInstanceOf[Chunk[Byte]]
          java.util.Base64.getEncoder.encodeToString(chunk.toArray)
        case _ =>
          throw new RuntimeException(
            s"Unsupported standard type for XML encoding: ${standardType.getClass.getSimpleName}"
          )
      }
      fieldName match {
        case Some(name) => Elem(null, name, Null, TopScope, false, Text(text))
        case None       => Text(text)
      }
    }

    private def encodeSequence[A](
      chunk: Chunk[A],
      elementSchema: Schema[A],
      fieldName: Option[String],
      cfg: Configuration
    ): Node = {
      val children = chunk.map(elem => encodeValue(elem, elementSchema, None, cfg))
      fieldName match {
        case Some(name) => Elem(null, name, Null, TopScope, false, children: _*)
        case None       => Group(children)
      }
    }

    private def encodeMap(
      map: Map[String, _],
      valueSchema: Schema[Any],
      fieldName: Option[String],
      cfg: Configuration
    ): Node = {
      val entries = map.toSeq.map {
        case (k: String, v: Any) =>
          Elem(
            null,
            "entry",
            Attribute("key", Text(k.toString), Null),
            TopScope,
            false,
            encodeValue(v.asInstanceOf[Any], valueSchema, None, cfg)
          )
      }
      fieldName match {
        case Some(name) => Elem(null, name, Null, TopScope, false, entries: _*)
        case None       => Group(entries)
      }
    }

    private def encodeSet[A](
      set: Set[A],
      elementSchema: Schema[A],
      fieldName: Option[String],
      cfg: Configuration
    ): Node =
      encodeSequence(Chunk.fromIterable(set), elementSchema, fieldName, cfg)

    private def encodeRecord[A](
      value: A,
      record: Schema.Record[A],
      fieldName: Option[String],
      cfg: Configuration
    ): Node = {
      val fields = record.fields.flatMap { field =>
        val fieldValue  = field.get(value)
        val isAttribute = field.annotations.exists(_.isInstanceOf[xmlAttribute])

        if (isAttribute) {
          None // Attributes are handled separately
        } else {
          Some(encodeValue(fieldValue, field.schema.asInstanceOf[Schema[Any]], Some(field.name), cfg))
        }
      }

      val attributes = record.fields.flatMap { field =>
        val fieldValue  = field.get(value)
        val isAttribute = field.annotations.exists(_.isInstanceOf[xmlAttribute])

        if (isAttribute) {
          Some(Attribute(field.name, Text(fieldValue.toString), Null))
        } else {
          None
        }
      }.foldLeft[MetaData](Null) { (acc, attr) =>
        attr.copy(next = acc)
      }

      val elemName = fieldName.getOrElse(record.id.name)
      Elem(null, elemName, attributes, TopScope, false, fields: _*)
    }

    private def encodeEnum1[A, A1](
      value: A,
      case1: Schema.Case[A, A1],
      fieldName: Option[String],
      cfg: Configuration
    ): Node = {
      val caseValue = case1.deconstruct(value)
      Elem(null, fieldName.getOrElse(case1.id), Null, TopScope, false, encodeValue(caseValue, case1.schema, None, cfg))
    }

    private def encodeEnum2[A, A1, A2](
      value: A,
      case1: Schema.Case[A, A1],
      case2: Schema.Case[A, A2],
      fieldName: Option[String],
      cfg: Configuration
    ): Node =
      case1.deconstructOption(value) match {
        case Some(caseValue) =>
          Elem(
            null,
            fieldName.getOrElse(case1.id),
            Null,
            TopScope,
            false,
            encodeValue(caseValue, case1.schema, None, cfg)
          )
        case None =>
          val caseValue = case2.deconstruct(value)
          Elem(
            null,
            fieldName.getOrElse(case2.id),
            Null,
            TopScope,
            false,
            encodeValue(caseValue, case2.schema, None, cfg)
          )
      }

    private def encodeEnum3[A, A1, A2, A3](
      value: A,
      case1: Schema.Case[A, A1],
      case2: Schema.Case[A, A2],
      case3: Schema.Case[A, A3],
      fieldName: Option[String],
      cfg: Configuration
    ): Node =
      case1.deconstructOption(value) match {
        case Some(caseValue) =>
          Elem(
            null,
            fieldName.getOrElse(case1.id),
            Null,
            TopScope,
            false,
            encodeValue(caseValue, case1.schema, None, cfg)
          )
        case None =>
          case2.deconstructOption(value) match {
            case Some(caseValue) =>
              Elem(
                null,
                fieldName.getOrElse(case2.id),
                Null,
                TopScope,
                false,
                encodeValue(caseValue, case2.schema, None, cfg)
              )
            case None =>
              val caseValue = case3.deconstruct(value)
              Elem(
                null,
                fieldName.getOrElse(case3.id),
                Null,
                TopScope,
                false,
                encodeValue(caseValue, case3.schema, None, cfg)
              )
          }
      }

    private def encodeEnumN[A](
      value: A,
      cases: Seq[Schema.Case[A, _]],
      fieldName: Option[String],
      cfg: Configuration
    ): Node =
      cases.find(_.deconstructOption(value).isDefined) match {
        case Some(matchedCase) =>
          val caseValue = matchedCase.deconstruct(value)
          Elem(
            null,
            fieldName.getOrElse(matchedCase.id),
            Null,
            TopScope,
            false,
            encodeValue(caseValue, matchedCase.schema.asInstanceOf[Schema[Any]], None, cfg)
          )
        case None =>
          throw new RuntimeException(s"No case matched for enum value: $value")
      }

    private def encodeOptional(
      value: Option[_],
      schema: Schema[Any],
      fieldName: Option[String],
      cfg: Configuration
    ): Node =
      value match {
        case Some(v) => encodeValue(v.asInstanceOf[Any], schema, fieldName, cfg)
        case None =>
          fieldName match {
            case Some(name) => Elem(null, name, Attribute("nil", Text("true"), Null), TopScope, false)
            case None       => Text("")
          }
      }

    private def encodeTuple2(
      value: (_, _),
      left: Schema[Any],
      right: Schema[Any],
      fieldName: Option[String],
      cfg: Configuration
    ): Node = {
      val (a, b) = value
      val children = Seq(
        encodeValue(a.asInstanceOf[Any], left, Some("_1"), cfg),
        encodeValue(b.asInstanceOf[Any], right, Some("_2"), cfg)
      )
      fieldName match {
        case Some(name) => Elem(null, name, Null, TopScope, false, children: _*)
        case None       => Group(children)
      }
    }

    private def encodeEither(
      value: Either[_, _],
      left: Schema[Any],
      right: Schema[Any],
      fieldName: Option[String],
      cfg: Configuration
    ): Node =
      value match {
        case Left(a) =>
          val elem = encodeValue(a.asInstanceOf[Any], left, None, cfg)
          fieldName match {
            case Some(name) => Elem(null, name, Attribute("type", Text("left"), Null), TopScope, false, elem)
            case None       => Elem(null, "left", Null, TopScope, false, elem)
          }
        case Right(b) =>
          val elem = encodeValue(b.asInstanceOf[Any], right, None, cfg)
          fieldName match {
            case Some(name) => Elem(null, name, Attribute("type", Text("right"), Null), TopScope, false, elem)
            case None       => Elem(null, "right", Null, TopScope, false, elem)
          }
      }
  }

  object XmlDecoder {

    def decode[A](schema: Schema[A], xml: String, cfg: Configuration): Either[DecodeError, A] =
      Try(XML.loadString(xml)).toEither match {
        case Left(error) =>
          Left(DecodeError.ReadError(Cause.fail(error), s"Failed to parse XML: ${error.getMessage}"))
        case Right(node) =>
          schema match {
            case Schema.Primitive(standardType, _) =>
              // For primitives, decode from the text content of the root element
              decodePrimitive(node, standardType)
            case _ =>
              decodeValue(node, schema, cfg)
          }
      }

    private def decodeValue[A](node: Node, schema: Schema[A], cfg: Configuration): Either[DecodeError, A] =
      schema match {
        case Schema.Primitive(standardType, _) =>
          decodePrimitive(node, standardType)

        case Schema.Sequence(elementSchema, fromChunk, _, _, _) =>
          decodeSequence(node, elementSchema, fromChunk, cfg)

        case Schema.Map(keySchema, valueSchema, _) =>
          decodeMap(node, keySchema, valueSchema, cfg)

        case Schema.Set(elementSchema, _) =>
          decodeSet(node, elementSchema, cfg)

        case record: Schema.Record[A] =>
          decodeRecord(node, record, cfg)

        case Schema.Enum1(_, case1, _) =>
          decodeEnum1(node, case1, cfg)

        case Schema.Enum2(_, case1, case2, _) =>
          decodeEnum2(node, case1, case2, cfg)

        case Schema.Enum3(_, case1, case2, case3, _) =>
          decodeEnum3(node, case1, case2, case3, cfg)

        case Schema.EnumN(_, cases, _) =>
          decodeEnumN(node, cases.toSeq, cfg)

        case Schema.Optional(schema, _) =>
          decodeOptional(node, schema, cfg)

        case Schema.Tuple2(left, right, _) =>
          decodeTuple2(node, left, right, cfg)

        case Schema.Either(left, right, _) =>
          decodeEither(node, left, right, cfg)

        case Schema.Transform(schema, f, _, _, _) =>
          decodeValue(node, schema, cfg).flatMap { value =>
            f(value) match {
              case Left(error)   => Left(DecodeError.ReadError(Cause.empty, error))
              case Right(result) => Right(result.asInstanceOf[A])
            }
          }

        case Schema.Lazy(schema0) =>
          decodeValue(node, schema0(), cfg)

        case _ =>
          Left(DecodeError.ReadError(Cause.empty, s"Unsupported schema type: $schema"))
      }

    private def decodePrimitive[A](node: Node, standardType: StandardType[A]): Either[DecodeError, A] = {
      val text = node.text
      Try {
        standardType match {
          case StandardType.StringType     => text.asInstanceOf[A]
          case StandardType.BoolType       => text.toBoolean.asInstanceOf[A]
          case StandardType.ByteType       => text.toByte.asInstanceOf[A]
          case StandardType.ShortType      => text.toShort.asInstanceOf[A]
          case StandardType.IntType        => text.toInt.asInstanceOf[A]
          case StandardType.LongType       => text.toLong.asInstanceOf[A]
          case StandardType.FloatType      => text.toFloat.asInstanceOf[A]
          case StandardType.DoubleType     => text.toDouble.asInstanceOf[A]
          case StandardType.CharType       => text.charAt(0).asInstanceOf[A]
          case StandardType.BigIntegerType => new java.math.BigInteger(text).asInstanceOf[A]
          case StandardType.BigDecimalType => new java.math.BigDecimal(text).asInstanceOf[A]
          case StandardType.UUIDType       => java.util.UUID.fromString(text).asInstanceOf[A]
          case StandardType.UnitType       => ().asInstanceOf[A]
          case StandardType.BinaryType =>
            val bytes = java.util.Base64.getDecoder.decode(text)
            Chunk.fromArray(bytes).asInstanceOf[A]
          case _ => text.asInstanceOf[A]
        }
      }.toEither.left
        .map(error => DecodeError.ReadError(Cause.fail(error), error.getMessage))
        .asInstanceOf[Either[DecodeError, A]]
    }

    private def decodeSequence[A, Col](
      node: Node,
      elementSchema: Schema[A],
      fromChunk: Chunk[A] => Col,
      cfg: Configuration
    ): Either[DecodeError, Col] = {
      val children = node match {
        case elem: Elem => elem.child.filter(_.isInstanceOf[Elem])
        case _          => Seq.empty
      }

      val decoded = children.foldLeft[Either[DecodeError, Chunk[A]]](Right(Chunk.empty)) { (acc, child) =>
        acc.flatMap { chunk =>
          decodeValue(child, elementSchema, cfg).map(chunk :+ _)
        }
      }

      decoded.map(fromChunk)
    }

    private def decodeMap[K, V](
      node: Node,
      keySchema: Schema[K],
      valueSchema: Schema[V],
      cfg: Configuration
    ): Either[DecodeError, Map[K, V]] = {
      val entries = node match {
        case elem: Elem => elem.child.filter(_.label == "entry")
        case _          => Seq.empty
      }

      entries.foldLeft[Either[DecodeError, Map[K, V]]](Right(Map.empty)) { (acc, entry) =>
        acc.flatMap { map =>
          entry.attribute("key") match {
            case Some(keyNode) =>
              for {
                key   <- decodePrimitive(Text(keyNode.text), keySchema.asInstanceOf[Schema.Primitive[K]].standardType)
                value <- decodeValue(entry.child.head, valueSchema, cfg)
              } yield map + (key -> value)
            case None =>
              Left(DecodeError.ReadError(Cause.empty, "Map entry missing 'key' attribute"))
          }
        }
      }
    }

    private def decodeSet[A](node: Node, elementSchema: Schema[A], cfg: Configuration): Either[DecodeError, Set[A]] =
      decodeSequence(node, elementSchema, (chunk: Chunk[A]) => chunk.toSet, cfg)

    private def decodeRecord[A](node: Node, record: Schema.Record[A], cfg: Configuration): Either[DecodeError, A] = {
      val elem = node match {
        case e: Elem => e
        case _       => return Left(DecodeError.ReadError(Cause.empty, "Expected element for record"))
      }

      val fieldValues = record.fields.foldLeft[Either[DecodeError, Chunk[Any]]](Right(Chunk.empty)) { (acc, field) =>
        acc.flatMap { values =>
          val isAttribute = field.annotations.exists(_.isInstanceOf[xmlAttribute])

          val fieldValue = if (isAttribute) {
            elem.attribute(field.name) match {
              case Some(attr) =>
                decodePrimitive(Text(attr.text), field.schema.asInstanceOf[Schema.Primitive[_]].standardType)
              case None =>
                if (field.schema.isInstanceOf[Schema.Optional[_]]) Right(None)
                else Left(DecodeError.ReadError(Cause.empty, s"Missing attribute: ${field.name}"))
            }
          } else {
            elem.child.find(_.label == field.name) match {
              case Some(child) => decodeValue(child, field.schema, cfg)
              case None =>
                if (field.schema.isInstanceOf[Schema.Optional[_]]) Right(None)
                else Left(DecodeError.ReadError(Cause.empty, s"Missing field: ${field.name}"))
            }
          }

          fieldValue.map(v => values :+ v)
        }
      }

      fieldValues.flatMap { values =>
        record.construct(values)(Unsafe.unsafe) match {
          case Left(error)  => Left(DecodeError.ReadError(Cause.empty, error))
          case Right(value) => Right(value)
        }
      }
    }

    private def decodeEnum1[A, A1](node: Node, case1: Schema.Case[A, A1], cfg: Configuration): Either[DecodeError, A] =
      if (node.label == case1.id) {
        decodeValue(node.child.head, case1.schema, cfg).map { value =>
          case1.construct(value)
        }
      } else {
        Left(DecodeError.ReadError(Cause.empty, s"Expected ${case1.id} but got ${node.label}"))
      }

    private def decodeEnum2[A, A1, A2](
      node: Node,
      case1: Schema.Case[A, A1],
      case2: Schema.Case[A, A2],
      cfg: Configuration
    ): Either[DecodeError, A] =
      if (node.label == case1.id) {
        decodeValue(node.child.head, case1.schema, cfg).map { value =>
          case1.construct(value)
        }
      } else if (node.label == case2.id) {
        decodeValue(node.child.head, case2.schema, cfg).map { value =>
          case2.construct(value)
        }
      } else {
        Left(DecodeError.ReadError(Cause.empty, s"Expected ${case1.id} or ${case2.id} but got ${node.label}"))
      }

    private def decodeEnum3[A, A1, A2, A3](
      node: Node,
      case1: Schema.Case[A, A1],
      case2: Schema.Case[A, A2],
      case3: Schema.Case[A, A3],
      cfg: Configuration
    ): Either[DecodeError, A] =
      if (node.label == case1.id) {
        decodeValue(node.child.head, case1.schema, cfg).map { value =>
          case1.construct(value)
        }
      } else if (node.label == case2.id) {
        decodeValue(node.child.head, case2.schema, cfg).map { value =>
          case2.construct(value)
        }
      } else if (node.label == case3.id) {
        decodeValue(node.child.head, case3.schema, cfg).map { value =>
          case3.construct(value)
        }
      } else {
        Left(
          DecodeError.ReadError(Cause.empty, s"Expected ${case1.id}, ${case2.id} or ${case3.id} but got ${node.label}")
        )
      }

    private def decodeEnumN[A](node: Node, cases: Seq[Schema.Case[A, _]], cfg: Configuration): Either[DecodeError, A] =
      cases.find(_.id == node.label) match {
        case Some(matchedCase) =>
          decodeValue(node.child.head, matchedCase.schema.asInstanceOf[Schema[Any]], cfg).map { value =>
            matchedCase.asInstanceOf[Schema.Case[A, Any]].construct(value)
          }
        case None =>
          Left(DecodeError.ReadError(Cause.empty, s"No matching case for ${node.label}"))
      }

    private def decodeOptional[A](node: Node, schema: Schema[A], cfg: Configuration): Either[DecodeError, Option[A]] =
      node match {
        case elem: Elem if elem.attribute("nil").exists(_.text == "true") => Right(None)
        case _ if node.text.isEmpty                                       => Right(None)
        case _                                                            => decodeValue(node, schema, cfg).map(Some(_))
      }

    private def decodeTuple2[A, B](
      node: Node,
      left: Schema[A],
      right: Schema[B],
      cfg: Configuration
    ): Either[DecodeError, (A, B)] = {
      val elem = node match {
        case e: Elem => e
        case _       => return Left(DecodeError.ReadError(Cause.empty, "Expected element for tuple"))
      }

      for {
        a <- elem.child.find(_.label == "_1") match {
              case Some(child) => decodeValue(child, left, cfg)
              case None        => Left(DecodeError.ReadError(Cause.empty, "Missing _1 in tuple"))
            }
        b <- elem.child.find(_.label == "_2") match {
              case Some(child) => decodeValue(child, right, cfg)
              case None        => Left(DecodeError.ReadError(Cause.empty, "Missing _2 in tuple"))
            }
      } yield (a, b)
    }

    private def decodeEither[A, B](
      node: Node,
      left: Schema[A],
      right: Schema[B],
      cfg: Configuration
    ): Either[DecodeError, Either[A, B]] =
      node match {
        case elem: Elem =>
          elem.attribute("type").map(_.text) match {
            case Some("left")  => decodeValue(elem.child.head, left, cfg).map(Left(_))
            case Some("right") => decodeValue(elem.child.head, right, cfg).map(Right(_))
            case _ =>
              if (elem.label == "left") decodeValue(elem.child.head, left, cfg).map(Left(_))
              else if (elem.label == "right") decodeValue(elem.child.head, right, cfg).map(Right(_))
              else
                Left(
                  DecodeError.ReadError(Cause.empty, "Either must have type attribute or be named 'left' or 'right'")
                )
          }
        case _ => Left(DecodeError.ReadError(Cause.empty, "Expected element for Either"))
      }
  }

  case class xmlAttribute() extends scala.annotation.StaticAnnotation
}
