package zio.schema.codec

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import javax.xml.parsers.SAXParserFactory

import scala.xml._

import org.xml.sax.InputSource

import zio._
import zio.schema._
import zio.schema.codec.XmlAnnotations._
import zio.stream.ZPipeline

object XmlCodec {

  case class Config(
    prettyPrint: Boolean = false,
    collectionElementName: String = "item",
    includeTypeAnnotations: Boolean = false,
    omitEmptyElements: Boolean = true,
    namespaceUri: Option[String] = None,
    namespacePrefix: Option[String] = None,
    wrapCollections: Boolean = false,
    xmlDeclaration: Boolean = true
  )

  object Config {
    val default: Config = Config()
  }

  private def getFieldName(field: Schema.Field[_, _]): String =
    field.annotations.collectFirst { case name(n) => n }.getOrElse(field.name.toString)

  private def isAttribute(annotations: Chunk[Any]): Boolean =
    annotations.exists(_.isInstanceOf[attribute])

  private def isText(annotations: Chunk[Any]): Boolean =
    annotations.exists(_.isInstanceOf[text])

  private def isCData(annotations: Chunk[Any]): Boolean =
    annotations.exists(_.isInstanceOf[cdata])

  private def shouldOmitEmpty(annotations: Chunk[Any]): Boolean =
    annotations.exists(_.isInstanceOf[omitEmpty])

  private def getRootName(schema: Schema[_]): String = schema match {
    case r: Schema.Record[_] => r.id.name
    case e: Schema.Enum[_]   => e.id.name
    case _                   => "root"
  }

  def encode[A](schema: Schema[A]): BinaryCodec[A] =
    encode(schema, Config.default)

  def encode[A](schema: Schema[A], config: Config): BinaryCodec[A] =
    new BinaryCodec[A] {
      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] =
        XmlDecoder.decode(schema, whole, config)

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
        ZPipeline.mapChunksEither(bytes => decode(bytes).map(Chunk.single))

      override def encode(value: A): Chunk[Byte] =
        XmlEncoder.encode(schema, value, config)

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
        ZPipeline.mapChunks(_.flatMap(encode))
    }

  object XmlEncoder {

    def encode[A](schema: Schema[A], value: A, config: Config): Chunk[Byte] = {
      val rootName = getRootName(schema)
      val nodes    = encodeNode(value, schema, rootName, config)

      val rootNode = if (nodes.size == 1) {
        nodes.head
      } else if (nodes.isEmpty) {
        Elem(null, "empty", Null, TopScope, true)
      } else {
        Elem(null, rootName, Null, TopScope, true, nodes: _*)
      }

      val xmlContent = if (config.prettyPrint) {
        new PrettyPrinter(80, 2).format(rootNode)
      } else {
        rootNode.toString
      }

      val finalXml = if (config.xmlDeclaration) {
        s"""<?xml version="1.0" encoding="UTF-8"?>\n$xmlContent"""
      } else {
        xmlContent
      }
      Chunk.fromArray(finalXml.getBytes(StandardCharsets.UTF_8))
    }

    private def encodeNode[A](value: A, schema: Schema[A], name: String, config: Config): Seq[Node] = schema match {
      case Schema.Primitive(stdType, _) =>
        Seq(encodePrimitiveWithAnnotations(value, stdType, name, config, Chunk.empty))

      case Schema.Optional(innerSchema, _) =>
        value.asInstanceOf[Option[Any]] match {
          case Some(v) => encodeNode(v, innerSchema.asInstanceOf[Schema[Any]], name, config)
          case None =>
            if (config.omitEmptyElements) Seq.empty
            else Seq(Elem(null, name, Null, TopScope, true))
        }

      case Schema.Sequence(elementSchema, _, g, _, _) =>
        val items = g(value)
        if (config.wrapCollections) {
          val children = items.flatMap(item => encodeNode(item, elementSchema, config.collectionElementName, config))
          Seq(
            Elem(
              null,
              name,
              Null,
              TopScope,
              true,
              children.toSeq: _*
            )
          )
        } else {
          items.flatMap(item => encodeNode(item, elementSchema, name, config)).toSeq
        }

      case Schema.Map(keySchema, valueSchema, _) =>
        val map = value.asInstanceOf[Map[Any, Any]]
        val entries = map.map {
          case (k, v) =>
            Elem(
              null,
              "entry",
              Null,
              TopScope,
              true,
              (encodeNode(k, keySchema.asInstanceOf[Schema[Any]], "key", config) ++ encodeNode(
                v,
                valueSchema.asInstanceOf[Schema[Any]],
                "value",
                config
              )): _*
            )
        }
        Seq(Elem(null, name, Null, TopScope, true, entries.toSeq: _*))

      case Schema.Set(elementSchema, _) =>
        val set = value.asInstanceOf[Set[Any]]
        if (config.wrapCollections) {
          val children = set.flatMap(
            item => encodeNode(item, elementSchema.asInstanceOf[Schema[Any]], config.collectionElementName, config)
          )
          Seq(
            Elem(
              null,
              name,
              Null,
              TopScope,
              true,
              children.toSeq: _*
            )
          )
        } else {
          set.flatMap(item => encodeNode(item, elementSchema.asInstanceOf[Schema[Any]], name, config)).toSeq
        }

      case Schema.Either(leftSchema, rightSchema, _) =>
        value.asInstanceOf[Either[Any, Any]] match {
          case Left(l) =>
            Seq(
              Elem(
                null,
                name,
                Null,
                TopScope,
                true,
                Elem(
                  null,
                  "left",
                  Null,
                  TopScope,
                  true,
                  encodeNode(l, leftSchema.asInstanceOf[Schema[Any]], "value", config): _*
                )
              )
            )
          case Right(r) =>
            Seq(
              Elem(
                null,
                name,
                Null,
                TopScope,
                true,
                Elem(
                  null,
                  "right",
                  Null,
                  TopScope,
                  true,
                  encodeNode(r, rightSchema.asInstanceOf[Schema[Any]], "value", config): _*
                )
              )
            )
        }

      case Schema.Tuple2(leftSchema, rightSchema, _) =>
        val tuple = value.asInstanceOf[(Any, Any)]
        Seq(
          Elem(
            null,
            name,
            Null,
            TopScope,
            true,
            (encodeNode(tuple._1, leftSchema.asInstanceOf[Schema[Any]], "_1", config) ++ encodeNode(
              tuple._2,
              rightSchema.asInstanceOf[Schema[Any]],
              "_2",
              config
            )): _*
          )
        )

      case Schema.Transform(codec, _, g, _, _) =>
        g(value) match {
          case Left(_) =>
            if (config.omitEmptyElements) Seq.empty
            else Seq(Elem(null, name, Null, TopScope, true))
          case Right(b) => encodeNode(b, codec, name, config)
        }

      case Schema.Lazy(inner) =>
        encodeNode(value, inner(), name, config)

      case r: Schema.Record[A] =>
        Seq(encodeRecord(value, r, name, config))

      case e: Schema.Enum[A] =>
        Seq(encodeEnum(value, e, name, config))

      case _ =>
        Seq(Elem(null, name, Null, TopScope, true, Text(value.toString)))
    }

    private def encodePrimitiveWithAnnotations(
      value: Any,
      stdType: StandardType[_],
      name: String,
      config: Config,
      annotations: Chunk[Any]
    ): Elem = {
      val text = (value, stdType) match {
        case (chunk: Chunk[_], StandardType.BinaryType) =>
          java.util.Base64.getEncoder.encodeToString(chunk.asInstanceOf[Chunk[Byte]].toArray)
        case _ => value.toString
      }

      val attributes = if (config.includeTypeAnnotations) {
        val typeName = stdType match {
          case StandardType.StringType => "xs:string"
          case StandardType.IntType    => "xs:int"
          case StandardType.LongType   => "xs:long"
          case StandardType.DoubleType => "xs:double"
          case StandardType.BoolType   => "xs:boolean"
          case _                       => "xs:anySimpleType"
        }
        new PrefixedAttribute("xsi", "type", typeName, Null)
      } else Null

      val textNode: Node = if (isCData(annotations) && text.nonEmpty) Unparsed(s"<![CDATA[$text]]>") else Text(text)
      Elem(null, name, attributes, TopScope, true, textNode)
    }

    private def encodeRecord[A](value: A, record: Schema.Record[A], name: String, config: Config): Elem = {
      val attributes = record.fields.collect {
        case field if isAttribute(field.annotations) =>
          val fieldName  = getFieldName(field)
          val fieldValue = field.get(value)
          val text = fieldValue match {
            case s: String => s
            case other     => other.toString
          }
          new UnprefixedAttribute(fieldName, text, Null)
      }

      val textContent: Option[(String, Boolean)] = record.fields.collectFirst {
        case field if isText(field.annotations) =>
          val fieldValue = field.get(value)
          val text = fieldValue match {
            case s: String => s
            case other     => other.toString
          }
          val useCData = isCData(field.annotations)
          (text, useCData)
      }

      val childElements = record.fields.flatMap { (field: Schema.Field[A, _]) =>
        if (!isAttribute(field.annotations) && !isText(field.annotations)) {
          val fieldName  = getFieldName(field)
          val fieldValue = field.get(value)
          val omitEmpty  = shouldOmitEmpty(field.annotations) || config.omitEmptyElements

          val isEmpty = fieldValue match {
            case None                              => true
            case coll: Iterable[_] if coll.isEmpty => true
            case _                                 => false
          }

          if (omitEmpty && isEmpty) {
            Seq.empty
          } else {
            field.schema match {
              case Schema.Primitive(stdType, _) if isCData(field.annotations) =>
                Seq(encodePrimitiveWithAnnotations(fieldValue, stdType, fieldName, config, field.annotations))
              case _ =>
                encodeNode(fieldValue, field.schema.asInstanceOf[Schema[Any]], fieldName, config)
            }
          }
        } else {
          Seq.empty
        }
      }

      val combinedAttrs = attributes.foldLeft[MetaData](Null)((acc, attr) => attr.copy(next = acc))

      val children: Seq[Node] = textContent match {
        case Some((txt, true))  => Seq(Unparsed(s"<![CDATA[$txt]]>"))
        case Some((txt, false)) => Seq(Text(txt))
        case None               => childElements
      }

      Elem(null, name, combinedAttrs, TopScope, children.isEmpty, children: _*)
    }

    private def encodeEnum[A](value: A, `enum`: Schema.Enum[A], name: String, config: Config): Elem = {
      val caseNode = `enum`.cases.collectFirst {
        case c if c.asInstanceOf[Schema.Case[A, Any]].deconstructOption(value).isDefined =>
          val caseValue = c.asInstanceOf[Schema.Case[A, Any]].deconstructOption(value).get
          val caseName  = c.id
          val result    = encodeNode(caseValue, c.schema.asInstanceOf[Schema[Any]], caseName, config)
          if (result.nonEmpty) result.head else Elem(null, caseName, Null, TopScope, true)
      }.getOrElse(Elem(null, "unknown", Null, TopScope, true))

      Elem(null, name, Null, TopScope, false, caseNode)
    }
  }

  object XmlDecoder {

    // disable DOCTYPE and external entities to prevent XXE attacks
    private def secureXML: SAXParser = {
      val factory = SAXParserFactory.newInstance()
      factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true)
      factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
      factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
      factory.newSAXParser()
    }

    def decode[A](schema: Schema[A], bytes: Chunk[Byte], config: Config): Either[DecodeError, A] =
      try {
        val parser = secureXML
        val xml    = XML.loadXML(new InputSource(new ByteArrayInputStream(bytes.toArray)), parser)
        decodeNode(Seq(xml), schema, config, "<root>")
      } catch {
        case e: Exception =>
          Left(DecodeError.ReadError(Cause.fail(e), s"XML parse failed: ${e.getMessage}"))
      }

    private def collectDecoded[A](
      results: scala.collection.Seq[Either[DecodeError, A]]
    ): Either[DecodeError, Chunk[A]] =
      results.collectFirst { case Left(err) => Left(err) }
        .getOrElse(Right(Chunk.fromIterable(results.collect { case Right(v) => v })))

    private def decodeNode[A](
      nodes: Seq[Node],
      schema: Schema[A],
      config: Config,
      path: String
    ): Either[DecodeError, A] =
      schema match {
        case Schema.Primitive(stdType, _) =>
          nodes.headOption match {
            case Some(node) => decodePrimitive(node.text, stdType, path)
            case None       => Left(DecodeError.ReadError(Cause.empty, s"Primitive missing: $path"))
          }

        case Schema.Optional(innerSchema, _) =>
          if (nodes.isEmpty) {
            Right(None.asInstanceOf[A])
          } else {
            if (nodes.head.label == "empty" && nodes.head.child.isEmpty) {
              Right(None.asInstanceOf[A])
            } else {
              decodeNode(nodes, innerSchema, config, s"$path??").map(v => Some(v).asInstanceOf[A])
            }
          }

        case Schema.Sequence(elementSchema, fromChunk, _, _, _) =>
          val items = if (config.wrapCollections) {
            nodes.headOption.map { wrapper =>
              wrapper.child.filter(_.label == config.collectionElementName)
            }.getOrElse(Seq.empty)
          } else {
            nodes
          }

          collectDecoded(
            items.zipWithIndex.map {
              case (child, idx) =>
                decodeNode(Seq(child), elementSchema, config, s"$path[$idx]")
            }
          ).map(values => fromChunk(values).asInstanceOf[A])

        case Schema.Map(keySchema, valueSchema, _) =>
          val mapNode =
            nodes.headOption.toRight(DecodeError.ReadError(Cause.empty, s"Map element missing: $path"))
          mapNode.flatMap { node =>
            val entries = node.child.filter(_.label == "entry")
            collectDecoded(
              entries.zipWithIndex.map {
                case (entry, idx) =>
                  val keyNode   = entry.child.find(_.label == "key").map(Seq(_)).getOrElse(Seq.empty)
                  val valueNode = entry.child.find(_.label == "value").map(Seq(_)).getOrElse(Seq.empty)

                  if (keyNode.nonEmpty && valueNode.nonEmpty) {
                    for {
                      key   <- decodeNode(keyNode, keySchema, config, s"$path.entry[$idx].key")
                      value <- decodeNode(valueNode, valueSchema, config, s"$path.entry[$idx].value")
                    } yield (key, value)
                  } else {
                    Left(
                      DecodeError.ReadError(Cause.empty, s"Incomplete map entry at $path.entry[$idx]")
                    )
                  }
              }
            ).map(pairs => pairs.map { case (k, v) => (k, v) }.toMap.asInstanceOf[A])
          }

        case Schema.Set(elementSchema, _) =>
          val items = if (config.wrapCollections) {
            nodes.headOption.map { wrapper =>
              wrapper.child.filter(_.label == config.collectionElementName)
            }.getOrElse(Seq.empty)
          } else {
            nodes
          }

          collectDecoded(
            items.zipWithIndex.map {
              case (child, idx) =>
                decodeNode(Seq(child), elementSchema, config, s"$path[$idx]")
            }
          ).map(_.toSet.asInstanceOf[A])

        case Schema.Either(leftSchema, rightSchema, _) =>
          nodes.headOption
            .toRight(DecodeError.ReadError(Cause.empty, s"Either element missing: $path"))
            .flatMap { node =>
              val leftNode  = node.child.find(_.label == "left")
              val rightNode = node.child.find(_.label == "right")
              (leftNode, rightNode) match {
                case (Some(l), None) =>
                  val valueNode = l.child.find(_.label == "value").getOrElse(l)
                  decodeNode(Seq(valueNode), leftSchema, config, s"$path.left").map(v => Left(v).asInstanceOf[A])
                case (None, Some(r)) =>
                  val valueNode = r.child.find(_.label == "value").getOrElse(r)
                  decodeNode(Seq(valueNode), rightSchema, config, s"$path.right").map(v => Right(v).asInstanceOf[A])
                case _ =>
                  Left(
                    DecodeError.ReadError(
                      Cause.empty,
                      s"Either must have left or right at $path"
                    )
                  )
              }
            }

        case Schema.Tuple2(leftSchema, rightSchema, _) =>
          nodes.headOption
            .toRight(DecodeError.ReadError(Cause.empty, s"Tuple element missing: $path"))
            .flatMap { node =>
              val left  = node.child.find(_.label == "_1")
              val right = node.child.find(_.label == "_2")
              (left, right) match {
                case (Some(l), Some(r)) =>
                  for {
                    leftValue  <- decodeNode(Seq(l), leftSchema, config, s"$path._1")
                    rightValue <- decodeNode(Seq(r), rightSchema, config, s"$path._2")
                  } yield (leftValue, rightValue).asInstanceOf[A]
                case _ =>
                  Left(DecodeError.ReadError(Cause.empty, s"Incomplete tuple at $path"))
              }
            }

        case Schema.Transform(codec, f, _, _, _) =>
          decodeNode(nodes, codec, config, path).flatMap { decoded =>
            f(decoded).left.map(err => DecodeError.ReadError(Cause.empty, s"Transform failed at $path: $err"))
          }

        case Schema.Lazy(inner) =>
          decodeNode(nodes, inner(), config, path)

        case r: Schema.Record[A] =>
          nodes.headOption match {
            case Some(node) => decodeRecord(node, r, config, path)
            case None       => Left(DecodeError.ReadError(Cause.empty, s"Record missing: $path"))
          }

        case e: Schema.Enum[A] =>
          nodes.headOption match {
            case Some(node) => decodeEnum(node, e, config, path)
            case None       => Left(DecodeError.ReadError(Cause.empty, s"Enum missing: $path"))
          }

        case _ =>
          Left(DecodeError.ReadError(Cause.empty, s"Unsupported schema: ${schema}"))
      }

    private def decodePrimitive[A](text: String, stdType: StandardType[A], path: String): Either[DecodeError, A] =
      try {
        val result = stdType match {
          case StandardType.UnitType   => ()
          case StandardType.StringType => text
          case StandardType.BoolType   => text.toBoolean
          case StandardType.ByteType   => text.toByte
          case StandardType.ShortType  => text.toShort
          case StandardType.IntType    => text.toInt
          case StandardType.LongType   => text.toLong
          case StandardType.FloatType  => text.toFloat
          case StandardType.DoubleType => text.toDouble
          case StandardType.BinaryType =>
            Chunk.fromArray(java.util.Base64.getDecoder.decode(text))
          case StandardType.CharType       => text.headOption.getOrElse('\u0000')
          case StandardType.BigDecimalType => new java.math.BigDecimal(text)
          case StandardType.BigIntegerType => new java.math.BigInteger(text)
          case _ =>
            return Left(DecodeError.ReadError(Cause.empty, s"Unsupported primitive type: $stdType at path: $path"))
        }
        Right(result.asInstanceOf[A])
      } catch {
        case e: Exception =>
          Left(
            DecodeError.ReadError(
              Cause.fail(e),
              s"Can't decode $stdType at $path from '$text': ${e.getMessage}"
            )
          )
      }

    private def decodeRecord[A](
      node: Node,
      record: Schema.Record[A],
      config: Config,
      path: String
    ): Either[DecodeError, A] = {
      val fieldResults = record.fields.map { field =>
        val fieldName = XmlCodec.getFieldName(field)
        val fieldPath = s"$path.$fieldName"

        if (isAttribute(field.annotations)) {
          val attrValue = node.attribute(fieldName).map(_.text).getOrElse("")
          field.schema match {
            case Schema.Primitive(stdType, _) => decodePrimitive(attrValue, stdType, fieldPath)
            case _                            => decodePrimitive(attrValue, StandardType.StringType, fieldPath)
          }
        } else if (isText(field.annotations)) {
          val textValue = node.text
          field.schema match {
            case Schema.Primitive(stdType, _) => decodePrimitive(textValue, stdType, fieldPath)
            case _                            => decodePrimitive(textValue, StandardType.StringType, fieldPath)
          }
        } else {
          val fieldNodes = (node \ fieldName)
          decodeNode(fieldNodes, field.schema, config, fieldPath)
        }
      }

      fieldResults.find(_.isLeft) match {
        case Some(Left(err)) => Left(err)
        case _ =>
          val values = fieldResults.collect { case Right(v) => v }
          Unsafe.unsafe { implicit u =>
            record
              .construct(Chunk.fromIterable(values))
              .left
              .map(err => DecodeError.ReadError(Cause.fail(err), path))
          }
      }
    }

    private def decodeEnum[A](
      node: Node,
      `enum`: Schema.Enum[A],
      config: Config,
      path: String
    ): Either[DecodeError, A] = {
      val caseNodes = node.child.filter(_.isInstanceOf[Elem])

      if (caseNodes.isEmpty) {
        return Left(
          DecodeError.ReadError(
            Cause.empty,
            s"Enum case missing at $path"
          )
        )
      }

      val caseNode = caseNodes.head
      val caseName = caseNode.label

      `enum`.cases.find(_.id == caseName) match {
        case Some(c) =>
          val castedCase = c.asInstanceOf[Schema.Case[A, Any]]
          decodeNode(Seq(caseNode), castedCase.schema, config, s"$path.$caseName").map(castedCase.construct)
        case None =>
          val availableCases = `enum`.cases.map(_.id).mkString(", ")
          Left(
            DecodeError.ReadError(
              Cause.empty,
              s"Unknown case '$caseName' at $path (expected: $availableCases)"
            )
          )
      }
    }
  }
}
