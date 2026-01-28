package zio.schema.codec

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.time._
import java.util.UUID
import javax.xml.parsers.SAXParserFactory

import scala.xml._

import org.xml.sax.InputSource

import zio._
import zio.schema._
import zio.schema.codec.XmlAnnotations._
import zio.stream.ZPipeline

object XmlCodec {

  case class Config(
    prettyPrint: Boolean = false,            // format output with indentation
    collectionElementName: String = "item",  // element name for collection items
    includeTypeAnnotations: Boolean = false, // add xsi:type attributes for primitives
    omitEmptyElements: Boolean = true,       // omit elements for None and empty collections
    namespaceUri: Option[String] = None,     // XML namespace URI
    namespacePrefix: Option[String] = None,  // XML namespace prefix
    wrapCollections: Boolean = false,        // wrap collection items in a parent element
    xmlDeclaration: Boolean = true           // include <?xml version="1.0"?> declaration
  )

  object Config {
    val default: Config = Config()

    val scalaXbCompatible: Config = Config(
      wrapCollections = true,
      omitEmptyElements = false,
      includeTypeAnnotations = false
    )
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
      val rootNode = selectRootNode(nodes, rootName)
      val xmlText  = formatXml(rootNode, config)
      Chunk.fromArray(xmlText.getBytes(StandardCharsets.UTF_8))
    }

    private def selectRootNode(nodes: Seq[Node], fallbackName: String): Elem =
      nodes match {
        case Seq(single) => single.asInstanceOf[Elem]
        case Seq()       => Elem(null, "empty", Null, TopScope, true)
        case multiple    => Elem(null, fallbackName, Null, TopScope, true, multiple: _*)
      }

    private def formatXml(node: Elem, config: Config): String = {
      val declaration = if (config.xmlDeclaration) "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" else ""
      val content = if (config.prettyPrint) {
        new PrettyPrinter(80, 2).format(node)
      } else {
        node.toString
      }
      declaration + content
    }

    private def encodeNode[A](value: A, schema: Schema[A], name: String, config: Config): Seq[Node] = schema match {
      case Schema.Primitive(stdType, _) =>
        Seq(encodePrimitiveWithAnnotations(value, stdType, name, config, Chunk.empty))

      case Schema.Optional(innerSchema, _) =>
        value.asInstanceOf[Option[Any]] match {
          case Some(v) => encodeNode(v, innerSchema, name, config)
          case None    => if (config.omitEmptyElements) Seq.empty else Seq(Elem(null, name, Null, TopScope, true))
        }

      case Schema.Sequence(elementSchema, _, g, _, _) =>
        encodeSequence(g(value), elementSchema, name, config)

      case Schema.Map(keySchema, valueSchema, _) =>
        encodeMap(value.asInstanceOf[Map[Any, Any]], keySchema, valueSchema, name, config)

      case Schema.Set(elementSchema, _) =>
        encodeSet(value.asInstanceOf[Set[Any]], elementSchema, name, config)

      case Schema.Either(leftSchema, rightSchema, _) =>
        encodeEither(value.asInstanceOf[Either[Any, Any]], leftSchema, rightSchema, name, config)

      case Schema.Tuple2(leftSchema, rightSchema, _) =>
        encodeTuple(value.asInstanceOf[(Any, Any)], leftSchema, rightSchema, name, config)

      case Schema.Transform(codec, _, g, _, _) =>
        g(value) match {
          case Left(err) => throw new IllegalArgumentException(s"Transform failed during encoding at $name: $err")
          case Right(b)  => encodeNode(b, codec, name, config)
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

    private def encodeSequence(
      items: Chunk[Any],
      elementSchema: Schema[Any],
      name: String,
      config: Config
    ): Seq[Node] = {
      val itemNodes = items.flatMap(item => encodeNode(item, elementSchema, config.collectionElementName, config))
      if (config.wrapCollections) {
        Seq(Elem(null, name, Null, TopScope, true, itemNodes.toSeq: _*))
      } else {
        items.flatMap(item => encodeNode(item, elementSchema, name, config)).toSeq
      }
    }

    private def encodeMap(
      map: Map[Any, Any],
      keySchema: Schema[Any],
      valueSchema: Schema[Any],
      name: String,
      config: Config
    ): Seq[Node] = {
      // Sort by key for reproducible XML output
      val entries = map.toSeq.sortBy(_._1.toString).map {
        case (k, v) =>
          val keyNodes   = encodeNode(k, keySchema, "key", config)
          val valueNodes = encodeNode(v, valueSchema, "value", config)
          Elem(null, "entry", Null, TopScope, true, (keyNodes ++ valueNodes): _*)
      }
      Seq(Elem(null, name, Null, TopScope, true, entries: _*))
    }

    private def encodeSet(
      set: Set[Any],
      elementSchema: Schema[Any],
      name: String,
      config: Config
    ): Seq[Node] = {
      val sortedItems = set.toSeq.sortBy(_.toString)
      if (config.wrapCollections) {
        val children =
          sortedItems.flatMap(item => encodeNode(item, elementSchema, config.collectionElementName, config))
        Seq(Elem(null, name, Null, TopScope, true, children: _*))
      } else {
        sortedItems.flatMap(item => encodeNode(item, elementSchema, name, config))
      }
    }

    private def encodeEither(
      either: Either[Any, Any],
      leftSchema: Schema[Any],
      rightSchema: Schema[Any],
      name: String,
      config: Config
    ): Seq[Node] = {
      val caseNode = either match {
        case Left(l) =>
          val leftNodes = encodeNode(l, leftSchema, "value", config)
          Elem(null, "left", Null, TopScope, true, leftNodes: _*)
        case Right(r) =>
          val rightNodes = encodeNode(r, rightSchema, "value", config)
          Elem(null, "right", Null, TopScope, true, rightNodes: _*)
      }
      Seq(Elem(null, name, Null, TopScope, true, caseNode))
    }

    private def encodeTuple(
      tuple: (Any, Any),
      leftSchema: Schema[Any],
      rightSchema: Schema[Any],
      name: String,
      config: Config
    ): Seq[Node] = {
      val leftNodes  = encodeNode(tuple._1, leftSchema, "_1", config)
      val rightNodes = encodeNode(tuple._2, rightSchema, "_2", config)
      Seq(Elem(null, name, Null, TopScope, true, (leftNodes ++ rightNodes): _*))
    }

    private def encodePrimitiveWithAnnotations(
      value: Any,
      stdType: StandardType[_],
      name: String,
      config: Config,
      annotations: Chunk[Any]
    ): Elem = {
      val text       = formatPrimitiveValue(value, stdType)
      val attributes = buildTypeAttributes(stdType, config)
      val textNode   = wrapTextNode(text, annotations)
      Elem(null, name, attributes, TopScope, true, textNode)
    }

    // Format primitive values as text for XML
    private def formatPrimitiveValue(value: Any, stdType: StandardType[_]): String =
      (value, stdType) match {
        case (chunk: Chunk[_], StandardType.BinaryType) =>
          java.util.Base64.getEncoder.encodeToString(chunk.asInstanceOf[Chunk[Byte]].toArray)
        case (uuid: UUID, StandardType.UUIDType)                               => uuid.toString
        case (instant: Instant, StandardType.InstantType)                      => instant.toString
        case (localDate: LocalDate, StandardType.LocalDateType)                => localDate.toString
        case (localTime: LocalTime, StandardType.LocalTimeType)                => localTime.toString
        case (localDateTime: LocalDateTime, StandardType.LocalDateTimeType)    => localDateTime.toString
        case (offsetTime: OffsetTime, StandardType.OffsetTimeType)             => offsetTime.toString
        case (offsetDateTime: OffsetDateTime, StandardType.OffsetDateTimeType) => offsetDateTime.toString
        case (zonedDateTime: ZonedDateTime, StandardType.ZonedDateTimeType)    => zonedDateTime.toString
        case (duration: Duration, StandardType.DurationType)                   => duration.toString
        case (period: Period, StandardType.PeriodType)                         => period.toString
        case (year: Year, StandardType.YearType)                               => year.toString
        case (yearMonth: YearMonth, StandardType.YearMonthType)                => yearMonth.toString
        case (monthDay: MonthDay, StandardType.MonthDayType)                   => monthDay.toString
        case (dayOfWeek: DayOfWeek, StandardType.DayOfWeekType)                => dayOfWeek.toString
        case (month: Month, StandardType.MonthType)                            => month.toString
        case (zoneId: ZoneId, StandardType.ZoneIdType)                         => zoneId.getId
        case (zoneOffset: ZoneOffset, StandardType.ZoneOffsetType)             => zoneOffset.getId
        case _                                                                 => value.toString
      }

    private def buildTypeAttributes(stdType: StandardType[_], config: Config): MetaData =
      if (!config.includeTypeAnnotations) {
        Null
      } else {
        val typeName = stdType match {
          case StandardType.StringType => "xs:string"
          case StandardType.IntType    => "xs:int"
          case StandardType.LongType   => "xs:long"
          case StandardType.DoubleType => "xs:double"
          case StandardType.BoolType   => "xs:boolean"
          case _                       => "xs:anySimpleType"
        }
        new PrefixedAttribute("xsi", "type", typeName, Null)
      }

    private def wrapTextNode(text: String, annotations: Chunk[Any]): Node =
      if (isCData(annotations) && text.nonEmpty) {
        Unparsed(s"<![CDATA[$text]]>")
      } else {
        Text(text)
      }

    private def encodeRecord[A](value: A, record: Schema.Record[A], name: String, config: Config): Elem = {
      val attributes    = collectAttributes(record, value)
      val textContent   = collectTextContent(record, value)
      val childElements = collectChildElements(record, value, config)
      buildRecordElement(name, attributes, textContent, childElements)
    }

    private def collectAttributes[A](record: Schema.Record[A], value: A): MetaData = {
      val attrs = record.fields.collect {
        case field if isAttribute(field.annotations) =>
          val fieldName  = getFieldName(field)
          val fieldValue = field.get(value)
          val text       = fieldValue.toString
          new UnprefixedAttribute(fieldName, text, Null)
      }
      attrs.foldLeft[MetaData](Null)((acc, attr) => attr.copy(next = acc))
    }

    private def collectTextContent[A](record: Schema.Record[A], value: A): Option[(String, Boolean)] =
      record.fields.collectFirst {
        case field if isText(field.annotations) =>
          val fieldValue = field.get(value)
          val text       = fieldValue.toString
          val useCData   = isCData(field.annotations)
          (text, useCData)
      }

    private def collectChildElements[A](
      record: Schema.Record[A],
      value: A,
      config: Config
    ): Seq[Node] =
      record.fields
        .filterNot(_.transient)
        .flatMap { (field: Schema.Field[A, _]) =>
          if (isAttribute(field.annotations) || isText(field.annotations)) {
            Seq.empty
          } else {
            encodeFieldAsChild(field, value, config)
          }
        }

    private def encodeFieldAsChild[A](
      field: Schema.Field[A, _],
      value: A,
      config: Config
    ): Seq[Node] = {
      val fieldName  = getFieldName(field)
      val fieldValue = field.get(value)
      val omitEmpty  = shouldOmitEmpty(field.annotations) || config.omitEmptyElements

      if (omitEmpty && isFieldEmpty(fieldValue)) {
        Seq.empty
      } else {
        field.schema match {
          case Schema.Primitive(stdType, _) if isCData(field.annotations) =>
            Seq(encodePrimitiveWithAnnotations(fieldValue, stdType, fieldName, config, field.annotations))
          case _ =>
            encodeNode(fieldValue, field.schema.asInstanceOf[Schema[Any]], fieldName, config)
        }
      }
    }

    private def isFieldEmpty(value: Any): Boolean =
      value match {
        case None                              => true
        case coll: Iterable[_] if coll.isEmpty => true
        case _                                 => false
      }

    private def buildRecordElement(
      name: String,
      attributes: MetaData,
      textContent: Option[(String, Boolean)],
      childElements: Seq[Node]
    ): Elem = {
      val children: Seq[Node] = textContent match {
        case Some((txt, true))  => Seq(Unparsed(s"<![CDATA[$txt]]>"))
        case Some((txt, false)) => Seq(Text(txt))
        case None               => childElements
      }
      Elem(null, name, attributes, TopScope, children.isEmpty, children: _*)
    }

    private def encodeEnum[A](value: A, `enum`: Schema.Enum[A], name: String, config: Config): Elem = {
      val caseNode = findMatchingCase(`enum`, value, config)
      Elem(null, name, Null, TopScope, false, caseNode)
    }

    private def findMatchingCase[A](
      `enum`: Schema.Enum[A],
      value: A,
      config: Config
    ): Node =
      `enum`.cases.collectFirst {
        case c if c.asInstanceOf[Schema.Case[A, Any]].deconstructOption(value).isDefined =>
          val caseValue = c.asInstanceOf[Schema.Case[A, Any]].deconstructOption(value).get
          val caseName  = c.id
          val nodes     = encodeNode(caseValue, c.schema.asInstanceOf[Schema[Any]], caseName, config)
          if (nodes.nonEmpty) nodes.head else Elem(null, caseName, Null, TopScope, true)
      }.getOrElse(Elem(null, "unknown", Null, TopScope, true))
  }

  object XmlDecoder {

    // XXE (XML External Entity) attack prevention
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
            case None       => Left(DecodeError.ReadError(Cause.empty, s"Missing primitive value at $path"))
          }

        case Schema.Optional(innerSchema, _) =>
          if (nodes.isEmpty) {
            Right(None.asInstanceOf[A])
          } else if (nodes.head.label == "empty" && nodes.head.child.isEmpty) {
            Right(None.asInstanceOf[A])
          } else {
            decodeNode(nodes, innerSchema, config, s"$path??").map(v => Some(v).asInstanceOf[A])
          }

        case Schema.Sequence(elementSchema, fromChunk, _, _, _) =>
          decodeSequence(nodes, elementSchema, fromChunk, config, path)

        case Schema.Map(keySchema, valueSchema, _) =>
          decodeMap(nodes, keySchema, valueSchema, config, path)

        case Schema.Set(elementSchema, _) =>
          decodeSet(nodes, elementSchema, config, path)

        case Schema.Either(leftSchema, rightSchema, _) =>
          decodeEither(nodes, leftSchema, rightSchema, config, path)

        case Schema.Tuple2(leftSchema, rightSchema, _) =>
          decodeTuple(nodes, leftSchema, rightSchema, config, path)

        case Schema.Transform(codec, f, _, _, _) =>
          decodeNode(nodes, codec, config, path).flatMap { decoded =>
            f(decoded).left.map(err => DecodeError.ReadError(Cause.empty, s"Transform failed at $path: $err"))
          }

        case Schema.Lazy(inner) =>
          decodeNode(nodes, inner(), config, path)

        case r: Schema.Record[A] =>
          nodes.headOption match {
            case Some(node) => decodeRecord(node, r, config, path)
            case None       => Left(DecodeError.ReadError(Cause.empty, s"Expected record element at $path"))
          }

        case e: Schema.Enum[A] =>
          nodes.headOption match {
            case Some(node) => decodeEnum(node, e, config, path)
            case None       => Left(DecodeError.ReadError(Cause.empty, s"Expected enum element at $path"))
          }

        case _ =>
          Left(DecodeError.ReadError(Cause.empty, s"Unsupported schema: ${schema}"))
      }

    private def decodeSequence[A](
      nodes: Seq[Node],
      elementSchema: Schema[Any],
      fromChunk: Chunk[Any] => A,
      config: Config,
      path: String
    ): Either[DecodeError, A] = {
      val items = if (config.wrapCollections) {
        nodes.headOption
          .map(_.child.filter(_.label == config.collectionElementName))
          .getOrElse(Seq.empty)
      } else {
        nodes
      }
      collectDecoded(
        items.zipWithIndex.map {
          case (child, idx) => decodeNode(Seq(child), elementSchema, config, s"$path[$idx]")
        }
      ).map(values => fromChunk(values.asInstanceOf[Chunk[Any]]).asInstanceOf[A])
    }

    private def decodeMap[K, V](
      nodes: Seq[Node],
      keySchema: Schema[K],
      valueSchema: Schema[V],
      config: Config,
      path: String
    ): Either[DecodeError, Map[K, V]] =
      nodes.headOption.toRight(DecodeError.ReadError(Cause.empty, s"Map element missing: $path")).flatMap { node =>
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
                Left(DecodeError.ReadError(Cause.empty, s"Incomplete map entry at $path.entry[$idx]"))
              }
          }
        ).map(_.toMap)
      }

    private def decodeSet[A](
      nodes: Seq[Node],
      elementSchema: Schema[Any],
      config: Config,
      path: String
    ): Either[DecodeError, Set[A]] = {
      val items = if (config.wrapCollections) {
        nodes.headOption
          .map(_.child.filter(_.label == config.collectionElementName))
          .getOrElse(Seq.empty)
      } else {
        nodes
      }
      collectDecoded(
        items.zipWithIndex.map {
          case (child, idx) => decodeNode(Seq(child), elementSchema, config, s"$path[$idx]")
        }
      ).map(_.toSet.asInstanceOf[Set[A]])
    }

    private def decodeEither[L, R](
      nodes: Seq[Node],
      leftSchema: Schema[L],
      rightSchema: Schema[R],
      config: Config,
      path: String
    ): Either[DecodeError, Either[L, R]] =
      nodes.headOption.toRight(DecodeError.ReadError(Cause.empty, s"Either element missing: $path")).flatMap { node =>
        val leftNode  = node.child.find(_.label == "left")
        val rightNode = node.child.find(_.label == "right")
        (leftNode, rightNode) match {
          case (Some(l), None) =>
            val valueNode = l.child.find(_.label == "value").getOrElse(l)
            decodeNode(Seq(valueNode), leftSchema, config, s"$path.left").map(Left(_))
          case (None, Some(r)) =>
            val valueNode = r.child.find(_.label == "value").getOrElse(r)
            decodeNode(Seq(valueNode), rightSchema, config, s"$path.right").map(Right(_))
          case _ =>
            Left(DecodeError.ReadError(Cause.empty, s"Either must have left or right at $path"))
        }
      }

    private def decodeTuple[L, R](
      nodes: Seq[Node],
      leftSchema: Schema[L],
      rightSchema: Schema[R],
      config: Config,
      path: String
    ): Either[DecodeError, (L, R)] =
      nodes.headOption.toRight(DecodeError.ReadError(Cause.empty, s"Tuple element missing: $path")).flatMap { node =>
        val left  = node.child.find(_.label == "_1")
        val right = node.child.find(_.label == "_2")
        (left, right) match {
          case (Some(l), Some(r)) =>
            for {
              leftValue  <- decodeNode(Seq(l), leftSchema, config, s"$path._1")
              rightValue <- decodeNode(Seq(r), rightSchema, config, s"$path._2")
            } yield (leftValue, rightValue)
          case _ =>
            Left(DecodeError.ReadError(Cause.empty, s"Incomplete tuple at $path"))
        }
      }

    private def collectDecoded[A](
      results: scala.collection.Seq[Either[DecodeError, A]]
    ): Either[DecodeError, Chunk[A]] =
      results.collectFirst { case Left(err) => Left(err) }
        .getOrElse(Right(Chunk.fromIterable(results.collect { case Right(v) => v })))

    private def decodePrimitive[A](text: String, stdType: StandardType[A], path: String): Either[DecodeError, A] =
      try {
        val result = stdType match {
          case StandardType.UnitType           => ()
          case StandardType.StringType         => text
          case StandardType.BoolType           => text.toBoolean
          case StandardType.ByteType           => text.toByte
          case StandardType.ShortType          => text.toShort
          case StandardType.IntType            => text.toInt
          case StandardType.LongType           => text.toLong
          case StandardType.FloatType          => text.toFloat
          case StandardType.DoubleType         => text.toDouble
          case StandardType.BinaryType         => Chunk.fromArray(java.util.Base64.getDecoder.decode(text))
          case StandardType.CharType           => text.headOption.getOrElse('\u0000')
          case StandardType.BigDecimalType     => new java.math.BigDecimal(text)
          case StandardType.BigIntegerType     => new java.math.BigInteger(text)
          case StandardType.UUIDType           => UUID.fromString(text)
          case StandardType.InstantType        => Instant.parse(text)
          case StandardType.LocalDateType      => LocalDate.parse(text)
          case StandardType.LocalTimeType      => LocalTime.parse(text)
          case StandardType.LocalDateTimeType  => LocalDateTime.parse(text)
          case StandardType.OffsetTimeType     => OffsetTime.parse(text)
          case StandardType.OffsetDateTimeType => OffsetDateTime.parse(text)
          case StandardType.ZonedDateTimeType  => ZonedDateTime.parse(text)
          case StandardType.DurationType       => java.time.Duration.parse(text)
          case StandardType.PeriodType         => Period.parse(text)
          case StandardType.YearType           => Year.parse(text)
          case StandardType.YearMonthType      => YearMonth.parse(text)
          case StandardType.MonthDayType       => MonthDay.parse(text)
          case StandardType.DayOfWeekType      => DayOfWeek.valueOf(text.toUpperCase)
          case StandardType.MonthType          => Month.valueOf(text.toUpperCase)
          case StandardType.ZoneIdType         => ZoneId.of(text)
          case StandardType.ZoneOffsetType     => ZoneOffset.of(text)
          case _ =>
            return Left(DecodeError.ReadError(Cause.empty, s"Cannot decode primitive type $stdType at $path"))
        }
        Right(result.asInstanceOf[A])
      } catch {
        case e: Exception =>
          Left(
            DecodeError.ReadError(
              Cause.fail(e),
              s"Failed to decode $stdType at $path from '$text': ${e.getClass.getSimpleName}: ${e.getMessage}"
            )
          )
      }

    private def decodeRecord[A](
      node: Node,
      record: Schema.Record[A],
      config: Config,
      path: String
    ): Either[DecodeError, A] = {
      val builder = ChunkBuilder.make[Any]()
      var idx     = 0
      while (idx < record.fields.length) {
        val field       = record.fields(idx)
        val fieldResult = decodeField(node, field, config, path)
        fieldResult match {
          case Left(err)    => return Left(err)
          case Right(value) => builder += value
        }
        idx += 1
      }
      constructRecord(record, builder.result(), path)
    }

    private def decodeField[R](
      node: Node,
      field: Schema.Field[R, _],
      config: Config,
      path: String
    ): Either[DecodeError, Any] = {
      val fieldName = XmlCodec.getFieldName(field)
      val fieldPath = s"$path.$fieldName"

      if (field.transient) {
        field.defaultValue.toRight(DecodeError.ReadError(Cause.empty, s"Transient field without default: $fieldPath"))
      } else if (isAttribute(field.annotations)) {
        decodeAttribute(node, field, fieldName, fieldPath)
      } else if (isText(field.annotations)) {
        decodeTextContent(node, field, fieldPath)
      } else {
        val fieldNodes = node \ fieldName
        decodeNode(fieldNodes, field.schema, config, fieldPath)
      }
    }

    private def decodeAttribute[R](
      node: Node,
      field: Schema.Field[R, _],
      fieldName: String,
      fieldPath: String
    ): Either[DecodeError, Any] = {
      val attrValue = node.attribute(fieldName).map(_.text).getOrElse("")
      field.schema match {
        case Schema.Primitive(stdType, _) => decodePrimitive(attrValue, stdType, fieldPath)
        case _                            => decodePrimitive(attrValue, StandardType.StringType, fieldPath)
      }
    }

    private def decodeTextContent[R](
      node: Node,
      field: Schema.Field[R, _],
      fieldPath: String
    ): Either[DecodeError, Any] = {
      val textValue = node.text
      field.schema match {
        case Schema.Primitive(stdType, _) => decodePrimitive(textValue, stdType, fieldPath)
        case _                            => decodePrimitive(textValue, StandardType.StringType, fieldPath)
      }
    }

    private def constructRecord[A](
      record: Schema.Record[A],
      values: Chunk[Any],
      path: String
    ): Either[DecodeError, A] =
      Unsafe.unsafe { implicit u =>
        record
          .construct(values)
          .left
          .map(err => DecodeError.ReadError(Cause.fail(err), path))
      }

    private def decodeEnum[A](
      node: Node,
      `enum`: Schema.Enum[A],
      config: Config,
      path: String
    ): Either[DecodeError, A] = {
      val caseNodes = node.child.filter(_.isInstanceOf[Elem])
      if (caseNodes.isEmpty) {
        return Left(DecodeError.ReadError(Cause.empty, s"No enum case found at $path"))
      }
      val caseNode = caseNodes.head
      val caseName = caseNode.label
      findEnumCase(`enum`, caseName, path).flatMap { c =>
        decodeNode(Seq(caseNode), c.schema, config, s"$path.$caseName").map(c.construct)
      }
    }

    private def findEnumCase[A](
      `enum`: Schema.Enum[A],
      caseName: String,
      path: String
    ): Either[DecodeError, Schema.Case[A, Any]] =
      `enum`.cases.find(_.id == caseName) match {
        case Some(c) => Right(c.asInstanceOf[Schema.Case[A, Any]])
        case None =>
          val availableCases = `enum`.cases.map(_.id).mkString(", ")
          Left(DecodeError.ReadError(Cause.empty, s"Unknown case '$caseName' at $path (expected: $availableCases)"))
      }
  }
}
