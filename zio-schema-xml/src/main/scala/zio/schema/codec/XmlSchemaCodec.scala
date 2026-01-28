package zio.schema.codec

import scala.collection.immutable.ListMap
import scala.util.Try
import scala.xml._

import zio.schema.StandardType._
import zio.schema._
import zio.schema.annotation.validate
import zio.schema.codec.XmlAnnotations
import zio.schema.validation.Validation
import zio.{ Cause, Chunk }

trait XmlSchemaCodec {
  def encode(schema: Schema[_]): Either[DecodeError, String]
  def decode(xsd: Chunk[Byte]): Either[DecodeError, Schema[_]]
}

object XmlSchemaCodec extends XmlSchemaCodec {

  final case class Config(
    targetNamespace: Option[String] = None,
    elementFormDefault: String = "qualified",
    attributeFormDefault: String = "unqualified",
    generateTypeAttributes: Boolean = false,
    formatOutput: Boolean = true
  )

  def encode(schema: Schema[_]): Either[DecodeError, String] = encode(schema, Config())

  def encode(schema: Schema[_], config: Config): Either[DecodeError, String] =
    try {
      val xsdNode = buildSchemaRoot(schema, config)
      val output  = formatSchemaOutput(xsdNode, config)
      Right(s"""<?xml version="1.0" encoding="UTF-8"?>\n$output""")
    } catch {
      case e: Exception => Left(DecodeError.ReadError(Cause.fail(e), s"Failed to generate XSD: ${e.getMessage}"))
    }

  private def buildSchemaRoot(schema: Schema[_], config: Config): Elem = {
    val (definitions, rootType) = collectDefinitions(schema, Set.empty)
    val rootName                = getRootName(schema)
    val rootElement             = buildRootElement(rootName, rootType, schema)
    val attributes              = buildSchemaAttributes(config)
    Elem(
      "xs",
      "schema",
      attributes,
      NamespaceBinding("xs", "http://www.w3.org/2001/XMLSchema", TopScope),
      minimizeEmpty = true,
      (rootElement +: definitions).toSeq: _*
    )
  }

  private def buildSchemaAttributes(config: Config): MetaData = {
    val namespaceAttrs = config.targetNamespace.fold(Null: MetaData)(
      ns => new UnprefixedAttribute("targetNamespace", ns, new UnprefixedAttribute("xmlns:tns", ns, Null))
    )
    new UnprefixedAttribute(
      "elementFormDefault",
      config.elementFormDefault,
      new UnprefixedAttribute("attributeFormDefault", config.attributeFormDefault, namespaceAttrs)
    )
  }

  private def buildRootElement(rootName: String, rootType: String, schema: Schema[_]): Elem = {
    val typeName = if (isComplex(schema)) s"${rootName}Type" else rootType
    elem("xs:element", "name" -> rootName, "type" -> typeName)
  }

  private def formatSchemaOutput(xsdNode: Elem, config: Config): String =
    if (config.formatOutput) {
      new PrettyPrinter(80, 2).format(xsdNode)
    } else {
      xsdNode.toString
    }

  def decode(xsdBytes: Chunk[Byte]): Either[DecodeError, Schema[_]] =
    try {
      val xsdString = new String(xsdBytes.toArray, "UTF-8")
      val xsdNode   = XML.loadString(xsdString)
      validateAndDecodeSchema(xsdNode)
    } catch {
      case e: Exception => Left(DecodeError.ReadError(Cause.fail(e), s"Unable to parse XSD: ${e.getMessage}"))
    }

  private def validateAndDecodeSchema(xsdNode: Node): Either[DecodeError, Schema[_]] =
    if (!isSchemaRoot(xsdNode)) {
      Left(DecodeError.ReadError(Cause.empty, "Root element must be xs:schema or xsd:schema"))
    } else if ((xsdNode \ "import").nonEmpty || (xsdNode \ "include").nonEmpty) {
      Left(DecodeError.ReadError(Cause.empty, "XSD imports and includes are not currently supported"))
    } else {
      val context = XsdContext.from(xsdNode)
      (xsdNode \ "element").headOption match {
        case None => Left(DecodeError.ReadError(Cause.empty, "Schema must contain at least one root element"))
        case Some(rootElement) =>
          val rootName = rootElement \@ "name"
          if (rootName.isEmpty) {
            Left(DecodeError.ReadError(Cause.empty, "Root element requires a name attribute"))
          } else {
            decodeElement(rootElement, context, Set.empty)
          }
      }
    }

  // The visited set prevents infinite loops on circular schema references
  private def collectDefinitions(schema: Schema[_], visited: Set[String]): (List[Node], String) =
    schema match {
      case r: Schema.Record[_] => collectRecordDefinitions(r, visited)
      case e: Schema.Enum[_]   => collectEnumDefinitions(e, visited)
      case Schema.Sequence(elemSchema, _, _, _, _) =>
        collectDefinitions(elemSchema, visited)
      case Schema.Map(k, v, _) =>
        collectMapDefinitions(k, v, visited)
      case Schema.Optional(inner, _)       => collectDefinitions(inner, visited)
      case Schema.Lazy(inner)              => collectDefinitions(inner(), visited)
      case Schema.Transform(s, _, _, _, _) => collectDefinitions(s, visited)
      case _                               => (Nil, simpleTypeName(schema))
    }

  private def collectRecordDefinitions(record: Schema.Record[_], visited: Set[String]): (List[Node], String) = {
    val name = s"${getRootName(record)}Type"
    if (visited(name)) {
      (Nil, name)
    } else {
      val (fieldDefs, _) = record.fields.foldRight((List.empty[Node], visited + name)) { (field, acc) =>
        val (defs, _) = collectDefinitions(field.schema, acc._2)
        (defs ++ acc._1, acc._2 ++ defs.collect {
          case e: Elem if (e \ "@name").text.nonEmpty => (e \ "@name").text
        })
      }

      // Separate attributes from elements
      val fieldElems             = record.fields.flatMap(mkFieldElem)
      val (attributes, elements) = fieldElems.partition(_.label == "attribute")

      val children = if (elements.nonEmpty) {
        val sequence = elem("xs:sequence", Nil, elements: _*)
        sequence +: attributes
      } else if (attributes.nonEmpty) {
        attributes
      } else {
        Seq(elem("xs:sequence", Nil))
      }

      val complexType = elem("xs:complexType", List("name" -> name), children: _*)
      (complexType :: fieldDefs, name)
    }
  }

  private def collectEnumDefinitions(enum: Schema.Enum[_], visited: Set[String]): (List[Node], String) = {
    val name = s"${getRootName(enum)}Type"
    if (visited(name)) {
      (Nil, name)
    } else {
      // Check if this is a simple string enumeration
      val isSimpleEnum = enum.cases.forall { c =>
        c.schema match {
          case Schema.Primitive(_, _)                  => true
          case r: Schema.Record[_] if r.fields.isEmpty => true
          case _                                       => false
        }
      }

      if (isSimpleEnum && enum.cases.nonEmpty) {
        val enumerationNodes = enum.cases.map { c =>
          elem("xs:enumeration", "value" -> c.id)
        }
        val restriction = elem(
          "xs:restriction",
          List("base" -> "xs:string"),
          enumerationNodes.toSeq: _*
        )
        val simpleType = elem("xs:simpleType", List("name" -> name), restriction)
        (List(simpleType), name)
      } else {
        val (allCaseDefs, caseElems) = enum.cases.foldLeft((List.empty[Node], List.empty[Node])) { (acc, c) =>
          val (defs, _) = collectDefinitions(c.schema, visited + name)
          val caseType  = schemaTypeName(c.schema, c.id)
          val caseElem  = elem("xs:element", "name" -> c.id, "type" -> caseType)
          (acc._1 ++ defs, acc._2 :+ caseElem)
        }
        val choice      = elem("xs:choice", Nil, caseElems: _*)
        val complexType = elem("xs:complexType", List("name" -> name), choice)
        (complexType :: allCaseDefs, name)
      }
    }
  }

  private def schemaTypeName(schema: Schema[_], fallbackName: String): String =
    schema match {
      case r: Schema.Record[_] => s"${getRootName(r)}Type"
      case e: Schema.Enum[_]   => s"${getRootName(e)}Type"
      case _                   => simpleTypeName(schema)
    }

  private def collectMapDefinitions(
    keySchema: Schema[Any],
    valueSchema: Schema[Any],
    visited: Set[String]
  ): (List[Node], String) = {
    val name = s"Map_${typeName(keySchema)}_${typeName(valueSchema)}"
    if (visited(name)) {
      (Nil, name)
    } else {
      val (kDefs, kType) = collectDefinitions(keySchema, visited + name)
      val (vDefs, vType) = collectDefinitions(valueSchema, visited)
      val entry          = buildMapEntry(kType, vType)
      val mapSeq         = buildMapSequence(entry)
      val mapType        = elem("xs:complexType", List("name" -> name), mapSeq)
      (mapType :: kDefs ::: vDefs, name)
    }
  }

  private def buildMapEntry(keyType: String, valueType: String): Node = {
    val entrySeq = elem(
      "xs:sequence",
      Nil,
      elem("xs:element", "name" -> "key", "type"   -> keyType),
      elem("xs:element", "name" -> "value", "type" -> valueType)
    )
    elem("xs:complexType", Nil, entrySeq)
  }

  private def buildMapSequence(entryType: Node): Node =
    elem(
      "xs:sequence",
      Nil,
      elem("xs:element", List("name" -> "entry", "minOccurs" -> "0", "maxOccurs" -> "unbounded"), entryType)
    )

  private def mkFieldElem(field: Schema.Field[_, _]): Option[Elem] = {
    val (baseSchema, min, max) = unwrapCardinality(field.schema)
    val typeName               = schemaTypeName(baseSchema, field.name)

    val isText = field.annotations.exists {
      case _: XmlAnnotations.text => true
      case _                      => false
    }

    if (isText) {
      None
    } else {
      val isAttribute = field.annotations.exists {
        case _: XmlAnnotations.attribute => true
        case _                           => false
      }

      if (isAttribute) {
        Some(
          elem(
            "xs:attribute",
            "name" -> field.name,
            "type" -> typeName,
            "use"  -> (if (min == 0) "optional" else "required")
          )
        )
      } else {
        Some(
          elem(
            "xs:element",
            "name"      -> field.name,
            "type"      -> typeName,
            "minOccurs" -> min.toString,
            "maxOccurs" -> (if (max == Int.MaxValue) "unbounded" else max.toString)
          )
        )
      }
    }
  }

  private def unwrapCardinality(schema: Schema[_]): (Schema[_], Int, Int) = schema match {
    case Schema.Optional(inner, _) =>
      val (s, _, max) = unwrapCardinality(inner)
      (s, 0, max)
    case Schema.Sequence(inner, _, _, _, _) =>
      val (s, _, _) = unwrapCardinality(inner)
      (s, 0, Int.MaxValue)
    case Schema.Set(inner, _) =>
      val (s, _, _) = unwrapCardinality(inner)
      (s, 0, Int.MaxValue)
    case Schema.Lazy(inner)                  => unwrapCardinality(inner())
    case Schema.Transform(inner, _, _, _, _) => unwrapCardinality(inner)
    case other                               => (other, 1, 1)
  }

  private def simpleTypeName(schema: Schema[_]): String = schema match {
    case Schema.Primitive(t, _) => primitiveToXsdType(t)
    case _                      => "xs:anyType"
  }

  private def primitiveToXsdType(stdType: StandardType[_]): String =
    stdType match {
      case StringType | UUIDType                                => "xs:string"
      case IntType                                              => "xs:int"
      case LongType                                             => "xs:long"
      case FloatType                                            => "xs:float"
      case DoubleType                                           => "xs:double"
      case BoolType                                             => "xs:boolean"
      case ByteType                                             => "xs:byte"
      case ShortType                                            => "xs:short"
      case BinaryType                                           => "xs:base64Binary"
      case LocalDateType                                        => "xs:date"
      case InstantType | ZonedDateTimeType | OffsetDateTimeType => "xs:dateTime"
      case _                                                    => "xs:string"
    }

  private def typeName(schema: Schema[_]): String =
    simpleTypeName(schema).stripPrefix("xs:")

  private def getRootName(schema: Schema[_]): String = schema match {
    case r: Schema.Record[_] =>
      r.id match {
        case TypeId.Nominal(_, _, name) => name
        case TypeId.Structural          => "Record"
      }
    case e: Schema.Enum[_] =>
      e.id match {
        case TypeId.Nominal(_, _, name) => name
        case TypeId.Structural          => "Enum"
      }
    case _ => "root"
  }

  private def isComplex(schema: Schema[_]): Boolean = schema match {
    case _: Schema.Record[_] | _: Schema.Enum[_] | _: Schema.Map[_, _] => true
    case Schema.Lazy(inner)                                            => isComplex(inner())
    case Schema.Transform(inner, _, _, _, _)                           => isComplex(inner)
    case _                                                             => false
  }

  // Null checks prevent empty attributes which can cause XSD validation issues
  private def elem(label: String, attributes: List[(String, String)], children: Node*): Elem = {
    val metaData = attributes.foldRight(Null: MetaData) {
      case ((k, v), acc) => if (v == null) acc else new UnprefixedAttribute(k, v, acc)
    }
    Elem(null, label, metaData, TopScope, minimizeEmpty = true, children: _*)
  }

  private def elem(label: String, attrs: (String, String)*): Elem =
    elem(label, attrs.toList)

  private def isSchemaRoot(node: Node): Boolean =
    node.label == "schema" && (node.prefix == "xs" || node.prefix == "xsd" || node.prefix == null)

  // Hold parsed XSD definitions to avoid multiple tree traversals during decoding
  private case class XsdContext(
    types: Map[String, Node],
    elements: Map[String, Node],
    namespacePrefix: String
  )

  private object XsdContext {

    def from(schemaNode: Node): XsdContext = {
      val prefix       = extractPrefix(schemaNode)
      val complexTypes = collectNodesByName(schemaNode \ "complexType")
      val simpleTypes  = collectNodesByName(schemaNode \ "simpleType")
      val elements     = collectNodesByName(schemaNode \ "element")

      XsdContext(
        types = complexTypes ++ simpleTypes,
        elements = elements,
        namespacePrefix = prefix
      )
    }

    private def extractPrefix(node: Node): String =
      node.prefix match {
        case null | "" => "xs"
        case p         => p
      }

    private def collectNodesByName(nodes: NodeSeq): Map[String, Node] =
      nodes.map(n => (n \@ "name") -> n).toMap
  }

  private def decodeElement(
    element: Node,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[_]] = {
    val ref      = element \@ "ref"
    val typeName = element \@ "type"

    if (ref.nonEmpty) {
      resolveElementRef(ref, context, visited)
    } else if (typeName.nonEmpty) {
      decodeType(typeName, context, visited)
    } else {
      decodeInlineType(element, context, visited)
    }
  }

  private def resolveElementRef(
    ref: String,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[_]] =
    context.elements
      .get(ref)
      .toRight(DecodeError.ReadError(Cause.empty, s"Cannot find referenced element: $ref"))
      .flatMap(decodeElement(_, context, visited))

  private def decodeInlineType(
    element: Node,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[_]] = {
    val name = element \@ "name"
    (element \ "complexType").headOption
      .orElse((element \ "simpleType").headOption)
      .toRight(DecodeError.ReadError(Cause.empty, s"Element '$name' lacks type definition"))
      .flatMap { typeNode =>
        typeNode.label match {
          case "complexType" => decodeComplexType(name, typeNode, context, visited)
          case _             => decodeSimpleType(name, typeNode, context, visited)
        }
      }
  }

  private def decodeType(
    typeName: String,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[_]] = {
    if (isPrimitiveType(typeName, context.namespacePrefix)) {
      return Right(primitiveTypeFromXsd(typeName))
    }
    val cleanName = typeName.split(":").last
    if (visited(cleanName)) {
      return Right(Schema.defer(Schema[String]))
    }
    context.types
      .get(cleanName)
      .toRight(DecodeError.ReadError(Cause.empty, s"Undefined type: $cleanName"))
      .flatMap { typeNode =>
        typeNode.label match {
          case "complexType" => decodeComplexType(cleanName, typeNode, context, visited + cleanName)
          case "simpleType"  => decodeSimpleType(cleanName, typeNode, context, visited + cleanName)
          case _             => Left(DecodeError.ReadError(Cause.empty, s"Unrecognized type definition label: ${typeNode.label}"))
        }
      }
  }

  private def isPrimitiveType(typeName: String, prefix: String): Boolean =
    typeName.startsWith(s"$prefix:") || typeName.startsWith("xs:") || typeName.startsWith("xsd:")

  private def primitiveTypeFromXsd(typeName: String): Schema[_] =
    typeName.split(":").last match {
      case "string"                     => Schema[String]
      case "boolean"                    => Schema[Boolean]
      case "short"                      => Schema[Short]
      case "int" | "integer"            => Schema[Int]
      case "long"                       => Schema[Long]
      case "float"                      => Schema[Float]
      case "double"                     => Schema[Double]
      case "byte"                       => Schema[Byte]
      case "base64Binary" | "hexBinary" => Schema[Chunk[Byte]]
      case "decimal"                    => Schema[java.math.BigDecimal]
      case "duration"                   => Schema[java.time.Duration]
      case "dateTime"                   => Schema[java.time.Instant]
      case "date"                       => Schema[java.time.LocalDate]
      case "time"                       => Schema[java.time.LocalTime]
      case _                            => Schema[String]
    }

  private def decodeComplexType(
    typeName: String,
    typeNode: Node,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[_]] =
    (typeNode \ "choice").headOption match {
      case Some(choice) => decodeEnumFromChoice(typeName, choice \ "element", context, visited)
      case None         => decodeRecordOrSimpleContent(typeName, typeNode, context, visited)
    }

  private def decodeRecordOrSimpleContent(
    typeName: String,
    typeNode: Node,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[_]] = {
    val content = (typeNode \ "sequence").headOption.orElse((typeNode \ "all").headOption)
    content match {
      case Some(c) => decodeRecordFromElements(typeName, c \ "element", context, visited)
      case None =>
        if ((typeNode \ "simpleContent").nonEmpty) {
          Right(Schema[String])
        } else {
          decodeRecordFromElements(typeName, NodeSeq.Empty, context, visited)
        }
    }
  }

  private def decodeRecordFromElements(
    typeName: String,
    elements: NodeSeq,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[_]] = {
    if (elements.isEmpty) {
      return Right(Schema.record(TypeId.parse(typeName)))
    }
    collectFields(elements, context, visited).map { fields =>
      Schema.record(TypeId.parse(typeName), fields: _*)
    }
  }

  private def collectFields(
    elements: NodeSeq,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Chunk[Schema.Field[ListMap[String, _], _]]] =
    elements.foldLeft[Either[DecodeError, Chunk[Schema.Field[ListMap[String, _], _]]]](Right(Chunk.empty)) {
      case (Right(acc), elem) =>
        decodeFieldElement(elem, context, visited).map(acc :+ _)
      case (left @ Left(_), _) =>
        left
    }

  private def decodeFieldElement(
    elem: Node,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema.Field[ListMap[String, _], _]] = {
    val fieldName = elem \@ "name"
    val minOccurs = elem \@ "minOccurs"
    val maxOccurs = elem \@ "maxOccurs"

    resolveFieldSchema(elem, fieldName, context, visited).map { schema =>
      val finalSchema = applyCardinality(schema, minOccurs, maxOccurs)
      Schema.Field[ListMap[String, _], Any](
        fieldName,
        finalSchema,
        get0 = (r: ListMap[String, _]) => r.get(fieldName),
        set0 = (r: ListMap[String, _], v: Any) => r + (fieldName -> v)
      )
    }
  }

  private def resolveFieldSchema(
    elem: Node,
    fieldName: String,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[Any]] =
    if ((elem \@ "ref").nonEmpty) {
      decodeElement(elem, context, visited).map(_.asInstanceOf[Schema[Any]])
    } else if ((elem \@ "type").nonEmpty) {
      decodeType(elem \@ "type", context, visited).map(_.asInstanceOf[Schema[Any]])
    } else {
      decodeInlineFieldType(elem, fieldName, context, visited)
    }

  private def decodeInlineFieldType(
    elem: Node,
    fieldName: String,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[Any]] =
    (elem \ "complexType").headOption.orElse((elem \ "simpleType").headOption) match {
      case Some(t) if t.label == "complexType" =>
        decodeComplexType(s"${fieldName}Type", t, context, visited).map(_.asInstanceOf[Schema[Any]])
      case Some(t) =>
        decodeSimpleType(s"${fieldName}Type", t, context, visited).map(_.asInstanceOf[Schema[Any]])
      case None =>
        Left(DecodeError.ReadError(Cause.empty, s"Field '$fieldName' is missing type information"))
    }

  // Map XSD cardinality to Schema wrappers
  private def applyCardinality(schema: Schema[Any], minOccurs: String, maxOccurs: String): Schema[Any] =
    (minOccurs, maxOccurs) match {
      case (_, "unbounded")                     => Schema.chunk(schema).asInstanceOf[Schema[Any]]
      case ("0", _) if maxOccurs != "unbounded" => schema.optional.asInstanceOf[Schema[Any]]
      case _                                    => schema
    }

  private def decodeEnumFromChoice(
    typeName: String,
    elements: NodeSeq,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[_]] =
    collectEnumCases(elements, context, visited).map { cases =>
      val caseSet = cases.foldRight[CaseSet.Aux[Any]](CaseSet.Empty[Any]()) { (c, cs) =>
        CaseSet.Cons(c, cs)
      }
      Schema.enumeration[Any, CaseSet.Aux[Any]](TypeId.parse(typeName), caseSet)
    }

  private def collectEnumCases(
    elements: NodeSeq,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Chunk[Schema.Case[Any, _]]] =
    elements.foldLeft[Either[DecodeError, Chunk[Schema.Case[Any, _]]]](Right(Chunk.empty)) {
      case (Right(acc), elem) =>
        decodeCaseElement(elem, context, visited).map(acc :+ _)
      case (left @ Left(_), _) =>
        left
    }

  private def decodeCaseElement(
    elem: Node,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema.Case[Any, _]] = {
    val caseName = elem \@ "name"
    resolveCaseSchema(elem, caseName, context, visited).map { schema =>
      Schema.Case[Any, Any](
        caseName,
        schema.asInstanceOf[Schema[Any]],
        _.asInstanceOf[Any],
        identity,
        (_: Any) => true
      )
    }
  }

  private def resolveCaseSchema(
    elem: Node,
    caseName: String,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[_]] =
    if ((elem \@ "type").nonEmpty) {
      decodeType(elem \@ "type", context, visited)
    } else {
      (elem \ "complexType").headOption.orElse((elem \ "simpleType").headOption) match {
        case Some(t) if t.label == "complexType" =>
          decodeComplexType(s"${caseName}Type", t, context, visited)
        case Some(t) =>
          decodeSimpleType(s"${caseName}Type", t, context, visited)
        case None =>
          Left(DecodeError.ReadError(Cause.empty, s"Enum case '$caseName' requires a type definition"))
      }
    }

  private def decodeSimpleType(
    typeName: String,
    typeNode: Node,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[_]] =
    (typeNode \ "restriction").headOption match {
      case Some(restriction) =>
        val enumerations = restriction \ "enumeration"
        if (enumerations.nonEmpty) {
          decodeEnumeration(typeName, enumerations)
        } else {
          decodeRestriction(restriction, context, visited)
        }
      case None =>
        Left(DecodeError.ReadError(Cause.empty, s"Simple type '$typeName' must have a restriction"))
    }

  private def decodeRestriction(
    restriction: Node,
    context: XsdContext,
    visited: Set[String]
  ): Either[DecodeError, Schema[_]] =
    decodeType(restriction \@ "base", context, visited).flatMap { base =>
      parseFacets(restriction, base).map(applyValidation(base, _))
    }

  // Handles facets
  private def parseFacets(restriction: Node, baseSchema: Schema[_]): Either[DecodeError, Validation[Any]] = {
    var validation: Validation[Any] = Validation.succeed[Any]

    if (baseSchema == Schema[String]) {
      validation = parseStringFacets(restriction, validation)
    }
    if (baseSchema == Schema[Int]) {
      validation = parseIntFacets(restriction, validation)
    }
    Right(validation)
  }

  private def parseStringFacets(restriction: Node, initial: Validation[Any]): Validation[Any] = {
    var v = initial
    (restriction \ "minLength").headOption.map(_ \@ "value").foreach { valStr =>
      Try(valStr.toInt).foreach(min => v = v && Validation.minLength(min).asInstanceOf[Validation[Any]])
    }
    (restriction \ "maxLength").headOption.map(_ \@ "value").foreach { valStr =>
      Try(valStr.toInt).foreach(max => v = v && Validation.maxLength(max).asInstanceOf[Validation[Any]])
    }
    v
  }

  private def parseIntFacets(restriction: Node, initial: Validation[Any]): Validation[Any] = {
    var v = initial
    (restriction \ "minInclusive").headOption.map(_ \@ "value").foreach { valStr =>
      Try(valStr.toInt).foreach(min => v = v && Validation.greaterThan(min - 1).asInstanceOf[Validation[Any]])
    }
    (restriction \ "maxInclusive").headOption.map(_ \@ "value").foreach { valStr =>
      Try(valStr.toInt).foreach(max => v = v && Validation.lessThan(max + 1).asInstanceOf[Validation[Any]])
    }
    v
  }

  private def applyValidation(schema: Schema[_], validation: Validation[Any]): Schema[_] =
    if (validation == Validation.succeed) schema else schema.asInstanceOf[Schema[Any]].annotate(validate(validation))

  private def decodeEnumeration(typeName: String, enumerations: NodeSeq): Either[DecodeError, Schema[_]] = {
    val cases = enumerations.map { n =>
      val v = n \@ "value"
      Schema.Case[String, String](v, Schema[String], _.asInstanceOf[String], identity, (s: String) => s == v)
    }.to(Chunk)

    val caseSet = cases.foldRight[CaseSet.Aux[String]](CaseSet.Empty[String]()) { (c, cs) =>
      CaseSet.Cons(c, cs)
    }
    Right(Schema.enumeration[String, CaseSet.Aux[String]](TypeId.parse(typeName), caseSet))
  }
}
