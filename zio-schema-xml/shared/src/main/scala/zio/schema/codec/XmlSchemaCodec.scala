package zio.schema.codec

import scala.collection.immutable.ListMap
import scala.util.Try
import scala.xml._

import zio.Chunk
import zio.schema.StandardType._
import zio.schema._
import zio.schema.annotation.validate
import zio.schema.validation.Validation

trait XmlSchemaCodec {
  def encode(schema: Schema[_]): Either[String, String]
  def decode(xsd: Chunk[Byte]): Either[String, Schema[_]]
}

object XmlSchemaCodec extends XmlSchemaCodec {

  final case class Config(
    targetNamespace: Option[String] = None,
    elementFormDefault: String = "qualified",
    attributeFormDefault: String = "unqualified",
    generateTypeAttributes: Boolean = false,
    formatOutput: Boolean = true
  )

  def encode(schema: Schema[_]): Either[String, String] = encode(schema, Config())

  def encode(schema: Schema[_], config: Config): Either[String, String] =
    try {
      val xsdNode = toXsd(schema, config)
      val output = if (config.formatOutput) {
        new PrettyPrinter(80, 2).format(xsdNode)
      } else {
        xsdNode.toString
      }
      Right(s"""<?xml version="1.0" encoding="UTF-8"?>\n$output""")
    } catch {
      case e: Exception => Left(s"XSD generation failed: ${e.getMessage}")
    }

  def decode(xsdBytes: Chunk[Byte]): Either[String, Schema[_]] =
    try {
      val xsdString = new String(xsdBytes.toArray, "UTF-8")
      val xsdNode   = XML.loadString(xsdString)

      if (!isSchemaRoot(xsdNode)) {
        Left("Not a schema element")
      } else if ((xsdNode \ "import").nonEmpty || (xsdNode \ "include").nonEmpty) {
        // multi-file XSD not supported
        Left("Imports/includes not supported")
      } else {
        val context = XsdContext.from(xsdNode)

        (xsdNode \ "element").headOption match {
          case None => Left("No root element")
          case Some(rootElement) =>
            val rootName = rootElement \@ "name"
            if (rootName.isEmpty) {
              Left("Root element needs a name")
            } else {
              decodeElement(rootElement, context, Set.empty)
            }
        }
      }
    } catch {
      case e: Exception => Left(s"XSD parse failed: ${e.getMessage}")
    }

  private def toXsd(schema: Schema[_], config: Config): Elem = {
    val (definitions, rootType) = collectDefinitions(schema, Set.empty)

    val namespaceAttrs = config.targetNamespace.fold(Null: MetaData)(
      ns => new UnprefixedAttribute("targetNamespace", ns, new UnprefixedAttribute("xmlns:tns", ns, Null))
    )

    val attributes = new UnprefixedAttribute(
      "elementFormDefault",
      config.elementFormDefault,
      new UnprefixedAttribute("attributeFormDefault", config.attributeFormDefault, namespaceAttrs)
    )

    val rootElement = elem(
      "xs:element",
      "name" -> getRootName(schema),
      "type" -> (if (isComplex(schema)) s"${getRootName(schema)}Type" else rootType)
    )

    Elem(
      "xs",
      "schema",
      attributes,
      NamespaceBinding("xs", "http://www.w3.org/2001/XMLSchema", TopScope),
      minimizeEmpty = true,
      (rootElement +: definitions).toSeq: _*
    )
  }

  private def collectDefinitions(schema: Schema[_], visited: Set[String]): (List[Node], String) =
    schema match {
      case r: Schema.Record[_] =>
        val name = s"${getRootName(r)}Type"
        if (visited(name)) (Nil, name)
        else {
          val (fields, nextVisited) = r.fields.foldRight((List.empty[Node], visited + name)) { (field, acc) =>
            val (defs, _) = collectDefinitions(field.schema, acc._2)
            (defs ++ acc._1, acc._2 ++ defs.collect {
              case e: Elem if (e \ "@name").text.nonEmpty => (e \ "@name").text
            })
          }

          val sequence    = elem("xs:sequence", Nil, r.fields.map(f => mkFieldElem(f)): _*)
          val complexType = elem("xs:complexType", List("name" -> name), sequence)
          (complexType :: fields, name)
        }

      case e: Schema.Enum[_] =>
        val name = s"${getRootName(e)}Type"
        if (visited(name)) (Nil, name)
        else {
          val (allCaseDefs, caseElems) = e.cases.foldLeft((List.empty[Node], List.empty[Node])) { (acc, c) =>
            val (defs, _) = collectDefinitions(c.schema, visited + name)
            val caseType = c.schema match {
              case _: Schema.Record[_] => s"${c.id}Type"
              case _: Schema.Enum[_]   => s"${c.id}Type"
              case _                   => simpleTypeName(c.schema)
            }
            val caseElem = elem("xs:element", "name" -> c.id, "type" -> caseType)
            (acc._1 ++ defs, acc._2 :+ caseElem)
          }
          val choice      = elem("xs:choice", Nil, caseElems: _*)
          val complexType = elem("xs:complexType", List("name" -> name), choice)
          (complexType :: allCaseDefs, name)
        }

      case Schema.Sequence(elemSchema, _, _, _, _) =>
        collectDefinitions(elemSchema, visited)

      case Schema.Map(k, v, _) =>
        val name = s"Map_${typeName(k)}_${typeName(v)}"
        if (visited(name)) (Nil, name)
        else {
          val (kDefs, kType) = collectDefinitions(k, visited + name)
          val (vDefs, vType) = collectDefinitions(v, visited)

          val entry = elem(
            "xs:sequence",
            Nil,
            elem("xs:element", "name" -> "key", "type"   -> kType),
            elem("xs:element", "name" -> "value", "type" -> vType)
          )
          val entryType = elem("xs:complexType", Nil, entry)
          val mapSeq = elem(
            "xs:sequence",
            Nil,
            elem("xs:element", List("name" -> "entry", "minOccurs" -> "0", "maxOccurs" -> "unbounded"), entryType)
          )
          val mapType = elem("xs:complexType", List("name" -> name), mapSeq)
          (mapType :: kDefs ::: vDefs, name)
        }

      case Schema.Optional(inner, _)       => collectDefinitions(inner, visited)
      case Schema.Lazy(inner)              => collectDefinitions(inner(), visited)
      case Schema.Transform(s, _, _, _, _) => collectDefinitions(s, visited)
      case _                               => (Nil, simpleTypeName(schema))
    }

  private def mkFieldElem(field: Schema.Field[_, _]): Elem = {
    val schema                 = field.schema.asInstanceOf[Schema[Any]]
    val (baseSchema, min, max) = unwrapCardinality(schema)
    val typeName = baseSchema match {
      case r: Schema.Record[_] => s"${getRootName(r)}Type"
      case e: Schema.Enum[_]   => s"${getRootName(e)}Type"
      case _                   => simpleTypeName(baseSchema)
    }

    elem(
      "xs:element",
      "name"      -> field.name,
      "type"      -> typeName,
      "minOccurs" -> min.toString,
      "maxOccurs" -> (if (max == Int.MaxValue) "unbounded" else max.toString)
    )
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
    case Schema.Primitive(t, _) =>
      t match {
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
    case _ => "xs:anyType"
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

  private def elem(label: String, attributes: List[(String, String)], children: Node*): Elem = {
    val metaData = attributes.foldRight(Null: MetaData) {
      case ((k, v), acc) =>
        if (v == null) acc else new UnprefixedAttribute(k, v, acc)
    }
    Elem(null, label, metaData, TopScope, minimizeEmpty = true, children: _*)
  }

  private def elem(label: String, attrs: (String, String)*): Elem =
    elem(label, attrs.toList)

  private def isSchemaRoot(node: Node): Boolean =
    node.label == "schema" && (node.prefix == "xs" || node.prefix == "xsd" || node.prefix == null)

  private case class XsdContext(
    types: Map[String, Node],
    elements: Map[String, Node],
    namespacePrefix: String
  )

  private object XsdContext {

    def from(schemaNode: Node): XsdContext = {
      val prefix = schemaNode.prefix match {
        case null | "" => "xs"
        case p         => p
      }

      val complexTypes = (schemaNode \ "complexType").map { ct =>
        (ct \@ "name") -> ct
      }.toMap

      val simpleTypes = (schemaNode \ "simpleType").map { st =>
        (st \@ "name") -> st
      }.toMap

      val elements = (schemaNode \ "element").map { elem =>
        (elem \@ "name") -> elem
      }.toMap

      XsdContext(
        types = complexTypes ++ simpleTypes,
        elements = elements,
        namespacePrefix = prefix
      )
    }
  }

  private def decodeElement(
    element: Node,
    context: XsdContext,
    visited: Set[String]
  ): Either[String, Schema[_]] = {
    val name     = element \@ "name"
    val typeName = element \@ "type"
    val ref      = element \@ "ref"

    if (ref.nonEmpty) {
      context.elements
        .get(ref)
        .toRight(s"Referenced element not found: $ref")
        .flatMap(decodeElement(_, context, visited))
    } else if (typeName.nonEmpty) {
      decodeType(typeName, context, visited)
    } else {
      (element \ "complexType").headOption
        .orElse((element \ "simpleType").headOption)
        .toRight(s"Element $name has no type information")
        .flatMap { typeNode =>
          if (typeNode.label == "complexType") decodeComplexType(name, typeNode, context, visited)
          else decodeSimpleType(name, typeNode, context, visited)
        }
    }
  }

  private def decodeType(
    typeName: String,
    context: XsdContext,
    visited: Set[String]
  ): Either[String, Schema[_]] = {
    if (isPrimitiveType(typeName, context.namespacePrefix)) return Right(primitiveTypeFromXsd(typeName))

    val cleanName = typeName.split(":").last
    if (visited(cleanName)) return Right(Schema.defer(Schema[String]))

    context.types.get(cleanName).toRight(s"Type not found: $cleanName").flatMap { typeNode =>
      typeNode.label match {
        case "complexType" => decodeComplexType(cleanName, typeNode, context, visited + cleanName)
        case "simpleType"  => decodeSimpleType(cleanName, typeNode, context, visited + cleanName)
        case _             => Left(s"Unknown type definition: ${typeNode.label}")
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
  ): Either[String, Schema[_]] =
    (typeNode \ "choice").headOption match {
      case Some(choice) => decodeEnumFromChoice(typeName, choice \ "element", context, visited)
      case None =>
        val content = (typeNode \ "sequence").headOption.orElse((typeNode \ "all").headOption)
        content match {
          case Some(c) => decodeRecordFromElements(typeName, c \ "element", context, visited)
          case None =>
            if ((typeNode \ "simpleContent").nonEmpty) Right(Schema[String])
            else decodeRecordFromElements(typeName, NodeSeq.Empty, context, visited)
        }
    }

  private def decodeRecordFromElements(
    typeName: String,
    elements: NodeSeq,
    context: XsdContext,
    visited: Set[String]
  ): Either[String, Schema[_]] = {
    if (elements.isEmpty) return Right(Schema.record(TypeId.parse(typeName)))

    val fieldsResult =
      elements.foldLeft[Either[String, Chunk[Schema.Field[ListMap[String, _], _]]]](Right(Chunk.empty)) {
        case (Right(acc), elem) =>
          val fieldName = elem \@ "name"
          val minOccurs = elem \@ "minOccurs"
          val maxOccurs = elem \@ "maxOccurs"

          val schemaOrError =
            if ((elem \@ "ref").nonEmpty) decodeElement(elem, context, visited)
            else if ((elem \@ "type").nonEmpty) decodeType(elem \@ "type", context, visited)
            else {
              (elem \ "complexType").headOption.orElse((elem \ "simpleType").headOption) match {
                case Some(t) if t.label == "complexType" => decodeComplexType(s"${fieldName}Type", t, context, visited)
                case Some(t)                             => decodeSimpleType(s"${fieldName}Type", t, context, visited)
                case None                                => Left(s"Field $fieldName has no type information")
              }
            }

          schemaOrError.map { s =>
            val finalSchema: Schema[Any] = (minOccurs, maxOccurs) match {
              case (_, "unbounded")                     => Schema.chunk(s.asInstanceOf[Schema[Any]]).asInstanceOf[Schema[Any]]
              case ("0", _) if maxOccurs != "unbounded" => s.optional.asInstanceOf[Schema[Any]]
              case _                                    => s.asInstanceOf[Schema[Any]]
            }
            acc :+ Schema.Field[ListMap[String, _], Any](
              fieldName,
              finalSchema,
              get0 = (r: ListMap[String, _]) => r.get(fieldName),
              set0 = (r: ListMap[String, _], v: Any) => r + (fieldName -> v)
            )
          }
        case (l @ Left(_), _) => l
      }

    fieldsResult.map(fields => Schema.record(TypeId.parse(typeName), fields: _*))
  }

  private def decodeEnumFromChoice(
    typeName: String,
    elements: NodeSeq,
    context: XsdContext,
    visited: Set[String]
  ): Either[String, Schema[_]] = {
    val casesResult = elements.foldLeft[Either[String, Chunk[Schema.Case[Any, _]]]](Right(Chunk.empty)) {
      case (Right(acc), elem) =>
        val caseName = elem \@ "name"
        val schemaOrError =
          if ((elem \@ "type").nonEmpty) decodeType(elem \@ "type", context, visited)
          else {
            (elem \ "complexType").headOption.orElse((elem \ "simpleType").headOption) match {
              case Some(t) if t.label == "complexType" => decodeComplexType(s"${caseName}Type", t, context, visited)
              case Some(t)                             => decodeSimpleType(s"${caseName}Type", t, context, visited)
              case None                                => Left(s"Case $caseName has no type information")
            }
          }
        schemaOrError.map { s =>
          acc :+ Schema
            .Case[Any, Any](caseName, s.asInstanceOf[Schema[Any]], _.asInstanceOf[Any], identity, (_: Any) => true)
        }
      case (l @ Left(_), _) => l
    }

    casesResult.map { cases =>
      val caseSet = cases.foldRight[CaseSet.Aux[Any]](CaseSet.Empty[Any]()) { (c, cs) =>
        CaseSet.Cons(c, cs)
      }
      Schema.enumeration[Any, CaseSet.Aux[Any]](TypeId.parse(typeName), caseSet)
    }
  }

  private def decodeSimpleType(
    typeName: String,
    typeNode: Node,
    context: XsdContext,
    visited: Set[String]
  ): Either[String, Schema[_]] =
    (typeNode \ "restriction").headOption match {
      case Some(restriction) =>
        val enumerations = restriction \ "enumeration"
        if (enumerations.nonEmpty) decodeEnumeration(typeName, enumerations)
        else {
          decodeType(restriction \@ "base", context, visited).flatMap { base =>
            parseFacets(restriction, base).map(applyValidation(base, _))
          }
        }
      case None => Left(s"Unsupported simple type: $typeName")
    }

  private def parseFacets(restriction: Node, baseSchema: Schema[_]): Either[String, Validation[Any]] = {
    var v: Validation[Any] = Validation.succeed[Any]

    // String validations
    if (baseSchema == Schema[String]) {
      (restriction \ "minLength").headOption.map(_ \@ "value").foreach { valStr =>
        Try(valStr.toInt).foreach(min => v = v && Validation.minLength(min).asInstanceOf[Validation[Any]])
      }
      (restriction \ "maxLength").headOption.map(_ \@ "value").foreach { valStr =>
        Try(valStr.toInt).foreach(max => v = v && Validation.maxLength(max).asInstanceOf[Validation[Any]])
      }
    }

    // Int validations
    if (baseSchema == Schema[Int]) {
      (restriction \ "minInclusive").headOption.map(_ \@ "value").foreach { valStr =>
        Try(valStr.toInt).foreach(min => v = v && Validation.greaterThan(min - 1).asInstanceOf[Validation[Any]])
      }
      (restriction \ "maxInclusive").headOption.map(_ \@ "value").foreach { valStr =>
        Try(valStr.toInt).foreach(max => v = v && Validation.lessThan(max + 1).asInstanceOf[Validation[Any]])
      }
    }
    Right(v)
  }

  private def applyValidation(schema: Schema[_], validation: Validation[Any]): Schema[_] =
    if (validation == Validation.succeed) schema else schema.asInstanceOf[Schema[Any]].annotate(validate(validation))

  private def decodeEnumeration(typeName: String, enumerations: NodeSeq): Either[String, Schema[_]] = {
    val cases = enumerations.map { n =>
      val v = n \@ "value"
      Schema.Case[String, String](v, Schema[String], _.asInstanceOf[String], identity, (s: String) => s == v)
    }.toVector
    val caseChunk = Chunk.fromIterable(cases)

    val caseSet = caseChunk.foldRight[CaseSet.Aux[String]](CaseSet.Empty[String]()) { (c, cs) =>
      CaseSet.Cons(c, cs)
    }
    Right(Schema.enumeration[String, CaseSet.Aux[String]](TypeId.parse(typeName), caseSet))
  }
}
