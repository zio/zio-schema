package zio.schema

import scala.collection.immutable.ListMap

import zio.{ Chunk, ChunkBuilder }

sealed trait SchemaAst { self =>
  def id: Int
  def lineage: Chunk[Int]
  def optional: Boolean
  def dimensions: Int

  def toSchema: Schema[_] = SchemaAst.materialize(self)

  override def toString: String = AstRenderer.render(self)
}

object SchemaAst {
  type Labelled = (String, SchemaAst)

  final case object Root extends SchemaAst {
    def id: Int             = 0
    def lineage: Chunk[Int] = Chunk.empty
    def optional: Boolean   = false
    def dimensions: Int     = 0
  }
  implicit val rootSchema: Schema[Root.type] = Schema[Unit].transform(_ => Root, _ => ())

  final case class Product(
    override val id: Int,
    override val lineage: Chunk[Int],
    fields: Chunk[Labelled] = Chunk.empty,
    override val optional: Boolean = false,
    override val dimensions: Int = 0
  ) extends SchemaAst

  object Product {
    implicit val schema: Schema[Product] = {
      Schema.CaseClass4(
        annotations = Chunk.empty,
        field1 = Schema.Field("id", Schema[Int]),
        field2 = Schema.Field("fields", Schema[Labelled].repeated),
        field3 = Schema.Field("optional", Schema[Boolean]),
        field4 = Schema.Field("dimensions", Schema[Int]),
        (id: Int, fields: Chunk[Labelled], optional: Boolean, dimensions: Int) =>
          Product(id, Chunk.empty, fields, optional, dimensions),
        _.id,
        _.fields,
        _.optional,
        _.dimensions
      )
    }
  }
  final case class Sum(
    override val id: Int,
    override val lineage: Chunk[Int],
    cases: Chunk[Labelled] = Chunk.empty,
    override val optional: Boolean = false,
    override val dimensions: Int = 0
  ) extends SchemaAst

  object Sum {
    implicit val schema: Schema[Sum] =
      Schema.CaseClass4(
        annotations = Chunk.empty,
        field1 = Schema.Field("id", Schema[Int]),
        field2 = Schema.Field("cases", Schema[Labelled].repeated),
        field3 = Schema.Field("optional", Schema[Boolean]),
        field4 = Schema.Field("dimensions", Schema[Int]),
        (id: Int, fields: Chunk[Labelled], optional: Boolean, dimensions: Int) =>
          Sum(id, Chunk.empty, fields, optional, dimensions),
        _.id,
        _.cases,
        _.optional,
        _.dimensions
      )
  }
  final case class FailNode(message: String, override val optional: Boolean = false, override val dimensions: Int = 0)
      extends SchemaAst {
    def id: Int             = 0
    def lineage: Chunk[Int] = Chunk.empty
  }

  object FailNode {
    implicit val schema: Schema[FailNode] = Schema.CaseClass3(
      annotations = Chunk.empty,
      field1 = Schema.Field("message", Schema[String]),
      field2 = Schema.Field("optional", Schema[Boolean]),
      field3 = Schema.Field("dimensions", Schema[Int]),
      (m: String, optional: Boolean, dimensions: Int) => FailNode(m, optional, dimensions),
      _.message,
      _.optional,
      _.dimensions
    )
  }
  final case class Value(
    valueType: StandardType[_],
    override val optional: Boolean = false,
    override val dimensions: Int = 0
  ) extends SchemaAst {
    def id: Int             = valueType.hashCode()
    def lineage: Chunk[Int] = Chunk.empty
  }

  object Value {
    implicit val schema: Schema[Value] =
      Schema
        .CaseClass3[String, Boolean, Int, (String, Boolean, Int)](
          annotations = Chunk.empty,
          field1 = Schema.Field("valueType", Schema[String]),
          field2 = Schema.Field("optional", Schema[Boolean]),
          field3 = Schema.Field("dimensions", Schema[Int]),
          construct = (v, o, d) => (v, o, d),
          extractField1 = _._1,
          extractField2 = _._2,
          extractField3 = _._3
        )
        .transformOrFail(fromTuple, tupled)

    private def tupled(value: Value): Either[String, (String, Boolean, Int)] =
      Right((value.valueType.tag, value.optional, value.dimensions))

    private def fromTuple(tuple: (String, Boolean, Int)): Either[String, Value] = tuple match {
      case (s, optional, dimensions) =>
        StandardType.fromString(s).map(typ => Value(typ, optional, dimensions)).toRight(s"unkown standard type $s")
    }
  }
  final case class Ref(
    refId: Int,
    override val lineage: Chunk[Int],
    optional: Boolean = false,
    dimensions: Int = 0
  ) extends SchemaAst {
    def id: Int = refId
  }

  object Ref {
    implicit val schema: Schema[Ref] =
      Schema.CaseClass3(
        annotations = Chunk.empty,
        field1 = Schema.Field("refId", Schema[Int]),
        field2 = Schema.Field("optional", Schema[Boolean]),
        field3 = Schema.Field("dimensions", Schema[Int]),
        (id: Int, optional: Boolean, dimensions: Int) => Ref(id, Chunk.empty, optional, dimensions),
        _.id,
        _.optional,
        _.dimensions
      )
  }

  final private[schema] case class NodeBuilder(
    id: Int,
    lineage: Chunk[Int],
    optional: Boolean = false,
    dimensions: Int = 0
  ) { self =>
    private val children: ChunkBuilder[Labelled] = ChunkBuilder.make[Labelled]()

    def addLabelledSubtree(label: String, schema: Schema[_]): NodeBuilder = {
      children += (label -> subtree(schema, lineage :+ id))
      self
    }

    def buildProduct(): Product = Product(id, lineage, children.result(), optional, dimensions)

    def buildSum(): Sum = Sum(id, lineage, children.result(), optional, dimensions)
  }

  def fromSchema[A](schema: Schema[A]): SchemaAst = schema match {
    case Schema.Primitive(typ)   => Value(typ)
    case Schema.Fail(message)    => FailNode(message)
    case Schema.Optional(schema) => subtree(schema, Chunk.empty, optional = true)
    case s @ Schema.EitherSchema(left, right) =>
      NodeBuilder(s.hashCode(), Chunk.empty)
        .addLabelledSubtree("left", left)
        .addLabelledSubtree("right", right)
        .buildSum()
    case s @ Schema.Tuple(left, right) =>
      NodeBuilder(s.hashCode(), Chunk.empty)
        .addLabelledSubtree("left", left)
        .addLabelledSubtree("right", right)
        .buildProduct()
    case Schema.Sequence(schema, _, _) =>
      subtree(schema, Chunk.empty, dimensions = 1)
    case Schema.Transform(schema, _, _) => subtree(schema, Chunk.empty)
    case lzy @ Schema.Lazy(_)           => fromSchema(lzy.schema)
    case s: Schema.Record[A] =>
      s.structure
        .foldLeft(NodeBuilder(s.hashCode(), Chunk.empty)) { (node, field) =>
          node.addLabelledSubtree(field.label, field.schema)
        }
        .buildProduct()
    case s: Schema.Enum[A] =>
      s.structure
        .foldLeft(NodeBuilder(s.hashCode(), Chunk.empty)) {
          case (node, (id, schema)) =>
            node.addLabelledSubtree(id, schema)
        }
        .buildSum()
    case Schema.Meta(ast) => ast
  }

  private[schema] def subtree(
    schema: Schema[_],
    lineage: Chunk[Int],
    optional: Boolean = false,
    dimensions: Int = 0
  ): SchemaAst =
    lineage
      .find(_ == schema.hashCode())
      .map { refId =>
        Ref(refId, lineage, optional, dimensions)
      }
      .getOrElse {
        schema match {
          case Schema.Primitive(typ)   => Value(typ, optional, dimensions)
          case Schema.Optional(schema) => subtree(schema, lineage, optional = true, dimensions)
          case s @ Schema.EitherSchema(left, right) =>
            NodeBuilder(s.hashCode(), lineage, optional, dimensions)
              .addLabelledSubtree("left", left)
              .addLabelledSubtree("right", right)
              .buildSum()
          case s @ Schema.Tuple(left, right) =>
            NodeBuilder(s.hashCode(), lineage, optional, dimensions)
              .addLabelledSubtree("left", left)
              .addLabelledSubtree("right", right)
              .buildProduct()
          case Schema.Sequence(schema, _, _) =>
            subtree(schema, lineage, optional, dimensions + 1)
          case Schema.Transform(schema, _, _) => subtree(schema, lineage, optional, dimensions)
          case lzy @ Schema.Lazy(_)           => subtree(lzy.schema, lineage, optional, dimensions)
          case s: Schema.Record[_] =>
            s.structure
              .foldLeft(NodeBuilder(s.hashCode(), lineage, optional, dimensions)) { (node, field) =>
                node.addLabelledSubtree(field.label, field.schema)
              }
              .buildProduct()
          case s: Schema.Enum[_] =>
            s.structure
              .foldLeft(NodeBuilder(s.hashCode(), lineage, optional, dimensions)) {
                case (node, (id, schema)) =>
                  node.addLabelledSubtree(id, schema)
              }
              .buildSum()
          case Schema.Fail(message) => FailNode(message)
          case Schema.Meta(ast)     => ast
        }
      }

  private[schema] def materialize(ast: SchemaAst, refs: Map[Int, SchemaAst] = Map.empty): Schema[_] = {
    val baseSchema = ast match {
      case SchemaAst.Value(typ, _, _) =>
        Schema.Primitive(typ)
        Schema.Primitive(typ)
      case SchemaAst.FailNode(msg, _, _) => Schema.Fail(msg)
      case SchemaAst.Ref(id, _, _, _) =>
        refs
          .get(id)
          .map(astRef => Schema.defer(materialize(astRef, Map.empty)))
          .getOrElse(Schema.Fail(s"invalid ref $id"))
      case n @ SchemaAst.Product(id, _, elems, _, _) =>
        Schema.GenericRecord(
          elems.map {
            case (label, ast) =>
              Schema.Field(label, materialize(ast, refs + (id -> n)))
          }
        )
      case n @ SchemaAst.Sum(id, _, elems, _, _) =>
        Schema.Enumeration(
          ListMap.empty ++ elems.map {
            case (label, ast) =>
              (label, materialize(ast, refs + (id -> n)))
          }
        )
      case _ => Schema.Fail("AST cannot be materialized to a Schema")
    }
    ast.optional -> ast.dimensions match {
      case (false, 0) => baseSchema
      case (true, 0)  => baseSchema.optional
      case (false, n) =>
        (0 until n).foldRight[Schema[_]](baseSchema)((_, schema) => schema.repeated)
      case (true, n) =>
        (0 until n).foldRight[Schema[_]](baseSchema)((_, schema) => schema.repeated).optional
    }
  }

  implicit lazy val schema: Schema[SchemaAst] =
    Schema.EnumN(
      Seq(
        Schema.Case("Value", Schema[Value], _.asInstanceOf[Value]),
        Schema.Case("Sum", Schema[Sum], _.asInstanceOf[Sum]),
        Schema.Case("Product", Schema[Product], _.asInstanceOf[Product]),
        Schema.Case("Root", Schema[Root.type], _.asInstanceOf[Root.type]),
        Schema.Case("Ref", Schema[Ref], _.asInstanceOf[Ref])
      )
    )

}

private[schema] object AstRenderer {
  private val INDENT_STEP = 2

  def render(ast: SchemaAst): String = ast match {
    case SchemaAst.Root        => ""
    case v: SchemaAst.Value    => renderValue(v, 0, None)
    case f: SchemaAst.FailNode => renderFail(f, 0, None)
    case SchemaAst.Product(id, _, fields, optional, dimensions) =>
      val buffer = new StringBuffer()
      if (optional) buffer.append("?")
      buffer.append(s"record(ref=$id)").append(renderDimensions(dimensions))
      buffer.append("\n").append(fields.map(renderField(_, INDENT_STEP)).mkString("\n")).toString
    case SchemaAst.Sum(id, _, cases, optional, dimensions) =>
      val buffer = new StringBuffer()
      if (optional) buffer.append("?")
      buffer.append(s"enum(ref=$id)").append(renderDimensions(dimensions))
      buffer.append("\n").append(cases.map(renderField(_, INDENT_STEP)).mkString("\n")).toString
    case SchemaAst.Ref(refId, _, optional, dimensions) =>
      val buffer = new StringBuffer()
      if (optional) buffer.append("?")
      buffer.append(s"{ref#$refId}").append(renderDimensions(dimensions))
      buffer.toString
  }

  def renderField(value: SchemaAst.Labelled, indent: Int): String = {
    val buffer = new StringBuffer()
    value match {
      case (_, SchemaAst.Root) => ""
      case (label, value: SchemaAst.Value) =>
        renderValue(value, indent, Some(label))
      case (label, fail: SchemaAst.FailNode) =>
        renderFail(fail, indent, Some(label))
      case (label, SchemaAst.Product(id, _, fields, optional, dimensions)) =>
        pad(buffer, indent)
        buffer.append(s"$label: ")
        if (optional) buffer.append("?")
        buffer.append(s"record(ref=$id)").append(renderDimensions(dimensions))
        buffer.append("\n").append(fields.map(renderField(_, indent + INDENT_STEP)).mkString("\n")).toString
      case (label, SchemaAst.Sum(id, _, cases, optional, dimensions)) =>
        pad(buffer, indent)
        buffer.append(s"$label: ")
        if (optional) buffer.append("?")
        buffer.append(s"enum(ref=$id)").append(renderDimensions(dimensions))
        buffer.append("\n").append(cases.map(renderField(_, indent + INDENT_STEP)).mkString("\n")).toString
      case (label, SchemaAst.Ref(refId, _, optional, dimensions)) =>
        pad(buffer, indent)
        buffer.append(s"$label: ")
        if (optional) buffer.append("?")
        buffer.append(s"{ref#$refId}").append(renderDimensions(dimensions)).toString
    }
  }

  def renderValue(value: SchemaAst.Value, indent: Int, label: Option[String]): String = {
    val buffer = new StringBuffer()
    pad(buffer, indent)
    label.foreach(l => buffer.append(s"$l: "))
    if (value.optional) buffer.append("?")
    buffer.append(value.valueType.tag).append(renderDimensions(value.dimensions)).toString
  }

  def renderFail(fail: SchemaAst.FailNode, indent: Int, label: Option[String]): String = {
    val buffer = new StringBuffer()
    pad(buffer, indent)
    label.foreach(l => buffer.append(s"$l: "))
    if (fail.optional) buffer.append("?")
    buffer.append(s"FAIL${renderDimensions(fail.dimensions)}: ${fail.message}").toString
  }

  def renderDimensions(dimensions: Int): String =
    if (dimensions > 0) (0 until dimensions).map(_ => "[]").mkString("")
    else ""

  private def pad(buffer: StringBuffer, indent: Int): StringBuffer = {
    if (indent > 0) {
      buffer.append("|")
      for (_ <- 0 until indent) {
        buffer.append("-")
      }
    }
    buffer
  }
}
