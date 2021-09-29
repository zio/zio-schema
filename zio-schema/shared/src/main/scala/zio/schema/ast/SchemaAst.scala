package zio.schema.ast

import scala.annotation.tailrec

import zio.schema._
import zio.{ Chunk, ChunkBuilder }

sealed trait SchemaAst { self =>
  def path: NodePath
  def optional: Boolean
  def dimensions: Int

  def toSchema: Schema[_] = SchemaAst.materialize(self)

  override def toString: String = AstRenderer.render(self)
}

object SchemaAst {
  import CaseSet._

  type Labelled = (String, SchemaAst)
  type Lineage  = Chunk[(Int, NodePath)]

  implicit val nodePathSchema: Schema[NodePath] =
    Schema[String].repeated
      .transform(NodePath(_), NodePath.unwrap)

  final case class Product(
    override val path: NodePath,
    fields: Chunk[Labelled] = Chunk.empty,
    override val optional: Boolean = false,
    override val dimensions: Int = 0
  ) extends SchemaAst

  object Product {
    implicit val schema: Schema[Product] = {
      Schema.CaseClass4(
        annotations = Chunk.empty,
        field1 = Schema.Field("path", Schema[String].repeated),
        field2 = Schema.Field("fields", Schema[Labelled].repeated),
        field3 = Schema.Field("optional", Schema[Boolean]),
        field4 = Schema.Field("dimensions", Schema[Int]),
        (path: Chunk[String], fields: Chunk[Labelled], optional: Boolean, dimensions: Int) =>
          Product(NodePath(path), fields, optional, dimensions),
        _.path,
        _.fields,
        _.optional,
        _.dimensions
      )
    }
  }
  final case class Sum(
    override val path: NodePath,
    cases: Chunk[Labelled] = Chunk.empty,
    override val optional: Boolean = false,
    override val dimensions: Int = 0
  ) extends SchemaAst

  object Sum {
    implicit val schema: Schema[Sum] =
      Schema.CaseClass4(
        annotations = Chunk.empty,
        field1 = Schema.Field("path", Schema[String].repeated),
        field2 = Schema.Field("cases", Schema[Labelled].repeated),
        field3 = Schema.Field("optional", Schema[Boolean]),
        field4 = Schema.Field("dimensions", Schema[Int]),
        (path: Chunk[String], fields: Chunk[Labelled], optional: Boolean, dimensions: Int) =>
          Sum(NodePath(path), fields, optional, dimensions),
        _.path,
        _.cases,
        _.optional,
        _.dimensions
      )
  }
  final case class FailNode(
    message: String,
    override val path: NodePath,
    override val optional: Boolean = false,
    override val dimensions: Int = 0
  ) extends SchemaAst

  object FailNode {
    implicit val schema: Schema[FailNode] = Schema.CaseClass4(
      annotations = Chunk.empty,
      field1 = Schema.Field("message", Schema[String]),
      field2 = Schema.Field("path", Schema[String].repeated),
      field3 = Schema.Field("optional", Schema[Boolean]),
      field4 = Schema.Field("dimensions", Schema[Int]),
      (m: String, path: Chunk[String], optional: Boolean, dimensions: Int) =>
        FailNode(m, NodePath(path), optional, dimensions),
      _.message,
      _.path,
      _.optional,
      _.dimensions
    )
  }
  final case class Value(
    valueType: StandardType[_],
    override val path: NodePath = NodePath.root,
    override val optional: Boolean = false,
    override val dimensions: Int = 0
  ) extends SchemaAst

  object Value {
    implicit val schema: Schema[Value] =
      Schema
        .CaseClass4[String, Chunk[String], Boolean, Int, (String, Chunk[String], Boolean, Int)](
          annotations = Chunk.empty,
          field1 = Schema.Field("valueType", Schema[String]),
          field2 = Schema.Field("path", Schema[String].repeated),
          field3 = Schema.Field("optional", Schema[Boolean]),
          field4 = Schema.Field("dimensions", Schema[Int]),
          construct = (v, p, o, d) => (v, p, o, d),
          extractField1 = _._1,
          extractField2 = _._2,
          extractField3 = _._3,
          extractField4 = _._4
        )
        .transformOrFail(fromTuple, tupled)

    private def tupled(value: Value): Either[String, (String, Chunk[String], Boolean, Int)] =
      Right((value.valueType.tag, value.path, value.optional, value.dimensions))

    private def fromTuple(tuple: (String, Chunk[String], Boolean, Int)): Either[String, Value] = tuple match {
      case (s, path, optional, dimensions) =>
        StandardType
          .fromString(s)
          .map(typ => Value(typ, NodePath(path), optional, dimensions))
          .toRight(s"unkown standard type $s")
    }
  }
  final case class Ref(
    refPath: NodePath,
    override val path: NodePath,
    optional: Boolean = false,
    dimensions: Int = 0
  ) extends SchemaAst

  object Ref {
    implicit val schema: Schema[Ref] =
      Schema.CaseClass4(
        annotations = Chunk.empty,
        field1 = Schema.Field("refPath", Schema[String].repeated),
        field2 = Schema.Field("path", Schema[String].repeated),
        field3 = Schema.Field("optional", Schema[Boolean]),
        field4 = Schema.Field("dimensions", Schema[Int]),
        (refPath: Chunk[String], path: Chunk[String], optional: Boolean, dimensions: Int) =>
          Ref(NodePath(refPath), NodePath(path), optional, dimensions),
        _.refPath,
        _.path,
        _.optional,
        _.dimensions
      )
  }

  final private[schema] case class NodeBuilder(
    path: NodePath,
    lineage: Lineage,
    optional: Boolean = false,
    dimensions: Int = 0
  ) { self =>
    private val children: ChunkBuilder[Labelled] = ChunkBuilder.make[Labelled]()

    def addLabelledSubtree(label: String, schema: Schema[_]): NodeBuilder = {
      children += (label -> subtree(path / label, lineage, schema))
      self
    }

    def buildProduct(): Product = Product(path, children.result(), optional, dimensions)

    def buildSum(): Sum = Sum(path, children.result(), optional, dimensions)
  }

  @tailrec
  def fromSchema[A](schema: Schema[A]): SchemaAst = schema match {
    case Schema.Primitive(typ)   => Value(typ, NodePath.root)
    case Schema.Fail(message)    => FailNode(message, NodePath.root)
    case Schema.Optional(schema) => subtree(NodePath.root, Chunk.empty, schema, optional = true)
    case Schema.EitherSchema(left, right) =>
      NodeBuilder(NodePath.root, Chunk.empty)
        .addLabelledSubtree("left", left)
        .addLabelledSubtree("right", right)
        .buildSum()
    case Schema.Tuple(left, right) =>
      NodeBuilder(NodePath.root, Chunk.empty)
        .addLabelledSubtree("left", left)
        .addLabelledSubtree("right", right)
        .buildProduct()
    case Schema.Sequence(schema, _, _) =>
      subtree(NodePath.root, Chunk.empty, schema, dimensions = 1)
    case Schema.Transform(schema, _, _) => subtree(NodePath.root, Chunk.empty, schema)
    case lzy @ Schema.Lazy(_)           => fromSchema(lzy.schema)
    case s: Schema.Record[A] =>
      s.structure
        .foldLeft(NodeBuilder(NodePath.root, Chunk(s.hashCode() -> NodePath.root))) { (node, field) =>
          node.addLabelledSubtree(field.label, field.schema)
        }
        .buildProduct()
    case s: Schema.Enum[A] =>
      s.structure
        .foldLeft(NodeBuilder(NodePath.root, Chunk(s.hashCode() -> NodePath.root))) {
          case (node, (id, schema)) =>
            node.addLabelledSubtree(id, schema)
        }
        .buildSum()
    case Schema.Meta(ast) => ast
  }

  private[schema] def subtree(
    path: NodePath,
    lineage: Lineage,
    schema: Schema[_],
    optional: Boolean = false,
    dimensions: Int = 0
  ): SchemaAst =
    lineage
      .find(_._1 == schema.hashCode())
      .map {
        case (_, refPath) =>
          Ref(refPath, path, optional, dimensions)
      }
      .getOrElse {
        schema match {
          case Schema.Primitive(typ)   => Value(typ, path, optional, dimensions)
          case Schema.Optional(schema) => subtree(path, lineage, schema, optional = true, dimensions)
          case Schema.EitherSchema(left, right) =>
            NodeBuilder(path, lineage, optional, dimensions)
              .addLabelledSubtree("left", left)
              .addLabelledSubtree("right", right)
              .buildSum()
          case Schema.Tuple(left, right) =>
            NodeBuilder(path, lineage, optional, dimensions)
              .addLabelledSubtree("left", left)
              .addLabelledSubtree("right", right)
              .buildProduct()
          case Schema.Sequence(schema, _, _) =>
            subtree(path, lineage, schema, optional, dimensions + 1)
          case Schema.Transform(schema, _, _) => subtree(path, lineage, schema, optional, dimensions)
          case lzy @ Schema.Lazy(_)           => subtree(path, lineage, lzy.schema, optional, dimensions)
          case s: Schema.Record[_] =>
            s.structure
              .foldLeft(NodeBuilder(path, lineage :+ (s.hashCode() -> path), optional, dimensions)) { (node, field) =>
                node.addLabelledSubtree(field.label, field.schema)
              }
              .buildProduct()
          case s: Schema.Enum[_] =>
            s.structure
              .foldLeft(NodeBuilder(path, lineage :+ (s.hashCode() -> path), optional, dimensions)) {
                case (node, (id, schema)) =>
                  node.addLabelledSubtree(id, schema)
              }
              .buildSum()
          case Schema.Fail(message) => FailNode(message, path)
          case Schema.Meta(ast)     => ast
        }
      }

  private[schema] def materialize(ast: SchemaAst, refs: Map[NodePath, SchemaAst] = Map.empty): Schema[_] = {
    val baseSchema = ast match {
      case SchemaAst.Value(typ, _, _, _) =>
        Schema.Primitive(typ)
      case SchemaAst.FailNode(msg, _, _, _) => Schema.Fail(msg)
      case SchemaAst.Ref(refPath, _, _, _) =>
        refs
          .get(refPath)
          .map(astRef => Schema.defer(materialize(astRef, Map.empty)))
          .getOrElse(Schema.Fail(s"invalid ref path $refPath"))
      case n @ SchemaAst.Product(path, elems, _, _) =>
        Schema.record(
          elems.map {
            case (label, ast) =>
              Schema.Field(label, materialize(ast, refs + (path -> n.copy(optional = false, dimensions = 0))))
          }: _*
        )
      case n @ SchemaAst.Sum(path, elems, _, _) =>
        Schema.enumeration[Any, CaseSet.Aux[Any]](
          elems.foldRight[CaseSet.Aux[Any]](CaseSet.Empty[Any]()) {
            case ((label, ast), acc) =>
              val _case: Schema.Case[Any, Any] = Schema
                .Case[Any, Any](
                  label,
                  materialize(ast, refs + (path -> n.copy(optional = false, dimensions = 0))).asInstanceOf[Schema[Any]],
                  identity[Any]
                )
              CaseSet.Cons(_case, acc)
          }
        )
      case ast => Schema.Fail(s"AST cannot be materialized to a Schema:\n$ast")
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
    Schema.EnumN[SchemaAst, CaseSet.Aux[SchemaAst]](
      caseOf[Value, SchemaAst]("Value")(_.asInstanceOf[Value]) ++
        caseOf[Sum, SchemaAst]("Sum")(_.asInstanceOf[Sum]) ++
        caseOf[Product, SchemaAst]("Product")(_.asInstanceOf[Product]) ++
        caseOf[Ref, SchemaAst]("Ref")(_.asInstanceOf[Ref])
    )

}

private[schema] object AstRenderer {
  private val INDENT_STEP = 2

  def render(ast: SchemaAst): String = ast match {
    case v: SchemaAst.Value    => renderValue(v, 0, None)
    case f: SchemaAst.FailNode => renderFail(f, 0, None)
    case SchemaAst.Product(_, fields, optional, dimensions) =>
      val buffer = new StringBuffer()
      if (optional) buffer.append("?")
      buffer.append(s"record").append(renderDimensions(dimensions))
      buffer.append("\n").append(fields.map(renderField(_, INDENT_STEP)).mkString("\n")).toString
    case SchemaAst.Sum(_, cases, optional, dimensions) =>
      val buffer = new StringBuffer()
      if (optional) buffer.append("?")
      buffer.append(s"enum").append(renderDimensions(dimensions))
      buffer.append("\n").append(cases.map(renderField(_, INDENT_STEP)).mkString("\n")).toString
    case SchemaAst.Ref(refPath, _, optional, dimensions) =>
      val buffer = new StringBuffer()
      if (optional) buffer.append("?")
      buffer.append(s"{ref#${refPath.render}}").append(renderDimensions(dimensions))
      buffer.toString
  }

  def renderField(value: SchemaAst.Labelled, indent: Int): String = {
    val buffer = new StringBuffer()
    value match {
      case (label, value: SchemaAst.Value) =>
        renderValue(value, indent, Some(label))
      case (label, fail: SchemaAst.FailNode) =>
        renderFail(fail, indent, Some(label))
      case (label, SchemaAst.Product(_, fields, optional, dimensions)) =>
        pad(buffer, indent)
        buffer.append(s"$label: ")
        if (optional) buffer.append("?")
        buffer.append(s"record").append(renderDimensions(dimensions))
        buffer.append("\n").append(fields.map(renderField(_, indent + INDENT_STEP)).mkString("\n")).toString
      case (label, SchemaAst.Sum(_, cases, optional, dimensions)) =>
        pad(buffer, indent)
        buffer.append(s"$label: ")
        if (optional) buffer.append("?")
        buffer.append(s"enum").append(renderDimensions(dimensions))
        buffer.append("\n").append(cases.map(renderField(_, indent + INDENT_STEP)).mkString("\n")).toString
      case (label, SchemaAst.Ref(refPath, _, optional, dimensions)) =>
        pad(buffer, indent)
        buffer.append(s"$label: ")
        if (optional) buffer.append("?")
        buffer.append(s"{ref#${refPath.render}}").append(renderDimensions(dimensions)).toString
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
