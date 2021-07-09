package zio.schema

import scala.collection.immutable.ListMap

import zio.{ Chunk, ChunkBuilder }

sealed trait Ast { self =>
  def id: Int
  def lineage: Chunk[Int]
  def optional: Boolean
  def dimensions: Int

  def toSchema: Schema[_] = Ast.materialize(self)

  override def toString: String = AstRenderer.render(self)
}

object Ast {
  type Labelled = (String, Ast)

  final case object Root extends Ast {
    def id: Int             = 0
    def lineage: Chunk[Int] = Chunk.empty
    def optional: Boolean   = false
    def dimensions: Int     = 0
  }
  final case class Product(
    override val id: Int,
    override val lineage: Chunk[Int],
    elements: Chunk[Labelled] = Chunk.empty,
    override val optional: Boolean = false,
    override val dimensions: Int = 0
  ) extends Ast
  final case class Sum(
    override val id: Int,
    override val lineage: Chunk[Int],
    cases: Chunk[Labelled] = Chunk.empty,
    override val optional: Boolean = false,
    override val dimensions: Int = 0
  ) extends Ast
  final case class FailNode(message: String, override val optional: Boolean = false, override val dimensions: Int = 0)
      extends Ast {
    def id: Int             = 0
    def lineage: Chunk[Int] = Chunk.empty
  }
  final case class Value(
    valueType: StandardType[_],
    override val optional: Boolean = false,
    override val dimensions: Int = 0
  ) extends Ast {
    def id: Int             = valueType.hashCode()
    def lineage: Chunk[Int] = Chunk.empty
  }

  object Value {

    private def tupled(value: Value): Either[String, (String, Boolean, Int)] =
      Right((value.valueType.tag, value.optional, value.dimensions))

    private def fromTuple(tuple: (String, Boolean, Int)): Either[String, Value] = tuple match {
      case (s, optional, dimensions) =>
        StandardType.fromString(s).map(typ => Value(typ, optional, dimensions)).toRight(s"unkown standard type $s")
    }

    implicit val schema: Schema[Value] =
      Schema[(String, Boolean, Int)].transformOrFail(fromTuple, tupled)
  }
  final case class Ref(
    refId: Int,
    override val lineage: Chunk[Int],
    optional: Boolean = false,
    dimensions: Int = 0
  ) extends Ast {
    def id: Int = refId
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

  def fromSchema[A](schema: Schema[A]): Ast = schema match {
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

  def subtree(schema: Schema[_], lineage: Chunk[Int], optional: Boolean = false, dimensions: Int = 0): Ast =
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

  def materialize(ast: Ast, refs: Map[Int, Ast] = Map.empty): Schema[_] = {
    val baseSchema = ast match {
      case Ast.Value(typ, _, _) =>
        Schema.Primitive(typ)
        Schema.Primitive(typ)
      case Ast.FailNode(msg, _, _) => Schema.Fail(msg)
      case Ast.Ref(id, _, _, _) =>
        refs
          .get(id)
          .map(astRef => Schema.defer(materialize(astRef, Map.empty)))
          .getOrElse(Schema.Fail(s"invalid ref $id"))
      case n @ Ast.Product(id, _, elems, _, _) =>
        Schema.GenericRecord(
          elems.map {
            case (label, ast) =>
              Schema.Field(label, materialize(ast, refs + (id -> n)))
          }
        )
      case n @ Ast.Sum(id, _, elems, _, _) =>
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

  implicit lazy val schema: Schema[Ast] = DeriveSchema.gen[Ast]

}

object AstRenderer {
  private val INDENT_STEP = 2

  def render(ast: Ast): String = ast match {
    case Ast.Root        => ""
    case v: Ast.Value    => renderValue(v, 0, None)
    case f: Ast.FailNode => renderFail(f, 0, None)
    case Ast.Product(id, _, fields, optional, dimensions) =>
      val buffer = new StringBuffer()
      if (optional) buffer.append("?")
      buffer.append(s"record(ref=$id)").append(renderDimensions(dimensions))
      buffer.append("\n").append(fields.map(renderField(_, INDENT_STEP)).mkString("\n")).toString
    case Ast.Sum(id, _, cases, optional, dimensions) =>
      val buffer = new StringBuffer()
      if (optional) buffer.append("?")
      buffer.append(s"enum(ref=$id)").append(renderDimensions(dimensions))
      buffer.append("\n").append(cases.map(renderField(_, INDENT_STEP)).mkString("\n")).toString
    case Ast.Ref(refId, _, optional, dimensions) =>
      val buffer = new StringBuffer()
      if (optional) buffer.append("?")
      buffer.append(s"{ref#$refId}").append(renderDimensions(dimensions))
      buffer.toString
  }

  def renderField(value: Ast.Labelled, indent: Int): String = {
    val buffer = new StringBuffer()
    value match {
      case (_, Ast.Root) => ""
      case (label, value: Ast.Value) =>
        renderValue(value, indent, Some(label))
      case (label, fail: Ast.FailNode) =>
        renderFail(fail, indent, Some(label))
      case (label, Ast.Product(id, _, fields, optional, dimensions)) =>
        pad(buffer, indent)
        buffer.append(s"$label: ")
        if (optional) buffer.append("?")
        buffer.append(s"record(ref=$id)").append(renderDimensions(dimensions))
        buffer.append("\n").append(fields.map(renderField(_, indent + INDENT_STEP)).mkString("\n")).toString
      case (label, Ast.Sum(id, _, cases, optional, dimensions)) =>
        pad(buffer, indent)
        buffer.append(s"$label: ")
        if (optional) buffer.append("?")
        buffer.append(s"enum(ref=$id)").append(renderDimensions(dimensions))
        buffer.append("\n").append(cases.map(renderField(_, indent + INDENT_STEP)).mkString("\n")).toString
      case (label, Ast.Ref(refId, _, optional, dimensions)) =>
        pad(buffer, indent)
        buffer.append(s"$label: ")
        if (optional) buffer.append("?")
        buffer.append(s"{ref#$refId}").append(renderDimensions(dimensions)).toString
    }
  }

  def renderValue(value: Ast.Value, indent: Int, label: Option[String]): String = {
    val buffer = new StringBuffer()
    pad(buffer, indent)
    label.foreach(l => buffer.append(s"$l: "))
    if (value.optional) buffer.append("?")
    buffer.append(value.valueType.tag).append(renderDimensions(value.dimensions)).toString
  }

  def renderFail(fail: Ast.FailNode, indent: Int, label: Option[String]): String = {
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
