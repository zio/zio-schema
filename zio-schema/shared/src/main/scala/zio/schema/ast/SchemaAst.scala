package zio.schema.ast

import scala.annotation.tailrec
import scala.collection.mutable

import zio.prelude.Equal
import zio.schema._
import zio.{ Chunk, ChunkBuilder }

sealed trait SchemaAst { self =>
  def path: NodePath
  def optional: Boolean

  def toSchema: Schema[_] = {
    val refMap = mutable.HashMap.empty[NodePath, Schema[_]]
    SchemaAst.materialize(self, refMap)
  }

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
    override val optional: Boolean = false
  ) extends SchemaAst

  object Product {
    implicit val schema: Schema[Product] = {
      Schema.CaseClass3(
        field1 = Schema.Field("path", Schema[String].repeated),
        field2 = Schema.Field("fields", Schema[Labelled].repeated),
        field3 = Schema.Field("optional", Schema[Boolean]),
        (path: Chunk[String], fields: Chunk[Labelled], optional: Boolean) => Product(NodePath(path), fields, optional),
        _.path,
        _.fields,
        _.optional
      )
    }
  }

  final case class Tuple(
    override val path: NodePath,
    left: SchemaAst,
    right: SchemaAst,
    override val optional: Boolean = false
  ) extends SchemaAst

  object Tuple {
    implicit val schema: Schema[Tuple] = {
      Schema.CaseClass4(
        field1 = Schema.Field("path", Schema[String].repeated),
        field2 = Schema.Field("left", Schema[SchemaAst]),
        field3 = Schema.Field("right", Schema[SchemaAst]),
        field4 = Schema.Field("optional", Schema[Boolean]),
        (path: Chunk[String], left: SchemaAst, right: SchemaAst, optional: Boolean) =>
          Tuple(NodePath(path), left, right, optional),
        _.path,
        _.left,
        _.right,
        _.optional
      )
    }
  }

  final case class Sum(
    override val path: NodePath,
    cases: Chunk[Labelled] = Chunk.empty,
    override val optional: Boolean = false
  ) extends SchemaAst

  object Sum {
    implicit val schema: Schema[Sum] =
      Schema.CaseClass3(
        field1 = Schema.Field("path", Schema[String].repeated),
        field2 = Schema.Field("cases", Schema[Labelled].repeated),
        field3 = Schema.Field("optional", Schema[Boolean]),
        (path: Chunk[String], fields: Chunk[Labelled], optional: Boolean) => Sum(NodePath(path), fields, optional),
        _.path,
        _.cases,
        _.optional
      )
  }

  final case class Either(
    override val path: NodePath,
    left: SchemaAst,
    right: SchemaAst,
    override val optional: Boolean = false
  ) extends SchemaAst

  object Either {
    implicit val schema: Schema[Either] = {
      Schema.CaseClass4(
        field1 = Schema.Field("path", Schema[String].repeated),
        field2 = Schema.Field("left", Schema[SchemaAst]),
        field3 = Schema.Field("right", Schema[SchemaAst]),
        field4 = Schema.Field("optional", Schema[Boolean]),
        (path: Chunk[String], left: SchemaAst, right: SchemaAst, optional: Boolean) =>
          Either(NodePath(path), left, right, optional),
        _.path,
        _.left,
        _.right,
        _.optional
      )
    }
  }

  final case class FailNode(
    message: String,
    override val path: NodePath,
    override val optional: Boolean = false
  ) extends SchemaAst

  object FailNode {
    implicit val schema: Schema[FailNode] = Schema.CaseClass3(
      field1 = Schema.Field("message", Schema[String]),
      field2 = Schema.Field("path", Schema[String].repeated),
      field3 = Schema.Field("optional", Schema[Boolean]),
      (m: String, path: Chunk[String], optional: Boolean) => FailNode(m, NodePath(path), optional),
      _.message,
      _.path,
      _.optional
    )
  }

  final case class ListNode(
    item: SchemaAst,
    override val path: NodePath,
    override val optional: Boolean = false
  ) extends SchemaAst

  object ListNode {
    implicit val schema: Schema[ListNode] = Schema.CaseClass3(
      field1 = Schema.Field("item", Schema[SchemaAst]),
      field2 = Schema.Field("path", Schema[String].repeated),
      field3 = Schema.Field("optional", Schema[Boolean]),
      (item: SchemaAst, path: Chunk[String], optional: Boolean) => ListNode(item, NodePath(path), optional),
      _.item,
      _.path,
      _.optional
    )
  }

  final case class Dictionary(
    keys: SchemaAst,
    values: SchemaAst,
    override val path: NodePath,
    override val optional: Boolean = false
  ) extends SchemaAst

  object Dictionary {
    implicit val schema: Schema[Dictionary] = Schema.CaseClass4(
      field1 = Schema.Field("keys", Schema[SchemaAst]),
      field2 = Schema.Field("values", Schema[SchemaAst]),
      field3 = Schema.Field("path", Schema[String].repeated),
      field4 = Schema.Field("optional", Schema[Boolean]),
      (keys: SchemaAst, values: SchemaAst, path: Chunk[String], optional: Boolean) =>
        Dictionary(keys, values, NodePath(path), optional),
      _.keys,
      _.values,
      _.path,
      _.optional
    )
  }

  final case class Value(
    valueType: StandardType[_],
    override val path: NodePath = NodePath.root,
    override val optional: Boolean = false
  ) extends SchemaAst

  object Value {
    implicit val schema: Schema[Value] =
      Schema
        .CaseClass3[String, Chunk[String], Boolean, (String, Chunk[String], Boolean)](
          field1 = Schema.Field("valueType", Schema[String]),
          field2 = Schema.Field("path", Schema[String].repeated),
          field3 = Schema.Field("optional", Schema[Boolean]),
          construct = (v, p, o) => (v, p, o),
          extractField1 = _._1,
          extractField2 = _._2,
          extractField3 = _._3
        )
        .transformOrFail(fromTuple, tupled)

    private def tupled(value: Value): scala.Either[String, (String, Chunk[String], Boolean)] =
      Right((value.valueType.tag, value.path, value.optional))

    private def fromTuple(tuple: (String, Chunk[String], Boolean)): scala.Either[String, Value] = tuple match {
      case (s, path, optional) =>
        StandardType
          .fromString(s)
          .map(typ => Value(typ, NodePath(path), optional))
          .toRight(s"unkown standard type $s")
    }
  }
  final case class Ref(
    refPath: NodePath,
    override val path: NodePath,
    optional: Boolean = false
  ) extends SchemaAst

  object Ref {
    implicit val schema: Schema[Ref] =
      Schema.CaseClass3(
        field1 = Schema.Field("refPath", Schema[String].repeated),
        field2 = Schema.Field("path", Schema[String].repeated),
        field3 = Schema.Field("optional", Schema[Boolean]),
        (refPath: Chunk[String], path: Chunk[String], optional: Boolean) =>
          Ref(NodePath(refPath), NodePath(path), optional),
        _.refPath,
        _.path,
        _.optional
      )
  }

  final private[schema] case class NodeBuilder(
    path: NodePath,
    lineage: Lineage,
    optional: Boolean = false
  ) { self =>
    private val children: ChunkBuilder[Labelled] = ChunkBuilder.make[Labelled]()

    def addLabelledSubtree(label: String, schema: Schema[_]): NodeBuilder = {
      children += (label -> subtree(path / label, lineage, schema))
      self
    }

    def buildProduct(): Product = Product(path, children.result(), optional)

    def buildSum(): Sum = Sum(path, children.result(), optional)
  }

  @tailrec
  def fromSchema[A](schema: Schema[A]): SchemaAst = schema match {
    case Schema.Primitive(typ, _)   => Value(typ, NodePath.root)
    case Schema.Fail(message, _)    => FailNode(message, NodePath.root)
    case Schema.Optional(schema, _) => subtree(NodePath.root, Chunk.empty, schema, optional = true)
    case Schema.EitherSchema(left, right, _) =>
      Either(
        NodePath.root,
        subtree(NodePath.root / "left", Chunk.empty, left),
        subtree(NodePath.root / "right", Chunk.empty, right)
      )
    case Schema.Tuple(left, right, _) =>
      Tuple(
        NodePath.root,
        subtree(NodePath.root / "left", Chunk.empty, left),
        subtree(NodePath.root / "right", Chunk.empty, right)
      )
    case Schema.Sequence(schema, _, _, _, _) =>
      ListNode(item = subtree(NodePath.root / "item", Chunk.empty, schema), NodePath.root)
    case Schema.MapSchema(ks, vs, _) =>
      Dictionary(
        keys = subtree(NodePath.root / "keys", Chunk.empty, ks),
        values = subtree(NodePath.root / "values", Chunk.empty, vs),
        NodePath.root
      )
    case Schema.SetSchema(schema, _) =>
      ListNode(item = subtree(NodePath.root / "item", Chunk.empty, schema), NodePath.root)
    case Schema.Transform(schema, _, _, _, _) => subtree(NodePath.root, Chunk.empty, schema)
    case lzy @ Schema.Lazy(_)                 => fromSchema(lzy.schema)
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
    case Schema.Meta(ast, _) => ast
  }

  private[schema] def subtree(
    path: NodePath,
    lineage: Lineage,
    schema: Schema[_],
    optional: Boolean = false
  ): SchemaAst =
    lineage
      .find(_._1 == schema.hashCode())
      .map {
        case (_, refPath) =>
          Ref(refPath, path, optional)
      }
      .getOrElse {
        schema match {
          case Schema.Primitive(typ, _)   => Value(typ, path, optional)
          case Schema.Optional(schema, _) => subtree(path, lineage, schema, optional = true)
          case Schema.EitherSchema(left, right, _) =>
            Either(
              path,
              subtree(path / "left", lineage, left, optional = false),
              subtree(path / "right", lineage, right, optional = false),
              optional
            )
          case Schema.Tuple(left, right, _) =>
            Tuple(
              path,
              subtree(path / "left", lineage, left, optional = false),
              subtree(path / "right", lineage, right, optional = false),
              optional
            )
          case Schema.Sequence(schema, _, _, _, _) =>
            ListNode(item = subtree(path / "item", lineage, schema, optional = false), path, optional)
          case Schema.MapSchema(ks, vs, _) =>
            Dictionary(
              keys = subtree(path / "keys", Chunk.empty, ks, optional = false),
              values = subtree(path / "values", Chunk.empty, vs, optional = false),
              path,
              optional
            )
          case Schema.SetSchema(schema @ _, _) =>
            ListNode(item = subtree(path / "item", lineage, schema, optional = false), path, optional)
          case Schema.Transform(schema, _, _, _, _) => subtree(path, lineage, schema, optional)
          case lzy @ Schema.Lazy(_)                 => subtree(path, lineage, lzy.schema, optional)
          case s: Schema.Record[_] =>
            s.structure
              .foldLeft(NodeBuilder(path, lineage :+ (s.hashCode() -> path), optional)) { (node, field) =>
                node.addLabelledSubtree(field.label, field.schema)
              }
              .buildProduct()
          case s: Schema.Enum[_] =>
            s.structure
              .foldLeft(NodeBuilder(path, lineage :+ (s.hashCode() -> path), optional)) {
                case (node, (id, schema)) =>
                  node.addLabelledSubtree(id, schema)
              }
              .buildSum()
          case Schema.Fail(message, _) => FailNode(message, path)
          case Schema.Meta(ast, _)     => ast
        }
      }

  private[schema] def materialize(ast: SchemaAst, refs: mutable.Map[NodePath, Schema[_]]): Schema[_] = {
    val baseSchema = ast match {
      case SchemaAst.Value(typ, _, _) =>
        Schema.Primitive(typ, Chunk.empty)
      case SchemaAst.FailNode(msg, _, _) => Schema.Fail(msg)
      case SchemaAst.Ref(refPath, _, _) =>
        Schema.defer(
          refs.getOrElse(refPath, Schema.Fail(s"invalid ref path $refPath"))
        )
      case SchemaAst.Product(_, elems, _) =>
        Schema.record(
          elems.map {
            case (label, ast) =>
              Schema.Field(label, materialize(ast, refs))
          }: _*
        )
      case SchemaAst.Tuple(_, left, right, _) =>
        Schema.tuple2(
          materialize(left, refs),
          materialize(right, refs)
        )
      case SchemaAst.Sum(_, elems, _) =>
        Schema.enumeration[Any, CaseSet.Aux[Any]](
          elems.foldRight[CaseSet.Aux[Any]](CaseSet.Empty[Any]()) {
            case ((label, ast), acc) =>
              val _case: Schema.Case[Any, Any] = Schema
                .Case[Any, Any](
                  label,
                  materialize(ast, refs).asInstanceOf[Schema[Any]],
                  identity[Any],
                  Chunk.empty
                )
              CaseSet.Cons(_case, acc)
          }
        )
      case SchemaAst.Either(_, left, right, _) =>
        Schema.either(
          materialize(left, refs),
          materialize(right, refs)
        )
      case SchemaAst.ListNode(itemAst, _, _) =>
        Schema.chunk(materialize(itemAst, refs))
      case SchemaAst.Dictionary(keyAst, valueAst, _, _) =>
        Schema.MapSchema(materialize(keyAst, refs), materialize(valueAst, refs), Chunk.empty)
      case ast => Schema.Fail(s"AST cannot be materialized to a Schema:\n$ast")
    }

    refs += ast.path -> baseSchema

    if (ast.optional) baseSchema.optional else baseSchema
  }

  implicit lazy val schema: Schema[SchemaAst] =
    Schema.EnumN[SchemaAst, CaseSet.Aux[SchemaAst]](
      caseOf[Value, SchemaAst]("Value")(_.asInstanceOf[Value]) ++
        caseOf[Sum, SchemaAst]("Sum")(_.asInstanceOf[Sum]) ++
        caseOf[Product, SchemaAst]("Product")(_.asInstanceOf[Product]) ++
        caseOf[Ref, SchemaAst]("Ref")(_.asInstanceOf[Ref]) ++ caseOf[ListNode, SchemaAst]("ListNode")(
        _.asInstanceOf[ListNode]
      ) ++
        caseOf[Dictionary, SchemaAst]("Dictionary")(_.asInstanceOf[Dictionary]),
      Chunk.empty
    )

  implicit val equals: Equal[SchemaAst] = Equal.default
}

private[schema] object AstRenderer {
  private val INDENT_STEP = 2

  def render(ast: SchemaAst): String = ast match {
    case v: SchemaAst.Value    => renderValue(v, 0, None)
    case f: SchemaAst.FailNode => renderFail(f, 0, None)
    case SchemaAst.Product(_, fields, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"product")
      if (optional) buffer.append("?")
      buffer.append("\n").append(fields.map(renderField(_, INDENT_STEP)).mkString("\n")).toString
    case SchemaAst.Tuple(_, left, right, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"tuple")
      if (optional) buffer.append("?")
      buffer
        .append("\n")
        .append(Chunk("left" -> left, "right" -> right).map(renderField(_, INDENT_STEP)).mkString("\n"))
        .toString
    case SchemaAst.Sum(_, cases, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"enum")
      if (optional) buffer.append("?")
      buffer.append("\n").append(cases.map(renderField(_, INDENT_STEP)).mkString("\n")).toString
    case SchemaAst.Either(_, left, right, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"either")
      if (optional) buffer.append("?")
      buffer
        .append("\n")
        .append(Chunk("left" -> left, "right" -> right).map(renderField(_, INDENT_STEP)).mkString("\n"))
        .toString
    case SchemaAst.ListNode(items, _, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"list")
      if (optional) buffer.append("?")
      buffer
        .append("\n")
        .append(Chunk("item" -> items).map(renderField(_, INDENT_STEP)).mkString("\n"))
        .toString
    case SchemaAst.Dictionary(keys, values, _, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"map")
      if (optional) buffer.append("?")
      buffer
        .append("\n")
        .append(Chunk("keys" -> keys, "values" -> values).map(renderField(_, INDENT_STEP)).mkString("\n"))
        .toString
    case SchemaAst.Ref(refPath, _, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"ref#$refPath")
      if (optional) buffer.append("?")
      buffer.toString
  }

  def renderField(value: SchemaAst.Labelled, indent: Int): String = {
    val buffer = new StringBuffer()
    value match {
      case (label, value: SchemaAst.Value) =>
        renderValue(value, indent, Some(label))
      case (label, fail: SchemaAst.FailNode) =>
        renderFail(fail, indent, Some(label))
      case (label, SchemaAst.Product(_, fields, optional)) =>
        pad(buffer, indent)
        buffer.append(s"$label: record")
        if (optional) buffer.append("?")
        buffer.append("\n").append(fields.map(renderField(_, indent + INDENT_STEP)).mkString("\n")).toString
      case (label, SchemaAst.Tuple(_, left, right, optional)) =>
        pad(buffer, indent)
        buffer.append(s"$label: tuple")
        if (optional) buffer.append("?")
        buffer
          .append("\n")
          .append(Chunk("left" -> left, "right" -> right).map(renderField(_, indent + INDENT_STEP)).mkString("\n"))
          .toString
      case (label, SchemaAst.Sum(_, cases, optional)) =>
        pad(buffer, indent)
        buffer.append(s"$label: enum")
        if (optional) buffer.append("?")
        buffer.append("\n").append(cases.map(renderField(_, indent + INDENT_STEP)).mkString("\n")).toString
      case (label, SchemaAst.Either(_, left, right, optional)) =>
        pad(buffer, indent)
        buffer.append(s"$label: either")
        if (optional) buffer.append("?")
        buffer
          .append("\n")
          .append(Chunk("left" -> left, "right" -> right).map(renderField(_, indent + INDENT_STEP)).mkString("\n"))
          .toString
      case (label, SchemaAst.ListNode(items, _, optional)) =>
        val buffer = new StringBuffer()
        buffer.append(s"$label: list")
        if (optional) buffer.append("?")
        buffer
          .append("\n")
          .append(Chunk("item" -> items).map(renderField(_, INDENT_STEP)).mkString("\n"))
          .toString
      case (label, SchemaAst.Dictionary(keys, values, _, optional)) =>
        val buffer = new StringBuffer()
        buffer.append(s"$label: map")
        if (optional) buffer.append("?")
        buffer
          .append("\n")
          .append(Chunk("keys" -> keys, "values" -> values).map(renderField(_, INDENT_STEP)).mkString("\n"))
          .toString
      case (label, SchemaAst.Ref(refPath, _, optional)) =>
        pad(buffer, indent)
        buffer.append(s"$label: ")
        if (optional) buffer.append("?")
        buffer.append(s"{ref#${refPath.render}}").toString
      case _ => ???
    }
  }

  def renderValue(value: SchemaAst.Value, indent: Int, label: Option[String]): String = {
    val buffer = new StringBuffer()
    pad(buffer, indent)
    label.foreach(l => buffer.append(s"$l: "))
    if (value.optional) buffer.append("?")
    buffer.append(value.valueType.tag).toString
  }

  def renderFail(fail: SchemaAst.FailNode, indent: Int, label: Option[String]): String = {
    val buffer = new StringBuffer()
    pad(buffer, indent)
    label.foreach(l => buffer.append(s"$l: "))
    if (fail.optional) buffer.append("?")
    buffer.append(s"FAIL: ${fail.message}").toString
  }

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
