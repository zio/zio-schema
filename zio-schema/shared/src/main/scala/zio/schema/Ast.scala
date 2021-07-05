package zio.schema

import zio.{ Chunk, ChunkBuilder }

sealed trait Ast { self =>
  def id: Int
  def lineage: Chunk[Int]

  def toSchema: Schema[_] = self match {
    case Ast.Value(typ, false, false) => Schema.Primitive(typ)
    case Ast.Value(typ, true, false)  => Schema.Primitive(typ).optional
    case Ast.Value(typ, false, true)  => Schema.chunk(Schema.Primitive(typ))
    case Ast.Value(typ, true, true)   => Schema.chunk(Schema.Primitive(typ)).optional
    case Ast.FailNode(msg, _, _)      => Schema.Fail(msg)
    case _                            => ???
  }
}

object Ast {
  final case object Root extends Ast {
    def id: Int             = 0
    def lineage: Chunk[Int] = Chunk.empty
  }
  final case class Node(
    override val id: Int,
    override val lineage: Chunk[Int],
    children: Chunk[Ast] = Chunk.empty,
    optional: Boolean = false,
    repeated: Boolean = false
  ) extends Ast
  final case class LabelledNode(label: String, node: Ast) extends Ast {
    def id: Int             = node.id
    def lineage: Chunk[Int] = node.lineage
  }
  final case class FailNode(message: String, optional: Boolean = false, repeated: Boolean = false) extends Ast {
    def id: Int             = 0
    def lineage: Chunk[Int] = Chunk.empty
  }
  final case class Value(valueType: StandardType[_], optional: Boolean = false, repeated: Boolean = false) extends Ast {
    def id: Int             = valueType.hashCode()
    def lineage: Chunk[Int] = Chunk.empty
  }

  object Value {

    private def tupled(value: Value): Either[String, (String, Boolean, Boolean)] =
      Right((value.valueType.tag, value.optional, value.repeated))

    private def fromTuple(tuple: (String, Boolean, Boolean)): Either[String, Value] = tuple match {
      case (s, optional, repeated) =>
        StandardType.fromString(s).map(typ => Value(typ, optional, repeated)).toRight(s"unkown standard type $s")
    }

    implicit val schema: Schema[Value] =
      Schema[(String, Boolean, Boolean)].transformOrFail(fromTuple, tupled)
  }
  final case class Ref(
    refId: Int,
    override val lineage: Chunk[Int],
    optional: Boolean = false,
    repeated: Boolean = false
  ) extends Ast {
    def id: Int = refId
  }

  final private[schema] case class NodeBuilder(
    id: Int,
    lineage: Chunk[Int],
    optional: Boolean = false,
    repeated: Boolean = false
  ) { self =>
    private val children: ChunkBuilder[Ast] = ChunkBuilder.make[Ast]()

    def addSubtree(schema: Schema[_]): NodeBuilder = {
      children += (subtree(schema, lineage :+ id))
      self
    }

    def addLabelledSubtree(label: String, schema: Schema[_]): NodeBuilder = {
      children += (labelledSubtree(label, schema, lineage :+ id))
      self
    }

    def build(): Node = Node(id, lineage, children.result(), optional, repeated)
  }

  def fromSchema[A](schema: Schema[A]): Ast = schema match {
    case Schema.Primitive(typ)   => Value(typ)
    case Schema.Fail(message)    => FailNode(message)
    case Schema.Optional(schema) => subtree(schema, Chunk.empty, optional = true)
    case s @ Schema.EitherSchema(left, right) =>
      NodeBuilder(s.hashCode(), Chunk.empty).addLabelledSubtree("left", left).addLabelledSubtree("right", right).build()
    case s @ Schema.Tuple(left, right) =>
      NodeBuilder(s.hashCode(), Chunk.empty).addLabelledSubtree("left", left).addLabelledSubtree("right", right).build()
    case Schema.Sequence(schema, _, _) =>
      subtree(schema, Chunk.empty, repeated = true)
    case Schema.Transform(schema, _, _) => subtree(schema, Chunk.empty)
    case lzy @ Schema.Lazy(_)           => fromSchema(lzy.schema)
    case s: Schema.Record[A] =>
      s.structure
        .foldLeft(NodeBuilder(s.hashCode(), Chunk.empty)) { (node, field) =>
          node.addLabelledSubtree(field.label, field.schema)
        }
        .build()
    case s: Schema.Enum[A] =>
      s.structure
        .foldLeft(NodeBuilder(s.hashCode(), Chunk.empty)) {
          case (node, (id, schema)) =>
            node.addLabelledSubtree(id, schema)
        }
        .build()
    case Schema.Meta(ast) => ast
  }

  def subtree(schema: Schema[_], lineage: Chunk[Int], optional: Boolean = false, repeated: Boolean = false): Ast =
    lineage
      .find(_ == schema.hashCode())
      .map { refId =>
        Ref(refId, lineage, optional, repeated)
      }
      .getOrElse {
        schema match {
          case Schema.Primitive(typ)   => Value(typ, optional, repeated)
          case Schema.Optional(schema) => subtree(schema, lineage, optional = true, repeated = false)
          case s @ Schema.EitherSchema(left, right) =>
            NodeBuilder(s.hashCode(), lineage)
              .addLabelledSubtree("left", left)
              .addLabelledSubtree("right", right)
              .build()
          case s @ Schema.Tuple(left, right) =>
            NodeBuilder(s.hashCode(), lineage)
              .addLabelledSubtree("left", left)
              .addLabelledSubtree("right", right)
              .build()
          case Schema.Sequence(schema, _, _) =>
            subtree(schema, lineage, optional = false, repeated = true)
          case Schema.Transform(schema, _, _) => subtree(schema, lineage, optional = false, repeated = false)
          case lzy @ Schema.Lazy(_)           => subtree(lzy.schema, lineage, optional, repeated)
          case s: Schema.Record[_] =>
            s.structure
              .foldRight(NodeBuilder(s.hashCode(), lineage)) { (field, node) =>
                node.addLabelledSubtree(field.label, field.schema)
              }
              .build()
          case s: Schema.Enum[_] =>
            s.structure
              .foldRight(NodeBuilder(s.hashCode(), lineage)) {
                case ((id, schema), node) =>
                  node.addLabelledSubtree(id, schema)
              }
              .build()
          case Schema.Fail(message) => FailNode(message)
          case Schema.Meta(ast)     => ast
        }
      }

  def labelledSubtree(
    label: String,
    schema: Schema[_],
    lineage: Chunk[Int],
    optional: Boolean = false,
    repeated: Boolean = false
  ): Ast =
    LabelledNode(label, subtree(schema, lineage, optional, repeated))

  implicit val schema: Schema[Ast] = DeriveSchema.gen[Ast]

}
