package zio.schema

import scala.collection.immutable.ListMap

import zio.{ Chunk, ChunkBuilder }

sealed trait Ast { self =>
  def id: Int
  def lineage: Chunk[Int]

  def toSchema: Schema[_] = Ast.materialize(self)
}

object Ast {
  type Labelled = (String, Ast)

  final case object Root extends Ast {
    def id: Int             = 0
    def lineage: Chunk[Int] = Chunk.empty
  }
  final case class Product(
    override val id: Int,
    override val lineage: Chunk[Int],
    elements: Chunk[Labelled] = Chunk.empty,
    optional: Boolean = false,
    repeated: Boolean = false
  ) extends Ast
  final case class Sum(
    override val id: Int,
    override val lineage: Chunk[Int],
    cases: Chunk[Labelled] = Chunk.empty,
    optional: Boolean = false,
    repeated: Boolean = false
  ) extends Ast
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
    private val children: ChunkBuilder[Labelled] = ChunkBuilder.make[Labelled]()

    def addLabelledSubtree(label: String, schema: Schema[_]): NodeBuilder = {
      children += (label -> subtree(schema, lineage :+ id))
      self
    }

    def buildProduct(): Product = Product(id, lineage, children.result(), optional, repeated)

    def buildSum(): Sum = Sum(id, lineage, children.result(), optional, repeated)
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
      subtree(schema, Chunk.empty, repeated = true)
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
              .buildSum()
          case s @ Schema.Tuple(left, right) =>
            NodeBuilder(s.hashCode(), lineage)
              .addLabelledSubtree("left", left)
              .addLabelledSubtree("right", right)
              .buildProduct()
          case Schema.Sequence(schema, _, _) =>
            subtree(schema, lineage, optional = false, repeated = true)
          case Schema.Transform(schema, _, _) => subtree(schema, lineage, optional = false, repeated = false)
          case lzy @ Schema.Lazy(_)           => subtree(lzy.schema, lineage, optional, repeated)
          case s: Schema.Record[_] =>
            s.structure
              .foldLeft(NodeBuilder(s.hashCode(), lineage)) { (node, field) =>
                node.addLabelledSubtree(field.label, field.schema)
              }
              .buildProduct()
          case s: Schema.Enum[_] =>
            s.structure
              .foldLeft(NodeBuilder(s.hashCode(), lineage)) {
                case (node, (id, schema)) =>
                  node.addLabelledSubtree(id, schema)
              }
              .buildSum()
          case Schema.Fail(message) => FailNode(message)
          case Schema.Meta(ast)     => ast
        }
      }

  def materialize(ast: Ast, refs: Map[Int, Ast] = Map.empty): Schema[_] = ast match {
    case Ast.Value(typ, false, false) => Schema.Primitive(typ)
    case Ast.Value(typ, true, false)  => Schema.Primitive(typ).optional
    case Ast.Value(typ, false, true)  => Schema.chunk(Schema.Primitive(typ))
    case Ast.Value(typ, true, true)   => Schema.chunk(Schema.Primitive(typ)).optional
    case Ast.FailNode(msg, _, _)      => Schema.Fail(msg)
    case Ast.Ref(id, _, _, _) =>
      refs
        .get(id)
        .map(astRef => Schema.defer(materialize(astRef, Map.empty)))
        .getOrElse(Schema.Fail(s"invalid ref $id"))
    case n @ Ast.Product(id, _, elems, false, false) =>
      Schema.GenericRecord(
        elems.map {
          case (label, ast) =>
            Schema.Field(label, materialize(ast, Map(id -> n)))
        }
      )
    case n @ Ast.Product(id, _, elems, true, false) =>
      Schema
        .GenericRecord(
          elems.map {
            case (label, ast) =>
              Schema.Field(label, materialize(ast, Map(id -> n)))
          }
        )
        .optional
    case n @ Ast.Product(id, _, elems, false, true) =>
      Schema
        .GenericRecord(
          elems.map {
            case (label, ast) =>
              Schema.Field(label, materialize(ast, Map(id -> n)))
          }
        )
        .repeated
    case n @ Ast.Sum(id, _, elems, false, false) =>
      Schema.Enumeration(
        ListMap.empty ++ elems.map {
          case (label, ast) =>
            (label, materialize(ast, Map(id -> n)))
        }
      )
    case n @ Ast.Sum(id, _, elems, true, false) =>
      Schema
        .Enumeration(
          ListMap.empty ++ elems.map {
            case (label, ast) =>
              (label, materialize(ast, Map(id -> n)))
          }
        )
        .optional
    case n @ Ast.Sum(id, _, elems, false, true) =>
      Schema
        .Enumeration(
          ListMap.empty ++ elems.map {
            case (label, ast) =>
              (label, materialize(ast, Map(id -> n)))
          }
        )
        .repeated
    case _ => Schema.Fail("AST cannot be materiazlied to a Schema")
  }

  implicit lazy val schema: Schema[Ast] = DeriveSchema.gen[Ast]

}
