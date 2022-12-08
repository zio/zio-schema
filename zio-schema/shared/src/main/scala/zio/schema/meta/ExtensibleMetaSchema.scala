package zio.schema.meta

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable

import zio.constraintless.TypeList
import zio.prelude._
import zio.schema._
import zio.{ Chunk, ChunkBuilder }

sealed trait ExtensibleMetaSchema[BuiltIn <: TypeList] { self =>
  def builtInInstances: SchemaInstances[BuiltIn]

  def path: NodePath
  def optional: Boolean

  def toSchema: Schema[_] = {
    val refMap = mutable.HashMap.empty[NodePath, Schema[_]]
    ExtensibleMetaSchema.materialize(self, refMap)(builtInInstances)
  }

  override def toString: String = AstRenderer.render(self)
}

object ExtensibleMetaSchema {
  import CaseSet._

  final case class Labelled[BuiltIn <: TypeList](label: String, schema: ExtensibleMetaSchema[BuiltIn])

  object Labelled {
    implicit def schema[BuiltIn <: TypeList](
      implicit builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[Labelled[BuiltIn]] =
      Schema
        .defer(Schema.tuple2(Schema[String], ExtensibleMetaSchema.schema[BuiltIn]))
        .transform(tuple => Labelled(tuple._1, tuple._2), l => (l.label, l.schema))
  }

  final case class Lineage(paths: Chunk[(Int, NodePath)]) {
    def :+(pair: (Int, NodePath)): Lineage = Lineage(paths :+ pair)
  }

  object Lineage {
    def apply(paths: (Int, NodePath)*): Lineage = Lineage(Chunk.fromIterable(paths))

    lazy val empty: Lineage = Lineage(Chunk.empty)

    implicit lazy val schema: Schema[Lineage] =
      Schema.chunk(Schema.tuple2(Schema[Int], Schema[NodePath])).transform(Lineage(_), _.paths)
  }

  implicit val nodePathSchema: Schema[NodePath] =
    Schema[String].repeated
      .transform(NodePath(_), NodePath.unwrap)

  final case class Product[BuiltIn <: TypeList](
    id: TypeId,
    override val path: NodePath,
    fields: Chunk[Labelled[BuiltIn]] = Chunk.empty,
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  object Product {
    implicit def schema[BuiltIn <: TypeList](
      implicit builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[Product[BuiltIn]] =
      Schema.CaseClass4(
        TypeId.parse("zio.schema.meta.MetaSchema.Product"),
        field01 = Schema.Field("id", Schema[TypeId], get0 = _.id, set0 = (a, value: TypeId) => a.copy(id = value)),
        field02 = Schema
          .Field(
            "path",
            Schema[String].repeated,
            get0 = _.path,
            set0 = (p, value: Chunk[String]) => p.copy(path = NodePath(value))
          ),
        field03 = Schema
          .Field(
            "fields",
            Schema[Labelled[BuiltIn]].repeated,
            get0 = _.fields,
            set0 = (a: Product[BuiltIn], value: Chunk[Labelled[BuiltIn]]) => a.copy(fields = value)
          ),
        field04 = Schema
          .Field(
            "optional",
            Schema[Boolean],
            get0 = _.optional,
            set0 = (a, value: Boolean) => a.copy(optional = value)
          ),
        (id: TypeId, path: Chunk[String], fields: Chunk[Labelled[BuiltIn]], optional: Boolean) =>
          Product(id, NodePath(path), fields, optional)
      )

  }
  final case class Tuple[BuiltIn <: TypeList](
    override val path: NodePath,
    left: ExtensibleMetaSchema[BuiltIn],
    right: ExtensibleMetaSchema[BuiltIn],
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  object Tuple {
    implicit def schema[BuiltIn <: TypeList](
      implicit builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[Tuple[BuiltIn]] =
      Schema.CaseClass4(
        TypeId.parse("zio.schema.meta.MetaSchema.Tuple"),
        field01 = Schema
          .Field(
            "path",
            Schema[String].repeated,
            get0 = _.path,
            set0 = (a, value: Chunk[String]) => a.copy(path = NodePath(value))
          ),
        field02 = Schema
          .Field(
            "left",
            Schema[ExtensibleMetaSchema[BuiltIn]],
            get0 = _.left,
            set0 = (a, value: ExtensibleMetaSchema[BuiltIn]) => a.copy(left = value)
          ),
        field03 = Schema
          .Field(
            "right",
            Schema[ExtensibleMetaSchema[BuiltIn]],
            get0 = _.right,
            set0 = (a, value: ExtensibleMetaSchema[BuiltIn]) => a.copy(right = value)
          ),
        field04 = Schema
          .Field(
            "optional",
            Schema[Boolean],
            get0 = _.optional,
            set0 = (a, value: Boolean) => a.copy(optional = value)
          ),
        (
          path: Chunk[String],
          left: ExtensibleMetaSchema[BuiltIn],
          right: ExtensibleMetaSchema[BuiltIn],
          optional: Boolean
        ) => Tuple(NodePath(path), left, right, optional)
      )

  }

  final case class Sum[BuiltIn <: TypeList](
    id: TypeId,
    override val path: NodePath,
    cases: Chunk[Labelled[BuiltIn]] = Chunk.empty,
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  object Sum {
    implicit def schema[BuiltIn <: TypeList](
      implicit builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[Sum[BuiltIn]] =
      Schema.CaseClass4(
        TypeId.parse("zio.schema.meta.MetaSchema.Sum"),
        field01 = Schema.Field("id", Schema[TypeId], get0 = _.id, set0 = (a, value: TypeId) => a.copy(id = value)),
        field02 = Schema
          .Field(
            "path",
            Schema[String].repeated,
            get0 = _.path,
            set0 = (a, value: Chunk[String]) => a.copy(path = NodePath(value))
          ),
        field03 = Schema.Field(
          "cases",
          Schema[Labelled[BuiltIn]].repeated,
          get0 = _.cases,
          set0 = (a, value: Chunk[Labelled[BuiltIn]]) => a.copy(cases = value)
        ),
        field04 = Schema
          .Field(
            "optional",
            Schema[Boolean],
            get0 = _.optional,
            set0 = (a, value: Boolean) => a.copy(optional = value)
          ),
        (id: TypeId, path: Chunk[String], fields: Chunk[Labelled[BuiltIn]], optional: Boolean) =>
          Sum(id, NodePath(path), fields, optional)
      )
  }

  final case class Either[BuiltIn <: TypeList](
    override val path: NodePath,
    left: ExtensibleMetaSchema[BuiltIn],
    right: ExtensibleMetaSchema[BuiltIn],
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  object Either {
    implicit def schema[BuiltIn <: TypeList](
      implicit builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[Either[BuiltIn]] =
      Schema.CaseClass4(
        TypeId.parse("zio.schema.meta.MetaSchema.Either"),
        field01 = Schema
          .Field(
            "path",
            Schema[String].repeated,
            get0 = _.path,
            set0 = (a, value: Chunk[String]) => a.copy(path = NodePath(value))
          ),
        field02 = Schema
          .Field(
            "left",
            Schema[ExtensibleMetaSchema[BuiltIn]],
            get0 = _.left,
            set0 = (a, value: ExtensibleMetaSchema[BuiltIn]) => a.copy(left = value)
          ),
        field03 = Schema
          .Field(
            "right",
            Schema[ExtensibleMetaSchema[BuiltIn]],
            get0 = _.right,
            set0 = (a, value: ExtensibleMetaSchema[BuiltIn]) => a.copy(right = value)
          ),
        field04 = Schema
          .Field(
            "optional",
            Schema[Boolean],
            get0 = _.optional,
            set0 = (a, value: Boolean) => a.copy(optional = value)
          ),
        (
          path: Chunk[String],
          left: ExtensibleMetaSchema[BuiltIn],
          right: ExtensibleMetaSchema[BuiltIn],
          optional: Boolean
        ) => Either(NodePath(path), left, right, optional)
      )

  }

  final case class FailNode[BuiltIn <: TypeList](
    message: String,
    override val path: NodePath,
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  object FailNode {
    implicit def schema[BuiltIn <: TypeList](
      implicit builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[FailNode[BuiltIn]] =
      Schema.CaseClass3(
        TypeId.parse("zio.schema.meta.MetaSchema.FailNode"),
        field01 = Schema
          .Field("message", Schema[String], get0 = _.message, set0 = (a, value: String) => a.copy(message = value)),
        field02 = Schema.Field(
          "path",
          Schema[String].repeated,
          get0 = _.path,
          set0 = (a, value: Chunk[String]) => a.copy(path = NodePath(value))
        ),
        field03 = Schema
          .Field(
            "optional",
            Schema[Boolean],
            get0 = _.optional,
            set0 = (a, value: Boolean) => a.copy(optional = value)
          ),
        (m: String, path: Chunk[String], optional: Boolean) => FailNode(m, NodePath(path), optional)
      )
  }

  final case class ListNode[BuiltIn <: TypeList](
    item: ExtensibleMetaSchema[BuiltIn],
    override val path: NodePath,
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  object ListNode {
    implicit def schema[BuiltIn <: TypeList](
      implicit builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[ListNode[BuiltIn]] =
      Schema.CaseClass3(
        TypeId.parse("zio.schema.meta.MetaSchema.ListNode"),
        field01 = Schema.Field(
          "item",
          Schema[ExtensibleMetaSchema[BuiltIn]],
          get0 = _.item,
          set0 = (a, value: ExtensibleMetaSchema[BuiltIn]) => a.copy(item = value)
        ),
        field02 = Schema.Field(
          "path",
          Schema[String].repeated,
          get0 = _.path,
          set0 = (a, value: Chunk[String]) => a.copy(path = NodePath(value))
        ),
        field03 = Schema
          .Field(
            "optional",
            Schema[Boolean],
            get0 = _.optional,
            set0 = (a, value: Boolean) => a.copy(optional = value)
          ),
        (item: ExtensibleMetaSchema[BuiltIn], path: Chunk[String], optional: Boolean) =>
          ListNode(item, NodePath(path), optional)
      )
  }

  final case class Dictionary[BuiltIn <: TypeList](
    keys: ExtensibleMetaSchema[BuiltIn],
    values: ExtensibleMetaSchema[BuiltIn],
    override val path: NodePath,
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  object Dictionary {
    implicit def schema[BuiltIn <: TypeList](
      implicit builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[Dictionary[BuiltIn]] =
      Schema.CaseClass4(
        TypeId.parse("zio.schema.meta.MetaSchema.Dictionary"),
        field01 = Schema.Field(
          "keys",
          Schema[ExtensibleMetaSchema[BuiltIn]],
          get0 = _.keys,
          set0 = (a, value: ExtensibleMetaSchema[BuiltIn]) => a.copy(keys = value)
        ),
        field02 = Schema
          .Field(
            "values",
            Schema[ExtensibleMetaSchema[BuiltIn]],
            get0 = _.values,
            set0 = (a, value: ExtensibleMetaSchema[BuiltIn]) => a.copy(values = value)
          ),
        field03 = Schema.Field(
          "path",
          Schema[String].repeated,
          get0 = _.path,
          set0 = (a, value: Chunk[String]) => a.copy(path = NodePath(value))
        ),
        field04 = Schema
          .Field(
            "optional",
            Schema[Boolean],
            get0 = _.optional,
            set0 = (a, value: Boolean) => a.copy(optional = value)
          ),
        (
          keys: ExtensibleMetaSchema[BuiltIn],
          values: ExtensibleMetaSchema[BuiltIn],
          path: Chunk[String],
          optional: Boolean
        ) => Dictionary(keys, values, NodePath(path), optional)
      )
  }

  final case class Value[BuiltIn <: TypeList](
    valueType: StandardType[_],
    override val path: NodePath = NodePath.root,
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  object Value {
    implicit def schema[BuiltIn <: TypeList](
      implicit builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[Value[BuiltIn]] =
      Schema
        .CaseClass3[String, Chunk[String], Boolean, (String, Chunk[String], Boolean)](
          TypeId.parse("zio.schema.meta.MetaSchema.Value"),
          field01 =
            Schema.Field("valueType", Schema[String], get0 = _._1, set0 = (a, value: String) => (value, a._2, a._3)),
          field02 = Schema
            .Field(
              "path",
              Schema[String].repeated,
              get0 = _._2,
              set0 = (a, value: Chunk[String]) => (a._1, value, a._3)
            ),
          field03 =
            Schema.Field("optional", Schema[Boolean], get0 = _._3, set0 = (a, value: Boolean) => (a._1, a._2, value)),
          construct0 = (v, p, o) => (v, p, o)
        )
        .transformOrFail(tup => fromTuple(tup), tupled)

    private def tupled[BuiltIn <: TypeList](
      value: Value[BuiltIn]
    ): scala.Either[String, (String, Chunk[String], Boolean)] =
      Right((value.valueType.tag, value.path, value.optional))

    private def fromTuple[BuiltIn <: TypeList](
      tuple: (String, Chunk[String], Boolean)
    )(implicit builtInInstances: SchemaInstances[BuiltIn]): scala.Either[String, Value[BuiltIn]] = tuple match {
      case (s, path, optional) =>
        StandardType
          .fromString(s)
          .map(typ => Value(typ, NodePath(path), optional))
          .toRight(s"unkown standard type $s")
    }
  }
  final case class Ref[BuiltIn <: TypeList](
    refPath: NodePath,
    override val path: NodePath,
    optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  object Ref {
    implicit def schema[BuiltIn <: TypeList](
      implicit builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[Ref[BuiltIn]] =
      Schema.CaseClass3(
        TypeId.parse("zio.schema.meta.MetaSchema.Ref"),
        field01 = Schema.Field(
          "refPath",
          Schema[String].repeated,
          get0 = _.refPath,
          set0 = (a, value: Chunk[String]) => a.copy(refPath = NodePath(value))
        ),
        field02 = Schema
          .Field(
            "path",
            Schema[String].repeated,
            get0 = _.path,
            set0 = (a, value: Chunk[String]) => a.copy(path = NodePath(value))
          ),
        field03 = Schema
          .Field(
            "optional",
            Schema[Boolean],
            get0 = _.optional,
            set0 = (a, value: Boolean) => a.copy(optional = value)
          ),
        (refPath: Chunk[String], path: Chunk[String], optional: Boolean) =>
          Ref(NodePath(refPath), NodePath(path), optional)
      )
  }

  final case class Known[BuiltIn <: TypeList](
    typeId: TypeId,
    override val path: NodePath,
    optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  object Known {
    implicit def schema[BuiltIn <: TypeList](
      implicit builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[Known[BuiltIn]] =
      Schema.CaseClass3(
        TypeId.parse("zio.schema.meta.MetaSchema.Known"),
        field01 =
          Schema.Field("typeId", Schema[TypeId], get0 = _.typeId, set0 = (a, value: TypeId) => a.copy(typeId = value)),
        field02 = Schema
          .Field(
            "path",
            Schema[String].repeated,
            get0 = _.path,
            set0 = (a, value: Chunk[String]) => a.copy(path = NodePath(value))
          ),
        field03 = Schema
          .Field(
            "optional",
            Schema[Boolean],
            get0 = _.optional,
            set0 = (a, value: Boolean) => a.copy(optional = value)
          ),
        (typeId: TypeId, path: Chunk[String], optional: Boolean) => Known(typeId, NodePath(path), optional)
      )
  }

  final private[schema] case class NodeBuilder[BuiltIn <: TypeList](
    path: NodePath,
    lineage: Lineage,
    optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn]) { self =>
    private val children: ChunkBuilder[Labelled[BuiltIn]] = ChunkBuilder.make[Labelled[BuiltIn]]()

    def addLabelledSubtree(label: String, schema: Schema[_]): NodeBuilder[BuiltIn] = {
      children += Labelled(label, subtree(path / label, lineage, schema))
      self
    }

    def buildProduct(id: TypeId): Product[BuiltIn] = Product(id, path, children.result(), optional)

    def buildSum(id: TypeId): Sum[BuiltIn] = Sum(id, path, children.result(), optional)
  }

  @tailrec
  def fromSchema[A, BuiltIn <: TypeList](
    schema: Schema[A]
  )(implicit builtInInstances: SchemaInstances[BuiltIn]): ExtensibleMetaSchema[BuiltIn] =
    getBuiltInTypeId(builtInInstances, schema) match {
      case Some(typeId) => Known(typeId, NodePath.root)
      case None =>
        schema match {
          case Schema.Primitive(typ, _)   => Value(typ, NodePath.root)
          case Schema.Fail(message, _)    => FailNode(message, NodePath.root)
          case Schema.Optional(schema, _) => subtree(NodePath.root, Lineage.empty, schema, optional = true)
          case Schema.Either(left, right, _) =>
            Either(
              NodePath.root,
              subtree(NodePath.root / "left", Lineage.empty, left),
              subtree(NodePath.root / "right", Lineage.empty, right)
            )
          case Schema.Tuple2(left, right, _) =>
            Tuple(
              NodePath.root,
              subtree(NodePath.root / "left", Lineage.empty, left),
              subtree(NodePath.root / "right", Lineage.empty, right)
            )
          case Schema.Sequence(schema, _, _, _, _) =>
            ListNode(item = subtree(NodePath.root / "item", Lineage.empty, schema), NodePath.root)
          case Schema.Map(ks, vs, _) =>
            Dictionary(
              keys = subtree(NodePath.root / "keys", Lineage.empty, ks),
              values = subtree(NodePath.root / "values", Lineage.empty, vs),
              NodePath.root
            )
          case Schema.Set(schema, _) =>
            ListNode(item = subtree(NodePath.root / "item", Lineage.empty, schema), NodePath.root)
          case Schema.Transform(schema, _, _, _, _) => subtree(NodePath.root, Lineage.empty, schema)
          case lzy @ Schema.Lazy(_)                 => fromSchema(lzy.schema)
          case s: Schema.Record[A] =>
            s.fields
              .foldLeft(NodeBuilder(NodePath.root, Lineage(s.hashCode() -> NodePath.root))) { (node, field) =>
                node.addLabelledSubtree(field.name, field.schema)
              }
              .buildProduct(s.id)
          case s: Schema.Enum[A] =>
            s.cases
              .foldLeft(NodeBuilder(NodePath.root, Lineage(s.hashCode() -> NodePath.root))) {
                case (node, caseValue) =>
                  node.addLabelledSubtree(caseValue.id, caseValue.schema)
              }
              .buildSum(s.id)
          case Schema.Dynamic(_) => fromSchema(DynamicValue.schema)
        }
    }

  private[schema] def subtree[BuiltIn <: TypeList](
    path: NodePath,
    lineage: Lineage,
    schema: Schema[_],
    optional: Boolean = false
  )(implicit builtInInstances: SchemaInstances[BuiltIn]): ExtensibleMetaSchema[BuiltIn] =
    lineage.paths
      .find(_._1 == schema.hashCode())
      .map {
        case (_, refPath) =>
          Ref(refPath, path, optional)
      }
      .getOrElse {
        getBuiltInTypeId(builtInInstances, schema) match {
          case Some(typeId) => Known(typeId, path, optional)
          case None =>
            schema match {
              case Schema.Primitive(typ, _)   => Value(typ, path, optional)
              case Schema.Optional(schema, _) => subtree(path, lineage, schema, optional = true)
              case Schema.Either(left, right, _) =>
                Either(
                  path,
                  subtree(path / "left", lineage, left, optional = false),
                  subtree(path / "right", lineage, right, optional = false),
                  optional
                )
              case Schema.Tuple2(left, right, _) =>
                Tuple(
                  path,
                  subtree(path / "left", lineage, left, optional = false),
                  subtree(path / "right", lineage, right, optional = false),
                  optional
                )
              case Schema.Sequence(schema, _, _, _, _) =>
                ListNode(item = subtree(path / "item", lineage, schema, optional = false), path, optional)
              case Schema.Map(ks, vs, _) =>
                Dictionary(
                  keys = subtree(path / "keys", Lineage.empty, ks, optional = false),
                  values = subtree(path / "values", Lineage.empty, vs, optional = false),
                  path,
                  optional
                )
              case Schema.Set(schema @ _, _) =>
                ListNode(item = subtree(path / "item", lineage, schema, optional = false), path, optional)
              case Schema.Transform(schema, _, _, _, _) => subtree(path, lineage, schema, optional)
              case lzy @ Schema.Lazy(_)                 => subtree(path, lineage, lzy.schema, optional)
              case s: Schema.Record[_] =>
                s.fields
                  .foldLeft(NodeBuilder(path, lineage :+ (s.hashCode() -> path), optional)) { (node, field) =>
                    node.addLabelledSubtree(field.name, field.schema)
                  }
                  .buildProduct(s.id)
              case s: Schema.Enum[_] =>
                s.cases
                  .foldLeft(NodeBuilder(path, lineage :+ (s.hashCode() -> path), optional)) {
                    case (node, caseValue) =>
                      node.addLabelledSubtree(caseValue.id, caseValue.schema)
                  }
                  .buildSum(s.id)
              case Schema.Fail(message, _) => FailNode(message, path)
              case Schema.Dynamic(_)       => subtree(path, lineage, DynamicValue.schema, optional)
            }
        }
      }

  private[schema] def materialize[BuiltIn <: TypeList](
    ast: ExtensibleMetaSchema[BuiltIn],
    refs: mutable.Map[NodePath, Schema[_]]
  )(implicit builtInInstances: SchemaInstances[BuiltIn]): Schema[_] = {
    val baseSchema = ast match {
      case ExtensibleMetaSchema.Value(typ, _, _) =>
        Schema.Primitive(typ, Chunk.empty)
      case ExtensibleMetaSchema.FailNode(msg, _, _) => Schema.Fail(msg)
      case ExtensibleMetaSchema.Ref(refPath, _, _) =>
        Schema.defer(
          refs.getOrElse(refPath, Schema.Fail(s"invalid ref path $refPath"))
        )
      case ExtensibleMetaSchema.Product(id, _, elems, _) =>
        Schema.record(
          id,
          elems.map {
            case Labelled(label, ast) =>
              Schema.Field(
                label,
                materialize(ast, refs).asInstanceOf[Schema[Any]],
                get0 = (p: ListMap[String, _]) => p(label),
                set0 = (p: ListMap[String, _], v: Any) => p.updated(label, v)
              )
          }: _*
        )
      case ExtensibleMetaSchema.Tuple(_, left, right, _) =>
        Schema.tuple2(
          materialize(left, refs),
          materialize(right, refs)
        )
      case ExtensibleMetaSchema.Sum(id, _, elems, _) =>
        Schema.enumeration[Any, CaseSet.Aux[Any]](
          id,
          elems.foldRight[CaseSet.Aux[Any]](CaseSet.Empty[Any]()) {
            case (Labelled(label, ast), acc) =>
              val _case: Schema.Case[Any, Any] = Schema
                .Case[Any, Any](
                  label,
                  materialize(ast, refs).asInstanceOf[Schema[Any]],
                  identity[Any],
                  identity[Any],
                  _.isInstanceOf[Any],
                  Chunk.empty
                )
              CaseSet.Cons(_case, acc)
          }
        )
      case ExtensibleMetaSchema.Either(_, left, right, _) =>
        Schema.either(
          materialize(left, refs),
          materialize(right, refs)
        )
      case ExtensibleMetaSchema.ListNode(itemAst, _, _) =>
        Schema.chunk(materialize(itemAst, refs))
      case ExtensibleMetaSchema.Dictionary(keyAst, valueAst, _, _) =>
        Schema.Map(materialize(keyAst, refs), materialize(valueAst, refs), Chunk.empty)
      case ExtensibleMetaSchema.Known(typeId, _, _) =>
        builtInInstances.all.collectFirst {
          case record: Schema.Record[_] if record.id == typeId => record
          case e: Schema.Enum[_] if e.id == typeId             => e
          case dyn: Schema.Dynamic if dyn.id == typeId         => dyn
        }.getOrElse(Schema.Fail(s"invalid known type id $typeId"))
      case ast => Schema.Fail(s"AST cannot be materialized to a Schema:\n$ast")
    }

    refs += ast.path -> baseSchema

    if (ast.optional) baseSchema.optional else baseSchema
  }

  implicit def schema[BuiltIn <: TypeList](
    implicit builtInInstances: SchemaInstances[BuiltIn]
  ): Schema[ExtensibleMetaSchema[BuiltIn]] =
    Schema.defer {
      Schema.EnumN[ExtensibleMetaSchema[BuiltIn], CaseSet.Aux[ExtensibleMetaSchema[BuiltIn]]](
        TypeId.parse("zio.schema.meta.ExtensibleMetaSchema[BuiltIn"),
        caseOf[Value[BuiltIn], ExtensibleMetaSchema[BuiltIn]]("Value")(_.asInstanceOf[Value[BuiltIn]])(
          _.asInstanceOf[ExtensibleMetaSchema[BuiltIn]]
        )(_.isInstanceOf[Value[BuiltIn]]) ++
          caseOf[Sum[BuiltIn], ExtensibleMetaSchema[BuiltIn]]("Sum")(_.asInstanceOf[Sum[BuiltIn]])(
            _.asInstanceOf[ExtensibleMetaSchema[BuiltIn]]
          )(_.isInstanceOf[Sum[BuiltIn]]) ++
          caseOf[Either[BuiltIn], ExtensibleMetaSchema[BuiltIn]]("Either")(
            _.asInstanceOf[Either[BuiltIn]]
          )(
            _.asInstanceOf[ExtensibleMetaSchema[BuiltIn]]
          )(
            _.isInstanceOf[Either[BuiltIn]]
          ) ++
          caseOf[Product[BuiltIn], ExtensibleMetaSchema[BuiltIn]]("Product")(
            _.asInstanceOf[Product[BuiltIn]]
          )(
            _.asInstanceOf[ExtensibleMetaSchema[BuiltIn]]
          )(
            _.isInstanceOf[Product[BuiltIn]]
          ) ++
          caseOf[Tuple[BuiltIn], ExtensibleMetaSchema[BuiltIn]]("Tuple")(_.asInstanceOf[Tuple[BuiltIn]])(
            _.asInstanceOf[ExtensibleMetaSchema[BuiltIn]]
          )(_.isInstanceOf[Tuple[BuiltIn]]) ++
          caseOf[Ref[BuiltIn], ExtensibleMetaSchema[BuiltIn]]("Ref")(_.asInstanceOf[Ref[BuiltIn]])(
            _.asInstanceOf[ExtensibleMetaSchema[BuiltIn]]
          )(_.isInstanceOf[Ref[BuiltIn]]) ++
          caseOf[ListNode[BuiltIn], ExtensibleMetaSchema[BuiltIn]]("ListNode")(
            _.asInstanceOf[ListNode[BuiltIn]]
          )(
            _.asInstanceOf[ExtensibleMetaSchema[BuiltIn]]
          )(
            _.isInstanceOf[ListNode[BuiltIn]]
          ) ++
          caseOf[Dictionary[BuiltIn], ExtensibleMetaSchema[BuiltIn]]("Dictionary")(
            _.asInstanceOf[Dictionary[BuiltIn]]
          )(
            _.asInstanceOf[ExtensibleMetaSchema[BuiltIn]]
          )(
            _.isInstanceOf[Dictionary[BuiltIn]]
          ) ++
          caseOf[Known[BuiltIn], ExtensibleMetaSchema[BuiltIn]]("Known")(
            _.asInstanceOf[Known[BuiltIn]]
          )(
            _.asInstanceOf[ExtensibleMetaSchema[BuiltIn]]
          )(
            _.isInstanceOf[Known[BuiltIn]]
          ) ++
          caseOf[FailNode[BuiltIn], ExtensibleMetaSchema[BuiltIn]]("Fail")(
            _.asInstanceOf[FailNode[BuiltIn]]
          )(
            _.asInstanceOf[ExtensibleMetaSchema[BuiltIn]]
          )(
            _.isInstanceOf[FailNode[BuiltIn]]
          ),
        Chunk.empty
      )
    }

  implicit val equals: Equal[MetaSchema] = Equal.default

  private def getBuiltInTypeId[BuiltIn <: TypeList](
    instances: SchemaInstances[BuiltIn],
    schema: Schema[_]
  ): Option[TypeId] =
    if (instances.all.contains(schema)) {
      schema match {
        case record: Schema.Record[_] => Some(record.id)
        case e: Schema.Enum[_]        => Some(e.id)
        case dyn: Schema.Dynamic      => Some(dyn.id)
        case _                        => None
      }
    } else None
}
