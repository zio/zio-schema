package zio.schema.meta

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable

import zio.constraintless.TypeList
import zio.prelude._
import zio.schema._
import zio.schema.annotation.simpleEnum
import zio.{Chunk, ChunkBuilder}

/**
 * A schema described as a tree of plain data nodes, which can itself be
 * serialized with [[ExtensibleMetaSchema.schema]]. [[MetaSchema]] is the
 * version used in most code; this trait adds the type list `BuiltIn` of types
 * that are kept by reference instead of being expanded.
 *
 * When [[ExtensibleMetaSchema.fromSchema]] meets a record, enum or dynamic
 * schema equal to one in [[builtInInstances]], it writes a
 * [[ExtensibleMetaSchema.Known]] node with that schema's [[zio.schema.TypeId]],
 * and [[toSchema]] puts the built-in schema back. The other nodes describe
 * records ([[ExtensibleMetaSchema.Product]]), enums
 * ([[ExtensibleMetaSchema.Sum]]), primitives ([[ExtensibleMetaSchema.Value]]),
 * collections, tuples, `Either`, `Fallback`, failures and recursive
 * references ([[ExtensibleMetaSchema.Ref]]).
 *
 * `toString` renders the tree as indented text, for example `product`
 * followed by one `|--name: type` line per field.
 *
 * @tparam BuiltIn
 *   the type list of built-in types, such as `DynamicValue :: End`
 */
sealed trait ExtensibleMetaSchema[BuiltIn <: TypeList] { self =>
  /** The schemas of the built-in types listed in `BuiltIn`. */
  def builtInInstances: SchemaInstances[BuiltIn]

  /** The path from the root of the tree to this node. */
  def path: NodePath
  /**
   * Returns `true` when the described value is wrapped in an `Option`. A
   * `Schema.Optional` becomes this flag on its inner node, not a node of its
   * own.
   */
  def optional: Boolean

  /**
   * Builds a schema from this tree. The result works on generic values: a
   * record node becomes a schema of `ListMap[String, _]`, an enum node a schema
   * of `Any`, a list node a schema of `Chunk`, and a built-in node the schema
   * from [[builtInInstances]] with the same type id (or `Schema.Fail` when
   * none has it). Transformations, annotations and validations of the
   * original schema are not part of the tree, so they are not in the result.
   */
  def toSchema: Schema[_] = {
    val refMap = mutable.HashMap.empty[NodePath, Schema[_]]
    ExtensibleMetaSchema.materialize(self, refMap)(builtInInstances)
  }

  override def toString: String = AstRenderer.render(self)
}

/** The node types of [[ExtensibleMetaSchema]] and the conversion from a schema. */
object ExtensibleMetaSchema {
  import CaseSet._

  /** A child node with its label: a record field name or an enum case id. */
  final case class Labelled[BuiltIn <: TypeList](label: String, schema: ExtensibleMetaSchema[BuiltIn])

  /** Holds the schema of [[Labelled]]. */
  object Labelled {
    /** The schema of [[Labelled]], encoded as a pair of label and node. */
    implicit def schema[BuiltIn <: TypeList](implicit
      builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[Labelled[BuiltIn]] =
      Schema
        .defer(Schema.tuple2(Schema[String], ExtensibleMetaSchema.schema[BuiltIn]))
        .transform(tuple => Labelled(tuple._1, tuple._2), l => (l.label, l.schema))
  }

  /**
   * The record and enum schemas on the way from the root to the current node,
   * as pairs of the schema's `hashCode` and its node path. Building a tree
   * from a schema uses it to find recursion: a schema already in the lineage
   * becomes a [[Ref]] to its path.
   */
  final case class Lineage(paths: Chunk[(Int, NodePath)]) {
    /** Returns this lineage with `pair` appended. */
    def :+(pair: (Int, NodePath)): Lineage = Lineage(paths :+ pair)
  }

  /** Constructors and the schema of [[Lineage]]. */
  object Lineage {
    /** Creates a lineage from the given pairs. */
    def apply(paths: (Int, NodePath)*): Lineage = Lineage(Chunk.fromIterable(paths))

    /** The lineage with no entries. */
    lazy val empty: Lineage = Lineage(Chunk.empty)

    /** The schema of [[Lineage]], encoded as a chunk of pairs. */
    implicit lazy val schema: Schema[Lineage] =
      Schema.chunk(Schema.tuple2(Schema[Int], Schema[NodePath])).transform(Lineage(_), _.paths)
  }

  /** The schema of [[NodePath]], encoded as a sequence of strings. */
  implicit val nodePathSchema: Schema[NodePath] =
    Schema[String].repeated
      .transform(NodePath(_), NodePath.unwrap)

  /**
   * A record node: the record's type id and one labelled child per field, in
   * field order. `fromSchema` creates it for a `Schema.Record`.
   */
  final case class Product[BuiltIn <: TypeList](
    id: TypeId,
    override val path: NodePath,
    fields: Chunk[Labelled[BuiltIn]] = Chunk.empty,
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  /** Holds the schema of [[Product]] nodes. */
  object Product {
    /** The schema of `Product` nodes. */
    implicit def schema[BuiltIn <: TypeList](implicit
      builtInInstances: SchemaInstances[BuiltIn]
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
  /**
   * A pair node with the children `left` and `right`. `fromSchema` creates it
   * for a `Schema.Tuple2`.
   */
  final case class Tuple[BuiltIn <: TypeList](
    override val path: NodePath,
    left: ExtensibleMetaSchema[BuiltIn],
    right: ExtensibleMetaSchema[BuiltIn],
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  /** Holds the schema of [[Tuple]] nodes. */
  object Tuple {
    /** The schema of `Tuple` nodes. */
    implicit def schema[BuiltIn <: TypeList](implicit
      builtInInstances: SchemaInstances[BuiltIn]
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

  /**
   * An enum node: the enum's type id and one labelled child per case, labelled
   * with the case id. `fromSchema` creates it for a `Schema.Enum`.
   */
  final case class Sum[BuiltIn <: TypeList](
    id: TypeId,
    override val path: NodePath,
    cases: Chunk[Labelled[BuiltIn]] = Chunk.empty,
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  /** Holds the schema of [[Sum]] nodes. */
  object Sum {
    /** The schema of `Sum` nodes. */
    implicit def schema[BuiltIn <: TypeList](implicit
      builtInInstances: SchemaInstances[BuiltIn]
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

  /**
   * An `Either` node with the children `left` and `right`. `fromSchema` creates
   * it for a `Schema.Either`, and for a `Schema.Fallback` that is not at the
   * root.
   */
  final case class Either[BuiltIn <: TypeList](
    override val path: NodePath,
    left: ExtensibleMetaSchema[BuiltIn],
    right: ExtensibleMetaSchema[BuiltIn],
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  /** Holds the schema of [[Either]] nodes. */
  object Either {
    /** The schema of `Either` nodes. */
    implicit def schema[BuiltIn <: TypeList](implicit
      builtInInstances: SchemaInstances[BuiltIn]
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

  /**
   * A `Fallback` node with the children `left` and `right`. `fromSchema` creates
   * it for a `Schema.Fallback` at the root of the schema.
   */
  final case class Fallback[BuiltIn <: TypeList](
    override val path: NodePath,
    left: ExtensibleMetaSchema[BuiltIn],
    right: ExtensibleMetaSchema[BuiltIn],
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  /** Holds the schema of [[Fallback]] nodes. */
  object Fallback {
    /** The schema of `Fallback` nodes. */
    implicit def schema[BuiltIn <: TypeList](implicit
      builtInInstances: SchemaInstances[BuiltIn]
    ): Schema[Fallback[BuiltIn]] =
      Schema.CaseClass4(
        TypeId.parse("zio.schema.meta.MetaSchema.Fallback"),
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
        ) => Fallback(NodePath(path), left, right, optional)
      )

  }

  /**
   * A node for a `Schema.Fail`, holding its error message.
   */
  final case class FailNode[BuiltIn <: TypeList](
    message: String,
    override val path: NodePath,
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  /** Holds the schema of [[FailNode]] nodes. */
  object FailNode {
    /** The schema of `FailNode` nodes. */
    implicit def schema[BuiltIn <: TypeList](implicit
      builtInInstances: SchemaInstances[BuiltIn]
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

  /**
   * A collection node with the child `item`. `fromSchema` creates it for a
   * `Schema.Sequence`, a `Schema.NonEmptySequence` and a `Schema.Set`.
   */
  final case class ListNode[BuiltIn <: TypeList](
    item: ExtensibleMetaSchema[BuiltIn],
    override val path: NodePath,
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  /** Holds the schema of [[ListNode]] nodes. */
  object ListNode {
    /** The schema of `ListNode` nodes. */
    implicit def schema[BuiltIn <: TypeList](implicit
      builtInInstances: SchemaInstances[BuiltIn]
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

  /**
   * A map node with the children `keys` and `values`. `fromSchema` creates it
   * for a `Schema.Map` and a `Schema.NonEmptyMap`.
   */
  final case class Dictionary[BuiltIn <: TypeList](
    keys: ExtensibleMetaSchema[BuiltIn],
    values: ExtensibleMetaSchema[BuiltIn],
    override val path: NodePath,
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  /** Holds the schema of [[Dictionary]] nodes. */
  object Dictionary {
    /** The schema of `Dictionary` nodes. */
    implicit def schema[BuiltIn <: TypeList](implicit
      builtInInstances: SchemaInstances[BuiltIn]
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

  /**
   * A primitive node holding its [[zio.schema.StandardType]]. `fromSchema`
   * creates it for a `Schema.Primitive`.
   */
  final case class Value[BuiltIn <: TypeList](
    valueType: StandardType[_],
    override val path: NodePath = NodePath.root,
    override val optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  /** Holds the schema of [[Value]] nodes. */
  object Value {
    /** The schema of `Value` nodes. */
    implicit def schema[BuiltIn <: TypeList](implicit
      builtInInstances: SchemaInstances[BuiltIn]
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
  /**
   * A reference to the ancestor node at `refPath`, written where a record or
   * enum schema refers back to itself.
   */
  final case class Ref[BuiltIn <: TypeList](
    refPath: NodePath,
    override val path: NodePath,
    optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  /** Holds the schema of [[Ref]] nodes. */
  object Ref {
    /** The schema of `Ref` nodes. */
    implicit def schema[BuiltIn <: TypeList](implicit
      builtInInstances: SchemaInstances[BuiltIn]
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

  /**
   * A built-in type, kept as its type id instead of being expanded. `toSchema`
   * replaces it with the schema in `builtInInstances` that has the same type id.
   */
  final case class Known[BuiltIn <: TypeList](
    typeId: TypeId,
    override val path: NodePath,
    optional: Boolean = false
  )(implicit val builtInInstances: SchemaInstances[BuiltIn])
      extends ExtensibleMetaSchema[BuiltIn]

  /** Holds the schema of [[Known]] nodes. */
  object Known {
    /** The schema of `Known` nodes. */
    implicit def schema[BuiltIn <: TypeList](implicit
      builtInInstances: SchemaInstances[BuiltIn]
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

  /**
   * Describes `schema` as a tree whose root has the path `NodePath.root`.
   * Record, enum and dynamic schemas equal to one in `builtInInstances` become
   * [[Known]] nodes, and a record or enum that refers back to itself becomes a
   * [[Ref]] at the point of recursion. A `Schema.Transform` and a `Schema.Lazy`
   * are described by the schema they wrap.
   *
   * A `Schema.Fallback` at the root becomes a [[Fallback]] node; a
   * `Schema.Fallback` nested inside another schema becomes an [[Either]] node.
   */
  @tailrec
  def fromSchema[A, BuiltIn <: TypeList](
    schema: Schema[A]
  )(implicit builtInInstances: SchemaInstances[BuiltIn]): ExtensibleMetaSchema[BuiltIn] =
    getBuiltInTypeId(builtInInstances, schema) match {
      case Some(typeId) => Known(typeId, NodePath.root)
      case None         =>
        schema match {
          case Schema.Primitive(typ, _)      => Value(typ, NodePath.root)
          case Schema.Fail(message, _)       => FailNode(message, NodePath.root)
          case Schema.Optional(schema, _)    => subtree(NodePath.root, Lineage.empty, schema, optional = true)
          case Schema.Either(left, right, _) =>
            Either(
              NodePath.root,
              subtree(NodePath.root / "left", Lineage.empty, left),
              subtree(NodePath.root / "right", Lineage.empty, right)
            )
          case Schema.Fallback(left, right, _, _) =>
            Fallback(
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
          case Schema.NonEmptySequence(schema, _, _, _, _) =>
            ListNode(item = subtree(NodePath.root / "item", Lineage.empty, schema), NodePath.root)
          case Schema.Map(ks, vs, _) =>
            Dictionary(
              keys = subtree(NodePath.root / "keys", Lineage.empty, ks),
              values = subtree(NodePath.root / "values", Lineage.empty, vs),
              NodePath.root
            )
          case Schema.NonEmptyMap(ks, vs, _) =>
            Dictionary(
              keys = subtree(NodePath.root / "keys", Lineage.empty, ks),
              values = subtree(NodePath.root / "values", Lineage.empty, vs),
              NodePath.root
            )
          case Schema.Set(schema, _) =>
            ListNode(item = subtree(NodePath.root / "item", Lineage.empty, schema), NodePath.root)
          case Schema.Transform(schema, _, _, _, _) => subtree(NodePath.root, Lineage.empty, schema)
          case lzy @ Schema.Lazy(_)                 => fromSchema(lzy.schema)
          case s: Schema.Record[A]                  =>
            s.fields
              .foldLeft(NodeBuilder(NodePath.root, Lineage(s.hashCode() -> NodePath.root))) { (node, field) =>
                node.addLabelledSubtree(field.name, field.schema)
              }
              .buildProduct(s.id)
          case s: Schema.Enum[A] =>
            s.cases
              .foldLeft(NodeBuilder(NodePath.root, Lineage(s.hashCode() -> NodePath.root))) { case (node, caseValue) =>
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
      .map { case (_, refPath) =>
        Ref(refPath, path, optional)
      }
      .getOrElse {
        getBuiltInTypeId(builtInInstances, schema) match {
          case Some(typeId) => Known(typeId, path, optional)
          case None         =>
            schema match {
              case Schema.Primitive(typ, _)      => Value(typ, path, optional)
              case Schema.Optional(schema, _)    => subtree(path, lineage, schema, optional = true)
              case Schema.Either(left, right, _) =>
                Either(
                  path,
                  subtree(path / "left", lineage, left, optional = false),
                  subtree(path / "right", lineage, right, optional = false),
                  optional
                )
              case Schema.Fallback(left, right, _, _) =>
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
              case Schema.NonEmptySequence(schema, _, _, _, _) =>
                ListNode(item = subtree(path / "item", lineage, schema, optional = false), path, optional)
              case Schema.Map(ks, vs, _) =>
                Dictionary(
                  keys = subtree(path / "keys", Lineage.empty, ks, optional = false),
                  values = subtree(path / "values", Lineage.empty, vs, optional = false),
                  path,
                  optional
                )
              case Schema.NonEmptyMap(ks, vs, _) =>
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
              case s: Schema.Record[_]                  =>
                s.fields
                  .foldLeft(NodeBuilder(path, lineage :+ (s.hashCode() -> path), optional)) { (node, field) =>
                    node.addLabelledSubtree(field.name, field.schema)
                  }
                  .buildProduct(s.id)
              case s: Schema.Enum[_] =>
                s.cases
                  .foldLeft(NodeBuilder(path, lineage :+ (s.hashCode() -> path), optional)) { case (node, caseValue) =>
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
      case ExtensibleMetaSchema.Ref(refPath, _, _)  =>
        def resolve[A](): Schema[A] =
          refs.getOrElse(refPath, Schema.Fail(s"invalid ref path $refPath")).asInstanceOf[Schema[A]]
        Schema.defer(resolve())
      case ExtensibleMetaSchema.Product(id, _, elems, _) =>
        Schema.record(
          id,
          elems.map { case Labelled(label, ast) =>
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
      case ExtensibleMetaSchema.Sum(id, _, elems, _) => {
        val (cases, isSimple) = elems.foldRight[(CaseSet.Aux[Any], Boolean)]((CaseSet.Empty[Any](), true)) {
          case (Labelled(label, ast), (acc, wasSimple)) => {
            val _case: Schema.Case[Any, Any] = Schema
              .Case[Any, Any](
                label,
                materialize(ast, refs).asInstanceOf[Schema[Any]],
                identity[Any],
                identity[Any],
                _.isInstanceOf[Any],
                Chunk.empty
              )
            val isSimple: Boolean = _case.schema match {
              case _: Schema.CaseClass0[_] => true
              case _                       => false
            }
            (CaseSet.Cons(_case, acc), wasSimple && isSimple)
          }
        }
        Schema.enumeration[Any, CaseSet.Aux[Any]](
          id,
          cases,
          if (isSimple) Chunk(new simpleEnum(true)) else Chunk.empty
        )
      }
      case ExtensibleMetaSchema.Either(_, left, right, _) =>
        Schema.either(
          materialize(left, refs),
          materialize(right, refs)
        )
      case ExtensibleMetaSchema.Fallback(_, left, right, _) =>
        Schema.Fallback(
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
    }

    refs += ast.path -> baseSchema

    if (ast.optional) baseSchema.optional else baseSchema
  }

  /**
   * The schema of [[ExtensibleMetaSchema]] trees: an enum with one case per
   * node type. Use it to encode a tree with a codec or to convert it to a
   * [[zio.schema.DynamicValue]].
   */
  implicit def schema[BuiltIn <: TypeList](implicit
    builtInInstances: SchemaInstances[BuiltIn]
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

  /** A ZIO Prelude `Equal` for [[MetaSchema]] that uses `==`. */
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
