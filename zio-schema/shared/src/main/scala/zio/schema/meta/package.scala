package zio.schema

import zio.constraintless.TypeList._

package object meta {

  /**
   * A path to a node in a [[MetaSchema]] tree, as a `Chunk[String]` of labels
   * from the root. The empty path is the root.
   */
  type NodePath = NodePath.Type

  /**
   * A `MetaSchema` is a schema described as plain data: a tree of
   * [[ExtensibleMetaSchema]] nodes (records, enums, collections, primitives and
   * references) that has a schema of its own, so it can be serialized, stored
   * and compared. Use it to send a schema to another process, to keep the
   * schema of stored data next to the data, or to derive a [[Migration]]
   * between two versions of a schema.
   *
   * `MetaSchema` is the [[ExtensibleMetaSchema]] whose only built-in type is
   * [[zio.schema.DynamicValue]]. [[ExtensibleMetaSchema#toSchema]] turns it
   * back into a `Schema[_]` that works on generic values: a record becomes a
   * `ListMap[String, _]`.
   *
   * {{{
   * import zio.schema._
   * import zio.schema.meta._
   *
   * val meta: MetaSchema = MetaSchema.fromSchema(Schema[Map[String, List[Int]]])
   *
   * val stored: DynamicValue                = MetaSchema.schema.toDynamic(meta)
   * val restored: Either[String, MetaSchema] = MetaSchema.schema.fromDynamic(stored)
   * // restored == Right(meta)
   *
   * val rebuilt: Schema[_] = meta.toSchema
   * }}}
   */
  type MetaSchema = ExtensibleMetaSchema[DynamicValue :: End]

  /**
   * Builds a [[MetaSchema]] from a schema, and holds the schema of `MetaSchema`
   * itself.
   */
  object MetaSchema {

    /**
     * A labelled child of a [[MetaSchema]] node: a record field or an enum
     * case.
     */
    type Labelled = ExtensibleMetaSchema.Labelled[DynamicValue :: End]

    /**
     * The schema of [[MetaSchema]] itself, used to encode a meta schema with
     * any codec or to turn it into a [[zio.schema.DynamicValue]].
     */
    val schema: Schema[MetaSchema] = ExtensibleMetaSchema.schema[DynamicValue :: End]

    /**
     * Describes `schema` as a [[MetaSchema]] tree. A schema that refers back to
     * itself, such as a recursive case class, becomes a tree with `Ref` nodes
     * at the points of recursion.
     *
     * {{{
     * import zio.schema._
     * import zio.schema.meta._
     *
     * val meta: MetaSchema = MetaSchema.fromSchema(Schema[Option[Int]])
     * // meta.optional == true
     * }}}
     */
    def fromSchema[A](schema: Schema[A]): MetaSchema = ExtensibleMetaSchema.fromSchema[A, DynamicValue :: End](schema)
  }
}
