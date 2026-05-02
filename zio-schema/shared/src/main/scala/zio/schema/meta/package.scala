package zio.schema

import zio.constraintless.TypeList._

package object meta {
  type NodePath = NodePath.Type

  type MetaSchema = ExtensibleMetaSchema[DynamicValue :: End]

  object MetaSchema {
    type Labelled = ExtensibleMetaSchema.Labelled[DynamicValue :: End]

    val schema: Schema[MetaSchema] = ExtensibleMetaSchema.schema[DynamicValue :: End]

    def fromSchema[A](schema: Schema[A]): MetaSchema = ExtensibleMetaSchema.fromSchema[A, DynamicValue :: End](schema)
  }
}
