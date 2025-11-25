package zio.schema.annotation

import scala.annotation.StaticAnnotation

/**
 * When applied to a Schema.Dynamic schema, indicates that the dynamic value
 * should be directly mapped to the target codec if possible.
 *
 * For example the JSON codec can encode DynamicValue.Record to a JSON object
 * directly instead of encoding the DynamicValue structure.
 */
final case class directDynamicMapping() extends StaticAnnotation
