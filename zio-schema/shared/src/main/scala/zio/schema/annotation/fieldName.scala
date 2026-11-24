package zio.schema.annotation

/**
 * Annotation for specifying an alternate name for a field of a case class. `Schema.Field#fieldName` returns this name instead of the field name, and codecs such as `JsonCodec` use it as the field's name when encoding and decoding.
 *
 * For example, when an API stores the user name as `user_name` but the case class field is named `userName`, annotate the field with `@fieldName("user_name")`: `JsonCodec` then encodes `User("ann")` as `{"user_name":"ann"}`.
 *
 * @param name
 *   The alternate name to use for the field.
 */
final case class fieldName(name: String) extends scala.annotation.StaticAnnotation
