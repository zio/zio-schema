package zio.schema.annotation

/**
 * Annotation specifying the default value that should be utilized when the
 * field is not present during deserialization. This is similar to
 * `@optionalField`, except that the default value is user-defined rather than
 * computed automatically using the field schema.
 *
 * @param value
 *   The default value to use for the field.
 */
final case class fieldDefaultValue[A](value: A) extends scala.annotation.StaticAnnotation
