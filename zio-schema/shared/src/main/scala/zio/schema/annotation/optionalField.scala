package zio.schema.annotation

/**
 * Annotation specifying that deserialization should not fail even if a value of
 * this type is not specified.
 */
final case class optionalField() extends scala.annotation.StaticAnnotation
