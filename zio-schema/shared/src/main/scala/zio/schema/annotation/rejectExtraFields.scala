package zio.schema.annotation

/**
 * Annotation specifying that deserialization should reject payloads that
 * contain more fields than specified.
 */
final case class rejectExtraFields() extends scala.annotation.StaticAnnotation
