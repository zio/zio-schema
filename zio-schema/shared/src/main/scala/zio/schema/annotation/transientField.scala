package zio.schema.annotation

/**
 * Annotation specifying that serialization should make no effort to encode the
 * field to which the annotation is applied.
 *
 * This is the dual of `@optionalField`.
 */
final case class transientField() extends scala.annotation.StaticAnnotation
