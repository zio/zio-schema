package zio.schema.annotation

/**
 * Annotation specifying that serialization should make no effort to encode the
 * case to which the annotation is applied.
 */
case class transientCase() extends scala.annotation.StaticAnnotation
