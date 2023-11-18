package zio.schema.annotation

import scala.annotation.StaticAnnotation

/**
 * Annotation for specifying an alternate identity for a case of an enum.
 * Currently, the subtype name is always used.
 *
 * This is the dual of `@fieldName`, but for sums rather than products.
 *
 * @param name The alternate name to use for the case.
 */
final case class caseName(name: String) extends StaticAnnotation
