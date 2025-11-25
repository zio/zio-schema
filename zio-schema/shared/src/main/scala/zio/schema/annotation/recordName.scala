package zio.schema.annotation

import scala.annotation.StaticAnnotation

/**
 * Annotation for specifying an alternate name for a record.
 *
 * @param name
 *   The alternate name to use for the record.
 */
final case class recordName(name: String) extends StaticAnnotation
