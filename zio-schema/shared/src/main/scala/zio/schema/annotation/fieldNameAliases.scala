package zio.schema.annotation

/**
 * Annotation for specifying a list of aliases if the field is somehow known by.
 *
 * @param aliases
 *   A variable-length sequence of Strings representing the aliases of the
 *   field.
 */
final case class fieldNameAliases(aliases: String*) extends scala.annotation.StaticAnnotation
