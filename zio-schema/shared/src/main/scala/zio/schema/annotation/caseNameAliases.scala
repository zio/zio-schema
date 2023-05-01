package zio.schema.annotation

/**
 * Annotation for specifying a list of aliases if the case name is somehow known by.
 *
 * @param aliases A variable-length sequence of Strings representing the aliases of the case.
 */
final case class caseNameAliases(aliases: String*) extends scala.annotation.StaticAnnotation
