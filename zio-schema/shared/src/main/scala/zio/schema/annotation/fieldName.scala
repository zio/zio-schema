package zio.schema.annotation

/**
 * Annotation for specifying an alternate identity for a field of a case class.
 * Currently, the field name is always used. But sometimes, it can be convenient
 * to have control over that name.
 *
 * For example, the API expects username to be stored as user_name, but inside
 * the case class, the field is named username. Such an annotation, applied
 * directly to the field, could indicate a different identity for the field than
 * the field name itself.
 *
 * @param name
 *   The alternate name to use for the field.
 */
final case class fieldName(name: String) extends scala.annotation.StaticAnnotation
