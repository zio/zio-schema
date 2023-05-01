package zio.schema.annotation

/**
 * Annotation used to specify the name of a field that will be used to contain the id which identifies
 * which term in an enum is being serialized / deserialized.
 *
 * For example, if you set `@discriminatorName("type")`, then a field called "type" will be used to store
 * the identity of the case of the enum that is being serialized / deserialized.
 *
 * @param tag the name of the field that will be used to store the identity of the case of the enum
 */
final case class discriminatorName(tag: String) extends scala.annotation.StaticAnnotation
