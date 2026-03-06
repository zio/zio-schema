package zio.schema.codec.xml

/**
 * Annotation for specifying that a field should be encoded as an XML attribute on the parent element instead of a child
 * element.
 *
 * When applied to a field in a case class, the field value will be rendered as an attribute of the enclosing XML
 * element. The field must have a primitive type since XML attributes can only hold string values.
 *
 * {{{
 * case class Person(
 *   @xmlAttribute id: Int,
 *   name: String
 * )
 * // Encodes as: <Person id="42"><name>Alice</name></Person>
 * }}}
 */
final case class xmlAttribute() extends scala.annotation.StaticAnnotation
