package zio.schema.codec.xml

/**
 * Annotation for specifying XML namespace information on a type or field.
 *
 * When applied to a case class or a field, adds namespace information to the corresponding XML element.
 *
 * {{{
 * @xmlNamespace("http://example.com/ns", Some("ex"))
 * case class Item(name: String)
 * // Encodes as: <ex:Item xmlns:ex="http://example.com/ns"><name>Alice</name></ex:Item>
 * }}}
 *
 * @param uri
 *   The namespace URI.
 * @param prefix
 *   An optional namespace prefix.
 */
final case class xmlNamespace(uri: String, prefix: Option[String] = None) extends scala.annotation.StaticAnnotation
