package zio.schema.codec

import scala.annotation.StaticAnnotation

object XmlAnnotations {

  // Custom element or attribute name
  final case class name(name: String) extends StaticAnnotation

  // Namespace URI
  final case class namespace(uri: String) extends StaticAnnotation

  // Namespace prefix
  final case class namespacePrefix(prefix: String) extends StaticAnnotation

  // Encode as XML attribute
  final case class attribute() extends StaticAnnotation

  // Encode as text content
  final case class text() extends StaticAnnotation

  final case class wrapped() extends StaticAnnotation

  // Custom element name for collection items
  final case class collectionElement(name: String) extends StaticAnnotation

  // Wrap text in CDATA section
  final case class cdata() extends StaticAnnotation

  // Omit when empty
  final case class omitEmpty() extends StaticAnnotation
}
