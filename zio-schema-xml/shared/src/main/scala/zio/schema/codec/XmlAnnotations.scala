package zio.schema.codec

import scala.annotation.StaticAnnotation

object XmlAnnotations {

  final case class name(name: String) extends StaticAnnotation

  final case class namespace(uri: String) extends StaticAnnotation

  final case class namespacePrefix(prefix: String) extends StaticAnnotation

  final case class attribute() extends StaticAnnotation

  final case class text() extends StaticAnnotation

  final case class wrapped() extends StaticAnnotation

  final case class collectionElement(name: String) extends StaticAnnotation

  final case class cdata() extends StaticAnnotation

  final case class omitEmpty() extends StaticAnnotation
}
