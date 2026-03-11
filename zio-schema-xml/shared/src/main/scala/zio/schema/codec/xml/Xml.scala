package zio.schema.codec.xml

import zio.Chunk

sealed trait Xml

object Xml {

  final case class Element(name: XmlName, attributes: Chunk[(XmlName, String)], children: Chunk[Xml]) extends Xml

  final case class Text(value: String) extends Xml

  final case class CData(value: String) extends Xml

  final case class Comment(value: String) extends Xml

  final case class ProcessingInstruction(target: String, data: String) extends Xml

  object Element {
    def apply(name: String): Element = Element(XmlName(name), Chunk.empty, Chunk.empty)

    def apply(name: String, children: Xml*): Element =
      Element(XmlName(name), Chunk.empty, Chunk.fromIterable(children))
  }
}
