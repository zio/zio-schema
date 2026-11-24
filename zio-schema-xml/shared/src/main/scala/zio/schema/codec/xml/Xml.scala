package zio.schema.codec.xml

import zio.Chunk

/**
 * A node of an XML document: an element, text, a CDATA section, a comment or a
 * processing instruction. `XmlReader.read` parses XML text into a tree of these
 * nodes, and `XmlWriter.write` renders one as text.
 */
sealed trait Xml

/** The XML node types. */
object Xml {

  /**
   * An element with a `name`, its `attributes` in document order, and its child
   * nodes.
   */
  final case class Element(name: XmlName, attributes: Chunk[(XmlName, String)], children: Chunk[Xml]) extends Xml

  /** A text node holding `value`. */
  final case class Text(value: String) extends Xml

  /** A CDATA section holding `value`. */
  final case class CData(value: String) extends Xml

  /** A comment holding `value`, without the comment delimiters. */
  final case class Comment(value: String) extends Xml

  /** A processing instruction with a `target` name and its `data` text. */
  final case class ProcessingInstruction(target: String, data: String) extends Xml

  /** Constructors for elements without a namespace or attributes. */
  object Element {

    /**
     * Creates an element named `name` with no namespace, attributes or
     * children.
     */
    def apply(name: String): Element = Element(XmlName(name), Chunk.empty, Chunk.empty)

    /**
     * Creates an element named `name` with no namespace or attributes and the
     * given children.
     */
    def apply(name: String, children: Xml*): Element =
      Element(XmlName(name), Chunk.empty, Chunk.fromIterable(children))
  }
}
