package zio.schema.codec.xml

/**
 * Options for `XmlWriter.write`.
 *
 * @param indentStep
 *   The number of spaces per nesting level; with 0 (the default), the whole document is written on one line.
 * @param includeDeclaration
 *   Writes an XML declaration before the root element when `true`.
 * @param encoding
 *   The encoding name written in the XML declaration. `XmlCodec` encodes the text as UTF-8 whatever this value is.
 */
final case class WriterConfig(
  indentStep: Int = 0,
  includeDeclaration: Boolean = false,
  encoding: String = "UTF-8"
)

/** Predefined writer options. */
object WriterConfig {
  /** No indentation and no XML declaration. */
  val default: WriterConfig         = WriterConfig()
  /** Indents nested elements by 2 spaces per level. */
  val pretty: WriterConfig          = WriterConfig(indentStep = 2)
  /** Writes an XML declaration with version 1.0 and encoding UTF-8 before the root element, with no indentation. */
  val withDeclaration: WriterConfig = WriterConfig(includeDeclaration = true)
}
