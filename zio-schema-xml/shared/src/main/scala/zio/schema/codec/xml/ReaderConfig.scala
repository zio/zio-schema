package zio.schema.codec.xml

/**
 * Options for `XmlReader.read`.
 *
 * @param preserveWhitespace
 *   Keeps child text nodes that hold only whitespace when `true`; drops them
 *   when `false` (the default).
 */
final case class ReaderConfig(
  maxDepth: Int = 1000,
  maxAttributes: Int = 1000,
  maxTextLength: Int = 10000000,
  preserveWhitespace: Boolean = false
)

/** Predefined reader options. */
object ReaderConfig {

  /** The default reader options. */
  val default: ReaderConfig = ReaderConfig()
}
