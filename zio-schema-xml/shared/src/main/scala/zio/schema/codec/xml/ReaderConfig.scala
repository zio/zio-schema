package zio.schema.codec.xml

final case class ReaderConfig(
  maxDepth: Int = 1000,
  maxAttributes: Int = 1000,
  maxTextLength: Int = 10000000,
  preserveWhitespace: Boolean = false
)

object ReaderConfig {
  val default: ReaderConfig = ReaderConfig()
}
