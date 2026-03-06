package zio.schema.codec.xml

final case class WriterConfig(
  indentStep: Int = 0,
  includeDeclaration: Boolean = false,
  encoding: String = "UTF-8"
)

object WriterConfig {
  val default: WriterConfig         = WriterConfig()
  val pretty: WriterConfig          = WriterConfig(indentStep = 2)
  val withDeclaration: WriterConfig = WriterConfig(includeDeclaration = true)
}
