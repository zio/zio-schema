package zio.schema.internal

final case class SourceLocation(path: String, line: Int, col: Int, method: String)

object SourceLocation {
  implicit def generate: SourceLocation = macro Macros.sourceLocation_impl
}
