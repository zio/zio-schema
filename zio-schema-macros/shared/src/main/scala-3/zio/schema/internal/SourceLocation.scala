package zio.schema.internal

import scala.quoted._

final case class SourceLocation(path: String, line: Int, col: Int)

object SourceLocation {
  implicit inline def generate: SourceLocation = ${ generateSourceLocation }

  def generateSourceLocation(using ctx: Quotes): Expr[SourceLocation] = {
    import ctx.reflect._

    val position = Position.ofMacroExpansion
    val path = Expr(position.sourceFile.path)
    val line = Expr(position.startLine)
    val col = Expr(position.startColumn)
    '{ SourceLocation($path, $line, $col) }
  }
}
