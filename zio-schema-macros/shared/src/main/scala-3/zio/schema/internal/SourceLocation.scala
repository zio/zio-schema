package zio.schema.internal

import scala.quoted._

/** The source file path, line and column of a call site, which the implicit `SourceLocation.generate` fills in. On Scala 2, `line` is 1-based; on Scala 3, it is 0-based. */
final case class SourceLocation(path: String, line: Int, col: Int)

/** Provides the implicit `SourceLocation` of the call site. */
object SourceLocation {
  /** Returns the `SourceLocation` of the place where the implicit is needed. On Scala 2 it is a macro; on Scala 3 it is an inline method. */
  implicit inline def generate: SourceLocation = ${ generateSourceLocation }

  /** The Scala 3 macro implementation of `generate`. */
  def generateSourceLocation(using ctx: Quotes): Expr[SourceLocation] = {
    import ctx.reflect._

    val position = Position.ofMacroExpansion
    val path     = Expr(position.sourceFile.path)
    val line     = Expr(position.startLine)
    val col      = Expr(position.startColumn)
    '{ SourceLocation($path, $line, $col) }
  }
}
