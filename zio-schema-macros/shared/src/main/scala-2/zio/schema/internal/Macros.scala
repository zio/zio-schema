package zio.schema.internal

import scala.reflect.macros.blackbox

/** Scala 2 macro implementations for `zio.schema.internal`. */
object Macros {
  private[internal] def location(c: blackbox.Context): (String, Int, Int) = {
    val path = c.enclosingPosition.source.path
    val line = c.enclosingPosition.line
    val col  = c.enclosingPosition.column
    (path, line, col)
  }

  /** The Scala 2 macro implementation of `SourceLocation.generate`. */
  def sourceLocation_impl(c: blackbox.Context): c.Expr[SourceLocation] = {
    import c.universe._
    val (path, line, col) = location(c)
    c.Expr[SourceLocation](q"""${c.prefix}($path, $line, $col)""")
  }
}
