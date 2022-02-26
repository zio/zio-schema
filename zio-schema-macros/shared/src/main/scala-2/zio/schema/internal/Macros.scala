package zio.schema.internal

import scala.reflect.macros.blackbox

object Macros {
  private[internal] def location(c: blackbox.Context): (String, Int, Int) = {
    val path = c.enclosingPosition.source.path
    val line = c.enclosingPosition.line
    val col  = c.enclosingPosition.column
    (path, line, col)
  }

  def sourceLocation_impl(c: blackbox.Context): c.Expr[SourceLocation] = {
    import c.universe._
    val (path, line, col) = location(c)
    c.Expr[SourceLocation](q"""${c.prefix}($path, $line, $col)""")
  }
}
