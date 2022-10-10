package zio.schema.internal

import scala.reflect.macros.blackbox

object Macros {
  private[internal] def location(c: blackbox.Context): (String, Int, Int, String) = {
    val path = c.enclosingPosition.source.path
    val line = c.enclosingPosition.line
    val col  = c.enclosingPosition.column
    val method = Some(c.internal.enclosingOwner).filter(_.isMethod).map(_.fullName).getOrElse("")
    (path, line, col, method)
  }

  def sourceLocation_impl(c: blackbox.Context): c.Expr[SourceLocation] = {
    import c.universe._
    val (path, line, col, method) = location(c)
    c.Expr[SourceLocation](q"""${c.prefix}($path, $line, $col, $method)""")
  }
}
