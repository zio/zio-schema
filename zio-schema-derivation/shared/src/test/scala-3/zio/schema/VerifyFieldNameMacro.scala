package zio.schema

import scala.quoted._

object VerifyFieldNameMacro {
  inline def verifyFieldName[F, S <: String & scala.Singleton] =
    ${ verifyFieldNameImpl[F, S] }

  def verifyFieldNameImpl[F: Type, S: Type](using ctx: Quotes): Expr[Boolean] = {
    import ctx.reflect._
    val f = TypeRepr.of[F].dealias
    val s = TypeRepr.of[S]
    if (f =:= s) '{ true } else '{ false }
  }
}
