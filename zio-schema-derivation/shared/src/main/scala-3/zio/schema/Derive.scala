package zio.schema

import scala.quoted._
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValueTuple}
import Schema._

object Derive {
  inline def derive[F[_], A](deriver: Deriver[F])(implicit schema: Schema[A]): F[A] = ${ deriveInstance[F, A]('deriver, 'schema) }

  private def deriveInstance[F[_]: Type, A: Type](deriver: Expr[Deriver[F]], schema: Expr[Schema[A]]): Expr[F[A]] =
    ???
}