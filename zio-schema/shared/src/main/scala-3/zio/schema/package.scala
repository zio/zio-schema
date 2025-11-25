package zio

package object schema {
  type Singleton = scala.Singleton

  extension [A <: C, B <: C, C](s: Schema.Fallback[A, B]) transparent inline def merge: Schema[C] =
    s.transform(
      _.merge,
      {
        case left: A  => Fallback.Left(left)
        case right: B => Fallback.Right(right)
      }
    )

  extension [A <: C, C](s: Schema[A]) transparent inline def orElse[B <: C](s0: Schema[B]): Schema[C] =
    Schema.Fallback(s, s0).merge

  extension [A <: C, B <: C, C](fallback: Fallback[A, B]) transparent inline def merge: C =
    fallback match {
      case Fallback.Left(left)    => left
      case Fallback.Right(right)  => right
      case Fallback.Both(left, _) => left
    }

}
