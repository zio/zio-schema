package zio

import scala.reflect.ClassTag

package object schema {
  type Singleton = scala.Singleton

  extension [A: ClassTag, B: ClassTag] (s: Schema.Fallback[A, B])
    def merge: Schema[A | B] =
      s.transform(_.merge, {
        case left: A  => Fallback.Left(left.asInstanceOf[A])
        case right: B => Fallback.Right(right.asInstanceOf[B])
      })

  extension [A: ClassTag] (s: Schema[A])
    def orElse[B: ClassTag](s0: Schema[B]): Schema[A | B] =
      Schema.Fallback(s, s0).merge

  extension [A: ClassTag, B: ClassTag] (fallback: Fallback[A, B])
    def merge: A | B =
      fallback match {
        case Fallback.Left(left) => left
        case Fallback.Right(right) => right
        case Fallback.Both(left, _) => left
      }

}
