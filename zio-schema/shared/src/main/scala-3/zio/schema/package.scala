package zio

/**
 * Type aliases that differ between Scala versions. On Scala 3, it also holds extension methods that merge the two sides of a [[Fallback]].
 */
package object schema {
  /**
   * The bound used for singleton field-name types such as `Schema.Field#Field`. It is `scala.Singleton` on Scala 2.13 and 3, and `Any` on Scala 2.12, which has no `Singleton` type.
   */
  type Singleton = scala.Singleton

  extension [A <: C, B <: C, C](s: Schema.Fallback[A, B])
    /**
     * Returns a `Schema[C]` for the common supertype `C` of the two sides of this fallback schema. Encoding a `C` that is an `A` uses the left schema, and one that is a `B` uses the right schema; decoding a `Fallback.Both` gives its left value.
     */
    transparent inline def merge: Schema[C] =
      s.transform(
        _.merge,
        {
          case left: A  => Fallback.Left(left)
          case right: B => Fallback.Right(right)
        }
      )

  extension [A <: C, C](s: Schema[A])
    /**
     * Returns a schema of the common supertype `C` that tries this schema first and `s0` second, built as `Schema.Fallback(s, s0).merge`. For example, `val s: Schema[Int | String] = Schema[Int].orElse(Schema[String])` compiles.
     */
    transparent inline def orElse[B <: C](s0: Schema[B]): Schema[C] =
      Schema.Fallback(s, s0).merge

  extension [A <: C, B <: C, C](fallback: Fallback[A, B])
    /**
     * Returns the value of this fallback as the common supertype `C`: the left value for `Fallback.Left` and `Fallback.Both`, and the right value for `Fallback.Right`.
     */
    transparent inline def merge: C =
      fallback match {
        case Fallback.Left(left)    => left
        case Fallback.Right(right)  => right
        case Fallback.Both(left, _) => left
      }

}
