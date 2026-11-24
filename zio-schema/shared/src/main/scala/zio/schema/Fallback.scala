package zio.schema

/**
 * `Fallback` represents an enriched `Either` type that can contain both a left
 * and a right value. The left value represents the default value, that
 * fallbacks to the right value when it is not found.
 */
sealed trait Fallback[+A, +B] { self =>

  /**
   * Transform a `Fallback` into an `Either`, using the left value for
   * `Fallback.Both`.
   */
  def toEither: Either[A, B] = self match {
    case Fallback.Left(l)    => Left(l)
    case Fallback.Right(r)   => Right(r)
    case Fallback.Both(l, _) => Left(l)
  }

  /**
   * Deletes the right part of `Fallback.Both` instances.
   */
  def simplify: Fallback[A, B] = self match {
    case Fallback.Both(left, _) => Fallback.Left(left)
    case other                  => other
  }

  /**
   * Applies `fa` to the left value or `fb` to the right value. For
   * `Fallback.Both` it applies `fa` to the left value.
   */
  def fold[C](fa: A => C, fb: B => C): C = self match {
    case Fallback.Left(left)    => fa(left)
    case Fallback.Right(right)  => fb(right)
    case Fallback.Both(left, _) => fa(left)
  }

  /** Applies `f` to the left value; the same as [[mapLeft]]. */
  def map[C](f: A => C): Fallback[C, B] = mapLeft(f)

  /** Applies `f` to the left value, in `Fallback.Left` and in `Fallback.Both`. */
  def mapLeft[C](f: A => C): Fallback[C, B] = self match {
    case Fallback.Left(left)        => Fallback.Left(f(left))
    case Fallback.Right(right)      => Fallback.Right(right)
    case Fallback.Both(left, right) => Fallback.Both(f(left), right)
  }

  /** Applies `f` to the right value, in `Fallback.Right` and in `Fallback.Both`. */
  def mapRight[C](f: B => C): Fallback[A, C] = self match {
    case Fallback.Left(left)        => Fallback.Left(left)
    case Fallback.Right(right)      => Fallback.Right(f(right))
    case Fallback.Both(left, right) => Fallback.Both(left, f(right))
  }

  /** Exchanges the left and right values. */
  def swap: Fallback[B, A] = self match {
    case Fallback.Left(left)        => Fallback.Right(left)
    case Fallback.Right(right)      => Fallback.Left(right)
    case Fallback.Both(left, right) => Fallback.Both(right, left)
  }
}

/** The cases of [[Fallback]]. */
object Fallback {

  /** Turns `scala.util.Left` into `Fallback.Left` and `scala.util.Right` into `Fallback.Right`. */
  def fromEither[A, B](either: Either[A, B]): Fallback[A, B] = either match {
    case scala.util.Left(value)  => Left(value)
    case scala.util.Right(value) => Right(value)
  }

  /** A `Fallback` with only a left value. */
  final case class Left[+A, +B](left: A)           extends Fallback[A, B]
  /** A `Fallback` with only a right value. */
  final case class Right[+A, +B](right: B)         extends Fallback[A, B]
  /** A `Fallback` with both a left and a right value. */
  final case class Both[+A, +B](left: A, right: B) extends Fallback[A, B]

}
