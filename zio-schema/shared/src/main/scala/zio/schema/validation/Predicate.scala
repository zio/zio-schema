package zio.schema.validation

import zio.Chunk

/**
 * A single check on a value of type `A`, the leaf of a `Validation`'s `Bool`
 * tree. Most code builds predicates through the `Validation` constructors.
 */
sealed trait Predicate[A] { self =>

  /** The errors a predicate reports. */
  type Errors = Chunk[ValidationError]

  /**
   * The outcome of a check: `Left` with the errors when it fails, or `Right`
   * with the errors to report if the check is negated with `!`.
   */
  type Result = Either[Errors, Errors]

  /** Checks `value` and returns a `Result`. */
  def validate(value: A): Result

  /**
   * Returns a predicate for `B` that converts the value with `f` and then
   * checks the result with this predicate.
   */
  def contramap[B](f: B => A): Predicate[B] = Predicate.Contramap(Bool.Leaf(self), f)
}

/** The built-in predicates. */
object Predicate {

  /** A predicate on strings. */
  sealed trait Str[A] extends Predicate[A]

  /** The string predicates. */
  object Str {

    /** Passes when the string has at least `n` characters. */
    final case class MinLength(n: Int) extends Str[String] {

      /**
       * Returns `Left` with `ValidationError.MinLength` when `value` is shorter
       * than `n`.
       */
      def validate(value: String): Result =
        if (value.length() >= n)
          Right(Chunk(ValidationError.MaxLength(n, value.length(), value)))
        else
          Left(Chunk(ValidationError.MinLength(n, value.length(), value)))
    }

    /** Passes when the string has at most `n` characters. */
    final case class MaxLength(n: Int) extends Str[String] {

      /**
       * Returns `Left` with `ValidationError.MaxLength` when `value` is longer
       * than `n`.
       */
      def validate(value: String): Result =
        if (value.length() <= n) Right(Chunk(ValidationError.MinLength(n, value.length(), value)))
        else Left(Chunk(ValidationError.MaxLength(n, value.length(), value)))
    }

    /** Passes when the whole string matches `r`. */
    final case class Matches(r: Regex) extends Str[String] {

      /**
       * Returns `Left` with `ValidationError.RegexMatch` when `value` does not
       * match `r`.
       */
      def validate(value: String): Result =
        if (r.test(value)) Right(Chunk(ValidationError.NotRegexMatch(value, r)))
        else Left(Chunk(ValidationError.RegexMatch(value, r)))
    }
  }

  // A => Boolean
  /** A predicate on numbers, compared with the `Numeric` of `numType`. */
  sealed trait Num[A] extends Predicate[A] {

    /** The numeric type whose ordering the predicate uses. */
    def numType: NumType[A]
  }

  /** The numeric predicates. */
  object Num {

    /** Passes when the number is strictly greater than `value`. */
    final case class GreaterThan[A](numType: NumType[A], value: A) extends Num[A] {

      /**
       * Returns `Left` with `ValidationError.GreaterThan` when `v` is not
       * greater than `value`.
       */
      def validate(v: A): Result =
        if (numType.numeric.compare(v, value) > 0)
          Right(Chunk(ValidationError.LessThan(v, value)))
        else
          Left(Chunk(ValidationError.GreaterThan(v, value)))
    }

    /** Passes when the number is strictly less than `value`. */
    final case class LessThan[A](numType: NumType[A], value: A) extends Num[A] {

      /**
       * Returns `Left` with `ValidationError.LessThan` when `v` is not less
       * than `value`.
       */
      def validate(v: A): Result =
        if (numType.numeric.compare(v, value) < 0)
          Right(Chunk(ValidationError.GreaterThan(v, value)))
        else
          Left(Chunk(ValidationError.LessThan(v, value)))
    }

    /** Passes when the number equals `value`. */
    final case class EqualTo[A](numType: NumType[A], value: A) extends Num[A] {

      /**
       * Returns `Left` with `ValidationError.EqualTo` when `v` is not equal to
       * `value`.
       */
      def validate(v: A): Result =
        if (numType.numeric.compare(v, value) == 0)
          Right(Chunk(ValidationError.NotEqualTo(v, value)))
        else
          Left(Chunk(ValidationError.EqualTo(v, value)))
    }
  }

  /** Passes for every value. */
  final case class True[A]() extends Predicate[A] { // A => True
    /** Returns `Right` with no errors. */
    def validate(value: A): Result = Right(Chunk.empty)
  }

  /**
   * Checks the value inside a `Some` with `pred`; a `None` passes when
   * `validNone` is `true`.
   */
  final case class Optional[A](pred: Bool[Predicate[A]], validNone: Boolean) extends Predicate[Option[A]] {

    /**
     * Returns `Left` with `ValidationError.EqualToNone` for a `None` when
     * `validNone` is `false`, or the errors of `pred` for a `Some`.
     */
    def validate(value: Option[A]): Result = value match {
      case None =>
        if (validNone) Right(Chunk(ValidationError.EqualToNone())) else Left(Chunk(ValidationError.EqualToNone()))
      case Some(v) => Validation(pred).validate(v).map(_ => Chunk.empty)
    }
  }

  /** Converts the value with `f` and checks the result with `pred`. */
  final case class Contramap[B, A](pred: Bool[Predicate[A]], f: (B => A)) extends Predicate[B] {

    /** Returns the result of checking `f(value)` with `pred`. */
    def validate(value: B): Result = Validation(pred).validate(f(value)).map(_ => Chunk.empty)
  }

  /** Checks a `Left` value with `left` and a `Right` value with `right`. */
  final case class Either[L, R](left: Bool[Predicate[L]], right: Bool[Predicate[R]])
      extends Predicate[scala.util.Either[L, R]] {

    /**
     * Returns the result of checking the `Left` or `Right` value with the
     * matching predicate.
     */
    def validate(value: scala.util.Either[L, R]): Result = value match {
      case scala.util.Left(l)  => Validation(left).validate(l).map(_ => Chunk.empty)
      case scala.util.Right(r) => Validation(right).validate(r).map(_ => Chunk.empty)
    }
  }

}
