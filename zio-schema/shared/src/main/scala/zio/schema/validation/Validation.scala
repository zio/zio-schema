package zio.schema.validation

import zio.Chunk

/**
 * A `Validation[A]` describes the checks that a value of type `A` must pass. It
 * wraps a `Bool` tree of `Predicate`s, so checks combine with `&&`, `||` and
 * `!`, and `validate` returns `Left` with the errors of the checks that failed.
 *
 * Build one with the constructors on the companion object: string lengths,
 * regular expressions, numeric bounds, `time`, and predefined checks such as
 * `email`, `ipV4` and `uuidV4`. Use it to check input after decoding; on a case
 * class, annotate a field with `zio.schema.annotation.validate` and
 * `Schema.validate` checks that field's validation.
 *
 * {{{
 * import zio.schema.validation.Validation
 *
 * val username: Validation[String] =
 *   Validation.minLength(3) && Validation.maxLength(12) && Validation.identifier
 *
 * val accepted = username.validate("zio_user") // Right(())
 * val rejected = username.validate("ab")       // Left(Chunk(MinLength(3,2,ab)))
 * }}}
 *
 * @param bool
 *   the boolean expression of predicates to check
 */
final case class Validation[A](bool: Bool[Predicate[A]]) { self =>

  /**
   * Returns a `Validation` that passes when both this one and `that` pass. When
   * both fail, the errors of both are returned.
   */
  def &&(that: Validation[A]): Validation[A] = Validation(self.bool && that.bool)

  /**
   * Returns a `Validation` that passes when this one or `that` passes. When
   * both fail, the errors of both are returned.
   */
  def ||(that: Validation[A]): Validation[A] = Validation(self.bool || that.bool)

  /**
   * Returns a `Validation` that passes when this one fails. Its errors describe
   * the opposite check: `!Validation.equalTo(5)` reports
   * `ValidationError.NotEqualTo` for `5`.
   *
   * The negation fails with an empty error `Chunk` for `Validation.succeed`,
   * and for `contramap`, `left`, `right`, `either` and `optional` (on a `Some`)
   * when the inner validation passes.
   */
  def unary_! : Validation[A] = Validation(!self.bool)

  /**
   * Returns a `Validation` for `Option[A]` that applies this validation to the
   * value inside a `Some`. A `None` passes when `validNone` is `true` (the
   * default) and fails with `ValidationError.EqualToNone` otherwise.
   */
  def optional(validNone: Boolean = true): Validation[Option[A]] =
    Validation(Bool.Leaf(Predicate.Optional(bool, validNone)))

  /**
   * Returns a `Validation` for `B` that converts the value with `f` and then
   * applies this validation to the result.
   *
   * {{{
   * import zio.schema.validation.Validation
   *
   * val threeDigits: Validation[Int] = Validation.minLength(3).contramap[Int](_.toString)
   *
   * val ok    = threeDigits.validate(123) // Right(())
   * val short = threeDigits.validate(12)  // Left(Chunk(MinLength(3,2,12)))
   * }}}
   */
  def contramap[B](f: B => A): Validation[B] = Validation(bool.map(_.contramap(f)))

  /**
   * Returns a `Validation` for `Either[B, A]` that applies this validation to a
   * `Right` value and `onLeft` to a `Left` value. With the default `onLeft`
   * (`Validation.fail`), every `Left` fails with an empty error `Chunk`.
   */
  def right[B](onLeft: Validation[B] = Validation.fail[B]): Validation[Either[B, A]] = Validation.either(onLeft, self)

  /**
   * Returns a `Validation` for `Either[A, B]` that applies this validation to a
   * `Left` value and `onRight` to a `Right` value. With the default `onRight`
   * (`Validation.fail`), every `Right` fails with an empty error `Chunk`.
   */
  def left[B](onRight: Validation[B] = Validation.fail[B]): Validation[Either[A, B]] = Validation.either(self, onRight)

  /**
   * Checks `value` and returns `Right(())` when it passes, or `Left` with the
   * errors of the checks that failed. `Validation.fail` and the default branch
   * of `left` and `right` fail with an empty error `Chunk`.
   */
  def validate(value: A): Either[Chunk[ValidationError], Unit] = {
    type Errors = Chunk[ValidationError]
    type Result = Either[Errors, Errors]
    def combineAnd(left: Result, right: Result): Result =
      (left, right) match {
        case (Left(leftErrors), Left(rightErrors))         => Left(leftErrors ++ rightErrors)
        case (Left(leftErrors), _)                         => Left(leftErrors)
        case (_, Left(rightErrors))                        => Left(rightErrors)
        case (Right(leftSuccesses), Right(rightSuccesses)) => Right(leftSuccesses ++ rightSuccesses)
      }

    def combineOr(left: Result, right: Result): Result =
      (left, right) match {
        case (Left(leftErrors), Left(rightErrors))         => Left(leftErrors ++ rightErrors)
        case (Left(_), right)                              => right
        case (right, Left(_))                              => right
        case (Right(leftSuccesses), Right(rightSuccesses)) => Right(leftSuccesses ++ rightSuccesses)
      }

    def loop(bool: Bool[Predicate[A]]): Result = {
      import Bool._
      bool match {
        case And(left, right) =>
          val leftValidation  = loop(left)
          val rightValidation = loop(right)
          combineAnd(leftValidation, rightValidation)
        case Or(left, right) =>
          val leftValidation  = loop(left)
          val rightValidation = loop(right)
          combineOr(leftValidation, rightValidation)
        case Leaf(predicate) => predicate.validate(value)
        case Not(value)      => loop(value).swap
      }
    }

    loop(self.bool).map(_ => ())
  }
}

/**
 * Constructors for `Validation`: string lengths, regular expressions, numeric
 * bounds and combinators, plus the predefined string checks inherited from
 * `Regexs` (`identifier`, `email`, `duration`, `ipV4`, `ipV6`, `uuidV4`) and
 * `Time` (`time`).
 */
object Validation extends Regexs with Time {
  import Predicate._

  // String operations
  /**
   * Returns a `Validation` that passes when the string has at least `n`
   * characters, and fails with `ValidationError.MinLength` otherwise.
   */
  def minLength(n: Int): Validation[String] = Validation(Bool.Leaf(Str.MinLength(n)))

  /**
   * Returns a `Validation` that passes when the string has at most `n`
   * characters, and fails with `ValidationError.MaxLength` otherwise.
   */
  def maxLength(n: Int): Validation[String] = Validation(Bool.Leaf(Str.MaxLength(n)))
  // Regex
  /**
   * Returns a `Validation` that passes when the whole string matches `r`, and
   * fails with `ValidationError.RegexMatch` otherwise.
   *
   * {{{
   * import zio.schema.validation.{Regex, Validation}
   *
   * val digits = Validation.regex(Regex.digit.atLeast(1))
   *
   * val ok    = digits.validate("2024") // Right(())
   * val mixed = digits.validate("12a")  // Left(Chunk(RegexMatch(12a,Repeat(Digit,Some(1),None))))
   * }}}
   */
  def regex(r: Regex): Validation[String] = Validation(Bool.Leaf(Str.Matches(r)))

  // Numerical operations
  /**
   * Returns a `Validation` that passes when the number is strictly greater than
   * `value`, and fails with `ValidationError.GreaterThan` otherwise.
   */
  def greaterThan[A](value: A)(implicit numType: NumType[A]): Validation[A] =
    Validation(Bool.Leaf(Num.GreaterThan(numType, value)))

  /**
   * Returns a `Validation` that passes when the number is strictly less than
   * `value`, and fails with `ValidationError.LessThan` otherwise.
   */
  def lessThan[A](value: A)(implicit numType: NumType[A]): Validation[A] =
    Validation(Bool.Leaf(Num.LessThan(numType, value)))

  /**
   * Returns a `Validation` that passes when the number is between `lower` and
   * `upper`, both included. A value below `lower` fails with
   * `ValidationError.GreaterThan` and `ValidationError.EqualTo`; a value above
   * `upper` fails with `ValidationError.LessThan` and
   * `ValidationError.EqualTo`.
   */
  def between[A](lower: A, upper: A)(implicit numType: NumType[A]): Validation[A] =
    (greaterThan(lower) || equalTo(lower)) && (lessThan(upper) || equalTo(upper))

  /**
   * Returns a `Validation` that passes when the number equals `value`, compared
   * with its `Numeric` ordering, and fails with `ValidationError.EqualTo`
   * otherwise.
   */
  def equalTo[A](value: A)(implicit numType: NumType[A]): Validation[A] =
    Validation(Bool.Leaf(Num.EqualTo(numType, value)))

  /** Returns a `Validation` that passes for every value. */
  def succeed[A]: Validation[A] = Validation(Bool.Leaf(Predicate.True[A]()))

  /**
   * Returns a `Validation` that fails for every value, with an empty error
   * `Chunk`.
   */
  def fail[A]: Validation[A] = !succeed[A]

  /**
   * Returns a `Validation` that passes when every one of `vs` passes; with no
   * arguments it passes.
   */
  def allOf[A](vs: Validation[A]*): Validation[A] = vs.foldLeft(succeed[A])(_ && _)

  /**
   * Returns a `Validation` that passes when every validation in `vl` passes;
   * for an empty `vl` it passes.
   */
  def allOf[A](vl: Iterable[Validation[A]]): Validation[A] = allOf(vl.toSeq: _*)

  /**
   * Returns a `Validation` that passes when at least one of `vs` passes; with
   * no arguments it fails with an empty error `Chunk`.
   */
  def anyOf[A](vs: Validation[A]*): Validation[A] = vs.foldLeft(fail[A])(_ || _)

  /**
   * Returns a `Validation` that passes when at least one validation in `vl`
   * passes; for an empty `vl` it fails with an empty error `Chunk`.
   */
  def anyOf[A](vl: Iterable[Validation[A]]): Validation[A] = anyOf(vl.toSeq: _*)

  /**
   * Returns a `Validation` for `Either[L, R]` that applies `left` to a `Left`
   * value and `right` to a `Right` value.
   */
  def either[L, R](left: Validation[L], right: Validation[R]): Validation[scala.util.Either[L, R]] =
    Validation(Bool.Leaf(Predicate.Either(left.bool, right.bool)))
}
