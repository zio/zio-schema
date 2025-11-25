package zio.schema.validation

import zio.Chunk

final case class Validation[A](bool: Bool[Predicate[A]]) { self =>
  def &&(that: Validation[A]): Validation[A] = Validation(self.bool && that.bool)
  def ||(that: Validation[A]): Validation[A] = Validation(self.bool || that.bool)
  def unary_! : Validation[A]                = Validation(!self.bool)

  /*
    Returns a `Validation` for `Option[A]` applying current `Validation` if found value is `Some(_)` and accepts `None` depending on `validNone`.
   */
  def optional(validNone: Boolean = true): Validation[Option[A]] =
    Validation(Bool.Leaf(Predicate.Optional(bool, validNone)))

  /*
    Returns a new `Validation` transforming a `B` value using `f` and then validating.
   */
  def contramap[B](f: B => A): Validation[B] = Validation(bool.map(_.contramap(f)))

  /*
    Returns a new `Validation` for `Either[B, A]`. With default `onLeft` fails on `Left` and applies current validation on `Right`.
   */
  def right[B](onLeft: Validation[B] = Validation.fail[B]): Validation[Either[B, A]] = Validation.either(onLeft, self)

  /*
    Returns a new `Validation` for `Either[A, B]`. With default `onRight` fails on `Right` and applies current validation on `Left`.
   */
  def left[B](onRight: Validation[B] = Validation.fail[B]): Validation[Either[A, B]] = Validation.either(self, onRight)

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

object Validation extends Regexs with Time {
  import Predicate._

  // String operations
  def minLength(n: Int): Validation[String] = Validation(Bool.Leaf(Str.MinLength(n)))
  def maxLength(n: Int): Validation[String] = Validation(Bool.Leaf(Str.MaxLength(n)))
  // Regex
  def regex(r: Regex): Validation[String] = Validation(Bool.Leaf(Str.Matches(r)))

  // Numerical operations
  def greaterThan[A](value: A)(implicit numType: NumType[A]): Validation[A] =
    Validation(Bool.Leaf(Num.GreaterThan(numType, value)))

  def lessThan[A](value: A)(implicit numType: NumType[A]): Validation[A] =
    Validation(Bool.Leaf(Num.LessThan(numType, value)))

  def between[A](lower: A, upper: A)(implicit numType: NumType[A]): Validation[A] =
    (greaterThan(lower) || equalTo(lower)) && (lessThan(upper) || equalTo(upper))

  def equalTo[A](value: A)(implicit numType: NumType[A]): Validation[A] =
    Validation(Bool.Leaf(Num.EqualTo(numType, value)))

  def succeed[A]: Validation[A] = Validation(Bool.Leaf(Predicate.True[A]()))
  def fail[A]: Validation[A]    = !succeed[A]

  def allOf[A](vs: Validation[A]*): Validation[A]          = vs.foldLeft(succeed[A])(_ && _)
  def allOf[A](vl: Iterable[Validation[A]]): Validation[A] = allOf(vl.toSeq: _*)

  def anyOf[A](vs: Validation[A]*): Validation[A]          = vs.foldLeft(fail[A])(_ || _)
  def anyOf[A](vl: Iterable[Validation[A]]): Validation[A] = anyOf(vl.toSeq: _*)

  def either[L, R](left: Validation[L], right: Validation[R]): Validation[scala.util.Either[L, R]] =
    Validation(Bool.Leaf(Predicate.Either(left.bool, right.bool)))
}
