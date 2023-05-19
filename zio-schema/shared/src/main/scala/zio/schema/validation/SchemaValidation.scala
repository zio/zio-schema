package zio.schema.validation

import zio.Chunk
import zio.prelude.Validation

final case class SchemaValidation[A](bool: Bool[Predicate[A]]) { self =>
  def &&(that: SchemaValidation[A]): SchemaValidation[A] = SchemaValidation(self.bool && that.bool)
  def ||(that: SchemaValidation[A]): SchemaValidation[A] = SchemaValidation(self.bool || that.bool)
  def unary_! : SchemaValidation[A]                      = SchemaValidation(!self.bool)

  def validate(value: A): Validation[SchemaValidationError, Unit] = {
    type Errors = Chunk[SchemaValidationError]
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

    val errors = loop(self.bool).left.getOrElse(Chunk.empty)
    errors.nonEmptyOrElse[Validation[SchemaValidationError, Unit]](Validation.succeed(()))(
      errors => Validation.failNonEmptyChunk(errors)
    )
  }
}

object SchemaValidation extends Regexs with Time {
  import Predicate._

  // String operations
  def minLength(n: Int): SchemaValidation[String] = SchemaValidation(Bool.Leaf(Str.MinLength(n)))
  def maxLength(n: Int): SchemaValidation[String] = SchemaValidation(Bool.Leaf(Str.MaxLength(n)))
  //Regex
  def regex(r: Regex): SchemaValidation[String] = SchemaValidation(Bool.Leaf(Str.Matches(r)))

  // Numerical operations
  def greaterThan[A](value: A)(implicit numType: NumType[A]): SchemaValidation[A] =
    SchemaValidation(Bool.Leaf(Num.GreaterThan(numType, value)))

  def lessThan[A](value: A)(implicit numType: NumType[A]): SchemaValidation[A] =
    SchemaValidation(Bool.Leaf(Num.LessThan(numType, value)))

  def between[A](lower: A, upper: A)(implicit numType: NumType[A]): SchemaValidation[A] =
    (greaterThan(lower) || equalTo(lower)) && (lessThan(upper) || equalTo(upper))

  def equalTo[A](value: A)(implicit numType: NumType[A]): SchemaValidation[A] =
    SchemaValidation(Bool.Leaf(Num.EqualTo(numType, value)))

  def succeed[A]: SchemaValidation[A] = SchemaValidation(Bool.Leaf(Predicate.True[A]()))
  def fail[A]: SchemaValidation[A]    = !succeed[A]

  def allOf[A](vs: SchemaValidation[A]*): SchemaValidation[A]          = vs.foldLeft(succeed[A])(_ && _)
  def allOf[A](vl: Iterable[SchemaValidation[A]]): SchemaValidation[A] = allOf(vl.toSeq: _*)

  def anyOf[A](vs: SchemaValidation[A]*): SchemaValidation[A]          = vs.foldLeft(fail[A])(_ || _)
  def anyOf[A](vl: Iterable[SchemaValidation[A]]): SchemaValidation[A] = anyOf(vl.toSeq: _*)
}
