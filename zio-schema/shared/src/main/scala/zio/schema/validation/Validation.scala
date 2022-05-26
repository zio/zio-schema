package zio.schema.validation

final case class Validation[A](bool: Bool[Predicate[A]]) { self =>
  def &&(that: Validation[A]): Validation[A] = Validation(self.bool && that.bool)
  def ||(that: Validation[A]): Validation[A] = Validation(self.bool || that.bool)
  def unary_! : Validation[A]                = Validation(!self.bool)

  def validate(value: A): Either[List[ValidationError], Unit] = {
    type Errors = List[ValidationError]
    type Result = Either[Errors, Errors]
    def combineAnd(left: Result, right: Result): Result =
      (left, right) match {
        case (Left(leftErrors), Left(rightErrors))         => Left((leftErrors ++ rightErrors))
        case (Left(leftErrors), _)                         => Left(leftErrors)
        case (_, Left(rightErrors))                        => Left(rightErrors)
        case (Right(leftSuccesses), Right(rightSuccesses)) => Right((leftSuccesses ++ rightSuccesses))
      }

    def combineOr(left: Result, right: Result): Result =
      (left, right) match {
        case (Left(leftErrors), Left(rightErrors))         => Left((leftErrors ++ rightErrors))
        case (Left(_), right)                              => right
        case (right, Left(_))                              => right
        case (Right(leftSuccesses), Right(rightSuccesses)) => Right((leftSuccesses ++ rightSuccesses))
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

object Validation extends Regexs {
  import Predicate._

  // String operations
  def minLength(n: Int): Validation[String]        = Validation(Bool.Leaf(Str.MinLength(n)))
  def maxLength(n: Int): Validation[String]        = Validation(Bool.Leaf(Str.MaxLength(n)))
  def dateTime(format: String): Validation[String] = Validation(Bool.Leaf(Str.DateFormat(format)))
  //Regex
  def regex(r: Regex): Validation[String] = Validation(Bool.Leaf(Str.Matches(r)))

  // Numerical operations
  def greaterThan[A](value: A)(implicit numType: NumType[A]): Validation[A] =
    Validation(Bool.Leaf(Num.GreaterThan(numType, value)))

  def lessThan[A](value: A)(implicit numType: NumType[A]): Validation[A] =
    Validation(Bool.Leaf(Num.LessThan(numType, value)))

  def equalTo[A](value: A)(implicit numType: NumType[A]): Validation[A] =
    Validation(Bool.Leaf(Num.EqualTo(numType, value)))

  def succeed[A]: Validation[A] = Validation(Bool.Leaf(Predicate.True[A]()))
  def fail[A]: Validation[A]    = !succeed[A]

  def allOf[A](vs: Validation[A]*): Validation[A]          = vs.foldLeft(succeed[A])(_ && _)
  def allOf[A](vl: Iterable[Validation[A]]): Validation[A] = allOf(vl.toSeq: _*)

  def anyOf[A](vs: Validation[A]*): Validation[A]          = vs.foldLeft(fail[A])(_ || _)
  def anyOf[A](vl: Iterable[Validation[A]]): Validation[A] = anyOf(vl.toSeq: _*)
}
