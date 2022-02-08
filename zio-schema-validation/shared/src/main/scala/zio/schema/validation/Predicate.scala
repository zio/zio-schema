package zio.schema.validation

import zio.schema.validation.utils._

sealed trait Predicate[A] {
  type Errors = List[String]
  type Result = Either[Errors, Errors]
  def validate(value: A): Result
}
object Predicate {
  // String => Boolean
  sealed trait Str[A] extends Predicate[A]
  object Str {
    final case class MinLength(n: Int) extends Str[String] {
      def validate(value: String): Result =
        if(value.length() >= n) Right(::(StringValidationErrors.RightTooLong().message, Nil)) // right is too long
        else Left(::(StringValidationErrors.LeftTooShort().message, Nil)) // TODO create error messages here for "" left is too short
        // TODO finish validate implementation for predicates and try to add tests
    }
    final case class MaxLength(n: Int) extends Str[String] {
      def validate(value: String): Result =
        if(value.length() <= n) Right(::(StringValidationErrors.RightTooShort().message, Nil))
        else Left(::(StringValidationErrors.LeftTooLong().message, Nil))
    }
    final case class Matches(r: Regex) extends Str[String] {
      // TODO Q: do we need a Regex implementation?
      def validate(value: String): Result = ???
    }
  }

  // A => Boolean
  sealed trait Num[A] extends Predicate[A] {
    def numType: NumType[A]
  }
  object Num {
    final case class GreaterThan[A](numType: NumType[A], value: A) extends Num[A] {
      def validate(v: A): Result =
        if(numType.numeric.compare(v, value) > 0)
          Right(::(s"$v should be less than or equal to $value", Nil))
        else
          Left(::(s"$v should be greater than $value", Nil))
    }
    final case class LessThan[A](numType: NumType[A], value: A) extends Num[A] {
      def validate(v: A): Result =
        if(numType.numeric.compare(v, value) < 0)
          Right(::(s"$v should be greater than or equal to $value", Nil))
        else
          Left(::(s"$v should be less than $value", Nil))
    }
    final case class EqualTo[A](numType: NumType[A], value: A) extends Num[A] {
      def validate(v: A): Result =
        if(numType.numeric.compare(v, value) == 0)
          Right(::(s"$v should be equal to $value", Nil))
        else
          Left(::(s"$v should equal to $value", Nil))
    }
  }

  final case class True[A]() extends Predicate[A] { // A => True
    def validate(value: A): Result = Right(Nil)
  }
}