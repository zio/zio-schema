package zio.schema.validation

import zio.schema.validation.utils._

sealed trait Predicate[A] {
  type Errors = List[ValidationErrors]
  type Result = Either[Errors, Errors]
  def validate(value: A): Result
}

object Predicate {
  // String => Boolean
  sealed trait Str[A] extends Predicate[A]

  object Str {
    final case class MinLength(n: Int) extends Str[String] {

      def validate(value: String): Result =
        if (value.length() >= n) Right(::(ValidationErrors.MinLength(n, value.length(), value), Nil)) // right is too long
        else
          Left(::(ValidationErrors.MinLength(n, value.length(), value), Nil)) // TODO create error messages here for "" left is too short
      // TODO finish validate implementation for predicates and try to add tests
    }
    final case class MaxLength(n: Int) extends Str[String] {

      def validate(value: String): Result =
        if (value.length() <= n) Right(::(ValidationErrors.MaxLength(n, value.length(), value), Nil))
        else Left(::(ValidationErrors.MaxLength(n, value.length(), value), Nil))
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
        if (numType.numeric.compare(v, value) > 0)
          Right(::(ValidationErrors.GreaterThan(v, value), Nil))
        else
          Left(::(ValidationErrors.GreaterThan(v, value), Nil))
    }
    final case class LessThan[A](numType: NumType[A], value: A) extends Num[A] {

      def validate(v: A): Result =
        if (numType.numeric.compare(v, value) < 0)
          Right(::(ValidationErrors.LessThan(v, value), Nil))
        else
          Left(::(ValidationErrors.LessThan(v, value), Nil))
    }
    final case class EqualTo[A](numType: NumType[A], value: A) extends Num[A] {

      def validate(v: A): Result =
        if (numType.numeric.compare(v, value) == 0)
          Right(::(ValidationErrors.EqualTo(v, value), Nil))
        else
          Left(::(ValidationErrors.EqualTo(v, value), Nil))
    }
  }

  final case class True[A]() extends Predicate[A] { // A => True
    def validate(value: A): Result = Right(Nil)
  }
}
