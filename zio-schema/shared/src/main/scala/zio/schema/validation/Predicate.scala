package zio.schema.validation

sealed trait Predicate[A] {
  type Errors = List[ValidationError]
  type Result = Either[Errors, Errors]
  def validate(value: A): Result
}

object Predicate {
  // String => Boolean
  sealed trait Str[A] extends Predicate[A]

  object Str {
    final case class MinLength(n: Int) extends Str[String] {

      def validate(value: String): Result =
        if (value.length() >= n)
          Right(::(ValidationError.MinLength(n, value.length(), value), Nil)) // right is too long
        else
          Left(::(ValidationError.MinLength(n, value.length(), value), Nil)) // TODO create inverted Errors as well
          // NOTE: inversion of MinLength error is MaxLength error for example
    }
    final case class MaxLength(n: Int) extends Str[String] {

      def validate(value: String): Result =
        if (value.length() <= n) Right(::(ValidationError.MaxLength(n, value.length(), value), Nil))
        else Left(::(ValidationError.MaxLength(n, value.length(), value), Nil))
    }
    final case class Matches(r: Regex) extends Str[String] {

      def validate(value: String): Result =
        if (r.test(value)) Right(::(ValidationError.RegexMatch(value, r), Nil))
        else Left(::(ValidationError.RegexMatch(value, r), Nil))
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
          Right(::(ValidationError.GreaterThan(v, value), Nil))
        else
          Left(::(ValidationError.GreaterThan(v, value), Nil))
    }
    final case class LessThan[A](numType: NumType[A], value: A) extends Num[A] {

      def validate(v: A): Result =
        if (numType.numeric.compare(v, value) < 0)
          Right(::(ValidationError.LessThan(v, value), Nil))
        else
          Left(::(ValidationError.LessThan(v, value), Nil))
    }
    final case class EqualTo[A](numType: NumType[A], value: A) extends Num[A] {

      def validate(v: A): Result =
        if (numType.numeric.compare(v, value) == 0)
          Right(::(ValidationError.EqualTo(v, value), Nil))
        else
          Left(::(ValidationError.EqualTo(v, value), Nil))
    }
  }

  final case class True[A]() extends Predicate[A] { // A => True
    def validate(value: A): Result = Right(Nil)
  }
}
