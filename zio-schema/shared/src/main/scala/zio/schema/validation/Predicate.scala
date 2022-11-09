package zio.schema.validation

import zio.Chunk

sealed trait Predicate[A] {
  type Errors = Chunk[ValidationError]
  type Result = Either[Errors, Errors]
  def validate(value: A): Result
}

object Predicate {
  sealed trait Str[A] extends Predicate[A]

  object Str {
    final case class MinLength(n: Int) extends Str[String] {

      def validate(value: String): Result =
        if (value.length() >= n)
          Right(Chunk(ValidationError.MaxLength(n, value.length(), value)))
        else
          Left(Chunk(ValidationError.MinLength(n, value.length(), value)))
    }
    final case class MaxLength(n: Int) extends Str[String] {

      def validate(value: String): Result =
        if (value.length() <= n) Right(Chunk(ValidationError.MinLength(n, value.length(), value)))
        else Left(Chunk(ValidationError.MaxLength(n, value.length(), value)))
    }
    final case class Matches(r: Regex) extends Str[String] {

      def validate(value: String): Result =
        if (r.test(value)) Right(Chunk(ValidationError.NotRegexMatch(value, r)))
        else Left(Chunk(ValidationError.RegexMatch(value, r)))
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
          Right(Chunk(ValidationError.LessThan(v, value)))
        else
          Left(Chunk(ValidationError.GreaterThan(v, value)))
    }
    final case class LessThan[A](numType: NumType[A], value: A) extends Num[A] {

      def validate(v: A): Result =
        if (numType.numeric.compare(v, value) < 0)
          Right(Chunk(ValidationError.GreaterThan(v, value)))
        else
          Left(Chunk(ValidationError.LessThan(v, value)))
    }
    final case class EqualTo[A](numType: NumType[A], value: A) extends Num[A] {

      def validate(v: A): Result =
        if (numType.numeric.compare(v, value) == 0)
          Right(Chunk(ValidationError.NotEqualTo(v, value)))
        else
          Left(Chunk(ValidationError.EqualTo(v, value)))
    }
  }

  final case class True[A]() extends Predicate[A] { // A => True
    def validate(value: A): Result = Right(Chunk.empty)
  }
}
