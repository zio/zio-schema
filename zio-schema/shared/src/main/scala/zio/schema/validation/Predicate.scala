package zio.schema.validation

import zio.Chunk

sealed trait Predicate[A] { self =>
  type Errors = Chunk[ValidationError]
  type Result = Either[Errors, Errors]
  def validate(value: A): Result
  def contramap[B](f: B => A): Predicate[B] = Predicate.Contramap(Bool.Leaf(self), f)
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

  final case class Optional[A](pred: Bool[Predicate[A]], validNone: Boolean) extends Predicate[Option[A]] {

    def validate(value: Option[A]): Result = value match {
      case None =>
        if (validNone) Right(Chunk(ValidationError.EqualToNone())) else Left(Chunk(ValidationError.EqualToNone()))
      case Some(v) => Validation(pred).validate(v).map(_ => Chunk.empty)
    }
  }

  final case class Contramap[B, A](pred: Bool[Predicate[A]], f: (B => A)) extends Predicate[B] {
    def validate(value: B): Result = Validation(pred).validate(f(value)).map(_ => Chunk.empty)
  }

  final case class Either[L, R](left: Bool[Predicate[L]], right: Bool[Predicate[R]])
      extends Predicate[scala.util.Either[L, R]] {

    def validate(value: scala.util.Either[L, R]): Result = value match {
      case scala.util.Left(l)  => Validation(left).validate(l).map(_ => Chunk.empty)
      case scala.util.Right(r) => Validation(right).validate(r).map(_ => Chunk.empty)
    }
  }

}
