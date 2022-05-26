package zio.schema.validation

sealed trait ValidationError {
  def message: String
}

object ValidationError {
  final case class MinLength(minLength: Int, actualLength: Int, string: String) extends ValidationError {
    override def message: String =
      s"Expected the length of $string to be at least $minLength characters but was $actualLength characters."
  }
  final case class MaxLength(maxLength: Int, actualLength: Int, string: String) extends ValidationError {
    override def message: String =
      s"Expected the length of $string to be at most $maxLength characters but was $actualLength characters."
  }

  final case class DateTimeFormatMatch(format: String, value: String) extends ValidationError {
    override def message: String =
      s"Format of $value should match $format"
  }

  final case class DateTimeFormatNotMatch(format: String, value: String) extends ValidationError {
    override def message: String =
      s"Format of $value should not match $format"
  }

  final case class GreaterThan[A](value: A, expected: A) extends ValidationError {
    override def message: String =
      s"$value should be greater than $expected"
  }
  final case class LessThan[A](value: A, expected: A) extends ValidationError {
    override def message: String =
      s"$value should be less than $expected"
  }
  final case class EqualTo[A](value: A, expected: A) extends ValidationError {
    override def message: String =
      s"$value should be equal to $expected"
  }

  final case class NotEqualTo[A](value: A, expected: A) extends ValidationError {
    override def message: String =
      s"$value should not be equal to $expected"
  }

  final case class RegexMatch(str: String, expected: Regex) extends ValidationError {
    override def message: String =
      s"$str does not match $expected"
  }

  final case class NotRegexMatch(str: String, expected: Regex) extends ValidationError {
    override def message: String =
      s"$str matches a regex other than $expected"
  }
}
