package zio.schema.validation.utils

//TODO move top level to the .validation package and put in a file called
// ValidationError.scala

sealed trait ValidationErrors {
  def message: String
}

object ValidationErrors {
  //TODO rename to ValidationError
  // store numbers and context.
  final case class MinLength(minLength: Int, actualLength: Int, string: String) extends ValidationErrors {
    override def message =
      s"Expected the length of $string to be at least $minLength characters but was $actualLength characters."
  }
  final case class MaxLength(maxLength: Int, actualLength: Int, string: String) extends ValidationErrors {
    override def message =
      s"Expected the length of $string to be at most $maxLength characters but was $actualLength characters."
  }

  final case class GreaterThan[A](value: A, expected: A) extends ValidationErrors {
    override def message =
      s"$value should be greater than $expected"
  }
  final case class LessThan[A](value: A, expected: A) extends ValidationErrors {
    override def message =
      s"$value should be less than $expected"
  }
  final case class EqualTo[A](value: A, expected: A) extends ValidationErrors {
    override def message =
      s"$value should be equal to $expected"
  }
}
