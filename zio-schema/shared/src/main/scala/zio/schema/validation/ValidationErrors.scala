package zio.schema.validation

/**
 * An error returned by `Validation.validate` and `Schema.validate`, with a
 * human-readable `message`.
 */
sealed trait ValidationError {

  /** Returns a human-readable description of the error. */
  def message: String
}

/** The cases of `ValidationError`. */
object ValidationError {

  /**
   * Reports that `string` has `actualLength` characters where at least
   * `minLength` were expected. Returned by `Validation.minLength`, and by a
   * negated `Validation.maxLength(n)` with `n` as `minLength`.
   */
  final case class MinLength(minLength: Int, actualLength: Int, string: String) extends ValidationError {
    override def message: String =
      s"Expected the length of $string to be at least $minLength characters but was $actualLength characters."
  }

  /**
   * Reports that `string` has `actualLength` characters where at most
   * `maxLength` were expected. Returned by `Validation.maxLength`, and by a
   * negated `Validation.minLength(n)` with `n` as `maxLength`.
   */
  final case class MaxLength(maxLength: Int, actualLength: Int, string: String) extends ValidationError {
    override def message: String =
      s"Expected the length of $string to be at most $maxLength characters but was $actualLength characters."
  }

  /**
   * `value` is not greater than `expected`. Returned by
   * `Validation.greaterThan` and by a negated `Validation.lessThan`.
   */
  final case class GreaterThan[A](value: A, expected: A) extends ValidationError {
    override def message: String =
      s"$value should be greater than $expected"
  }

  /**
   * `value` is not less than `expected`. Returned by `Validation.lessThan` and
   * by a negated `Validation.greaterThan`.
   */
  final case class LessThan[A](value: A, expected: A) extends ValidationError {
    override def message: String =
      s"$value should be less than $expected"
  }

  /** `value` is not equal to `expected`. Returned by `Validation.equalTo`. */
  final case class EqualTo[A](value: A, expected: A) extends ValidationError {
    override def message: String =
      s"$value should be equal to $expected"
  }

  /**
   * The value is `None`. Returned by `Validation#optional` with
   * `validNone = false`, and by a negated `optional` with `validNone = true`.
   */
  final case class EqualToNone() extends ValidationError {
    override def message: String =
      s"Value should not be None"
  }

  /** `value` is a `Left`. No built-in validation returns this error. */
  final case class EqualToLeft[A](value: A) extends ValidationError {
    override def message: String =
      s"$value should not be Left"
  }

  /** `value` is a `Right`. No built-in validation returns this error. */
  final case class EqualToRight[A](value: A) extends ValidationError {
    override def message: String =
      s"$value should not be Right"
  }

  /**
   * `value` is equal to `expected`. Returned by a negated `Validation.equalTo`.
   */
  final case class NotEqualTo[A](value: A, expected: A) extends ValidationError {
    override def message: String =
      s"$value should not be equal to $expected"
  }

  /**
   * `str` does not match the regular expression `expected`. Returned by
   * `Validation.regex`.
   */
  final case class RegexMatch(str: String, expected: Regex) extends ValidationError {
    override def message: String =
      s"$str does not match $expected"
  }

  /**
   * `str` matches the regular expression `expected`. Returned by a negated
   * `Validation.regex`.
   */
  final case class NotRegexMatch(str: String, expected: Regex) extends ValidationError {
    override def message: String =
      s"$str matches a regex other than $expected"
  }

  /** A record has no value for `field`. Returned by `Schema.validate`. */
  final case class MissingField(field: zio.schema.Schema.Field[_, _]) extends ValidationError {
    override def message: String =
      s"Missing field ${field.name}"
  }

  /**
   * An error with a free-form `message`. `Schema.validate` returns it, with the
   * error text, when a `Schema.Transform` cannot convert a value back to its
   * underlying type.
   */
  final case class Generic(message: String) extends ValidationError
}
