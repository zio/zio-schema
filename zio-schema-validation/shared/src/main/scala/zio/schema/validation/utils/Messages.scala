package zio.schema.validation.utils


sealed trait StringValidationErrors {
  def message: String
}
object StringValidationErrors {
  final case class RightTooLong(message: String = "Right too long") extends StringValidationErrors
  final case class LeftTooShort(message: String = "Left too short") extends StringValidationErrors
  final case class RightTooShort(message: String = "Right too short") extends StringValidationErrors
  final case class LeftTooLong(message: String = "Left too long")
}
