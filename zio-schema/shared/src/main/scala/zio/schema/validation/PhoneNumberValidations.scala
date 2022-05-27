package zio.schema.validation

trait PhoneNumberValidations {

  val optionalSpace: Regex     = Regex.literal(" ").atMost(1)
  val optionalSeparator: Regex = Regex.oneOf('-', ' ').atMost(1)
  val twoDigits: Regex         = Regex.digit.exactly(2)
  val threeDigits: Regex       = Regex.digit.exactly(3)
  val plus: Regex              = Regex.literal("+")
  val doubleZero: Regex        = Regex.literal("00")

  lazy val phoneNumberCh: Validation[String] = {
    val internationalPrefix = (plus | doubleZero) ~ Regex.literal("41")
    val nationalPrefix      = Regex.literal("0")
    val prefix              = (internationalPrefix | nationalPrefix)
    Validation.regex(
      prefix ~ optionalSpace ~
        twoDigits ~ optionalSpace ~
        threeDigits ~ optionalSpace ~
        twoDigits ~ optionalSpace ~
        twoDigits
    )
  }

  lazy val phoneNumberDe: Validation[String] = {
    val optionalSpace       = Regex.literal(" ").atMost(1)
    val internationalPrefix = (Regex.literal("+") | Regex.literal("00")) ~ Regex.literal("49")
    val nationalPrefix      = Regex.literal("0")
    val digitNonZero        = Regex.oneOf('1', '2', '3', '4', '5', '6', '7', '8', '9')
    val areaPrefix          = digitNonZero ~ Regex.digit.between(1, 4)
    val phoneNumber         = Regex.digit.between(3, 9)

    Validation.regex(
      (internationalPrefix | nationalPrefix) ~ optionalSpace ~
        areaPrefix ~ optionalSpace ~
        phoneNumber
    )
  }

  lazy val phoneNumberHu: Validation[String] = {
    val internationalPrefix = (plus | doubleZero) ~ Regex.literal("36")
    val nationalPrefix      = Regex.literal("06")
    val areaCode            = (Regex.oneOf('2', '3', '4', '5', '6', '7', '8', '9') ~ Regex.digit) | Regex.oneOf('1')
    val prefix              = (internationalPrefix | nationalPrefix)
    Validation.regex(
      prefix ~ optionalSeparator ~
        areaCode ~ optionalSeparator ~
        threeDigits ~ optionalSeparator ~
        twoDigits ~ optionalSeparator ~
        twoDigits
    )
  }

  lazy val phoneNumberPt: Validation[String] = {
    val internationalPrefix = (plus | doubleZero) ~ Regex.literal("351")
    val nationalPrefix      = Regex.literal("")
    val prefix              = (internationalPrefix | nationalPrefix)
    Validation.regex(
      prefix ~ optionalSpace ~
        ((twoDigits ~ optionalSpace ~ threeDigits ~ optionalSpace ~ twoDigits ~ optionalSpace ~ twoDigits) |
          (threeDigits ~ optionalSpace ~ threeDigits ~ optionalSpace ~ threeDigits)))
  }

}

object PhoneNumberValidations extends PhoneNumberValidations {

}
