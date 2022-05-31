package zio.schema.validation

trait Regexs {

  val identifier: Validation[String] =
    Validation.regex((Regex.digitOrLetter | Regex.oneOf('_')).atLeast(1))

  //^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$
  lazy val email: Validation[String] = {
    val username       = Regex.letter ~ (Regex.digitOrLetter | Regex.oneOf('_', '.', '+', '-')).atLeast(0)
    val topLevelDomain = (Regex.digitOrLetter | Regex.oneOf('-')).between(2, 4)
    val domain =
      ((Regex.digitOrLetter | Regex.oneOf('-')).atLeast(1) ~
        (Regex.oneOf('.'))).atLeast(1) ~
        topLevelDomain

    Validation.regex(
      username ~
        Regex.oneOf('@') ~
        domain
    )
  }

  /**
   * Checks whether a certain string represents a valid IPv4 address.
   */
  lazy val ipV4: Validation[String] = {
    // Four parts of "^(25[0-5]|2[0-4]\\d|[01]?\\d\\d?)$".r
    val separator           = Regex.literal(".").exactly(1)
    val isZeroToFour: Regex = Regex.oneOf('0', '1', '2', '3', '4')
    val isZeroToFive: Regex = isZeroToFour | Regex.oneOf('5')
    val isZeroOrOne: Regex  = Regex.oneOf('0', '1')
    val is250To255: Regex   = Regex.literal("25") ~ isZeroToFive
    val is200To249: Regex   = Regex.literal("2") ~ isZeroToFour ~ Regex.digit
    val isZeroTo199: Regex  = (isZeroOrOne ~ Regex.digit.exactly(2)) | Regex.digit.atMost(2)

    val bytePart = is250To255 | is200To249 | isZeroTo199

    Validation.regex(bytePart ~ separator ~ bytePart ~ separator ~ bytePart ~ separator ~ bytePart)
  }

  object PhoneNumberRegexs {
    val optionalSpace: Regex     = Regex.literal(" ").atMost(1)
    val optionalSeparator: Regex = Regex.oneOf('-', ' ').atMost(1)
    val twoDigits: Regex         = Regex.digit.exactly(2)
    val threeDigits: Regex       = Regex.digit.exactly(3)
    val plus: Regex              = Regex.literal("+")
    val doubleZero: Regex        = Regex.literal("00")
  }

  lazy val phoneNumberCh: Validation[String] = {
    import PhoneNumberRegexs._

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
    import PhoneNumberRegexs._

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
}
