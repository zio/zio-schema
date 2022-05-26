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

  lazy val phoneNumberCh: Validation[String] = {
    val optionalSpace       = Regex.literal(" ").atMost(1)
    val twoDigits           = Regex.digit.exactly(2)
    val threeDigits         = Regex.digit.exactly(3)
    val plus                = Regex.literal("+")
    val doubleZero          = Regex.literal("00")
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
}
