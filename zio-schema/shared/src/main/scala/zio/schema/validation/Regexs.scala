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
    val optionalSpace = Regex.oneOf(' ').atMost(1)
    val twoDigits     = Regex.digit.between(2, 2)
    val threeDigits   = Regex.digit.between(3, 3)
    Validation.regex(
      (((Regex.oneOf('+') | Regex
        .oneOf('0')
        .between(2, 2)) ~ Regex.oneOf('4') ~ Regex.oneOf('1')) | Regex.oneOf('0')) ~ optionalSpace ~
        twoDigits ~ optionalSpace ~
        threeDigits ~ optionalSpace ~
        twoDigits ~ optionalSpace ~
        twoDigits
    )
  }
}
