package zio.schema.validation

trait Regexs {

  val identifier: Validation[String] =
    Validation.regex((Regex.digitOrLetter | Regex.oneOf('_')).atLeast(1))

  //^[\w-\.]+@([\w-]+\.)+[\w-]{2,4}$
  lazy val email: Validation[String] = {
    val username       = Regex.letter ~ (Regex.digitOrLetter | Regex.oneOf('_', '.', '+', '-')).atLeast(0)
    val topLevelDomain = (Regex.digitOrLetter | Regex.oneOf('-')).between(2, 4)
    val domain =
      ((Regex.digitOrLetter | Regex.oneOf('-')).atLeast(1) ~ Regex.oneOf('.')).atLeast(1) ~ topLevelDomain

    Validation.regex(
      username ~
        Regex.oneOf('@') ~
        domain
    )
  }

  lazy val duration: Validation[String] = {

    val posDigit = Regex.between('1', '9')
    val integer  = Regex.digit.+
    val number   = integer ~ (Regex.oneOf('.') ~ Regex.digit.* ~ posDigit).?

    val second = number ~ Regex.oneOf('S')
    val minute = number ~ Regex.oneOf('M') ~ second.?
    val hour   = number ~ Regex.oneOf('H') ~ minute.? ~ second.?
    val time   = Regex.oneOf('T') ~ (hour | minute | second)

    val day      = number ~ Regex.oneOf('D')
    val week     = number ~ Regex.oneOf('W')
    val month    = number ~ Regex.oneOf('M') ~ day.?
    val year     = number ~ Regex.oneOf('Y') ~ month.? ~ day.?
    val date     = (day | month | year) ~ time.?
    val duration = Regex.oneOf('P') ~ (date | time | week)
    val regex    = BuiltInRegex(duration)

    Validation.builtInRegex(regex)
  }

  /**
   * Checks whether a certain string represents a valid IPv4 address.
   */
  lazy val ipV4: Validation[String] = {
    val separator           = Regex.literal(".").exactly(1)
    val isZeroOrOne: Regex  = Regex.oneOf('0', '1')
    val isZeroToFour: Regex = isZeroOrOne | Regex.oneOf('2', '3', '4')
    val isZeroToFive: Regex = isZeroToFour | Regex.oneOf('5')
    val is250To255: Regex   = Regex.literal("25") ~ isZeroToFive
    val is200To249: Regex   = Regex.literal("2") ~ isZeroToFour ~ Regex.digit
    val isZeroTo199: Regex  = (isZeroOrOne ~ Regex.digit.exactly(2)) | Regex.digit.atMost(2)

    val bytePart = is250To255 | is200To249 | isZeroTo199

    Validation.regex(bytePart ~ separator ~ bytePart ~ separator ~ bytePart ~ separator ~ bytePart)
  }

}
