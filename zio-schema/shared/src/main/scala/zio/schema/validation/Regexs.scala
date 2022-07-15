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
        Regex.oneOf('.')).atLeast(1) ~
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

  /**
   * Checks whether a certain string represents a valid IPv6 address.
   */
  lazy val ipV6: Validation[String] = {

    val is0To4: Regex     = Regex.oneOf('0', '1', '2', '3', '4')
    val is0To5: Regex     = is0To4 | Regex.oneOf('5')
    val twoDigits: Regex  = Regex.digitNonZero.exactly(1) ~ Regex.digit.exactly(1)
    val is0To99: Regex    = Regex.digit.exactly(1) | twoDigits.exactly(1)
    val is100To199: Regex = Regex.oneOf('1') ~ Regex.digit.exactly(2)
    val is200To249: Regex = Regex.oneOf('2') ~ is0To4 ~ Regex.digit
    val is250To255: Regex = Regex.literal("25") ~ is0To5
    val ipv4Part: Regex   = is250To255 | is200To249 | is100To199 | is0To99
    val ipv4: Regex       = ipv4Part ~ (Regex.oneOf('.').exactly(1) ~ ipv4Part).exactly(3)

    val oneDigitHex   = Regex.hexDigit.exactly(1)
    val twoDigitHex   = Regex.hexDigitNonZero.exactly(1) ~ Regex.hexDigit.exactly(1)
    val threeDigitHex = Regex.hexDigitNonZero.exactly(1) ~ Regex.hexDigit.exactly(2)
    val fourDigitHex  = Regex.hexDigitNonZero.exactly(1) ~ Regex.hexDigit.exactly(3)

    val hexGroup: Regex = fourDigitHex | threeDigitHex | twoDigitHex | oneDigitHex

    val colon: Regex = Regex.oneOf(':').exactly(1)

    val g8: Regex = (hexGroup ~ colon).exactly(7) ~ (hexGroup | colon)
    val g7: Regex = (hexGroup ~ colon).exactly(6) ~ ((colon ~ hexGroup) | ipv4 | colon)

    def g(n: Int): Regex =
      (hexGroup ~ colon).exactly(n - 1) ~
        ((colon ~ hexGroup).between(1, 8 - n) | ((colon ~ hexGroup).between(0, 6 - n) ~ (colon ~ ipv4)) | colon)

    Validation.regex(g(1) | g(2) | g(3) | g(4) | g(5) | g(6) | g7 | g8)
  }

  lazy val uuidV4: Validation[String] = {
    val hexOctect = Regex.hexDigit ~ Regex.hexDigit
    val sep       = Regex.oneOf('-')

    val timeLow            = hexOctect.exactly(4)
    val timeMid            = hexOctect.exactly(2)
    val timeHighAndVersion = Regex.oneOf('4') ~ Regex.hexDigit ~ hexOctect
    val clockSeq           = Regex.CharacterSet(Set('8', '9', 'a', 'A', 'b', 'B')) ~ Regex.hexDigit ~ hexOctect
    val node               = hexOctect.exactly(6)

    Validation.regex(
      timeLow ~ sep ~
        timeMid ~ sep ~
        timeHighAndVersion ~ sep ~
        clockSeq ~ sep ~
        node
    )
  }

}
