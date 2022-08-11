package zio.schema.validation

trait Regexs {

  val identifier: Validation[String] =
    Validation.regex((Regex.digitOrLetter | Regex.oneOf('_')).atLeast(1))

  /**
   * Checks whether a certain string represents a valid email address.
   */
  lazy val email: Validation[String] = {
    val localPart               = Regex.letter ~ (Regex.digitOrLetter | Regex.oneOf('_', '.', '+', '-')).atMost(63)
    val domainSegment           = Regex.digitOrLetter | Regex.oneOf('-')
    val topLevelDomain          = domainSegment.between(2, 4)
    val domainPart              = (domainSegment.atLeast(1) ~ Regex.oneOf('.')).atLeast(1) ~ topLevelDomain
    val ipBlock: Regex => Regex = Regex.oneOf('[') ~ _ ~ Regex.oneOf(']')
    val emailBeginning          = localPart ~ Regex.oneOf('@')
    val emailWithNormalDomain   = emailBeginning ~ domainPart
    val emailWithIpv4Domain     = emailBeginning ~ ipBlock(ipV4Regex)
    val emailWithIpv6Domain     = emailBeginning ~ ipBlock(Regex.literal("IPv6:") ~ ipV6Regex)
    val completeEmail           = emailWithNormalDomain | emailWithIpv4Domain | emailWithIpv6Domain

    Validation.regex(completeEmail) && Validation.maxLength(254)
  }

  private lazy val ipV4Regex: Regex = {
    val separator   = Regex.literal(".").exactly(1)
    val is0Or1      = Regex.oneOf('0', '1')
    val is0To4      = is0Or1 | Regex.oneOf('2', '3', '4')
    val is0To5      = is0To4 | Regex.oneOf('5')
    val is250To255  = Regex.literal("25") ~ is0To5
    val is200To249  = Regex.oneOf('2') ~ is0To4 ~ Regex.digit
    val isZeroTo199 = (is0Or1 ~ Regex.digit.exactly(2)) | Regex.digit.atMost(2)

    val bytePart = is250To255 | is200To249 | isZeroTo199

    bytePart ~ separator ~ bytePart ~ separator ~ bytePart ~ separator ~ bytePart
  }

  /**
   * Checks whether a certain string represents a valid IPv4 address.
   */
  lazy val ipV4: Validation[String] = Validation.regex(ipV4Regex)

  private lazy val ipV6Regex: Regex = {
    val oneDigitHex   = Regex.hexDigit.exactly(1)
    val twoDigitHex   = Regex.hexDigitNonZero.exactly(1) ~ Regex.hexDigit.exactly(1)
    val threeDigitHex = Regex.hexDigitNonZero.exactly(1) ~ Regex.hexDigit.exactly(2)
    val fourDigitHex  = Regex.hexDigitNonZero.exactly(1) ~ Regex.hexDigit.exactly(3)

    val hexGroup: Regex = fourDigitHex | threeDigitHex | twoDigitHex | oneDigitHex

    val colon: Regex = Regex.oneOf(':').exactly(1)

    val g8: Regex = (hexGroup ~ colon).exactly(7) ~ (hexGroup | colon)
    val g7: Regex = (hexGroup ~ colon).exactly(6) ~ ((colon ~ hexGroup) | ipV4Regex | colon)

    def g(n: Int): Regex =
      (hexGroup ~ colon).exactly(n - 1) ~
        ((colon ~ hexGroup).between(1, 8 - n) | ((colon ~ hexGroup).between(0, 6 - n) ~ (colon ~ ipV4Regex)) | colon)

    g(1) | g(2) | g(3) | g(4) | g(5) | g(6) | g7 | g8
  }

  /**
   * Checks whether a certain string represents a valid IPv6 address.
   */
  lazy val ipV6: Validation[String] = Validation.regex(ipV6Regex)

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
