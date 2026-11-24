package zio.schema.validation

/**
 * Predefined string validations built on `Regex`. `Validation` extends this trait, so they are available as `Validation.email`, `Validation.ipV4` and so on.
 */
trait Regexs {

  /**
   * Passes when the string has one or more ASCII letters, digits or underscores and nothing else. A leading digit passes (`1abc`); `a-b` and the empty string fail.
   */
  val identifier: Validation[String] =
    Validation.regex((Regex.digitOrLetter | Regex.oneOf('_')).atLeast(1))

  /**
   * Passes when the string has the shape of an email address and at most 254 characters: a local part of up to 64 characters, `@`, then a domain name, a bracketed IPv4 address (`a@[192.168.0.1]`) or a bracketed `IPv6:` address. The domain's last label needs at least two characters (`a@b.c` fails).
   *
   * The check is a pattern match only: `a..b@c.com` and `a.@b.com` pass. A bracketed IPv6 address follows the rules of `ipV6`, so `a@[IPv6:::1]` fails.
   */
  lazy val email: Validation[String] = {
    val localPart = (Regex.digitOrLetter | Regex.oneOf('_', '+', '-')) ~ (Regex.digitOrLetter | Regex.oneOf(
      '_',
      '.',
      '+',
      '-'
    )).atMost(63)
    val domainSegment           = Regex.digitOrLetter | Regex.oneOf('-')
    val topLevelDomain          = domainSegment.between(2, 63)
    val domainPart              = (domainSegment.atLeast(1) ~ Regex.oneOf('.')).atLeast(1) ~ topLevelDomain
    val ipBlock: Regex => Regex = Regex.oneOf('[') ~ _ ~ Regex.oneOf(']')
    val emailBeginning          = localPart ~ Regex.oneOf('@')
    val emailWithNormalDomain   = emailBeginning ~ domainPart
    val emailWithIpv4Domain     = emailBeginning ~ ipBlock(ipV4Regex)
    val emailWithIpv6Domain     = emailBeginning ~ ipBlock(Regex.literal("IPv6:") ~ ipV6Regex)
    val completeEmail           = emailWithNormalDomain | emailWithIpv4Domain | emailWithIpv6Domain

    Validation.regex(completeEmail) && Validation.maxLength(254)
  }

  /**
   * Passes when the string is an ISO-8601 duration such as `P1D`, `PT1H`, `P2W` or `P1Y2M3DT4H5M6S`. Fractions pass when their last digit is not `0` (`PT1.25S` passes, `PT0.50S` fails). Weeks combined with other units (`P1W1D`), negative durations (`-P1D`) and a bare `P` or `PT` fail.
   */
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

    Validation.regex(duration)
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
   * Passes when the string is four dot-separated decimal parts, each from 0 to 255, such as `192.168.0.1`; `256.1.1.1` and `1.2.3` fail. A part may have leading zeros (`001.02.3.4` passes) or be empty (`1..2.3` and `...` pass).
   */
  lazy val ipV4: Validation[String] = Validation.regex(ipV4Regex)

  private lazy val ipV6Regex: Regex = {
    val oneDigitHex   = Regex.hexDigit.exactly(1)
    val twoDigitHex   = Regex.hexDigitNonZero.exactly(1) ~ Regex.hexDigit.exactly(1)
    val threeDigitHex = Regex.hexDigitNonZero.exactly(1) ~ Regex.hexDigit.exactly(2)
    val fourDigitHex  = Regex.hexDigit.exactly(1) ~ Regex.hexDigit.exactly(3)

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
   * Passes when the string is an IPv6 address written in full (`1:2:3:4:5:6:7:8`) or with `::` after at least one group (`2001:db8::1`, `1::`), optionally ending in an IPv4 address (`1::1.2.3.4`). Addresses that start with `::` fail (`::1`, `::`, `::ffff:192.168.0.1`), as do zone ids (`fe80::1%eth0`) and two- or three-digit groups with a leading `0` (`01::1`).
   */
  lazy val ipV6: Validation[String] = Validation.regex(ipV6Regex)

  /**
   * Passes when the string is a version 4 UUID in the 8-4-4-4-12 hexadecimal form, in upper or lower case: the version digit is `4`, and the variant digit is `8`, `9`, `a` or `b` (either case). `123e4567-e89b-42d3-a456-556642440000` passes.
   */
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
