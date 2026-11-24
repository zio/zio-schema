package zio.schema.validation

import java.util.regex.Pattern

/**
 * A regular expression described as data, used by `Validation.regex`. Build one
 * from the constructors on the companion object and combine them with `~`, `|`
 * and the repetition methods; `Regex.toRegexString` renders it as a
 * `java.util.regex.Pattern` string.
 *
 * {{{
 * import zio.schema.validation.Regex
 *
 * val zipCode: Regex = Regex.digit.exactly(5) ~ (Regex.oneOf('-') ~ Regex.digit.exactly(4)).?
 *
 * val ok       = zipCode.test("12345-6789") // true
 * val tooShort = zipCode.test("1234")       // false
 * }}}
 */
sealed trait Regex { self =>

  /** Returns a regex that matches this one `n` or more times in a row. */
  def atLeast(n: Int): Regex = Regex.Repeat(this, Some(n), None)

  /**
   * Returns a regex that matches this one between zero and `n` times in a row.
   */
  def atMost(n: Int): Regex = Regex.Repeat(this, None, Some(n))

  /**
   * Returns a regex that matches this one between `min` and `max` times in a
   * row, both included.
   */
  def between(min: Int, max: Int): Regex = Regex.Repeat(this, Some(min), Some(max))

  /** Returns a regex that matches this one exactly `n` times in a row. */
  def exactly(n: Int): Regex = Regex.Repeat(this, Some(n), Some(n))

  /**
   * Returns a regex that matches this one one or more times, the same as
   * `atLeast(1)`.
   */
  def + : Regex = atLeast(1)

  /**
   * Returns a regex that matches this one zero or more times, the same as
   * `atLeast(0)`.
   */
  def * : Regex = atLeast(0)

  /**
   * Returns a regex that matches this one zero or one time, the same as
   * `atMost(1)`.
   */
  def ? : Regex = atMost(1)

  /** Returns a regex that matches this one followed by `that`. */
  def ~(that: Regex): Regex = Regex.Sequence(this, that)

  /** Returns a regex that matches either this one or `that`. */
  def |(that: Regex): Regex = Regex.Alternate(this, that)

  /**
   * Returns `true` when the whole `string` matches this regex. It compiles the
   * pattern from `Regex.toRegexString` on each call.
   *
   * Throws a `java.util.regex.PatternSyntaxException` when the generated
   * pattern is invalid, for example for an empty `Regex.CharacterSet` or a
   * repeat whose minimum is greater than its maximum.
   */
  def test(string: String): Boolean = Pattern.compile(Regex.toRegexString(self)).matcher(string).matches()

}

/** The cases of `Regex`, its constructors, and predefined character classes. */
object Regex {

  /**
   * Matches one character from `set`. An empty `set` gives an invalid pattern.
   */
  final case class CharacterSet(set: Set[Char]) extends Regex

  /**
   * Matches the text `str` exactly; special regex characters in it are escaped.
   */
  final case class Literal(str: String) extends Regex

  /**
   * Matches one character in the range from `start` to `end`, both included.
   */
  final case class Between(start: Char, end: Char) extends Regex

  /**
   * Matches `regex` repeated between `min` and `max` times. A missing `min`
   * means zero and a missing `max` means no upper limit; `Regex.toRegexString`
   * throws a `java.lang.IllegalArgumentException` when both are `None`.
   */
  final case class Repeat(regex: Regex, min: Option[Int], max: Option[Int]) extends Regex

  /** Matches `first` followed by `second`. */
  final case class Sequence(first: Regex, second: Regex) extends Regex

  /** Matches one ASCII letter, `a` to `z` or `A` to `Z`. */
  case object Letter extends Regex

  /** Matches one ASCII digit, `0` to `9` (Java's `\d`). */
  case object Digit extends Regex

  /** Matches the empty string. */
  case object Empty extends Regex

  /** Matches `left` or `right`. */
  final case class Alternate(left: Regex, right: Regex) extends Regex

  /**
   * Returns a regex that matches one character from `minChar` to `maxChar`,
   * both included.
   */
  def between(minChar: Char, maxChar: Char): Regex = Between(minChar, maxChar)

  /**
   * Returns a regex that matches one character for which `f` returns `true`. It
   * tests `f` on each of the 65536 `Char` values when called.
   */
  def filter(f: Char => Boolean): Regex = CharacterSet((Char.MinValue to Char.MaxValue).filter(f).toSet)

  /** Returns a regex that matches the text `str` exactly. */
  def literal(str: String): Regex = Literal(str)

  /** Returns a regex that matches one of `chars`. */
  def oneOf(chars: Char*): Regex = CharacterSet(chars.toSet)

  /** Matches one ASCII digit, `0` to `9`. */
  val digit: Regex = Digit

  /** Matches one digit from `1` to `9`. */
  val digitNonZero: Regex = between('1', '9')

  /** Matches one ASCII letter, `a` to `z` or `A` to `Z`. */
  val letter: Regex = Letter

  /** Matches one ASCII digit or letter. */
  val digitOrLetter: Regex = digit | letter

  /** Matches one hexadecimal digit: `0` to `9`, `a` to `f` or `A` to `F`. */
  val hexDigit: Regex = Digit | between('a', 'f') | between('A', 'F')

  /**
   * Matches one hexadecimal digit other than `0`: `1` to `9`, `a` to `f` or `A`
   * to `F`.
   */
  val hexDigitNonZero: Regex = digitNonZero | between('a', 'f') | between('A', 'F')

  private def escapeChar(ch: Char): String =
    ch match {
      case '.'  => "\\."
      case '^'  => "\\^"
      case '$'  => "\\$"
      case '|'  => "\\|"
      case '*'  => "\\*"
      case '+'  => "\\+"
      case '?'  => "\\?"
      case '('  => "\\("
      case ')'  => "\\)"
      case '['  => "\\["
      case ']'  => "\\]"
      case '{'  => "\\{"
      case '}'  => "\\}"
      case '\\' => "\\\\"
      case '-'  => "\\-"
      case _    => ch.toString
    }

  /**
   * Returns `regex` as a `java.util.regex.Pattern` string, for example `(\d)+`
   * for `Regex.digit.+` and `[a-zA-Z]` for `Regex.letter`. It escapes the
   * special characters of literals and character sets.
   *
   * Throws a `java.lang.IllegalArgumentException` when `regex` contains a
   * `Repeat` with neither a minimum nor a maximum.
   */
  def toRegexString(regex: Regex): String = {
    def loop(regex: Regex): String =
      regex match {
        case CharacterSet(elem) if elem.size == 1 =>
          escapeChar(elem.head)
        case CharacterSet(set) =>
          set.toList.sorted.map(escapeChar).mkString("[", "", "]")
        case Literal(str) =>
          val escaped = str.flatMap(escapeChar)
          s"($escaped)"
        case Between(start, end) =>
          val escapedStart = escapeChar(start)
          val escapedEnd   = escapeChar(end)
          s"[$escapedStart-$escapedEnd]"
        case Repeat(regex, Some(0), None) =>
          s"(${loop(regex)})*"
        case Repeat(regex, Some(1), None) =>
          s"(${loop(regex)})+"
        case Repeat(regex, None, Some(1)) =>
          s"(${loop(regex)})?"
        case Repeat(regex, Some(n), Some(m)) if n == m =>
          s"(${loop(regex)}){$n}"
        case Repeat(regex, Some(n), None) =>
          s"(${loop(regex)}){$n,}"
        case Repeat(regex, None, Some(m)) =>
          s"(${loop(regex)}){0,$m}"
        case Repeat(regex, Some(n), Some(m)) =>
          s"(${loop(regex)}){$n,$m}"
        case Repeat(_, None, None) =>
          throw new IllegalArgumentException("Cannot have no repeat count")
        case Empty                  => ""
        case Alternate(left, right) =>
          s"((${loop(left)})|(${loop(right)}))"
        case Regex.Letter =>
          "[a-zA-Z]"
        case Regex.Digit =>
          "\\d"
        case Regex.Sequence(first, second) =>
          s"${loop(first)}${loop(second)}"
      }

    loop(regex)
  }
}
