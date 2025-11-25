package zio.schema.validation

import java.util.regex.Pattern

sealed trait Regex { self =>
  def atLeast(n: Int): Regex             = Regex.Repeat(this, Some(n), None)
  def atMost(n: Int): Regex              = Regex.Repeat(this, None, Some(n))
  def between(min: Int, max: Int): Regex = Regex.Repeat(this, Some(min), Some(max))
  def exactly(n: Int): Regex             = Regex.Repeat(this, Some(n), Some(n))
  def + : Regex                          = atLeast(1)
  def * : Regex                          = atLeast(0)
  def ? : Regex                          = atMost(1)
  def ~(that: Regex): Regex              = Regex.Sequence(this, that)
  def |(that: Regex): Regex              = Regex.Alternate(this, that)
  def test(string: String): Boolean      = Pattern.compile(Regex.toRegexString(self)).matcher(string).matches()

}

object Regex {
  final case class CharacterSet(set: Set[Char]) extends Regex

  final case class Literal(str: String) extends Regex

  final case class Between(start: Char, end: Char) extends Regex

  final case class Repeat(regex: Regex, min: Option[Int], max: Option[Int]) extends Regex

  final case class Sequence(first: Regex, second: Regex) extends Regex

  case object Letter extends Regex

  case object Digit extends Regex

  case object Empty extends Regex

  final case class Alternate(left: Regex, right: Regex) extends Regex

  def between(minChar: Char, maxChar: Char): Regex = Between(minChar, maxChar)

  def filter(f: Char => Boolean): Regex = CharacterSet((Char.MinValue to Char.MaxValue).filter(f).toSet)

  def literal(str: String): Regex = Literal(str)

  def oneOf(chars: Char*): Regex = CharacterSet(chars.toSet)

  val digit: Regex = Digit

  val digitNonZero: Regex = between('1', '9')

  val letter: Regex = Letter

  val digitOrLetter: Regex = digit | letter

  val hexDigit: Regex = Digit | between('a', 'f') | between('A', 'F')

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
      case _    => ch.toString
    }

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
