package zio.schema.validation

import zio.schema.validation.Regex.{ Alternate, CharacterSet, Empty, Repeat }

case class BuiltInRegex(rege: Regex) { self =>

  def test(s: String): Boolean = {
    val compiledRegex = BuiltInRegex.toRegexString(rege).r
    compiledRegex.matches(s)
  }
}

/**
 * Produce a regex string from the encoding and use scala's native regex matcher
 */
object BuiltInRegex {

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
        case Empty => ""
        case Alternate(left, right) =>
          s"(${loop(left)})|(${loop(right)})"
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
