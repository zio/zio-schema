package zio.schema.validation
import zio.schema.validation.Regex.CharacterSet
import zio.schema.validation.Regex.Repeat
import zio.schema.validation.Regex.Empty
import zio.schema.validation.Regex.Alternate

sealed trait Regex {
  def atLeast(n: Int): Regex             = Regex.Repeat(this, Some(n), None)
  def atMost(n: Int): Regex              = Regex.Repeat(this, None, Some(n))
  def between(min: Int, max: Int): Regex = Regex.Repeat(this, Some(min), Some(max))
  def + : Regex                          = atLeast(1)
  def * : Regex                          = atLeast(0)
  def ~(that: Regex): Regex              = Regex.Sequence(this, that)
  def |(that: Regex): Regex              = Regex.Alternate(this, that)

  def test(string: String): Boolean = {
    def loop(regex: Regex, n: Int): Int =
      regex match {
        case CharacterSet(set) =>
          if (n >= string.length) -1
          else {
            val char = string.charAt(n)
            if (set.contains(char)) n + 1
            else -1
          }
        case Repeat(regex, min0, max0) =>
          val min            = min0.getOrElse(0)
          val max            = max0.getOrElse(Int.MaxValue)
          var matchCount     = 0
          var n2             = n
          var lastValidIndex = n2
          while (matchCount < max && n2 >= 0) {
            n2 = loop(regex, n2)
            if (n2 >= 0) {
              matchCount = matchCount + 1
              lastValidIndex = n2
            }
          }
          if (matchCount >= min && matchCount <= max) lastValidIndex
          else -1
        case Empty => n
        case Alternate(left, right) =>
          val n2 = loop(left, n)
          if (n2 >= 0) n2
          else loop(right, n)
        case Regex.Character =>
          if (n >= string.length) -1
          else {
            val char = string.charAt(n)
            if (char.isValidChar) n + 1 //TODO check isValidChar does what we want
            else -1
          }
        case Regex.Digit =>
          if (n >= string.length) -1
          else {
            val char = string.charAt(n)
            if (char.isValidInt) n + 1 //TODO check isValidInt does what we want
            else -1
          }
        case Regex.Sequence(first, second) =>
          val n2 = loop(first, n)
          if (n2 < 0) -1
          else loop(second, n2)
      }

    loop(this, 0) >= 0
  }
}

object Regex /*extends Regexs*/ {
  final case class CharacterSet(set: Set[Char]) extends Regex

  final case class Repeat(regex: Regex, min: Option[Int], max: Option[Int]) extends Regex

  final case class Sequence(first: Regex, second: Regex) extends Regex

  final case object Character extends Regex

  final case object Digit extends Regex

  final case object Empty extends Regex

  final case class Alternate(left: Regex, right: Regex) extends Regex

  def oneOf(chars: Char*): Regex = CharacterSet(chars.toSet)

  def filter(f: Char => Boolean): Regex = CharacterSet((Char.MinValue to Char.MaxValue).filter(f).toSet)

  def between(minChar: Char, maxChar: Char): Regex = CharacterSet((minChar to maxChar).toSet)

  val digit: Regex = Digit

  val character: Regex = Character

  val digitOrCharacter: Regex = digit | character
}
