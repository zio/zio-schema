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

  private def bool2int(b: Boolean) = if (b) 1 else 0

  def test(string: String): Boolean = {
    def loop(regex: Regex, n: Int): Int =
      if (n >= string.length()) -1
      else
        regex match {
          case CharacterSet(set)             => ???
          case Repeat(regex, min, max)       => ???
          case Empty                         => ???
          case Alternate(left, right)        => ???
          case Regex.Character               => bool2int(string(n).isValidChar)
          case Regex.Digit                   => bool2int(string(n).isDigit)
          case Regex.Sequence(first, second) => ???
        }

    loop(this, 0) >= 0
  }
}

object Regex extends CommonRegexs {
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

trait CommonRegexs {
  val email: Regex = ???
  // TODO Phone numbers, ip addresses, currencies, etc
}
