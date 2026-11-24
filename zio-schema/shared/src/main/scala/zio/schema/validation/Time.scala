package zio.schema.validation

import scala.annotation.tailrec
import scala.collection.mutable

/**
 * Provides `time`, the time-of-day validation that the `Validation` companion
 * object inherits.
 */
trait Time {

  /**
   * Returns a `Validation` that passes when the string matches the time
   * `format`. The letters are close to those of
   * `java.time.format.DateTimeFormatter`, with these accepted ranges:
   *
   *   - `H`: hour, one or two digits, `0` to `24` (`00` and `24` pass)
   *   - `HH`: hour, two digits, `00` to `24`
   *   - `h`: hour, one or two digits, `0` to `12` (`0` and `00` pass)
   *   - `hh`: hour, two digits, `00` to `12`
   *   - `m` and `s`: minute and second, one or two digits, `0` to `59`
   *   - `mm` and `ss`: minute and second, two digits, `00` to `59`
   *   - `S` to `SSSSSSSSS`: fraction of a second, exactly as many digits as
   *     letters (one to nine)
   *   - `a`: `AM` or `PM`, upper case
   *
   * Other characters are matched literally. Each field is checked on its own,
   * so `HH:mm a` accepts `13:10 PM`. A letter that follows a non-letter literal
   * is also matched literally: `HH:xx` accepts `10:xx`.
   *
   * {{{
   * import zio.schema.validation.Validation
   *
   * val hourMinute = Validation.time("HH:mm")
   *
   * val padded   = hourMinute.validate("01:10").isRight // true
   * val unpadded = hourMinute.validate("1:10").isRight  // false
   * }}}
   *
   * Throws a `java.lang.IllegalArgumentException` when `format` has no time
   * field, repeats a field (`H:H`), has a field longer than allowed (`HHH`), or
   * has another letter at its start or right after a field (`HHx`).
   */
  def time(format: String): Validation[String] = {
    val regex = parseFormat(format)
    Validation.regex(regex)
  }

  sealed private trait Field
  private case class TimeField(letter: Char, length: Int, maxLength: Int) extends Field
  private case class Literal(value: String)                               extends Field

  private val fields = Map[Char, Field](
    'H' -> TimeField('H', 1, 2),
    'h' -> TimeField('h', 1, 2),
    'm' -> TimeField('m', 1, 2),
    's' -> TimeField('s', 1, 2),
    'a' -> TimeField('a', 1, 1),
    'S' -> TimeField('S', 1, 9)
  )

  private def parseFormat(format: String): Regex = {
    val length               = format.length
    var pos                  = 0
    var field: Option[Field] = None
    val usedFields           = mutable.ListBuffer.empty[Field]
    val result               = mutable.ListBuffer.empty[Field]

    def setField(cur: Char): Unit = {
      field = fields.get(cur)

      field.fold {
        if (cur >= 'a' && cur <= 'z' || cur >= 'A' && cur <= 'Z') {
          throw new IllegalArgumentException(s"Invalid time format: $format. All letters are reserved.")
        } else {
          field = Some(Literal(cur.toString))
        }
      } { f =>
        if (usedFields.contains(f)) {
          throw new IllegalArgumentException(s"Character $cur already used in format $format")
        } else {
          usedFields += f
        }
      }
    }

    while (pos < length) {
      val cur = format.charAt(pos)

      field match {
        case None                                                 => setField(cur)
        case Some(f @ TimeField(letter, _, _)) if (letter != cur) =>
          result += f
          setField(cur)
        case Some(TimeField(letter, length, maxLength)) if (length == maxLength) =>
          throw new IllegalArgumentException(s"Invalid time format: $format max length for ${letter} is ${maxLength}")
        case Some(f @ TimeField(_, length, _)) =>
          field = Some(f.copy(length = length + 1))
        case Some(l @ Literal(_)) if fields.contains(cur) =>
          result += l
          setField(cur)
        case Some(l @ Literal(value)) =>
          field = Some(l.copy(value = value + cur))

      }

      pos += 1

    }
    field.foreach(f => result += f)

    @tailrec
    def loop(list: List[Field], regex: Seq[Regex]): Seq[Regex] = list match {
      case Nil          => regex
      case head :: tail => loop(tail, regex :+ fieldToRegex(head))
    }

    if (usedFields.isEmpty) {
      throw new IllegalArgumentException(s"There is no time field (${fields.keySet.mkString(",")}) in format $format")
    }

    val regexes = loop(result.toList, Seq.empty)

    if (regexes.isEmpty) {
      throw new IllegalArgumentException(s"Invalid time format: $format")
    } else {
      regexes.reduce(_ ~ _)
    }
  }

  private val from20to24 = Regex.oneOf('2') ~ Regex.between('0', '4')
  private val from10to19 = Regex.oneOf('1') ~ Regex.digit
  private val from00to09 = Regex.oneOf('0') ~ Regex.digit
  private val from00to19 = Regex.oneOf('0', '1') ~ Regex.digit
  private val from10to12 = Regex.oneOf('1') ~ Regex.oneOf('0', '1', '2')
  private val from10to59 = Regex.between('1', '5') ~ Regex.digit
  private val from00to59 = Regex.between('0', '5') ~ Regex.digit
  private val from0to9   = Regex.digit

  private def fieldToRegex(field: Field): Regex = field match {
    case TimeField('H', 1, _)                        => from20to24 | from10to19 | from00to09 | from0to9
    case TimeField('H', 2, _)                        => from00to19 | from20to24
    case TimeField('h', 1, _)                        => from10to12 | from00to09 | from0to9
    case TimeField('h', 2, _)                        => from00to09 | from10to12
    case TimeField('m', 1, _) | TimeField('s', 1, _) => from10to59 | from00to09 | from0to9
    case TimeField('m', 2, _) | TimeField('s', 2, _) => from00to59
    case TimeField('S', length, _)                   => Regex.digit.between(length, length)
    case TimeField('a', _, _)                        => Regex.oneOf('A', 'P').between(1, 1) ~ Regex.oneOf('M').between(1, 1)
    case TimeField(_, _, _)                          =>
      throw new IllegalArgumentException(s"Something went terribly wrong. This is a bug. Please report it.")
    case Literal(l) => l.map(c => Regex.oneOf(c)).reduce(_ ~ _)
  }

}
