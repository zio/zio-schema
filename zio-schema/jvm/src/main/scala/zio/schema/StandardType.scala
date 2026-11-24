package zio.schema

import java.math.BigInteger
import java.time
import java.time._

import zio.Chunk

/**
 * A `StandardType[A]` names one of the primitive types that zio-schema handles directly: `Unit`, `String`, `Boolean`, the numeric primitives, `Char`, `Chunk[Byte]` (binary), `java.math.BigDecimal`, `java.math.BigInteger`, `java.util.UUID`, `java.util.Currency` and 16 `java.time` types. Each instance carries a string `tag`, a `defaultValue` and an `Ordering[A]`.
 *
 * A [[Schema.Primitive]] wraps a `StandardType`, and codecs match on it to decide how to write the value. Summon an instance with `StandardType[A]`, and turn it into a schema with `Schema.primitive[A]`.
 *
 * {{{
 * val intType: StandardType[Int] = StandardType[Int]
 * val tag: String                = intType.tag          // "int"
 * val default                    = intType.defaultValue // Right(0)
 * val byTag                      = StandardType.fromString("localDate") // Some(localDate)
 * val schema: Schema[Int]        = Schema.primitive(intType)
 * }}}
 */
sealed trait StandardType[A] extends Ordering[A] { self =>
  /**
   * Returns the name of this standard type, such as `"int"` or `"localDate"`. `StandardType.fromString` maps a tag back to its instance, and `toString` returns the tag.
   */
  def tag: String
  /**
   * Returns the default value of this type, or `Left` with a message when the type has none on this platform. Most instances return a fixed value (`0`, `""`, `false`, an empty chunk, `Instant.EPOCH`). `UUIDType`, `YearType`, `YearMonthType`, `ZoneIdType`, `DayOfWeekType` and the `now`-based date and time types compute a new value from a random source, the clock, the system zone or the default locale on each call. On Scala.js, `UUIDType` and `CurrencyType` return `Left`; on Scala Native, `CurrencyType` returns `Left`.
   */
  def defaultValue: Either[String, A]
  override def toString: String = tag

  /**
   * Converts `value` to an `A` through a primitive schema for this type, or
   * returns `Left` with a message when `value` does not hold an `A`.
   */
  def toTypedPrimitive(value: DynamicValue): Either[String, A] =
    value.toTypedValue(Schema.primitive[A](self))
}

/**
 * The [[StandardType]] instances, one implicit object per supported primitive type, and lookup by tag.
 */
object StandardType {

  private[schema] object Tags {
    final val UNIT             = "unit"
    final val STRING           = "string"
    final val BOOL             = "boolean"
    final val BYTE             = "byte"
    final val SHORT            = "short"
    final val INT              = "int"
    final val LONG             = "long"
    final val FLOAT            = "float"
    final val DOUBLE           = "double"
    final val BINARY           = "binary"
    final val CHAR             = "char"
    final val BIG_DECIMAL      = "bigDecimal"
    final val BIG_INTEGER      = "bigInteger"
    final val DAY_OF_WEEK      = "dayOfWeek"
    final val MONTH            = "month"
    final val MONTH_DAY        = "monthDay"
    final val PERIOD           = "period"
    final val YEAR             = "year"
    final val YEAR_MONTH       = "yearMonth"
    final val ZONE_ID          = "zoneId"
    final val ZONE_OFFSET      = "zoneOffset"
    final val DURATION         = "duration"
    final val INSTANT          = "instant"
    final val LOCAL_DATE       = "localDate"
    final val LOCAL_TIME       = "localTime"
    final val LOCAL_DATE_TIME  = "localDateTime"
    final val OFFSET_TIME      = "offsetTime"
    final val OFFSET_DATE_TIME = "offsetDateTime"
    final val ZONED_DATE_TIME  = "zonedDateTime"
    final val UUID             = "uuid"
    final val CURRENCY         = "currency"
  }

  /**
   * Returns the standard type whose `tag` equals `tag`, for example `Some(UnitType)` for `"unit"`. For a string that is not one of the 31 known tags it throws a `scala.MatchError` instead of returning `None`.
   */
  def fromString(tag: String): Option[StandardType[_]] =
    tag match {
      case Tags.UNIT             => Some(UnitType)
      case Tags.STRING           => Some(StringType)
      case Tags.BOOL             => Some(BoolType)
      case Tags.BYTE             => Some(ByteType)
      case Tags.SHORT            => Some(ShortType)
      case Tags.INT              => Some(IntType)
      case Tags.LONG             => Some(LongType)
      case Tags.FLOAT            => Some(FloatType)
      case Tags.DOUBLE           => Some(DoubleType)
      case Tags.BINARY           => Some(BinaryType)
      case Tags.CHAR             => Some(CharType)
      case Tags.BIG_DECIMAL      => Some(BigDecimalType)
      case Tags.BIG_INTEGER      => Some(BigIntegerType)
      case Tags.MONTH            => Some(MonthType)
      case Tags.MONTH_DAY        => Some(MonthDayType)
      case Tags.PERIOD           => Some(PeriodType)
      case Tags.DAY_OF_WEEK      => Some(DayOfWeekType)
      case Tags.YEAR             => Some(YearType)
      case Tags.YEAR_MONTH       => Some(YearMonthType)
      case Tags.ZONE_ID          => Some(ZoneIdType)
      case Tags.ZONE_OFFSET      => Some(ZoneOffsetType)
      case Tags.DURATION         => Some(DurationType)
      case Tags.INSTANT          => Some(InstantType)
      case Tags.LOCAL_DATE       => Some(LocalDateType)
      case Tags.LOCAL_TIME       => Some(LocalTimeType)
      case Tags.LOCAL_DATE_TIME  => Some(LocalDateTimeType)
      case Tags.OFFSET_TIME      => Some(OffsetTimeType)
      case Tags.OFFSET_DATE_TIME => Some(OffsetDateTimeType)
      case Tags.ZONED_DATE_TIME  => Some(ZonedDateTimeType)
      case Tags.UUID             => Some(UUIDType)
      case Tags.CURRENCY         => Some(CurrencyType)
    }

  /**
   * Summons the implicit `StandardType[A]`, so `StandardType[Int]` returns `IntType`.
   */
  def apply[A](implicit standardType: StandardType[A]): StandardType[A] = standardType

  /**
   * The standard type for `Unit`, tagged `"unit"`. Its default value is `()`, and its ordering treats every two values as equal.
   */
  implicit object UnitType extends StandardType[Unit] {
    override def tag: String                        = Tags.UNIT
    override def compare(x: Unit, y: Unit): Int     = 0
    override val defaultValue: Either[String, Unit] = Right(())
  }

  /**
   * The standard type for `String`, tagged `"string"`. Its default value is the empty string, and it orders strings with `String.compareTo`.
   */
  implicit object StringType extends StandardType[String] {
    override def tag: String                          = Tags.STRING
    override def compare(x: String, y: String): Int   = x.compareTo(y)
    override val defaultValue: Either[String, String] = Right("")
  }

  /**
   * The standard type for `Boolean`, tagged `"boolean"`. Its default value is `false`, and `false` orders before `true`.
   */
  implicit object BoolType extends StandardType[Boolean] {
    override def tag: String                           = Tags.BOOL
    override def compare(x: Boolean, y: Boolean): Int  = x.compareTo(y)
    override val defaultValue: Either[String, Boolean] = Right(false)
  }

  /**
   * The standard type for `Byte`, tagged `"byte"`. Its default value is `0`, and it orders values numerically.
   */
  implicit object ByteType extends StandardType[Byte] {
    override def tag: String                        = Tags.BYTE
    override def compare(x: Byte, y: Byte): Int     = x.compareTo(y)
    override val defaultValue: Either[String, Byte] = Right(0.toByte)
  }

  /**
   * The standard type for `Short`, tagged `"short"`. Its default value is `0`, and it orders values numerically.
   */
  implicit object ShortType extends StandardType[Short] {
    override def tag: String                         = Tags.SHORT
    override def compare(x: Short, y: Short): Int    = x.compareTo(y)
    override val defaultValue: Either[String, Short] = Right(0.asInstanceOf[Short])
  }

  /**
   * The standard type for `Int`, tagged `"int"`. Its default value is `0`, and it orders values numerically.
   */
  implicit object IntType extends StandardType[Int] {
    override def tag: String                       = Tags.INT
    override def compare(x: Int, y: Int): Int      = x.compareTo(y)
    override val defaultValue: Either[String, Int] = Right(0)
  }

  /**
   * The standard type for `Long`, tagged `"long"`. Its default value is `0L`, and it orders values numerically.
   */
  implicit object LongType extends StandardType[Long] {
    override def tag: String                        = Tags.LONG
    override def compare(x: Long, y: Long): Int     = x.compareTo(y)
    override val defaultValue: Either[String, Long] = Right(0L)
  }

  /**
   * The standard type for `Float`, tagged `"float"`. Its default value is `0.0f`. It orders values like `java.lang.Float.compare`: `NaN` orders after every other value, and `-0.0f` before `0.0f`.
   */
  implicit object FloatType extends StandardType[Float] {
    override def tag: String                         = Tags.FLOAT
    override def compare(x: Float, y: Float): Int    = x.compareTo(y)
    override val defaultValue: Either[String, Float] = Right(0.0f)
  }

  /**
   * The standard type for `Double`, tagged `"double"`. Its default value is `0.0`. It orders values like `java.lang.Double.compare`: `NaN` orders after every other value, and `-0.0` before `0.0`.
   */
  implicit object DoubleType extends StandardType[Double] {
    override def tag: String                          = Tags.DOUBLE
    override def compare(x: Double, y: Double): Int   = x.compareTo(y)
    override val defaultValue: Either[String, Double] = Right(0.0)
  }

  /**
   * The standard type for binary data as `Chunk[Byte]`, tagged `"binary"`. Its default value is the empty chunk. It compares two chunks by the sum of their bytes, so `Chunk(1, 2)` and `Chunk(3)` compare as equal.
   */
  implicit object BinaryType extends StandardType[Chunk[Byte]] {
    override def tag: String                                  = Tags.BINARY
    override def compare(x: Chunk[Byte], y: Chunk[Byte]): Int = x.sum.compare(y.sum)
    override val defaultValue: Either[String, Chunk[Byte]]    = Right(Chunk.empty)
  }

  /**
   * The standard type for `Char`, tagged `"char"`. Its default value is the NUL character (code point 0), and it orders characters by code unit.
   */
  implicit object CharType extends StandardType[Char] {
    override def tag: String                    = Tags.CHAR
    override def compare(x: Char, y: Char): Int = x.compareTo(y)
    // The NUL Unicode character is used as the default value for
    // `StandardType[Char]` because the empty Char '' does not compile
    override val defaultValue: Either[String, Char] = Right('\u0000')
  }

  /**
   * The standard type for `java.util.UUID`, tagged `"uuid"`, ordered with `UUID.compareTo`. On the JVM and Scala Native its default value is a new random UUID on each call. On Scala.js the default value is `Left("UUID generation not available in ScalaJS")`.
   */
  implicit object UUIDType extends StandardType[java.util.UUID] {
    override def tag: String                                        = Tags.UUID
    override def compare(x: java.util.UUID, y: java.util.UUID): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.util.UUID]       = Right(java.util.UUID.randomUUID())
  }

  /**
   * The standard type for `java.util.Currency`, tagged `"currency"`, ordered by currency code. On the JVM its default value is the currency of the default locale, or `Left` with an explanation when the locale has no currency or the lookup throws. On Scala.js and Scala Native the default value is `Left`.
   */
  implicit object CurrencyType extends StandardType[java.util.Currency] {
    override def tag: String                                      = Tags.CURRENCY
    override def defaultValue: Either[String, java.util.Currency] =
      try {
        val currency = java.util.Currency.getInstance(java.util.Locale.getDefault())
        if (currency == null) {
          Left(
            "Could not get default currency. In most cases, this is because a currency could not be determined from the provided locale (e.g. when the locale is set to Antarctica). Please inspect the default locale to ensure that it is properly set and try again."
          )
        } else {
          Right(currency)
        }

      } catch {
        case ex: Throwable =>
          Left(
            s"Could not get default currency. In most cases, this is because the default locale was not set on the JVM. If this is the case, running java.util.Locale.setDefault() before getting the default currency should fix this. Error message: ${ex.getMessage}."
          )
      }

    override def compare(x: java.util.Currency, y: java.util.Currency): Int =
      x.getCurrencyCode.compareTo(y.getCurrencyCode)
  }

  /**
   * The standard type for `java.math.BigDecimal`, tagged `"bigDecimal"`. Its default value is `BigDecimal.ZERO`. It orders values with `compareTo`, so `1.0` and `1.00` compare as equal.
   */
  implicit object BigDecimalType extends StandardType[java.math.BigDecimal] {
    override def tag: String                                                    = Tags.BIG_DECIMAL
    override def compare(x: java.math.BigDecimal, y: java.math.BigDecimal): Int = x.compareTo(y)
    override val defaultValue: Either[String, java.math.BigDecimal]             = Right(java.math.BigDecimal.ZERO)
  }

  /**
   * The standard type for `java.math.BigInteger`, tagged `"bigInteger"`. Its default value is `BigInteger.ZERO`, and it orders values numerically.
   */
  implicit object BigIntegerType extends StandardType[java.math.BigInteger] {
    override def tag: String                                        = Tags.BIG_INTEGER
    override def compare(x: BigInteger, y: BigInteger): Int         = x.compareTo(y)
    override val defaultValue: Either[String, java.math.BigInteger] = Right(java.math.BigInteger.ZERO)
  }

  // java.time specific types
  /**
   * The standard type for `java.time.DayOfWeek`, tagged `"dayOfWeek"`, ordered from `MONDAY` to `SUNDAY`. Its default value is the first day of the week for the default locale.
   */
  implicit object DayOfWeekType extends StandardType[java.time.DayOfWeek] {
    override def tag: String                              = Tags.DAY_OF_WEEK
    override def compare(x: DayOfWeek, y: DayOfWeek): Int = x.getValue.compareTo(y.getValue)
    override def defaultValue: Either[String, DayOfWeek]  =
      Right(java.time.temporal.WeekFields.of(java.util.Locale.getDefault).getFirstDayOfWeek)
  }

  /**
   * The standard type for `java.time.Month`, tagged `"month"`, ordered from `JANUARY` to `DECEMBER`. Its default value is `JANUARY`.
   */
  implicit object MonthType extends StandardType[java.time.Month] {
    override def tag: String                                   = Tags.MONTH
    override def compare(x: Month, y: Month): Int              = x.getValue.compareTo(y.getValue)
    override val defaultValue: Either[String, java.time.Month] = Right(java.time.Month.JANUARY)
  }

  /**
   * The standard type for `java.time.MonthDay`, tagged `"monthDay"`, ordered by month and then day. Its default value is January 1.
   */
  implicit object MonthDayType extends StandardType[java.time.MonthDay] {
    override def tag: String                                      = Tags.MONTH_DAY
    override def compare(x: MonthDay, y: MonthDay): Int           = x.compareTo(y)
    override val defaultValue: Either[String, java.time.MonthDay] =
      Right(java.time.MonthDay.of(java.time.Month.JANUARY, 1))
  }

  /**
   * The standard type for `java.time.Period`, tagged `"period"`. Its default value is `Period.ZERO`. It compares two periods by adding each one to the date 0000-01-01 and comparing the results, so `Period.ofDays(31)` and `Period.ofMonths(1)` compare as equal.
   */
  implicit object PeriodType extends StandardType[java.time.Period] {
    override def tag: String                        = Tags.PERIOD
    override def compare(x: Period, y: Period): Int = {
      val startDate = time.LocalDate.of(0, 1, 1)
      startDate.plus(x).compareTo(startDate.plus(y))
    }
    override val defaultValue: Either[String, java.time.Period] = Right(java.time.Period.ZERO)
  }

  /**
   * The standard type for `java.time.Year`, tagged `"year"`, ordered by year number. Its default value is the current year, read from the clock on each call.
   */
  implicit object YearType extends StandardType[java.time.Year] {
    override def tag: String                                  = Tags.YEAR
    override def compare(x: Year, y: Year): Int               = x.getValue.compareTo(y.getValue)
    override def defaultValue: Either[String, java.time.Year] = Right(java.time.Year.now)
  }

  /**
   * The standard type for `java.time.YearMonth`, tagged `"yearMonth"`, ordered chronologically. Its default value is the current year and month, read from the clock on each call.
   */
  implicit object YearMonthType extends StandardType[java.time.YearMonth] {
    override def tag: String                                       = Tags.YEAR_MONTH
    override def compare(x: YearMonth, y: YearMonth): Int          = x.compareTo(y)
    override def defaultValue: Either[String, java.time.YearMonth] = Right(java.time.YearMonth.now)
  }

  /**
   * The standard type for `java.time.ZoneId`, tagged `"zoneId"`, ordered by the zone id string (for example `"America/New_York"` before `"Europe/Paris"`). Its default value is `ZoneId.systemDefault`, read on each call.
   */
  implicit object ZoneIdType extends StandardType[java.time.ZoneId] {
    override def tag: String                                    = Tags.ZONE_ID
    override def compare(x: ZoneId, y: ZoneId): Int             = x.getId.compareTo(y.getId) // TODO is there a better comparison
    override def defaultValue: Either[String, java.time.ZoneId] = Right(java.time.ZoneId.systemDefault)
  }

  /**
   * The standard type for `java.time.ZoneOffset`, tagged `"zoneOffset"`. Its default value is `ZoneOffset.UTC`. It orders values with `ZoneOffset.compareTo`, which puts `+10:00` before `+09:00`.
   */
  implicit object ZoneOffsetType extends StandardType[java.time.ZoneOffset] {
    override def tag: String                                        = Tags.ZONE_OFFSET
    override def compare(x: ZoneOffset, y: ZoneOffset): Int         = x.compareTo(y)
    override val defaultValue: Either[String, java.time.ZoneOffset] = Right(java.time.ZoneOffset.UTC)
  }

  /**
   * The standard type for `java.time.Duration`, tagged `"duration"`, ordered by length. Its default value is `Duration.ZERO`.
   */
  implicit object DurationType extends StandardType[java.time.Duration] {
    override def tag: String                                      = Tags.DURATION
    override def compare(x: time.Duration, y: time.Duration): Int = x.compareTo(y)
    override val defaultValue: Either[String, java.time.Duration] = Right(java.time.Duration.ZERO)
  }

  /**
   * The standard type for `java.time.Instant`, tagged `"instant"`, ordered on the time-line. Its default value is `Instant.EPOCH`.
   */
  implicit object InstantType extends StandardType[java.time.Instant] {
    override def tag: String = Tags.INSTANT

    override val defaultValue: Either[String, Instant] = Right(java.time.Instant.EPOCH)

    override def compare(x: Instant, y: Instant): Int = x.compareTo(y)
  }

  /**
   * The standard type for `java.time.LocalDate`, tagged `"localDate"`, ordered chronologically. Its default value is `LocalDate.now`, read from the clock on each call.
   */
  implicit object LocalDateType extends StandardType[java.time.LocalDate] {
    override def tag: String = Tags.LOCAL_DATE

    override def defaultValue: Either[String, LocalDate] = Right(java.time.LocalDate.now)

    override def compare(x: LocalDate, y: LocalDate): Int = x.compareTo(y)
  }

  /**
   * The standard type for `java.time.LocalTime`, tagged `"localTime"`, ordered chronologically. Its default value is `LocalTime.MIDNIGHT`.
   */
  implicit object LocalTimeType extends StandardType[java.time.LocalTime] {
    override def tag: String = Tags.LOCAL_TIME

    override val defaultValue: Either[String, LocalTime] = Right(java.time.LocalTime.MIDNIGHT)

    override def compare(x: LocalTime, y: LocalTime): Int = x.compareTo(y)
  }

  /**
   * The standard type for `java.time.LocalDateTime`, tagged `"localDateTime"`, ordered chronologically. Its default value is `LocalDateTime.now`, read from the clock on each call.
   */
  implicit object LocalDateTimeType extends StandardType[java.time.LocalDateTime] {
    override def tag: String = Tags.LOCAL_DATE_TIME

    override def defaultValue: Either[String, LocalDateTime] = Right(java.time.LocalDateTime.now)

    override def compare(x: LocalDateTime, y: LocalDateTime): Int = x.compareTo(y)
  }

  /**
   * The standard type for `java.time.OffsetTime`, tagged `"offsetTime"`, ordered with `OffsetTime.compareTo`. Its default value is `OffsetTime.now`, read from the clock on each call.
   */
  implicit object OffsetTimeType extends StandardType[java.time.OffsetTime] {
    override def tag: String = Tags.OFFSET_TIME

    override def defaultValue: Either[String, OffsetTime] = Right(java.time.OffsetTime.now)

    override def compare(x: OffsetTime, y: OffsetTime): Int = x.compareTo(y)
  }

  /**
   * The standard type for `java.time.OffsetDateTime`, tagged `"offsetDateTime"`, ordered with `OffsetDateTime.compareTo`. Its default value is `OffsetDateTime.now`, read from the clock on each call.
   */
  implicit object OffsetDateTimeType extends StandardType[java.time.OffsetDateTime] {
    override def tag: String = Tags.OFFSET_DATE_TIME

    override def defaultValue: Either[String, OffsetDateTime] = Right(java.time.OffsetDateTime.now)

    override def compare(x: OffsetDateTime, y: OffsetDateTime): Int = x.compareTo(y)
  }

  /**
   * The standard type for `java.time.ZonedDateTime`, tagged `"zonedDateTime"`, ordered with `ZonedDateTime.compareTo`. Its default value is `ZonedDateTime.now`, read from the clock on each call.
   */
  implicit object ZonedDateTimeType extends StandardType[java.time.ZonedDateTime] {
    override def tag: String = Tags.ZONED_DATE_TIME

    override def defaultValue: Either[String, ZonedDateTime] = Right(java.time.ZonedDateTime.now)

    override def compare(x: ZonedDateTime, y: ZonedDateTime): Int = x.compareTo(y)
  }
}
