package zio.schema

import java.math.BigInteger
import java.time
import java.time._
import zio.Chunk
import zio.prelude.Validation

sealed trait StandardType[A] extends Ordering[A] { self =>
  def tag: String
  def defaultValue: Either[String, A]
  override def toString: String = tag

  /**
   * Converts a DynamicValue into a primitive type.
   */
  def toTypedPrimitive(value: DynamicValue): Validation[String, A] =
    value.toTypedValue(Schema.primitive[A](self))
}

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
    final val LOCAL_DATE_TIME  = "localDateTIme"
    final val OFFSET_TIME      = "offsetTime"
    final val OFFSET_DATE_TIME = "offsetDateTime"
    final val ZONED_DATE_TIME  = "zonedDateTime"
    final val UUID             = "uuid"
  }

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
    }

  def apply[A](implicit standardType: StandardType[A]): StandardType[A] = standardType

  implicit object UnitType extends StandardType[Unit] {
    override def tag: String                        = Tags.UNIT
    override def compare(x: Unit, y: Unit): Int     = 0
    override def defaultValue: Either[String, Unit] = Right(())
  }

  implicit object StringType extends StandardType[String] {
    override def tag: String                          = Tags.STRING
    override def compare(x: String, y: String): Int   = x.compareTo(y)
    override def defaultValue: Either[String, String] = Right("")
  }

  implicit object BoolType extends StandardType[Boolean] {
    override def tag: String                           = Tags.BOOL
    override def compare(x: Boolean, y: Boolean): Int  = x.compareTo(y)
    override def defaultValue: Either[String, Boolean] = Right(false)
  }

  implicit object ByteType extends StandardType[Byte] {
    override def tag: String                        = Tags.BYTE
    override def compare(x: Byte, y: Byte): Int     = x.compareTo(y)
    override def defaultValue: Either[String, Byte] = Right(0.toByte)
  }

  implicit object ShortType extends StandardType[Short] {
    override def tag: String                         = Tags.SHORT
    override def compare(x: Short, y: Short): Int    = x.compareTo(y)
    override def defaultValue: Either[String, Short] = Right(0.asInstanceOf[Short])
  }

  implicit object IntType extends StandardType[Int] {
    override def tag: String                       = Tags.INT
    override def compare(x: Int, y: Int): Int      = x.compareTo(y)
    override def defaultValue: Either[String, Int] = Right(0)
  }

  implicit object LongType extends StandardType[Long] {
    override def tag: String                        = Tags.LONG
    override def compare(x: Long, y: Long): Int     = x.compareTo(y)
    override def defaultValue: Either[String, Long] = Right(0.asInstanceOf[Long])
  }

  implicit object FloatType extends StandardType[Float] {
    override def tag: String                         = Tags.FLOAT
    override def compare(x: Float, y: Float): Int    = x.compareTo(y)
    override def defaultValue: Either[String, Float] = Right(0.0.asInstanceOf[Float])
  }

  implicit object DoubleType extends StandardType[Double] {
    override def tag: String                          = Tags.DOUBLE
    override def compare(x: Double, y: Double): Int   = x.compareTo(y)
    override def defaultValue: Either[String, Double] = Right(0.0)
  }

  implicit object BinaryType extends StandardType[Chunk[Byte]] {
    override def tag: String                                  = Tags.BINARY
    override def compare(x: Chunk[Byte], y: Chunk[Byte]): Int = x.sum.compare(y.sum)
    override def defaultValue: Either[String, Chunk[Byte]]    = Right(Chunk.empty)
  }

  implicit object CharType extends StandardType[Char] {
    override def tag: String                    = Tags.CHAR
    override def compare(x: Char, y: Char): Int = x.compareTo(y)
    // The NUL Unicode character is used as the default value for
    // `StandardType[Char]` because the empty Char '' does not compile
    override def defaultValue: Either[String, Char] = Right('\u0000')
  }

  implicit object UUIDType extends StandardType[java.util.UUID] {
    override def tag: String                                        = Tags.UUID
    override def compare(x: java.util.UUID, y: java.util.UUID): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.util.UUID]       = Right(java.util.UUID.randomUUID())
  }

  implicit object BigDecimalType extends StandardType[java.math.BigDecimal] {
    override def tag: String                                                    = Tags.BIG_DECIMAL
    override def compare(x: java.math.BigDecimal, y: java.math.BigDecimal): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.math.BigDecimal]             = Right(java.math.BigDecimal.ZERO)
  }

  implicit object BigIntegerType extends StandardType[java.math.BigInteger] {
    override def tag: String                                        = Tags.BIG_INTEGER
    override def compare(x: BigInteger, y: BigInteger): Int         = x.compareTo(y)
    override def defaultValue: Either[String, java.math.BigInteger] = Right(java.math.BigInteger.ZERO)
  }

  //java.time specific types
  implicit object DayOfWeekType extends StandardType[DayOfWeek] {
    override def tag: String                              = Tags.DAY_OF_WEEK
    override def compare(x: DayOfWeek, y: DayOfWeek): Int = x.getValue.compareTo(y.getValue)
    override def defaultValue: Either[String, DayOfWeek] =
      Right(java.time.temporal.WeekFields.of(java.util.Locale.getDefault).getFirstDayOfWeek)
  }

  implicit object MonthType extends StandardType[java.time.Month] {
    override def tag: String                                   = Tags.MONTH
    override def compare(x: Month, y: Month): Int              = x.getValue.compareTo(y.getValue)
    override def defaultValue: Either[String, java.time.Month] = Right(java.time.Month.JANUARY)
  }

  implicit object MonthDayType extends StandardType[java.time.MonthDay] {
    override def tag: String                            = Tags.MONTH_DAY
    override def compare(x: MonthDay, y: MonthDay): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.MonthDay] =
      Right(java.time.MonthDay.of(java.time.Month.JANUARY, 1))
  }

  implicit object PeriodType extends StandardType[java.time.Period] {
    override def tag: String = Tags.PERIOD
    override def compare(x: Period, y: Period): Int = {
      val startDate = time.LocalDate.of(0, 1, 1)
      startDate.plus(x).compareTo(startDate.plus(y))
    }
    override def defaultValue: Either[String, java.time.Period] = Right(java.time.Period.ZERO)
  }

  implicit object YearType extends StandardType[java.time.Year] {
    override def tag: String                                  = Tags.YEAR
    override def compare(x: Year, y: Year): Int               = x.getValue.compareTo(y.getValue)
    override def defaultValue: Either[String, java.time.Year] = Right(java.time.Year.now)
  }

  implicit object YearMonthType extends StandardType[java.time.YearMonth] {
    override def tag: String                                       = Tags.YEAR_MONTH
    override def compare(x: YearMonth, y: YearMonth): Int          = x.compareTo(y)
    override def defaultValue: Either[String, java.time.YearMonth] = Right(java.time.YearMonth.now)
  }

  implicit object ZoneIdType extends StandardType[java.time.ZoneId] {
    override def tag: String                                    = Tags.ZONE_ID
    override def compare(x: ZoneId, y: ZoneId): Int             = x.getId.compareTo(y.getId) // TODO is there a better comparison
    override def defaultValue: Either[String, java.time.ZoneId] = Right(java.time.ZoneId.systemDefault)
  }

  implicit object ZoneOffsetType extends StandardType[java.time.ZoneOffset] {
    override def tag: String                                        = Tags.ZONE_OFFSET
    override def compare(x: ZoneOffset, y: ZoneOffset): Int         = x.compareTo(y)
    override def defaultValue: Either[String, java.time.ZoneOffset] = Right(java.time.ZoneOffset.UTC)
  }

  implicit object DurationType extends StandardType[java.time.Duration] {
    override def tag: String                                      = Tags.DURATION
    override def compare(x: time.Duration, y: time.Duration): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.Duration] = Right(java.time.Duration.ZERO)
  }

  implicit object InstantType extends StandardType[java.time.Instant] {
    override def tag: String = Tags.INSTANT

    override def defaultValue: Either[String, Instant] = Right(java.time.Instant.EPOCH)

    override def compare(x: Instant, y: Instant): Int = x.compareTo(y)
  }

  implicit object LocalDateType extends StandardType[java.time.LocalDate] {
    override def tag: String = Tags.LOCAL_DATE

    override def defaultValue: Either[String, LocalDate] = Right(java.time.LocalDate.now)

    override def compare(x: LocalDate, y: LocalDate): Int = x.compareTo(y)
  }

  implicit object LocalTimeType extends StandardType[java.time.LocalTime] {
    override def tag: String = Tags.LOCAL_TIME

    override def defaultValue: Either[String, LocalTime] = Right(java.time.LocalTime.MIDNIGHT)

    override def compare(x: LocalTime, y: LocalTime): Int = x.compareTo(y)
  }

  implicit object LocalDateTimeType extends StandardType[java.time.LocalDateTime] {
    override def tag: String = Tags.LOCAL_DATE_TIME

    override def defaultValue: Either[String, LocalDateTime] = Right(java.time.LocalDateTime.now)

    override def compare(x: LocalDateTime, y: LocalDateTime): Int = x.compareTo(y)
  }

  implicit object OffsetTimeType extends StandardType[java.time.OffsetTime] {
    override def tag: String = Tags.OFFSET_TIME

    override def defaultValue: Either[String, OffsetTime] = Right(java.time.OffsetTime.now)

    override def compare(x: OffsetTime, y: OffsetTime): Int = x.compareTo(y)
  }

  implicit object OffsetDateTimeType extends StandardType[java.time.OffsetDateTime] {
    override def tag: String = Tags.OFFSET_DATE_TIME

    override def defaultValue: Either[String, OffsetDateTime] = Right(java.time.OffsetDateTime.now)

    override def compare(x: OffsetDateTime, y: OffsetDateTime): Int = x.compareTo(y)
  }

  implicit object ZonedDateTimeType extends StandardType[java.time.ZonedDateTime] {
    override def tag: String = Tags.ZONED_DATE_TIME

    override def defaultValue: Either[String, ZonedDateTime] = Right(java.time.ZonedDateTime.now)

    override def compare(x: ZonedDateTime, y: ZonedDateTime): Int = x.compareTo(y)
  }
}
