package zio.schema

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import java.time
import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.{ ChronoUnit, TemporalUnit }

import zio.Chunk

sealed trait StandardType[A] extends Ordering[A] { self =>

  def tag: String

  def coerce[B](that: StandardType[B]): Option[A => Either[String, B]] =
    that match {
      case `self`                  => Some(a => Right(a.asInstanceOf[B]))
      case StandardType.StringType => Some(a => Right(a.toString))
      case _                       => None
    }

  def defaultValue: Either[String, A]

  override def toString: String = tag

}

object StandardType {

  private[schema] object Tags {
    final val UNIT             = "unit"
    final val STRING           = "string"
    final val BOOL             = "boolean"
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
      case Tags.INSTANT          => Some(InstantType(DateTimeFormatter.ISO_INSTANT))
      case Tags.LOCAL_DATE       => Some(LocalDateType(DateTimeFormatter.ISO_LOCAL_DATE))
      case Tags.LOCAL_TIME       => Some(LocalTimeType(DateTimeFormatter.ISO_LOCAL_TIME))
      case Tags.LOCAL_DATE_TIME  => Some(LocalDateTimeType(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
      case Tags.OFFSET_TIME      => Some(OffsetTimeType(DateTimeFormatter.ISO_OFFSET_TIME))
      case Tags.OFFSET_DATE_TIME => Some(OffsetDateTimeType(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
      case Tags.ZONED_DATE_TIME  => Some(ZonedDateTimeType(DateTimeFormatter.ISO_ZONED_DATE_TIME))
      case Tags.UUID             => Some(UUIDType)
      case units =>
        try {
          Some(Duration(ChronoUnit.valueOf(units)))
        } catch { case _: Throwable => None }
    }

  def apply[A](implicit standatdType: StandardType[A]): StandardType[A] = standatdType

  def fromTemporalUnits(units: String): Option[StandardType[java.time.Duration]] =
    ChronoUnit.values().find(_.toString == units).map(Duration(_))

  implicit object UnitType extends StandardType[Unit] {
    override def tag                                = Tags.UNIT
    override def compare(x: Unit, y: Unit): Int     = 0
    override def defaultValue: Either[String, Unit] = Right(())
  }

  implicit object StringType extends StandardType[String] {
    override def tag                                  = Tags.STRING
    override def compare(x: String, y: String): Int   = x.compareTo(y)
    override def defaultValue: Either[String, String] = Right("")

    override def coerce[B](that: StandardType[B]): Option[String => Either[String, B]] =
      that match {
        case UnitType   => None
        case StringType => Some(a => Right(a))
        case BoolType   => Some(a => a.toBooleanOption.toRight(s"""String "$a" could not be coerced into a Boolean"""))
        case ShortType  => Some(a => a.toShortOption.toRight(s"""String "$a" could not be coerced into a Short"""))
        case IntType    => Some(a => a.toIntOption.toRight(s"""String "$a" could not be coerced into an Int"""))
        case LongType   => Some(a => a.toLongOption.toRight(s"""String "$a" could not be coerced into a Long"""))
        case FloatType  => Some(a => a.toFloatOption.toRight(s"""String "$a" could not be coerced into a Float"""))
        case DoubleType => Some(a => a.toDoubleOption.toRight(s"""String "$a" could not be coerced into a Double"""))
        case BinaryType => Some(a => Right(Chunk.fromArray(a.getBytes)))
        case CharType   => None
        case UUIDType =>
          Some(
            a =>
              EitherUtils
                .attemptOrElse(java.util.UUID.fromString(a), s"""String "$a" could not be coerced into a UUID""")
          )
        case BigDecimalType =>
          Some(
            a =>
              EitherUtils
                .attemptOrElse(BigDecimal(a).bigDecimal, s"""String "$a" could not be coerced into a BigDecimal""")
          )
        case BigIntegerType =>
          Some(
            a =>
              EitherUtils.attemptOrElse(BigInt(a).bigInteger, s"""String "$a" could not be coerced into a BigInteger""")
          )
        case DayOfWeekType =>
          Some(
            a =>
              EitherUtils.attemptOrElse(DayOfWeek.valueOf(a), s"""String "$a" could not be coerced into a DayOfWeek""")
          )
        case MonthType =>
          Some(
            a => EitherUtils.attemptOrElse(Month.valueOf(a), s"""String "$a" could not be coerced into a Month""")
          )
        case MonthDayType =>
          Some(
            a => EitherUtils.attemptOrElse(MonthDay.parse(a), s"""String "$a" could not be coerced into a MonthDay""")
          )
        case PeriodType =>
          Some(a => EitherUtils.attemptOrElse(Period.parse(a), s"""String "$a" could not be coerced into a Period"""))
        case YearType =>
          Some(a => EitherUtils.attemptOrElse(Year.parse(a), s"""String "$a" could not be coerced into a Year"""))
        case YearMonthType =>
          Some(
            a => EitherUtils.attemptOrElse(YearMonth.parse(a), s"""String "$a" could not be coerced into a YearMonth""")
          )
        case ZoneIdType =>
          Some(a => EitherUtils.attemptOrElse(ZoneId.of(a), s"""String "$a" could not be coerced into a ZoneId"""))
        case ZoneOffsetType =>
          Some(
            a => EitherUtils.attemptOrElse(ZoneOffset.of(a), s"""String "$a" could not be coerced into a ZoneOffset""")
          )
        case Duration(_) =>
          Some(
            a =>
              EitherUtils
                .attemptOrElse(java.time.Duration.parse(a), s"""String "$a" could not be coerced into a Duration""")
          )
        case InstantType(formatter) =>
          Some(
            a =>
              EitherUtils
                .attemptOrElse(Instant.from(formatter.parse(a)), s"""String "$a" could not be coerced into a Instant""")
          )
        case LocalDateType(formatter) =>
          Some(
            a =>
              EitherUtils
                .attemptOrElse(
                  LocalDate.from(formatter.parse(a)),
                  s"""String "$a" could not be coerced into a LocalDate"""
                )
          )
        case LocalTimeType(formatter) =>
          Some(
            a =>
              EitherUtils
                .attemptOrElse(
                  LocalTime.from(formatter.parse(a)),
                  s"""String "$a" could not be coerced into a LocalTime"""
                )
          )
        case LocalDateTimeType(formatter) =>
          Some(
            a =>
              EitherUtils.attemptOrElse(
                LocalDateTime.from(formatter.parse(a)),
                s"""String "$a" could not be coerced into a LocalDateTime"""
              )
          )
        case OffsetTimeType(formatter) =>
          Some(
            a =>
              EitherUtils
                .attemptOrElse(
                  OffsetTime.from(formatter.parse(a)),
                  s"""String "$a" could not be coerced into a OffsetTime"""
                )
          )
        case OffsetDateTimeType(formatter) =>
          Some(
            a =>
              EitherUtils.attemptOrElse(
                OffsetDateTime.from(formatter.parse(a)),
                s"""String "$a" could not be coerced into a OffsetDateTime"""
              )
          )
        case ZonedDateTimeType(formatter) =>
          Some(
            a =>
              EitherUtils.attemptOrElse(
                ZonedDateTime.from(formatter.parse(a)),
                s"""String "$a" could not be coerced into a ZonedDateTime"""
              )
          )
      }
  }

  implicit object BoolType extends StandardType[Boolean] {
    override def tag                                   = Tags.BOOL
    override def compare(x: Boolean, y: Boolean): Int  = x.compareTo(y)
    override def defaultValue: Either[String, Boolean] = Right(false)
  }

  implicit object ShortType extends StandardType[Short] {
    override def tag                                 = Tags.SHORT
    override def compare(x: Short, y: Short): Int    = x.compareTo(y)
    override def defaultValue: Either[String, Short] = Right(0.asInstanceOf[Short])

    override def coerce[B](that: StandardType[B]): Option[Short => Either[String, B]] =
      that match {
        case StringType     => Some(a => Right(a.toString))
        case ShortType      => Some(a => Right(a))
        case IntType        => Some(a => Right(a.toInt))
        case LongType       => Some(a => Right(a.toLong))
        case FloatType      => Some(a => Right(a.toFloat))
        case DoubleType     => Some(a => Right(a.toDouble))
        case CharType       => Some(a => Right(a.toChar))
        case BigDecimalType => Some(a => Right(java.math.BigDecimal.valueOf(a.toLong)))
        case BigIntegerType => Some(a => Right(BigInteger.valueOf(a.toLong)))
        case _              => None
      }
  }

  implicit object IntType extends StandardType[Int] {
    override def tag                               = Tags.INT
    override def compare(x: Int, y: Int): Int      = x.compareTo(y)
    override def defaultValue: Either[String, Int] = Right(0)

    override def coerce[B](that: StandardType[B]): Option[Int => Either[String, B]] =
      that match {
        case StringType     => Some(a => Right(a.toString))
        case ShortType      => Some(a => Right(a.toShort))
        case IntType        => Some(a => Right(a))
        case LongType       => Some(a => Right(a.toLong))
        case FloatType      => Some(a => Right(a.toFloat))
        case DoubleType     => Some(a => Right(a.toDouble))
        case CharType       => Some(a => Right(a.toChar))
        case BigDecimalType => Some(a => Right(java.math.BigDecimal.valueOf(a.toLong)))
        case BigIntegerType => Some(a => Right(BigInteger.valueOf(a.toLong)))
        case _              => None
      }
  }

  implicit object LongType extends StandardType[Long] {
    override def tag                                = Tags.LONG
    override def compare(x: Long, y: Long): Int     = x.compareTo(y)
    override def defaultValue: Either[String, Long] = Right(0.asInstanceOf[Long])

    override def coerce[B](that: StandardType[B]): Option[Long => Either[String, B]] =
      that match {
        case StringType     => Some(a => Right(a.toString))
        case ShortType      => Some(a => Right(a.toShort))
        case IntType        => Some(a => Right(a.toInt))
        case LongType       => Some(a => Right(a))
        case FloatType      => Some(a => Right(a.toFloat))
        case DoubleType     => Some(a => Right(a.toDouble))
        case CharType       => Some(a => Right(a.toChar))
        case BigDecimalType => Some(a => Right(java.math.BigDecimal.valueOf(a)))
        case BigIntegerType => Some(a => Right(BigInteger.valueOf(a)))
        case _              => None
      }
  }

  implicit object FloatType extends StandardType[Float] {
    override def tag                                 = Tags.FLOAT
    override def compare(x: Float, y: Float): Int    = x.compareTo(y)
    override def defaultValue: Either[String, Float] = Right(0.0.asInstanceOf[Float])

    override def coerce[B](that: StandardType[B]): Option[Float => Either[String, B]] =
      that match {
        case StringType     => Some(a => Right(a.toString))
        case ShortType      => Some(a => Right(a.toShort))
        case IntType        => Some(a => Right(a.toInt))
        case LongType       => Some(a => Right(a.toLong))
        case FloatType      => Some(a => Right(a))
        case DoubleType     => Some(a => Right(a.toDouble))
        case CharType       => Some(a => Right(a.toChar))
        case BigDecimalType => Some(a => Right(java.math.BigDecimal.valueOf(a)))
        case BigIntegerType => Some(a => Right(BigInteger.valueOf(a.toLong)))
        case _              => None
      }

  }

  implicit object DoubleType extends StandardType[Double] {
    override def tag                                  = Tags.DOUBLE
    override def compare(x: Double, y: Double): Int   = x.compareTo(y)
    override def defaultValue: Either[String, Double] = Right(0.0)

    override def coerce[B](that: StandardType[B]): Option[Double => Either[String, B]] =
      that match {
        case StringType     => Some(a => Right(a.toString))
        case ShortType      => Some(a => Right(a.toShort))
        case IntType        => Some(a => Right(a.toInt))
        case LongType       => Some(a => Right(a.toLong))
        case FloatType      => Some(a => Right(a.toFloat))
        case DoubleType     => Some(a => Right(a))
        case CharType       => Some(a => Right(a.toChar))
        case BigDecimalType => Some(a => Right(java.math.BigDecimal.valueOf(a)))
        case BigIntegerType => Some(a => Right(BigInteger.valueOf(a.toLong)))
        case _              => None
      }

  }

  implicit object BinaryType extends StandardType[Chunk[Byte]] {
    override def tag                                          = Tags.BINARY
    override def compare(x: Chunk[Byte], y: Chunk[Byte]): Int = x.sum.compare(y.sum)
    override def defaultValue: Either[String, Chunk[Byte]]    = Right(Chunk.empty)

    override def coerce[B](that: StandardType[B]): Option[Chunk[Byte] => Either[String, B]] =
      that match {
        case StringType => Some(a => Right(new String(a.toArray, StandardCharsets.UTF_8)))
        case _          => None
      }
  }

  implicit object CharType extends StandardType[Char] {
    override def tag                            = Tags.CHAR
    override def compare(x: Char, y: Char): Int = x.compareTo(y)
    // The NUL Unicode character is used as the default value for
    // `StandardType[Char]` because the empty Char '' does not compile
    override def defaultValue: Either[String, Char] = Right('\u0000')

    override def coerce[B](that: StandardType[B]): Option[Char => Either[String, B]] =
      that match {
        case StringType     => Some(a => Right(a.toString))
        case ShortType      => Some(a => Right(a.toShort))
        case IntType        => Some(a => Right(a.toInt))
        case LongType       => Some(a => Right(a.toLong))
        case FloatType      => Some(a => Right(a.toFloat))
        case DoubleType     => Some(a => Right(a.toDouble))
        case CharType       => Some(a => Right(a))
        case BigDecimalType => Some(a => Right(java.math.BigDecimal.valueOf(a)))
        case BigIntegerType => Some(a => Right(BigInteger.valueOf(a.toLong)))
        case _              => None
      }

  }

  implicit object UUIDType extends StandardType[java.util.UUID] {
    override def tag                                                = Tags.UUID
    override def compare(x: java.util.UUID, y: java.util.UUID): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.util.UUID]       = Right(java.util.UUID.randomUUID())

    override def coerce[B](that: StandardType[B]): Option[java.util.UUID => Either[String, B]] =
      that match {
        case StringType => Some(a => Right(a.toString))
        case _          => None
      }
  }

  implicit object BigDecimalType extends StandardType[java.math.BigDecimal] {
    override def tag                                                            = Tags.BIG_DECIMAL
    override def compare(x: java.math.BigDecimal, y: java.math.BigDecimal): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.math.BigDecimal]             = Right(java.math.BigDecimal.ZERO)

    override def coerce[B](that: StandardType[B]): Option[java.math.BigDecimal => Either[String, B]] =
      that match {
        case StringType     => Some(a => Right(a.toString))
        case ShortType      => Some(a => Right(a.shortValue()))
        case IntType        => Some(a => Right(a.intValue()))
        case LongType       => Some(a => Right(a.longValue()))
        case FloatType      => Some(a => Right(a.floatValue()))
        case DoubleType     => Some(a => Right(a.doubleValue()))
        case CharType       => Some(a => Right(a.intValue().toChar))
        case BigDecimalType => Some(a => Right(a))
        case BigIntegerType => Some(a => Right(BigInteger.valueOf(a.longValue())))
        case _              => None
      }

  }

  implicit object BigIntegerType extends StandardType[java.math.BigInteger] {
    override def tag                                                = Tags.BIG_INTEGER
    override def compare(x: BigInteger, y: BigInteger): Int         = x.compareTo(y)
    override def defaultValue: Either[String, java.math.BigInteger] = Right(java.math.BigInteger.ZERO)

    override def coerce[B](that: StandardType[B]): Option[BigInteger => Either[String, B]] =
      that match {
        case StringType     => Some(a => Right(a.toString))
        case ShortType      => Some(a => Right(a.shortValue()))
        case IntType        => Some(a => Right(a.intValue()))
        case LongType       => Some(a => Right(a.longValue()))
        case FloatType      => Some(a => Right(a.floatValue()))
        case DoubleType     => Some(a => Right(a.doubleValue()))
        case CharType       => Some(a => Right(a.intValue().toChar))
        case BigDecimalType => Some(a => Right(java.math.BigDecimal.valueOf(a.longValue())))
        case BigIntegerType => Some(a => Right(a))
        case _              => None
      }
  }

  //java.time specific types
  implicit object DayOfWeekType extends StandardType[DayOfWeek] {
    override def tag                                      = Tags.DAY_OF_WEEK
    override def compare(x: DayOfWeek, y: DayOfWeek): Int = x.getValue.compareTo(y.getValue)
    override def defaultValue: Either[String, DayOfWeek] =
      Right(java.time.temporal.WeekFields.of(java.util.Locale.getDefault).getFirstDayOfWeek)
  }

  implicit object MonthType extends StandardType[java.time.Month] {
    override def tag                                           = Tags.MONTH
    override def compare(x: Month, y: Month): Int              = x.getValue.compareTo(y.getValue)
    override def defaultValue: Either[String, java.time.Month] = Right(java.time.Month.JANUARY)
  }

  implicit object MonthDayType extends StandardType[java.time.MonthDay] {
    override def tag                                    = Tags.MONTH_DAY
    override def compare(x: MonthDay, y: MonthDay): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.MonthDay] =
      Right(java.time.MonthDay.of(java.time.Month.JANUARY, 1))
  }

  implicit object PeriodType extends StandardType[java.time.Period] {
    override def tag = Tags.PERIOD
    override def compare(x: Period, y: Period): Int = {
      val startDate = time.LocalDate.of(0, 1, 1)
      startDate.plus(x).compareTo(startDate.plus(y))
    }
    override def defaultValue: Either[String, java.time.Period] = Right(java.time.Period.ZERO)
  }

  implicit object YearType extends StandardType[java.time.Year] {
    override def tag                                          = Tags.YEAR
    override def compare(x: Year, y: Year): Int               = x.getValue.compareTo(y.getValue)
    override def defaultValue: Either[String, java.time.Year] = Right(java.time.Year.now)
  }

  implicit object YearMonthType extends StandardType[java.time.YearMonth] {
    override def tag                                               = Tags.YEAR_MONTH
    override def compare(x: YearMonth, y: YearMonth): Int          = x.compareTo(y)
    override def defaultValue: Either[String, java.time.YearMonth] = Right(java.time.YearMonth.now)
  }

  implicit object ZoneIdType extends StandardType[java.time.ZoneId] {
    override def tag                                            = Tags.ZONE_ID
    override def compare(x: ZoneId, y: ZoneId): Int             = x.getId.compareTo(y.getId) // TODO is there a better comparison
    override def defaultValue: Either[String, java.time.ZoneId] = Right(java.time.ZoneId.systemDefault)
  }

  implicit object ZoneOffsetType extends StandardType[java.time.ZoneOffset] {
    override def tag                                                = Tags.ZONE_OFFSET
    override def compare(x: ZoneOffset, y: ZoneOffset): Int         = x.compareTo(y)
    override def defaultValue: Either[String, java.time.ZoneOffset] = Right(java.time.ZoneOffset.UTC)
  }

  final case class Duration(temporalUnit: TemporalUnit) extends StandardType[java.time.Duration] {
    override def tag: String                                      = temporalUnit.toString().toUpperCase()
    override def compare(x: time.Duration, y: time.Duration): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.Duration] = Right(java.time.Duration.ZERO)
  }

  final case class InstantType(formatter: DateTimeFormatter) extends StandardType[java.time.Instant] {
    override def tag                                             = Tags.INSTANT
    override def compare(x: time.Instant, y: time.Instant): Int  = x.compareTo(y)
    override def defaultValue: Either[String, java.time.Instant] = Right(java.time.Instant.EPOCH)

    override def coerce[B](that: StandardType[B]): Option[Instant => Either[String, B]] =
      that match {
        case that if this == that => Some(x => Right(x.asInstanceOf[B]))
        case StringType           => Some(x => Right(formatter.format(x)))
        case LongType             => Some(x => Right(x.toEpochMilli))
        case _                    => None
      }

  }

  final case class LocalDateType(formatter: DateTimeFormatter) extends StandardType[java.time.LocalDate] {
    override def tag                                                = Tags.LOCAL_DATE
    override def compare(x: time.LocalDate, y: time.LocalDate): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.LocalDate]  = Right(java.time.LocalDate.now)

    override def coerce[B](that: StandardType[B]): Option[LocalDate => Either[String, B]] =
      that match {
        case StringType => Some(a => Right(a.format(formatter)))
        case _          => None
      }
  }

  final case class LocalTimeType(formatter: DateTimeFormatter) extends StandardType[java.time.LocalTime] {
    override def tag                                                = Tags.LOCAL_TIME
    override def compare(x: time.LocalTime, y: time.LocalTime): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.LocalTime]  = Right(java.time.LocalTime.MIDNIGHT)

    override def coerce[B](that: StandardType[B]): Option[LocalTime => Either[String, B]] =
      that match {
        case StringType => Some(a => Right(a.format(formatter)))
        case _          => None
      }
  }

  final case class LocalDateTimeType(formatter: DateTimeFormatter) extends StandardType[java.time.LocalDateTime] {
    override def tag                                                        = Tags.LOCAL_DATE_TIME
    override def compare(x: time.LocalDateTime, y: time.LocalDateTime): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.LocalDateTime]      = Right(java.time.LocalDateTime.now)

    override def coerce[B](that: StandardType[B]): Option[LocalDateTime => Either[String, B]] =
      that match {
        case StringType => Some(a => Right(a.format(formatter)))
        case _          => None
      }
  }

  final case class OffsetTimeType(formatter: DateTimeFormatter) extends StandardType[java.time.OffsetTime] {
    override def tag                                                  = Tags.OFFSET_TIME
    override def compare(x: time.OffsetTime, y: time.OffsetTime): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.OffsetTime]   = Right(java.time.OffsetTime.now)

    override def coerce[B](that: StandardType[B]): Option[OffsetTime => Either[String, B]] =
      that match {
        case StringType => Some(a => Right(a.format(formatter)))
        case _          => None
      }
  }

  final case class OffsetDateTimeType(formatter: DateTimeFormatter) extends StandardType[java.time.OffsetDateTime] {
    override def tag                                                          = Tags.OFFSET_DATE_TIME
    override def compare(x: time.OffsetDateTime, y: time.OffsetDateTime): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.OffsetDateTime]       = Right(java.time.OffsetDateTime.now)

    override def coerce[B](that: StandardType[B]): Option[OffsetDateTime => Either[String, B]] =
      that match {
        case StringType => Some(a => Right(a.format(formatter)))
        case _          => None
      }
  }

  final case class ZonedDateTimeType(formatter: DateTimeFormatter) extends StandardType[java.time.ZonedDateTime] {
    override def tag                                                        = Tags.ZONED_DATE_TIME
    override def compare(x: time.ZonedDateTime, y: time.ZonedDateTime): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.ZonedDateTime]      = Right(java.time.ZonedDateTime.now)

    override def coerce[B](that: StandardType[B]): Option[ZonedDateTime => Either[String, B]] =
      that match {
        case StringType => Some(a => Right(a.format(formatter)))
        case _          => None
      }
  }
}

trait DefaultJavaTimeSchemas {
  implicit val instantSchema: Schema[java.time.Instant] =
    Schema.primitive(StandardType.InstantType(DateTimeFormatter.ISO_INSTANT))
  implicit val localDateSchema: Schema[java.time.LocalDate] =
    Schema.primitive(StandardType.LocalDateType(DateTimeFormatter.ISO_LOCAL_DATE))
  implicit val localTimeSchema: Schema[java.time.LocalTime] =
    Schema.primitive(StandardType.LocalTimeType(DateTimeFormatter.ISO_LOCAL_TIME))
  implicit val localDateTimeSchema: Schema[java.time.LocalDateTime] =
    Schema.primitive(StandardType.LocalDateTimeType(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
  implicit val offsetTimeSchema: Schema[java.time.OffsetTime] =
    Schema.primitive(StandardType.OffsetTimeType(DateTimeFormatter.ISO_OFFSET_TIME))
  implicit val offsetDateTimeSchema: Schema[java.time.OffsetDateTime] =
    Schema.primitive(StandardType.OffsetDateTimeType(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
  implicit val zonedDateTimeSchema: Schema[java.time.ZonedDateTime] =
    Schema.primitive(StandardType.ZonedDateTimeType(DateTimeFormatter.ISO_ZONED_DATE_TIME))
}
