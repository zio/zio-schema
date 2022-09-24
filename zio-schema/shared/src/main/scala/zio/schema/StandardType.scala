package zio.schema

import java.math.BigInteger
import java.time
import java.time._
import java.time.format.DateTimeFormatter

import zio.Chunk
import zio.schema._
import java.util.{ UUID => JUUID }

sealed trait StandardType[A] extends Ordering[A] { self =>
  val id: TypeId
  def defaultValue: Either[String, A]
  override def toString: String = id.toString()

  /**
   * Converts a DynamicValue into a primitive type.
   */
  def toTypedPrimitive(value: DynamicValue): Either[String, A] =
    value.toTypedValue(Schema.primitive[A](self))
}

object StandardType {

  private[schema] object Tags {
    final val UNIT             = TypeId.gen[Unit]
    final val STRING           = TypeId.gen[String]
    final val BOOL             = TypeId.gen[Boolean]
    final val BYTE             = TypeId.gen[Byte]
    final val SHORT            = TypeId.gen[Short]
    final val INT              = TypeId.gen[Int]
    final val LONG             = TypeId.gen[Long]
    final val FLOAT            = TypeId.gen[Double]
    final val DOUBLE           = TypeId.gen[Double]
    final val BINARY           = TypeId.gen[Chunk[Byte]]
    final val CHAR             = TypeId.gen[Char]
    final val BIG_DECIMAL      = TypeId.gen[BigDecimal]
    final val BIG_INTEGER      = TypeId.gen[BigInt]
    final val DAY_OF_WEEK      = TypeId.gen[DayOfWeek]
    final val MONTH            = TypeId.gen[Month]
    final val MONTH_DAY        = TypeId.gen[MonthDay]
    final val PERIOD           = TypeId.gen[Period]
    final val YEAR             = TypeId.gen[Year]
    final val YEAR_MONTH       = TypeId.gen[YearMonth]
    final val ZONE_ID          = TypeId.gen[ZoneId]
    final val ZONE_OFFSET      = TypeId.gen[ZoneOffset]
    final val DURATION         = TypeId.gen[Duration]
    final val INSTANT          = TypeId.gen[Instant]
    final val LOCAL_DATE       = TypeId.gen[LocalDate]
    final val LOCAL_TIME       = TypeId.gen[LocalTime]
    final val LOCAL_DATE_TIME  = TypeId.gen[LocalDateTime]
    final val OFFSET_TIME      = TypeId.gen[OffsetTime]
    final val OFFSET_DATE_TIME = TypeId.gen[OffsetDateTime]
    final val ZONED_DATE_TIME  = TypeId.gen[ZonedDateTime]
    final val UUID             = TypeId.gen[JUUID]
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
      case Tags.INSTANT          => Some(InstantType(DateTimeFormatter.ISO_INSTANT))
      case Tags.LOCAL_DATE       => Some(LocalDateType(DateTimeFormatter.ISO_LOCAL_DATE))
      case Tags.LOCAL_TIME       => Some(LocalTimeType(DateTimeFormatter.ISO_LOCAL_TIME))
      case Tags.LOCAL_DATE_TIME  => Some(LocalDateTimeType(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
      case Tags.OFFSET_TIME      => Some(OffsetTimeType(DateTimeFormatter.ISO_OFFSET_TIME))
      case Tags.OFFSET_DATE_TIME => Some(OffsetDateTimeType(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
      case Tags.ZONED_DATE_TIME  => Some(ZonedDateTimeType(DateTimeFormatter.ISO_ZONED_DATE_TIME))
      case Tags.UUID             => Some(UUIDType)
    } 

  def apply[A](implicit standardType: StandardType[A]): StandardType[A] = standardType

  implicit object UnitType extends StandardType[Unit] {
    override val id: TypeId                        = Tags.UNIT
    override def compare(x: Unit, y: Unit): Int     = 0
    override def defaultValue: Either[String, Unit] = Right(())
  }

  implicit object StringType extends StandardType[String] {
    override val id: TypeId                          = Tags.STRING
    override def compare(x: String, y: String): Int   = x.compareTo(y)
    override def defaultValue: Either[String, String] = Right("")
  }

  implicit object BoolType extends StandardType[Boolean] {
    override val id: TypeId                           = Tags.BOOL
    override def compare(x: Boolean, y: Boolean): Int  = x.compareTo(y)
    override def defaultValue: Either[String, Boolean] = Right(false)
  }

  implicit object ByteType extends StandardType[Byte] {
    override val id: TypeId                        = Tags.BYTE
    override def compare(x: Byte, y: Byte): Int     = x.compareTo(y)
    override def defaultValue: Either[String, Byte] = Right(0.toByte)
  }

  implicit object ShortType extends StandardType[Short] {
    override val id: TypeId                         = Tags.SHORT
    override def compare(x: Short, y: Short): Int    = x.compareTo(y)
    override def defaultValue: Either[String, Short] = Right(0.asInstanceOf[Short])
  }

  implicit object IntType extends StandardType[Int] {
    override val id: TypeId                       = Tags.INT
    override def compare(x: Int, y: Int): Int      = x.compareTo(y)
    override def defaultValue: Either[String, Int] = Right(0)
  }

  implicit object LongType extends StandardType[Long] {
    override val id: TypeId                        = Tags.LONG
    override def compare(x: Long, y: Long): Int     = x.compareTo(y)
    override def defaultValue: Either[String, Long] = Right(0.asInstanceOf[Long])
  }

  implicit object FloatType extends StandardType[Float] {
    override val id: TypeId                         = Tags.FLOAT
    override def compare(x: Float, y: Float): Int    = x.compareTo(y)
    override def defaultValue: Either[String, Float] = Right(0.0.asInstanceOf[Float])
  }

  implicit object DoubleType extends StandardType[Double] {
    override val id: TypeId                          = Tags.DOUBLE
    override def compare(x: Double, y: Double): Int   = x.compareTo(y)
    override def defaultValue: Either[String, Double] = Right(0.0)
  }

  implicit object BinaryType extends StandardType[Chunk[Byte]] {
    override val id: TypeId                                  = Tags.BINARY
    override def compare(x: Chunk[Byte], y: Chunk[Byte]): Int = x.sum.compare(y.sum)
    override def defaultValue: Either[String, Chunk[Byte]]    = Right(Chunk.empty)
  }

  implicit object CharType extends StandardType[Char] {
    override val id: TypeId                    = Tags.CHAR
    override def compare(x: Char, y: Char): Int = x.compareTo(y)
    // The NUL Unicode character is used as the default value for
    // `StandardType[Char]` because the empty Char '' does not compile
    override def defaultValue: Either[String, Char] = Right('\u0000')
  }

  implicit object UUIDType extends StandardType[java.util.UUID] {
    override val id: TypeId                                        = Tags.UUID
    override def compare(x: java.util.UUID, y: java.util.UUID): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.util.UUID]       = Right(java.util.UUID.randomUUID())
  }

  implicit object BigDecimalType extends StandardType[java.math.BigDecimal] {
    override val id: TypeId                                                    = Tags.BIG_DECIMAL
    override def compare(x: java.math.BigDecimal, y: java.math.BigDecimal): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.math.BigDecimal]             = Right(java.math.BigDecimal.ZERO)
  }

  implicit object BigIntegerType extends StandardType[java.math.BigInteger] {
    override val id: TypeId                                        = Tags.BIG_INTEGER
    override def compare(x: BigInteger, y: BigInteger): Int         = x.compareTo(y)
    override def defaultValue: Either[String, java.math.BigInteger] = Right(java.math.BigInteger.ZERO)
  }

  //java.time specific types
  implicit object DayOfWeekType extends StandardType[DayOfWeek] {
    override val id: TypeId                              = Tags.DAY_OF_WEEK
    override def compare(x: DayOfWeek, y: DayOfWeek): Int = x.getValue.compareTo(y.getValue)
    override def defaultValue: Either[String, DayOfWeek] =
      Right(java.time.temporal.WeekFields.of(java.util.Locale.getDefault).getFirstDayOfWeek)
  }

  implicit object MonthType extends StandardType[java.time.Month] {
    override val id: TypeId                                   = Tags.MONTH
    override def compare(x: Month, y: Month): Int              = x.getValue.compareTo(y.getValue)
    override def defaultValue: Either[String, java.time.Month] = Right(java.time.Month.JANUARY)
  }

  implicit object MonthDayType extends StandardType[java.time.MonthDay] {
    override val id: TypeId                            = Tags.MONTH_DAY
    override def compare(x: MonthDay, y: MonthDay): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.MonthDay] =
      Right(java.time.MonthDay.of(java.time.Month.JANUARY, 1))
  }

  implicit object PeriodType extends StandardType[java.time.Period] {
    override val id: TypeId = Tags.PERIOD
    override def compare(x: Period, y: Period): Int = {
      val startDate = time.LocalDate.of(0, 1, 1)
      startDate.plus(x).compareTo(startDate.plus(y))
    }
    override def defaultValue: Either[String, java.time.Period] = Right(java.time.Period.ZERO)
  }

  implicit object YearType extends StandardType[java.time.Year] {
    override val id: TypeId                                  = Tags.YEAR
    override def compare(x: Year, y: Year): Int               = x.getValue.compareTo(y.getValue)
    override def defaultValue: Either[String, java.time.Year] = Right(java.time.Year.now)
  }

  implicit object YearMonthType extends StandardType[java.time.YearMonth] {
    override val id: TypeId                                       = Tags.YEAR_MONTH
    override def compare(x: YearMonth, y: YearMonth): Int          = x.compareTo(y)
    override def defaultValue: Either[String, java.time.YearMonth] = Right(java.time.YearMonth.now)
  }

  implicit object ZoneIdType extends StandardType[java.time.ZoneId] {
    override val id: TypeId                                    = Tags.ZONE_ID
    override def compare(x: ZoneId, y: ZoneId): Int             = x.getId.compareTo(y.getId) // TODO is there a better comparison
    override def defaultValue: Either[String, java.time.ZoneId] = Right(java.time.ZoneId.systemDefault)
  }

  implicit object ZoneOffsetType extends StandardType[java.time.ZoneOffset] {
    override val id: TypeId                                        = Tags.ZONE_OFFSET
    override def compare(x: ZoneOffset, y: ZoneOffset): Int         = x.compareTo(y)
    override def defaultValue: Either[String, java.time.ZoneOffset] = Right(java.time.ZoneOffset.UTC)
  }

  implicit object DurationType extends StandardType[java.time.Duration] {
    override val id: TypeId                                      = Tags.DURATION
    override def compare(x: time.Duration, y: time.Duration): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.Duration] = Right(java.time.Duration.ZERO)
  }

  final case class InstantType(formatter: DateTimeFormatter) extends StandardType[java.time.Instant] {
    override val id: TypeId                                     = Tags.INSTANT
    override def compare(x: time.Instant, y: time.Instant): Int  = x.compareTo(y)
    override def defaultValue: Either[String, java.time.Instant] = Right(java.time.Instant.EPOCH)
  }

  final case class LocalDateType(formatter: DateTimeFormatter) extends StandardType[java.time.LocalDate] {
    override val id: TypeId                                        = Tags.LOCAL_DATE
    override def compare(x: time.LocalDate, y: time.LocalDate): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.LocalDate]  = Right(java.time.LocalDate.now)
  }

  final case class LocalTimeType(formatter: DateTimeFormatter) extends StandardType[java.time.LocalTime] {
    override val id: TypeId                                        = Tags.LOCAL_TIME
    override def compare(x: time.LocalTime, y: time.LocalTime): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.LocalTime]  = Right(java.time.LocalTime.MIDNIGHT)
  }

  final case class LocalDateTimeType(formatter: DateTimeFormatter) extends StandardType[java.time.LocalDateTime] {
    override val id: TypeId                                                = Tags.LOCAL_DATE_TIME
    override def compare(x: time.LocalDateTime, y: time.LocalDateTime): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.LocalDateTime]      = Right(java.time.LocalDateTime.now)
  }

  final case class OffsetTimeType(formatter: DateTimeFormatter) extends StandardType[java.time.OffsetTime] {
    override val id: TypeId                                          = Tags.OFFSET_TIME
    override def compare(x: time.OffsetTime, y: time.OffsetTime): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.OffsetTime]   = Right(java.time.OffsetTime.now)
  }

  final case class OffsetDateTimeType(formatter: DateTimeFormatter) extends StandardType[java.time.OffsetDateTime] {
    override val id: TypeId                                                  = Tags.OFFSET_DATE_TIME
    override def compare(x: time.OffsetDateTime, y: time.OffsetDateTime): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.OffsetDateTime]       = Right(java.time.OffsetDateTime.now)
  }

  final case class ZonedDateTimeType(formatter: DateTimeFormatter) extends StandardType[java.time.ZonedDateTime] {
    override val id: TypeId                                                = Tags.ZONED_DATE_TIME
    override def compare(x: time.ZonedDateTime, y: time.ZonedDateTime): Int = x.compareTo(y)
    override def defaultValue: Either[String, java.time.ZonedDateTime]      = Right(java.time.ZonedDateTime.now)
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
