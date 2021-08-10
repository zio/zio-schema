package zio.schema

import java.math.BigInteger
import java.time
import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.{ ChronoUnit, TemporalUnit }

import scala.annotation.tailrec
import zio.Chunk

sealed trait StandardType[A] extends  Ordering[A]{
  def tag: String
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
      case Tags.MONTH            => Some(Month)
      case Tags.MONTH_DAY        => Some(MonthDay)
      case Tags.PERIOD           => Some(Period)
      case Tags.DAY_OF_WEEK      => Some(DayOfWeekType)
      case Tags.YEAR             => Some(Year)
      case Tags.YEAR_MONTH       => Some(YearMonth)
      case Tags.ZONE_ID          => Some(ZoneId)
      case Tags.ZONE_OFFSET      => Some(ZoneOffset)
      case Tags.INSTANT          => Some(Instant(DateTimeFormatter.ISO_INSTANT))
      case Tags.LOCAL_DATE       => Some(LocalDate(DateTimeFormatter.ISO_LOCAL_DATE))
      case Tags.LOCAL_TIME       => Some(LocalTime(DateTimeFormatter.ISO_LOCAL_TIME))
      case Tags.LOCAL_DATE_TIME  => Some(LocalDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
      case Tags.OFFSET_TIME      => Some(OffsetTime(DateTimeFormatter.ISO_OFFSET_TIME))
      case Tags.OFFSET_DATE_TIME => Some(OffsetDateTime(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
      case Tags.ZONED_DATE_TIME  => Some(ZonedDateTime(DateTimeFormatter.ISO_ZONED_DATE_TIME))
      case units =>
        try {
          Some(Duration(ChronoUnit.valueOf(units)))
        } catch { case _: Throwable => None }
    }

  def fromTemporalUnits(units: String): Option[StandardType[java.time.Duration]] =
    ChronoUnit.values().find(_.toString == units).map(Duration(_))

  implicit object UnitType   extends StandardType[Unit]        {
    override def compare(x: Unit, y: Unit): Int = 0
    override def tag = Tags.UNIT
  }

  implicit object StringType extends StandardType[String]      {
    override def compare(x: String, y: String): Int = x.compareTo(y)
    override def tag = Tags.STRING
  }

  implicit object BoolType   extends StandardType[Boolean]     {
    override def compare(x: Boolean, y: Boolean): Int = x.compareTo(y)
    override def tag = Tags.BOOL
  }

  implicit object ShortType  extends StandardType[Short]       {
    override def compare(x: Short, y: Short): Int = x.compareTo(y)
    override def tag = Tags.SHORT
  }

  implicit object IntType    extends StandardType[Int]         {
    override def compare(x: Int, y: Int): Int = x.compareTo(y)
    override def tag = Tags.INT
  }

  implicit object LongType   extends StandardType[Long]        {
    override def compare(x: Long, y: Long): Int = x.compareTo(y)
    override def tag = Tags.LONG
  }

  implicit object FloatType  extends StandardType[Float]       {
    override def compare(x: Float, y: Float): Int = x.compareTo(y)
    override def tag = Tags.FLOAT
  }

  implicit object DoubleType extends StandardType[Double]      {
    override def compare(x: Double, y: Double): Int = x.compareTo(y)
    override def tag = Tags.DOUBLE
  }

  implicit object BinaryType extends StandardType[Chunk[Byte]] {
    override def compare(x: Chunk[Byte], y: Chunk[Byte]): Int = {
      val j = x.length
      val k = y.length
      @tailrec
      def loop(i: Int): Int =
        if (i == j && i == k) 0
        else if (i == j) -1
        else if (i == k) 1
        else {
          val compare = x(i).compareTo(y(i))
          if (compare == 0) loop(i + 1) else compare
        }
      loop(0)
    }
    override def tag = Tags.BINARY
  }

  implicit object CharType   extends StandardType[Char]        {
    override def compare(x: Char, y: Char): Int = x.compareTo(y)
    override def tag = Tags.CHAR
  }

  implicit object BigDecimalType extends StandardType[java.math.BigDecimal] {
    override def compare(x: java.math.BigDecimal, y: java.math.BigDecimal): Int = x.compareTo(y)
    override def tag = Tags.BIG_DECIMAL
  }

  implicit object BigIntegerType extends StandardType[java.math.BigInteger] {
    override def compare(x: BigInteger, y: BigInteger): Int = x.compareTo(y)
    override def tag = Tags.BIG_INTEGER
  }

  //java.time specific types
  implicit object DayOfWeekType extends StandardType[DayOfWeek]            {
    override def compare(x: DayOfWeek, y: DayOfWeek): Int = x.getValue.compareTo(y.getValue)
    override def tag = Tags.DAY_OF_WEEK
  }

  implicit object Month         extends StandardType[java.time.Month]      {
    override def compare(x: Month, y: Month): Int = x.getValue.compareTo(y.getValue)
    override def tag = Tags.MONTH
  }

  implicit object MonthDay      extends StandardType[java.time.MonthDay]   {
    override def compare(x: MonthDay, y: MonthDay): Int = x.compareTo(y)
    override def tag = Tags.MONTH_DAY
  }

  implicit object Period        extends StandardType[java.time.Period]     {
    override def compare(x: Period, y: Period): Int = 0 //TODO how to do this? in general not comperable because months vary in days
    override def tag = Tags.PERIOD
  }

  implicit object Year          extends StandardType[java.time.Year]       {
    override def compare(x: Year, y: Year): Int = x.getValue.compareTo(y.getValue)
    override def tag = Tags.YEAR
  }

  implicit object YearMonth     extends StandardType[java.time.YearMonth]  {
    override def compare(x: YearMonth, y: YearMonth): Int = x.compareTo(y)
    override def tag = Tags.YEAR_MONTH
  }

  implicit object ZoneId        extends StandardType[java.time.ZoneId]     {
    override def compare(x: ZoneId, y: ZoneId): Int = x.getId.compareTo(y.getId) //TODO is there a better comparison
    override def tag = Tags.ZONE_ID
  }

  implicit object ZoneOffset    extends StandardType[java.time.ZoneOffset] {
    override def compare(x: ZoneOffset, y: ZoneOffset): Int = x.compareTo(y)
    override def tag = Tags.ZONE_OFFSET
  }

  final case class Duration(temporalUnit: TemporalUnit) extends StandardType[java.time.Duration] {
    override def compare(x: time.Duration, y: time.Duration): Int = x.compareTo(y)
    override def tag: String = temporalUnit.toString().toUpperCase()
  }

  final case class Instant(formatter: DateTimeFormatter) extends StandardType[java.time.Instant] {
    override def compare(x: time.Instant, y: time.Instant): Int = x.compareTo(y)
    override def tag = Tags.INSTANT
  }

  final case class LocalDate(formatter: DateTimeFormatter) extends StandardType[java.time.LocalDate] {
    override def compare(x: time.LocalDate, y: time.LocalDate): Int = x.compareTo(y)
    override def tag = Tags.LOCAL_DATE
  }

  final case class LocalTime(formatter: DateTimeFormatter) extends StandardType[java.time.LocalTime] {
    override def compare(x: time.LocalTime, y: time.LocalTime): Int = x.compareTo(y)
    override def tag = Tags.LOCAL_TIME
  }

  final case class LocalDateTime(formatter: DateTimeFormatter) extends StandardType[java.time.LocalDateTime] {
    override def compare(x: time.LocalDateTime, y: time.LocalDateTime): Int = x.compareTo(y)
    override def tag = Tags.LOCAL_DATE_TIME
  }

  final case class OffsetTime(formatter: DateTimeFormatter) extends StandardType[java.time.OffsetTime] {
    override def compare(x: time.OffsetTime, y: time.OffsetTime): Int = x.compareTo(y)
    override def tag = Tags.OFFSET_TIME
  }

  final case class OffsetDateTime(formatter: DateTimeFormatter) extends StandardType[java.time.OffsetDateTime] {
    override def compare(x: time.OffsetDateTime, y: time.OffsetDateTime): Int = x.compareTo(y)
    override def tag = Tags.OFFSET_DATE_TIME
  }

  final case class ZonedDateTime(formatter: DateTimeFormatter) extends StandardType[java.time.ZonedDateTime] {
    override def compare(x: time.ZonedDateTime, y: time.ZonedDateTime): Int = x.compareTo(y)
    override def tag = Tags.ZONED_DATE_TIME
  }

}
