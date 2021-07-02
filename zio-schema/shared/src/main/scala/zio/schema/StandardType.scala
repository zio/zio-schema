package zio.schema

import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.{ ChronoUnit, TemporalUnit }

import zio.Chunk

sealed trait StandardType[A] {
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
      case _                     => None
    }

  def fromTemporalUnits(units: String): Option[StandardType[java.time.Duration]] =
    ChronoUnit.values().find(_.toString == units).map(Duration(_))

  implicit object UnitType   extends StandardType[Unit]        { override def tag = Tags.UNIT   }
  implicit object StringType extends StandardType[String]      { override def tag = Tags.STRING }
  implicit object BoolType   extends StandardType[Boolean]     { override def tag = Tags.BOOL   }
  implicit object ShortType  extends StandardType[Short]       { override def tag = Tags.SHORT  }
  implicit object IntType    extends StandardType[Int]         { override def tag = Tags.INT    }
  implicit object LongType   extends StandardType[Long]        { override def tag = Tags.LONG   }
  implicit object FloatType  extends StandardType[Float]       { override def tag = Tags.FLOAT  }
  implicit object DoubleType extends StandardType[Double]      { override def tag = Tags.DOUBLE }
  implicit object BinaryType extends StandardType[Chunk[Byte]] { override def tag = Tags.BINARY }
  implicit object CharType   extends StandardType[Char]        { override def tag = Tags.CHAR   }

  implicit object BigDecimalType extends StandardType[java.math.BigDecimal] { override def tag = Tags.BIG_DECIMAL }
  implicit object BigIntegerType extends StandardType[java.math.BigInteger] { override def tag = Tags.BIG_INTEGER }

  //java.time specific types
  implicit object DayOfWeekType extends StandardType[DayOfWeek]            { override def tag = Tags.DAY_OF_WEEK }
  implicit object Month         extends StandardType[java.time.Month]      { override def tag = Tags.MONTH       }
  implicit object MonthDay      extends StandardType[java.time.MonthDay]   { override def tag = Tags.MONTH_DAY   }
  implicit object Period        extends StandardType[java.time.Period]     { override def tag = Tags.PERIOD      }
  implicit object Year          extends StandardType[java.time.Year]       { override def tag = Tags.YEAR        }
  implicit object YearMonth     extends StandardType[java.time.YearMonth]  { override def tag = Tags.YEAR_MONTH  }
  implicit object ZoneId        extends StandardType[java.time.ZoneId]     { override def tag = Tags.ZONE_ID     }
  implicit object ZoneOffset    extends StandardType[java.time.ZoneOffset] { override def tag = Tags.ZONE_OFFSET }
  final case class Duration(temporalUnit: TemporalUnit) extends StandardType[java.time.Duration] {
    override def tag = Tags.DURATION
  }
  final case class Instant(formatter: DateTimeFormatter) extends StandardType[java.time.Instant] {
    override def tag = Tags.INSTANT
  }
  final case class LocalDate(formatter: DateTimeFormatter) extends StandardType[java.time.LocalDate] {
    override def tag = Tags.LOCAL_DATE
  }
  final case class LocalTime(formatter: DateTimeFormatter) extends StandardType[java.time.LocalTime] {
    override def tag = Tags.LOCAL_TIME
  }
  final case class LocalDateTime(formatter: DateTimeFormatter) extends StandardType[java.time.LocalDateTime] {
    override def tag = Tags.LOCAL_DATE_TIME
  }
  final case class OffsetTime(formatter: DateTimeFormatter) extends StandardType[java.time.OffsetTime] {
    override def tag = Tags.OFFSET_TIME
  }
  final case class OffsetDateTime(formatter: DateTimeFormatter) extends StandardType[java.time.OffsetDateTime] {
    override def tag = Tags.OFFSET_DATE_TIME
  }
  final case class ZonedDateTime(formatter: DateTimeFormatter) extends StandardType[java.time.ZonedDateTime] {
    override def tag = Tags.ZONED_DATE_TIME
  }
}
