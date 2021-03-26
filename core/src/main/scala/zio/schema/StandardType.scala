package zio.schema

import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalUnit

import zio.Chunk

sealed trait StandardType[A]

object StandardType {
  implicit object UnitType   extends StandardType[Unit]
  implicit object StringType extends StandardType[String]
  implicit object BoolType   extends StandardType[Boolean]
  implicit object ShortType  extends StandardType[Short]
  implicit object IntType    extends StandardType[Int]
  implicit object LongType   extends StandardType[Long]
  implicit object FloatType  extends StandardType[Float]
  implicit object DoubleType extends StandardType[Double]
  implicit object BinaryType extends StandardType[Chunk[Byte]]
  implicit object CharType   extends StandardType[Char]

  implicit object BigDecimalType extends StandardType[java.math.BigDecimal]
  implicit object BigIntegerType extends StandardType[java.math.BigInteger]

  //java.time specific types
  implicit object DayOfWeekType                                 extends StandardType[DayOfWeek]
  implicit object Month                                         extends StandardType[java.time.Month]
  implicit object MonthDay                                      extends StandardType[java.time.MonthDay]
  implicit object Period                                        extends StandardType[java.time.Period]
  implicit object Year                                          extends StandardType[java.time.Year]
  implicit object YearMonth                                     extends StandardType[java.time.YearMonth]
  implicit object ZoneId                                        extends StandardType[java.time.ZoneId]
  implicit object ZoneOffset                                    extends StandardType[java.time.ZoneOffset]
  final case class Duration(temporalUnit: TemporalUnit)         extends StandardType[java.time.Duration]
  final case class Instant(formatter: DateTimeFormatter)        extends StandardType[java.time.Instant]
  final case class LocalDate(formatter: DateTimeFormatter)      extends StandardType[java.time.LocalDate]
  final case class LocalTime(formatter: DateTimeFormatter)      extends StandardType[java.time.LocalTime]
  final case class LocalDateTime(formatter: DateTimeFormatter)  extends StandardType[java.time.LocalDateTime]
  final case class OffsetTime(formatter: DateTimeFormatter)     extends StandardType[java.time.OffsetTime]
  final case class OffsetDateTime(formatter: DateTimeFormatter) extends StandardType[java.time.OffsetDateTime]
  final case class ZonedDateTime(formatter: DateTimeFormatter)  extends StandardType[java.time.ZonedDateTime]
}
