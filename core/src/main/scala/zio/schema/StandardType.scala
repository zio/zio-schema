package zio.schema

import java.time._

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
  implicit object ByteType   extends StandardType[Byte]
  implicit object CharType   extends StandardType[Char]
  //java.time specific types
  implicit object DayOfWeekType      extends StandardType[DayOfWeek]
  implicit object DurationType       extends StandardType[Duration]
  implicit object InstantType        extends StandardType[Instant]
  implicit object LocalDateType      extends StandardType[LocalDate]
  implicit object LocalDateTimeType  extends StandardType[LocalDateTime]
  implicit object LocalTimeType      extends StandardType[LocalTime]
  implicit object MonthType          extends StandardType[Month]
  implicit object MonthDayType       extends StandardType[MonthDay]
  implicit object OffsetDateTimeType extends StandardType[OffsetDateTime]
  implicit object OffsetTimeType     extends StandardType[OffsetTime]
  implicit object PeriodType         extends StandardType[Period]
  implicit object YearType           extends StandardType[Year]
  implicit object YearMonthType      extends StandardType[YearMonth]
  implicit object ZonedDateTimeType  extends StandardType[ZonedDateTime]
  implicit object ZoneIdType         extends StandardType[ZoneId]
  implicit object ZoneOffsetType     extends StandardType[ZoneOffset]
}
