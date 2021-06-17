package zio.schema

import zio.Chunk

import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.TemporalUnit

sealed trait StandardType[A] {
  def defaultValue: Either[String, A]
}

object StandardType {
  implicit object UnitType extends StandardType[Unit] {
    def defaultValue: Either[String, Unit] = Right(())
  }

  implicit object StringType extends StandardType[String] {
    def defaultValue: Either[String, String] = Right("")
  }

  implicit object BoolType extends StandardType[Boolean] {
    def defaultValue: Either[String, Boolean] = Right(true)
  }

  implicit object ShortType extends StandardType[Short] {
    def defaultValue: Either[String, Short] = Right(0.asInstanceOf[Short])
  }

  implicit object IntType extends StandardType[Int] {
    def defaultValue: Either[String, Int] = Right(0)
  }

  implicit object LongType extends StandardType[Long] {
    def defaultValue: Either[String, Long] = Right(0.asInstanceOf[Long])
  }

  implicit object FloatType extends StandardType[Float] {
    def defaultValue: Either[String, Float] = Right(0.0.asInstanceOf[Float])
  }

  implicit object DoubleType extends StandardType[Double] {
    def defaultValue: Either[String, Double] = Right(0.0)
  }

  implicit object BinaryType extends StandardType[Chunk[Byte]] {
    def defaultValue: Either[String, Chunk[Byte]] = Right(Chunk.empty)
  }

  implicit object CharType extends StandardType[Char] {
    // The NUL Unicode character is used as the default value for
    // `StandardType[Char]` because the empty Char '' does not compile
    def defaultValue: Either[String, Char] = Right('\u0000')
  }

  implicit object BigDecimalType extends StandardType[java.math.BigDecimal] {
    def defaultValue = Right(java.math.BigDecimal.ZERO)
  }

  implicit object BigIntegerType extends StandardType[java.math.BigInteger] {
    def defaultValue = Right(java.math.BigInteger.ZERO)
  }

  //java.time specific types
  implicit object DayOfWeekType extends StandardType[DayOfWeek] {
    def defaultValue = Right(java.time.temporal.WeekFields.of(java.util.Locale.getDefault).getFirstDayOfWeek)
  }

  implicit object Month extends StandardType[java.time.Month] {
    def defaultValue = Right(java.time.Month.JANUARY)
  }

  implicit object MonthDay extends StandardType[java.time.MonthDay] {
    def defaultValue: Either[String, java.time.MonthDay] = Right(java.time.MonthDay.of(java.time.Month.JANUARY, 1))
  }

  implicit object Period extends StandardType[java.time.Period] {
    def defaultValue: Either[String, java.time.Period] = Right(java.time.Period.ZERO)
  }

  implicit object Year extends StandardType[java.time.Year] {
    def defaultValue: Either[String, java.time.Year] = Right(java.time.Year.now)
  }

  implicit object YearMonth extends StandardType[java.time.YearMonth] {

    def defaultValue: Either[String, java.time.YearMonth] =
      Right(java.time.YearMonth.now)
  }

  implicit object ZoneId extends StandardType[java.time.ZoneId] {
    def defaultValue: Either[String, java.time.ZoneId] = Right(java.time.ZoneId.systemDefault)
  }

  implicit object ZoneOffset extends StandardType[java.time.ZoneOffset] {
    def defaultValue: Either[String, java.time.ZoneOffset] = Right(java.time.ZoneOffset.UTC)
  }

  final case class Duration(temporalUnit: TemporalUnit) extends StandardType[java.time.Duration] {
    def defaultValue: Either[String, java.time.Duration] = Right(java.time.Duration.ZERO)
  }

  final case class Instant(formatter: DateTimeFormatter) extends StandardType[java.time.Instant] {
    def defaultValue: Either[String, java.time.Instant] = Right(java.time.Instant.EPOCH)
  }

  final case class LocalDate(formatter: DateTimeFormatter) extends StandardType[java.time.LocalDate] {
    def defaultValue: Either[String, java.time.LocalDate] = Right(java.time.LocalDate.now)
  }

  final case class LocalTime(formatter: DateTimeFormatter) extends StandardType[java.time.LocalTime] {
    def defaultValue: Either[String, java.time.LocalTime] = Right(java.time.LocalTime.MIDNIGHT)
  }

  final case class LocalDateTime(formatter: DateTimeFormatter) extends StandardType[java.time.LocalDateTime] {
    def defaultValue: Either[String, java.time.LocalDateTime] = Right(java.time.LocalDateTime.now)
  }

  final case class OffsetTime(formatter: DateTimeFormatter) extends StandardType[java.time.OffsetTime] {
    def defaultValue: Either[String, java.time.OffsetTime] = Right(java.time.OffsetTime.now)
  }

  final case class OffsetDateTime(formatter: DateTimeFormatter) extends StandardType[java.time.OffsetDateTime] {
    def defaultValue: Either[String, java.time.OffsetDateTime] = Right(java.time.OffsetDateTime.now)
  }

  final case class ZonedDateTime(formatter: DateTimeFormatter) extends StandardType[java.time.ZonedDateTime] {
    def defaultValue: Either[String, java.time.ZonedDateTime] = Right(java.time.ZonedDateTime.now)
  }
}
