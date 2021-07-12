package zio.schema

import java.math.{ BigDecimal, BigInteger }
import java.time._

import scala.collection.JavaConverters._

import com.github.ghik.silencer.silent

import zio.random.Random
import zio.test._

private[schema] object PrimitiveGen {

  def apply(standardType: StandardType[_]): Gen[Sized with Random, _] =
    standardType match {
      case _: StandardType.UnitType.type       => Gen.unit
      case _: StandardType.StringType.type     => Gen.anyString
      case _: StandardType.BoolType.type       => Gen.boolean
      case _: StandardType.ShortType.type      => Gen.anyShort
      case _: StandardType.IntType.type        => Gen.anyInt
      case _: StandardType.LongType.type       => Gen.anyLong
      case _: StandardType.FloatType.type      => Gen.anyFloat
      case _: StandardType.DoubleType.type     => Gen.anyDouble
      case _: StandardType.BinaryType.type     => Gen.chunkOf(Gen.anyByte)
      case _: StandardType.CharType.type       => Gen.anyASCIIChar
      case _: StandardType.BigDecimalType.type => Gen.anyDouble.map(BigDecimal.valueOf)
      case _: StandardType.BigIntegerType.type => Gen.anyLong.map(BigInteger.valueOf)
      case _: StandardType.DayOfWeekType.type  => anyDayOfWeek
      case _: StandardType.Month.type          => anyMonth
      case _: StandardType.MonthDay.type       => anyMonthDay
      case _: StandardType.Period.type         => anyPeriod
      case _: StandardType.Year.type           => anyYear
      case _: StandardType.YearMonth.type      => anyYearMonth
      case _: StandardType.ZoneId.type         => anyZoneId
      case _: StandardType.ZoneOffset.type     => anyZoneOffset
      case _: StandardType.Duration            => anyDuration
      case _: StandardType.Instant             => Gen.anyInstant
      case _: StandardType.LocalDate           => anyLocalDate
      case _: StandardType.LocalTime           => anyLocalTime
      case _: StandardType.LocalDateTime       => Gen.anyLocalDateTime
      case _: StandardType.OffsetTime          => anyOffsetTime
      case _: StandardType.OffsetDateTime      => Gen.anyOffsetDateTime
      case _: StandardType.ZonedDateTime       => anyZonedDateTime
    }

  def anyDayOfWeek: Gen[Random, DayOfWeek] =
    Gen.elements(
      DayOfWeek.MONDAY,
      DayOfWeek.TUESDAY,
      DayOfWeek.WEDNESDAY,
      DayOfWeek.THURSDAY,
      DayOfWeek.FRIDAY,
      DayOfWeek.SATURDAY,
      DayOfWeek.SUNDAY
    )

  def anyDuration: Gen[Random, Duration] =
    for {
      seconds <- Gen.anyLong
      nanos   <- Gen.long(0, 999999999L)
    } yield Duration.ofSeconds(seconds, nanos)

  def anyLocalDate: Gen[Random, LocalDate] =
    for {
      year   <- anyYear
      month  <- Gen.int(1, 12)
      maxLen = if (!year.isLeap && month == 2) 28 else Month.of(month).maxLength
      day    <- Gen.int(1, maxLen)
    } yield LocalDate.of(year.getValue, month, day)

  def anyLocalTime: Gen[Random, LocalTime] =
    for {
      hour   <- Gen.int(0, 23)
      minute <- Gen.int(0, 59)
      second <- Gen.int(0, 59)
      nanos  <- Gen.int(0, 999999999)
    } yield LocalTime.of(hour, minute, second, nanos)

  def anyMonth: Gen[Random, Month] =
    Gen.elements(
      Month.JANUARY,
      Month.FEBRUARY,
      Month.MARCH,
      Month.APRIL,
      Month.MAY,
      Month.JUNE,
      Month.JULY,
      Month.AUGUST,
      Month.SEPTEMBER,
      Month.OCTOBER,
      Month.NOVEMBER,
      Month.DECEMBER
    )

  def anyMonthDay: Gen[Random, MonthDay] =
    for {
      month <- Gen.int(1, 12).map(Month.of)
      days  <- Gen.int(1, month.maxLength())
    } yield MonthDay.of(month, days)

  def anyOffsetTime: Gen[Random, OffsetTime] =
    for {
      time   <- anyLocalTime
      offset <- anyZoneOffset
    } yield OffsetTime.of(time, ZoneOffset.ofTotalSeconds(-offset.getTotalSeconds))

  def anyPeriod: Gen[Random, Period] =
    for {
      years  <- Gen.int(0, Int.MaxValue)
      months <- Gen.int(0, Int.MaxValue)
      days   <- Gen.int(0, Int.MaxValue)
    } yield Period.of(years, months, days)

  def anyYear: Gen[Random, Year] =
    Gen.int(Year.MIN_VALUE, Year.MAX_VALUE).map(Year.of)

  def anyYearMonth: Gen[Random, YearMonth] =
    for {
      year  <- anyYear
      month <- Gen.int(1, 12)
    } yield YearMonth.of(year.getValue(), month)

  def anyZonedDateTime: Gen[Random, ZonedDateTime] =
    for {
      dateTime <- Gen.anyLocalDateTime
      zoneId   <- anyZoneId
    } yield ZonedDateTime.of(dateTime, zoneId)

  @silent("JavaConverters")
  final def anyZoneId: Gen[Random, ZoneId] =
    Gen.elements(ZoneId.getAvailableZoneIds.asScala.map(ZoneId.of).toList: _*).noShrink

  def anyZoneOffset: Gen[Random, ZoneOffset] =
    Gen
      .int(ZoneOffset.MIN.getTotalSeconds, ZoneOffset.MAX.getTotalSeconds)
      .map(ZoneOffset.ofTotalSeconds)

}
