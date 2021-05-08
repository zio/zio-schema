package zio.schema

import java.math.{ BigDecimal, BigInteger }
import java.time._

import scala.jdk.CollectionConverters._

import zio.random.Random
import zio.test._

object PrimitiveGen {

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
      case _: StandardType.Instant             => anyInstant
      case _: StandardType.LocalDate           => anyLocalDate
      case _: StandardType.LocalTime           => anyLocalTime
      case _: StandardType.LocalDateTime       => anyLocalDateTime
      case _: StandardType.OffsetTime          => anyOffsetTime
      case _: StandardType.OffsetDateTime      => anyOffsetDateTime
      case _: StandardType.ZonedDateTime       => anyZonedDateTime
    }

  val anyDayOfWeek: Gen[Random, DayOfWeek] =
    Gen.oneOf(
      Gen.const(DayOfWeek.MONDAY),
      Gen.const(DayOfWeek.TUESDAY),
      Gen.const(DayOfWeek.WEDNESDAY),
      Gen.const(DayOfWeek.THURSDAY),
      Gen.const(DayOfWeek.FRIDAY),
      Gen.const(DayOfWeek.SATURDAY),
      Gen.const(DayOfWeek.SUNDAY)
    )

  val anyMonth: Gen[Random, Month] =
    Gen.oneOf(
      Gen.const(Month.JANUARY),
      Gen.const(Month.FEBRUARY),
      Gen.const(Month.MARCH),
      Gen.const(Month.APRIL),
      Gen.const(Month.MAY),
      Gen.const(Month.JUNE),
      Gen.const(Month.JULY),
      Gen.const(Month.AUGUST),
      Gen.const(Month.SEPTEMBER),
      Gen.const(Month.OCTOBER),
      Gen.const(Month.NOVEMBER),
      Gen.const(Month.DECEMBER)
    )

  val anyMonthDay: Gen[Random, MonthDay] =
    for {
      month <- anyMonth
      days  <- Gen.int(1, month.maxLength())
    } yield MonthDay.of(month, days)

  val anyPeriod: Gen[Random, Period] =
    for {
      years  <- Gen.int(0, Int.MaxValue)
      months <- Gen.int(0, Int.MaxValue)
      days   <- Gen.int(0, Int.MaxValue)
    } yield Period.of(years, months, days)

  val anyYear: Gen[Random, Year] =
    Gen.int(Year.MIN_VALUE, Year.MAX_VALUE).map(Year.of)

  val anyYearMonth: Gen[Random, YearMonth] =
    for {
      year  <- anyYear
      month <- anyMonth
    } yield YearMonth.of(year.getValue(), month.getValue())

  val anyZoneId: Gen[Random, ZoneId] = {
    val zoneIds = ZoneId.SHORT_IDS.keySet().asScala
    val idMap   = Set.range(0, zoneIds.size).zip(zoneIds).toMap

    Gen.int(0, zoneIds.size).map(idx => ZoneId.of(idMap(idx)))
  }

  val anyZoneOffset: Gen[Random, ZoneOffset] =
    anyZoneId.map { zoneId =>
      val instant = Instant.now()
      zoneId.getRules().getOffset(instant)
    }

  val anyDuration: Gen[Random, Duration] =
    for {
      seconds <- Gen.anyLong
      nanos   <- Gen.long(0, 999999999L)
    } yield Duration.ofSeconds(seconds, nanos)

  val anyInstant: Gen[Random, Instant] =
    for {
      seconds <- Gen.long(Instant.MIN.getEpochSecond(), Instant.MAX.getEpochSecond())
      nanos   <- Gen.int(Instant.MIN.getNano(), Instant.MAX.getNano())
    } yield Instant.ofEpochSecond(seconds, nanos.toLong)

  val anyLocalDate: Gen[Random, LocalDate] =
    for {
      yaer     <- anyYear
      monthDay <- anyMonthDay
    } yield LocalDate.of(yaer.getValue(), monthDay.getMonth(), monthDay.getDayOfMonth())

  val anyLocalTime: Gen[Random, LocalTime] =
    for {
      hour   <- Gen.int(0, 23)
      minute <- Gen.int(0, 59)
      second <- Gen.int(0, 59)
      nanos  <- Gen.int(0, 999999999)
    } yield LocalTime.of(hour, minute, second, nanos)

  val anyLocalDateTime: Gen[Random, LocalDateTime] =
    for {
      date <- anyLocalDate
      time <- anyLocalTime
    } yield LocalDateTime.of(
      date.getYear(),
      date.getMonth(),
      date.getDayOfMonth(),
      time.getHour(),
      time.getMinute(),
      time.getSecond(),
      time.getNano()
    )

  val anyOffsetTime: Gen[Random, OffsetTime] =
    for {
      time   <- anyLocalTime
      offset <- anyZoneOffset
    } yield OffsetTime.of(time, offset)

  val anyOffsetDateTime: Gen[Random, OffsetDateTime] =
    for {
      dateTime <- anyLocalDateTime
      offset   <- anyZoneOffset
    } yield OffsetDateTime.of(dateTime, offset)

  val anyZonedDateTime: Gen[Random, ZonedDateTime] =
    for {
      dateTime <- anyLocalDateTime
      zoneId   <- anyZoneId
    } yield ZonedDateTime.of(dateTime, zoneId)

}
