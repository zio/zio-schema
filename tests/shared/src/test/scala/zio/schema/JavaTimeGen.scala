package zio.schema

import java.time._
import java.time.temporal.ChronoField

import zio.test.Gen

object JavaTimeGen {

  val anyDayOfWeek: Gen[Any, DayOfWeek] = Gen.oneOf(
    Gen.const(DayOfWeek.MONDAY),
    Gen.const(DayOfWeek.TUESDAY),
    Gen.const(DayOfWeek.WEDNESDAY),
    Gen.const(DayOfWeek.THURSDAY),
    Gen.const(DayOfWeek.FRIDAY),
    Gen.const(DayOfWeek.SATURDAY),
    Gen.const(DayOfWeek.SUNDAY)
  )

  val anyMonth: Gen[Any, Month] = Gen.oneOf(
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

  val anyNanoOfDay: Gen[Any, Long] = chronoFieldValue(ChronoField.NANO_OF_DAY)

  val anyEpochDay: Gen[Any, Long] = Gen.long(LocalDate.MIN.toEpochDay, LocalDate.MAX.toEpochDay)

  val anyMonthOfYear: Gen[Any, Int] = chronoFieldValue(ChronoField.MONTH_OF_YEAR).map(_.toInt)

  val anyMonthDay: Gen[Any, MonthDay] =
    for {
      month      <- anyMonth
      dayOfMonth <- Gen.int(1, month.maxLength)
    } yield MonthDay.of(month, dayOfMonth)

  // Needs to be an ISO-8601 year between 0000 and 9999
  val anyIntYear: Gen[Any, Int] = Gen.int(0, 9999)

  val anyYear: Gen[Any, Year] = anyIntYear.map(Year.of)

  val anyYearMonth: Gen[Any, YearMonth] =
    anyIntYear.zipWith(anyMonthOfYear) { (year, month) =>
      YearMonth.of(year, month)
    }

  private def chronoFieldValue(chronoField: ChronoField) = {
    val range = chronoField.range
    Gen.long(range.getMinimum, range.getMaximum)
  }

  // FIXME There is a bug in JDK Duration parsing that caused issues in zio-json (https://github.com/zio/zio-json/issues/214).
  // Do not generate Durations with - seconds.Once that is addressed can remove filter condition
  val anyDuration: Gen[Any, Duration] = Gen
    .long(0, 999999999L)
    .zipWith(Gen.long(0, 999999999L)) { (seconds, nanos) =>
      Duration.ofSeconds(seconds, nanos)
    }

  val anyPeriod: Gen[Any, Period] =
    for {
      years  <- Gen.int(-99999, 99999)
      months <- Gen.int
      days   <- Gen.int
    } yield Period.of(years, months, days)

  val anyInstant: Gen[Any, Instant] = Gen
    .long(Instant.MIN.getEpochSecond, Instant.MAX.getEpochSecond)
    .zipWith(Gen.int(Instant.MIN.getNano, Instant.MAX.getNano)) { (seconds, nanos) =>
      Instant.ofEpochSecond(seconds, nanos.toLong)
    }

  val anyLocalDate: Gen[Any, LocalDate] = anyEpochDay.map(LocalDate.ofEpochDay)

  val anyLocalTime: Gen[Any, LocalTime] = anyNanoOfDay.map(LocalTime.ofNanoOfDay)

  val anyLocalDateTime: Gen[Any, LocalDateTime] = anyLocalDate.zipWith(anyLocalTime) { (date, time) =>
    LocalDateTime.of(date, time)
  }

  val anyZoneOffset: Gen[Any, ZoneOffset] =
    Gen.int(ZoneOffset.MIN.getTotalSeconds, ZoneOffset.MAX.getTotalSeconds).map(ZoneOffset.ofTotalSeconds)

  // This uses ZoneRulesProvider which has an effectful static initializer.
//  private val regionZoneIds =
//    ZIO.succeed(ZoneId.getAvailableZoneIds.asScala.toSet.map(ZoneId.of))
//
//  private val zoneOffsets =
//    (ZoneOffset.MIN.getTotalSeconds to ZoneOffset.MAX.getTotalSeconds).map(ZoneOffset.ofTotalSeconds)

//  private val zoneIds = regionZoneIds.map(_.toList ++ zoneOffsets)

  // FIXME: Shuffle is really slow.
  // private val zoneIds =
  //  for {
  //    ids      <- regionZoneIds
  //    all      = ids ++ zoneOffsets
  //    shuffled <- Random.shuffle(all.toList)
  //  } yield shuffled

  // FIXME Sampling causes some sort of pathological performance issue.
  val anyZoneId: Gen[Any, ZoneId] = Gen.const(ZoneId.systemDefault())
//    Gen(ZStream.fromIterableM(zoneIds).map {
//      case offset: ZoneOffset => Sample.noShrink(offset)
//      // FIXME: This is really slow even when it isn't shrinking.
//      //Sample.shrinkIntegral(ZoneOffset.UTC.getTotalSeconds)(offset.getTotalSeconds).map { seconds =>
//      //  ZoneOffset.ofTotalSeconds(seconds)
//      //}
//      case zone => Sample.noShrink(zone)
//    })

  // TODO: This needs to be double checked. I have encountered problems generating these in the past.
  //  See https://github.com/BotTech/scala-hedgehog-spines/blob/master/core/src/main/scala/com/lightbend/hedgehog/generators/time/TimeGenerators.scala
  val anyZonedDateTime: Gen[Any, ZonedDateTime] = anyLocalDateTime.zipWith(anyZoneId) { (dateTime, zone) =>
    ZonedDateTime.of(dateTime, zone)
  }

  val anyOffsetTime: Gen[Any, OffsetTime] = anyLocalTime.zipWith(anyZoneOffset) { (time, offset) =>
    OffsetTime.of(time, offset)
  }

  val anyOffsetDateTime: Gen[Any, OffsetDateTime] = anyLocalDateTime.zipWith(anyZoneOffset) { (dateTime, offset) =>
    OffsetDateTime.of(dateTime, offset)
  }
}
