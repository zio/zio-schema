package zio.schema

import java.time._
import java.time.temporal.ChronoField
import scala.jdk.CollectionConverters._
import zio.ZIO
import zio.random.Random
import zio.stream.ZStream
import zio.test.{ Gen, Sample }

object JavaTimeGen {

  val anyDayOfWeek: Gen[Random, DayOfWeek] = Gen.oneOf(
    Gen.const(DayOfWeek.MONDAY),
    Gen.const(DayOfWeek.TUESDAY),
    Gen.const(DayOfWeek.WEDNESDAY),
    Gen.const(DayOfWeek.THURSDAY),
    Gen.const(DayOfWeek.FRIDAY),
    Gen.const(DayOfWeek.SATURDAY),
    Gen.const(DayOfWeek.SUNDAY)
  )

  val anyMonth: Gen[Random, Month] = Gen.oneOf(
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

  val anyNanoOfDay: Gen[Random, Long] = chronoFieldValue(ChronoField.NANO_OF_DAY)

  val anyEpochDay: Gen[Random, Long] = chronoFieldValue(ChronoField.EPOCH_DAY)

  val anyMonthOfYear: Gen[Random, Int] = chronoFieldValue(ChronoField.MONTH_OF_YEAR).map(_.toInt)

  val anyMonthDay: Gen[Random, MonthDay] =
    for {
      month      <- anyMonth
      dayOfMonth <- Gen.int(1, month.maxLength)
    } yield MonthDay.of(month, dayOfMonth)

  val anyIntYear: Gen[Random, Int] = chronoFieldValue(ChronoField.YEAR).map(_.toInt)

  val anyYear: Gen[Random, Year] = anyIntYear.map(Year.of)

  val anyYearMonth: Gen[Random, YearMonth] =
    anyIntYear.zipWith(anyMonthOfYear) { (year, month) =>
      YearMonth.of(year, month)
    }

  private def chronoFieldValue(chronoField: ChronoField) = {
    val range = chronoField.range
    Gen.long(range.getMinimum, range.getMaximum)
  }

  val anyDuration: Gen[Random, Duration] = Gen.anyLong.zipWith(Gen.long(0, 999_999_999L)) { (seconds, nanos) =>
    Duration.ofSeconds(seconds, nanos)
  }

  val anyPeriod: Gen[Random, Period] =
    for {
      years  <- Gen.anyInt
      months <- Gen.anyInt
      days   <- Gen.anyInt
    } yield Period.of(years, months, days)

  val anyInstant: Gen[Random, Instant] = Gen
    .long(Instant.MIN.getEpochSecond, Instant.MAX.getEpochSecond)
    .zipWith(Gen.int(Instant.MIN.getNano, Instant.MAX.getNano)) { (seconds, nanos) =>
      Instant.ofEpochSecond(seconds, nanos.toLong)
    }

  val anyLocalDate: Gen[Random, LocalDate] = anyEpochDay.map(LocalDate.ofEpochDay)

  val anyLocalTime: Gen[Random, LocalTime] = anyNanoOfDay.map(LocalTime.ofNanoOfDay)

  val anyLocalDateTime: Gen[Random, LocalDateTime] = anyLocalDate.zipWith(anyLocalTime) { (date, time) =>
    LocalDateTime.of(date, time)
  }

  val anyZoneOffset: Gen[Random, ZoneOffset] =
    Gen.int(ZoneOffset.MIN.getTotalSeconds, ZoneOffset.MAX.getTotalSeconds).map(ZoneOffset.ofTotalSeconds)

  // This uses ZoneRulesProvider which has an effectful static initializer.
  private val regionZoneIds =
    ZIO.succeed(ZoneId.getAvailableZoneIds.asScala.toSet.map(ZoneId.of))

  private val zoneOffsets =
    (ZoneOffset.MIN.getTotalSeconds to ZoneOffset.MAX.getTotalSeconds).map(ZoneOffset.ofTotalSeconds)

  private val zoneIds = regionZoneIds.map(_.toList ++ zoneOffsets)

  // FIXME: Shuffle is really slow.
  //private val zoneIds =
  //  for {
  //    ids      <- regionZoneIds
  //    all      = ids ++ zoneOffsets
  //    random   <- ZIO.service[Random.Service]
  //    shuffled <- random.shuffle(all.toList)
  //  } yield shuffled

  val anyZoneId: Gen[Random, ZoneId] =
    Gen(ZStream.fromIterableM(zoneIds).map {
      case offset: ZoneOffset => Sample.noShrink(offset)
      // FIXME: This is really slow even when it isn't shrinking.
      //Sample.shrinkIntegral(ZoneOffset.UTC.getTotalSeconds)(offset.getTotalSeconds).map { seconds =>
      //  ZoneOffset.ofTotalSeconds(seconds)
      //}
      case zone => Sample.noShrink(zone)
    })

  // TODO: This needs to be double checked. I have encountered problems generating these in the past.
  //  See https://github.com/BotTech/scala-hedgehog-spines/blob/master/core/src/main/scala/com/lightbend/hedgehog/generators/time/TimeGenerators.scala
  val anyZonedDateTime: Gen[Random, ZonedDateTime] = anyLocalDateTime.zipWith(anyZoneId) { (dateTime, zone) =>
    ZonedDateTime.of(dateTime, zone)
  }

  val anyOffsetTime: Gen[Random, OffsetTime] = anyLocalTime.zipWith(anyZoneOffset) { (time, offset) =>
    OffsetTime.of(time, offset)
  }

  val anyOffsetDateTime: Gen[Random, OffsetDateTime] = anyLocalDateTime.zipWith(anyZoneOffset) { (dateTime, offset) =>
    OffsetDateTime.of(dateTime, offset)
  }
}
