package zio.schema

import java.time.format.DateTimeFormatter
import java.time.temporal.{ ChronoUnit }

import zio.random.Random
import zio.test.{ Gen, Sized }

object StandardTypeGen {

  val anyStandardType: Gen[Random, StandardType[_]] = Gen.oneOf(
    Gen.const(StandardType.UnitType),
    Gen.const(StandardType.StringType),
    Gen.const(StandardType.BoolType),
    Gen.const(StandardType.ShortType),
    Gen.const(StandardType.IntType),
    Gen.const(StandardType.LongType),
    Gen.const(StandardType.FloatType),
    Gen.const(StandardType.DoubleType),
    Gen.const(StandardType.BinaryType),
    Gen.const(StandardType.CharType),
    Gen.const(StandardType.DayOfWeekType),
    Gen.const(StandardType.Duration(ChronoUnit.SECONDS)),
    Gen.const(StandardType.Instant(DateTimeFormatter.ISO_DATE_TIME)),
    Gen.const(StandardType.LocalDate(DateTimeFormatter.ISO_DATE)),
    Gen.const(StandardType.LocalDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME)),
    Gen.const(StandardType.LocalTime(DateTimeFormatter.ISO_LOCAL_TIME)),
    Gen.const(StandardType.Month),
    Gen.const(StandardType.MonthDay),
    Gen.const(StandardType.OffsetDateTime(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
    Gen.const(StandardType.OffsetTime(DateTimeFormatter.ISO_OFFSET_TIME)),
    Gen.const(StandardType.Period),
    Gen.const(StandardType.Year),
//    Gen.const(StandardType.YearMonth),
    Gen.const(StandardType.ZonedDateTime(DateTimeFormatter.ISO_ZONED_DATE_TIME)),
    Gen.const(StandardType.ZoneId),
    Gen.const(StandardType.ZoneOffset)
  )

  type StandardTypeAndGen[A] = (StandardType[A], Gen[Random with Sized, A])

  val anyStandardTypeAndGen: Gen[Random, StandardTypeAndGen[_]] = {
    anyStandardType.map {
      case typ @ StandardType.UnitType          => typ -> Gen.unit: StandardTypeAndGen[_]
      case typ @ StandardType.StringType        => typ -> Gen.anyString
      case typ @ StandardType.BoolType          => typ -> Gen.boolean
      case typ @ StandardType.ShortType         => typ -> Gen.anyShort
      case typ @ StandardType.IntType           => typ -> Gen.anyInt
      case typ @ StandardType.LongType          => typ -> Gen.anyLong
      case typ @ StandardType.FloatType         => typ -> Gen.anyFloat
      case typ @ StandardType.DoubleType        => typ -> Gen.anyDouble
      case typ @ StandardType.BinaryType        => typ -> Gen.chunkOf(Gen.anyByte)
      case typ @ StandardType.CharType          => typ -> Gen.anyASCIIChar
      case typ @ StandardType.DayOfWeekType     => typ -> JavaTimeGen.anyDayOfWeek
      case typ @ StandardType.Duration(_)       => typ -> JavaTimeGen.anyDuration
      case typ @ StandardType.Instant(_)        => typ -> JavaTimeGen.anyInstant
      case typ @ StandardType.LocalDate(_)      => typ -> JavaTimeGen.anyLocalDate
      case typ @ StandardType.LocalDateTime(_)  => typ -> JavaTimeGen.anyLocalDateTime
      case typ @ StandardType.LocalTime(_)      => typ -> JavaTimeGen.anyLocalTime
      case typ @ StandardType.Month             => typ -> JavaTimeGen.anyMonth
      case typ @ StandardType.MonthDay          => typ -> JavaTimeGen.anyMonthDay
      case typ @ StandardType.OffsetDateTime(_) => typ -> JavaTimeGen.anyOffsetDateTime
      case typ @ StandardType.OffsetTime(_)     => typ -> JavaTimeGen.anyOffsetTime
      case typ @ StandardType.Period            => typ -> JavaTimeGen.anyPeriod
      case typ @ StandardType.Year              => typ -> JavaTimeGen.anyYear
      case typ @ StandardType.YearMonth         => typ -> JavaTimeGen.anyYearMonth
      case typ @ StandardType.ZonedDateTime(_)  => typ -> JavaTimeGen.anyZonedDateTime
      case typ @ StandardType.ZoneId            => typ -> JavaTimeGen.anyZoneId
      case typ @ StandardType.ZoneOffset        => typ -> JavaTimeGen.anyZoneOffset
      case _                                    => StandardType.UnitType -> Gen.unit: StandardTypeAndGen[_]
    }
  }
}
