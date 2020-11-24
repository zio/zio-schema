package zio.schema

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
    Gen.const(StandardType.ByteType),
    Gen.const(StandardType.CharType),
    Gen.const(StandardType.DayOfWeekType),
    Gen.const(StandardType.DurationType),
    Gen.const(StandardType.InstantType),
    Gen.const(StandardType.LocalDateType),
    Gen.const(StandardType.LocalDateTimeType),
    Gen.const(StandardType.LocalTimeType),
    Gen.const(StandardType.MonthType),
    Gen.const(StandardType.MonthDayType),
    Gen.const(StandardType.OffsetDateTimeType),
    Gen.const(StandardType.OffsetTimeType),
    Gen.const(StandardType.PeriodType),
    Gen.const(StandardType.YearType),
    Gen.const(StandardType.YearMonthType),
    Gen.const(StandardType.ZonedDateTimeType),
    Gen.const(StandardType.ZoneIdType),
    Gen.const(StandardType.ZoneOffsetType)
  )

  type StandardTypeAndGen[A] = (StandardType[A], Gen[Random with Sized, A])

  val anyStandardTypeAndGen: Gen[Random, StandardTypeAndGen[_]] = {
    anyStandardType.map {
      case typ @ StandardType.UnitType           => typ -> Gen.unit: StandardTypeAndGen[_]
      case typ @ StandardType.StringType         => typ -> Gen.anyString
      case typ @ StandardType.BoolType           => typ -> Gen.boolean
      case typ @ StandardType.ShortType          => typ -> Gen.anyShort
      case typ @ StandardType.IntType            => typ -> Gen.anyInt
      case typ @ StandardType.LongType           => typ -> Gen.anyLong
      case typ @ StandardType.FloatType          => typ -> Gen.anyFloat
      case typ @ StandardType.DoubleType         => typ -> Gen.anyDouble
      case typ @ StandardType.ByteType           => typ -> Gen.anyByte
      case typ @ StandardType.CharType           => typ -> Gen.anyChar
      case typ @ StandardType.DayOfWeekType      => typ -> JavaTimeGen.anyDayOfWeek
      case typ @ StandardType.DurationType       => typ -> JavaTimeGen.anyDuration
      case typ @ StandardType.InstantType        => typ -> JavaTimeGen.anyInstant
      case typ @ StandardType.LocalDateType      => typ -> JavaTimeGen.anyLocalDate
      case typ @ StandardType.LocalDateTimeType  => typ -> JavaTimeGen.anyLocalDateTime
      case typ @ StandardType.LocalTimeType      => typ -> JavaTimeGen.anyLocalTime
      case typ @ StandardType.MonthType          => typ -> JavaTimeGen.anyMonth
      case typ @ StandardType.MonthDayType       => typ -> JavaTimeGen.anyMonthDay
      case typ @ StandardType.OffsetDateTimeType => typ -> JavaTimeGen.anyOffsetDateTime
      case typ @ StandardType.OffsetTimeType     => typ -> JavaTimeGen.anyOffsetTime
      case typ @ StandardType.PeriodType         => typ -> JavaTimeGen.anyPeriod
      case typ @ StandardType.YearType           => typ -> JavaTimeGen.anyYear
      case typ @ StandardType.YearMonthType      => typ -> JavaTimeGen.anyYearMonth
      case typ @ StandardType.ZonedDateTimeType  => typ -> JavaTimeGen.anyZonedDateTime
      case typ @ StandardType.ZoneIdType         => typ -> JavaTimeGen.anyZoneId
      case typ @ StandardType.ZoneOffsetType     => typ -> JavaTimeGen.anyZoneOffset
    }
  }
}
