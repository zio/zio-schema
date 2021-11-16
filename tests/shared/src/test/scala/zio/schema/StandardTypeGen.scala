package zio.schema

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import zio.random.Random
import zio.test.{ Gen, Sized }

object StandardTypeGen {

  val anyStandardType: Gen[Random, StandardType[_]] = Gen.fromIterable(
    List(
      (StandardType.StringType),
      (StandardType.BoolType),
      (StandardType.ShortType),
      (StandardType.IntType),
      (StandardType.LongType),
      (StandardType.FloatType),
      (StandardType.DoubleType),
      (StandardType.BinaryType),
      (StandardType.BigDecimalType),
      (StandardType.BigIntegerType),
      (StandardType.CharType),
      (StandardType.UUIDType),
      (StandardType.DayOfWeekType),
      (StandardType.Duration(ChronoUnit.SECONDS)),
      (StandardType.Instant(DateTimeFormatter.ISO_DATE_TIME)),
      (StandardType.LocalDate(DateTimeFormatter.ISO_DATE)),
      (StandardType.LocalDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME)),
      (StandardType.LocalTime(DateTimeFormatter.ISO_LOCAL_TIME)),
      (StandardType.Month),
      (StandardType.MonthDay),
      (StandardType.OffsetDateTime(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
      (StandardType.OffsetTime(DateTimeFormatter.ISO_OFFSET_TIME)),
      (StandardType.Period),
      (StandardType.Year),
      (StandardType.YearMonth),
      (StandardType.ZonedDateTime(DateTimeFormatter.ISO_ZONED_DATE_TIME)),
      (StandardType.ZoneId)
    )
    //FIXME For some reason adding this causes other unrelated tests to break.
//    Gen.const(StandardType.ZoneOffset)
  )

  type StandardTypeAndGen[A] = (StandardType[A], Gen[Random with Sized, A])

  val anyStandardTypeAndGen: Gen[Random, StandardTypeAndGen[_]] = {
    anyStandardType.map {
      case typ: StandardType.StringType.type     => typ -> Gen.anyString
      case typ: StandardType.BoolType.type       => typ -> Gen.boolean
      case typ: StandardType.ShortType.type      => typ -> Gen.anyShort
      case typ: StandardType.IntType.type        => typ -> Gen.anyInt
      case typ: StandardType.LongType.type       => typ -> Gen.anyLong
      case typ: StandardType.FloatType.type      => typ -> Gen.anyFloat
      case typ: StandardType.DoubleType.type     => typ -> Gen.anyDouble
      case typ: StandardType.BinaryType.type     => typ -> Gen.chunkOf(Gen.anyByte)
      case typ: StandardType.CharType.type       => typ -> Gen.anyASCIIChar
      case typ: StandardType.UUIDType.type       => typ -> Gen.anyUUID
      case typ: StandardType.BigDecimalType.type => typ -> Gen.anyDouble.map(d => java.math.BigDecimal.valueOf(d))
      case typ: StandardType.BigIntegerType.type => typ -> Gen.anyLong.map(n => java.math.BigInteger.valueOf(n))
      case typ: StandardType.DayOfWeekType.type  => typ -> JavaTimeGen.anyDayOfWeek
      case typ: StandardType.Duration            => typ -> JavaTimeGen.anyDuration
      case typ: StandardType.Instant             => typ -> JavaTimeGen.anyInstant
      case typ: StandardType.LocalDate           => typ -> JavaTimeGen.anyLocalDate
      case typ: StandardType.LocalDateTime       => typ -> JavaTimeGen.anyLocalDateTime
      case typ: StandardType.LocalTime           => typ -> JavaTimeGen.anyLocalTime
      case typ: StandardType.Month.type          => typ -> JavaTimeGen.anyMonth
      case typ: StandardType.MonthDay.type       => typ -> JavaTimeGen.anyMonthDay
      case typ: StandardType.OffsetDateTime      => typ -> JavaTimeGen.anyOffsetDateTime
      case typ: StandardType.OffsetTime          => typ -> JavaTimeGen.anyOffsetTime
      case typ: StandardType.Period.type         => typ -> JavaTimeGen.anyPeriod
      case typ: StandardType.Year.type           => typ -> JavaTimeGen.anyYear
      case typ: StandardType.YearMonth.type      => typ -> JavaTimeGen.anyYearMonth
      case typ: StandardType.ZonedDateTime       => typ -> JavaTimeGen.anyZonedDateTime
      case typ: StandardType.ZoneId.type         => typ -> JavaTimeGen.anyZoneId
      case typ: StandardType.ZoneOffset.type     => typ -> JavaTimeGen.anyZoneOffset
      case _                                     => StandardType.UnitType -> Gen.unit: StandardTypeAndGen[_]
    }
  }
}
