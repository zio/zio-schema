package zio.schema

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import zio.schema.GenUtil._
import zio.test._

object PrimitiveGenSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[Environment, Failure] =
    suite("PrimitiveGenSpec")(
      testM("derive a generator of Binary values") {
        checkGenValue(Schema.primitive(StandardType.BinaryType))
      },
      testM("derive a generator of Boolean values") {
        checkGenValue(Schema.primitive(StandardType.BoolType))
      },
      testM("derive a generator of Char values") {
        checkGenValue(Schema.primitive(StandardType.CharType))
      },
      testM("derive a generator of Double values") {
        checkGenValue(Schema.primitive(StandardType.DoubleType))
      },
      testM("derive a generator of Float values") {
        checkGenValue(Schema.primitive(StandardType.FloatType))
      },
      testM("derive a generator of Int values") {
        checkGenValue(Schema.primitive(StandardType.IntType))
      },
      testM("dderive a generator of Long values") {
        checkGenValue(Schema.primitive(StandardType.LongType))
      },
      testM("derive a generator of Short values") {
        checkGenValue(Schema.primitive(StandardType.ShortType))
      },
      testM("derive a generator of String values") {
        checkGenValue(Schema.primitive(StandardType.StringType))
      },
      testM("derive a generator of Unit values") {
        checkGenValue(Schema.primitive(StandardType.UnitType))
      },
      testM("derive a generator of java.math.BigDecimal values") {
        checkGenValue(Schema.primitive(StandardType.BigDecimalType))
      },
      testM("derive a generator of java.math.BigInteger values") {
        checkGenValue(Schema.primitive(StandardType.BigIntegerType))
      },
      testM("derive a generator of java.time.DayOfWeek values") {
        checkGenValue(Schema.primitive(StandardType.DayOfWeekType))
      },
      testM("derive a generator of java.time.Duration values") {
        checkGenValue(Schema.primitive(StandardType.Duration(ChronoUnit.SECONDS)))
      },
      testM("derive a generator of java.time.Instant values") {
        checkGenValue(Schema.primitive(StandardType.Instant(DateTimeFormatter.ISO_INSTANT)))
      },
      testM("derive a generator of java.time.LocalDate values") {
        checkGenValue(Schema.primitive(StandardType.LocalDate(DateTimeFormatter.ISO_LOCAL_DATE)))
      },
      testM("derive a generator of java.time.LocalDateTime values") {
        checkGenValue(Schema.primitive(StandardType.LocalDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME)))
      },
      testM("derive a generator of java.time.LocalTime values") {
        checkGenValue(Schema.primitive(StandardType.LocalTime(DateTimeFormatter.ISO_LOCAL_TIME)))
      },
      testM("derive a generator of java.time.Month values") {
        checkGenValue(Schema.primitive(StandardType.Month))
      },
      testM("derive a generator of java.time.MonthDay values") {
        checkGenValue(Schema.primitive(StandardType.MonthDay))
      },
      testM("derive a generator of java.time.OffsetDate values") {
        checkGenValue(Schema.primitive(StandardType.OffsetDateTime(DateTimeFormatter.ISO_OFFSET_DATE_TIME)))
      },
      testM("derive a generator of java.time.OffsetTime values") {
        checkGenValue(Schema.primitive(StandardType.OffsetTime(DateTimeFormatter.ISO_OFFSET_TIME)))
      },
      testM("derive a generator of java.time.Period values") {
        checkGenValue(Schema.primitive(StandardType.Period))
      },
      testM("derive a generator of java.time.Year values") {
        checkGenValue(Schema.primitive(StandardType.Year))
      },
      testM("derive a generator of java.time.YearMonth values") {
        checkGenValue(Schema.primitive(StandardType.YearMonth))
      },
      testM("derive a generator of java.time.ZoneId values") {
        checkGenValue(Schema.primitive(StandardType.ZoneId))
      },
      testM("derive a generator of java.time.ZoneOffset") {
        checkGenValue(Schema.primitive(StandardType.ZoneOffset))
      },
      testM("derive a generator of java.time.ZonedDateTime values") {
        checkGenValue(Schema.primitive(StandardType.ZonedDateTime(DateTimeFormatter.ISO_ZONED_DATE_TIME)))
      }
    )
}
