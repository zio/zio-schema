package zio.schema

import zio._
import zio.random.Random
import zio.schema.Schema.Primitive
import zio.test._
import zio.test.Assertion._

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

object GenericSpec extends DefaultRunnableSpec {

  def spec =
    suite("GenericSpec")(
      suite("Primitives")(primitiveTests: _*),
      testM("round-trips Records") {
        check(SchemaGen.anyRecordOfRecordsAndValue) {
          case (schema, a) => assert(schema.fromGeneric(schema.toGeneric(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips Enumerations") {
        check(SchemaGen.anyEnumerationAndValue) {
          case (schema, a) =>
            assert(schema.fromGeneric(schema.toGeneric(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips Eithers") {
        check(SchemaGen.anyEitherAndValue) {
          case (schema, a) => assert(schema.fromGeneric(schema.toGeneric(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips Tuples") {
        check(SchemaGen.anyTupleAndValue) {
          case (schema, a) => assert(schema.fromGeneric(schema.toGeneric(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips Optionals") {
        check(SchemaGen.anyOptionalAndValue) {
          case (schema, a) => assert(schema.fromGeneric(schema.toGeneric(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips Transform") {
        check(SchemaGen.anyTransformAndValue) {
          case (schema, a) => assert(schema.fromGeneric(schema.toGeneric(a)))(isRight(equalTo(a)))
        }
      }
    )

  val primitiveTests = schemasAndGens.map {
    case SchemaTest(name, schema, gen) =>
      testM(s"round-trips $name") {
        genericLaw(gen, schema)
      }
  }

  private def genericLaw[R, A](gen: Gen[R, A], schema: Schema[A]): URIO[R with TestConfig, TestResult] =
    check(gen) { a =>
      assert(schema.fromGeneric(schema.toGeneric(a)))(isRight(equalTo(a)))
    }

  final private case class SchemaTest[A](name: String, schema: Schema[A], gen: Gen[Sized with Random, A])

  private def schemasAndGens: List[SchemaTest[_]] = List(
    SchemaTest("String", Primitive(StandardType.StringType), Gen.anyString),
    SchemaTest("Bool", Primitive(StandardType.BoolType), Gen.boolean),
    SchemaTest("Short", Primitive(StandardType.ShortType), Gen.anyShort),
    SchemaTest("Int", Primitive(StandardType.IntType), Gen.anyInt),
    SchemaTest("Long", Primitive(StandardType.LongType), Gen.anyLong),
    SchemaTest("Float", Primitive(StandardType.FloatType), Gen.anyFloat),
    SchemaTest("Double", Primitive(StandardType.DoubleType), Gen.anyDouble),
    SchemaTest("Binary", Primitive(StandardType.BinaryType), Gen.chunkOf(Gen.anyByte)),
    SchemaTest("Char", Primitive(StandardType.CharType), Gen.anyASCIIChar),
    SchemaTest(
      "BigDecimal",
      Primitive(StandardType.BigDecimalType),
      Gen.anyDouble.map(d => java.math.BigDecimal.valueOf(d))
    ),
    SchemaTest(
      "BigInteger",
      Primitive(StandardType.BigIntegerType),
      Gen.anyLong.map(n => java.math.BigInteger.valueOf(n))
    ),
    SchemaTest("DayOfWeek", Primitive(StandardType.DayOfWeekType), JavaTimeGen.anyDayOfWeek),
    SchemaTest("Duration", Primitive(StandardType.Duration(ChronoUnit.SECONDS)), JavaTimeGen.anyDuration),
    SchemaTest("Instant", Primitive(StandardType.Instant(DateTimeFormatter.ISO_DATE_TIME)), JavaTimeGen.anyInstant),
    SchemaTest("LocalDate", Primitive(StandardType.LocalDate(DateTimeFormatter.ISO_DATE)), JavaTimeGen.anyLocalDate),
    SchemaTest(
      "LocalDateTime",
      Primitive(StandardType.LocalDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME)),
      JavaTimeGen.anyLocalDateTime
    ),
    SchemaTest(
      "LocalTime",
      Primitive(StandardType.LocalTime(DateTimeFormatter.ISO_LOCAL_TIME)),
      JavaTimeGen.anyLocalTime
    ),
    SchemaTest("Month", Primitive(StandardType.Month), JavaTimeGen.anyMonth),
    SchemaTest("MonthDay", Primitive(StandardType.MonthDay), JavaTimeGen.anyMonthDay),
    SchemaTest(
      "OffsetDateTime",
      Primitive(StandardType.OffsetDateTime(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
      JavaTimeGen.anyOffsetDateTime
    ),
    SchemaTest(
      "OffsetTime",
      Primitive(StandardType.OffsetTime(DateTimeFormatter.ISO_OFFSET_TIME)),
      JavaTimeGen.anyOffsetTime
    ),
    SchemaTest("Period", Primitive(StandardType.Period), JavaTimeGen.anyPeriod),
    SchemaTest("Year", Primitive(StandardType.Year), JavaTimeGen.anyYear),
    SchemaTest("YearMonth", Primitive(StandardType.YearMonth), JavaTimeGen.anyYearMonth),
    SchemaTest(
      "ZonedDateTime",
      Primitive(StandardType.ZonedDateTime(DateTimeFormatter.ISO_ZONED_DATE_TIME)),
      JavaTimeGen.anyZonedDateTime
    ),
    SchemaTest("ZoneId", Primitive(StandardType.ZoneId), JavaTimeGen.anyZoneId),
    SchemaTest("ZoneOffset", Primitive(StandardType.ZoneOffset), JavaTimeGen.anyZoneOffset),
    SchemaTest("UnitType", Primitive(StandardType.UnitType), Gen.unit)
  )

}
