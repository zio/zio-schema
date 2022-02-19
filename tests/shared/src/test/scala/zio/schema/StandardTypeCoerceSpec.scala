package zio.schema

import zio.test._
import zio.test.environment.TestEnvironment

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

object StandardTypeCoerceSpec extends DefaultRunnableSpec {

  def spec: ZSpec[TestEnvironment, Any] = suite("StandardTypeCoerceSpec")(
    suite("coerce")(
      suite("String")(
        test("-> Int") {
          val coercion = StandardType.StringType.coerce(StandardType.IntType)

          assertTrue(
            coercion.get("-1") == Right(-1),
            coercion.get("1") == Right(1),
            coercion.get("hello").isLeft,
            coercion.get("1.5").isLeft
          )
        },
        test("-> Long") {
          val coercion = StandardType.StringType.coerce(StandardType.LongType)

          assertTrue(
            coercion.get("-5") == Right(-5L),
            coercion.get("5") == Right(5L),
            coercion.get("999999999999") == Right(999_999_999_999L),
            coercion.get("hello").isLeft,
            coercion.get("1.5").isLeft
          )
        },
        test("-> Float") {
          val coercion = StandardType.StringType.coerce(StandardType.FloatType)

          assertTrue(
            coercion.get("1.5") == Right(1.5f),
            coercion.get("hello").isLeft
          )
        },
        test("-> Double") {
          val coercion = StandardType.StringType.coerce(StandardType.DoubleType)

          assertTrue(
            coercion.get("1.5") == Right(1.5),
            coercion.get("hello").isLeft
          )
        },
        test("-> Boolean") {
          val coercion = StandardType.StringType.coerce(StandardType.BoolType)

          assertTrue(
            coercion.get("true") == Right(true),
            coercion.get("false") == Right(false),
            coercion.get("hello").isLeft,
            coercion.get("1").isLeft
          )
        },
        test("-> LocalDate") {
          val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
          val coercion  = StandardType.StringType.coerce(StandardType.LocalDateType(formatter))

          assertTrue(
            coercion.get("2020-12-31") == Right(java.time.LocalDate.parse("2020-12-31", formatter)),
            coercion.get("hello").isLeft
          )
        },
        test("-> LocalTime") {
          val formatter = DateTimeFormatter.ofPattern("HH:mm:ss")
          val coercion  = StandardType.StringType.coerce(StandardType.LocalTimeType(formatter))

          assertTrue(
            coercion.get("12:34:56") == Right(java.time.LocalTime.parse("12:34:56", formatter)),
            coercion.get("hello").isLeft
          )
        },
        test("-> LocalDateTime") {
          val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
          val coercion  = StandardType.StringType.coerce(StandardType.LocalDateTimeType(formatter))

          assertTrue(
            coercion.get("2020-12-31 12:34:56") == Right(
              java.time.LocalDateTime.parse("2020-12-31 12:34:56", formatter)
            ),
            coercion.get("hello").isLeft
          )
        },
        test("-> ZonedDateTime") {
          val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss Z")
          val coercion  = StandardType.StringType.coerce(StandardType.ZonedDateTimeType(formatter))

          assertTrue(
            coercion.get("2020-12-31 12:34:56 +0100") == Right(
              java.time.ZonedDateTime.parse("2020-12-31 12:34:56 +0100", formatter)
            ),
            coercion.get("hello").isLeft
          )
        },
        test("-> Duration") {
          val temporalUnit = ChronoUnit.SECONDS
          val coercion     = StandardType.StringType.coerce(StandardType.Duration(temporalUnit))

          assertTrue(
            coercion.get("PT1S") == Right(java.time.Duration.of(1, temporalUnit)),
            coercion.get("hello").isLeft
          )
        },
        test("-> Period") {
          val coercion = StandardType.StringType.coerce(StandardType.PeriodType)

          assertTrue(
            coercion.get("P1Y2M3D") == Right(java.time.Period.of(1, 2, 3)),
            coercion.get("hello").isLeft
          )
        },
        test("-> UUID") {
          val coercion = StandardType.StringType.coerce(StandardType.UUIDType)

          assertTrue(
            coercion.get("00000000-0000-0000-0000-000000000000") == Right(
              java.util.UUID.fromString("00000000-0000-0000-0000-000000000000")
            ),
            coercion.get("hello").isLeft
          )
        }
      ),
      suite("Int")(
        test("-> String") {
          val coercion = StandardType.IntType.coerce(StandardType.StringType)

          assertTrue(
            coercion.get(1) == Right("1"),
            coercion.get(-1) == Right("-1"),
            coercion.get(0) == Right("0")
          )
        },
        test("-> Long") {
          val coercion = StandardType.IntType.coerce(StandardType.LongType)

          assertTrue(
            coercion.get(1) == Right(1L),
            coercion.get(-1) == Right(-1L),
            coercion.get(0) == Right(0L)
          )
        },
        test("-> Float") {
          val coercion = StandardType.IntType.coerce(StandardType.FloatType)

          assertTrue(
            coercion.get(1) == Right(1f),
            coercion.get(-1) == Right(-1f),
            coercion.get(0) == Right(0f)
          )
        },
        test("-> Double") {
          val coercion = StandardType.IntType.coerce(StandardType.DoubleType)

          assertTrue(
            coercion.get(1) == Right(1.0),
            coercion.get(-1) == Right(-1.0),
            coercion.get(0) == Right(0.0)
          )
        }
      ),
      suite("LocalDate")(
        test("-> String") {
          val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
          val coercion  = StandardType.LocalDateType(formatter).coerce(StandardType.StringType)

          assertTrue(
            coercion.get(java.time.LocalDate.parse("2020-12-31", formatter)) == Right("2020-12-31")
          )
        }
      ),
      suite("LocalTime")(
        test("-> String") {
          val formatter = DateTimeFormatter.ofPattern("HH:mm:ss")
          val coercion  = StandardType.LocalTimeType(formatter).coerce(StandardType.StringType)

          assertTrue(
            coercion.get(java.time.LocalTime.parse("12:34:56", formatter)) == Right("12:34:56")
          )
        }
      ),
      suite("LocalDateTime")(
        test("-> String") {
          val formatter     = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
          val coercion      = StandardType.LocalDateTimeType(formatter).coerce(StandardType.StringType)
          val localDateTime = java.time.LocalDateTime.parse("2020-12-31 12:34:56", formatter)

          assertTrue(
            coercion.get(localDateTime) == Right("2020-12-31 12:34:56")
          )
        }
      ),
      suite("ZonedDateTime")(
        test("-> String") {
          val formatter     = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss Z")
          val coercion      = StandardType.ZonedDateTimeType(formatter).coerce(StandardType.StringType)
          val zonedDateTime = java.time.ZonedDateTime.parse("2020-12-31 12:34:56 +0100", formatter)

          assertTrue(
            coercion.get(zonedDateTime) == Right("2020-12-31 12:34:56 +0100")
          )
        }
      ),
      suite("Duration")(
        test("-> String") {
          val temporalUnit = ChronoUnit.SECONDS
          val coercion     = StandardType.Duration(temporalUnit).coerce(StandardType.StringType)

          assertTrue(
            coercion.get(java.time.Duration.of(1, temporalUnit)) == Right("PT1S"),
            coercion.get(java.time.Duration.of(0, temporalUnit)) == Right("PT0S")
          )
        }
      ),
      suite("Instant")(
        test("-> Long") {
          val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss.SSS")
          val coercion  = StandardType.InstantType(formatter).coerce(StandardType.LongType)

          assertTrue(
            coercion.get(java.time.Instant.ofEpochMilli(1)) == Right(1L),
            coercion.get(java.time.Instant.ofEpochMilli(0)) == Right(0L)
          )
        }
      )
    )
  )
}
