package zio.schema

import zio._
import zio.schema.DeriveSchema.gen
import zio.schema.Schema.Primitive
import zio.test.Assertion._
import zio.test._

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

object DefaultValueSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("Default Value Spec")(
    suite("Primitives")(
      test("UnitType default value") {
        assert(Primitive(StandardType.UnitType).defaultValue)(isRight(equalTo(())))
      },
      test("StringType default value") {
        assert(Primitive(StandardType.StringType).defaultValue)(isRight(equalTo("")))
      },
      test("BoolType default value") {
        assert(Primitive(StandardType.BoolType).defaultValue)(isRight(equalTo(true)))
      },
      test("ShortType default value") {
        assert(Primitive(StandardType.ShortType).defaultValue)(isRight(equalTo(0.asInstanceOf[Short])))
      },
      test("IntType default value") {
        assert(Primitive(StandardType.IntType).defaultValue)(isRight(equalTo(0)))
      },
      test("LongType default value") {
        assert(Primitive(StandardType.LongType).defaultValue)(isRight(equalTo(0.asInstanceOf[Long])))
      },
      test("FloatType default value") {
        assert(Primitive(StandardType.FloatType).defaultValue)(isRight(equalTo(0.0.asInstanceOf[Float])))
      },
      test("DoubleType default value") {
        assert(Primitive(StandardType.DoubleType).defaultValue)(isRight(equalTo(0.0)))
      },
      test("BinaryType default value") {
        assert(Primitive(StandardType.BinaryType).defaultValue)(isRight(equalTo(Chunk.empty)))
      },
      test("CharType default value") {
        assert(Primitive(StandardType.CharType).defaultValue)(isRight(equalTo('\u0000')))
      },
      test("BigDecimalType default value") {
        assert(Primitive(StandardType.BigDecimalType).defaultValue)(isRight(equalTo(java.math.BigDecimal.ZERO)))
      },
      test("BigIntegerType default value") {
        assert(Primitive(StandardType.BigIntegerType).defaultValue)(isRight(equalTo(java.math.BigInteger.ZERO)))
      },
      test("DayOfWeekType default value") {
        assert(Primitive(StandardType.DayOfWeekType).defaultValue)(
          isRight(equalTo(java.time.temporal.WeekFields.of(java.util.Locale.getDefault).getFirstDayOfWeek))
        )
      },
      test("Month default value") {
        assert(Primitive(StandardType.Month).defaultValue)(isRight(equalTo(java.time.Month.JANUARY)))
      },
      test("MonthDay default value") {
        assert(Primitive(StandardType.MonthDay).defaultValue)(
          isRight(equalTo(java.time.MonthDay.of(java.time.Month.JANUARY, 1)))
        )
      },
      test("Period default value") {
        assert(Primitive(StandardType.Period).defaultValue)(isRight(equalTo(java.time.Period.ZERO)))
      },
      test("Year default value") {
        assert(Primitive(StandardType.Year).defaultValue)(
          isRight(equalTo(java.time.Year.now))
        )
      },
      test("YearMonth default value") {
        assert(Primitive(StandardType.YearMonth).defaultValue)(isRight(equalTo(java.time.YearMonth.now)))
      },
      test("ZoneId default value") {
        assert(Primitive(StandardType.ZoneId).defaultValue)(isRight(equalTo(java.time.ZoneId.systemDefault)))
      },
      test("ZoneOffset default value") {
        assert(Primitive(StandardType.ZoneOffset).defaultValue)(isRight(equalTo(java.time.ZoneOffset.UTC)))
      },
      test("Duration default value") {
        assert(Primitive(StandardType.Duration(ChronoUnit.SECONDS)).defaultValue)(
          isRight(equalTo(java.time.Duration.ZERO))
        )
      },
      test("Instant default value") {
        assert(Primitive(StandardType.Instant(DateTimeFormatter.ISO_INSTANT)).defaultValue)(
          isRight(equalTo(java.time.Instant.EPOCH))
        )
      },
      test("LocalDate default value") {
        assert(Primitive(StandardType.LocalDate(DateTimeFormatter.ISO_LOCAL_DATE)).defaultValue)(
          isRight(isSubtype[java.time.LocalDate](anything))
        )
      },
      test("LocalTime default value") {
        assert(Primitive(StandardType.LocalTime(DateTimeFormatter.ISO_LOCAL_TIME)).defaultValue)(
          isRight(equalTo(java.time.LocalTime.MIDNIGHT))
        )
      },
      test("LocalDateTime default value") {
        assert(Primitive(StandardType.LocalDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME)).defaultValue)(
          isRight(isSubtype[java.time.LocalDateTime](anything))
        )
      },
      test("OffsetTime default value") {
        assert(Primitive(StandardType.OffsetTime(DateTimeFormatter.ISO_OFFSET_TIME)).defaultValue)(
          isRight(isSubtype[java.time.OffsetTime](anything))
        )
      },
      test("OffsetDateTime default value") {
        assert(Primitive(StandardType.OffsetDateTime(DateTimeFormatter.ISO_OFFSET_DATE_TIME)).defaultValue)(
          isRight(isSubtype[java.time.OffsetDateTime](anything))
        )
      },
      test("ZonedDateTime default value") {
        assert(Primitive(StandardType.ZonedDateTime(DateTimeFormatter.ISO_ZONED_DATE_TIME)).defaultValue)(
          isRight(isSubtype[java.time.ZonedDateTime](anything))
        )
      }
    ),
    suite("Records")(
      test("un-nested") {
        sealed case class UserId(id: String)

        val expected: Schema[UserId] =
          Schema.CaseClass1(
            annotations = Chunk.empty,
            field = Schema.Field("id", Schema.Primitive(StandardType.StringType)),
            UserId.apply,
            (uid: UserId) => uid.id
          )

        assert(expected.defaultValue)(isRight(equalTo(UserId(""))))
      },
      test("nested") {
        sealed case class UserId(id: String)
        sealed case class User(name: String, id: UserId)

        val expected: Schema[User] =
          Schema.CaseClass2(
            annotations = Chunk.empty,
            field1 = Schema.Field("name", Schema.Primitive(StandardType.StringType)),
            field2 = Schema.Field(
              "id",
              Schema.CaseClass1(
                annotations = Chunk.empty,
                field = Schema.Field("id", Schema.Primitive(StandardType.StringType)),
                UserId.apply,
                (uid: UserId) => uid.id
              )
            ),
            User.apply,
            (u: User) => u.name,
            (u: User) => u.id
          )

        assert(expected.defaultValue)(isRight(equalTo(User("", UserId("")))))
      }
    ),
    suite("Enum")(
      test("basic") {
        sealed trait Status
        case class Ok(response: List[String]) extends Status
        case class Failed(code: Int, reason: String, additionalExplanation: Option[String], remark: String = "oops")
            extends Status
        case object Pending extends Status

        val expected: Schema[Status] =
          Schema.Enum3(
            Schema.Case("Failed", Schema[Failed], (s: Status) => s.asInstanceOf[Failed]),
            Schema.Case("Ok", Schema[Ok], (s: Status) => s.asInstanceOf[Ok]),
            Schema.Case(
              "Pending",
              Schema[Pending.type],
              (s: Status) => s.asInstanceOf[Pending.type]
            )
          )

        assert(expected.defaultValue)(isRight(equalTo(Failed(0, "", None, ""))))
      }
    ),
    suite("EitherSchema")(
      test("left") {
        val leftSchema = Schema.left(Schema.primitive(StandardType.IntType))
        assert(leftSchema.defaultValue)(isRight(isLeft(equalTo(0))))
      },
      test("right") {
        val rightSchema = Schema.right(Schema.primitive(StandardType.StringType))
        assert(rightSchema.defaultValue)(isRight(isRight(equalTo(""))))
      }
    )
  )
}
