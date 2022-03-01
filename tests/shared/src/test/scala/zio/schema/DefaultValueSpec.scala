package zio.schema

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import zio.Chunk
import zio.schema.CaseSet.caseOf
import zio.schema.Schema.{ Lazy, Primitive }
import zio.test.Assertion._
import zio.test.{ DefaultRunnableSpec, ZSpec, assert }

object DefaultValueSpec extends DefaultRunnableSpec {
  // Record Tests
  sealed case class UserId(id: String)
  sealed case class User(id: UserId, name: String, age: Int)

  // Enum Tests
  sealed trait Status
  case class Ok(response: List[String]) extends Status

  object Ok {
    implicit lazy val schema: Schema[Ok] = DeriveSchema.gen[Ok]
  }
  case class Failed(code: Int, reason: String, additionalExplanation: Option[String], remark: String = "oops")
      extends Status

  object Failed {
    implicit lazy val schema: Schema[Failed] = DeriveSchema.gen[Failed]
  }
  case object Pending extends Status

  object Status {
    implicit lazy val schema: Schema[Status] = DeriveSchema.gen[Status]
  }

  def spec: ZSpec[Environment, Failure] = suite("Default Value Spec")(
    suite("Primitive")(
      test("UnitType default value") {
        assert(Primitive(StandardType.UnitType).defaultValue)(isRight(equalTo(())))
      },
      test("StringType default value") {
        assert(Primitive(StandardType.StringType).defaultValue)(isRight(equalTo("")))
      },
      test("BoolType default value") {
        assert(Primitive(StandardType.BoolType).defaultValue)(isRight(equalTo(false)))
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
        assert(Primitive(StandardType.MonthType).defaultValue)(isRight(equalTo(java.time.Month.JANUARY)))
      },
      test("MonthDay default value") {
        assert(Primitive(StandardType.MonthDayType).defaultValue)(
          isRight(equalTo(java.time.MonthDay.of(java.time.Month.JANUARY, 1)))
        )
      },
      test("Period default value") {
        assert(Primitive(StandardType.PeriodType).defaultValue)(isRight(equalTo(java.time.Period.ZERO)))
      },
      test("Year default value") {
        assert(Primitive(StandardType.YearType).defaultValue)(
          isRight(equalTo(java.time.Year.now))
        )
      },
      test("YearMonth default value") {
        assert(Primitive(StandardType.YearMonthType).defaultValue)(isRight(equalTo(java.time.YearMonth.now)))
      },
      test("ZoneId default value") {
        assert(Primitive(StandardType.ZoneIdType).defaultValue)(isRight(equalTo(java.time.ZoneId.systemDefault)))
      },
      test("ZoneOffset default value") {
        assert(Primitive(StandardType.ZoneOffsetType).defaultValue)(isRight(equalTo(java.time.ZoneOffset.UTC)))
      },
      test("Duration default value") {
        assert(Primitive(StandardType.Duration(ChronoUnit.SECONDS)).defaultValue)(
          isRight(equalTo(java.time.Duration.ZERO))
        )
      },
      test("Instant default value") {
        assert(Primitive(StandardType.InstantType(DateTimeFormatter.ISO_INSTANT)).defaultValue)(
          isRight(equalTo(java.time.Instant.EPOCH))
        )
      },
      test("LocalDate default value") {
        assert(Primitive(StandardType.LocalDateType(DateTimeFormatter.ISO_LOCAL_DATE)).defaultValue)(
          isRight(isSubtype[java.time.LocalDate](anything))
        )
      },
      test("LocalTime default value") {
        assert(Primitive(StandardType.LocalTimeType(DateTimeFormatter.ISO_LOCAL_TIME)).defaultValue)(
          isRight(equalTo(java.time.LocalTime.MIDNIGHT))
        )
      },
      test("LocalDateTime default value") {
        assert(Primitive(StandardType.LocalDateTimeType(DateTimeFormatter.ISO_LOCAL_DATE_TIME)).defaultValue)(
          isRight(isSubtype[java.time.LocalDateTime](anything))
        )
      },
      test("OffsetTime default value") {
        assert(Primitive(StandardType.OffsetTimeType(DateTimeFormatter.ISO_OFFSET_TIME)).defaultValue)(
          isRight(isSubtype[java.time.OffsetTime](anything))
        )
      },
      test("OffsetDateTime default value") {
        assert(Primitive(StandardType.OffsetDateTimeType(DateTimeFormatter.ISO_OFFSET_DATE_TIME)).defaultValue)(
          isRight(isSubtype[java.time.OffsetDateTime](anything))
        )
      },
      test("ZonedDateTime default value") {
        assert(Primitive(StandardType.ZonedDateTimeType(DateTimeFormatter.ISO_ZONED_DATE_TIME)).defaultValue)(
          isRight(isSubtype[java.time.ZonedDateTime](anything))
        )
      }
    ),
    suite("Record")(
      test("basic") {
        val schema: Schema[UserId] =
          Schema.CaseClass1(
            field = Schema.Field("id", Schema.Primitive(StandardType.StringType)),
            UserId.apply,
            (uid: UserId) => uid.id
          )
        assert(schema.defaultValue)(isRight(equalTo(UserId(""))))
      },
      test("recursive") {
        val expected: Schema[User] =
          Schema.CaseClass3(
            field1 = Schema.Field(
              "id",
              Schema.CaseClass1(
                field = Schema.Field("id", Schema.Primitive(StandardType.StringType)),
                UserId.apply,
                (uid: UserId) => uid.id
              )
            ),
            field2 = Schema.Field("name", Schema.Primitive(StandardType.StringType)),
            field3 = Schema.Field("age", Schema.Primitive(StandardType.IntType)),
            User.apply,
            (u: User) => u.id,
            (u: User) => u.name,
            (u: User) => u.age
          )
        assert(expected.defaultValue)(isRight(equalTo(User(UserId(""), "", 0))))
      }
    ),
    suite("Sequence")(
      test("chunk") {
        assert(Schema.chunk[Int].defaultValue)(isRight(equalTo(Chunk(0))))
      },
      test("list") {
        assert(Schema.list[Int].defaultValue)(isRight(equalTo(List(0))))
      }
    ),
    suite("Enumeration")(
      test("defaults to first case") {
        val schema = Schema.enumeration[Any, CaseSet.Aux[Any]](
          caseOf[Int, Any]("myInt")(_.asInstanceOf[Int]) ++ caseOf[String, Any]("myString")(_.asInstanceOf[String])
        )
        assert(schema.defaultValue)(isRight(equalTo(0)))
      }
    ),
    suite("Transform")(
      test("returns transformed default value") {
        val schema: Schema[String] = Schema.primitive(StandardType.IntType).transform[String](_.toString, _.toInt)
        assert(schema.defaultValue)(isRight(equalTo("0")))
      }
    ),
    suite("Optional")(
      test("defaults to None") {
        val schema: Schema[Option[Int]] = Schema.option[Int]
        assert(schema.defaultValue)(isRight(isNone))
      }
    ),
    suite("Fail")(
      test("defaults to the error message") {
        val schema: Schema[Nothing] = Schema.fail("failing")
        assert(schema.defaultValue)(isLeft(equalTo("failing")))
      }
    ),
    suite("Tuple")(
      test("defaults to default value of tuple members") {
        val schema: Schema[(Int, String)] =
          Schema.tuple2(Schema.primitive(StandardType.IntType), Schema.primitive(StandardType.StringType))
        assert(schema.defaultValue)(isRight(equalTo((0, ""))))
      }
    ),
    suite("Lazy")(
      test("calls the schema thunk") {
        val schema: Lazy[Int] = Schema.Lazy(() => Schema.primitive(StandardType.IntType))
        assert(schema.defaultValue)(isRight(equalTo(0)))
      }
    ),
    suite("Enum")(
      test("defaults to first case") {
        import zio.schema.DeriveSchema._

        val schema: Schema[Status] =
          Schema.Enum3(
            Schema.Case("Failed", Schema[Failed], (s: Status) => s.asInstanceOf[Failed]),
            Schema.Case("Ok", Schema[Ok], (s: Status) => s.asInstanceOf[Ok]),
            Schema.Case(
              "Pending",
              Schema[Pending.type],
              (s: Status) => s.asInstanceOf[Pending.type]
            )
          )
        assert(schema.defaultValue)(isRight(equalTo(Failed(0, "", None, ""))))
      }
    ),
    suite("EitherSchema")(
      test("either") {
        val eitherSchema: Schema[Either[Int, String]] = Schema.either(
          Schema.primitive(StandardType.IntType),
          Schema.primitive(StandardType.StringType)
        )
        assert(eitherSchema.defaultValue)(isRight(isLeft(equalTo(0))))
      },
      test("left") {
        val leftSchema: Schema[Left[Int, Nothing]] = Schema.left(Schema.primitive(StandardType.IntType))
        assert(leftSchema.defaultValue)(isRight(isLeft(equalTo(0))))
      },
      test("right") {
        val rightSchema: Schema[Right[Nothing, String]] = Schema.right(Schema.primitive(StandardType.StringType))
        assert(rightSchema.defaultValue)(isRight(isRight(equalTo(""))))
      }
    )
  )
}
