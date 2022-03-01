package zio.schema

import zio.schema.StandardType._
import zio.schema.types.Arities._
import zio.schema.types.{ Arities, Recursive }
import zio.test.Assertion._
import zio.test._
import zio.{ Chunk, Random, URIO, ZIO }

object DiffSpec extends DefaultRunnableSpec with DefaultJavaTimeSchemas {

  def spec: ZSpec[Environment, Failure] = suite("DiffSpec")(
    suite("identity law")(
      suite("standard types")(
        test("Int")(diffIdentityLaw[Int]),
        test("Long")(diffIdentityLaw[Long]),
        test("Float")(diffIdentityLaw[Float]),
        test("Double")(diffIdentityLaw[Double]),
        test("Boolean")(diffIdentityLaw[Boolean]),
        test("Bytes")(diffIdentityLaw[Chunk[Byte]]),
        suite("Either") {
          test("primitive")(diffIdentityLaw[Either[String, String]])
        },
        suite("Option") {
          test("primitive")(diffIdentityLaw[Option[String]])
        }
      ),
      suite("records")(
        test("singleton")(diffIdentityLaw[Singleton.type]),
        test("case class")(diffIdentityLaw[Pet.Dog]),
        test("generic record")(diffIdentityLaw[SchemaGen.Arity24]),
        test("recursive")(diffIdentityLaw[Recursive.RecursiveList])
      ),
      suite("enums")(
        test("sealed trait")(diffIdentityLaw[Pet]),
        test("high arity")(diffIdentityLaw[Arities]) @@ TestAspect.ignore,
        test("recursive")(diffIdentityLaw[Recursive])
      )
    ),
    suite("diff law")(
      suite("standard types")(
        test("Int")(diffLaw[Int]),
        test("Long")(diffLaw[Long]),
        test("Float")(diffLaw[Float]),
        test("Double")(diffLaw[Double]),
        test("Boolean")(diffLaw[Boolean]),
        test("String")(diffLaw[String]),
        test("ZonedDateTime")(diffLaw[java.time.ZonedDateTime]),
        test("OffsetDateTime")(diffLaw[java.time.OffsetDateTime]),
        test("OffsetTime")(diffLaw[java.time.OffsetTime]),
        test("LocalTime")(diffLaw[java.time.LocalTime]),
        test("LocalDate")(diffLaw[java.time.LocalDate]),
        test("Instant")(diffLaw[java.time.Instant]),
        test("Duration")(diffLaw[java.time.Duration]),
        test("ZoneOffset")(diffLaw[java.time.ZoneOffset]),
        test("ZoneId")(diffLaw[java.time.ZoneId]),
        test("YearMonth")(diffLaw[java.time.YearMonth]),
        test("Year")(diffLaw[java.time.Year]),
        test("Period")(diffLaw[java.time.Period]),
        test("MonthDay")(diffLaw[java.time.MonthDay]) @@ TestAspect.ignore, // TODO Leap years!
        test("Month")(diffLaw[java.time.Month]),
        test("DayOfWeek")(diffLaw[java.time.DayOfWeek]),
        test("BigInteger")(diffLaw[java.math.BigInteger]),
        test("BigDecimal")(diffLaw[java.math.BigDecimal]),
        test("Bytes")(diffLaw[Chunk[Byte]])
      ),
      suite("sequences")(
        suite("of standard types")(
          test("Int")(diffLaw[List[Int]]),
          test("Long")(diffLaw[List[Long]]),
          test("Float")(diffLaw[List[Float]]),
          test("Double")(diffLaw[List[Double]]),
          test("Boolean")(diffLaw[List[Boolean]]),
          test("String")(diffLaw[List[String]]),
          test("ZonedDateTime")(diffLaw[List[java.time.ZonedDateTime]]),
          test("OffsetDateTime")(diffLaw[List[java.time.OffsetDateTime]]),
          test("OffsetTime")(diffLaw[List[java.time.OffsetTime]]),
          test("LocalTime")(diffLaw[List[java.time.LocalTime]]),
          test("LocalDate")(diffLaw[List[java.time.LocalDate]]),
          test("Instant")(diffLaw[List[java.time.Instant]]),
          test("Duration")(diffLaw[List[java.time.Duration]]),
          test("ZoneOffset")(diffLaw[List[java.time.ZoneOffset]]),
          test("ZoneId")(diffLaw[List[java.time.ZoneId]]),
          test("YearMonth")(diffLaw[List[java.time.YearMonth]]),
          test("Year")(diffLaw[List[java.time.Year]]),
          test("Period")(diffLaw[List[java.time.Period]]),
          test("MonthDay")(diffLaw[List[java.time.MonthDay]]) @@ TestAspect.ignore, // TODO Leap years!
          test("Month")(diffLaw[List[java.time.Month]]),
          test("DayOfWeek")(diffLaw[List[java.time.DayOfWeek]]),
          test("BigInteger")(diffLaw[List[java.math.BigInteger]]),
          test("BigDecimal")(diffLaw[List[java.math.BigDecimal]])
        ),
        suite("of records")(
          test("Dog")(diffLaw[List[Pet.Dog]])
        ),
        suite("of enumerations")(
          test("Pet")(diffLaw[List[Pet]]),
          test("recursive")(diffLaw[List[Recursive]])
        )
      ),
      suite("sets")(
        suite("of standard types")(
          test("Int")(diffLaw[Set[Int]]),
          test("Long")(diffLaw[Set[Long]]),
          test("Float")(diffLaw[Set[Float]]),
          test("Double")(diffLaw[Set[Double]]),
          test("Boolean")(diffLaw[Set[Boolean]]),
          test("String")(diffLaw[Set[String]]),
          test("ZonedDateTime")(diffLaw[Set[java.time.ZonedDateTime]]),
          test("OffsetDateTime")(diffLaw[Set[java.time.OffsetDateTime]]),
          test("OffsetTime")(diffLaw[Set[java.time.OffsetTime]]),
          test("LocalTime")(diffLaw[Set[java.time.LocalTime]]),
          test("LocalDate")(diffLaw[Set[java.time.LocalDate]]),
          test("Instant")(diffLaw[Set[java.time.Instant]]),
          test("Duration")(diffLaw[Set[java.time.Duration]]),
          test("ZoneOffset")(diffLaw[Set[java.time.ZoneOffset]]),
          test("ZoneId")(diffLaw[Set[java.time.ZoneId]]),
          test("YearMonth")(diffLaw[Set[java.time.YearMonth]]),
          test("Year")(diffLaw[Set[java.time.Year]]),
          test("Period")(diffLaw[Set[java.time.Period]]),
          test("MonthDay")(diffLaw[Set[java.time.MonthDay]]) @@ TestAspect.ignore, // TODO Leap years!
          test("Month")(diffLaw[Set[java.time.Month]]),
          test("DayOfWeek")(diffLaw[Set[java.time.DayOfWeek]]),
          test("BigInteger")(diffLaw[Set[java.math.BigInteger]]),
          test("BigDecimal")(diffLaw[Set[java.math.BigDecimal]])
        ),
        suite("of records")(
          test("Dog")(diffLaw[Set[Pet.Dog]])
        ),
        suite("of enumerations")(
          test("Pet")(diffLaw[Set[Pet]]),
          test("recursive")(diffLaw[Set[Recursive]])
        )
      ),
      suite("maps")(
        suite("of standard types")(
          test("Int -> Int")(diffLaw[Map[Int, Int]])
        ),
        suite("of records")(
          test("Int -> Dog")(diffLaw[Map[Int, Pet.Dog]]),
          test("Dog -> Cat")(diffLaw[Map[Pet.Dog, Pet.Cat]])
        ),
        suite("of enumerations")(
          test("Int -> Pet")(diffLaw[Map[Int, Pet]]),
          test("Dog -> Pet")(diffLaw[Map[Pet.Dog, Pet]]),
          test("Pet -> Pet")(diffLaw[Map[Pet, Pet]])
        )
      ),
      suite("records")(
        test("singleton")(diffLaw[Singleton.type]),
        test("case class")(diffLaw[Pet.Dog]),
        test("generic record")(diffLaw[SchemaGen.Arity24]),
        test("recursive")(diffLaw[Recursive.RecursiveEither])
      ),
      suite("enums")(
        test("sealed trait")(diffLaw[Pet]),
        test("high arity")(diffLaw[Arities]) @@ TestAspect.ignore,
        test("recursive")(diffLaw[Recursive])
      )
    ),
    suite("not comparable")(
      test("Left <-> Right") {
        notComparable[Either[String, String]](_.isLeft, _.isRight)(_.isLeft)
      },
      test("Separate enum cases") {
        notComparable[Pet](_.isInstanceOf[Pet.Dog], _.isInstanceOf[Pet.Cat])(_.isLeft)
      }
    )
  )

  private def diffIdentityLaw[A](implicit schema: Schema[A]): URIO[Random with Sized with TestConfig, TestResult] =
    check(DeriveGen.gen[A]) { a =>
      assertTrue(schema.diff(a, a).isIdentical)
    }

  private def diffLaw[A](
    implicit checkConstructor: CheckConstructor[Random with Sized with TestConfig, TestResult],
    schema: Schema[A]
  ): ZIO[checkConstructor.OutEnvironment, checkConstructor.OutError, TestResult] = {
    val gen = DeriveGen.gen[A]
    check(gen <*> gen) {
      case (l, r) =>
        val diff = schema.diff(l, r)
        if (diff.isComparable) {
          val patched = schema.diff(l, r).patch(l)
          if (patched.isLeft) println(diff)
          assert(patched)(isRight(equalTo(r)))
        } else {
          assertCompletes
        }
    }
  }

  private def notComparable[A](leftFilter: A => Boolean, rightFilter: A => Boolean)(
    assertion: Either[String, A] => Boolean
  )(implicit schema: Schema[A]): URIO[Random with Sized with TestConfig, TestResult] = {
    val gen = DeriveGen.gen[A]

    check(gen.withFilter(leftFilter) <*> gen.withFilter(rightFilter)) {
      case (l, r) =>
        assert(assertion(schema.diff(l, r).patch(l)))(isTrue)
    }
  }

  sealed trait Pet

  object Pet {
    case class Dog(name: String) extends Pet

    object Dog {
      implicit lazy val schema: Schema[Dog] = DeriveSchema.gen[Dog]
    }
    case class Cat(name: String) extends Pet

    object Cat {
      implicit lazy val schema: Schema[Cat] = DeriveSchema.gen
    }
    case class Parrot(name: String, color: Int = 55) extends Pet

    object Parrot {
      implicit val schema: Schema[Parrot] = DeriveSchema.gen
    }

    implicit lazy val schema: Schema[Pet] = DeriveSchema.gen
  }

  case class Person(name: String, age: Int)

  object Person {
    implicit lazy val schema: Schema[Person] = DeriveSchema.gen
  }

  case object Singleton

  implicit val singletonSchema: Schema[Singleton.type] = Schema.singleton(Singleton)
}
