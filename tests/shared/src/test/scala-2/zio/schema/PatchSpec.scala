package zio.schema

import zio.schema.StandardType._
import zio.schema.types.Arities._
import zio.schema.types.{ Arities, Recursive }
import zio.test.Assertion._
import zio.test._
import zio.{ Chunk, Scope, URIO }

object PatchSpec extends ZIOSpecDefault with DefaultJavaTimeSchemas {

  def spec: Spec[TestEnvironment with Scope, Any] = suite("PatchSpec")(
    suite("identity law")(
      suite("standard types")(
        test("Int")(patchIdentityLaw[Int]),
        test("Long")(patchIdentityLaw[Long]),
        test("Float")(patchIdentityLaw[Float]),
        test("Double")(patchIdentityLaw[Double]),
        test("Boolean")(patchIdentityLaw[Boolean]),
        test("Bytes")(patchIdentityLaw[Chunk[Byte]]),
        suite("Either") {
          test("primitive")(patchIdentityLaw[Either[String, String]])
        },
        suite("Option") {
          test("primitive")(patchIdentityLaw[Option[String]])
        }
      ),
      suite("records")(
        test("singleton")(patchIdentityLaw[Singleton.type]),
        test("case class")(patchIdentityLaw[Pet.Dog]),
        test("generic record")(patchIdentityLaw[SchemaGen.Arity24]),
        test("recursive")(patchIdentityLaw[Recursive.RecursiveList])
      ),
      suite("enums")(
        test("sealed trait")(patchIdentityLaw[Pet]),
        test("high arity")(patchIdentityLaw[Arities]) @@ TestAspect.ignore,
        test("recursive")(patchIdentityLaw[Recursive])
      )
    ),
    suite("patch law")(
      suite("standard types")(
        test("Int")(patchLaw[Int]),
        test("Long")(patchLaw[Long]),
        test("Float")(patchLaw[Float]),
        test("Double")(patchLaw[Double]),
        test("Boolean")(patchLaw[Boolean]),
        test("String")(patchLaw[String]),
        test("ZonedDateTime")(patchLaw[java.time.ZonedDateTime]),
        test("OffsetDateTime")(patchLaw[java.time.OffsetDateTime]),
        test("OffsetTime")(patchLaw[java.time.OffsetTime]),
        test("LocalTime")(patchLaw[java.time.LocalTime]),
        test("LocalDate")(patchLaw[java.time.LocalDate]),
        test("Instant")(patchLaw[java.time.Instant]),
        test("Duration")(patchLaw[java.time.Duration]),
        test("ZoneOffset")(patchLaw[java.time.ZoneOffset]),
        test("ZoneId")(patchLaw[java.time.ZoneId]),
        test("YearMonth")(patchLaw[java.time.YearMonth]),
        test("Year")(patchLaw[java.time.Year]),
        test("Period")(patchLaw[java.time.Period]),
        test("MonthDay")(patchLaw[java.time.MonthDay]) @@ TestAspect.ignore, // TODO Leap years!
        test("Month")(patchLaw[java.time.Month]),
        test("DayOfWeek")(patchLaw[java.time.DayOfWeek]),
        test("BigInteger")(patchLaw[java.math.BigInteger]),
        test("BigDecimal")(patchLaw[java.math.BigDecimal]),
        test("Bytes")(patchLaw[Chunk[Byte]])
      ),
      suite("sequences")(
        suite("of standard types")(
          test("Int")(patchLaw[List[Int]]),
          test("Long")(patchLaw[List[Long]]),
          test("Float")(patchLaw[List[Float]]),
          test("Double")(patchLaw[List[Double]]),
          test("Boolean")(patchLaw[List[Boolean]]),
          test("String")(patchLaw[List[String]]),
          test("ZonedDateTime")(patchLaw[List[java.time.ZonedDateTime]]),
          test("OffsetDateTime")(patchLaw[List[java.time.OffsetDateTime]]),
          test("OffsetTime")(patchLaw[List[java.time.OffsetTime]]),
          test("LocalTime")(patchLaw[List[java.time.LocalTime]]),
          test("LocalDate")(patchLaw[List[java.time.LocalDate]]),
          test("Instant")(patchLaw[List[java.time.Instant]]),
          test("Duration")(patchLaw[List[java.time.Duration]]),
          test("ZoneOffset")(patchLaw[List[java.time.ZoneOffset]]),
          test("ZoneId")(patchLaw[List[java.time.ZoneId]]),
          test("YearMonth")(patchLaw[List[java.time.YearMonth]]),
          test("Year")(patchLaw[List[java.time.Year]]),
          test("Period")(patchLaw[List[java.time.Period]]),
          test("MonthDay")(patchLaw[List[java.time.MonthDay]]) @@ TestAspect.ignore, // TODO Leap years!
          test("Month")(patchLaw[List[java.time.Month]]),
          test("DayOfWeek")(patchLaw[List[java.time.DayOfWeek]]),
          test("BigInteger")(patchLaw[List[java.math.BigInteger]]),
          test("BigDecimal")(patchLaw[List[java.math.BigDecimal]])
        ),
        suite("of records")(
          test("Dog")(patchLaw[List[Pet.Dog]])
        ),
        suite("of enumerations")(
          test("Pet")(patchLaw[List[Pet]]),
          test("recursive")(patchLaw[List[Recursive]])
        )
      ),
      suite("sets")(
        suite("of standard types")(
          test("Int")(patchLaw[Set[Int]]),
          test("Long")(patchLaw[Set[Long]]),
          test("Float")(patchLaw[Set[Float]]),
          test("Double")(patchLaw[Set[Double]]),
          test("Boolean")(patchLaw[Set[Boolean]]),
          test("String")(patchLaw[Set[String]]),
          test("ZonedDateTime")(patchLaw[Set[java.time.ZonedDateTime]]),
          test("OffsetDateTime")(patchLaw[Set[java.time.OffsetDateTime]]),
          test("OffsetTime")(patchLaw[Set[java.time.OffsetTime]]),
          test("LocalTime")(patchLaw[Set[java.time.LocalTime]]),
          test("LocalDate")(patchLaw[Set[java.time.LocalDate]]),
          test("Instant")(patchLaw[Set[java.time.Instant]]),
          test("Duration")(patchLaw[Set[java.time.Duration]]),
          test("ZoneOffset")(patchLaw[Set[java.time.ZoneOffset]]),
          test("ZoneId")(patchLaw[Set[java.time.ZoneId]]),
          test("YearMonth")(patchLaw[Set[java.time.YearMonth]]),
          test("Year")(patchLaw[Set[java.time.Year]]),
          test("Period")(patchLaw[Set[java.time.Period]]),
          test("MonthDay")(patchLaw[Set[java.time.MonthDay]]) @@ TestAspect.ignore, // TODO Leap years!
          test("Month")(patchLaw[Set[java.time.Month]]),
          test("DayOfWeek")(patchLaw[Set[java.time.DayOfWeek]]),
          test("BigInteger")(patchLaw[Set[java.math.BigInteger]]),
          test("BigDecimal")(patchLaw[Set[java.math.BigDecimal]])
        ),
        suite("of records")(
          test("Dog")(patchLaw[Set[Pet.Dog]])
        ),
        suite("of enumerations")(
          test("Pet")(patchLaw[Set[Pet]]),
          test("recursive")(patchLaw[Set[Recursive]])
        )
      ),
      suite("maps")(
        suite("of standard types")(
          test("Int -> Int")(patchLaw[Map[Int, Int]])
        ),
        suite("of records")(
          test("Int -> Dog")(patchLaw[Map[Int, Pet.Dog]]),
          test("Dog -> Cat")(patchLaw[Map[Pet.Dog, Pet.Cat]])
        ),
        suite("of enumerations")(
          test("Int -> Pet")(patchLaw[Map[Int, Pet]]),
          test("Dog -> Pet")(patchLaw[Map[Pet.Dog, Pet]]),
          test("Pet -> Pet")(patchLaw[Map[Pet, Pet]])
        )
      ),
      suite("records")(
        test("singleton")(patchLaw[Singleton.type]),
        test("case class")(patchLaw[Pet.Dog]),
        test("generic record")(patchLaw[SchemaGen.Arity24]),
        test("recursive")(patchLaw[Recursive.RecursiveEither])
      ),
      suite("enums")(
        test("sealed trait")(patchLaw[Pet]),
        test("high arity")(patchLaw[Arities]) @@ TestAspect.ignore,
        test("recursive")(patchLaw[Recursive])
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

  private def patchIdentityLaw[A](implicit schema: Schema[A]): URIO[Sized with TestConfig, TestResult] =
    check(DeriveGen.gen[A]) { a =>
      assertTrue(schema.diff(a, a).isIdentical)
    }

  private def patchLaw[A](implicit schema: Schema[A]): URIO[Sized with TestConfig, TestResult] = {
    val gen = DeriveGen.gen[A]
    check(gen <*> gen) {
      case (l, r) =>
        val diff = schema.diff(l, r)
        if (diff.isComparable) {
          val afterInvert        = diff.invert.invert
          val patched            = schema.diff(l, r).patch(l)
          val patchedAfterInvert = afterInvert.patch(l)
          if (patched.isLeft || patchedAfterInvert.isLeft) println(diff)
          assert(patched)(isRight(equalTo(r))) && assert(patchedAfterInvert)(isRight(equalTo(r)))
        } else {
          assertTrue(true)
        }
    }
  }

  private def notComparable[A](leftFilter: A => Boolean, rightFilter: A => Boolean)(
    assertion: Either[String, A] => Boolean
  )(implicit schema: Schema[A]): URIO[Sized with TestConfig, TestResult] = {
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
