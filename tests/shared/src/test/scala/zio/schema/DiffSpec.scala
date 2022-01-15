package zio.schema

import zio.random.Random
import zio.schema.StandardType._
import zio.schema.types.Arities._
import zio.schema.types.{ Arities, Recursive }
import zio.test.Assertion._
import zio.test._
import zio.{ Chunk, URIO }

object DiffSpec extends DefaultRunnableSpec with DefaultJavaTimeSchemas {

  def spec: ZSpec[Environment, Failure] = suite("DiffSpec")(
    suite("identity law")(
      suite("standard types")(
        testM("Int")(diffIdentityLaw[Int]),
        testM("Long")(diffIdentityLaw[Long]),
        testM("Float")(diffIdentityLaw[Float]),
        testM("Double")(diffIdentityLaw[Double]),
        testM("Boolean")(diffIdentityLaw[Boolean]),
        testM("Bytes")(diffIdentityLaw[Chunk[Byte]]),
        suite("Either") {
          testM("primitive")(diffIdentityLaw[Either[String, String]])
        },
        suite("Option") {
          testM("primitive")(diffIdentityLaw[Option[String]])
        }
      ),
      suite("records")(
        testM("singleton")(diffIdentityLaw[Singleton.type]),
        testM("case class")(diffIdentityLaw[Pet.Dog]),
        testM("generic record")(diffIdentityLaw[SchemaGen.Arity24]),
        testM("recursive")(diffIdentityLaw[Recursive.RecursiveList])
      ),
      suite("enums")(
        testM("sealed trait")(diffIdentityLaw[Pet]),
        testM("high arity")(diffIdentityLaw[Arities]) @@ TestAspect.ignore,
        testM("recursive")(diffIdentityLaw[Recursive])
      )
    ),
    suite("diff law")(
      suite("standard types")(
        testM("Int")(diffLaw[Int]),
        testM("Long")(diffLaw[Long]),
        testM("Float")(diffLaw[Float]),
        testM("Double")(diffLaw[Double]),
        testM("Boolean")(diffLaw[Boolean]),
        testM("String")(diffLaw[String]),
        testM("ZonedDateTime")(diffLaw[java.time.ZonedDateTime]),
        testM("OffsetDateTime")(diffLaw[java.time.OffsetDateTime]),
        testM("OffsetTime")(diffLaw[java.time.OffsetTime]),
        testM("LocalTime")(diffLaw[java.time.LocalTime]),
        testM("LocalDate")(diffLaw[java.time.LocalDate]),
        testM("Instant")(diffLaw[java.time.Instant]),
        testM("Duration")(diffLaw[java.time.Duration]),
        testM("ZoneOffset")(diffLaw[java.time.ZoneOffset]),
        testM("ZoneId")(diffLaw[java.time.ZoneId]),
        testM("YearMonth")(diffLaw[java.time.YearMonth]),
        testM("Year")(diffLaw[java.time.Year]),
        testM("Period")(diffLaw[java.time.Period]),
        testM("MonthDay")(diffLaw[java.time.MonthDay]) @@ TestAspect.ignore, // TODO Leap years!
        testM("Month")(diffLaw[java.time.Month]),
        testM("DayOfWeek")(diffLaw[java.time.DayOfWeek]),
        testM("BigInteger")(diffLaw[java.math.BigInteger]),
        testM("BigDecimal")(diffLaw[java.math.BigDecimal]),
        testM("Bytes")(diffLaw[Chunk[Byte]])
      ),
      suite("sequences")(
        suite("of standard types")(
          testM("Int")(diffLaw[List[Int]]),
          testM("Long")(diffLaw[List[Long]]),
          testM("Float")(diffLaw[List[Float]]),
          testM("Double")(diffLaw[List[Double]]),
          testM("Boolean")(diffLaw[List[Boolean]]),
          testM("String")(diffLaw[List[String]]),
          testM("ZonedDateTime")(diffLaw[List[java.time.ZonedDateTime]]),
          testM("OffsetDateTime")(diffLaw[List[java.time.OffsetDateTime]]),
          testM("OffsetTime")(diffLaw[List[java.time.OffsetTime]]),
          testM("LocalTime")(diffLaw[List[java.time.LocalTime]]),
          testM("LocalDate")(diffLaw[List[java.time.LocalDate]]),
          testM("Instant")(diffLaw[List[java.time.Instant]]),
          testM("Duration")(diffLaw[List[java.time.Duration]]),
          testM("ZoneOffset")(diffLaw[List[java.time.ZoneOffset]]),
          testM("ZoneId")(diffLaw[List[java.time.ZoneId]]),
          testM("YearMonth")(diffLaw[List[java.time.YearMonth]]),
          testM("Year")(diffLaw[List[java.time.Year]]),
          testM("Period")(diffLaw[List[java.time.Period]]),
          testM("MonthDay")(diffLaw[List[java.time.MonthDay]]) @@ TestAspect.ignore, // TODO Leap years!
          testM("Month")(diffLaw[List[java.time.Month]]),
          testM("DayOfWeek")(diffLaw[List[java.time.DayOfWeek]]),
          testM("BigInteger")(diffLaw[List[java.math.BigInteger]]),
          testM("BigDecimal")(diffLaw[List[java.math.BigDecimal]])
        ),
        suite("of records")(
          testM("Dog")(diffLaw[List[Pet.Dog]])
        ),
        suite("of enumerations")(
          testM("Pet")(diffLaw[List[Pet]]),
          testM("recursive")(diffLaw[List[Recursive]])
        )
      ),
      suite("sets")(
        suite("of standard types")(
          testM("Int")(diffLaw[Set[Int]]),
          testM("Long")(diffLaw[Set[Long]]),
          testM("Float")(diffLaw[Set[Float]]),
          testM("Double")(diffLaw[Set[Double]]),
          testM("Boolean")(diffLaw[Set[Boolean]]),
          testM("String")(diffLaw[Set[String]]),
          testM("ZonedDateTime")(diffLaw[Set[java.time.ZonedDateTime]]),
          testM("OffsetDateTime")(diffLaw[Set[java.time.OffsetDateTime]]),
          testM("OffsetTime")(diffLaw[Set[java.time.OffsetTime]]),
          testM("LocalTime")(diffLaw[Set[java.time.LocalTime]]),
          testM("LocalDate")(diffLaw[Set[java.time.LocalDate]]),
          testM("Instant")(diffLaw[Set[java.time.Instant]]),
          testM("Duration")(diffLaw[Set[java.time.Duration]]),
          testM("ZoneOffset")(diffLaw[Set[java.time.ZoneOffset]]),
          testM("ZoneId")(diffLaw[Set[java.time.ZoneId]]),
          testM("YearMonth")(diffLaw[Set[java.time.YearMonth]]),
          testM("Year")(diffLaw[Set[java.time.Year]]),
          testM("Period")(diffLaw[Set[java.time.Period]]),
          testM("MonthDay")(diffLaw[Set[java.time.MonthDay]]) @@ TestAspect.ignore, // TODO Leap years!
          testM("Month")(diffLaw[Set[java.time.Month]]),
          testM("DayOfWeek")(diffLaw[Set[java.time.DayOfWeek]]),
          testM("BigInteger")(diffLaw[Set[java.math.BigInteger]]),
          testM("BigDecimal")(diffLaw[Set[java.math.BigDecimal]])
        ),
        suite("of records")(
          testM("Dog")(diffLaw[Set[Pet.Dog]])
        ),
        suite("of enumerations")(
          testM("Pet")(diffLaw[Set[Pet]]),
          testM("recursive")(diffLaw[Set[Recursive]])
        )
      ),
      suite("maps")(
        suite("of standard types")(
          testM("Int -> Int")(diffLaw[Map[Int, Int]])
        ),
        suite("of records")(
          testM("Int -> Dog")(diffLaw[Map[Int, Pet.Dog]]),
          testM("Dog -> Cat")(diffLaw[Map[Pet.Dog, Pet.Cat]])
        ),
        suite("of enumerations")(
          testM("Int -> Pet")(diffLaw[Map[Int, Pet]]),
          testM("Dog -> Pet")(diffLaw[Map[Pet.Dog, Pet]]),
          testM("Pet -> Pet")(diffLaw[Map[Pet, Pet]])
        )
      ),
      suite("records")(
        testM("singleton")(diffLaw[Singleton.type]),
        testM("case class")(diffLaw[Pet.Dog]),
        testM("generic record")(diffLaw[SchemaGen.Arity24]),
        testM("recursive")(diffLaw[Recursive.RecursiveEither])
      ),
      suite("enums")(
        testM("sealed trait")(diffLaw[Pet]),
        testM("high arity")(diffLaw[Arities]) @@ TestAspect.ignore,
        testM("recursive")(diffLaw[Recursive])
      )
    ),
    suite("not comparable")(
      testM("Left <-> Right") {
        notComparable[Either[String, String]](_.isLeft, _.isRight)(_.isLeft)
      },
      testM("Separate enum cases") {
        notComparable[Pet](_.isInstanceOf[Pet.Dog], _.isInstanceOf[Pet.Cat])(_.isLeft)
      }
    )
  )

  private def diffIdentityLaw[A](implicit schema: Schema[A]): URIO[Random with Sized with TestConfig, TestResult] =
    check(DeriveGen.gen[A]) { a =>
      assertTrue(schema.diff(a, a).isIdentical)
    }

  private def diffLaw[A](implicit schema: Schema[A]): URIO[Random with Sized with TestConfig, TestResult] = {
    val gen = DeriveGen.gen[A]
    check(gen <*> gen) {
      case (l, r) =>
        val diff = schema.diff(l, r)
        if (diff.isComparable) {
          val patched = schema.diff(l, r).patch(l)
          if (patched.isLeft) println(diff)
          assert(patched)(isRight(equalTo(r)))
        } else {
          assertTrue(true)
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
