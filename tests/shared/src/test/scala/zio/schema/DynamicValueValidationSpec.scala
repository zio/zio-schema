package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.prelude.Validation
import zio.schema.DynamicValue._
import zio.test.Assertion._
import zio.test._

/**
 * Tests for [[DynamicValue#validate]], which uses zio-prelude's
 * [[Validation]] to accumulate ALL errors rather than stopping at the first.
 */
object DynamicValueValidationSpec extends ZIOSpecDefault {

  final case class Person(name: String, age: Int)
  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  final case class Address(street: String, city: String, zip: String)
  object Address {
    implicit val schema: Schema[Address] = DeriveSchema.gen[Address]
  }

  private def errorsOf[A](v: Validation[String, A]): List[String] =
    v.fold(_.toList, _ => Nil)

  private def isSuccess[A](expected: A): Assertion[Validation[String, A]] =
    Assertion.assertion("isSuccess")(v => v.fold(_ => false, _ == expected))

  private def isFailure[A]: Assertion[Validation[String, A]] =
    Assertion.assertion("isFailure")(v => v.fold(_ => true, _ => false))

  def spec: Spec[Environment, Any] =
    suite("DynamicValue#validate")(

      suite("primitives")(
        test("succeeds for a valid primitive") {
          val dv     = DynamicValue.Primitive(42, StandardType[Int])
          val result = dv.validate[Int]
          assertTrue(result == Validation.succeed(42))
        },
        test("fails for a mismatched primitive type") {
          val dv     = DynamicValue.Primitive("hello", StandardType[String])
          val result = dv.validate[Int]
          assert(result)(isFailure)
        }
      ),

      suite("records - single field failure")(
        test("succeeds when all fields are valid") {
          val dv = DynamicValue.Record(
            TypeId.parse("zio.schema.DynamicValueValidationSpec.Person"),
            ListMap(
              "name" -> DynamicValue.Primitive("Alice", StandardType[String]),
              "age"  -> DynamicValue.Primitive(30, StandardType[Int])
            )
          )
          assert(dv.validate[Person])(isSuccess(Person("Alice", 30)))
        },
        test("reports at least one error when one field is wrong") {
          val dv = DynamicValue.Record(
            TypeId.parse("zio.schema.DynamicValueValidationSpec.Person"),
            ListMap(
              "name" -> DynamicValue.Primitive("Bob", StandardType[String]),
              "age"  -> DynamicValue.Primitive("not-a-number", StandardType[String])
            )
          )
          val result = dv.validate[Person]
          assert(result)(isFailure) && assertTrue(errorsOf(result).length >= 1)
        }
      ),

      suite("records - multiple field failures accumulated")(
        test("accumulates errors from BOTH wrong fields simultaneously") {
          val dv = DynamicValue.Record(
            TypeId.parse("zio.schema.DynamicValueValidationSpec.Person"),
            ListMap(
              "name" -> DynamicValue.Primitive(99, StandardType[Int]),
              "age"  -> DynamicValue.Primitive("old", StandardType[String])
            )
          )
          val result = dv.validate[Person]
          assert(result)(isFailure) && assertTrue(errorsOf(result).length >= 2)
        },
        test("accumulates errors across three wrong fields") {
          val dv = DynamicValue.Record(
            TypeId.parse("zio.schema.DynamicValueValidationSpec.Address"),
            ListMap(
              "street" -> DynamicValue.Primitive(1, StandardType[Int]),
              "city"   -> DynamicValue.Primitive(2, StandardType[Int]),
              "zip"    -> DynamicValue.Primitive(3, StandardType[Int])
            )
          )
          val result = dv.validate[Address]
          assert(result)(isFailure) && assertTrue(errorsOf(result).length >= 3)
        }
      ),

      suite("sequences - multiple element failures accumulated")(
        test("succeeds for a valid sequence") {
          val dv = DynamicValue.Sequence(
            Chunk(
              DynamicValue.Primitive(1, StandardType[Int]),
              DynamicValue.Primitive(2, StandardType[Int]),
              DynamicValue.Primitive(3, StandardType[Int])
            )
          )
          assert(dv.validate[List[Int]](Schema.list(Schema.primitive[Int])))(isSuccess(List(1, 2, 3)))
        },
        test("accumulates errors from multiple bad elements") {
          val dv = DynamicValue.Sequence(
            Chunk(
              DynamicValue.Primitive(1, StandardType[Int]),
              DynamicValue.Primitive("bad", StandardType[String]),
              DynamicValue.Primitive(3, StandardType[Int]),
              DynamicValue.Primitive("also-bad", StandardType[String])
            )
          )
          val result = dv.validate[List[Int]](Schema.list(Schema.primitive[Int]))
          assert(result)(isFailure) && assertTrue(errorsOf(result).length >= 2)
        }
      ),

      suite("tuples - both sides reported")(
        test("succeeds for a valid tuple") {
          val dv = DynamicValue.Tuple(
            DynamicValue.Primitive("hello", StandardType[String]),
            DynamicValue.Primitive(42, StandardType[Int])
          )
          assert(dv.validate[(String, Int)])(isSuccess(("hello", 42)))
        },
        test("accumulates errors from both sides of a failing tuple") {
          val dv = DynamicValue.Tuple(
            DynamicValue.Primitive(99, StandardType[Int]),
            DynamicValue.Primitive("bad", StandardType[String])
          )
          val result = dv.validate[(String, Int)]
          assert(result)(isFailure) && assertTrue(errorsOf(result).length >= 2)
        }
      ),

      suite("DynamicValue.Error")(
        test("always fails with its embedded message") {
          val dv     = DynamicValue.Error("something went wrong")
          val result = dv.validate[Int]
          assert(result)(isFailure) &&
          assertTrue(errorsOf(result).contains("something went wrong"))
        }
      ),

      suite("options")(
        test("succeeds for Some with valid inner value") {
          val dv = DynamicValue.SomeValue(DynamicValue.Primitive(7, StandardType[Int]))
          assert(dv.validate[Option[Int]])(isSuccess(Some(7)))
        },
        test("succeeds for None") {
          assert(DynamicValue.NoneValue.validate[Option[Int]])(isSuccess(None))
        },
        test("fails when inner value is wrong type") {
          val dv = DynamicValue.SomeValue(DynamicValue.Primitive("oops", StandardType[String]))
          assert(dv.validate[Option[Int]])(isFailure)
        }
      ),

      suite("either")(
        test("succeeds for Left") {
          val dv = DynamicValue.LeftValue(DynamicValue.Primitive(1, StandardType[Int]))
          assert(dv.validate[Either[Int, String]])(isSuccess(Left(1)))
        },
        test("succeeds for Right") {
          val dv = DynamicValue.RightValue(DynamicValue.Primitive("ok", StandardType[String]))
          assert(dv.validate[Either[Int, String]])(isSuccess(Right("ok")))
        }
      ),

      suite("validate vs toTypedValue")(
        test("validate accumulates while toTypedValue short-circuits") {
          val dv = DynamicValue.Record(
            TypeId.parse("zio.schema.DynamicValueValidationSpec.Person"),
            ListMap(
              "name" -> DynamicValue.Primitive(1, StandardType[Int]),
              "age"  -> DynamicValue.Primitive("x", StandardType[String])
            )
          )
          val eitherResult     = dv.toTypedValue[Person]
          val validationResult = dv.validate[Person]
          assertTrue(eitherResult.isLeft) &&
          assertTrue(errorsOf(validationResult).length >= 2)
        }
      )
    )
}
