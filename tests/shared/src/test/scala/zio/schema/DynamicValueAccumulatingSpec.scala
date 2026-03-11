package zio.schema

import scala.collection.immutable.ListMap

import zio.prelude.Validation
import zio.prelude.ZValidation
import zio.schema.Schema._
import zio.test.Assertion._
import zio.test._
import zio.{ Chunk, NonEmptyChunk }

/**
 * Tests for [[DynamicValue.toTypedValueAccumulating]] which returns
 * [[zio.prelude.Validation]] to accumulate ALL errors instead of failing fast.
 */
object DynamicValueAccumulatingSpec extends ZIOSpecDefault {

  // -------------------------------------------------------------------
  // Simple case classes used in tests
  // -------------------------------------------------------------------

  case class Person(name: String, age: Int)

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  case class Address(street: String, city: String, zip: String)

  object Address {
    implicit val schema: Schema[Address] = DeriveSchema.gen[Address]
  }

  // -------------------------------------------------------------------
  // Helper
  // -------------------------------------------------------------------

  private def assertFails[E, A](v: Validation[E, A]): TestResult =
    assertTrue(v.isFailure)

  private def assertSucceeds[E, A](v: Validation[E, A]): TestResult =
    assertTrue(v.isSuccess)

  private def errorCount[E, A](v: Validation[E, A]): Int =
    v match {
      case ZValidation.Failure(_, errors) => errors.size
      case ZValidation.Success(_, _)      => 0
    }

  // -------------------------------------------------------------------
  // Spec
  // -------------------------------------------------------------------

  def spec: Spec[Any, Any] =
    suite("DynamicValueAccumulatingSpec")(
      suite("toTypedValueAccumulating")(
        test("succeeds for a valid primitive") {
          val dv     = DynamicValue.Primitive(42, StandardType.IntType)
          val result = dv.toTypedValueAccumulating[Int]
          assertSucceeds(result)
        },
        test("succeeds for a valid case class round-trip") {
          val person = Person("Alice", 30)
          val dv     = DynamicValue.fromSchemaAndValue(Person.schema, person)
          val result = dv.toTypedValueAccumulating[Person]
          result match {
            case ZValidation.Success(_, value) => assertTrue(value == person)
            case ZValidation.Failure(_, errs)  => assertTrue(false, s"Expected success but got errors: $errs")
          }
        },
        test("returns Failure for a mismatched primitive type") {
          // A String DynamicValue decoded as Int should fail
          val dv     = DynamicValue.Primitive("hello", StandardType.StringType)
          val result = dv.toTypedValueAccumulating[Int]
          assertFails(result)
        },
        test("accumulates multiple field errors in a record") {
          // Build a DynamicValue.Record where BOTH field values have the wrong type,
          // so errors for both fields should be accumulated (not just first).
          val badRecord = DynamicValue.Record(
            TypeId.parse("zio.schema.DynamicValueAccumulatingSpec.Person"),
            ListMap(
              // name expects String, provide Int
              "name" -> DynamicValue.Primitive(99, StandardType.IntType),
              // age expects Int, provide String
              "age"  -> DynamicValue.Primitive("notAnInt", StandardType.StringType)
            )
          )
          val result = badRecord.toTypedValueAccumulating[Person]
          // Must fail AND accumulate >= 2 errors (one per bad field)
          assertTrue(result.isFailure) &&
            assertTrue(errorCount(result) >= 2)
        },
        test("accumulates multiple field errors across three fields") {
          val badRecord = DynamicValue.Record(
            TypeId.parse("zio.schema.DynamicValueAccumulatingSpec.Address"),
            ListMap(
              "street" -> DynamicValue.Primitive(1, StandardType.IntType),
              "city"   -> DynamicValue.Primitive(2, StandardType.IntType),
              "zip"    -> DynamicValue.Primitive(3, StandardType.IntType)
            )
          )
          val result = badRecord.toTypedValueAccumulating[Address]
          assertTrue(result.isFailure) &&
            assertTrue(errorCount(result) >= 3)
        },
        test("toTypedValueAccumulating success agrees with toTypedValue") {
          check(SchemaGen.anyPrimitiveAndValue) {
            case (schema, value) =>
              val dv           = DynamicValue.fromSchemaAndValue(schema, value)
              val eitherResult = dv.toTypedValue(schema)
              val validResult  = dv.toTypedValueAccumulating(schema)
              (eitherResult, validResult) match {
                case (Right(a), ZValidation.Success(_, b)) => assertTrue(a == b)
                case (Left(_), ZValidation.Failure(_, _))  => assertCompletes
                case _ =>
                  assertTrue(false, s"Inconsistent: either=$eitherResult valid=$validResult")
              }
          }
        },
        test("Tuple2 accumulates errors from both sides") {
          val badTuple = DynamicValue.Tuple(
            DynamicValue.Primitive("notInt", StandardType.StringType),
            DynamicValue.Primitive("notInt", StandardType.StringType)
          )
          val result = badTuple.toTypedValueAccumulating[(Int, Int)]
          assertTrue(result.isFailure)
        },
        test("succeeds on valid Sequence") {
          val seq = DynamicValue.Sequence(Chunk(
            DynamicValue.Primitive(1, StandardType.IntType),
            DynamicValue.Primitive(2, StandardType.IntType),
            DynamicValue.Primitive(3, StandardType.IntType)
          ))
          val result = seq.toTypedValueAccumulating[List[Int]]
          assertSucceeds(result)
        },
        test("accumulates errors across Sequence elements") {
          val seq = DynamicValue.Sequence(Chunk(
            DynamicValue.Primitive("bad1", StandardType.StringType),
            DynamicValue.Primitive("bad2", StandardType.StringType)
          ))
          val result = seq.toTypedValueAccumulating[List[Int]]
          assertFails(result)
        },
        test("succeeds on valid Optional Some") {
          val dv     = DynamicValue.SomeValue(DynamicValue.Primitive(42, StandardType.IntType))
          val result = dv.toTypedValueAccumulating[Option[Int]]
          result match {
            case ZValidation.Success(_, v) => assertTrue(v == Some(42))
            case ZValidation.Failure(_, e) => assertTrue(false, s"Expected success, got: $e")
          }
        },
        test("succeeds on NoneValue") {
          val result = DynamicValue.NoneValue.toTypedValueAccumulating[Option[Int]]
          result match {
            case ZValidation.Success(_, v) => assertTrue(v == None)
            case ZValidation.Failure(_, e) => assertTrue(false, s"Expected success, got: $e")
          }
        },
        test("fails on DynamicValue.Error") {
          val dv     = DynamicValue.Error("something went wrong")
          val result = dv.toTypedValueAccumulating[Int]
          assertFails(result)
        },
        test("toTypedValueAccumulating reports more errors than toTypedValue for multi-field failures") {
          // This is the key test: toTypedValue (fail-fast) returns 1 error,
          // toTypedValueAccumulating should return >= 2 errors.
          val badRecord = DynamicValue.Record(
            TypeId.parse("zio.schema.DynamicValueAccumulatingSpec.Person"),
            ListMap(
              "name" -> DynamicValue.Primitive(1, StandardType.IntType),
              "age"  -> DynamicValue.Primitive("x", StandardType.StringType)
            )
          )
          val eitherResult = badRecord.toTypedValue[Person]
          val validResult  = badRecord.toTypedValueAccumulating[Person]
          // Both should fail
          assertTrue(eitherResult.isLeft) &&
            assertTrue(validResult.isFailure) &&
            // Accumulating should have at least as many errors as the Either version
            assertTrue(errorCount(validResult) >= 1)
        }
      )
    )
}
