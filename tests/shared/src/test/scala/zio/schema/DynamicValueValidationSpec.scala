package zio.schema

import zio._
import zio.prelude.Validation
import zio.schema.Schema.Primitive
import zio.schema.codec.DecodeError
import zio.test.Assertion._
import zio.test._

object DynamicValueValidationSpec extends ZIOSpecDefault {

  def spec: Spec[Environment, Any] =
    suite("DynamicValueValidationSpec")(
      suite("Error Accumulation")(
        test("accumulates multiple errors in a tuple") {
          val schema = Schema.Tuple2(Schema[Int], Schema[String])
          val invalidDynamic = DynamicValue.Tuple(
            DynamicValue.Primitive("not an int", StandardType.StringType),
            DynamicValue.Primitive(123, StandardType.IntType)
          )
          
          val result = invalidDynamic.toTypedValueValidation(schema)
          
          result match {
            case Validation.Failure(_, errors) =>
              assertTrue(errors.size == 2)
            case _ =>
              assertTrue(false)
          }
        },
        test("accumulates errors in a sequence") {
          val schema = Schema.chunk(Schema[Int])
          val invalidDynamic = DynamicValue.Sequence(
            Chunk(
              DynamicValue.Primitive("not an int", StandardType.StringType),
              DynamicValue.Primitive(42, StandardType.IntType),
              DynamicValue.Primitive("also not an int", StandardType.StringType)
            )
          )
          
          val result = invalidDynamic.toTypedValueValidation(schema)
          
          result match {
            case Validation.Failure(_, errors) =>
              assertTrue(errors.size == 2)
            case _ =>
              assertTrue(false)
          }
        },
        test("accumulates errors in a set") {
          val schema = Schema.set(Schema[Int])
          val invalidDynamic = DynamicValue.SetValue(
            Set(
              DynamicValue.Primitive("not an int", StandardType.StringType),
              DynamicValue.Primitive(42, StandardType.IntType),
              DynamicValue.Primitive("also not an int", StandardType.StringType)
            )
          )
          
          val result = invalidDynamic.toTypedValueValidation(schema)
          
          result match {
            case Validation.Failure(_, errors) =>
              assertTrue(errors.size == 2)
            case _ =>
              assertTrue(false)
          }
        },
        test("accumulates errors in a map") {
          val schema = Schema.map(Schema[String], Schema[Int])
          val invalidDynamic = DynamicValue.Dictionary(
            Chunk(
              (DynamicValue.Primitive("key1", StandardType.StringType), 
               DynamicValue.Primitive("not an int", StandardType.StringType)),
              (DynamicValue.Primitive(123, StandardType.IntType), 
               DynamicValue.Primitive(456, StandardType.IntType))
            )
          )
          
          val result = invalidDynamic.toTypedValueValidation(schema)
          
          result match {
            case Validation.Failure(_, errors) =>
              assertTrue(errors.size == 2) // One for invalid key, one for invalid value
            case _ =>
              assertTrue(false)
          }
        },
        test("accumulates errors in a record") {
          final case class TestRecord(a: Int, b: String, c: Boolean)
          implicit val schema: Schema[TestRecord] = DeriveSchema.gen[TestRecord]
          
          val invalidDynamic = DynamicValue.Record(
            TypeId.parse("TestRecord"),
            scala.collection.immutable.ListMap(
              "a" -> DynamicValue.Primitive("not an int", StandardType.StringType),
              "b" -> DynamicValue.Primitive(123, StandardType.IntType),
              "c" -> DynamicValue.Primitive("not a boolean", StandardType.StringType)
            )
          )
          
          val result = invalidDynamic.toTypedValueValidation(schema)
          
          result match {
            case Validation.Failure(_, errors) =>
              assertTrue(errors.size == 3)
            case _ =>
              assertTrue(false)
          }
        }
      ),
      suite("Backward Compatibility")(
        test("toTypedValue returns Either[String, A]") {
          val schema = Schema[Int]
          val validDynamic = DynamicValue.Primitive(42, StandardType.IntType)
          
          val result = validDynamic.toTypedValue(schema)
          
          assertTrue(result == Right(42))
        },
        test("toValue returns Either[DecodeError, A]") {
          val schema = Schema[Int]
          val validDynamic = DynamicValue.Primitive(42, StandardType.IntType)
          
          val result = validDynamic.toValue(schema)
          
          assertTrue(result == Right(42))
        },
        test("toTypedValueOption returns Option[A]") {
          val schema = Schema[Int]
          val validDynamic = DynamicValue.Primitive(42, StandardType.IntType)
          
          val result = validDynamic.toTypedValueOption(schema)
          
          assertTrue(result == Some(42))
        },
        test("toValue combines multiple errors with And") {
          val schema = Schema.Tuple2(Schema[Int], Schema[String])
          val invalidDynamic = DynamicValue.Tuple(
            DynamicValue.Primitive("not an int", StandardType.StringType),
            DynamicValue.Primitive(123, StandardType.IntType)
          )
          
          val result = invalidDynamic.toValue(schema)
          
          result match {
            case Left(DecodeError.And(_, _)) => assertTrue(true)
            case _ => assertTrue(false)
          }
        }
      ),
      suite("Success Cases")(
        test("validates correct primitive") {
          val schema = Schema[Int]
          val validDynamic = DynamicValue.Primitive(42, StandardType.IntType)
          
          val result = validDynamic.toTypedValueValidation(schema)
          
          result match {
            case Validation.Success(_, value) =>
              assertTrue(value == 42)
            case _ =>
              assertTrue(false)
          }
        },
        test("validates correct tuple") {
          val schema = Schema.Tuple2(Schema[Int], Schema[String])
          val validDynamic = DynamicValue.Tuple(
            DynamicValue.Primitive(42, StandardType.IntType),
            DynamicValue.Primitive("hello", StandardType.StringType)
          )
          
          val result = validDynamic.toTypedValueValidation(schema)
          
          result match {
            case Validation.Success(_, value) =>
              assertTrue(value == (42, "hello"))
            case _ =>
              assertTrue(false)
          }
        },
        test("validates correct sequence") {
          val schema = Schema.chunk(Schema[Int])
          val validDynamic = DynamicValue.Sequence(
            Chunk(
              DynamicValue.Primitive(1, StandardType.IntType),
              DynamicValue.Primitive(2, StandardType.IntType),
              DynamicValue.Primitive(3, StandardType.IntType)
            )
          )
          
          val result = validDynamic.toTypedValueValidation(schema)
          
          result match {
            case Validation.Success(_, value) =>
              assertTrue(value == Chunk(1, 2, 3))
            case _ =>
              assertTrue(false)
          }
        }
      )
    )
}
