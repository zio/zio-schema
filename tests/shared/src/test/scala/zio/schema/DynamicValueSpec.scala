package zio.schema

import scala.collection.immutable.ListMap

import zio._
import zio.prelude.ZValidation
import zio.schema.Schema.Primitive
import zio.schema.SchemaGen._
import zio.test.Assertion._
import zio.test.{ Sized, TestConfig, _ }

object DynamicValueSpec extends ZIOSpecDefault {
  final case class Person(name: String, age: Int)

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  def spec: Spec[Environment, Any] =
    suite("DynamicValueSpec")(
      suite("round-trip")(
        suite("Primitives")(primitiveTests: _*),
        test("round-trips Records") {
          check(SchemaGen.anyRecordOfRecordsAndValue) {
            case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips Enumerations") {
          check(SchemaGen.anyEnumerationAndValue) {
            case (schema, a) =>
              assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips Eithers") {
          check(SchemaGen.anyEitherAndValue) {
            case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips Tuples") {
          check(SchemaGen.anyTupleAndValue) {
            case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips Optionals") {
          check(SchemaGen.anyOptionalAndValue) {
            case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips Transform") {
          check(SchemaGen.anyTransformAndValue) {
            case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips CaseClass") {
          check(SchemaGen.anyCaseClassAndValue) {
            case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips Enum") {
          check(SchemaGen.anyEnumAndValue) {
            case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips any un-nested schema") {
          check(SchemaGen.anyLeafAndValue) {
            case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips any nested schema") {
          check(SchemaGen.anyTree(1).flatMap(s => DynamicValueGen.anyDynamicValueOfSchema(s).map(s -> _))) {
            case (schema, dynamic) =>
              assert(schema.fromDynamic(dynamic))(isRight)
          }
        },
        test("round-trips recursive data types") {
          check(SchemaGen.anyRecursiveTypeAndValue) {
            case (schema, a) =>
              assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips sequence") {
          check(SchemaGen.anySequenceAndValue) {
            case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips set") {
          check(SchemaGen.anySetAndValue) {
            case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips map") {
          check(SchemaGen.anyMapAndValue) {
            case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        }
      ),
      suite("stack safety")(
        test("fromSchemaAndValue is stack safe") {
          check(Json.genDeep) { json =>
            val _ = DynamicValue.fromSchemaAndValue(Json.schema, json)
            assertCompletes
          }
        } @@ TestAspect.size(100),
        test("toTyped is stack safe") {
          check(Json.genDeep) { json =>
            val dyn   = DynamicValue.fromSchemaAndValue(Json.schema, json)
            val json2 = dyn.toTypedValue(Json.schema)
            assertTrue(json2 == Right(json))
          }
        } @@ TestAspect.size(250) @@ TestAspect.ignore
      ),
      suite("accumulating decode")(
        test("succeeds for valid record round-trips") {
          val person = Person("Alice", 30)
          val result = DynamicValue.fromSchemaAndValue(Person.schema, person).toTypedValueAccumulating[Person]

          assert(result)(equalTo(zio.prelude.Validation.succeed(person)))
        },
        test("matches toTypedValue on successful primitive decodes") {
          check(SchemaGen.anyPrimitiveAndValue) {
            case (schema, value) =>
              val dynamic = DynamicValue.fromSchemaAndValue(schema, value)

              assertTrue(dynamic.toTypedValue(schema) == Right(value)) &&
              assertTrue(dynamic.toTypedValueAccumulating(schema) == zio.prelude.Validation.succeed(value))
          }
        },
        test("decodes records using schema field order") {
          val reorderedRecord = DynamicValue.Record(
            TypeId.parse("zio.schema.DynamicValueSpec.Person"),
            ListMap(
              "age"  -> DynamicValue.Primitive(30, StandardType.IntType),
              "name" -> DynamicValue.Primitive("Alice", StandardType.StringType)
            )
          )

          assert(reorderedRecord.toTypedValueAccumulating[Person])(equalTo(zio.prelude.Validation.succeed(Person("Alice", 30))))
        },
        test("accumulates multiple record field errors") {
          val badRecord = DynamicValue.Record(
            TypeId.parse("zio.schema.DynamicValueSpec.Person"),
            ListMap(
              "name" -> DynamicValue.Primitive(42, StandardType.IntType),
              "age"  -> DynamicValue.Primitive("not-an-int", StandardType.StringType)
            )
          )
          val accumulating = badRecord.toTypedValueAccumulating[Person]

          assertTrue(badRecord.toTypedValue[Person].isLeft) &&
          assertTrue(accumulating.isFailure) &&
          assertTrue(errorCount(accumulating) >= 2)
        },
        test("accumulates tuple element errors") {
          val badTuple = DynamicValue.Tuple(
            DynamicValue.Primitive("not-an-int", StandardType.StringType),
            DynamicValue.Primitive("also-not-an-int", StandardType.StringType)
          )
          val result = badTuple.toTypedValueAccumulating[(Int, Int)]

          assertTrue(result.isFailure) &&
          assertTrue(errorCount(result) >= 2)
        },
        test("accumulates set element errors") {
          val badSet = DynamicValue.SetValue(
            Set(
              DynamicValue.Primitive("first", StandardType.StringType),
              DynamicValue.Primitive("second", StandardType.StringType)
            )
          )
          val result = badSet.toTypedValueAccumulating[Set[Int]]

          assertTrue(result.isFailure) &&
          assertTrue(errorCount(result) >= 2)
        },
        test("accumulates map key and value errors") {
          val badMap = DynamicValue.Dictionary(
            Chunk(
              (
                DynamicValue.Primitive("bad-key-1", StandardType.StringType),
                DynamicValue.Primitive("bad-value-1", StandardType.StringType)
              ),
              (
                DynamicValue.Primitive("bad-key-2", StandardType.StringType),
                DynamicValue.Primitive("bad-value-2", StandardType.StringType)
              )
            )
          )
          val result = badMap.toTypedValueAccumulating[Map[Int, Int]]

          assertTrue(result.isFailure) &&
          assertTrue(errorCount(result) >= 4)
        },
        test("accumulates sequence element errors") {
          val badSequence = DynamicValue.Sequence(
            Chunk(
              DynamicValue.Primitive("first", StandardType.StringType),
              DynamicValue.Primitive("second", StandardType.StringType)
            )
          )
          val result = badSequence.toTypedValueAccumulating[List[Int]]

          assertTrue(result.isFailure) &&
          assertTrue(errorCount(result) >= 2)
        }
      )
    )

  val primitiveTests: List[Spec[Sized with TestConfig, Nothing]] = schemasAndGens.map {
    case SchemaTest(name, standardType, gen) =>
      test(s"round-trips $name") {
        dynamicValueLaw(gen, Primitive(standardType, Chunk.empty))
      }
  }

  private def dynamicValueLaw[R, A](gen: Gen[R, A], schema: Schema[A]): URIO[R with TestConfig, TestResult] =
    check(gen) { a =>
      assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
    }

  private def errorCount[A](result: zio.prelude.Validation[String, A]): Int =
    result match {
      case ZValidation.Failure(_, errs) => errs.size
      case ZValidation.Success(_, _)    => 0
    }

}
