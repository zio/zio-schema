package zio.schema

import zio._
import zio.schema.Schema.Primitive
import zio.schema.SchemaGen._
import zio.test.Assertion._
import zio.test.{ Sized, TestConfig, _ }

object DynamicValueSpec extends ZIOSpecDefault {

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
      suite("deterministic hashCode")(
        test("Record hashCode is stable across multiple calls") {
          val record = DynamicValue.Record(
            TypeId.parse("test.Record"),
            scala.collection.immutable.ListMap(
              "z" -> DynamicValue.Primitive(42, StandardType.IntType),
              "a" -> DynamicValue.Primitive("hello", StandardType.StringType),
              "m" -> DynamicValue.Primitive(true, StandardType.BoolType)
            )
          )
          val hash1 = record.hashCode()
          val hash2 = record.hashCode()
          val hash3 = record.hashCode()
          assertTrue(hash1 == hash2 && hash2 == hash3)
        },
        test("Record hashCode is independent of insertion order") {
          val record1 = DynamicValue.Record(
            TypeId.parse("test.Record"),
            scala.collection.immutable.ListMap(
              "a" -> DynamicValue.Primitive(1, StandardType.IntType),
              "b" -> DynamicValue.Primitive(2, StandardType.IntType)
            )
          )
          val record2 = DynamicValue.Record(
            TypeId.parse("test.Record"),
            scala.collection.immutable.ListMap(
              "b" -> DynamicValue.Primitive(2, StandardType.IntType),
              "a" -> DynamicValue.Primitive(1, StandardType.IntType)
            )
          )
          assertTrue(record1.hashCode() == record2.hashCode())
        },
        test("SetValue hashCode is stable across multiple calls") {
          val setValue = DynamicValue.SetValue(
            Set(
              DynamicValue.Primitive(1, StandardType.IntType),
              DynamicValue.Primitive(2, StandardType.IntType),
              DynamicValue.Primitive(3, StandardType.IntType)
            )
          )
          val hash1 = setValue.hashCode()
          val hash2 = setValue.hashCode()
          val hash3 = setValue.hashCode()
          assertTrue(hash1 == hash2 && hash2 == hash3)
        },
        test("equal DynamicValues have the same hashCode") {
          check(SchemaGen.anyRecordOfRecordsAndValue) {
            case (schema, a) =>
              val dyn1 = schema.toDynamic(a)
              val dyn2 = schema.toDynamic(a)
              assertTrue(dyn1 == dyn2 && dyn1.hashCode() == dyn2.hashCode())
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

}
