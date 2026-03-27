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
      suite("deterministic hashCode")(
        test("Primitive hashCode is stable across separate instances") {
          val a = DynamicValue.Primitive(42, StandardType.IntType)
          val b = DynamicValue.Primitive(42, StandardType.IntType)
          assertTrue(a.hashCode() == b.hashCode()) && assertTrue(a == b)
        },
        test("Primitive hashCode uses tag, not identity") {
          // Two calls must yield the same hash, proving we don't use System.identityHashCode
          val h1 = DynamicValue.Primitive("hello", StandardType.StringType).hashCode()
          val h2 = DynamicValue.Primitive("hello", StandardType.StringType).hashCode()
          assertTrue(h1 == h2)
        },
        test("different Primitive values have different hashCodes") {
          val a = DynamicValue.Primitive(1, StandardType.IntType)
          val b = DynamicValue.Primitive(2, StandardType.IntType)
          assertTrue(a.hashCode() != b.hashCode())
        },
        test("Dictionary hashCode is order-independent") {
          val k1 = DynamicValue.Primitive("a", StandardType.StringType)
          val v1 = DynamicValue.Primitive(1, StandardType.IntType)
          val k2 = DynamicValue.Primitive("b", StandardType.StringType)
          val v2 = DynamicValue.Primitive(2, StandardType.IntType)
          val d1 = DynamicValue.Dictionary(Chunk((k1, v1), (k2, v2)))
          val d2 = DynamicValue.Dictionary(Chunk((k2, v2), (k1, v1)))
          assertTrue(d1.hashCode() == d2.hashCode()) && assertTrue(d1 == d2)
        },
        test("Dictionary with different entries are not equal") {
          val k1 = DynamicValue.Primitive("a", StandardType.StringType)
          val v1 = DynamicValue.Primitive(1, StandardType.IntType)
          val k2 = DynamicValue.Primitive("b", StandardType.StringType)
          val v2 = DynamicValue.Primitive(2, StandardType.IntType)
          val v3 = DynamicValue.Primitive(3, StandardType.IntType)
          val d1 = DynamicValue.Dictionary(Chunk((k1, v1), (k2, v2)))
          val d2 = DynamicValue.Dictionary(Chunk((k1, v1), (k2, v3)))
          assertTrue(d1 != d2)
        },
        test("equivalent dynamic values from toDynamic have equal hashCode") {
          check(SchemaGen.anyTreeAndValue) {
            case (schema, value) =>
              val dyn1 = schema.toDynamic(value)
              val dyn2 = schema.toDynamic(value)
              assertTrue(dyn1.hashCode() == dyn2.hashCode()) && assertTrue(dyn1 == dyn2)
          }
        },
        test("hashCode/equals contract: equal objects have equal hashCodes") {
          check(SchemaGen.anyLeafAndValue) {
            case (schema, value) =>
              val dyn1 = schema.toDynamic(value)
              val dyn2 = schema.toDynamic(value)
              assertTrue(
                !(dyn1 == dyn2) || (dyn1.hashCode() == dyn2.hashCode())
              )
          }
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

}
