package zio.schema

import scala.collection.immutable.ListMap
import scala.util.hashing.MurmurHash3

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
      suite("hashCode")(
        test("Primitive uses the stable standard type tag") {
          val value = DynamicValue.Primitive(42, StandardType.IntType)
          val seed =
            MurmurHash3.mix(MurmurHash3.stringHash("zio.schema.DynamicValue.Primitive"), StandardType.IntType.tag.##)
          val expected = MurmurHash3.finalizeHash(MurmurHash3.mix(seed, 42.##), 2)

          assertTrue(value.hashCode == expected)
        },
        test("Dictionary hashCode is independent from entry order") {
          val key1 = DynamicValue.Primitive("one", StandardType.StringType)
          val key2 = DynamicValue.Primitive("two", StandardType.StringType)
          val one = DynamicValue.Dictionary(
            Chunk(
              key1 -> DynamicValue.Primitive(1, StandardType.IntType),
              key2 -> DynamicValue.Primitive(2, StandardType.IntType)
            )
          )
          val two = DynamicValue.Dictionary(
            Chunk(
              key2 -> DynamicValue.Primitive(2, StandardType.IntType),
              key1 -> DynamicValue.Primitive(1, StandardType.IntType)
            )
          )

          assertTrue(one.hashCode == two.hashCode)
        },
        test("Record hashCode is independent from field construction order") {
          val id = TypeId.parse("zio.schema.tests.RecordHash")
          val left = DynamicValue.Record(
            id,
            ListMap(
              "a" -> DynamicValue.Primitive(1, StandardType.IntType),
              "b" -> DynamicValue.Primitive("x", StandardType.StringType)
            )
          )
          val right = DynamicValue.Record(
            id,
            ListMap(
              "b" -> DynamicValue.Primitive("x", StandardType.StringType),
              "a" -> DynamicValue.Primitive(1, StandardType.IntType)
            )
          )

          assertTrue(left.hashCode == right.hashCode)
        },
        test("Singleton uses the runtime class name instead of identity") {
          object Marker

          val value    = DynamicValue.Singleton(Marker)
          val expected = MurmurHash3.stringHash(Marker.getClass.getName)

          assertTrue(value.hashCode == expected)
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
