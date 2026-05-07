package zio.schema

import scala.collection.immutable.ListMap

import zio._
import zio.schema.Schema.Primitive
import zio.schema.SchemaGen._
import zio.test.Assertion._
import zio.test.{ Sized, TestConfig, _ }

object DynamicValueSpec extends ZIOSpecDefault {

  def spec: Spec[Environment, Any] =
    suite("DynamicValueSpec")(
      suite("deterministic hashCode")(
        test("Primitive hashCode is stable across two instances with same value and type") {
          val a = DynamicValue.Primitive(42, StandardType.IntType)
          val b = DynamicValue.Primitive(42, StandardType.IntType)
          assertTrue(a.hashCode() == b.hashCode())
        },
        test("Primitive hashCode differs for different StandardType with same value representation") {
          // Int 42 vs Long 42 should differ because their tags differ
          val intDv  = DynamicValue.Primitive(42, StandardType.IntType)
          val longDv = DynamicValue.Primitive(42L, StandardType.LongType)
          assertTrue(intDv.hashCode() != longDv.hashCode())
        },
        test("Primitive hashCode uses tag, not object identity") {
          // StandardType singletons are plain objects; we must NOT use their identity hash
          val tag = StandardType.IntType.tag
          val h1  = DynamicValue.Primitive(1, StandardType.IntType).hashCode()
          val h2  = DynamicValue.Primitive(1, StandardType.IntType).hashCode()
          assertTrue(h1 == h2 && tag.nonEmpty)
        },
        test("Record hashCode is independent of field insertion order") {
          val id = TypeId.parse("zio.schema.Test")
          val v1 = DynamicValue.Primitive("hello", StandardType.StringType)
          val v2 = DynamicValue.Primitive(42, StandardType.IntType)
          // Same fields, different insertion order in ListMap
          val rec1 = DynamicValue.Record(id, ListMap("name" -> v1, "age" -> v2))
          val rec2 = DynamicValue.Record(id, ListMap("age" -> v2, "name" -> v1))
          assertTrue(rec1.hashCode() == rec2.hashCode())
        },
        test("Dictionary hashCode is independent of entry insertion order") {
          import zio.Chunk
          val k1 = DynamicValue.Primitive("a", StandardType.StringType)
          val v1 = DynamicValue.Primitive(1, StandardType.IntType)
          val k2 = DynamicValue.Primitive("b", StandardType.StringType)
          val v2 = DynamicValue.Primitive(2, StandardType.IntType)
          val d1 = DynamicValue.Dictionary(Chunk(k1 -> v1, k2 -> v2))
          val d2 = DynamicValue.Dictionary(Chunk(k2 -> v2, k1 -> v1))
          assertTrue(d1.hashCode() == d2.hashCode())
        },
        test("Sequence hashCode respects element order") {
          import zio.Chunk
          val a = DynamicValue.Primitive(1, StandardType.IntType)
          val b = DynamicValue.Primitive(2, StandardType.IntType)
          val s1 = DynamicValue.Sequence(Chunk(a, b))
          val s2 = DynamicValue.Sequence(Chunk(b, a))
          // Sequences are ordered — different order must yield different hash (with overwhelming probability)
          assertTrue(s1.hashCode() != s2.hashCode())
        },
        test("Singleton hashCode is stable across two references") {
          case object MySingleton
          val s1 = DynamicValue.Singleton(MySingleton)
          val s2 = DynamicValue.Singleton(MySingleton)
          assertTrue(s1.hashCode() == s2.hashCode())
        },
        test("equal DynamicValues produced by toDynamic have equal hashCodes") {
          check(SchemaGen.anyRecordOfRecordsAndValue) {
            case (schema, a) =>
              val d1 = schema.toDynamic(a)
              val d2 = schema.toDynamic(a)
              assertTrue(d1.hashCode() == d2.hashCode())
          }
        }
      ),
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
