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
      suite("validate")(
        // ── Property: validate agrees with toTypedValue ─────────────────────
        test("returns Right(()) for any well-formed leaf value") {
          check(SchemaGen.anyLeafAndValue) {
            case (schema, a) =>
              assertTrue(schema.toDynamic(a).validate(schema) == Right(()))
          }
        },
        test("returns Right(()) for any well-formed record value") {
          check(SchemaGen.anyRecordOfRecordsAndValue) {
            case (schema, a) =>
              assertTrue(schema.toDynamic(a).validate(schema) == Right(()))
          }
        },
        test("succeeds whenever toTypedValue succeeds (consistency property)") {
          check(SchemaGen.anyTree(1).flatMap(s => DynamicValueGen.anyDynamicValueOfSchema(s).map(s -> _))) {
            case (schema, dynamic) =>
              if (dynamic.toTypedValue(schema).isRight)
                assertTrue(dynamic.validate(schema) == Right(()))
              else
                assertCompletes
          }
        },
        // ── Primitives ──────────────────────────────────────────────────────
        test("primitive type match returns Right(())") {
          val dv = DynamicValue.Primitive(42, StandardType.IntType)
          assertTrue(dv.validate(Schema.primitive[Int]) == Right(()))
        },
        test("primitive type mismatch returns exactly one error") {
          val dv     = DynamicValue.Primitive("oops", StandardType.StringType)
          val errors = dv.validate(Schema.primitive[Int]).left.toOption.get
          assertTrue(errors.size == 1) &&
            assertTrue(errors(0).contains("mismatch"))
        },
        // ── Optional ────────────────────────────────────────────────────────
        test("NoneValue is always valid for Optional") {
          assertTrue(DynamicValue.NoneValue.validate(Schema.Optional(Schema.primitive[Int])) == Right(()))
        },
        test("SomeValue with wrong inner type returns one error") {
          val dv     = DynamicValue.SomeValue(DynamicValue.Primitive("bad", StandardType.StringType))
          val errors = dv.validate(Schema.Optional(Schema.primitive[Int])).left.toOption.get
          assertTrue(errors.size == 1)
        },
        // ── Sequence ────────────────────────────────────────────────────────
        test("Sequence with all valid elements returns Right(())") {
          val dv = DynamicValue.Sequence(
            Chunk(
              DynamicValue.Primitive(1, StandardType.IntType),
              DynamicValue.Primitive(2, StandardType.IntType)
            )
          )
          assertTrue(dv.validate(Schema.chunk(Schema.primitive[Int])) == Right(()))
        },
        test("Sequence accumulates errors from every invalid element") {
          val dv = DynamicValue.Sequence(
            Chunk(
              DynamicValue.Primitive("x", StandardType.StringType),
              DynamicValue.Primitive(1, StandardType.IntType),
              DynamicValue.Primitive("y", StandardType.StringType)
            )
          )
          val errors = dv.validate(Schema.chunk(Schema.primitive[Int])).left.toOption.get
          assertTrue(errors.size == 2)
        },
        // ── Tuple ───────────────────────────────────────────────────────────
        test("Tuple with both sides valid returns Right(())") {
          val dv = DynamicValue.Tuple(
            DynamicValue.Primitive(1, StandardType.IntType),
            DynamicValue.Primitive("hello", StandardType.StringType)
          )
          assertTrue(dv.validate(Schema.tuple2(Schema.primitive[Int], Schema.primitive[String])) == Right(()))
        },
        test("Tuple accumulates errors from both sides") {
          val dv = DynamicValue.Tuple(
            DynamicValue.Primitive("bad", StandardType.StringType),
            DynamicValue.Primitive(42, StandardType.IntType)
          )
          val errors = dv.validate(Schema.tuple2(Schema.primitive[Int], Schema.primitive[String])).left.toOption.get
          assertTrue(errors.size == 2)
        },
        // ── Either ──────────────────────────────────────────────────────────
        test("LeftValue validates against the left schema") {
          val dv = DynamicValue.LeftValue(DynamicValue.Primitive(1, StandardType.IntType))
          assertTrue(dv.validate(Schema.either(Schema.primitive[Int], Schema.primitive[String])) == Right(()))
        },
        test("LeftValue with wrong type returns one error") {
          val dv     = DynamicValue.LeftValue(DynamicValue.Primitive("bad", StandardType.StringType))
          val errors = dv.validate(Schema.either(Schema.primitive[Int], Schema.primitive[String])).left.toOption.get
          assertTrue(errors.size == 1)
        },
        test("RightValue validates against the right schema") {
          val dv = DynamicValue.RightValue(DynamicValue.Primitive("ok", StandardType.StringType))
          assertTrue(dv.validate(Schema.either(Schema.primitive[Int], Schema.primitive[String])) == Right(()))
        },
        // ── Record ──────────────────────────────────────────────────────────
        test("Record with all correct fields returns Right(())") {
          val schema = types.Arities.Arity1.schema
          val dv = DynamicValue.Record(
            TypeId.Structural,
            ListMap("f1" -> DynamicValue.Primitive(42, StandardType.IntType))
          )
          assertTrue(dv.validate(schema) == Right(()))
        },
        test("Record with missing required field reports a dotted-path error") {
          val schema = types.Arities.Arity1.schema
          val dv     = DynamicValue.Record(TypeId.Structural, ListMap.empty)
          val errors = dv.validate(schema).left.toOption.get
          assertTrue(errors.size == 1) &&
            assertTrue(errors(0).contains("f1")) &&
            assertTrue(errors(0).contains("missing"))
        },
        test("Record accumulates errors from all invalid fields") {
          val schema = types.Arities.Arity2.schema
          val dv = DynamicValue.Record(
            TypeId.Structural,
            ListMap(
              "f1" -> DynamicValue.Primitive("wrong", StandardType.StringType),
              "f2" -> DynamicValue.Primitive(99, StandardType.IntType)
            )
          )
          val errors = dv.validate(schema).left.toOption.get
          assertTrue(errors.size == 2)
        },
        // ── Enumeration ─────────────────────────────────────────────────────
        test("Enumeration with unknown case returns one error naming the case") {
          val unknownCase = "___nonExistentCase___"
          val dv          = DynamicValue.Enumeration(TypeId.Structural, unknownCase -> DynamicValue.NoneValue)
          val errors      = dv.validate(DynamicValue.schema).left.toOption.get
          assertTrue(errors.size == 1) &&
            assertTrue(errors(0).contains(unknownCase))
        },
        // ── DynamicValue.Error ───────────────────────────────────────────────
        test("DynamicValue.Error always fails validation") {
          val errors = DynamicValue.Error("boom").validate(Schema.primitive[Int]).left.toOption.get
          assertTrue(errors.size == 1) &&
            assertTrue(errors(0).contains("boom"))
        },
        // ── Lazy ────────────────────────────────────────────────────────────
        test("Lazy schema delegates to its inner schema") {
          val dv = DynamicValue.Primitive(7, StandardType.IntType)
          assertTrue(dv.validate(Schema.defer(Schema.primitive[Int])) == Right(()))
        },
        // ── Transform ───────────────────────────────────────────────────────
        test("Transform delegates to the inner schema") {
          val transformedSchema = Schema
            .primitive[String]
            .transformOrFail[Int](s => s.toIntOption.toRight("not an int"), i => Right(i.toString))
          val dv = DynamicValue.Primitive("hello", StandardType.StringType)
          assertTrue(dv.validate(transformedSchema) == Right(()))
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
