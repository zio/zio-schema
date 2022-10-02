package zio.schema

import zio._
import zio.schema.Schema.Primitive
import zio.schema.SchemaGen._
import zio.test.Assertion._
import zio.test.{ Sized, TestConfig, _ }

object DynamicValueSpec extends ZIOSpecDefault {

  trait Roundtrip {
    val name: String
    def apply[A](schema: Schema[A], a: A): TestResult
  }

  object Roundtrip {

    object simple extends Roundtrip {
      val name = "simple"

      def apply[A](schema: Schema[A], a: A) =
        assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
    }

    object serialized extends Roundtrip {
      val name = "serialized"

      override def apply[A](schema: Schema[A], a: A): TestResult = {
        val schemaAst = schema.ast
        val schema2   = schemaAst.toSchema
        assert(schema2.fromDynamic(schema.toDynamic(a)).asInstanceOf[Either[String, A]])(isRight(equalTo(a)))
      }
    }
  }

  def spec: Spec[Environment, Any] =
    suite("DynamicValueSpec")(
      roundtripTestsCore(Roundtrip.simple),
      roundtripTestsUserDefined(Roundtrip.simple),
      roundtripTestsCore(Roundtrip.serialized) @@ TestAspect.sized(10) @@ TestAspect.samples(10)
    ) @@ TestAspect.sequential

  def roundtripTestsCore(roundtrip: Roundtrip) =
    suite(s"Core types - ${roundtrip.name}")(
      suite("Primitives")(primitiveTests(roundtrip): _*),
      test("round-trips Records") {
        check(SchemaGen.anyRecordOfRecordsAndValue) {
          case (schema, a) =>
            roundtrip(schema, a)
        }
      },
      test("round-trips Enumerations") {
        check(SchemaGen.anyEnumerationAndValue) {
          case (schema, a) =>
            roundtrip(schema, a)
        }
      },
      test("round-trips Eithers") {
        check(SchemaGen.anyEitherAndValue) {
          case (schema, a) => roundtrip(schema, a)
        }
      },
      test("round-trips Tuples") {
        check(SchemaGen.anyTupleAndValue) {
          case (schema, a) => roundtrip(schema, a)
        }
      },
      test("round-trips tuple-3") {
        check(SchemaGen.anyPrimitiveAndValue, SchemaGen.anyPrimitiveAndValue, SchemaGen.anyPrimitiveAndValue) {
          case ((schema1, a), (schema2, b), (schema3, c)) =>
            val tupleSchema = Schema.tuple3(schema1, schema2, schema3)
            roundtrip(tupleSchema, (a, b, c))
        }
      },
      test("round-trips nested tuple-2") {
        check(SchemaGen.anyTupleAndValue, SchemaGen.anyTupleAndValue) {
          case ((schema1, a), (schema2, b)) =>
            val tupleSchema = Schema.tuple2(schema1, schema2)
            roundtrip(tupleSchema, (a, b))
        }
      },
      test("round-trips nested tuple-3") {
        check(SchemaGen.anyTupleAndValue, SchemaGen.anyTupleAndValue, SchemaGen.anyTupleAndValue) {
          case ((schema1, a), (schema2, b), (schema3, c)) =>
            val tupleSchema = Schema.tuple3(schema1, schema2, schema3)
            roundtrip(tupleSchema, (a, b, c))
        }
      },
      test("round-trips Optionals") {
        check(SchemaGen.anyOptionalAndValue) {
          case (schema, a) => roundtrip(schema, a)
        }
      },
      test("round-trips any un-nested schema") {
        check(SchemaGen.anyLeafAndValue) {
          case (schema, a) => roundtrip(schema, a)
        }
      },
      test("round-trips any nested schema") {
        check(SchemaGen.anyTree(1).flatMap(s => DynamicValueGen.anyDynamicValueOfSchema(s).map(s -> _))) {
          case (schema, dynamic) =>
            assert(schema.fromDynamic(dynamic))(isRight)
        }
      },
      test("round-trip semiDynamic") {
        val gen = for {
          schemaAndGen       <- SchemaGen.anyGenericRecordAndGen()
          (schema, valueGen) = schemaAndGen
          value              <- valueGen
        } yield schema -> value
        check(gen) {
          case (schema, value) =>
            val semiDynamicSchema = Schema.semiDynamic(defaultValue = Right(value -> schema))
            roundtrip(semiDynamicSchema, value -> schema)
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
    )

  def roundtripTestsUserDefined(roundtrip: Roundtrip) =
    suite(s"User defined types - ${roundtrip.name}")(
      test("round-trips recursive data types") {
        check(SchemaGen.anyRecursiveTypeAndValue) {
          case (schema, a) =>
            roundtrip(schema, a)
        }
      },
      test("round-trips CaseClass") {
        check(SchemaGen.anyCaseClassAndValue) {
          case (schema, a) => roundtrip(schema, a)
        }
      },
      test("round-trips Enum") {
        check(SchemaGen.anyEnumAndValue) {
          case (schema, a) => roundtrip(schema, a)
        }
      },
      test("round-trips Transform") {
        check(SchemaGen.anyTransformAndValue) {
          case (schema, a) => roundtrip(schema, a)
        }
      }
    )

  def primitiveTests(roundtrip: Roundtrip): List[Spec[Sized with TestConfig, Nothing]] = schemasAndGens.map {
    case SchemaTest(name, standardType, gen) =>
      test(s"round-trips $name") {
        check(gen) { a =>
          roundtrip(Primitive(standardType, Chunk.empty), a)
        }
      }
  }
}
