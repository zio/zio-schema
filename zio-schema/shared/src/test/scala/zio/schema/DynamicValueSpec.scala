package zio.schema

import zio._
import zio.random.Random
import zio.schema.Schema.Primitive
import zio.schema.SchemaGen._
import zio.test.Assertion._
import zio.test._

object DynamicValueSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("DynamicValueSpec")(
      suite("Primitives")(primitiveTests: _*),
      testM("round-trips Records") {
        check(SchemaGen.anyRecordOfRecordsAndValue) {
          case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips Enumerations") {
        check(SchemaGen.anyEnumerationAndValue) {
          case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips Eithers") {
        check(SchemaGen.anyEitherAndValue) {
          case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips Tuples") {
        check(SchemaGen.anyTupleAndValue) {
          case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips Optionals") {
        check(SchemaGen.anyOptionalAndValue) {
          case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips Transform") {
        check(SchemaGen.anyTransformAndValue) {
          case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips CaseClass") {
        check(SchemaGen.anyCaseClassAndValue) {
          case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips Enum") {
        check(SchemaGen.anyEnumAndValue) {
          case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips any un-nested schema") {
        check(SchemaGen.anyLeafAndValue) {
          case (schema, a) => assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
        }
      },
      testM("round-trips any nested schema") {
        check(SchemaGen.anyTree(1).flatMap(s => DynamicValueGen.anyDynamicValueOfSchema(s).map(s -> _))) {
          case (schema, dynamic) =>
            assert(schema.fromDynamic(dynamic))(isRight)
        }
      },
      testM("round-trips recursive data types") {
        check(SchemaGen.anyRecursiveTypeAndValue) {
          case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
        }
      }
    )

  val primitiveTests: List[ZSpec[Sized with Random with TestConfig, Nothing]] = schemasAndGens.map {
    case SchemaTest(name, standardType, gen) =>
      testM(s"round-trips $name") {
        dynamicValueLaw(gen, Primitive(standardType, Chunk.empty))
      }
  }

  private def dynamicValueLaw[R, A](gen: Gen[R, A], schema: Schema[A]): URIO[R with TestConfig, TestResult] =
    check(gen) { a =>
      assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
    }

}
