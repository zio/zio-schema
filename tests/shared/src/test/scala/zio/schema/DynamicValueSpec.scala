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
      // Regression tests for https://github.com/zio/zio-schema/issues/511
      suite("issue-511: toTypedValue via AST-reconstructed schema")(
        test("toTypedValue succeeds when schema is rebuilt from AST with registry") {
          val schema  = Issue511Types.userSchema
          val user    = Issue511Types.User("John", 30)
          val dynamic = schema.toDynamic(user)

          // Without registry: the AST round-trip produces a GenericRecord and
          // toTypedValue returns a ListMap, not a User.  The registry overload
          // of toSchema restores the typed constructor.
          val record   = schema.asInstanceOf[Schema.Record[Issue511Types.User]]
          val registry = Map(record.id -> (schema: Schema[_]))
          val viaAst   = dynamic.toTypedValue(schema.ast.toSchema(registry).asInstanceOf[Schema[Issue511Types.User]])
          assertTrue(viaAst == Right(user))
        },
        test("toTypedValue without registry still yields a value (generic decoding)") {
          val schema  = Issue511Types.userSchema
          val user    = Issue511Types.User("Jane", 25)
          val dynamic = schema.toDynamic(user)

          // Without registry the reconstructed schema is GenericRecord; decoding
          // succeeds but returns a ListMap — that is the expected generic-mode
          // behaviour and should not throw.
          val result = dynamic.toTypedValue(schema.ast.toSchema)
          assertTrue(result.isRight)
        },
        test("toTypedValue with registry works for nested case classes") {
          val addressSchema = Issue511Types.addressSchema
          val personSchema  = Issue511Types.personSchema
          val person        = Issue511Types.Person("Alice", Issue511Types.Address("Main St", "12345"))
          val dynamic       = personSchema.toDynamic(person)

          val registry = Map(
            personSchema.asInstanceOf[Schema.Record[Issue511Types.Person]].id   -> (personSchema: Schema[_]),
            addressSchema.asInstanceOf[Schema.Record[Issue511Types.Address]].id -> (addressSchema: Schema[_])
          )
          val viaAst = dynamic.toTypedValue(personSchema.ast.toSchema(registry).asInstanceOf[Schema[Issue511Types.Person]])
          assertTrue(viaAst == Right(person))
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

/** Types used by the issue-511 regression tests (must be top-level to allow macro derivation). */
private object Issue511Types {
  final case class User(name: String, age: Int)
  val userSchema: Schema[User] = DeriveSchema.gen[User]

  final case class Address(street: String, zip: String)
  val addressSchema: Schema[Address] = DeriveSchema.gen[Address]

  final case class Person(name: String, address: Address)
  val personSchema: Schema[Person] = DeriveSchema.gen[Person]
}
