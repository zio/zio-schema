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
      suite("toTypedValue with constructor registry")(
        test("converts a record rebuilt from its AST back to a typed value") {
          val user        = RegistryUser("John", 30)
          val userDynamic = RegistryUser.schema.toDynamic(user)
          val registry    = DynamicValue.Constructor.registry(RegistryUser.schema)

          val typed = userDynamic.toTypedValue(registry)(RegistryUser.schema.ast.toSchema)

          assertTrue(typed == Right(user))
        },
        test("decodes to a generic record when the registry has no matching constructor") {
          val userDynamic = RegistryUser.schema.toDynamic(RegistryUser("John", 30))

          val typed = userDynamic.toTypedValue(PartialFunction.empty[TypeId, DynamicValue.Constructor[_]])(
            RegistryUser.schema.ast.toSchema
          )

          assertTrue(typed == Right(ListMap("name" -> "John", "age" -> 30)))
        },
        test("converts nested records, options and sequences rebuilt from their AST") {
          val person = RegistryPerson(
            "John",
            RegistryAddress("Main St", "12345"),
            Some(RegistryAddress("2nd St", "67890")),
            Chunk(RegistryAddress("3rd St", "54321"))
          )
          val personDynamic = RegistryPerson.schema.toDynamic(person)
          val registry      = DynamicValue.Constructor.registry(RegistryPerson.schema, RegistryAddress.schema)

          val typed = personDynamic.toTypedValue(registry)(RegistryPerson.schema.ast.toSchema)

          assertTrue(typed == Right(person))
        },
        test("converts enum cases rebuilt from their AST") {
          val shapes        = Chunk[RegistryShape](RegistryShape.Circle(2.0), RegistryShape.Rectangle(3.0, 4.0))
          val shapesDynamic = Schema[Chunk[RegistryShape]].toDynamic(shapes)
          val registry =
            DynamicValue.Constructor.registry(RegistryShape.Circle.schema, RegistryShape.Rectangle.schema)

          val typed = shapesDynamic.toTypedValue(registry)(Schema[Chunk[RegistryShape]].ast.toSchema)

          assertTrue(typed == Right(shapes))
        },
        test("fails when the dynamic record does not match the registered constructor") {
          val userDynamic = RegistryUser.schema.toDynamic(RegistryUser("John", 30))
          val incomplete = userDynamic match {
            case DynamicValue.Record(id, values) => DynamicValue.Record(id, values - "age")
            case other                           => other
          }
          val registry = DynamicValue.Constructor.registry(RegistryUser.schema)

          val typed = incomplete.toTypedValue(registry)(RegistryUser.schema.ast.toSchema)

          assertTrue(typed.isLeft)
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

  final case class RegistryUser(name: String, age: Int)

  object RegistryUser {
    implicit val schema: Schema[RegistryUser] = DeriveSchema.gen[RegistryUser]
  }

  final case class RegistryAddress(street: String, zip: String)

  object RegistryAddress {
    implicit val schema: Schema[RegistryAddress] = DeriveSchema.gen[RegistryAddress]
  }

  final case class RegistryPerson(
    name: String,
    address: RegistryAddress,
    previousAddress: Option[RegistryAddress],
    otherAddresses: Chunk[RegistryAddress]
  )

  object RegistryPerson {
    implicit val schema: Schema[RegistryPerson] = DeriveSchema.gen[RegistryPerson]
  }

  sealed trait RegistryShape

  object RegistryShape {

    final case class Circle(radius: Double) extends RegistryShape

    object Circle {
      implicit val schema: Schema[Circle] = DeriveSchema.gen[Circle]
    }

    final case class Rectangle(width: Double, height: Double) extends RegistryShape

    object Rectangle {
      implicit val schema: Schema[Rectangle] = DeriveSchema.gen[Rectangle]
    }

    implicit val schema: Schema[RegistryShape] = DeriveSchema.gen[RegistryShape]
  }

}
