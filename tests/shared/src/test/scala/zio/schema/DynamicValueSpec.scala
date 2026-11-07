package zio.schema

import zio._
import zio.schema.Schema.Primitive
import zio.schema.SchemaGen._
import zio.test.Assertion._
import zio.test.{Sized, TestConfig, _}

object DynamicValueSpec extends ZIOSpecDefault {

  def spec: Spec[Environment, Any] =
    suite("DynamicValueSpec")(
      suite("round-trip")(
        suite("Primitives")(primitiveTests: _*),
        test("round-trips Records") {
          check(SchemaGen.anyRecordOfRecordsAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips Enumerations") {
          check(SchemaGen.anyEnumerationAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips Eithers") {
          check(SchemaGen.anyEitherAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips Tuples") {
          check(SchemaGen.anyTupleAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips Optionals") {
          check(SchemaGen.anyOptionalAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips Transform") {
          check(SchemaGen.anyTransformAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips CaseClass") {
          check(SchemaGen.anyCaseClassAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips Enum") {
          check(SchemaGen.anyEnumAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips any un-nested schema") {
          check(SchemaGen.anyLeafAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips any nested schema") {
          check(SchemaGen.anyTree(1).flatMap(s => DynamicValueGen.anyDynamicValueOfSchema(s).map(s -> _))) {
            case (schema, dynamic) =>
              assert(schema.fromDynamic(dynamic))(isRight)
          }
        },
        test("round-trips recursive data types") {
          check(SchemaGen.anyRecursiveTypeAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips sequence") {
          check(SchemaGen.anySequenceAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips set") {
          check(SchemaGen.anySetAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
          }
        },
        test("round-trips map") {
          check(SchemaGen.anyMapAndValue) { case (schema, a) =>
            assert(schema.fromDynamic(schema.toDynamic(a)))(isRight(equalTo(a)))
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
      suite("toTypedValue with Constructor registry (#511)")(
        test("reconstructs a typed case class when schema is rebuilt from AST") {
          val user        = AstUser("John", 30)
          val userDynamic = AstUser.schema.toDynamic(user)
          val registry    = DynamicValue.Constructor.registry(AstUser.schema)

          val typed = userDynamic.toTypedValue(registry)(AstUser.schema.ast.toSchema)

          assertTrue(typed == Right(user))
        },
        test("automatically extracts nested record and enum case constructors from parent schema") {
          val person = AstPerson(
            "John",
            AstAddress("Main St", "12345"),
            Some(AstAddress("2nd St", "67890")),
            Chunk(AstShape.Circle(2.5), AstShape.Rectangle(3.0, 4.0))
          )
          val personDynamic = AstPerson.schema.toDynamic(person)
          val registry      = DynamicValue.Constructor.registry(AstPerson.schema)

          val typed = personDynamic.toTypedValue(registry)(AstPerson.schema.ast.toSchema)

          assertTrue(typed == Right(person))
        },
        test("falls back to ListMap when TypeId is not present in registry") {
          val userDynamic = AstUser.schema.toDynamic(AstUser("John", 30))
          val emptyReg    = PartialFunction.empty[TypeId, DynamicValue.Constructor[_]]

          val typed = userDynamic.toTypedValue(emptyReg)(AstUser.schema.ast.toSchema)

          assertTrue(typed == Right(scala.collection.immutable.ListMap("name" -> "John", "age" -> 30)))
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


  final case class AstUser(name: String, age: Int)
  object AstUser {
    implicit val schema: Schema[AstUser] = DeriveSchema.gen[AstUser]
  }

  final case class AstAddress(street: String, zip: String)
  object AstAddress {
    implicit val schema: Schema[AstAddress] = DeriveSchema.gen[AstAddress]
  }

  sealed trait AstShape
  object AstShape {
    final case class Circle(radius: Double)                    extends AstShape
    final case class Rectangle(width: Double, height: Double) extends AstShape
    implicit val schema: Schema[AstShape] = DeriveSchema.gen[AstShape]
  }

  final case class AstPerson(
    name: String,
    address: AstAddress,
    previousAddress: Option[AstAddress],
    shapes: Chunk[AstShape]
  )
  object AstPerson {
    implicit val schema: Schema[AstPerson] = DeriveSchema.gen[AstPerson]
  }
}
