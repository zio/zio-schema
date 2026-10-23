package zio.schema

import scala.collection.immutable.ListMap

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
      suite("schema rebuilt from AST")(
        test("reconstructs a record through a constructor registry") {
          val user        = AstUser("John", 30)
          val userDynamic = AstUser.schema.toDynamic(user)
          val registry    = DynamicValue.ConstructorRegistry.fromSchemas(AstUser.schema)

          val typed = userDynamic.toTypedValue(registry)(AstUser.schema.ast.toSchema)

          assertTrue(typed == Right(user)) &&
          assertTrue(userDynamic.toValue(registry)(AstUser.schema.ast.toSchema).toOption.exists(_ == user)) &&
          assertTrue(userDynamic.toTypedValueOption(registry)(AstUser.schema.ast.toSchema).exists(_ == user))
        },
        test("decodes to a generic record when the record is not registered") {
          val userDynamic = AstUser.schema.toDynamic(AstUser("John", 30))

          val withoutRegistry =
            userDynamic.toTypedValue(AstUser.schema.ast.toSchema)
          val withEmptyRegistry =
            userDynamic.toTypedValue(DynamicValue.ConstructorRegistry.empty)(AstUser.schema.ast.toSchema)

          assertTrue(withoutRegistry == Right(ListMap("name" -> "John", "age" -> 30))) &&
          assertTrue(withEmptyRegistry == Right(ListMap("name" -> "John", "age" -> 30)))
        },
        test("reconstructs records nested in options, sequences, maps and either from a root-built registry") {
          val person = AstPerson(
            "John",
            AstAddress("Main St", "12345"),
            Some(AstAddress("2nd St", "67890")),
            Chunk(AstAddress("3rd St", "54321")),
            Map("home" -> AstAddress("4th St", "11111")),
            Right(AstAddress("5th St", "22222"))
          )
          val personDynamic = AstPerson.schema.toDynamic(person)
          val registry      = DynamicValue.ConstructorRegistry.fromSchemas(AstPerson.schema)

          val typed = personDynamic.toTypedValue(registry)(AstPerson.schema.ast.toSchema)

          assertTrue(typed == Right(person))
        },
        test("reconstructs recursive records") {
          val list        = AstNode(1, Some(AstNode(2, None)))
          val listDynamic = AstNode.schema.toDynamic(list)
          val registry    = DynamicValue.ConstructorRegistry.fromSchemas(AstNode.schema)

          val typed = listDynamic.toTypedValue(registry)(AstNode.schema.ast.toSchema)

          assertTrue(typed == Right(list))
        },
        test("reconstructs enumeration cases, including case objects") {
          val shapes        = Chunk[AstShape](AstShape.Circle(2.0), AstShape.Point)
          val shapesDynamic = Schema[Chunk[AstShape]].toDynamic(shapes)
          val registry      = DynamicValue.ConstructorRegistry.fromSchemas(Schema[Chunk[AstShape]])

          val typed = shapesDynamic.toTypedValue(registry)(Schema[Chunk[AstShape]].ast.toSchema)

          assertTrue(typed == Right(shapes))
        },
        test("fails when the dynamic record misses a field required by the registered schema") {
          val incomplete = AstUser.schema.toDynamic(AstUser("John", 30)) match {
            case DynamicValue.Record(id, values) => DynamicValue.Record(id, values - "age")
            case other                           => other
          }
          val registry = DynamicValue.ConstructorRegistry.fromSchemas(AstUser.schema)

          val typed = incomplete.toTypedValue(registry)(AstUser.schema.ast.toSchema)

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

  final case class AstUser(name: String, age: Int)

  object AstUser {
    implicit val schema: Schema[AstUser] = DeriveSchema.gen[AstUser]
  }

  final case class AstAddress(street: String, zip: String)

  object AstAddress {
    implicit val schema: Schema[AstAddress] = DeriveSchema.gen[AstAddress]
  }

  final case class AstPerson(
    name: String,
    address: AstAddress,
    previous: Option[AstAddress],
    others: Chunk[AstAddress],
    aliases: Map[String, AstAddress],
    contact: Either[String, AstAddress]
  )

  object AstPerson {
    implicit val schema: Schema[AstPerson] = DeriveSchema.gen[AstPerson]
  }

  final case class AstNode(value: Int, next: Option[AstNode])

  object AstNode {
    implicit val schema: Schema[AstNode] = DeriveSchema.gen[AstNode]
  }

  sealed trait AstShape

  object AstShape {
    final case class Circle(radius: Double) extends AstShape

    object Circle {
      implicit val schema: Schema[Circle] = DeriveSchema.gen[Circle]
    }

    case object Point extends AstShape

    implicit val schema: Schema[AstShape] = DeriveSchema.gen[AstShape]
  }

}
