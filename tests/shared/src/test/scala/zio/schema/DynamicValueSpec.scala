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
      suite("AST-rebuilt schema")(
        test("recovers a record rebuilt from its AST through a registry") {
          val user        = RegUser("John", 30)
          val userDynamic = RegUser.schema.toDynamic(user)
          val registry    = DynamicValue.registry(RegUser.schema)
          val rebuilt     = RegUser.schema.ast.toSchema.asInstanceOf[Schema[RegUser]]

          assertTrue(
            userDynamic.toTypedValue(registry)(rebuilt) == Right(user),
            userDynamic.toValue(registry)(rebuilt) == Right(user),
            userDynamic.toTypedValueOption(registry)(rebuilt).contains(user)
          )
        },
        test("decodes a generic record when the registry has no entry") {
          val userDynamic = RegUser.schema.toDynamic(RegUser("John", 30))

          val withoutRegistry =
            userDynamic.toTypedValue(RegUser.schema.ast.toSchema)
          val withEmptyRegistry =
            userDynamic.toTypedValue(PartialFunction.empty[TypeId, Schema[_]])(RegUser.schema.ast.toSchema)

          assertTrue(
            withoutRegistry == Right(ListMap("name" -> "John", "age" -> 30)),
            withEmptyRegistry == Right(ListMap("name" -> "John", "age" -> 30))
          )
        },
        test("recovers nested records, options, sequences, maps, and eithers from a root-built registry") {
          val person = RegPerson(
            "John",
            RegAddress("Main St", "12345"),
            Some(RegAddress("2nd St", "67890")),
            Chunk(RegAddress("3rd St", "54321")),
            Map("home" -> RegAddress("4th St", "11111")),
            Right(RegAddress("5th St", "22222"))
          )
          val personDynamic = RegPerson.schema.toDynamic(person)
          val registry      = DynamicValue.registry(RegPerson.schema)

          val typed = personDynamic.toTypedValue(registry)(RegPerson.schema.ast.toSchema)

          assertTrue(typed == Right(person))
        },
        test("recovers recursive records rebuilt from their AST") {
          val list        = RegNode(1, Some(RegNode(2, None)))
          val listDynamic = RegNode.schema.toDynamic(list)
          val registry    = DynamicValue.registry(RegNode.schema)

          val typed = listDynamic.toTypedValue(registry)(RegNode.schema.ast.toSchema)

          assertTrue(typed == Right(list))
        },
        test("recovers enum cases (including case objects) rebuilt from their AST") {
          val shapes        = Chunk[RegShape](RegShape.Circle(2.0), RegShape.Rectangle(3.0, 4.0), RegShape.Point)
          val shapesDynamic = Schema[Chunk[RegShape]].toDynamic(shapes)
          val registry      = DynamicValue.registry(Schema[Chunk[RegShape]])

          val typed = shapesDynamic.toTypedValue(registry)(Schema[Chunk[RegShape]].ast.toSchema)

          assertTrue(typed == Right(shapes))
        },
        test("fails when the dynamic record does not match the registered schema") {
          val userDynamic = RegUser.schema.toDynamic(RegUser("John", 30))
          val incomplete  = userDynamic match {
            case DynamicValue.Record(id, values) => DynamicValue.Record(id, values - "age")
            case other                           => other
          }
          val registry = DynamicValue.registry(RegUser.schema)

          val typed = incomplete.toTypedValue(registry)(RegUser.schema.ast.toSchema)

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

  final case class RegUser(name: String, age: Int)

  object RegUser {
    implicit val schema: Schema[RegUser] = DeriveSchema.gen[RegUser]
  }

  final case class RegAddress(street: String, zip: String)

  object RegAddress {
    implicit val schema: Schema[RegAddress] = DeriveSchema.gen[RegAddress]
  }

  final case class RegPerson(
    name: String,
    address: RegAddress,
    previous: Option[RegAddress],
    others: Chunk[RegAddress],
    aliases: Map[String, RegAddress],
    contact: Either[String, RegAddress]
  )

  object RegPerson {
    implicit val schema: Schema[RegPerson] = DeriveSchema.gen[RegPerson]
  }

  final case class RegNode(value: Int, next: Option[RegNode])

  object RegNode {
    implicit val schema: Schema[RegNode] = DeriveSchema.gen[RegNode]
  }

  sealed trait RegShape

  object RegShape {
    final case class Circle(radius: Double) extends RegShape

    object Circle {
      implicit val schema: Schema[Circle] = DeriveSchema.gen[Circle]
    }

    final case class Rectangle(width: Double, height: Double) extends RegShape

    object Rectangle {
      implicit val schema: Schema[Rectangle] = DeriveSchema.gen[Rectangle]
    }

    case object Point extends RegShape

    implicit val schema: Schema[RegShape] = DeriveSchema.gen[RegShape]
  }

}
