package zio.schema

import zio._
import zio.schema.CaseSet._
import zio.schema.SchemaAssertions._
import zio.schema.ast._
import zio.test._

object SchemaAstSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("SchemaAst")(
    suite("from schema")(
      test("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ, _) =>
            assertTrue(SchemaAst.fromSchema(s) == SchemaAst.Value(typ))
        }
      }
    ),
    suite("optional")(
      test("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ, _) =>
            assertTrue(SchemaAst.fromSchema(s.optional) == SchemaAst.Value(typ, optional = true))
        }
      }
    ),
    suite("sequence")(
      test("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ, _) =>
            assertTrue(SchemaAst.fromSchema(Schema.chunk(s)) == SchemaAst.Value(typ, dimensions = 1))
        }
      }
    ),
    suite("record")(
      test("generic") {
        val schema =
          Schema.record(
            Chunk(
              Schema.Field("a", Schema[String]),
              Schema.Field("b", Schema[Int])
            ): _*
          )
        val expectedAst =
          SchemaAst.Product(
            path = NodePath.root,
            fields = Chunk(
              ("a", SchemaAst.Value(StandardType.StringType, NodePath.root / "a")),
              ("b", SchemaAst.Value(StandardType.IntType, NodePath.root / "b"))
            )
          )
        assertTrue(SchemaAst.fromSchema(schema) == expectedAst)
      },
      test("case class") {
        val schema = Schema[SchemaGen.Arity2]
        val expectedAst =
          SchemaAst.Product(
            path = NodePath.root,
            fields = Chunk(
              "value1" -> SchemaAst.Value(StandardType.StringType, NodePath.root / "value1"),
              "value2" -> SchemaAst.Product(
                path = NodePath.root / "value2",
                fields = Chunk(
                  "value" -> SchemaAst.Value(StandardType.IntType, NodePath.root / "value2" / "value")
                )
              )
            )
          )

        assertTrue(SchemaAst.fromSchema(schema) == expectedAst)
      },
      test("recursive case class") {
        val schema = Schema[Recursive]
        val ast    = SchemaAst.fromSchema(schema)

        val recursiveRef: Option[SchemaAst] = ast match {
          case SchemaAst.Product(_, elements, _, _) =>
            elements.find {
              case ("r", _) => true
              case _        => false
            }.map(_._2)
          case _ => None
        }
        assertTrue(
          recursiveRef.exists {
            case SchemaAst.Ref(pathRef, _, _, _) => pathRef == Chunk.empty
            case _                               => false
          }
        )
      }
    ),
    suite("enumeration")(
      test("generic") {
        val schema =
          Schema.enumeration[Any, CaseSet.Aux[Any]](
            caseOf[SchemaGen.Arity1, Any]("type1")(_.asInstanceOf[SchemaGen.Arity1]) ++ caseOf[SchemaGen.Arity2, Any](
              "type2"
            )(_.asInstanceOf[SchemaGen.Arity2])
          )
        val expectedAst =
          SchemaAst.Sum(
            path = NodePath.root,
            cases = Chunk(
              "type1" -> SchemaAst.Product(
                path = NodePath.root / "type1",
                fields = Chunk(
                  "value" -> SchemaAst.Value(StandardType.IntType, NodePath.root / "type1" / "value")
                )
              ),
              "type2" -> SchemaAst.Product(
                path = NodePath.root / "type2",
                fields = Chunk(
                  "value1" -> SchemaAst.Value(StandardType.StringType, NodePath.root / "type2" / "value1"),
                  "value2" -> SchemaAst.Product(
                    path = NodePath.root / "type2" / "value2",
                    fields = Chunk(
                      "value" -> SchemaAst
                        .Value(StandardType.IntType, NodePath.root / "type2" / "value2" / "value")
                    )
                  )
                )
              )
            )
          )
        assertTrue(SchemaAst.fromSchema(schema) == expectedAst)
      },
      test("sealed trait") {
        val schema = Schema[Pet]
        val expectedAst = SchemaAst.Sum(
          path = NodePath.root,
          cases = Chunk(
            "Cat" -> SchemaAst.Product(
              path = NodePath.root / "Cat",
              fields = Chunk(
                "name"    -> SchemaAst.Value(StandardType.StringType, NodePath.root / "Cat" / "name"),
                "hasHair" -> SchemaAst.Value(StandardType.BoolType, NodePath.root / "Cat" / "hasHair")
              )
            ),
            "Dog" -> SchemaAst.Product(
              path = NodePath.root / "Dog",
              fields = Chunk("name" -> SchemaAst.Value(StandardType.StringType, NodePath.root / "Dog" / "name"))
            ),
            "Rock" -> SchemaAst.Value(StandardType.UnitType, NodePath.root / "Rock")
          )
        )
        assertTrue(SchemaAst.fromSchema(schema) == expectedAst)
      }
    ),
    suite("materialization")(
      test("primitive") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(SchemaAst.fromSchema(schema).toSchema)(hasSameAst(schema))
        }
      },
      test("optional") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(SchemaAst.fromSchema(schema.optional).toSchema)(hasSameAst(schema.optional))
        }
      },
      test("sequence") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(SchemaAst.fromSchema(schema.repeated).toSchema)(hasSameAst(schema.repeated))
        }
      },
      test("tuple") {
        check(SchemaGen.anyPrimitive <*> SchemaGen.anyPrimitive) {
          case (left, right) =>
            assert(SchemaAst.fromSchema(left <*> right).toSchema)(hasSameAst(left <*> right))
        }
      },
      test("either") {
        check(SchemaGen.anyPrimitive <*> SchemaGen.anyPrimitive) {
          case (left, right) =>
            assert(SchemaAst.fromSchema(left <+> right).toSchema)(hasSameAst(left <+> right))
        }
      },
      test("case class") {
        check(SchemaGen.anyCaseClassSchema) { schema =>
          assert(SchemaAst.fromSchema(schema).toSchema)(hasSameAst(schema))
        }
      },
      test("sealed trait") {
        check(SchemaGen.anyEnumSchema) { schema =>
          assert(SchemaAst.fromSchema(schema).toSchema)(hasSameAst(schema))
        }
      },
      test("recursive type") {
        check(SchemaGen.anyRecursiveType) { schema =>
          assert(SchemaAst.fromSchema(schema).toSchema)(hasSameAst(schema))
        }
      },
      test("any schema") {
        check(SchemaGen.anySchema) { schema =>
          assert(SchemaAst.fromSchema(schema).toSchema)(hasSameAst(schema))
        }
      } @@ TestAspect.shrinks(0)
    )
  )

  case class Recursive(id: String, r: Option[Recursive])

  object Recursive {
    implicit lazy val schema: Schema[Recursive] = DeriveSchema.gen[Recursive]
  }

  sealed trait Pet
  case object Rock             extends Pet
  case class Dog(name: String) extends Pet

  object Dog {
    implicit lazy val schema: Schema[Dog] = DeriveSchema.gen[Dog]
  }
  case class Cat(name: String, hasHair: Boolean) extends Pet

  object Cat {
    implicit lazy val schema: Schema[Cat] = DeriveSchema.gen[Cat]
  }

  object Pet {
    implicit lazy val schema: Schema[Pet] = DeriveSchema.gen[Pet]
  }

}
