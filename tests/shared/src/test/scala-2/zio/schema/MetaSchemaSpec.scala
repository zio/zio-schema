package zio.schema

import zio._
import zio.schema.CaseSet._
import zio.schema.SchemaAssertions._
import zio.schema.meta.{MetaSchema, NodePath}
import zio.test._

object MetaSchemaSpec extends ZIOSpecDefault {

  def spec: Spec[Environment, Any] = suite("MetaSchema")(
    suite("from schema")(
      test("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ, _) =>
            assertTrue(MetaSchema.fromSchema(s) == MetaSchema.Value(typ))
        }
      }
    ),
    suite("optional")(
      test("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ, _) =>
            assertTrue(MetaSchema.fromSchema(s.optional) == MetaSchema.Value(typ, optional = true))
        }
      }
    ),
    suite("sequence")(
      test("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ, _) =>
            assertTrue(
              MetaSchema.fromSchema(Schema.chunk(s)) == MetaSchema
                .ListNode(MetaSchema.Value(typ, path = NodePath.root / "item"), NodePath.root)
            )
        }
      }
    ),
    suite("record")(
      test("generic") {
        val schema =
          Schema.record(
            TypeId.parse("zio.schema.MetaSchema.Product"),
            Chunk(
              Schema.Field[_ <: Singleton with String]("a", Schema[String]),
              Schema.Field[_ <: Singleton with Int]("b", Schema[Int])
            ): _*
          )
        val expectedAst =
          MetaSchema.Product(
            id = TypeId.parse("zio.schema.MetaSchema.Product"),
            path = NodePath.root,
            fields = Chunk(
              ("a", MetaSchema.Value(StandardType.StringType, NodePath.root / "a")),
              ("b", MetaSchema.Value(StandardType.IntType, NodePath.root / "b"))
            )
          )
        assertTrue(MetaSchema.fromSchema(schema) == expectedAst)
      },
      test("case class") {
        val schema = Schema[SchemaGen.Arity2]
        val expectedAst =
          MetaSchema.Product(
            id = TypeId.parse("zio.schema.SchemaGen.Arity2"),
            path = NodePath.root,
            fields = Chunk(
              "value1" -> MetaSchema.Value(StandardType.StringType, NodePath.root / "value1"),
              "value2" -> MetaSchema.Product(
                id = TypeId.parse("zio.schema.SchemaGen.Arity1"),
                path = NodePath.root / "value2",
                fields = Chunk(
                  "value" -> MetaSchema.Value(StandardType.IntType, NodePath.root / "value2" / "value")
                )
              )
            )
          )

        assertTrue(MetaSchema.fromSchema(schema) == expectedAst)
      },
      test("recursive case class") {
        val schema = Schema[Recursive]
        val ast    = MetaSchema.fromSchema(schema)

        val recursiveRef: Option[MetaSchema] = ast match {
          case MetaSchema.Product(_, _, elements, _) =>
            elements.find {
              case ("r", _) => true
              case _        => false
            }.map(_._2)
          case _ => None
        }
        assertTrue(
          recursiveRef.exists {
            case MetaSchema.Ref(pathRef, _, _) => pathRef == Chunk.empty
            case _                             => false
          }
        )
      }
    ),
    suite("enumeration")(
      test("generic") {
        val schema =
          Schema.enumeration[Any, CaseSet.Aux[Any]](
            TypeId.Structural,
            caseOf[SchemaGen.Arity1, Any]("type1")(_.asInstanceOf[SchemaGen.Arity1]) ++ caseOf[SchemaGen.Arity2, Any](
              "type2"
            )(_.asInstanceOf[SchemaGen.Arity2])
          )
        val expectedAst =
          MetaSchema.Sum(
            TypeId.Structural,
            path = NodePath.root,
            cases = Chunk(
              "type1" -> MetaSchema.Product(
                id = TypeId.parse("zio.schema.SchemaGen.Arity1"),
                path = NodePath.root / "type1",
                fields = Chunk(
                  "value" -> MetaSchema.Value(StandardType.IntType, NodePath.root / "type1" / "value")
                )
              ),
              "type2" -> MetaSchema.Product(
                id = TypeId.parse("zio.schema.SchemaGen.Arity2"),
                path = NodePath.root / "type2",
                fields = Chunk(
                  "value1" -> MetaSchema.Value(StandardType.StringType, NodePath.root / "type2" / "value1"),
                  "value2" -> MetaSchema.Product(
                    id = TypeId.parse("zio.schema.SchemaGen.Arity1"),
                    path = NodePath.root / "type2" / "value2",
                    fields = Chunk(
                      "value" -> MetaSchema
                        .Value(StandardType.IntType, NodePath.root / "type2" / "value2" / "value")
                    )
                  )
                )
              )
            )
          )
        assertTrue(MetaSchema.fromSchema(schema) == expectedAst)
      },
      test("sealed trait") {
        val schema = Schema[Pet]
        val expectedAst = MetaSchema.Sum(
          TypeId.parse("zio.schema.MetaSchemaSpec.Pet"),
          path = NodePath.root,
          cases = Chunk(
            "Rock" -> MetaSchema.Value(StandardType.UnitType, NodePath.root / "Rock"),
            "Dog" -> MetaSchema.Product(
              id = TypeId.parse("zio.schema.MetaSchemaSpec.Dog"),
              path = NodePath.root / "Dog",
              fields = Chunk("name" -> MetaSchema.Value(StandardType.StringType, NodePath.root / "Dog" / "name"))
            ),
            "Cat" -> MetaSchema.Product(
              id = TypeId.parse("zio.schema.MetaSchemaSpec.Cat"),
              path = NodePath.root / "Cat",
              fields = Chunk(
                "name"    -> MetaSchema.Value(StandardType.StringType, NodePath.root / "Cat" / "name"),
                "hasHair" -> MetaSchema.Value(StandardType.BoolType, NodePath.root / "Cat" / "hasHair")
              )
            )
          )
        )
        assertTrue(MetaSchema.fromSchema(schema) == expectedAst)
      }
    ),
    suite("materialization")(
      test("simple recursive product") {
        val schema = Recursive.schema
        assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema.asInstanceOf[Schema[_]]))
      },
      test("simple recursive sum") {
        val schema = RecursiveSum.schema
        assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema.asInstanceOf[Schema[_]]))
      },
      test("primitive") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema.asInstanceOf[Schema[_]]))
        }
      },
      test("optional") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(MetaSchema.fromSchema(schema.optional).toSchema)(hasSameSchemaStructure(schema.optional))
        }
      },
      test("sequence") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(MetaSchema.fromSchema(schema.repeated).toSchema)(hasSameSchemaStructure(schema.repeated))
        }
      },
      test("tuple") {
        check(SchemaGen.anyPrimitive <*> SchemaGen.anyPrimitive) {
          case (left, right) =>
            assert(MetaSchema.fromSchema(left <*> right).toSchema)(hasSameSchemaStructure(left <*> right))
        }
      },
      test("either") {
        check(SchemaGen.anyPrimitive <*> SchemaGen.anyPrimitive) {
          case (left, right) =>
            assert(MetaSchema.fromSchema(left <+> right).toSchema)(hasSameSchemaStructure(left <+> right))
        }
      },
      test("case class") {
        check(SchemaGen.anyCaseClassSchema) { schema =>
          assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema))
        }
      },
      test("sealed trait") {
        check(SchemaGen.anyEnumSchema) { schema =>
          assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema))
        }
      },
      test("recursive type") {
        check(SchemaGen.anyRecursiveType) { schema =>
          assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema))
        }
      },
      test("any schema") {
        check(SchemaGen.anySchema) { schema =>
          assert(MetaSchema.fromSchema(schema).toSchema)(hasSameSchemaStructure(schema))
        }
      } @@ TestAspect.shrinks(0),
      test("sequence of optional primitives") {
        val schema       = Schema[List[Option[Int]]]
        val materialized = MetaSchema.fromSchema(schema).toSchema
        assert(materialized)(hasSameSchemaStructure(schema))
      },
      test("optional sequence of primitives") {
        val schema       = Schema[Option[List[String]]]
        val materialized = MetaSchema.fromSchema(schema).toSchema
        assert(materialized)(hasSameSchemaStructure(schema))
      }
    )
  )

  case class Recursive(id: String, r: Option[Recursive])

  object Recursive {
    implicit lazy val schema: Schema[Recursive] = DeriveSchema.gen[Recursive]
  }

  sealed trait RecursiveSum

  object RecursiveSum {
    case object Leaf                                                              extends RecursiveSum
    final case class Node(label: String, left: RecursiveSum, right: RecursiveSum) extends RecursiveSum

    implicit lazy val schema: Schema[RecursiveSum] = DeriveSchema.gen[RecursiveSum]
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
