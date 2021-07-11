package zio.schema

import scala.collection.immutable.ListMap

import zio._
import zio.schema.SchemaAssertions._
import zio.test._

object SchemaAstSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("SchemaAst")(
    suite("from schema")(
      testM("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ) =>
            assertTrue(SchemaAst.fromSchema(s) == SchemaAst.Value(typ))
        }
      }
    ),
    suite("optional")(
      testM("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ) =>
            assertTrue(SchemaAst.fromSchema(s.optional) == SchemaAst.Value(typ, optional = true))
        }
      }
    ),
    suite("sequence")(
      testM("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ) =>
            assertTrue(SchemaAst.fromSchema(Schema.chunk(s)) == SchemaAst.Value(typ, dimensions = 1))
        }
      }
    ),
    suite("record")(
      test("generic") {
        val schema =
          Schema.GenericRecord(
            Chunk(
              Schema.Field("a", Schema[String]),
              Schema.Field("b", Schema[Int])
            )
          )
        val expectedAst =
          SchemaAst.Product(
            id = schema.hashCode(),
            lineage = Chunk.empty,
            fields = Chunk(
              ("a", SchemaAst.fromSchema(Schema[String])),
              ("b", SchemaAst.fromSchema(Schema[Int]))
            )
          )
        assertTrue(SchemaAst.fromSchema(schema) == expectedAst)
      },
      test("case class") {
        val schema = Schema[SchemaGen.Arity2]
        val expectedAst =
          SchemaAst.Product(
            id = schema.hashCode(),
            lineage = Chunk.empty,
            fields = Chunk(
              "value1" -> SchemaAst.fromSchema(Schema[String]),
              "value2" -> SchemaAst.Product(
                id = Schema[SchemaGen.Arity1].hashCode(),
                lineage = Chunk(schema.hashCode()),
                fields = Chunk(
                  "value" -> SchemaAst.Value(StandardType.IntType)
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
          case SchemaAst.Product(_, _, elements, _, _) =>
            elements.find {
              case ("r", _) => true
              case _        => false
            }.map(_._2)
          case _ => None
        }
        assertTrue(
          recursiveRef.exists(_.id == ast.id)
        )
      }
    ),
    suite("enumeration")(
      test("generic") {
        val schema =
          Schema.Enumeration(ListMap("type1" -> Schema[SchemaGen.Arity1], "type2" -> Schema[SchemaGen.Arity2]))
        val expectedAst =
          SchemaAst.Sum(
            id = schema.hashCode(),
            lineage = Chunk.empty,
            cases = Chunk(
              "type1" -> SchemaAst.Product(
                id = Schema[SchemaGen.Arity1].hashCode(),
                lineage = Chunk(schema.hashCode()),
                fields = Chunk(
                  "value" -> SchemaAst.Value(StandardType.IntType)
                )
              ),
              "type2" -> SchemaAst.Product(
                id = Schema[SchemaGen.Arity2].hashCode(),
                lineage = Chunk(schema.hashCode()),
                fields = Chunk(
                  "value1" -> SchemaAst.Value(StandardType.StringType),
                  "value2" -> SchemaAst.Product(
                    id = Schema[SchemaGen.Arity1].hashCode(),
                    lineage = Chunk(schema.hashCode(), Schema[SchemaGen.Arity2].hashCode()),
                    fields = Chunk(
                      "value" -> SchemaAst.Value(StandardType.IntType)
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
          id = schema.hashCode(),
          lineage = Chunk.empty,
          cases = Chunk(
            "Cat" -> SchemaAst.Product(
              id = Schema[Cat].hashCode(),
              lineage = Chunk(schema.hashCode()),
              fields = Chunk(
                "name"    -> SchemaAst.Value(StandardType.StringType),
                "hasHair" -> SchemaAst.Value(StandardType.BoolType)
              )
            ),
            "Dog" -> SchemaAst.Product(
              id = Schema[Dog].hashCode(),
              lineage = Chunk(schema.hashCode()),
              fields = Chunk("name" -> SchemaAst.Value(StandardType.StringType))
            ),
            "Rock" -> SchemaAst.Value(StandardType.UnitType)
          )
        )
        assertTrue(SchemaAst.fromSchema(schema) == expectedAst)
      }
    ),
    suite("materialization")(
      testM("primitive") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(SchemaAst.fromSchema(schema).toSchema)(hasSameAst(schema))
        }
      },
      testM("optional") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(SchemaAst.fromSchema(schema.optional).toSchema)(hasSameAst(schema.optional))
        }
      },
      testM("sequence") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(SchemaAst.fromSchema(schema.repeated).toSchema)(hasSameAst(schema.repeated))
        }
      },
      testM("tuple") {
        check(SchemaGen.anyPrimitive <*> SchemaGen.anyPrimitive) {
          case (left, right) =>
            assert(SchemaAst.fromSchema(left <*> right).toSchema)(hasSameAst(left <*> right))
        }
      },
      testM("either") {
        check(SchemaGen.anyPrimitive <*> SchemaGen.anyPrimitive) {
          case (left, right) =>
            assert(SchemaAst.fromSchema(left <+> right).toSchema)(hasSameAst(left <+> right))
        }
      },
      testM("case class") {
        check(SchemaGen.anyCaseClassSchema) { schema =>
          assert(SchemaAst.fromSchema(schema).toSchema)(hasSameAst(schema))
        }
      },
      testM("sealed trait") {
        check(SchemaGen.anyEnumSchema) { schema =>
          assert(SchemaAst.fromSchema(schema).toSchema)(hasSameAst(schema))
        }
      },
      testM("recursive type") {
        check(SchemaGen.anyRecursiveType) { schema =>
          assert(SchemaAst.fromSchema(schema).toSchema)(hasSameAst(schema))
        }
      },
      testM("any schema") {
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
