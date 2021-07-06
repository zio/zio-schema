package zio.schema

import scala.collection.immutable.ListMap

import zio._
import zio.schema.SchemaAssertions._
import zio.test._

object AstSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("ast")(
    suite("from schema")(
      testM("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ) =>
            assertTrue(Ast.fromSchema(s) == Ast.Value(typ))
        }
      }
    ),
    suite("optional")(
      testM("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ) =>
            assertTrue(Ast.fromSchema(s.optional) == Ast.Value(typ, true))
        }
      }
    ),
    suite("sequqnce")(
      testM("primitive") {
        check(SchemaGen.anyPrimitive) {
          case s @ Schema.Primitive(typ) =>
            assertTrue(Ast.fromSchema(Schema.chunk(s)) == Ast.Value(typ, false, true))
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
          Ast.Product(
            id = schema.hashCode(),
            lineage = Chunk.empty,
            elements = Chunk(
              ("a", Ast.fromSchema(Schema[String])),
              ("b", Ast.fromSchema(Schema[Int]))
            )
          )
        assertTrue(Ast.fromSchema(schema) == expectedAst)
      },
      test("case class") {
        val schema = Schema[SchemaGen.Arity2]
        val expectedAst =
          Ast.Product(
            id = schema.hashCode(),
            lineage = Chunk.empty,
            elements = Chunk(
              "value1" -> Ast.fromSchema(Schema[String]),
              "value2" -> Ast.Product(
                id = Schema[SchemaGen.Arity1].hashCode(),
                lineage = Chunk(schema.hashCode()),
                elements = Chunk(
                  "value" -> Ast.Value(StandardType.IntType)
                )
              )
            )
          )

        assertTrue(Ast.fromSchema(schema) == expectedAst)
      },
      test("recursive case class") {
        val schema = Schema[Recursive]
        val ast    = Ast.fromSchema(schema)

        val recursiveRef: Option[Ast] = ast match {
          case Ast.Product(_, _, elements, _, _) =>
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
          Ast.Sum(
            id = schema.hashCode(),
            lineage = Chunk.empty,
            cases = Chunk(
              "type1" -> Ast.Product(
                id = Schema[SchemaGen.Arity1].hashCode(),
                lineage = Chunk(schema.hashCode()),
                elements = Chunk(
                  "value" -> Ast.Value(StandardType.IntType)
                )
              ),
              "type2" -> Ast.Product(
                id = Schema[SchemaGen.Arity2].hashCode(),
                lineage = Chunk(schema.hashCode()),
                elements = Chunk(
                  "value1" -> Ast.Value(StandardType.StringType),
                  "value2" -> Ast.Product(
                    id = Schema[SchemaGen.Arity1].hashCode(),
                    lineage = Chunk(schema.hashCode(), Schema[SchemaGen.Arity2].hashCode()),
                    elements = Chunk(
                      "value" -> Ast.Value(StandardType.IntType)
                    )
                  )
                )
              )
            )
          )
        assertTrue(Ast.fromSchema(schema) == expectedAst)
      },
      test("sealed trait") {
        val schema = Schema[Pet]
        val expectedAst = Ast.Sum(
          id = schema.hashCode(),
          lineage = Chunk.empty,
          cases = Chunk(
            "Cat" -> Ast.Product(
              id = Schema[Cat].hashCode(),
              lineage = Chunk(schema.hashCode()),
              elements = Chunk(
                "name"    -> Ast.Value(StandardType.StringType),
                "hasHair" -> Ast.Value(StandardType.BoolType)
              )
            ),
            "Dog" -> Ast.Product(
              id = Schema[Dog].hashCode(),
              lineage = Chunk(schema.hashCode()),
              elements = Chunk("name" -> Ast.Value(StandardType.StringType))
            ),
            "Rock" -> Ast.Value(StandardType.UnitType)
          )
        )
        assertTrue(Ast.fromSchema(schema) == expectedAst)
      }
    ),
    suite("materialization")(
      testM("primitive") {
        check(SchemaGen.anyPrimitive) { schema =>
          assert(Ast.fromSchema(schema).toSchema)(hasSameAst(schema))
        }
      }
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
