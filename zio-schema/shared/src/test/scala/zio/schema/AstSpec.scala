package zio.schema

import zio._
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
          Ast.Node(
            id = schema.hashCode(),
            lineage = Chunk.empty,
            children = Chunk(
              Ast.LabelledNode("a", Ast.fromSchema(Schema[String])),
              Ast.LabelledNode("b", Ast.fromSchema(Schema[Int]))
            )
          )
        assertTrue(Ast.fromSchema(schema) == expectedAst)
      },
      test("case class") {
        val schema = Schema[SchemaGen.Arity2]
        val expectedAst =
          Ast.Node(
            id = schema.hashCode(),
            lineage = Chunk.empty,
            children = Chunk(
              Ast.LabelledNode("value1", Ast.fromSchema(Schema[String])),
              Ast.LabelledNode(
                "value2",
                Ast.fromSchema(Schema[SchemaGen.Arity1]).asInstanceOf[Ast.Node].copy(lineage = Chunk(schema.hashCode()))
              )
            )
          )
        assertTrue(Ast.fromSchema(schema) == expectedAst)
      }
    ),
    suite("recursive structures")(
      test("recursive case class") {
        val schema = Schema[Recursive]
        val ast    = Ast.fromSchema(schema)

        val recursiveRef: Option[Ast] = ast match {
          case Ast.Node(_, _, children, _, _) =>
            children.find {
              case Ast.LabelledNode("r", _) => true
              case _                        => false
            }
          case _ => None
        }
        assertTrue(
          recursiveRef.exists(_.id == ast.id)
        )
      }
    )
  )

  case class Recursive(id: String, r: Option[Recursive])

  object Recursive {
    implicit lazy val schema: Schema[Recursive] = DeriveSchema.gen[Recursive]
  }

}
