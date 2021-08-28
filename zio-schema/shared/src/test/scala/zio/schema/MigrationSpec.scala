package zio.schema

import scala.collection.immutable.ListMap

import zio._
import zio.schema.syntax._
import zio.test.AssertionM.Render.param
import zio.test._

object MigrationSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[Environment, Failure] = suite("AstTransformation Spec")(
    suite("Derivation")(
      suite("Value")(
        test("change type") {
          val from = SchemaAst.Value(StandardType.IntType, optional = false, 0)
          val to   = SchemaAst.Value(StandardType.StringType, optional = false, 0)

          assertTrue(
            Migration
              .derive(from, to) == Right(Chunk(Migration.ChangeType(Chunk.empty, StandardType.StringType)))
          )
        },
        test("optional") {
          val from = SchemaAst.Value(StandardType.IntType, optional = false, 0)
          val to   = SchemaAst.Value(StandardType.IntType, optional = true, 0)

          assertTrue(
            Migration
              .derive(from, to) == Right(Chunk(Migration.Optional(Chunk.empty)))
          )
        },
        test("require") {
          val from = SchemaAst.Value(StandardType.IntType, optional = true, 0)
          val to   = SchemaAst.Value(StandardType.IntType, optional = false, 0)

          assertTrue(
            Migration
              .derive(from, to) == Right(Chunk(Migration.Require(Chunk.empty)))
          )
        },
        test("increment dimensions") {
          val from = SchemaAst.Value(StandardType.IntType, optional = true, 0)
          val to   = SchemaAst.Value(StandardType.IntType, optional = true, 2)

          assertTrue(
            Migration
              .derive(from, to) == Right(Chunk(Migration.IncrementDimensions(Chunk.empty, 2)))
          )
        },
        test("decrement dimensions") {
          val from = SchemaAst.Value(StandardType.IntType, optional = true, 2)
          val to   = SchemaAst.Value(StandardType.IntType, optional = true, 0)

          assertTrue(
            Migration
              .derive(from, to) == Right(Chunk(Migration.DecrementDimensions(Chunk.empty, 2)))
          )
        }
      ),
      suite("Record")(
        test("change field type") {
          assertTrue(
            containsTransformation[Nested1, Nested2](Migration.ChangeType(Chunk("v2"), StandardType.IntType))
          )
        },
        test("add node") {
          assertTrue(
            addsNode[Nested1, Nested2](Chunk("v3"))
          )
        },
        test("delete node") {
          assertTrue(
            deletesNode[Nested2, Nested1](Chunk("v3"))
          )
        },
        test("add nested node") {
          assertTrue(
            addsNode[Outer1, Outer2](Chunk("v2", "v3"))
          )
        },
        test("delete nested node") {
          assertTrue(
            deletesNode[Outer2, Outer1](Chunk("v2", "v3"))
          )
        }
      ),
      suite("Sum")(
        test("add node") {
          assertTrue(
            addsNode[Pet1, Pet2](Chunk("Hamster"))
          )
        },
        test("delete node") {
          assertTrue(
            deletesNode[Pet2, Pet1](Chunk("Hamster"))
          )
        }
      )
    ),
    suite("Transformation")(
      test("delete node from record") {
        assert(Migration.DeleteNode(Chunk("v2")))(
          tranformsValueTo(
            Nested1(0, "foo"),
            DynamicValue.Record(ListMap("v1" -> DynamicValue.Primitive(0, StandardType.IntType)))
          )
        )
      },
      test("delete node from nested record") {
        assert(Migration.DeleteNode(Chunk("v2", "v2")))(
          tranformsValueTo(
            Outer1("foo", Nested1(0, "bar")),
            DynamicValue.Record(
              ListMap(
                "v1" -> DynamicValue.Primitive("foo", StandardType.StringType),
                "v2" -> DynamicValue.Record(
                  ListMap(
                    "v1" -> DynamicValue.Primitive(0, StandardType.IntType)
                  )
                )
              )
            )
          )
        )
      },
      test("require node") {
        assert(Migration.Require(Chunk("v2")))(
          tranformsValueTo(
            OptionalField(0, Some("foo")),
            DynamicValue.Record(
              ListMap(
                "v1" -> DynamicValue.Primitive(0, StandardType.IntType),
                "v2" -> DynamicValue.Primitive("foo", StandardType.StringType)
              )
            )
          )
        )
      },
      test("require node fails") {
        assert(Migration.Require(Chunk("v2")))(failsToTransform(OptionalField(0, None)))
      },
      test("optional") {
        assert(Migration.Optional(Chunk("v2")))(
          tranformsValueTo(
            Nested1(0, "foo"),
            DynamicValue.Record(
              ListMap(
                "v1" -> DynamicValue.Primitive(0, StandardType.IntType),
                "v2" -> DynamicValue.SomeValue(DynamicValue.Primitive("foo", StandardType.StringType))
              )
            )
          )
        )
      }
    )
  )

  def containsTransformation[From: Schema, To: Schema](expectedTransform: Migration): Boolean =
    Migration
      .derive(SchemaAst.fromSchema(Schema[From]), SchemaAst.fromSchema(Schema[To]))
      .map(_.contains(expectedTransform))
      .getOrElse(false)

  def addsNode[From: Schema, To: Schema](expectedPath: Chunk[String]): Boolean =
    Migration
      .derive(SchemaAst.fromSchema(Schema[From]), SchemaAst.fromSchema(Schema[To]))
      .map(
        _.exists {
          case Migration.AddNode(path, _) => path == expectedPath
          case _                          => false
        }
      )
      .getOrElse(false)

  def deletesNode[From: Schema, To: Schema](expectedPath: Chunk[String]): Boolean =
    Migration
      .derive(SchemaAst.fromSchema(Schema[From]), SchemaAst.fromSchema(Schema[To]))
      .map(
        _.exists {
          case Migration.DeleteNode(path) => path == expectedPath
          case _                          => false
        }
      )
      .getOrElse(false)

  def tranformsValueTo[A: Schema](value: A, expected: DynamicValue): Assertion[Migration] =
    Assertion.assertion("transformsValueTo")(param(value), param(expected)) { transform =>
      transform.migrate(value.dynamic) == Right(expected)
    }

  def failsToTransform[A: Schema](value: A): Assertion[Migration] =
    Assertion.assertion("failsToTransform")(param(value)) { transform =>
      transform.migrate(value.dynamic).isLeft
    }

  case class Nested1(v1: Int, v2: String)

  object Nested1 {
    implicit def schema: Schema[Nested1] = DeriveSchema.gen
  }
  case class Nested2(v1: Int, v2: Int, v3: String)

  object Nested2 {
    implicit def schema: Schema[Nested2] = DeriveSchema.gen
  }

  case class Outer1(v1: String, v2: Nested1)

  object Outer1 {
    implicit def schema: Schema[Outer1] = DeriveSchema.gen
  }
  case class Outer2(v1: Int, v2: Nested2)

  object Outer2 {
    implicit def schema: Schema[Outer2] = DeriveSchema.gen
  }

  case class OptionalField(v1: Int, v2: Option[String])

  object OptionalField {
    implicit def schema: Schema[OptionalField] = DeriveSchema.gen
  }

  sealed trait Pet1

  object Pet1 {
    case object Dog extends Pet1
    case object Cat extends Pet1

    implicit def schema: Schema[Pet1] = DeriveSchema.gen
  }

  sealed trait Pet2

  object Pet2 {
    case object Dog     extends Pet2
    case object Cat     extends Pet2
    case object Hamster extends Pet2

    implicit def schema: Schema[Pet2] = DeriveSchema.gen
  }

}
