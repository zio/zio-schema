package zio.schema

import scala.collection.immutable.ListMap

import zio._
import zio.schema.ast._
import zio.schema.syntax._
import zio.test.AssertionM.Render.param
import zio.test._

object MigrationSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[Environment, Failure] = suite("Migration Spec")(
    suite("Derivation")(
      suite("Value")(
        test("change type") {
          val from = SchemaAst.Value(StandardType.IntType, NodePath.root)
          val to   = SchemaAst.Value(StandardType.StringType, NodePath.root)

          assertTrue(
            Migration
              .derive(from, to) == Right(Chunk(Migration.ChangeType(NodePath.root, StandardType.StringType)))
          )
        },
        test("optional") {
          val from = SchemaAst.Value(StandardType.IntType, NodePath.root, optional = false)
          val to   = SchemaAst.Value(StandardType.IntType, NodePath.root, optional = true)

          assertTrue(
            Migration
              .derive(from, to) == Right(Chunk(Migration.Optional(NodePath.root)))
          )
        },
        test("require") {
          val from = SchemaAst.Value(StandardType.IntType, NodePath.root, optional = true)
          val to   = SchemaAst.Value(StandardType.IntType, NodePath.root, optional = false)

          assertTrue(
            Migration
              .derive(from, to) == Right(Chunk(Migration.Require(NodePath.root)))
          )
        },
        test("increment dimensions") {
          val from = SchemaAst.Value(StandardType.IntType, NodePath.root, optional = true, dimensions = 0)
          val to   = SchemaAst.Value(StandardType.IntType, NodePath.root, optional = true, dimensions = 2)

          assertTrue(
            Migration
              .derive(from, to) == Right(Chunk(Migration.IncrementDimensions(NodePath.root, 2)))
          )
        },
        test("decrement dimensions") {
          val from = SchemaAst.Value(StandardType.IntType, NodePath.root, optional = true, dimensions = 2)
          val to   = SchemaAst.Value(StandardType.IntType, NodePath.root, optional = true, dimensions = 0)

          assertTrue(
            Migration
              .derive(from, to) == Right(Chunk(Migration.DecrementDimensions(NodePath.root, 2)))
          )
        }
      ),
      suite("Record")(
        test("change field type") {
          assertTrue(
            containsTransformation[Nested1, Nested2](
              Migration.ChangeType(NodePath.root / "v2", StandardType.IntType)
            )
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
        },
        test("delete recursive node") {
          assertTrue(
            includesMigration[Recursive1, Recursive2](
              Migration.Recursive(NodePath.root, NodePath.root / "r", Migration.DeleteNode(NodePath.root / "v2"))
            )
          )
        }
      ),
      suite("Sum")(
        test("add case") {
          assertTrue(
            addsCase[Pet1, Pet2](Chunk("Hamster"))
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
        assert(Migration.DeleteNode(NodePath.root / "v2"))(
          transformsValueTo(
            Nested1(0, "foo"),
            DynamicValue.Record(ListMap("v1" -> DynamicValue.Primitive(0, StandardType.IntType)))
          )
        )
      },
      test("delete node from nested record") {
        assert(Migration.DeleteNode(NodePath.root / "v2" / "v2"))(
          transformsValueTo(
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
      test("delete node recursively") {
        assert(Migration.Recursive(NodePath.root, NodePath.root / "r", Migration.DeleteNode(NodePath.root / "v2")))(
          transformsValueTo(
            Recursive1(0, "", Some(Recursive1(1, "", Some(Recursive1(2, "", None))))),
            DynamicValue.Record(
              ListMap(
                "v1" -> DynamicValue.Primitive(0, StandardType.IntType),
                "v2" -> DynamicValue.Primitive("", StandardType.StringType),
                "r" -> DynamicValue.SomeValue(
                  DynamicValue.Record(
                    ListMap(
                      "v1" -> DynamicValue.Primitive(1, StandardType.IntType),
                      "r" -> DynamicValue.SomeValue(
                        DynamicValue.Record(
                          ListMap(
                            "v1" -> DynamicValue.Primitive(2, StandardType.IntType),
                            "r"  -> DynamicValue.NoneValue
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      },
      test("require node") {
        assert(Migration.Require(NodePath.root / "v2"))(
          transformsValueTo(
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
        assert(Migration.Require(NodePath.root / "v2"))(failsToTransform(OptionalField(0, None)))
      },
      test("optional") {
        assert(Migration.Optional(NodePath.root / "v2"))(
          transformsValueTo(
            Nested1(0, "foo"),
            DynamicValue.Record(
              ListMap(
                "v1" -> DynamicValue.Primitive(0, StandardType.IntType),
                "v2" -> DynamicValue.SomeValue(DynamicValue.Primitive("foo", StandardType.StringType))
              )
            )
          )
        )
      },
      test("ignore add case") {
        val value: Pet1  = Pet1.Dog("name")
        val dynamicValue = value.dynamic
        assert(Migration.AddCase(NodePath.root, SchemaAst.Value(StandardType.UnitType, NodePath.root)))(
          transformsValueTo(value, dynamicValue)
        )
      },
      test("fail to remove instantiated case") {
        val value: Pet2  = Pet2.Hamster("name")
        val dynamicValue = value.dynamic

        assertTrue(Migration.DeleteNode(NodePath.root / "Hamster").migrate(dynamicValue).isLeft)
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

  def addsCase[From: Schema, To: Schema](expectedPath: Chunk[String]): Boolean =
    Migration
      .derive(SchemaAst.fromSchema(Schema[From]), SchemaAst.fromSchema(Schema[To]))
      .map(
        _.exists {
          case Migration.AddCase(path, _) => path == expectedPath
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

  def includesMigration[From: Schema, To: Schema](m: Migration): Boolean =
    Migration
      .derive(SchemaAst.fromSchema(Schema[From]), SchemaAst.fromSchema(Schema[To]))
      .map(
        _.contains(m)
      )
      .getOrElse(false)

  def transformsValueTo[A: Schema](value: A, expected: DynamicValue): Assertion[Migration] =
    Assertion.assertion("transformsValueTo")(param(value), param(expected)) { transform =>
      val transformed = transform.migrate(value.dynamic)
      transformed == Right(expected)
    }

  def failsToTransform[A: Schema](value: A): Assertion[Migration] =
    Assertion.assertion("failsToTransform")(param(value)) { transform =>
      transform.migrate(value.dynamic).isLeft
    }

  case class Recursive1(v1: Int, v2: String, r: Option[Recursive1])

  object Recursive1 {
    implicit lazy val schema: Schema[Recursive1] = DeriveSchema.gen
  }

  case class Recursive2(v1: Int, r: Option[Recursive2])

  object Recursive2 {
    implicit lazy val schema: Schema[Recursive2] = DeriveSchema.gen
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
    case class Dog(name: String) extends Pet1
    case class Cat(name: String) extends Pet1

    implicit def schema: Schema[Pet1] = DeriveSchema.gen
  }

  sealed trait Pet2

  object Pet2 {
    case class Dog(name: String)     extends Pet2
    case class Cat(name: String)     extends Pet2
    case class Hamster(name: String) extends Pet2

    implicit def schema: Schema[Pet2] = DeriveSchema.gen
  }

}
