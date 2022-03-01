package zio.schema

import zio.Chunk
import zio.schema.syntax._
import zio.test.Assertion
import zio.test.AssertionM.Render.param

object SchemaAssertions {

  def migratesTo[A: Schema, B: Schema](expected: B): Assertion[A] =
    Assertion.assertion("migratesTo")(param(expected)) { value =>
      value.migrate[B] match {
        case Left(_)                   => false
        case Right(m) if m != expected => false
        case _ =>
          true
      }
    }

  def cannotMigrateValue[A: Schema, B: Schema]: Assertion[A] =
    Assertion.assertion("cannotMigrateTo")() { value =>
      value.migrate[B].isLeft
    }

  def hasSameSchema(expected: Schema[_]): Assertion[Schema[_]] =
    Assertion.assertion("hasSameSchema")(param(expected))(
      actual => Schema.strictEquality.equal(expected, actual)
    )

  def hasSameSchemaStructure(expected: Schema[_]): Assertion[Schema[_]] =
    Assertion.assertion("hasSameSchemaStructure")(param(expected))(
      actual => Schema.structureEquality.equal(expected, actual)
    )

  def hasSameAst(expected: Schema[_]): Assertion[Schema[_]] =
    Assertion.assertion("hasSameAst")(param(expected))(actual => equalsAst(expected, actual))

  private def equalsAst(expected: Schema[_], actual: Schema[_], depth: Int = 0): Boolean = (expected, actual) match {
    case (Schema.Primitive(StandardType.Duration(_), _), Schema.Primitive(StandardType.Duration(_), _))       => true
    case (Schema.Primitive(StandardType.InstantType(_), _), Schema.Primitive(StandardType.InstantType(_), _)) => true
    case (Schema.Primitive(StandardType.LocalDateType(_), _), Schema.Primitive(StandardType.LocalDateType(_), _)) =>
      true
    case (Schema.Primitive(StandardType.LocalTimeType(_), _), Schema.Primitive(StandardType.LocalTimeType(_), _)) =>
      true
    case (
        Schema.Primitive(StandardType.LocalDateTimeType(_), _),
        Schema.Primitive(StandardType.LocalDateTimeType(_), _)
        ) =>
      true
    case (
        Schema.Primitive(StandardType.ZonedDateTimeType(_), _),
        Schema.Primitive(StandardType.ZonedDateTimeType(_), _)
        ) =>
      true
    case (Schema.Primitive(StandardType.OffsetTimeType(_), _), Schema.Primitive(StandardType.OffsetTimeType(_), _)) =>
      true
    case (
        Schema.Primitive(StandardType.OffsetDateTimeType(_), _),
        Schema.Primitive(StandardType.OffsetDateTimeType(_), _)
        ) =>
      true
    case (Schema.Primitive(tpe1, _), Schema.Primitive(tpe2, _))     => tpe1 == tpe2
    case (Schema.Optional(expected, _), Schema.Optional(actual, _)) => equalsAst(expected, actual, depth)
    case (Schema.Tuple(expectedLeft, expectedRight, _), Schema.Tuple(actualLeft, actualRight, _)) =>
      equalsAst(expectedLeft, actualLeft, depth) && equalsAst(expectedRight, actualRight, depth)
    case (Schema.Tuple(expectedLeft, expectedRight, _), Schema.GenericRecord(structure, _)) =>
      structure.toChunk.size == 2 &&
        structure.toChunk.find(_.label == "left").exists(f => equalsAst(expectedLeft, f.schema, depth)) &&
        structure.toChunk.find(_.label == "right").exists(f => equalsAst(expectedRight, f.schema, depth))
    case (Schema.EitherSchema(expectedLeft, expectedRight, _), Schema.EitherSchema(actualLeft, actualRight, _)) =>
      equalsAst(expectedLeft, actualLeft, depth) && equalsAst(expectedRight, actualRight, depth)
    case (Schema.EitherSchema(expectedLeft, expectedRight, _), right: Schema.Enum[_]) =>
      right.structure.size == 2 &&
        right.structure.get("left").exists(actualLeft => equalsAst(expectedLeft, actualLeft, depth)) &&
        right.structure.get("right").exists(actualRight => equalsAst(expectedRight, actualRight, depth))
    case (Schema.Sequence(expected, _, _, _, _), Schema.Sequence(actual, _, _, _, _)) =>
      equalsAst(expected, actual, depth)
    case (expected: Schema.Record[_], actual: Schema.Record[_]) =>
      expected.structure.zipAll(actual.structure).forall {
        case (Some(Schema.Field(expectedLabel, expectedSchema, _)), Some(Schema.Field(actualLabel, actualSchema, _))) =>
          expectedLabel == actualLabel && equalsAst(expectedSchema, actualSchema, depth)
        case _ => false
      }
    case (expected: Schema.Enum[_], actual: Schema.Enum[_]) =>
      Chunk.fromIterable(expected.structure).zipAll(Chunk.fromIterable(actual.structure)).forall {
        case (Some((expectedId, expectedSchema)), Some((actualId, actualSchema))) =>
          actualId == expectedId && equalsAst(expectedSchema, actualSchema, depth)
        case _ => false
      }
    case (expected, Schema.Transform(actualSchema, _, _, _, _)) =>
      equalsAst(expected, actualSchema, depth)
    case (Schema.Transform(expected, _, _, _, _), actual) =>
      equalsAst(expected, actual, depth)
    case (expected: Schema.Lazy[_], actual) => if (depth > 10) true else equalsAst(expected.schema, actual, depth + 1)
    case (expected, actual: Schema.Lazy[_]) => if (depth > 10) true else equalsAst(expected, actual.schema, depth + 1)
    case _                                  => false
  }
}
