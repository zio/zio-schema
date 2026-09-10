package zio.schema

import zio.schema.syntax._
import zio.test.Assertion

object SchemaAssertions {

  def migratesTo[A: Schema, B: Schema](expected: B): Assertion[A] =
    Assertion.assertion("migratesTo") { value =>
      value.migrate[B] match {
        case Left(_)                   => false
        case Right(m) if m != expected => false
        case _                         =>
          true
      }
    }

  def cannotMigrateValue[A: Schema, B: Schema]: Assertion[A] =
    Assertion.assertion("cannotMigrateTo") { value =>
      value.migrate[B].isLeft
    }

  def hasSameSchema(expected: Schema[_]): Assertion[Schema[_]] =
    Assertion.assertion("hasSameSchema")(actual => Schema.strictEquality.equal(expected, actual))

  def hasSameSchemaStructure(expected: Schema[_]): Assertion[Schema[_]] =
    Assertion.assertion("hasSameSchemaStructure")(actual => Schema.structureEquality.equal(expected, actual))

  def hasSameAst(expected: Schema[_]): Assertion[Schema[_]] =
    Assertion.assertion("hasSameAst")(actual => equalsAst(expected, actual))

  private def equalsAst(expected: Schema[_], actual: Schema[_], depth: Int = 0): Boolean = (expected, actual) match {
    case (Schema.Primitive(StandardType.DurationType, _), Schema.Primitive(StandardType.DurationType, _))   => true
    case (Schema.Primitive(StandardType.InstantType, _), Schema.Primitive(StandardType.InstantType, _))     => true
    case (Schema.Primitive(StandardType.LocalDateType, _), Schema.Primitive(StandardType.LocalDateType, _)) =>
      true
    case (Schema.Primitive(StandardType.LocalTimeType, _), Schema.Primitive(StandardType.LocalTimeType, _)) =>
      true
    case (
          Schema.Primitive(StandardType.LocalDateTimeType, _),
          Schema.Primitive(StandardType.LocalDateTimeType, _)
        ) =>
      true
    case (
          Schema.Primitive(StandardType.ZonedDateTimeType, _),
          Schema.Primitive(StandardType.ZonedDateTimeType, _)
        ) =>
      true
    case (Schema.Primitive(StandardType.OffsetTimeType, _), Schema.Primitive(StandardType.OffsetTimeType, _)) =>
      true
    case (
          Schema.Primitive(StandardType.OffsetDateTimeType, _),
          Schema.Primitive(StandardType.OffsetDateTimeType, _)
        ) =>
      true
    case (Schema.Primitive(tpe1, _), Schema.Primitive(tpe2, _))                                     => tpe1 == tpe2
    case (Schema.Optional(expected, _), Schema.Optional(actual, _))                                 => equalsAst(expected, actual, depth)
    case (Schema.Tuple2(expectedLeft, expectedRight, _), Schema.Tuple2(actualLeft, actualRight, _)) =>
      equalsAst(expectedLeft, actualLeft, depth) && equalsAst(expectedRight, actualRight, depth)
    case (Schema.Tuple2(expectedLeft, expectedRight, _), Schema.GenericRecord(_, structure, _)) =>
      structure.toChunk.size == 2 &&
      structure.toChunk.find(_.name == "left").exists(f => equalsAst(expectedLeft, f.schema, depth)) &&
      structure.toChunk.find(_.name == "right").exists(f => equalsAst(expectedRight, f.schema, depth))
    case (Schema.Either(expectedLeft, expectedRight, _), Schema.Either(actualLeft, actualRight, _)) =>
      equalsAst(expectedLeft, actualLeft, depth) && equalsAst(expectedRight, actualRight, depth)
    case (Schema.Either(expectedLeft, expectedRight, _), right: Schema.Enum[_]) =>
      right.cases.size == 2 &&
      right.caseOf("left").exists(actualLeft => equalsAst(expectedLeft, actualLeft.schema, depth)) &&
      right.caseOf("right").exists(actualRight => equalsAst(expectedRight, actualRight.schema, depth))
    case (Schema.Sequence(expected, _, _, _, _), Schema.Sequence(actual, _, _, _, _)) =>
      equalsAst(expected, actual, depth)
    case (expected: Schema.Record[_], actual: Schema.Record[_]) =>
      expected.fields.zipAll(actual.fields).forall {
        case (
              Some(Schema.Field(expectedLabel, expectedSchema, _, _, _, _)),
              Some(Schema.Field(actualLabel, actualSchema, _, _, _, _))
            ) =>
          expectedLabel == actualLabel && equalsAst(expectedSchema, actualSchema, depth)
        case _ => false
      }
    case (expected: Schema.Enum[_], actual: Schema.Enum[_]) =>
      expected.cases.zipAll(actual.cases).forall {
        case (Some(expectedCase), Some(actualCase)) =>
          actualCase.id == expectedCase.id && equalsAst(expectedCase.schema, actualCase.schema, depth)
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
