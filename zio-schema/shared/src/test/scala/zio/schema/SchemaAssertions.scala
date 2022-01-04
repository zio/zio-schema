package zio.schema

import scala.collection.immutable.ListMap

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

  def hasSameSchema[A](expected: Schema[A]): Assertion[Schema[A]] =
    Assertion.assertion("hasSameSchema")(param(expected))(actual => equalsSchema(expected, actual))

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
    case (Schema.Sequence(expected, _, _, _), Schema.Sequence(actual, _, _, _)) => equalsAst(expected, actual, depth)
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
    case (expected, Schema.Transform(actualSchema, _, _, _)) =>
      equalsAst(expected, actualSchema, depth)
    case (Schema.Transform(expected, _, _, _), actual) =>
      equalsAst(expected, actual, depth)
    case (expected: Schema.Lazy[_], actual) => if (depth > 10) true else equalsAst(expected.schema, actual, depth + 1)
    case (expected, actual: Schema.Lazy[_]) => if (depth > 10) true else equalsAst(expected, actual.schema, depth + 1)
    case _                                  => false
  }

  private def equalsSchema[A](left: Schema[A], right: Schema[A]): Boolean =
    (left: Schema[_], right: Schema[_]) match {
      case (Schema.Transform(codec1, _, _, a1), Schema.Transform(codec2, _, _, a2)) =>
        equalsSchema(codec1, codec2) && equalsAnnotations(a1, a2)
      case (Schema.GenericRecord(structure1, a1), Schema.GenericRecord(structure2, a2)) =>
        hasSameFields(structure1.toChunk, structure2.toChunk) &&
          structure1.toChunk.forall {
            case Schema.Field(label, schema, _) =>
              val left: Schema[Any]  = schema.asInstanceOf[Schema[Any]]
              val right: Schema[Any] = structure2.toChunk.find(_.label == label).asInstanceOf[Schema[Any]]
              equalsSchema(left, right)
          } && equalsAnnotations(a1, a2)
      case (left: Schema.Record[_], right: Schema.Record[_]) =>
        hasSameStructure(left.asInstanceOf[Schema.Record[A]], right.asInstanceOf[Schema.Record[A]]) &&
          hasSameAnnotations(left.annotations.toList, right.annotations.toList)
      case (Schema.Sequence(element1, _, _, a1), Schema.Sequence(element2, _, _, a2)) =>
        equalsSchema(element1, element2) && equalsAnnotations(a1, a2)
      case (Schema.Primitive(standardType1, a1), Schema.Primitive(standardType2, a2)) =>
        standardType1 == standardType2 && equalsAnnotations(a1, a2)
      case (Schema.Tuple(left1, right1, a1), Schema.Tuple(left2, right2, a2)) =>
        equalsSchema(left1, left2) && equalsSchema(right1, right2) && equalsAnnotations(a1, a2)
      case (Schema.Optional(codec1, _), Schema.Optional(codec2, _)) => equalsSchema(codec1, codec2)
      case (l: Schema.Enum[_], r: Schema.Enum[_])                   => hasSameCases(l.structure, r.structure)
      case (l @ Schema.Lazy(_), r @ Schema.Lazy(_)) =>
        equalsSchema(l.schema.asInstanceOf[Schema[Any]], r.schema.asInstanceOf[Schema[Any]])
      case (lazySchema @ Schema.Lazy(_), eagerSchema) =>
        equalsSchema(lazySchema.schema.asInstanceOf[Schema[Any]], eagerSchema.asInstanceOf[Schema[Any]])
      case (eagerSchema, lazySchema @ Schema.Lazy(_)) =>
        equalsSchema(lazySchema.asInstanceOf[Schema[Any]], eagerSchema.asInstanceOf[Schema[Any]])
      case _ => false
    }

  private def equalsAnnotations(l: Chunk[Any], r: Chunk[Any]): Boolean = l.equals(r)

  private def hasSameCases(left: ListMap[String, Schema[_]], right: ListMap[String, Schema[_]]): Boolean =
    left.zip(right).forall {
      case ((lLabel, lSchema), (rLabel, rSchema)) =>
        lLabel == rLabel &&
          equalsSchema(lSchema.asInstanceOf[Schema[Any]], rSchema.asInstanceOf[Schema[Any]])
    }

  private def hasSameStructure[A](left: Schema.Record[A], right: Schema.Record[A]): Boolean =
    left.structure.zip(right.structure).forall {
      case (Schema.Field(lLabel, lSchema, lAnnotations), Schema.Field(rLabel, rSchema, rAnnotations)) =>
        lLabel == rLabel && hasSameAnnotations(lAnnotations.toList, rAnnotations.toList) && equalsSchema(
          lSchema,
          rSchema
        )
    }

  private def hasSameFields(left: Chunk[Schema.Field[_]], right: Chunk[Schema.Field[_]]): Boolean =
    left.map(_.label) == right.map(_.label)

  private def hasSameAnnotations(left: List[Any], right: List[Any]): Boolean = (left, right) match {
    case (Nil, Nil) => true
    case (lhead :: ltail, rhead :: rtail) if lhead.isInstanceOf[Product] && rhead.isInstanceOf[Product] =>
      (lhead == rhead) && hasSameAnnotations(ltail, rtail)
    case (lhead :: ltail, rhead :: rtail) =>
      (lhead.getClass.getCanonicalName == rhead.getClass.getCanonicalName) && hasSameAnnotations(ltail, rtail)
    case _ => false
  }

}
