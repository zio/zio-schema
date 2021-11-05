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

  def hasSameSchema[A](expected: Schema[A]): Assertion[Schema[A]] =
    Assertion.assertion("hasSameSchema")(param(expected))(actual => equalsSchema(expected, actual))

  def hasSameAst(expected: Schema[_]): Assertion[Schema[_]] =
    Assertion.assertion("hasSameAst")(param(expected))(actual => equalsAst(expected, actual))

  private def equalsAst(expected: Schema[_], actual: Schema[_], depth: Int = 0): Boolean = (expected, actual) match {
    case (Schema.Primitive(StandardType.Duration(_), _), Schema.Primitive(StandardType.Duration(_), _))   => true
    case (Schema.Primitive(StandardType.Instant(_), _), Schema.Primitive(StandardType.Instant(_), _))     => true
    case (Schema.Primitive(StandardType.LocalDate(_), _), Schema.Primitive(StandardType.LocalDate(_), _)) => true
    case (Schema.Primitive(StandardType.LocalTime(_), _), Schema.Primitive(StandardType.LocalTime(_), _)) => true
    case (Schema.Primitive(StandardType.LocalDateTime(_), _), Schema.Primitive(StandardType.LocalDateTime(_), _)) =>
      true
    case (Schema.Primitive(StandardType.ZonedDateTime(_), _), Schema.Primitive(StandardType.ZonedDateTime(_), _)) =>
      true
    case (Schema.Primitive(StandardType.OffsetTime(_), _), Schema.Primitive(StandardType.OffsetTime(_), _)) => true
    case (Schema.Primitive(StandardType.OffsetDateTime(_), _), Schema.Primitive(StandardType.OffsetDateTime(_), _)) =>
      true
    case (Schema.Primitive(tpe1, _), Schema.Primitive(tpe2, _))     => tpe1 == tpe2
    case (Schema.Optional(expected, _), Schema.Optional(actual, _)) => equalsAst(expected, actual, depth)
    case (Schema.Tuple(expectedLeft, expectedRight, _), Schema.Tuple(actualLeft, actualRight, _)) =>
      equalsAst(expectedLeft, actualLeft, depth) && equalsAst(expectedRight, actualRight, depth)
    case (Schema.Tuple(expectedLeft, expectedRight, _), Schema.GenericRecord(structure)) =>
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
      case (Schema.GenericRecord(structure1), Schema.GenericRecord(structure2)) =>
        hasSameFields(structure1.toChunk, structure2.toChunk) &&
          structure1.toChunk.forall {
            case Schema.Field(label, schema, _) =>
              val left: Schema[Any]  = schema.asInstanceOf[Schema[Any]]
              val right: Schema[Any] = structure2.toChunk.find(_.label == label).asInstanceOf[Schema[Any]]
              equalsSchema(left, right)
          }
      case (left: Schema.Record[_], right: Schema.Record[_]) =>
        hasSameStructure(left.asInstanceOf[Schema.Record[A]], right.asInstanceOf[Schema.Record[A]]) && equalsAnnotations(
          left.annotations,
          right.annotations
        )
      case (Schema.Sequence(element1, _, _, _), Schema.Sequence(element2, _, _, _)) => equalsSchema(element1, element2)
      case (Schema.Primitive(standardType1, a1), Schema.Primitive(standardType2, a2)) =>
        standardType1 == standardType2 && equalsAnnotations(a1, a2)
      case (Schema.Tuple(left1, right1, _), Schema.Tuple(left2, right2, _)) =>
        equalsSchema(left1, left2) && equalsSchema(right1, right2)
      case (Schema.Optional(codec1, a1), Schema.Optional(codec2, a2)) =>
        equalsSchema(codec1, codec2) && equalsAnnotations(a1, a2)
      case (Schema.Enum1(l, lA), Schema.Enum1(r, rA)) => equalsCase(l, r) && lA.equals(rA)
      case (Schema.Enum2(l1, l2, lA), Schema.Enum2(r1, r2, rA)) =>
        hasSameCases(Seq(l1, l2), Seq(r1, r2)) && equalsAnnotations(lA, rA)
      case (Schema.Enum3(l1, l2, l3, lA), Schema.Enum3(r1, r2, r3, rA)) =>
        hasSameCases(Seq(l1, l2, l3), Seq(r1, r2, r3)) && lA.equals(rA)
      case (Schema.EnumN(ls, lA), Schema.EnumN(rs, rA)) => hasSameCases(ls.toSeq, rs.toSeq) && lA.equals(rA)
      case (l @ Schema.Lazy(_, _), r @ Schema.Lazy(_, _)) =>
        equalsSchema(l.schema.asInstanceOf[Schema[Any]], r.schema.asInstanceOf[Schema[Any]])
      case (lazySchema @ Schema.Lazy(_, _), eagerSchema) =>
        equalsSchema(lazySchema.schema.asInstanceOf[Schema[Any]], eagerSchema.asInstanceOf[Schema[Any]])
      case (eagerSchema, lazySchema @ Schema.Lazy(_, _)) =>
        equalsSchema(lazySchema.asInstanceOf[Schema[Any]], eagerSchema.asInstanceOf[Schema[Any]])
      case _ => false
    }

  private def equalsAnnotations(l: Chunk[Any], r: Chunk[Any]): Boolean = l.equals(r)

  private def equalsCase(left: Schema.Case[_, _], right: Schema.Case[_, _]): Boolean =
    left.id == right.id && equalsSchema(left.codec.asInstanceOf[Schema[Any]], right.codec.asInstanceOf[Schema[Any]]) && equalsAnnotations(
      left.annotations,
      right.annotations
    )

  private def hasSameCases(ls: Seq[Schema.Case[_, _]], rs: Seq[Schema.Case[_, _]]): Boolean =
    ls.map(l => rs.exists(r => equalsCase(l, r))).reduce(_ && _) && rs
      .map(r => ls.exists(l => equalsCase(l, r)))
      .reduce(_ && _)

  private def hasSameStructure[A](left: Schema.Record[A], right: Schema.Record[A]): Boolean =
    left.structure.zip(right.structure).forall {
      case (Schema.Field(lLabel, lSchema, lAnnotations), Schema.Field(rLabel, rSchema, rAnnotations)) =>
        lLabel == rLabel && lAnnotations.toSet == rAnnotations.toSet && equalsSchema(lSchema, rSchema)
    }

  private def hasSameFields(left: Chunk[Schema.Field[_]], right: Chunk[Schema.Field[_]]): Boolean =
    left.map(_.label) == right.map(_.label)

}
