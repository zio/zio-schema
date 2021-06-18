package zio.schema

import java.math.BigInteger
import java.time.temporal.{ ChronoUnit, Temporal => JTemporal, TemporalAmount, TemporalUnit }
import java.time.{
  DayOfWeek,
  Duration => JDuration,
  Instant,
  LocalDate,
  LocalDateTime,
  LocalTime,
  Month => JMonth,
  MonthDay,
  OffsetDateTime,
  OffsetTime,
  Year,
  YearMonth,
  ZoneId,
  ZonedDateTime
}

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.internal.MyersDiff

trait Differ[A] { self =>

  def apply(thisValue: A, thatValue: A): Diff

  /**
   * A symbolic operator for [[zip]].
   */
  def <*>[B](that: Differ[B]): Differ[(A, B)] = self.zip(that)

  def zip[B](that: Differ[B]): Differ[(A, B)] = Differ.tuple(self, that)

  def transform[B](f: B => A): Differ[B] = (thisValue: B, thatValue: B) => self.apply(f(thisValue), f(thatValue))

  def transformOrFail[B](f: B => Either[String, A]): Differ[B] =
    (thisValue: B, thatValue: B) =>
      f(thisValue) -> f(thatValue) match {
        case (Right(l), Right(r)) => self(l, r)
        case _                    => Diff.NotComparable
      }

  def foreach[Col[_]](toChunk: Col[A] => Chunk[A]): Differ[Col[A]] =
    (theseAs: Col[A], thoseAs: Col[A]) =>
      Diff
        .Sequence(
          toChunk(theseAs).zipAll(toChunk(thoseAs)).map {
            case (Some(left), Some(right)) => self.apply(left, right)
            case (None, Some(right))       => Diff.Total(right, Diff.Tag.Right)
            case (Some(left), None)        => Diff.Total(left, Diff.Tag.Left)
            case (None, None)              => Diff.Identical
          }
        )
        .orIdentical

  def optional: Differ[Option[A]] = Differ.instancePartial {
    case (Some(l), Some(r)) => self(l, r)
    case (Some(l), None)    => Diff.Total(l, Diff.Tag.Left)
    case (None, Some(r))    => Diff.Total(r, Diff.Tag.Right)
    case (None, None)       => Diff.Identical
  }
}

object Differ {

  def fromSchema[A](schema: Schema[A]): Differ[A] = schema match {
    case Schema.Primitive(StandardType.BinaryType)     => binary
    case Schema.Primitive(StandardType.IntType)        => numeric[Int]
    case Schema.Primitive(StandardType.ShortType)      => numeric[Short]
    case Schema.Primitive(StandardType.DoubleType)     => numeric[Double]
    case Schema.Primitive(StandardType.FloatType)      => numeric[Float]
    case Schema.Primitive(StandardType.LongType)       => numeric[Long]
    case Schema.Primitive(StandardType.CharType)       => numeric[Char]
    case Schema.Primitive(StandardType.BoolType)       => bool
    case Schema.Primitive(StandardType.BigDecimalType) => bigDecimal
    case Schema.Primitive(StandardType.BigIntegerType) => bigInt
    case Schema.Primitive(StandardType.StringType)     => string
    case Schema.Primitive(StandardType.DayOfWeekType)  => dayOfWeek
    case Schema.Primitive(StandardType.Month)          => month
    case Schema.Primitive(StandardType.MonthDay)       => monthDay
    case Schema.Primitive(StandardType.Year) =>
      temporal[Year](ChronoUnit.YEARS)
    case Schema.Primitive(StandardType.YearMonth) =>
      temporal[YearMonth](ChronoUnit.MONTHS)
    case Schema.Primitive(StandardType.ZoneId) => string.transform[ZoneId](_.getId)
    case Schema.Primitive(StandardType.Instant(_)) =>
      temporal[Instant](ChronoUnit.MILLIS)
    case Schema.Primitive(StandardType.Duration(_)) =>
      temporalAmount[JDuration](ChronoUnit.MILLIS)
    case Schema.Primitive(StandardType.LocalDate(_)) =>
      temporal[LocalDate](ChronoUnit.DAYS)
    case Schema.Primitive(StandardType.LocalTime(_)) =>
      temporal[LocalTime](ChronoUnit.MILLIS)
    case Schema.Primitive(StandardType.LocalDateTime(_)) =>
      temporal[LocalDateTime](ChronoUnit.MILLIS)
    case Schema.Primitive(StandardType.OffsetTime(_)) =>
      temporal[OffsetTime](ChronoUnit.MILLIS)
    case Schema.Primitive(StandardType.OffsetDateTime(_)) =>
      temporal[OffsetDateTime](ChronoUnit.MILLIS)
    case Schema.Primitive(StandardType.ZonedDateTime(_)) =>
      temporal[ZonedDateTime](ChronoUnit.MILLIS)
    case Schema.Tuple(leftSchema, rightSchema)        => fromSchema(leftSchema) <*> fromSchema(rightSchema)
    case Schema.Optional(schema)                      => fromSchema(schema).optional
    case Schema.Sequence(schema, _, f)                => fromSchema(schema).foreach(f)
    case Schema.EitherSchema(leftSchema, rightSchema) => either(fromSchema(leftSchema), fromSchema(rightSchema))
    case s @ Schema.Lazy(_)                           => fromSchema(s.schema)
    case Schema.Transform(schema, _, f)               => fromSchema(schema).transformOrFail(f)
    case Schema.Fail(_)                               => fail
    case Schema.GenericRecord(structure)              => record(structure)
    case ProductDiffer(differ)                        => differ
    case Schema.Enum1(c)                              => enum(c)
    case Schema.Enum2(c1, c2)                         => enum(c1, c2)
    case Schema.Enum3(c1, c2, c3)                     => enum(c1, c2, c3)
    case Schema.EnumN(cs)                             => enum(cs: _*)
    case Schema.Enumeration(structure)                => enumeration(structure)
  }

  def binary: Differ[Chunk[Byte]] =
    (theseBytes: Chunk[Byte], thoseBytes: Chunk[Byte]) =>
      Diff.Sequence {
        theseBytes.zipAll(thoseBytes).map {
          case (Some(thisByte), Some(thatByte)) if (thisByte ^ thatByte) != 0 => Diff.Binary(thisByte ^ thatByte)
          case (Some(_), Some(_))                                             => Diff.Identical
          case (None, Some(thatByte))                                         => Diff.Total(thatByte, Diff.Tag.Right)
          case (Some(thisByte), None)                                         => Diff.Total(thisByte, Diff.Tag.Left)
        }
      }.orIdentical

  def bool: Differ[Boolean] =
    (thisBool: Boolean, thatBool: Boolean) => Diff.Bool(thisBool ^ thatBool)

  def numeric[A](implicit numeric: Numeric[A]): Differ[A] =
    (thisValue: A, thatValue: A) =>
      numeric.minus(thisValue, thatValue) match {
        case distance if distance == numeric.zero => Diff.Identical
        case distance                             => Diff.Number(distance)
      }

  def temporalAmount[A <: TemporalAmount](units: TemporalUnit): Differ[A] =
    (thisA: A, thatA: A) => Diff.TemporalAmount(thisA.get(units) - thatA.get(units), units)

  def temporal[A <: JTemporal](units: ChronoUnit): Differ[A] =
    (thisA: A, thatA: A) => Diff.Temporal(units.between(thisA, thatA), units)

  val dayOfWeek: Differ[DayOfWeek] =
    (thisDay: DayOfWeek, thatDay: DayOfWeek) =>
      if (thisDay == thatDay)
        Diff.Identical
      else
        Diff.Temporal((thatDay.getValue - thisDay.getValue).toLong, ChronoUnit.DAYS)

  val month: Differ[JMonth] =
    (thisMonth: JMonth, thatMonth: JMonth) =>
      if (thisMonth == thatMonth)
        Diff.Identical
      else
        Diff.Temporal((thatMonth.getValue - thisMonth.getValue).toLong, ChronoUnit.MONTHS)

  val monthDay: Differ[MonthDay] =
    (thisMonthDay: MonthDay, thatMonthDay: MonthDay) =>
      if (thisMonthDay == thatMonthDay)
        Diff.Identical
      else
        Diff.MonthDays(
          ChronoUnit.DAYS.between(thisMonthDay.atYear(2001), thatMonthDay.atYear(2001)).toInt,
          ChronoUnit.DAYS.between(thisMonthDay.atYear(2000), thatMonthDay.atYear(2000)).toInt
        )

  val bigInt: Differ[BigInteger] =
    (thisValue: BigInteger, thatValue: BigInteger) =>
      thisValue.subtract(thatValue) match {
        case BigInteger.ZERO => Diff.Identical
        case distance        => Diff.BigInt(distance)
      }

  val bigDecimal: Differ[java.math.BigDecimal] =
    (thisValue: java.math.BigDecimal, thatValue: java.math.BigDecimal) =>
      thisValue.subtract(thatValue) match {
        case d if d.compareTo(java.math.BigDecimal.ZERO) == 0 => Diff.Identical
        case distance                                         => Diff.BigDecimal(distance)
      }

  def tuple[A, B](left: Differ[A], right: Differ[B]): Differ[(A, B)] =
    (thisValue: (A, B), thatValue: (A, B)) =>
      (thisValue, thatValue) match {
        case ((thisA, thisB), (thatA, thatB)) =>
          left(thisA, thatA) <*> right(thisB, thatB)
      }

  def either[A, B](left: Differ[A], right: Differ[B]) =
    instancePartial[Either[A, B]] {
      case (Left(l), Left(r))   => left(l, r)
      case (Right(l), Right(r)) => right(l, r)
    }

  def identical[A]: Differ[A] = (_: A, _: A) => Diff.Identical

  def fail[A]: Differ[A] = (_: A, _: A) => Diff.NotComparable

  def record(structure: Chunk[Schema.Field[_]]): Differ[ListMap[String, _]] =
    (thisValue: ListMap[String, _], thatValue: ListMap[String, _]) =>
      if (!(conformsToStructure(thisValue, structure) && conformsToStructure(thatValue, structure)))
        Diff.NotComparable
      else
        Diff
          .Record(
            ListMap.empty ++ thisValue.toList.zip(thatValue.toList).zipWithIndex.map {
              case (((thisKey, thisValue), (_, thatValue)), fieldIndex) =>
                thisKey -> fromSchema(structure(fieldIndex).schema)
                  .asInstanceOf[Differ[Any]]
                  .apply(thisValue, thatValue)
            }
          )
          .orIdentical

  private def conformsToStructure(map: ListMap[String, _], structure: Chunk[Schema.Field[_]]): Boolean =
    structure.foldRight(true) {
      case (_, false)                  => false
      case (field: Schema.Field[a], _) => map.get(field.label).map(_.isInstanceOf[a]).getOrElse(false)
    }

  def enum[Z](cases: Schema.Case[_ <: Z, Z]*): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      cases
        .foldRight[Option[Diff]](None) {
          case (_, diff @ Some(_)) => diff
          case (subtype, _) =>
            subtype.deconstruct(thisZ) -> (subtype.deconstruct(thatZ)) match {
              case (Some(thisA), Some(thatA)) =>
                Some(fromSchema(subtype.codec)(thisA, thatA))
              case _ => None
            }
        }
        .getOrElse(Diff.NotComparable)

  def enumeration(structure: ListMap[String, Schema[_]]): Differ[(String, _)] =
    instancePartial[(String, _)] {
      case ((thisKey, thisValue), (thatKey, thatValue)) if thisKey == thatKey =>
        structure
          .get(thisKey)
          .map(fromSchema(_).asInstanceOf[Differ[Any]].apply(thisValue, thatValue))
          .getOrElse(Diff.NotComparable)
    }

  val string: Differ[String] = MyersDiff

  def instancePartial[A](f: PartialFunction[(A, A), Diff]) =
    new Differ[A] {
      override def apply(thisValue: A, thatValue: A): Diff =
        f.applyOrElse((thisValue, thatValue), (_: (A, A)) => Diff.NotComparable)
    }

}

sealed trait Diff { self =>

  /**
   * A symbolic operator for [[zip]].
   */
  def <*>(that: Diff): Diff = self.zip(that)

  def zip(that: Diff): Diff = Diff.Tuple(self, that)
}

object Diff {
  final case object Identical extends Diff

  final case class Binary(xor: Int) extends Diff

  final case class Bool(xor: Boolean) extends Diff

  final case class Number[A: Numeric](distance: A) extends Diff

  final case class BigInt(distance: BigInteger) extends Diff

  final case class BigDecimal(distance: java.math.BigDecimal) extends Diff

  final case class Temporal(distance: Long, timeUnit: TemporalUnit) extends Diff

  final case class TemporalAmount(difference: Long, units: TemporalUnit) extends Diff

  final case class MonthDays(difference: Int, leapYearDifference: Int) extends Diff

  final case class Tuple(leftDifference: Diff, rightDifference: Diff) extends Diff

  final case class Myers(edits: Chunk[Edit]) extends Diff

  final case class Total[A](value: A, tag: Tag) extends Diff

  /**
   * Represents diff between incomparable values. For instance Left(1) and Right("a")
   */
  case object NotComparable extends Diff

  /**
   * Diff between two sequence of elements. The length of differences will be
   * the length of the longest list.
   *
   * If both this and that have an element at index i then the ith element
   * of difference will contain the diff between those elements
   *
   * If either this or that do not have an element at index i then the ith element
   * of differences will be a total diff with the element and a tag representing which
   * input was missing the ith index.
   */
  final case class Sequence(differences: Chunk[Diff]) extends Diff { self =>

    def orIdentical: Diff =
      if (differences.forall(_ == Diff.Identical))
        Diff.Identical
      else
        self
  }

  /**
   * Set of elements which differ between two sets.
   */
  final case class Set[A](differences: Set[Total[A]]) extends Diff

  /**
   * Map of field-level diffs between two records. The map of differences
   * is keyed to the records field names.
   */
  final case class Record(differences: ListMap[String, Diff]) extends Diff { self =>

    def orIdentical: Diff =
      if (differences.values.forall(_ == Diff.Identical))
        Diff.Identical
      else
        self
  }

  sealed trait Tag

  object Tag {
    case object Left  extends Tag
    case object Right extends Tag
  }

  sealed trait Edit

  object Edit {
    final case class Delete(s: String) extends Edit
    final case class Insert(s: String) extends Edit
    final case class Keep(s: String)   extends Edit
  }
}

object ProductDiffer {

  def unapply[A](schema: Schema[A]): Option[Differ[A]] = schema match {
    case Schema.CaseObject(_)                                                                    => Some(Differ.identical[A])
    case s: Schema.CaseClass1[_, A]                                                              => Some(product1(s))
    case s: Schema.CaseClass2[_, _, A]                                                           => Some(product2(s))
    case s: Schema.CaseClass3[_, _, _, A]                                                        => Some(product3(s))
    case s: Schema.CaseClass4[_, _, _, _, A]                                                     => Some(product4(s))
    case s: Schema.CaseClass5[_, _, _, _, _, A]                                                  => Some(product5(s))
    case s: Schema.CaseClass6[_, _, _, _, _, _, A]                                               => Some(product6(s))
    case s: Schema.CaseClass7[_, _, _, _, _, _, _, A]                                            => Some(product7(s))
    case s: Schema.CaseClass8[_, _, _, _, _, _, _, _, A]                                         => Some(product8(s))
    case s: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, A]                                      => Some(product9(s))
    case s: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, A]                                  => Some(product10(s))
    case s: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, A]                               => Some(product11(s))
    case s: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, A]                            => Some(product12(s))
    case s: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, A]                         => Some(product13(s))
    case s: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, A]                      => Some(product14(s))
    case s: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]                   => Some(product15(s))
    case s: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]                => Some(product16(s))
    case s: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]             => Some(product17(s))
    case s: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]          => Some(product18(s))
    case s: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]       => Some(product19(s))
    case s: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]    => Some(product20(s))
    case s: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] => Some(product21(s))
    case s: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
      Some(product22(s))
    case _ => None
  }

  def product1[A, Z](schema: Schema.CaseClass1[A, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap(
            fieldDiffer(schema.field, schema.extractField)(thisZ, thatZ)
          )
        )
        .orIdentical

  def product2[A1, A2, Z](schema: Schema.CaseClass2[A1, A2, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product3[A1, A2, A3, Z](schema: Schema.CaseClass3[A1, A2, A3, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product4[A1, A2, A3, A4, Z](schema: Schema.CaseClass4[A1, A2, A3, A4, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product5[A1, A2, A3, A4, A5, Z](schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product6[A1, A2, A3, A4, A5, A6, Z](schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product7[A1, A2, A3, A4, A5, A6, A7, Z](schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product8[A1, A2, A3, A4, A5, A6, A7, A8, Z](
    schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](
    schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
    schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
    schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10),
            fieldDiffer(schema.field11, schema.extractField11)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
    schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10),
            fieldDiffer(schema.field11, schema.extractField11),
            fieldDiffer(schema.field12, schema.extractField12)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
    schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10),
            fieldDiffer(schema.field11, schema.extractField11),
            fieldDiffer(schema.field12, schema.extractField12),
            fieldDiffer(schema.field13, schema.extractField13)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
    schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10),
            fieldDiffer(schema.field11, schema.extractField11),
            fieldDiffer(schema.field12, schema.extractField12),
            fieldDiffer(schema.field13, schema.extractField13),
            fieldDiffer(schema.field14, schema.extractField14)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
    schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10),
            fieldDiffer(schema.field11, schema.extractField11),
            fieldDiffer(schema.field12, schema.extractField12),
            fieldDiffer(schema.field13, schema.extractField13),
            fieldDiffer(schema.field14, schema.extractField14),
            fieldDiffer(schema.field15, schema.extractField15)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](
    schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10),
            fieldDiffer(schema.field11, schema.extractField11),
            fieldDiffer(schema.field12, schema.extractField12),
            fieldDiffer(schema.field13, schema.extractField13),
            fieldDiffer(schema.field14, schema.extractField14),
            fieldDiffer(schema.field15, schema.extractField15),
            fieldDiffer(schema.field16, schema.extractField16)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](
    schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10),
            fieldDiffer(schema.field11, schema.extractField11),
            fieldDiffer(schema.field12, schema.extractField12),
            fieldDiffer(schema.field13, schema.extractField13),
            fieldDiffer(schema.field14, schema.extractField14),
            fieldDiffer(schema.field15, schema.extractField15),
            fieldDiffer(schema.field16, schema.extractField16),
            fieldDiffer(schema.field17, schema.extractField17)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](
    schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10),
            fieldDiffer(schema.field11, schema.extractField11),
            fieldDiffer(schema.field12, schema.extractField12),
            fieldDiffer(schema.field13, schema.extractField13),
            fieldDiffer(schema.field14, schema.extractField14),
            fieldDiffer(schema.field15, schema.extractField15),
            fieldDiffer(schema.field16, schema.extractField16),
            fieldDiffer(schema.field17, schema.extractField17),
            fieldDiffer(schema.field18, schema.extractField18)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](
    schema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10),
            fieldDiffer(schema.field11, schema.extractField11),
            fieldDiffer(schema.field12, schema.extractField12),
            fieldDiffer(schema.field13, schema.extractField13),
            fieldDiffer(schema.field14, schema.extractField14),
            fieldDiffer(schema.field15, schema.extractField15),
            fieldDiffer(schema.field16, schema.extractField16),
            fieldDiffer(schema.field17, schema.extractField17),
            fieldDiffer(schema.field18, schema.extractField18),
            fieldDiffer(schema.field19, schema.extractField19)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](
    schema: Schema.CaseClass20[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      Z
    ]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10),
            fieldDiffer(schema.field11, schema.extractField11),
            fieldDiffer(schema.field12, schema.extractField12),
            fieldDiffer(schema.field13, schema.extractField13),
            fieldDiffer(schema.field14, schema.extractField14),
            fieldDiffer(schema.field15, schema.extractField15),
            fieldDiffer(schema.field16, schema.extractField16),
            fieldDiffer(schema.field17, schema.extractField17),
            fieldDiffer(schema.field18, schema.extractField18),
            fieldDiffer(schema.field19, schema.extractField19),
            fieldDiffer(schema.field20, schema.extractField20)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](
    schema: Schema.CaseClass21[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      Z
    ]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10),
            fieldDiffer(schema.field11, schema.extractField11),
            fieldDiffer(schema.field12, schema.extractField12),
            fieldDiffer(schema.field13, schema.extractField13),
            fieldDiffer(schema.field14, schema.extractField14),
            fieldDiffer(schema.field15, schema.extractField15),
            fieldDiffer(schema.field16, schema.extractField16),
            fieldDiffer(schema.field17, schema.extractField17),
            fieldDiffer(schema.field18, schema.extractField18),
            fieldDiffer(schema.field19, schema.extractField19),
            fieldDiffer(schema.field20, schema.extractField20),
            fieldDiffer(schema.field21, schema.extractField21)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](
    schema: Schema.CaseClass22[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      A22,
      Z
    ]
  ): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1, schema.extractField1),
            fieldDiffer(schema.field2, schema.extractField2),
            fieldDiffer(schema.field3, schema.extractField3),
            fieldDiffer(schema.field4, schema.extractField4),
            fieldDiffer(schema.field5, schema.extractField5),
            fieldDiffer(schema.field6, schema.extractField6),
            fieldDiffer(schema.field7, schema.extractField7),
            fieldDiffer(schema.field8, schema.extractField8),
            fieldDiffer(schema.field9, schema.extractField9),
            fieldDiffer(schema.field10, schema.extractField10),
            fieldDiffer(schema.field11, schema.extractField11),
            fieldDiffer(schema.field12, schema.extractField12),
            fieldDiffer(schema.field13, schema.extractField13),
            fieldDiffer(schema.field14, schema.extractField14),
            fieldDiffer(schema.field15, schema.extractField15),
            fieldDiffer(schema.field16, schema.extractField16),
            fieldDiffer(schema.field17, schema.extractField17),
            fieldDiffer(schema.field18, schema.extractField18),
            fieldDiffer(schema.field19, schema.extractField19),
            fieldDiffer(schema.field20, schema.extractField20),
            fieldDiffer(schema.field21, schema.extractField21),
            fieldDiffer(schema.field22, schema.extractField22)
          ).map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  private def fieldDiffer[A, Z](field: Schema.Field[A], extract: Z => A): (Z, Z) => (String, Diff) =
    (thisZ: Z, thatZ: Z) => field.label -> Differ.fromSchema(field.schema)(extract(thisZ), extract(thatZ))
}
