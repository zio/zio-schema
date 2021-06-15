package zio.schema

import java.math.BigInteger
import java.time.{ DayOfWeek, Instant }
import java.util.concurrent.TimeUnit

import scala.collection.immutable.ListMap
import scala.concurrent.duration.TimeUnit

import zio.Chunk

trait DiffAlgorithm[A] { self =>

  def apply(thisValue: A, thatValue: A): Diff

  /**
   * A symbolic operator for [[zip]].
   */
  def <*>[B](that: DiffAlgorithm[B]): DiffAlgorithm[(A, B)] = self.zip(that)

  def zip[B](that: DiffAlgorithm[B]): DiffAlgorithm[(A, B)] = DiffAlgorithm.tuple(self, that)

  def transform[B](f: B => A): DiffAlgorithm[B] = (thisValue: B, thatValue: B) => self.apply(f(thisValue), f(thatValue))

  def transformOrFail[B](f: B => Either[String, A]): DiffAlgorithm[B] =
    (thisValue: B, thatValue: B) =>
      f(thisValue) -> f(thatValue) match {
        case (Right(l), Right(r)) => self(l, r)
        case _                    => Diff.NotComparable
      }

  def foreach[Col[_]](toChunk: Col[A] => Chunk[A]): DiffAlgorithm[Col[A]] =
    (theseAs: Col[A], thoseAs: Col[A]) => {
      val sequenceDiff = Diff.Sequence(
        toChunk(theseAs).zipAll(toChunk(thoseAs)).map {
          case (Some(left), Some(right)) => self.apply(left, right)
          case (None, Some(right))       => Diff.Total(right, Diff.Tag.Right)
          case (Some(left), None)        => Diff.Total(left, Diff.Tag.Left)
          case (None, None)              => Diff.Identical
        }
      )
      if (sequenceDiff.differences.forall(_ == Diff.Identical)) Diff.Identical else sequenceDiff
    }

  def optional: DiffAlgorithm[Option[A]] = DiffAlgorithm.instancePartial {
    case (Some(l), Some(r)) => self(l, r)
    case (Some(l), None)    => Diff.Total(l, Diff.Tag.Left)
    case (None, Some(r))    => Diff.Total(r, Diff.Tag.Right)
    case (None, None)       => Diff.Identical
  }
}

object DiffAlgorithm {

  def fromSchema[A](schema: Schema[A]): DiffAlgorithm[A] = schema match {
    case Schema.Primitive(StandardType.IntType)        => numeric[Int]
    case Schema.Primitive(StandardType.ShortType)      => numeric[Short]
    case Schema.Primitive(StandardType.DoubleType)     => numeric[Double]
    case Schema.Primitive(StandardType.FloatType)      => numeric[Float]
    case Schema.Primitive(StandardType.LongType)       => numeric[Long]
    case Schema.Primitive(StandardType.BigDecimalType) => bigDecimal
    case Schema.Primitive(StandardType.BigIntegerType) => bigInt
    case Schema.Primitive(StandardType.Duration(_)) =>
      temporal[java.time.Duration](TimeUnit.MILLISECONDS)(
        (d1: java.time.Duration, d2: java.time.Duration) => d1.minus(d2).toMillis
      )
    case Schema.Primitive(StandardType.DayOfWeekType) => dayOfWeek
    case Schema.Primitive(StandardType.Instant(_)) =>
      temporal[Instant](TimeUnit.MILLISECONDS)((i1, i2) => i1.toEpochMilli - i2.toEpochMilli)
    case Schema.Tuple(leftSchema, rightSchema)        => fromSchema(leftSchema) <*> fromSchema(rightSchema)
    case Schema.Optional(schema)                      => fromSchema(schema).optional
    case Schema.Sequence(schema, _, f)                => fromSchema(schema).foreach(f)
    case Schema.EitherSchema(leftSchema, rightSchema) => either(fromSchema(leftSchema), fromSchema(rightSchema))
    case s @ Schema.Lazy(_)                           => fromSchema(s.schema)
    case Schema.Transform(schema, f, _)               => fromSchema(schema).transformOrFail(f)
    case Schema.Fail(_)                               => fail
    case Schema.GenericRecord(structure)              => record(structure)
    case s: Schema.CaseClass1[_, A]                   => caseClass1(s)
    case _                                            => string.transform(_.toString)
  }

  def numeric[A](implicit numeric: Numeric[A]): DiffAlgorithm[A] =
    (thisValue: A, thatValue: A) =>
      numeric.minus(thisValue, thatValue) match {
        case distance if distance == numeric.zero => Diff.Identical
        case distance                             => Diff.Number(distance)
      }

  def temporal[A](units: TimeUnit)(metric: (A, A) => Long): DiffAlgorithm[A] =
    (thisValue: A, thatValue: A) => Diff.Temporal(metric(thisValue, thatValue), units)

  val dayOfWeek: DiffAlgorithm[DayOfWeek] =
    (thisValue: DayOfWeek, thatValue: DayOfWeek) => {
      var distance = 0L
      do {
        distance += 1
      } while (!thisValue.plus(distance).equals(thatValue))
      Diff.Temporal(distance, TimeUnit.DAYS)
    }

  val bigInt: DiffAlgorithm[BigInteger] =
    (thisValue: BigInteger, thatValue: BigInteger) =>
      thisValue.subtract(thatValue) match {
        case BigInteger.ZERO => Diff.Identical
        case distance        => Diff.BigInt(distance)
      }

  val bigDecimal: DiffAlgorithm[java.math.BigDecimal] =
    (thisValue: java.math.BigDecimal, thatValue: java.math.BigDecimal) =>
      thisValue.subtract(thatValue) match {
        case d if d.compareTo(java.math.BigDecimal.ZERO) == 0 => Diff.Identical
        case distance                                         => Diff.BigDecimal(distance)
      }

  def tuple[A, B](left: DiffAlgorithm[A], right: DiffAlgorithm[B]): DiffAlgorithm[(A, B)] =
    (thisValue: (A, B), thatValue: (A, B)) =>
      (thisValue, thatValue) match {
        case ((thisA, thisB), (thatA, thatB)) =>
          left(thisA, thatA) <*> right(thisB, thatB)
      }

  def either[A, B](left: DiffAlgorithm[A], right: DiffAlgorithm[B]) =
    instancePartial[Either[A, B]] {
      case (Left(l), Left(r))   => left(l, r)
      case (Right(l), Right(r)) => right(l, r)
    }

  def fail[A]: DiffAlgorithm[A] = (_: A, _: A) => Diff.NotComparable

  // TODO This assumes for the moment that both maps conform to the schema structure
  def record(structure: Chunk[Schema.Field[_]]): DiffAlgorithm[ListMap[String, _]] =
    (thisValue: ListMap[String, _], thatValue: ListMap[String, _]) =>
      if (thisValue == thatValue)
        Diff.Identical
      else
        Diff.Record(
          ListMap.empty ++ thisValue.toList.zip(thatValue.toList).zipWithIndex.map {
            case (((thisKey, thisValue), (_, thatValue)), fieldIndex) =>
              thisKey -> fromSchema(structure(fieldIndex).schema)
                .asInstanceOf[DiffAlgorithm[Any]]
                .apply(thisValue, thatValue)
          }
        )

  def caseClass1[A, Z](schema: Schema.CaseClass1[A, Z]): DiffAlgorithm[Z] =
    (thisA: Z, thatA: Z) =>
      if (thisA == thatA)
        Diff.Identical
      else
        Diff.Record(
          ListMap(
            schema.field.label -> fromSchema(schema.field.schema)(
              schema.extractField(thisA),
              schema.extractField(thatA)
            )
          )
        )

  /**
   * Port implementation of Myers diff algorithm from zio-test here
   */
  val string: DiffAlgorithm[String] = fail[String]

  def instancePartial[A](f: PartialFunction[(A, A), Diff]) =
    new DiffAlgorithm[A] {
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

  final case class Number[A: Numeric](distance: A) extends Diff

  final case class BigInt(distance: BigInteger) extends Diff

  final case class BigDecimal(distance: java.math.BigDecimal) extends Diff

  final case class Temporal(distance: Long, timeUnit: TimeUnit) extends Diff

  final case class Tuple(leftDifference: Diff, rightDifference: Diff) extends Diff

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
  final case class Sequence(differences: Chunk[Diff]) extends Diff

  /**
   * Set of elements which differ between two sets.
   */
  final case class Set[A](differences: Set[(A, Tag, Diff)]) extends Diff

  /**
   * Map of field-level diffs between two records. The map of differences
   * is keyed to the records field names.
   */
  final case class Record(differences: ListMap[String, Diff]) extends Diff

  sealed trait Tag

  object Tag {
    case object Left  extends Tag
    case object Right extends Tag
  }
}
