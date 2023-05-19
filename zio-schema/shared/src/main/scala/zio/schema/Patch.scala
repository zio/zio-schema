package zio.schema

import zio.Chunk
import zio.schema.diff.Edit
import zio.schema.meta.Migration
import zio.prelude.Validation

import java.math.{ BigInteger, MathContext }
import java.time.temporal.{ ChronoField, ChronoUnit }
import java.time.{
  DayOfWeek,
  Instant,
  LocalDate,
  LocalDateTime,
  LocalTime,
  MonthDay,
  OffsetDateTime,
  OffsetTime,
  Period,
  Year,
  YearMonth,
  ZoneId,
  ZoneOffset,
  Duration => JDuration,
  ZonedDateTime => JZonedDateTime
}
import scala.annotation.tailrec
import scala.collection.immutable.ListMap

sealed trait Patch[A] { self =>

  /**
   * A symbolic operator for [[zip]].
   */
  def <*>[B](that: Patch[B]): Patch[(A, B)] = self.zip(that)

  def zip[B](that: Patch[B]): Patch[(A, B)] = Patch.Tuple(self, that)

  def patch(a: A): Validation[String, A]

  def invert: Patch[A]

  def isIdentical: Boolean = false

  def isComparable: Boolean = true
}

object Patch {

  def invert[A](patch: Patch[A]): Patch[A] = patch.invert

  def identical[A]: Identical[A] = Identical()

  def notComparable[A]: NotComparable[A] = NotComparable()

  final case class Identical[A]() extends Patch[A] {
    override def patch(a: A): Validation[String, A] = Validation.succeed(a)
    override def isIdentical: Boolean               = true

    override def invert: Patch[A] = this
  }

  final case class Bool(xor: Boolean) extends Patch[Boolean] {
    override def patch(a: Boolean): Validation[String, Boolean] = Validation.succeed(a ^ xor)

    override def invert: Patch[Boolean] = this
  }

  final case class Number[A](distance: A)(implicit ev: Numeric[A]) extends Patch[A] {
    override def patch(input: A): Validation[String, A] =
      Validation.succeed(ev.minus(input, distance))

    override def invert: Patch[A] = Number(ev.negate(distance))
  }

  final case class BigInt(distance: BigInteger) extends Patch[BigInteger] {
    override def patch(input: BigInteger): Validation[String, BigInteger] =
      Validation.succeed(input.subtract(distance))

    override def invert: Patch[BigInteger] = BigInt(distance.negate())
  }

  final case class BigDecimal(distance: java.math.BigDecimal, precision: Int) extends Patch[java.math.BigDecimal] {
    override def patch(input: java.math.BigDecimal): Validation[String, java.math.BigDecimal] = {
      val mc = new MathContext(precision)
      Validation.succeed(input.round(mc).subtract(distance, mc))
    }

    override def invert: Patch[java.math.BigDecimal] = BigDecimal(distance.negate(), precision)
  }

  final case class Temporal[A](distances: List[Long], tpe: StandardType[A]) extends Patch[A] { self =>
    override def patch(a: A): Validation[String, A] =
      (tpe, distances) match {
        case (_: StandardType.YearType.type, distance :: Nil) =>
          Validation.succeed(Year.of(a.asInstanceOf[Year].getValue - distance.toInt).asInstanceOf[A])
        case (_: StandardType.YearMonthType.type, distance :: Nil) =>
          Validation.succeed(
            YearMonth
              .now()
              .`with`(
                ChronoField.PROLEPTIC_MONTH,
                a.asInstanceOf[YearMonth].getLong(ChronoField.PROLEPTIC_MONTH) - distance
              )
              .asInstanceOf[A]
          )
        case (_: StandardType.ZonedDateTimeType.type, distance :: Nil) =>
          Validation.succeed(LocalDate.ofEpochDay(a.asInstanceOf[LocalDate].toEpochDay - distance).asInstanceOf[A])
        case (_: StandardType.InstantType.type, dist1 :: dist2 :: Nil) =>
          Validation.succeed(
            Instant
              .ofEpochSecond(a.asInstanceOf[Instant].getEpochSecond - dist1, a.asInstanceOf[Instant].getNano() - dist2)
              .asInstanceOf[A]
          )
        case (_: StandardType.LocalTimeType.type, distance :: Nil) =>
          Validation.succeed(LocalTime.ofNanoOfDay(a.asInstanceOf[LocalTime].toNanoOfDay - distance).asInstanceOf[A])
        case (_: StandardType.LocalDateTimeType.type, dist1 :: dist2 :: Nil) =>
          Validation.succeed {
            LocalDateTime
              .of(
                LocalDate.ofEpochDay(a.asInstanceOf[LocalDateTime].toLocalDate.toEpochDay - dist1),
                LocalTime.ofNanoOfDay(a.asInstanceOf[LocalDateTime].toLocalTime.toNanoOfDay - dist2)
              )
              .asInstanceOf[A]
          }
        case (_: StandardType.OffsetTimeType.type, dist1 :: dist2 :: Nil) =>
          Validation.succeed {
            OffsetTime
              .of(
                LocalTime.ofNanoOfDay(a.asInstanceOf[OffsetTime].toLocalTime.toNanoOfDay - dist1),
                ZoneOffset.ofTotalSeconds(a.asInstanceOf[OffsetTime].getOffset.getTotalSeconds - dist2.toInt)
              )
              .asInstanceOf[A]
          }
        case (_: StandardType.OffsetDateTimeType.type, dist1 :: dist2 :: dist3 :: Nil) =>
          Validation.succeed {
            OffsetDateTime
              .of(
                LocalDate.ofEpochDay(a.asInstanceOf[OffsetDateTime].toLocalDate.toEpochDay - dist1),
                LocalTime.ofNanoOfDay(a.asInstanceOf[OffsetDateTime].toLocalTime.toNanoOfDay - dist2),
                ZoneOffset.ofTotalSeconds(a.asInstanceOf[OffsetDateTime].getOffset.getTotalSeconds - dist3.toInt)
              )
              .asInstanceOf[A]
          }
        case (_: StandardType.PeriodType.type, dayAdjustment :: monthAdjustment :: yearAdjustment :: Nil) =>
          Validation(
            Period
              .of(
                a.asInstanceOf[Period].getYears - yearAdjustment.toInt,
                a.asInstanceOf[Period].getMonths - monthAdjustment.toInt,
                a.asInstanceOf[Period].getDays - dayAdjustment.toInt
              )
              .asInstanceOf[A]
          ).mapError(_ => s"Invalid java.time.Period diff $self")
        case (_: StandardType.ZoneOffsetType.type, distance :: Nil) =>
          Validation(
            ZoneOffset.ofTotalSeconds(a.asInstanceOf[ZoneOffset].getTotalSeconds + distance.toInt).asInstanceOf[A]
          ).mapError(e => s"Patched offset is invalid: ${e.getMessage}")
        case (_: StandardType.DayOfWeekType.type, distance :: Nil) =>
          Validation.succeed(a.asInstanceOf[DayOfWeek].plus(distance).asInstanceOf[A])
        case (_: StandardType.MonthType.type, distance :: Nil) =>
          Validation.succeed(a.asInstanceOf[java.time.Month].plus(distance).asInstanceOf[A])
        case (_: StandardType.DurationType.type, dist1 :: dist2 :: Nil) =>
          Validation.succeed(
            JDuration
              .ofSeconds(a.asInstanceOf[JDuration].getSeconds - dist1, a.asInstanceOf[JDuration].getNano() - dist2)
              .asInstanceOf[A]
          )
        //      // TODO need to deal with leap year differences
        case (_: StandardType.MonthDayType.type, regDiff :: _ :: Nil) =>
          Validation.succeed(
            MonthDay.from(ChronoUnit.DAYS.addTo(a.asInstanceOf[MonthDay].atYear(2001), regDiff)).asInstanceOf[A]
          )
        case (_: StandardType.LocalDateType.type, dist :: Nil) =>
          Validation.succeed(
            LocalDate
              .ofEpochDay(a.asInstanceOf[LocalDate].toEpochDay - dist)
              .asInstanceOf[A]
          )
        case (s, _) => Validation.fail(s"Cannot apply temporal diff to value with type $s")
      }

    override def invert: Patch[A] = Temporal(distances.map(-_), tpe)
  }

  final case class ZonedDateTime(localDateTimeDiff: Patch[java.time.LocalDateTime], zoneIdDiff: Patch[String])
      extends Patch[java.time.ZonedDateTime] {
    override def patch(input: JZonedDateTime): Validation[String, JZonedDateTime] =
      for {
        patchedLocalDateTime <- localDateTimeDiff.patch(input.toLocalDateTime)
        patchedZoneId        <- zoneIdDiff.patch(input.getZone.getId)
        patched <- Validation(JZonedDateTime.of(patchedLocalDateTime, ZoneId.of(patchedZoneId)))
                    .mapError(
                      e =>
                        s"Patched ZonedDateTime is not valid. Patched values $patchedLocalDateTime, $patchedZoneId. Error=${e.getMessage}"
                    )
      } yield patched

    override def invert: Patch[JZonedDateTime] = ZonedDateTime(localDateTimeDiff.invert, zoneIdDiff.invert)
  }

  final case class Tuple[A, B](leftDifference: Patch[A], rightDifference: Patch[B]) extends Patch[(A, B)] {

    override def isIdentical: Boolean = leftDifference.isIdentical && rightDifference.isIdentical
    override def patch(input: (A, B)): Validation[String, (A, B)] =
      for {
        l <- leftDifference.patch(input._1)
        r <- rightDifference.patch(input._2)
      } yield (l, r)

    override def invert: Patch[(A, B)] = Tuple(leftDifference.invert, rightDifference.invert)
  }

  final case class LCS[A](edits: Chunk[Edit[A]]) extends Patch[Chunk[A]] {
    override def patch(as: Chunk[A]): Validation[String, Chunk[A]] = {
      import zio.schema.diff.{ Edit => ZEdit }

      @tailrec
      def calc(in: List[A], edits: List[Edit[A]], result: List[A]): Validation[String, Chunk[A]] = (in, edits) match {
        case (_ :: _, Nil)                            => Validation.fail(s"Incorrect Patch - no instructions for these items: ${in.mkString}.")
        case (h :: _, ZEdit.Delete(s) :: _) if s != h => Validation.fail(s"Cannot Delete $s - current letter is $h.")
        case (Nil, ZEdit.Delete(s) :: _)              => Validation.fail(s"Cannot Delete $s - no items left to delete.")
        case (_ :: t, ZEdit.Delete(_) :: tail)        => calc(t, tail, result)
        case (h :: _, ZEdit.Keep(s) :: _) if s != h   => Validation.fail(s"Cannot Keep $s - current letter is $h.")
        case (Nil, ZEdit.Keep(s) :: _)                => Validation.fail(s"Cannot Keep $s - no items left to keep.")
        case (h :: t, ZEdit.Keep(_) :: tail)          => calc(t, tail, result :+ h)
        case (in, ZEdit.Insert(s) :: tail)            => calc(in, tail, result :+ s)
        case (Nil, Nil)                               => Validation.succeed(Chunk.fromIterable(result))
      }

      calc(as.toList, edits.toList, Nil)
    }

    override def invert: Patch[Chunk[A]] = LCS(edits.map(Edit.invert))
  }

  final case class Total[A](value: A) extends Patch[A] {
    override def patch(input: A): Validation[String, A] = Validation.succeed(value)
    override def invert: Patch[A]                       = Total(value)
  }

  final case class EitherDiff[A, B](diff: Either[Patch[A], Patch[B]]) extends Patch[Either[A, B]] {
    override def isIdentical: Boolean = diff.fold(_.isIdentical, _.isIdentical)

    override def isComparable: Boolean = diff.fold(_.isComparable, _.isComparable)

    override def patch(input: Either[A, B]): Validation[String, Either[A, B]] =
      (input, diff) match {
        case (Left(_), Right(_)) => Validation.fail(s"Cannot apply a right diff to a left value")
        case (Right(_), Left(_)) => Validation.fail(s"Cannot apply a left diff to a right value")
        case (Left(in), Left(diff)) =>
          diff.patch(in).map(e => Left(e))
        case (Right(in), Right(diff)) =>
          diff.patch(in).map(e => Right(e))
      }

    override def invert: Patch[Either[A, B]] = diff match {
      case Left(value)  => EitherDiff(Left(value.invert))
      case Right(value) => EitherDiff(Right(value.invert))
    }
  }

  final case class Transform[A, B](patch: Patch[A], f: A => Validation[String, B], g: B => Validation[String, A])
      extends Patch[B] {
    override def isIdentical: Boolean = patch.isIdentical

    override def isComparable: Boolean = patch.isComparable

    override def patch(input: B): Validation[String, B] =
      for {
        a  <- g(input)
        a1 <- patch.patch(a)
        b  <- f(a1)
      } yield b

    override def invert: Patch[B] = Transform(patch.invert, f, g)
  }

  /**
   * Represents diff between incomparable values. For instance Left(1) and zio.prelude.Validation.succeed("a")
   */
  final case class NotComparable[A]() extends Patch[A] {
    override def patch(input: A): Validation[String, A] =
      Validation.fail(s"Non-comparable diff cannot be applied")

    override def isComparable: Boolean = false

    override def invert: Patch[A] = this
  }

  final case class SchemaMigration(migrations: Chunk[Migration]) extends Patch[Schema[_]] { self =>

    //TODO Probably need to implement this
    override def patch(input: Schema[_]): Validation[String, Schema[_]] =
      Validation.fail(s"Schema migrations cannot be applied")

    def orIdentical: Patch[Schema[_]] =
      if (migrations.isEmpty) Patch.identical
      else self

    override def invert: Patch[Schema[_]] = SchemaMigration(migrations.reverse)
  }

  /**
   * Map of field-level diffs between two records. The map of differences
   * is keyed to the records field names.
   */
  final case class Record[R](differences: ListMap[String, Patch[_]], schema: Schema.Record[R]) extends Patch[R] {
    self =>
    override def isIdentical: Boolean = differences.forall(_._2.isIdentical)

    override def isComparable: Boolean = differences.forall(_._2.isComparable)

    override def patch(input: R): Validation[String, R] = {
      val structure = schema.fields

      val patchedDynamicValue = schema.toDynamic(input) match {
        case DynamicValue.Record(name, values) =>
          Validation
            .validateAll(
              differences.map {
                case (key, diff) =>
                  (structure.find(_.name == key).map(_.schema), values.get(key)) match {
                    case (Some(schema: Schema[b]), Some(oldValue)) =>
                      val oldVal = oldValue.toTypedValue(schema)
                      oldVal
                        .flatMap(v => diff.asInstanceOf[Patch[Any]].patch(v))
                        .map(v => schema.asInstanceOf[Schema[Any]].toDynamic(v))
                        .map(newValue => key -> newValue)
                    case _ =>
                      Validation.fail(s"Values=$values and structure=$structure have incompatible shape.")
                  }
              }
            )
            .map(r => (name, ListMap.from(r)))

        case dv => Validation.fail(s"Failed to apply record diff. Unexpected dynamic value for record: $dv")
      }

      patchedDynamicValue.flatMap { newValues =>
        schema.fromDynamic(DynamicValue.Record(newValues._1, newValues._2))
      }
    }

    def orIdentical: Patch[R] =
      if (differences.values.forall(_.isIdentical))
        Patch.identical
      else
        self

    override def invert: Patch[R] = {
      val inverted: ListMap[String, Patch[_]] =
        differences.foldLeft[ListMap[String, Patch[_]]](ListMap.empty) {
          case (map, (key, value)) => map.updated(key, value.invert)
        }
      Record(inverted, schema)
    }

  }

}
