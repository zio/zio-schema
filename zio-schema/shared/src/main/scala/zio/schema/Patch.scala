package zio.schema

import java.math.{BigInteger, MathContext}
import java.time.temporal.{ChronoField, ChronoUnit}
import java.time.{
  DayOfWeek,
  Duration => JDuration,
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
  ZonedDateTime => JZonedDateTime
}

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.diff.Edit
import zio.schema.meta.Migration

/**
 * A `Patch[A]` describes the change that turns one value of type `A` into
 * another. A [[Differ]] produces it (`Schema#diff` uses `Differ.fromSchema`),
 * `patch` applies it to a value, and `invert` returns the patch that undoes it.
 *
 * Use a patch to record what changed between two versions of a value, and to
 * apply or undo that change later.
 *
 * {{{
 * val schema = Schema[(String, Int)]
 * val change = schema.diff(("Ana", 31), ("Ann", 32))
 *
 * change.patch(("Ana", 31))        // Right((Ann,32))
 * change.invert.patch(("Ann", 32)) // Right((Ana,31))
 * }}}
 */
sealed trait Patch[A] { self =>

  /**
   * A symbolic operator for [[zip]].
   */
  def <*>[B](that: Patch[B]): Patch[(A, B)] = self.zip(that)

  /**
   * Combines this patch with `that` into a patch for pairs, which applies this
   * patch to the first element and `that` to the second.
   */
  def zip[B](that: Patch[B]): Patch[(A, B)] = Patch.Tuple(self, that)

  /**
   * Applies this patch to `a`. Returns `Left` with a message when the patch
   * doesn't fit the value, for example a patch for the `Left` side of an
   * `Either` applied to a `Right` value.
   */
  def patch(a: A): Either[String, A]

  /**
   * Returns the patch that reverses this one: applied to the patched value, it
   * gives back the original value. `Total` and `Currency` patches don't hold
   * the original value, so their inverse equals the patch itself.
   */
  def invert: Patch[A]

  /**
   * Returns `true` when this patch makes no change. `Identical` returns `true`;
   * `Tuple`, `Record`, `EitherDiff`, `Fallback` and `Transform` return `true`
   * when their inner patches are identical; the other patches return `false`.
   */
  def isIdentical: Boolean = false

  /**
   * Returns `false` when the patch comes from two values that couldn't be
   * compared, as `NotComparable` does. `Record`, `EitherDiff`, `Fallback` and
   * `Transform` return `false` when an inner patch is not comparable; `Tuple`
   * returns `true` in that case.
   */
  def isComparable: Boolean = true
}

/**
 * Constructors and cases of [[Patch]].
 */
object Patch {

  /**
   * Returns `patch.invert`, the patch that reverses `patch`.
   */
  def invert[A](patch: Patch[A]): Patch[A] = patch.invert

  /**
   * Returns a patch that leaves the value unchanged.
   */
  def identical[A]: Identical[A] = Identical()

  /**
   * Returns the patch for two values that can't be compared. Applying it
   * returns `Left("Non-comparable diff cannot be applied")`.
   */
  def notComparable[A]: NotComparable[A] = NotComparable()

  /**
   * A patch that makes no change: `patch` returns its input.
   */
  final case class Identical[A]() extends Patch[A] {
    override def patch(a: A): Either[String, A] = Right(a)
    override def isIdentical: Boolean           = true

    override def invert: Patch[A] = this
  }

  /**
   * A patch for `Boolean` values. `patch` returns the input XOR `xor`, so
   * `Bool(true)` flips the value and `Bool(false)` keeps it.
   */
  final case class Bool(xor: Boolean) extends Patch[Boolean] {
    override def patch(a: Boolean): Either[String, Boolean] = Right(a ^ xor)

    override def invert: Patch[Boolean] = this
  }

  /**
   * A patch for numeric values: `patch` subtracts `distance` from the input.
   * `Differ.numeric` sets `distance` to `thisValue - thatValue`, so the patch
   * turns `thisValue` into `thatValue`.
   */
  final case class Number[A](distance: A)(implicit ev: Numeric[A]) extends Patch[A] {
    override def patch(input: A): Either[String, A] =
      Right(ev.minus(input, distance))

    override def invert: Patch[A] = Number(ev.negate(distance))
  }

  /**
   * A patch for `java.math.BigInteger` values: `patch` subtracts `distance`
   * from the input.
   */
  final case class BigInt(distance: BigInteger) extends Patch[BigInteger] {
    override def patch(input: BigInteger): Either[String, BigInteger] =
      Right(input.subtract(distance))

    override def invert: Patch[BigInteger] = BigInt(distance.negate())
  }

  /**
   * A patch for `java.math.BigDecimal` values: `patch` rounds the input to
   * `precision` significant digits, then subtracts `distance` with that
   * precision.
   */
  final case class BigDecimal(distance: java.math.BigDecimal, precision: Int) extends Patch[java.math.BigDecimal] {
    override def patch(input: java.math.BigDecimal): Either[String, java.math.BigDecimal] = {
      val mc = new MathContext(precision)
      Right(input.round(mc).subtract(distance, mc))
    }

    override def invert: Patch[java.math.BigDecimal] = BigDecimal(distance.negate(), precision)
  }

  /**
   * A patch for `java.time` values of the standard type `tpe`. `distances`
   * holds one to three component differences whose meaning depends on `tpe`,
   * for example the difference in epoch days for `LocalDate`, or in epoch
   * seconds and nanoseconds for `Instant`.
   *
   * `patch` returns `Left` when `tpe` and the number of distances don't form a
   * supported combination, and when a patched `ZoneOffset` is out of range.
   */
  final case class Temporal[A](distances: List[Long], tpe: StandardType[A]) extends Patch[A] { self =>
    override def patch(a: A): Either[String, A] =
      (tpe, distances) match {
        case (_: StandardType.YearType.type, distance :: Nil) =>
          Right(Year.of(a.asInstanceOf[Year].getValue - distance.toInt).asInstanceOf[A])
        case (_: StandardType.YearMonthType.type, distance :: Nil) =>
          Right(
            YearMonth
              .now()
              .`with`(
                ChronoField.PROLEPTIC_MONTH,
                a.asInstanceOf[YearMonth].getLong(ChronoField.PROLEPTIC_MONTH) - distance
              )
              .asInstanceOf[A]
          )
        case (_: StandardType.ZonedDateTimeType.type, distance :: Nil) =>
          Right(
            a.asInstanceOf[JZonedDateTime]
              .`with`(ChronoField.EPOCH_DAY, a.asInstanceOf[JZonedDateTime].toLocalDate.toEpochDay - distance)
              .asInstanceOf[A]
          )
        case (_: StandardType.InstantType.type, dist1 :: dist2 :: Nil) =>
          Right(
            Instant
              .ofEpochSecond(a.asInstanceOf[Instant].getEpochSecond - dist1, a.asInstanceOf[Instant].getNano() - dist2)
              .asInstanceOf[A]
          )
        case (_: StandardType.LocalTimeType.type, distance :: Nil) =>
          Right(LocalTime.ofNanoOfDay(a.asInstanceOf[LocalTime].toNanoOfDay - distance).asInstanceOf[A])
        case (_: StandardType.LocalDateTimeType.type, dist1 :: dist2 :: Nil) =>
          Right {
            LocalDateTime
              .of(
                LocalDate.ofEpochDay(a.asInstanceOf[LocalDateTime].toLocalDate.toEpochDay - dist1),
                LocalTime.ofNanoOfDay(a.asInstanceOf[LocalDateTime].toLocalTime.toNanoOfDay - dist2)
              )
              .asInstanceOf[A]
          }
        case (_: StandardType.OffsetTimeType.type, dist1 :: dist2 :: Nil) =>
          Right {
            OffsetTime
              .of(
                LocalTime.ofNanoOfDay(a.asInstanceOf[OffsetTime].toLocalTime.toNanoOfDay - dist1),
                ZoneOffset.ofTotalSeconds(a.asInstanceOf[OffsetTime].getOffset.getTotalSeconds - dist2.toInt)
              )
              .asInstanceOf[A]
          }
        case (_: StandardType.OffsetDateTimeType.type, dist1 :: dist2 :: dist3 :: Nil) =>
          Right {
            OffsetDateTime
              .of(
                LocalDate.ofEpochDay(a.asInstanceOf[OffsetDateTime].toLocalDate.toEpochDay - dist1),
                LocalTime.ofNanoOfDay(a.asInstanceOf[OffsetDateTime].toLocalTime.toNanoOfDay - dist2),
                ZoneOffset.ofTotalSeconds(a.asInstanceOf[OffsetDateTime].getOffset.getTotalSeconds - dist3.toInt)
              )
              .asInstanceOf[A]
          }
        case (_: StandardType.PeriodType.type, dayAdjustment :: monthAdjustment :: yearAdjustment :: Nil) =>
          try {
            Right(
              Period
                .of(
                  a.asInstanceOf[Period].getYears - yearAdjustment.toInt,
                  a.asInstanceOf[Period].getMonths - monthAdjustment.toInt,
                  a.asInstanceOf[Period].getDays - dayAdjustment.toInt
                )
                .asInstanceOf[A]
            )
          } catch { case _: Throwable => Left(s"Invalid java.time.Period diff $self") }
        case (_: StandardType.ZoneOffsetType.type, distance :: Nil) =>
          try {
            Right(
              ZoneOffset.ofTotalSeconds(a.asInstanceOf[ZoneOffset].getTotalSeconds + distance.toInt).asInstanceOf[A]
            )
          } catch { case t: Throwable => Left(s"Patched offset is invalid: ${t.getMessage}") }
        case (_: StandardType.DayOfWeekType.type, distance :: Nil) =>
          Right(a.asInstanceOf[DayOfWeek].plus(distance).asInstanceOf[A])
        case (_: StandardType.MonthType.type, distance :: Nil) =>
          Right(a.asInstanceOf[java.time.Month].plus(distance).asInstanceOf[A])
        case (_: StandardType.DurationType.type, dist1 :: dist2 :: Nil) =>
          Right(
            JDuration
              .ofSeconds(a.asInstanceOf[JDuration].getSeconds - dist1, a.asInstanceOf[JDuration].getNano() - dist2)
              .asInstanceOf[A]
          )
        //      // TODO need to deal with leap year differences
        case (_: StandardType.MonthDayType.type, regDiff :: _ :: Nil) =>
          Right(
            MonthDay.from(ChronoUnit.DAYS.addTo(a.asInstanceOf[MonthDay].atYear(2001), regDiff.toLong)).asInstanceOf[A]
          )
        case (_: StandardType.LocalDateType.type, dist :: Nil) =>
          Right(
            LocalDate
              .ofEpochDay(a.asInstanceOf[LocalDate].toEpochDay - dist)
              .asInstanceOf[A]
          )
        case (s, _) => Left(s"Cannot apply temporal diff to value with type $s")
      }

    override def invert: Patch[A] = Temporal(distances.map(-_), tpe)
  }

  /**
   * A patch for `java.time.ZonedDateTime` values. `patch` applies
   * `localDateTimeDiff` to the local date-time and `zoneIdDiff` to the zone id,
   * then combines the results, and returns `Left` when they don't form a valid
   * `ZonedDateTime`.
   */
  final case class ZonedDateTime(localDateTimeDiff: Patch[java.time.LocalDateTime], zoneIdDiff: Patch[String])
      extends Patch[java.time.ZonedDateTime] {
    override def patch(input: JZonedDateTime): scala.Either[String, JZonedDateTime] =
      for {
        patchedLocalDateTime <- localDateTimeDiff.patch(input.toLocalDateTime)
        patchedZoneId        <- zoneIdDiff.patch(input.getZone.getId)
        patched              <- try {
                     Right(JZonedDateTime.of(patchedLocalDateTime, ZoneId.of(patchedZoneId)))
                   } catch {
                     case e: Throwable =>
                       Left(
                         s"Patched ZonedDateTime is not valid. Patched values $patchedLocalDateTime, $patchedZoneId. Error=${e.getMessage}"
                       )
                   }
      } yield patched

    override def invert: Patch[JZonedDateTime] = ZonedDateTime(localDateTimeDiff.invert, zoneIdDiff.invert)
  }

  /**
   * A patch for `java.util.Currency` values, holding the target currency.
   * `Differ.currency` produces it. `patch` returns its input unchanged: it
   * doesn't apply `currencyCodeDiff`. `Differ.fromSchema` diffs currencies as
   * strings and doesn't produce this patch.
   */
  final case class Currency(currencyCodeDiff: java.util.Currency) extends Patch[java.util.Currency] {
    override def patch(input: java.util.Currency): scala.Either[String, java.util.Currency] =
      Right(input)

    override def invert: Patch[java.util.Currency] = Currency(currencyCodeDiff)
  }

  /**
   * A patch for pairs: `patch` applies `leftDifference` to the first element
   * and `rightDifference` to the second.
   */
  final case class Tuple[A, B](leftDifference: Patch[A], rightDifference: Patch[B]) extends Patch[(A, B)] {

    override def isIdentical: Boolean                         = leftDifference.isIdentical && rightDifference.isIdentical
    override def patch(input: (A, B)): Either[String, (A, B)] =
      for {
        l <- leftDifference.patch(input._1)
        r <- rightDifference.patch(input._2)
      } yield (l, r)

    override def invert: Patch[(A, B)] = Tuple(leftDifference.invert, rightDifference.invert)
  }

  /**
   * A patch for `Chunk` values, as a sequence of `Keep`, `Insert` and `Delete`
   * edits built from the longest common subsequence of the two chunks. Strings,
   * lists, sets, maps and binary values are diffed this way too.
   *
   * `patch` walks the input and the edits together, and returns `Left` when an
   * edit doesn't match the input element or the edits don't cover the whole
   * input.
   */
  final case class LCS[A](edits: Chunk[Edit[A]]) extends Patch[Chunk[A]] {
    override def patch(as: Chunk[A]): Either[String, Chunk[A]] = {
      import zio.schema.diff.{Edit => ZEdit}

      @tailrec
      def calc(in: List[A], edits: List[Edit[A]], result: List[A]): Either[String, Chunk[A]] = (in, edits) match {
        case (_ :: _, Nil)                            => Left(s"Incorrect Patch - no instructions for these items: ${in.mkString}.")
        case (h :: _, ZEdit.Delete(s) :: _) if s != h => Left(s"Cannot Delete $s - current letter is $h.")
        case (Nil, ZEdit.Delete(s) :: _)              => Left(s"Cannot Delete $s - no items left to delete.")
        case (_ :: t, ZEdit.Delete(_) :: tail)        => calc(t, tail, result)
        case (h :: _, ZEdit.Keep(s) :: _) if s != h   => Left(s"Cannot Keep $s - current letter is $h.")
        case (Nil, ZEdit.Keep(s) :: _)                => Left(s"Cannot Keep $s - no items left to keep.")
        case (h :: t, ZEdit.Keep(_) :: tail)          => calc(t, tail, result :+ h)
        case (in, ZEdit.Insert(s) :: tail)            => calc(in, tail, result :+ s)
        case (Nil, Nil)                               => Right(Chunk.fromIterable(result))
      }

      calc(as.toList, edits.toList, Nil)
    }

    override def invert: Patch[Chunk[A]] = LCS(edits.map(Edit.invert))
  }

  /**
   * A patch that replaces the input with `value`, whatever the input is.
   * `Differ#optional` produces it when one of the two values is `None`.
   */
  final case class Total[A](value: A) extends Patch[A] {
    override def patch(input: A): Either[String, A] = Right(value)
    override def invert: Patch[A]                   = Total(value)
  }

  /**
   * A patch for `Either` values. A `Left` patch applies to a `Left` value and a
   * `Right` patch to a `Right` value; applying it to the other side returns
   * `Left`.
   */
  final case class EitherDiff[A, B](diff: Either[Patch[A], Patch[B]]) extends Patch[Either[A, B]] {
    override def isIdentical: Boolean = diff.fold(_.isIdentical, _.isIdentical)

    override def isComparable: Boolean = diff.fold(_.isComparable, _.isComparable)

    override def patch(input: Either[A, B]): Either[String, Either[A, B]] = (input, diff) match {
      case (Left(_), Right(_))    => Left(s"Cannot apply a right diff to a left value")
      case (Right(_), Left(_))    => Left(s"Cannot apply a left diff to a right value")
      case (Left(in), Left(diff)) =>
        diff.patch(in).map(Left(_))
      case (Right(in), Right(diff)) =>
        diff.patch(in).map(Right(_))
    }

    override def invert: Patch[Either[A, B]] = diff match {
      case Left(value)  => EitherDiff(Left(value.invert))
      case Right(value) => EitherDiff(Right(value.invert))
    }
  }

  /**
   * A patch for [[zio.schema.Fallback]] values. A `Left` or `Right` patch
   * applies to the matching side; on a `Both` value it patches that side and
   * returns only that side. A `Both` patch on a `Both` value patches both
   * sides, keeps the sides that succeed, and returns `Left` when neither does.
   */
  final case class Fallback[A, B](diff: zio.schema.Fallback[Patch[A], Patch[B]])
      extends Patch[zio.schema.Fallback[A, B]] {
    override def isIdentical: Boolean = diff.fold(_.isIdentical, _.isIdentical)

    override def isComparable: Boolean = diff.fold(_.isComparable, _.isComparable)

    override def patch(input: zio.schema.Fallback[A, B]): Either[String, zio.schema.Fallback[A, B]] =
      (input, diff) match {
        case (zio.schema.Fallback.Left(_), zio.schema.Fallback.Right(_)) =>
          Left(s"Cannot apply a right diff to a left value")
        case (zio.schema.Fallback.Right(_), zio.schema.Fallback.Left(_)) =>
          Left(s"Cannot apply a left diff to a right value")
        case (zio.schema.Fallback.Left(in), zio.schema.Fallback.Left(diff)) =>
          diff.patch(in).map(zio.schema.Fallback.Left(_))
        case (zio.schema.Fallback.Right(in), zio.schema.Fallback.Right(diff)) =>
          diff.patch(in).map(zio.schema.Fallback.Right(_))

        case (zio.schema.Fallback.Both(in, _), zio.schema.Fallback.Left(diff)) =>
          diff.patch(in).map(zio.schema.Fallback.Left(_))
        case (zio.schema.Fallback.Both(_, in), zio.schema.Fallback.Right(diff)) =>
          diff.patch(in).map(zio.schema.Fallback.Right(_))
        case (zio.schema.Fallback.Left(in), zio.schema.Fallback.Both(diff, _)) =>
          diff.patch(in).map(zio.schema.Fallback.Left(_))
        case (zio.schema.Fallback.Right(in), zio.schema.Fallback.Both(_, diff)) =>
          diff.patch(in).map(zio.schema.Fallback.Right(_))
        case (zio.schema.Fallback.Both(inLeft, inRight), zio.schema.Fallback.Both(diffLeft, diffRight)) =>
          (diffLeft.patch(inLeft), diffRight.patch(inRight)) match {
            case (Right(a), Right(b)) => Right(zio.schema.Fallback.Both(a, b))
            case (Left(_), Right(b))  => Right(zio.schema.Fallback.Right(b))
            case (Right(a), Left(_))  => Right(zio.schema.Fallback.Left(a))
            case (Left(_), Left(_))   => Left(s"Diff wasn't appliable neither in left or right fallback")

          }

      }

    override def invert: Patch[zio.schema.Fallback[A, B]] = diff match {
      case zio.schema.Fallback.Left(value)       => Fallback(zio.schema.Fallback.Left(value.invert))
      case zio.schema.Fallback.Right(value)      => Fallback(zio.schema.Fallback.Right(value.invert))
      case zio.schema.Fallback.Both(left, right) => Fallback(zio.schema.Fallback.Both(left.invert, right.invert))
    }
  }

  /**
   * A patch for a type `B` that converts to and from the patched type `A`.
   * `patch` converts the input with `g`, applies `patch` to it, and converts
   * the result back with `f`.
   */
  final case class Transform[A, B](patch: Patch[A], f: A => Either[String, B], g: B => Either[String, A])
      extends Patch[B] {
    override def isIdentical: Boolean = patch.isIdentical

    override def isComparable: Boolean = patch.isComparable

    override def patch(input: B): Either[String, B] =
      for {
        a  <- g(input)
        a1 <- patch.patch(a)
        b  <- f(a1)
      } yield b

    override def invert: Patch[B] = Transform(patch.invert, f, g)
  }

  /**
   * Represents the diff between values that can't be compared, for instance
   * `Left(1)` and `Right("a")`. Applying it returns
   * `Left("Non-comparable diff cannot be applied")`.
   */
  final case class NotComparable[A]() extends Patch[A] {
    override def patch(input: A): Either[String, A] =
      Left(s"Non-comparable diff cannot be applied")

    override def isComparable: Boolean = false

    override def invert: Patch[A] = this
  }

  /**
   * A patch between two schemas, as the migrations from one to the other.
   * `patch` returns `Left("Schema migrations cannot be applied")`.
   */
  final case class SchemaMigration(migrations: Chunk[Migration]) extends Patch[Schema[_]] { self =>

    // TODO Probably need to implement this
    override def patch(input: Schema[_]): Either[String, Schema[_]] = Left(s"Schema migrations cannot be applied")

    /**
     * Returns `Patch.identical` when there are no migrations, otherwise this
     * patch.
     */
    def orIdentical: Patch[Schema[_]] =
      if (migrations.isEmpty) Patch.identical
      else self

    override def invert: Patch[Schema[_]] = SchemaMigration(migrations.reverse)
  }

  /**
   * Map of field-level diffs between two records. The map of differences is
   * keyed to the records field names.
   */
  final case class Record[R](differences: ListMap[String, Patch[_]], schema: Schema.Record[R]) extends Patch[R] {
    self =>
    override def isIdentical: Boolean = differences.forall(_._2.isIdentical)

    override def isComparable: Boolean = differences.forall(_._2.isComparable)

    override def patch(input: R): Either[String, R] = {
      val structure = schema.fields

      val patchedDynamicValue = schema.toDynamic(input) match {
        case DynamicValue.Record(name, values) => {
          differences
            .foldLeft[Either[String, ListMap[String, DynamicValue]]](Right(values)) {
              case (Right(record), (key, diff)) =>
                (structure.find(_.name == key).map(_.schema), values.get(key)) match {
                  case (Some(schema: Schema[b]), Some(oldValue)) =>
                    val oldVal = oldValue.toTypedValue(schema)
                    oldVal
                      .flatMap(v => diff.asInstanceOf[Patch[Any]].patch(v))
                      .map(v => schema.asInstanceOf[Schema[Any]].toDynamic(v)) match {
                      case Left(error)     => Left(error)
                      case Right(newValue) => Right(record + (key -> newValue))
                    }
                  case _ =>
                    Left(s"Values=$values and structure=$structure have incompatible shape.")
                }
              case (Left(string), _) => Left(string)
            }
            .map(r => (name, r))

        }

        case dv => Left(s"Failed to apply record diff. Unexpected dynamic value for record: $dv")
      }

      patchedDynamicValue.flatMap { newValues =>
        schema.fromDynamic(DynamicValue.Record(newValues._1, newValues._2))
      }
    }

    /**
     * Returns `Patch.identical` when every field patch is identical, otherwise
     * this patch.
     */
    def orIdentical: Patch[R] =
      if (differences.values.forall(_.isIdentical))
        Patch.identical
      else
        self

    override def invert: Patch[R] = {
      val inverted: ListMap[String, Patch[_]] =
        differences.foldLeft[ListMap[String, Patch[_]]](ListMap.empty) { case (map, (key, value)) =>
          map.updated(key, value.invert)
        }
      Record(inverted, schema)
    }

  }

}
