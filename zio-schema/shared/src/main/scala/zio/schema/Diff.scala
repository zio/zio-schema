package zio.schema

import java.math.BigInteger
import java.time.temporal.{ ChronoField, ChronoUnit }
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
  ZoneOffset,
  ZonedDateTime => JZonedDateTime
}
import java.util.UUID

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.internal.MyersDiff
import scala.runtime.ByteRef

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

  def foreach[Col](toChunk: Col => Chunk[A]): Differ[Col] =
    (theseAs: Col, thoseAs: Col) =>
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
    case Schema.Primitive(StandardType.BinaryType, _)        => binary
    case Schema.Primitive(StandardType.IntType, _)           => numeric[Int]
    case Schema.Primitive(StandardType.ShortType, _)         => numeric[Short]
    case Schema.Primitive(StandardType.DoubleType, _)        => numeric[Double]
    case Schema.Primitive(StandardType.FloatType, _)         => numeric[Float]
    case Schema.Primitive(StandardType.LongType, _)          => numeric[Long]
    case Schema.Primitive(StandardType.CharType, _)          => numeric[Char]
    case Schema.Primitive(StandardType.BoolType, _)          => bool
    case Schema.Primitive(StandardType.BigDecimalType, _)    => bigDecimal
    case Schema.Primitive(StandardType.BigIntegerType, _)    => bigInt
    case Schema.Primitive(StandardType.StringType, _)        => string
    case Schema.Primitive(StandardType.UUIDType, _)          => string.transform(_.toString)
    case Schema.Primitive(StandardType.ZoneId, _)            => string.transform[ZoneId](_.getId)
    case Schema.Primitive(StandardType.DayOfWeekType, _)     => dayOfWeek
    case Schema.Primitive(StandardType.Month, _)             => month
    case Schema.Primitive(StandardType.MonthDay, _)          => monthDay
    case Schema.Primitive(StandardType.Year, _)              => year
    case Schema.Primitive(StandardType.YearMonth, _)         => yearMonth
    case Schema.Primitive(StandardType.LocalDate(_), _)      => localDate
    case Schema.Primitive(StandardType.Instant(_), _)        => instant
    case Schema.Primitive(StandardType.Duration(_), _)       => duration
    case Schema.Primitive(StandardType.LocalTime(_), _)      => localTime
    case Schema.Primitive(StandardType.LocalDateTime(_), _)  => localDateTime
    case Schema.Primitive(StandardType.OffsetTime(_), _)     => offsetTime
    case Schema.Primitive(StandardType.OffsetDateTime(_), _) => offsetDateTime
    case Schema.Primitive(StandardType.ZonedDateTime(_), _)  => zonedDateTime
    case Schema.Tuple(leftSchema, rightSchema, _)            => fromSchema(leftSchema) <*> fromSchema(rightSchema)
    case Schema.Optional(schema, _)                          => fromSchema(schema).optional
    case Schema.Sequence(schema, _, f, _)                    => fromSchema(schema).foreach(f)
    case Schema.EitherSchema(leftSchema, rightSchema, _)     => either(fromSchema(leftSchema), fromSchema(rightSchema))
    case s @ Schema.Lazy(_)                                  => fromSchema(s.schema)
    case Schema.Transform(schema, _, f, _)                   => fromSchema(schema).transformOrFail(f)
    case Schema.Fail(_, _)                                   => fail
    case Schema.GenericRecord(structure, _)                  => record(structure.toChunk)
    case ProductDiffer(differ)                               => differ
    case Schema.Enum1(c, _)                                  => enum(c)
    case Schema.Enum2(c1, c2, _)                             => enum(c1, c2)
    case Schema.Enum3(c1, c2, c3, _)                         => enum(c1, c2, c3)
    case Schema.EnumN(cs, _)                                 => enum(cs.toSeq: _*)
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

  val year: Differ[Year] =
    (thisYear: Year, thatYear: Year) =>
      if (thisYear == thatYear)
        Diff.Identical
      else
        Diff.Temporal(List[Long]((thisYear.getValue - thatYear.getValue).toLong))

  val yearMonth: Differ[YearMonth] =
    (thisYearMonth: YearMonth, thatYearMonth: YearMonth) =>
      if (thisYearMonth == thatYearMonth)
        Diff.Identical
      else
        Diff.Temporal(
          List[Long](
            thisYearMonth.getLong(ChronoField.PROLEPTIC_MONTH) - thatYearMonth.getLong(ChronoField.PROLEPTIC_MONTH)
          )
        )

  val localDate: Differ[LocalDate] =
    (thisLocalDate: LocalDate, thatLocalDate: LocalDate) =>
      if (thisLocalDate == thatLocalDate)
        Diff.Identical
      else
        Diff.Temporal(List[Long](thisLocalDate.toEpochDay - thatLocalDate.toEpochDay))

  val instant: Differ[Instant] =
    (thisInstant: Instant, thatInstant: Instant) =>
      if (thisInstant == thatInstant)
        Diff.Identical
      else
        Diff.Temporal(
          List[Long](
            thisInstant.getEpochSecond - thatInstant.getEpochSecond,
            (thisInstant.getNano - thatInstant.getNano).toLong
          )
        )

  val duration: Differ[JDuration] =
    (thisDuration: JDuration, thatDuration: JDuration) =>
      if (thisDuration == thatDuration)
        Diff.Identical
      else
        Diff.Temporal(
          List[Long](
            thisDuration.getSeconds - thatDuration.getSeconds,
            (thisDuration.getNano - thatDuration.getNano).toLong
          )
        )

  val localTime: Differ[LocalTime] =
    (thisLocalTime: LocalTime, thatLocalTime: LocalTime) =>
      if (thisLocalTime == thatLocalTime)
        Diff.Identical
      else
        Diff.Temporal(List[Long](thisLocalTime.toNanoOfDay - thatLocalTime.toNanoOfDay))

  val localDateTime: Differ[LocalDateTime] =
    (thisLocalDateTime: LocalDateTime, thatLocalDateTime: LocalDateTime) =>
      if (thisLocalDateTime == thatLocalDateTime)
        Diff.Identical
      else
        Diff.Temporal(
          List[Long](
            thisLocalDateTime.toLocalDate.toEpochDay - thatLocalDateTime.toLocalDate.toEpochDay,
            thisLocalDateTime.toLocalTime.toNanoOfDay - thatLocalDateTime.toLocalTime.toNanoOfDay
          )
        )

  val offsetTime: Differ[OffsetTime] =
    (thisOffsetTime: OffsetTime, thatOffsetTime: OffsetTime) =>
      if (thisOffsetTime == thatOffsetTime)
        Diff.Identical
      else
        Diff.Temporal(
          List[Long](
            thisOffsetTime.toLocalTime.toNanoOfDay - thatOffsetTime.toLocalTime.toNanoOfDay,
            (thisOffsetTime.getOffset.getTotalSeconds - thatOffsetTime.getOffset.getTotalSeconds).toLong
          )
        )

  val offsetDateTime: Differ[OffsetDateTime] =
    (thisOffsetDateTime: OffsetDateTime, thatOffsetDateTime: OffsetDateTime) =>
      if (thisOffsetDateTime == thatOffsetDateTime)
        Diff.Identical
      else
        Diff.Temporal(
          List[Long](
            thisOffsetDateTime.toLocalDate.toEpochDay - thatOffsetDateTime.toLocalDate.toEpochDay,
            thisOffsetDateTime.toLocalTime.toNanoOfDay - thatOffsetDateTime.toLocalTime.toNanoOfDay,
            (thisOffsetDateTime.getOffset.getTotalSeconds - thatOffsetDateTime.getOffset.getTotalSeconds).toLong
          )
        )

  val zonedDateTime: Differ[JZonedDateTime] =
    (thisZonedDateTime: JZonedDateTime, thatZonedDateTime: JZonedDateTime) =>
      if (thisZonedDateTime == thatZonedDateTime)
        Diff.Identical
      else
        Diff.ZonedDateTime(
          localDateTime(thisZonedDateTime.toLocalDateTime, thatZonedDateTime.toLocalDateTime),
          MyersDiff(thisZonedDateTime.getZone.getId, thatZonedDateTime.getZone.getId)
        )

  val dayOfWeek: Differ[DayOfWeek] =
    (thisDay: DayOfWeek, thatDay: DayOfWeek) =>
      if (thisDay == thatDay)
        Diff.Identical
      else
        Diff.Temporal(List[Long]((thatDay.getValue - thisDay.getValue).toLong))

  val month: Differ[JMonth] =
    (thisMonth: JMonth, thatMonth: JMonth) =>
      if (thisMonth == thatMonth)
        Diff.Identical
      else
        Diff.Temporal(List[Long]((thatMonth.getValue - thisMonth.getValue).toLong))

  val monthDay: Differ[MonthDay] =
    (thisMonthDay: MonthDay, thatMonthDay: MonthDay) =>
      if (thisMonthDay == thatMonthDay)
        Diff.Identical
      else
        Diff.Temporal(
          List[Long](
            ChronoUnit.DAYS.between(thisMonthDay.atYear(2001), thatMonthDay.atYear(2001)),
            ChronoUnit.DAYS.between(thisMonthDay.atYear(2000), thatMonthDay.atYear(2000))
          )
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

  def either[A, B](left: Differ[A], right: Differ[B]): Differ[Either[A, B]] =
    instancePartial[Either[A, B]] {
      case (Left(l), Left(r))   => Diff.Either(left(l, r), Diff.Tag.Left)
      case (Right(l), Right(r)) => Diff.Either(right(l, r), Diff.Tag.Right)
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
      case (field: Schema.Field[a], _) => map.get(field.label).exists(_.isInstanceOf[a])
    }

  def enumN[Z](cases: Schema.Case[_, Z]*): Differ[Z] =
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

  //scalafmt: { maxColumn = 400 }
  def patch[A](a: A)(implicit schema: Schema[A]): Either[String, A] = {
    import zio.schema.Diff._
    import zio.schema.Schema._

    (schema, self) match {
      case (_, NotComparable)                                                                      => Left(s"Not Comparable: Schema=$schema and Diff=$self.")
      case (_, Identical)                                                                          => Right(a)
      case (Schema.Primitive(StandardType.StringType, _), Myers(edits))                            => patchStringFromMyersEdits(a, edits)
      case (Schema.Primitive(StandardType.UUIDType, _), Myers(edits))                              => patchStringFromMyersEdits(a.toString, edits).map(UUID.fromString)
      case (Schema.Primitive(StandardType.ZoneId, _), Myers(edits))                                => patchStringFromMyersEdits(a.getId(), edits).map(ZoneId.of)
      case (Primitive(StandardType.IntType, _), Number(distance: Int))                             => patchNumeric[Int](a, distance)
      case (Primitive(StandardType.ShortType, _), Number(distance: Short))                         => patchNumeric[Short](a, distance)
      case (Primitive(StandardType.DoubleType, _), Number(distance: Double))                       => patchNumeric[Double](a, distance)
      case (Primitive(StandardType.FloatType, _), Number(distance: Float))                         => patchNumeric[Float](a, distance)
      case (Primitive(StandardType.LongType, _), Number(distance: Long))                           => patchNumeric[Long](a, distance)
      case (Primitive(StandardType.CharType, _), Number(distance: Char))                           => patchNumeric[Char](a, distance)
      case (Primitive(StandardType.BoolType, _), Bool(xor: Boolean))                               => Right(a ^ xor)
      case (Primitive(StandardType.BigDecimalType, _), BigDecimal(distance: java.math.BigDecimal)) => Right(a.subtract(distance))
      case (Primitive(StandardType.BigIntegerType, _), BigInt(distance: java.math.BigInteger))     => Right(a.subtract(distance))
      case (Primitive(StandardType.BinaryType, _), Diff.Sequence(diffs)) =>
        diffs
          .zipAll(a.asInstanceOf[Chunk[Byte]])
          .flatMap {
            case (Some(Total(right: Byte, Tag.Right)), _) => Some(Right(right))
            case (Some(Total(_, Tag.Left)), _)            => None
            case (Some(Binary(xor)), Some(value: Byte))   => Some(Right((value ^ xor).toByte))
            case (Some(Identical), Some(value))           => Some(Right(value))
            case (Some(diff), _)                          => Some(Left(s"Schema=$schema should not contain Diff=$diff."))
            case (None, Some(value))                      => Some(Left(s"Diff missing for value=$value."))
            case (None, None)                             => Some(Left(s"Unknown error in binary sequence."))
          }
          .partitionMap(identity) match {
          case (Chunk.empty, values) => Right(values)
          case (errors, _)           => Left(s"Running patch produced the following error(s): ${errors.toList}.")
        }
      case (Primitive(StandardType.Year, _), Temporal(distance :: Nil))             => Right(Year.of(a.getValue - distance.toInt))
      case (Primitive(StandardType.YearMonth, _), Temporal(distance :: Nil))        => Right(YearMonth.now().`with`(ChronoField.PROLEPTIC_MONTH, a.getLong(ChronoField.PROLEPTIC_MONTH) - distance))
      case (Primitive(StandardType.LocalDate(_), _), Temporal(distance :: Nil))     => Right(LocalDate.ofEpochDay(a.toEpochDay - distance))
      case (Primitive(StandardType.Instant(_), _), Temporal(dist1 :: dist2 :: Nil)) => Right(Instant.ofEpochSecond(a.getEpochSecond - dist1, a.getNano() - dist2))
      case (Primitive(StandardType.LocalTime(_), _), Temporal(distance :: Nil))     => Right(LocalTime.ofNanoOfDay(a.toNanoOfDay - distance))
      case (Primitive(StandardType.LocalDateTime(_), _), Temporal(dist1 :: dist2 :: Nil)) =>
        Right {
          LocalDateTime.of(
            LocalDate.ofEpochDay(a.toLocalDate.toEpochDay - dist1),
            LocalTime.ofNanoOfDay(a.toLocalTime.toNanoOfDay - dist2)
          )
        }
      case (Primitive(StandardType.OffsetTime(_), _), Temporal(dist1 :: dist2 :: Nil)) =>
        Right {
          OffsetTime.of(
            LocalTime.ofNanoOfDay(a.toLocalTime.toNanoOfDay - dist1),
            ZoneOffset.ofTotalSeconds(a.getOffset.getTotalSeconds - dist2.toInt)
          )
        }
      case (Primitive(StandardType.OffsetDateTime(_), _), Temporal(dist1 :: dist2 :: dist3 :: Nil)) =>
        Right {
          OffsetDateTime.of(
            LocalDate.ofEpochDay(a.toLocalDate.toEpochDay - dist1),
            LocalTime.ofNanoOfDay(a.toLocalTime.toNanoOfDay - dist2),
            ZoneOffset.ofTotalSeconds(a.getOffset.getTotalSeconds - dist3.toInt)
          )
        }
      case (Primitive(StandardType.ZonedDateTime(_), _), ZonedDateTime(Identical, Identical)) => Right(a)
      case (Primitive(StandardType.ZonedDateTime(_), _), ZonedDateTime(Identical, Myers(edits))) =>
        patchStringFromMyersEdits(a.getZone.getId, edits).map { zoneIdString =>
          JZonedDateTime.of(a.toLocalDateTime, ZoneId.of(zoneIdString))
        }
      case (Primitive(StandardType.ZonedDateTime(_), _), ZonedDateTime(Temporal(dist1 :: dist2 :: Nil), Identical)) => {
        val localDateTime = LocalDateTime.of(LocalDate.ofEpochDay(a.toLocalDate.toEpochDay - dist1), LocalTime.ofNanoOfDay(a.toLocalTime.toNanoOfDay - dist2))
        Right(JZonedDateTime.of(localDateTime, a.getZone))
      }
      case (Primitive(StandardType.ZonedDateTime(_), _), ZonedDateTime(Temporal(dist1 :: dist2 :: Nil), Myers(edits))) => {
        val localDateTime = LocalDateTime.of(LocalDate.ofEpochDay(a.toLocalDate.toEpochDay - dist1), LocalTime.ofNanoOfDay(a.toLocalTime.toNanoOfDay - dist2))
        patchStringFromMyersEdits(a.getZone.getId, edits).map { zoneIdString =>
          JZonedDateTime.of(localDateTime, ZoneId.of(zoneIdString))
        }
      }
      case (Primitive(StandardType.DayOfWeekType, _), Temporal(distance :: Nil))     => Right(a.plus(distance))
      case (Primitive(StandardType.Month, _), Temporal(distance :: Nil))             => Right(a.plus(distance))
      case (Primitive(StandardType.Duration(_), _), Temporal(dist1 :: dist2 :: Nil)) => Right(JDuration.ofSeconds(a.getSeconds - dist1, a.getNano() - dist2))
      // TODO need to deal with leap year differences
      case (Primitive(StandardType.MonthDay), Temporal(regDiff :: _ :: Nil)) => Right(MonthDay.from(ChronoUnit.DAYS.addTo(a.atYear(2001), regDiff.toLong)))
      case (s @ Schema.Lazy(_), diff)                                        => diff.patch(a)(s.schema)
      case (Optional(_), Total(_, Diff.Tag.Left))                            => Right(None)
      case (schema: Optional[t], Total(right, Diff.Tag.Right))               => Right(Some(right.asInstanceOf[t]))
      case (schema: Optional[t], diff) =>
        a match {
          case None       => Right(None)
          case Some(b: t) => diff.patch(b)(schema.codec).map(Some(_))
        }

      case (schema: EitherSchema[_, r], Diff.Either(diff, Diff.Tag.Right)) =>
        a match {
          case Right(b: r) => diff.patch(b)(schema.right).map(Right(_))
          case e @ Left(_) => Left(s"Value should be Right not: $e.")
        }

      case (schema: EitherSchema[l, _], Diff.Either(diff, Diff.Tag.Left)) =>
        a match {
          case Left(b: l)   => diff.patch(b)(schema.left).map(Left(_))
          case e @ Right(_) => Left(s"Value should be Left not: $e.")
        }

      case (schema: Schema.Tuple[l, r], Diff.Tuple(leftDiff, rightDiff)) => {
        val (left: l, right: r) = a
        val leftPatch           = leftDiff.patch(left)(schema.left)
        val rightPatch          = rightDiff.patch(right)(schema.right)

        (leftPatch, rightPatch) match {
          case (Left(e1), Left(e2)) => Left(s"Errors: $e1 and $e2.")
          case (_, Left(e))         => Left(e)
          case (Left(e), _)         => Left(e)
          case (Right(l), Right(r)) => Right((l, r))
        }
      }

      case (schema: Schema.Sequence[col, t], Diff.Sequence(diffs)) =>
        diffs
          .zipAll(schema.toChunk(a))
          .flatMap {
            case (Some(Total(right, Tag.Right)), _) => Some(Right(right))
            case (Some(Total(_, Tag.Left)), _)      => None
            case (Some(diff), Some(value))          => Some(diff.patch(value)(schema.schemaA))
            case (None, Some(value))                => Some(Left(s"Diff missing for value=$value."))
            case (Some(diff), None)                 => Some(Left(s"Value missing for Diff=$diff."))
            case (None, None)                       => Some(Left(s"Unknown error in sequence."))
          }
          .partitionMap(identity) match {
          case (Chunk.empty, values) => Right(schema.fromChunk(values.asInstanceOf[Chunk[t]]))
          case (errors, _)           => Left(s"Running patch produced the following error(s): ${errors.toList}.")
        }

      case (Schema.Transform(schema, f, g, _), diff) =>
        for {
          b       <- g(a)
          patched <- diff.patch(b)(schema)
          backToA <- f(patched)
        } yield backToA

      case (Schema.GenericRecord(structure: FieldSet, _), Diff.Record(diffs: ListMap[String, Diff])) => {
        val values: ListMap[String, DynamicValue] = schema.toDynamic(a) match {
          case DynamicValue.Record(values) => values
          case _                           => ListMap.empty
        }
        patchProductData(structure.toChunk, values, diffs).asInstanceOf[scala.Either[String, A]]
      }

      case (schema: Schema.Record[A], Diff.Record(diffs: ListMap[String, Diff])) => {
        val values: ListMap[String, DynamicValue] = schema.toDynamic(a) match {
          case DynamicValue.Record(values) => values
          case _                           => ListMap.empty
        }
        patchProductData(schema.structure, values, diffs)
          .map(m => Chunk.fromIterable(m.values))
          .flatMap(schema.rawConstruct)
      }

      case (Schema.Enum1(c1, _), diff)         => patchEnumData(a, diff, c1)
      case (Schema.Enum2(c1, c2, _), diff)     => patchEnumData(a, diff, c1, c2)
      case (Schema.Enum3(c1, c2, c3, _), diff) => patchEnumData(a, diff, c1, c2, c3)
      case (Schema.EnumN(cases, _), diff)      => patchEnumData(a, diff, cases.toSeq: _*)
      case (schema @ Fail(_, _), _)            => Left(s"Failed Schema=$schema cannot be patched.")
      case (_, _)                              => Left(s"Incompatible Schema=$schema and Diff=$self.")
    }
  }

  def patchNumeric[A](a: A, distance: A)(implicit numeric: Numeric[A]) =
    Right(numeric.minus(a, distance))

  def patchProductData(structure: Chunk[Schema.Field[_]], values: ListMap[String, DynamicValue], diffs: ListMap[String, Diff]): Either[String, ListMap[String, Any]] =
    diffs.foldLeft[Either[String, ListMap[String, Any]]](Right(values)) {
      case (Right(record), (key, diff)) =>
        (structure.find(_.label == key).map(_.schema), values.get(key)) match {
          case (Some(schema: Schema[b]), Some(oldValue)) =>
            val oldVal = oldValue.toTypedValue(schema)
            oldVal.flatMap(v => diff.patch(v)(schema)) match {
              case Left(error)     => Left(error)
              case Right(newValue) => Right(record + (key -> newValue))
            }
          case _ =>
            Left(s"Values=$values and structure=$structure have incompatible shape.")
        }
      case (Left(string), _) => Left(string)
    }

  def patchEnumData[A](a: A, diff: Diff, cases: Schema.Case[_, A]*): Either[String, A] =
    cases
      .foldRight[Option[Either[String, A]]](None) {
        case (_, diff @ Some(_)) => diff
        case (subtype, _) =>
          subtype.deconstruct(a) -> subtype.codec match {
            case (Some(_), schema: Schema[_]) =>
              Some(diff.patch(a)(schema.asInstanceOf[Schema[A]]))
            case _ => None
          }
      }
      .getOrElse(Left(s"Incompatible Enum cases=$cases and A=$a."))

  def patchStringFromMyersEdits(input: String, edits: Chunk[Diff.Edit]): Either[String, String] = {
    import zio.schema.Diff.Edit.{ Delete, Insert, Keep }

    @tailrec
    def calc(in: List[Char], edits: List[Diff.Edit], result: List[Char]): Either[String, String] = (in, edits) match {
      case (_ :: _, Nil)                               => Left(s"Incorrect Diff - no istructions for these letters: ${in.mkString}.")
      case (h :: _, Delete(s) :: _) if s != h.toString => Left(s"Cannot Delete $s - current letter is $h.")
      case (Nil, Delete(s) :: _)                       => Left(s"Cannot Delete $s - no letters left to delete.")
      case (_ :: t, Delete(_) :: tail)                 => calc(t, tail, result)
      case (h :: _, Keep(s) :: _) if s != h.toString   => Left(s"Cannot Keep $s - current letter is $h.")
      case (Nil, Keep(s) :: _)                         => Left(s"Cannot Keep $s - no letters left to keep.")
      case (h :: t, Keep(_) :: tail)                   => calc(t, tail, result :+ h)
      case (in, Insert(s) :: tail)                     => calc(in, tail, result ++ s.toList)
      case (Nil, Nil)                                  => Right(result.mkString)
    }

    calc(input.toList, edits.toList, Nil)
  }
}

object Diff {
  case object Identical extends Diff

  final case class Binary(xor: Int) extends Diff

  final case class Bool(xor: Boolean) extends Diff

  final case class Number[A: Numeric](distance: A) extends Diff

  final case class BigInt(distance: BigInteger) extends Diff

  final case class BigDecimal(distance: java.math.BigDecimal) extends Diff

  final case class Temporal(distances: List[Long]) extends Diff

  final case class ZonedDateTime(localDateTimeDiff: Diff, zoneIdDiff: Diff) extends Diff

  final case class Tuple(leftDifference: Diff, rightDifference: Diff) extends Diff

  final case class Myers(edits: Chunk[Edit]) extends Diff

  final case class Total[A](value: A, tag: Tag) extends Diff

  final case class Either(value: Diff, tag: Tag) extends Diff

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

//scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
object ProductDiffer {

  def unapply[A](schema: Schema[A]): Option[Differ[A]] = schema match {
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
        .Record(ListMap(fieldDiffer(schema.field, schema.extractField)(thisZ, thatZ)))
        .orIdentical

  def product2[A1, A2, Z](schema: Schema.CaseClass2[A1, A2, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1, schema.extractField1), fieldDiffer(schema.field2, schema.extractField2)).map(_.apply(thisZ, thatZ)))
        .orIdentical

  def product3[A1, A2, A3, Z](schema: Schema.CaseClass3[A1, A2, A3, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1, schema.extractField1), fieldDiffer(schema.field2, schema.extractField2), fieldDiffer(schema.field3, schema.extractField3)).map(_.apply(thisZ, thatZ)))
        .orIdentical

  def product4[A1, A2, A3, A4, Z](schema: Schema.CaseClass4[A1, A2, A3, A4, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1, schema.extractField1), fieldDiffer(schema.field2, schema.extractField2), fieldDiffer(schema.field3, schema.extractField3), fieldDiffer(schema.field4, schema.extractField4)).map(_.apply(thisZ, thatZ)))
        .orIdentical

  def product5[A1, A2, A3, A4, A5, Z](schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1, schema.extractField1), fieldDiffer(schema.field2, schema.extractField2), fieldDiffer(schema.field3, schema.extractField3), fieldDiffer(schema.field4, schema.extractField4), fieldDiffer(schema.field5, schema.extractField5)).map(_.apply(thisZ, thatZ)))
        .orIdentical

  def product6[A1, A2, A3, A4, A5, A6, Z](schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1, schema.extractField1), fieldDiffer(schema.field2, schema.extractField2), fieldDiffer(schema.field3, schema.extractField3), fieldDiffer(schema.field4, schema.extractField4), fieldDiffer(schema.field5, schema.extractField5), fieldDiffer(schema.field6, schema.extractField6)).map(_.apply(thisZ, thatZ)))
        .orIdentical

  def product7[A1, A2, A3, A4, A5, A6, A7, Z](schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(fieldDiffer(schema.field1, schema.extractField1), fieldDiffer(schema.field2, schema.extractField2), fieldDiffer(schema.field3, schema.extractField3), fieldDiffer(schema.field4, schema.extractField4), fieldDiffer(schema.field5, schema.extractField5), fieldDiffer(schema.field6, schema.extractField6), fieldDiffer(schema.field7, schema.extractField7))
            .map(_.apply(thisZ, thatZ))
        )
        .orIdentical

  def product8[A1, A2, A3, A4, A5, A6, A7, A8, Z](schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z]): Differ[Z] =
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

  def product9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]): Differ[Z] =
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

  def product10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]): Differ[Z] =
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

  def product11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]): Differ[Z] =
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

  def product12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]): Differ[Z] =
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

  def product13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]): Differ[Z] =
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

  def product14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]): Differ[Z] =
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

  def product15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]): Differ[Z] =
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

  def product16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]): Differ[Z] =
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

  def product17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]): Differ[Z] =
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

  def product18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]): Differ[Z] =
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

  def product19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](schema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]): Differ[Z] =
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

  def product20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](schema: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z]): Differ[Z] =
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

  def product21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](schema: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z]): Differ[Z] =
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

  def product22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](schema: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z]): Differ[Z] =
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
