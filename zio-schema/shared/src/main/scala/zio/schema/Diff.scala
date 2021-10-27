package zio.schema

import java.math.BigInteger
import java.time.temporal.{ ChronoField, ChronoUnit, Temporal => JTemporal }
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
  ZonedDateTime
}
import java.util.UUID

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

  def patch[A](schema: Schema[A], diff: Diff): Either[String, A => Either[String, A]] =
    Differ.patch(schema, diff)
}

object Differ {

  def fromSchema[A](schema: Schema[A]): Differ[A] = schema match {
    case Schema.Primitive(StandardType.BinaryType)        => binary
    case Schema.Primitive(StandardType.IntType)           => numeric[Int]
    case Schema.Primitive(StandardType.ShortType)         => numeric[Short]
    case Schema.Primitive(StandardType.DoubleType)        => numeric[Double]
    case Schema.Primitive(StandardType.FloatType)         => numeric[Float]
    case Schema.Primitive(StandardType.LongType)          => numeric[Long]
    case Schema.Primitive(StandardType.CharType)          => numeric[Char]
    case Schema.Primitive(StandardType.BoolType)          => bool
    case Schema.Primitive(StandardType.BigDecimalType)    => bigDecimal
    case Schema.Primitive(StandardType.BigIntegerType)    => bigInt
    case Schema.Primitive(StandardType.StringType)        => string
    case Schema.Primitive(StandardType.UUIDType)          => string.transform(_.toString)
    case Schema.Primitive(StandardType.ZoneId)            => string.transform[ZoneId](_.getId)
    case Schema.Primitive(StandardType.DayOfWeekType)     => dayOfWeek
    case Schema.Primitive(StandardType.Month)             => month
    case Schema.Primitive(StandardType.MonthDay)          => monthDay
    case Schema.Primitive(StandardType.Year)              => year
    case Schema.Primitive(StandardType.YearMonth)         => yearMonth
    case Schema.Primitive(StandardType.LocalDate(_))      => localDate
    case Schema.Primitive(StandardType.Instant(_))        => instant
    case Schema.Primitive(StandardType.Duration(_))       => duration
    case Schema.Primitive(StandardType.LocalTime(_))      => localTime
    case Schema.Primitive(StandardType.LocalDateTime(_))  => localDateTime
    case Schema.Primitive(StandardType.OffsetTime(_))     => offsetTime
    case Schema.Primitive(StandardType.OffsetDateTime(_)) => offsetDateTime
    case Schema.Primitive(StandardType.ZonedDateTime(_))  => zonedDateTime
    case Schema.Tuple(leftSchema, rightSchema)            => fromSchema(leftSchema) <*> fromSchema(rightSchema)
    case Schema.Optional(schema)                          => fromSchema(schema).optional
    case Schema.Sequence(schema, _, f)                    => fromSchema(schema).foreach(f)
    case Schema.EitherSchema(leftSchema, rightSchema)     => either(fromSchema(leftSchema), fromSchema(rightSchema))
    case s @ Schema.Lazy(_)                               => fromSchema(s.schema)
    case Schema.Transform(schema, _, f)                   => fromSchema(schema).transformOrFail(f)
    case Schema.Fail(_)                                   => fail
    case Schema.GenericRecord(structure)                  => record(structure.toChunk)
    case ProductDiffer(differ)                            => differ
    case Schema.Enum1(c)                                  => enum(c)
    case Schema.Enum2(c1, c2)                             => enum(c1, c2)
    case Schema.Enum3(c1, c2, c3)                         => enum(c1, c2, c3)
    case Schema.EnumN(cs)                                 => enum(cs.toSeq: _*)
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

  val zonedDateTime: Differ[ZonedDateTime] =
    (thisZonedDateTime: ZonedDateTime, thatZonedDateTime: ZonedDateTime) =>
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

  def enum[Z](cases: Schema.Case[_, Z]*): Differ[Z] =
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

  def patch[A](schema: Schema[A], diff: Diff): Either[String, A => Either[String, A]] =
    (schema, diff) match {
      case (_, Diff.NotComparable)                                                    => Left(s"Not Comparable: Schema=$schema and Diff=$diff.")
      case (_, Diff.Identical)                                                        => Right((a: A) => Right(a))
      case (Schema.Primitive(StandardType.StringType), diff)                          => string.patch[String](schema, diff)
      case (Schema.Primitive(StandardType.UUIDType), diff)                            => string.patch[UUID](schema, diff)
      case (Schema.Primitive(StandardType.IntType), Diff.Number(distance: Int))       => patchNumeric[Int](distance)
      case (Schema.Primitive(StandardType.ShortType), Diff.Number(distance: Short))   => patchNumeric[Short](distance)
      case (Schema.Primitive(StandardType.DoubleType), Diff.Number(distance: Double)) => patchNumeric[Double](distance)
      case (Schema.Primitive(StandardType.FloatType), Diff.Number(distance: Float))   => patchNumeric[Float](distance)
      case (Schema.Primitive(StandardType.LongType), Diff.Number(distance: Long))     => patchNumeric[Long](distance)
      case (Schema.Primitive(StandardType.CharType), Diff.Number(distance: Char))     => patchNumeric[Char](distance)
      case (Schema.Primitive(StandardType.BigDecimalType), Diff.BigDecimal(distance: java.math.BigDecimal)) =>
        Right((a: A) => Right(a.subtract(distance)))
      case (Schema.Primitive(StandardType.BigIntegerType), Diff.BigInt(distance: java.math.BigInteger)) =>
        Right((a: A) => Right(a.subtract(distance)))
      case (Schema.Primitive(StandardType.BoolType), Diff.Bool(xor: Boolean)) =>
        Right((a: A) => Right(a ^ xor))
      case (Schema.Primitive(StandardType.BinaryType), Diff.Sequence(diffs)) =>
        Right { (a: Chunk[Byte]) =>
          diffs
            .zipAll(a)
            .flatMap {
              case (Some(Diff.Total(right: Byte, Diff.Tag.Right)), _) => Some(Right(right))
              case (Some(Diff.Total(_, Diff.Tag.Left)), _)            => None
              case (Some(Diff.Binary(xor)), Some(value: Byte))        => Some(Right((value ^ xor).toByte))
              case (Some(Diff.Identical), Some(value))                => Some(Right(value))
              case (Some(diff), _)                                    => Some(Left(s"Schema=$schema should not contain Diff=$diff."))
              case (None, Some(value))                                => Some(Left(s"Diff missing for value=$value."))
              case (None, None)                                       => Some(Left(s"Unknown error in binary sequence."))
            }
            .partitionMap(identity) match {
            case (Chunk.empty, values) => Right(values)
            case (errors, _)           => Left(s"Running patch produced the following error(s): ${errors.toList}.")
          }
        }

      case (Schema.Primitive(StandardType.ZoneId), diff) => string.patch[ZoneId](schema, diff)
      case (Schema.Primitive(StandardType.Year), Diff.Temporal(distance :: Nil)) =>
        Right((a: A) => Right(Year.of(a.getValue - distance.toInt)))
      case (Schema.Primitive(StandardType.YearMonth), Diff.Temporal(distance :: Nil)) =>
        Right { (a: A) =>
          Right(YearMonth.now().`with`(ChronoField.PROLEPTIC_MONTH, a.getLong(ChronoField.PROLEPTIC_MONTH) - distance))
        }
      case (Schema.Primitive(StandardType.LocalDate(_)), Diff.Temporal(distance :: Nil)) =>
        Right((a: A) => Right(LocalDate.ofEpochDay(a.toEpochDay - distance)))
      case (Schema.Primitive(StandardType.Instant(_)), Diff.Temporal(dist1 :: dist2 :: Nil)) =>
        Right((a: A) => Right(Instant.ofEpochSecond(a.getEpochSecond - dist1, a.getNano() - dist2)))
      case (Schema.Primitive(StandardType.LocalTime(_)), Diff.Temporal(distance :: Nil)) =>
        Right((a: A) => Right(LocalTime.ofNanoOfDay(a.toNanoOfDay - distance)))
      case (Schema.Primitive(StandardType.LocalDateTime(_)), Diff.Temporal(dist1 :: dist2 :: Nil)) =>
        Right { (a: A) =>
          Right {
            LocalDateTime.of(
              LocalDate.ofEpochDay(a.toLocalDate.toEpochDay - dist1),
              LocalTime.ofNanoOfDay(a.toLocalTime.toNanoOfDay - dist2)
            )
          }
        }
      case (Schema.Primitive(StandardType.OffsetTime(_)), Diff.Temporal(dist1 :: dist2 :: Nil)) =>
        Right { (a: A) =>
          Right {
            OffsetTime.of(
              LocalTime.ofNanoOfDay(a.toLocalTime.toNanoOfDay - dist1),
              ZoneOffset.ofTotalSeconds(a.getOffset.getTotalSeconds - dist2.toInt)
            )
          }
        }
      case (Schema.Primitive(StandardType.OffsetDateTime(_)), Diff.Temporal(dist1 :: dist2 :: dist3 :: Nil)) =>
        Right { (a: A) =>
          Right {
            OffsetDateTime.of(
              LocalDate.ofEpochDay(a.toLocalDate.toEpochDay - dist1),
              LocalTime.ofNanoOfDay(a.toLocalTime.toNanoOfDay - dist2),
              ZoneOffset.ofTotalSeconds(a.getOffset.getTotalSeconds - dist3.toInt)
            )
          }
        }
      case (Schema.Primitive(StandardType.ZonedDateTime(_)), Diff.ZonedDateTime(Diff.Identical, Diff.Identical)) =>
        Right((a: A) => Right(a))
      case (Schema.Primitive(StandardType.ZonedDateTime(_)), Diff.ZonedDateTime(Diff.Identical, Diff.Myers(edits))) =>
        Right { (a: A) =>
          (
            MyersDiff.calculateStringFromEdits(a.getZone.getId, edits).map { zoneIdString =>
              ZonedDateTime.of(a.toLocalDateTime, ZoneId.of(zoneIdString))
            }
          )
        }
      case (
          Schema.Primitive(StandardType.ZonedDateTime(_)),
          Diff.ZonedDateTime(Diff.Temporal(dist1 :: dist2 :: Nil), Diff.Myers(edits))
          ) =>
        Right { (a: A) =>
          {
            val localDateTime = LocalDateTime.of(
              LocalDate.ofEpochDay(a.toLocalDate.toEpochDay - dist1),
              LocalTime.ofNanoOfDay(a.toLocalTime.toNanoOfDay - dist2)
            )

            MyersDiff.calculateStringFromEdits(a.getZone.getId, edits).map { zoneIdString =>
              ZonedDateTime.of(localDateTime, ZoneId.of(zoneIdString))
            }
          }
        }
      case (
          Schema.Primitive(StandardType.ZonedDateTime(_)),
          Diff.ZonedDateTime(Diff.Temporal(dist1 :: dist2 :: Nil), Diff.Identical)
          ) =>
        Right { (a: A) =>
          {
            val localDateTime = LocalDateTime.of(
              LocalDate.ofEpochDay(a.toLocalDate.toEpochDay - dist1),
              LocalTime.ofNanoOfDay(a.toLocalTime.toNanoOfDay - dist2)
            )

            Right {
              ZonedDateTime.of(localDateTime, a.getZone)
            }
          }
        }

      case (Schema.Primitive(StandardType.DayOfWeekType), Diff.Temporal(distance :: Nil)) =>
        Right((a: A) => Right(a.plus(distance)))
      case (Schema.Primitive(StandardType.Month), Diff.Temporal(distance :: Nil)) =>
        Right((a: A) => Right(a.plus(distance)))
      case (Schema.Primitive(StandardType.Duration(_)), Diff.Temporal(dist1 :: dist2 :: Nil)) => {
        Right((a: A) => Right(JDuration.ofSeconds(a.getSeconds - dist1, a.getNano() - dist2)))
      }

      // TODO need to figure out how to deal with this -- which one to add regDiff or leapDiff?
      case (Schema.Primitive(StandardType.MonthDay), Diff.Temporal(regDiff :: _ :: Nil)) =>
        Right((a: A) => Right(MonthDay.from(ChronoUnit.DAYS.addTo(a.atYear(2001), regDiff.toLong))))

      case (s @ Schema.Lazy(_), diff) => patch(s.schema, diff)

      case (Schema.Optional(_), Diff.Total(_, Diff.Tag.Left))      => Right((_: A) => Right(None))
      case (Schema.Optional(_), Diff.Total(right, Diff.Tag.Right)) => Right((_: A) => Right(Some(right)))
      case (Schema.Optional(schema), diff)                         => patch(schema, diff).map(f => (a: A) => f(a.get).map(Some(_)))

      case (Schema.Tuple(leftSchema, rightSchema), Diff.Tuple(leftDiff, rightDiff)) => {
        patch(leftSchema, leftDiff) -> patch(rightSchema, rightDiff) match {
          case (Left(e1), Left(e2)) => Left(s"Errors: $e1 and $e2.")
          case (_, Left(e))         => Left(e)
          case (Left(e), _)         => Left(e)
          case (Right(f1), Right(f2)) =>
            Right((a: A) => {
              f1(a._1) -> f2(a._2) match {
                case (Left(e1), Left(e2)) => Left(s"Errors: $e1 and $e2.")
                case (_, Left(e))         => Left(e)
                case (Left(e), _)         => Left(e)
                case (Right(l), Right(r)) => Right((l, r))
              }
            })
        }
      }

      case (Schema.Sequence(schema, fromChunk, toChunk), Diff.Sequence(diffs)) =>
        Right { (a: A) =>
          diffs
            .zipAll(toChunk(a))
            .flatMap {
              case (Some(Diff.Total(right, Diff.Tag.Right)), _) => Some(Right(right))
              case (Some(Diff.Total(_, Diff.Tag.Left)), _)      => None
              case (Some(diff), Some(value))                    => Some(patch(schema, diff).flatMap(f => f(value)))
              case (None, Some(value))                          => Some(Left(s"Diff missing for value=$value."))
              case (Some(diff), None)                           => Some(Left(s"Value missing for Diff=$diff."))
              case (None, None)                                 => Some(Left(s"Unknown error in sequence."))
            }
            .partitionMap(identity) match {
            case (Chunk.empty, values) => Right(fromChunk(values))
            case (errors, _)           => Left(s"Running patch produced the following error(s): ${errors.toList}.")
          }
        }

      case (Schema.EitherSchema(_, rightSchema), Diff.Either(diff, Diff.Tag.Right)) =>
        patch(rightSchema, diff).map { f => (a: A) =>
          a.flatMap(f(_)) match {
            case Left(error)  => Left(error.toString)
            case Right(value) => Right(Right(value))
          }
        }

      case (Schema.EitherSchema(leftSchema, _), Diff.Either(diff, Diff.Tag.Left)) =>
        patch(leftSchema, diff).map { f => (a: A) =>
          a.left.flatMap(f(_)) match {
            case Left(error)  => Left(error)
            case Right(value) => Right(Left(value))
          }
        }

      case (Schema.Transform(schema, f, g), diff) =>
        Right { (a: A) =>
          for {
            to      <- g(a)
            patchF  <- patch(schema, diff)
            backToA <- patchF(to).flatMap(f)
          } yield backToA
        }

      case (Schema.GenericRecord(structure: FieldSet), Diff.Record(diffs: ListMap[String, Diff])) =>
        Right { (a: A) =>
          val values: ListMap[String, DynamicValue] = schema.toDynamic(a) match {
            case DynamicValue.Record(values) => values
            case _                           => ListMap.empty
          }
          patchProductData(structure.toChunk, values, diffs).asInstanceOf[Either[String, A]]
        }

      case (schema: Schema.Record[A], Diff.Record(diffs: ListMap[String, Diff])) =>
        Right { (a: A) =>
          val values: ListMap[String, DynamicValue] = schema.toDynamic(a) match {
            case DynamicValue.Record(values) => values
            case _                           => ListMap.empty
          }
          patchProductData(schema.structure, values, diffs)
            .map(m => Chunk.fromIterable(m.values))
            .flatMap(schema.rawConstruct)
        }

      case (Schema.Enum1(c1), diff)         => patchEnumData(diff, c1)
      case (Schema.Enum2(c1, c2), diff)     => patchEnumData(diff, c1, c2)
      case (Schema.Enum3(c1, c2, c3), diff) => patchEnumData(diff, c1, c2, c3)
      case (Schema.EnumN(cases), diff)      => patchEnumData(diff, cases.toSeq: _*)

      case (schema @ Schema.Fail(_), _) => Left(s"Failed Schema=$schema cannot be patched.")
      case (_, _)                       => Left(s"Incompatible Schema=$schema and Diff=$diff.")
    }

  def patchEnumData[A](diff: Diff, cases: Schema.Case[_, A]*): Either[String, A => Either[String, A]] =
    Right { (a: A) =>
      cases
        .foldRight[Option[Either[String, A]]](None) {
          case (_, diff @ Some(_)) => diff
          case (subtype, _) =>
            subtype.deconstruct(a) -> subtype.codec match {
              case (Some(_), schema: Schema[_]) =>
                Some(patch(schema.asInstanceOf[Schema[A]], diff).flatMap(_.apply(a)))
              case _ => None
            }
        }
        .getOrElse(Left(s"Incompatible Enum cases=$cases and A=$a."))
    }

  def patchProductData(
    structure: Chunk[Schema.Field[_]],
    values: ListMap[String, DynamicValue],
    diffs: ListMap[String, Diff]
  ): Either[String, ListMap[String, Any]] =
    diffs.foldLeft[Either[String, ListMap[String, Any]]](Right(values)) {
      case (Right(record), (key, diff)) =>
        (structure.find(_.label == key).map(_.schema), values.get(key)) match {
          case (Some(schema: Schema[b]), Some(oldValue)) =>
            val oldVal = oldValue.toTypedValue(schema)
            val patchF = patch(schema, diff)
            oldVal.flatMap(v => patchF.flatMap(_.apply(v))) match {
              case Left(error)     => Left(error)
              case Right(newValue) => Right(record + (key -> newValue))
            }
          case _ =>
            Left(s"Values=$values and structure=$structure have incompatible shape.")
        }
      case (Left(string), _) => Left(string)
    }

  def patchTemporal[A <: JTemporal](units: ChronoUnit, distance: Long): Right[Nothing, A => Right[Nothing, A]] =
    Right((a: A) => Right(units.addTo(a, distance)))

  def patchNumeric[A](distance: A)(implicit numeric: Numeric[A]): Right[Nothing, A => Right[Nothing, A]] =
    Right((a: A) => Right(numeric.minus(a, distance)))
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
