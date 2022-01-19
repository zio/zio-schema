package zio.schema

import java.math.{ BigInteger, MathContext }
import java.time.format.DateTimeFormatter
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
  Period,
  Year,
  YearMonth,
  ZoneId,
  ZoneOffset,
  ZonedDateTime => JZonedDateTime
}
import java.util.UUID

import scala.annotation.{ nowarn, tailrec }
import scala.collection.immutable.ListMap

import zio.schema.ast.Migration
import zio.schema.diff.Edit
import zio.{ Chunk, ChunkBuilder }

trait Differ[A] { self =>

  def apply(thisValue: A, thatValue: A): Diff[A]

  /**
   * A symbolic operator for [[zip]].
   */
  def <*>[B](that: Differ[B]): Differ[(A, B)] = self.zip(that)

  def zip[B](that: Differ[B]): Differ[(A, B)] = Differ.tuple(self, that)

  def transform[B](f: B => A, g: A => B): Differ[B] =
    (thisValue: B, thatValue: B) =>
      Diff.Transform(self(f(thisValue), f(thatValue)), g.andThen(Right(_)), f.andThen(Right(_)))

  def transformOrFail[B](f: B => Either[String, A], g: A => Either[String, B]): Differ[B] =
    (thisValue: B, thatValue: B) =>
      f(thisValue) -> f(thatValue) match {
        case (Right(l), Right(r)) => Diff.Transform(self(l, r), g, f)
        case _                    => Diff.notComparable
      }

  def chunk: Differ[Chunk[A]] = Differ.LCSDiff[A]

  def optional: Differ[Option[A]] = Differ.instancePartial {
    case (Some(l), Some(r)) =>
      Diff.Transform[A, Option[A]](
        self(l, r),
        (a: A) => Right(Some(a)),
        (a: Option[A]) => a.map(Right(_)).getOrElse(Left("Diff cannot be applied to None value"))
      )
    case (Some(_), None) => Diff.Total(None)
    case (None, Some(r)) => Diff.Total(Some(r))
    case (None, None)    => Diff.identical
  }
}

object Differ {
  import ProductDiffer._

  private[schema] object LCSDiff {

    def apply[A]: Differ[Chunk[A]] = new Differ[Chunk[A]] {
      override def apply(original: Chunk[A], modified: Chunk[A]): Diff[Chunk[A]] = {
        var varOriginal                      = original
        var varModified                      = modified
        var longestCommonSubstring: Chunk[A] = getLongestCommonSubsequence(original, modified)

        val buffer: ChunkBuilder[Edit[A]] = ChunkBuilder.make()

        while (longestCommonSubstring.size > 0) {
          val headOfLongestCommonSubstring = longestCommonSubstring(0)
          longestCommonSubstring = longestCommonSubstring.drop(1)

          var headOfModified = varModified(0)
          var loop           = true

          while (loop) {
            headOfModified = varModified(0)
            varModified = varModified.drop(1)
            if (headOfModified != headOfLongestCommonSubstring)
              buffer += Edit.Insert(headOfModified)

            loop = varModified.size > 0 && headOfModified != headOfLongestCommonSubstring
          }

          var headOfOriginal = varOriginal(0)
          loop = true

          while (loop) {
            headOfOriginal = varOriginal(0)
            varOriginal = varOriginal.drop(1)
            if (headOfOriginal != headOfLongestCommonSubstring)
              buffer += Edit.Delete(headOfOriginal)

            loop = varOriginal.size > 0 && headOfOriginal != headOfLongestCommonSubstring
          }

          buffer += Edit.Keep(headOfLongestCommonSubstring)
        }

        while (varModified.size > 0) {
          val headOfModified = varModified(0)
          varModified = varModified.drop(1)
          buffer += Edit.Insert(headOfModified)
        }

        while (varOriginal.size > 0) {
          val headOfOriginal = varOriginal(0)
          varOriginal = varOriginal.drop(1)
          buffer += Edit.Delete(headOfOriginal)
        }

        val edits = buffer.result()

        if (isIdentical(edits)) Diff.identical else Diff.LCS(edits)

      }

      private def isIdentical(edits: Chunk[Edit[A]]): Boolean =
        edits.isEmpty || edits.forall {
          case Edit.Keep(_) => true
          case _            => false
        }

      def getLongestCommonSubsequence(original: Chunk[A], modified: Chunk[A]): Chunk[A] =
        if (original.length == 0 || modified.length == 0) Chunk.empty
        else if (original == modified) original
        else {
          val myersMatrix: Array[Array[Int]]            = initializeMyersMatrix(original, modified)
          val longestCommonSubsequence: ChunkBuilder[A] = ChunkBuilder.make()

          var originalPosition = original.length
          var modifiedPosition = modified.length

          var loop = true

          while (loop) {
            if (myersMatrix(originalPosition)(modifiedPosition) == myersMatrix(originalPosition - 1)(modifiedPosition)) {
              originalPosition -= 1
            } else if (myersMatrix(originalPosition)(modifiedPosition) == myersMatrix(originalPosition)(
                         modifiedPosition - 1
                       )) {
              modifiedPosition -= 1
            } else {
              longestCommonSubsequence += original(originalPosition - 1)
              originalPosition -= 1
              modifiedPosition -= 1
            }

            loop = originalPosition > 0 && modifiedPosition > 0
          }

          longestCommonSubsequence.result().reverse
        }

      private def initializeMyersMatrix(original: Chunk[A], modified: Chunk[A]): Array[Array[Int]] = {
        val originalLength = original.length
        val modifiedLength = modified.length

        val myersMatrix = Array.fill[Int](originalLength + 1, modifiedLength + 1)(0)

        for (i <- 0 until originalLength) {
          for (j <- 0 until modifiedLength) {
            if (original(i) == modified(j)) {
              myersMatrix(i + 1)(j + 1) = myersMatrix(i)(j) + 1
            } else {
              if (myersMatrix(i)(j + 1) >= myersMatrix(i + 1)(j)) {
                myersMatrix(i + 1)(j + 1) = myersMatrix(i)(j + 1)
              } else {
                myersMatrix(i + 1)(j + 1) = myersMatrix(i + 1)(j)
              }
            }
          }
        }

        myersMatrix
      }
    }

    val string: Differ[String] = apply[Int].transform(
      (s: String) => Chunk.fromArray(s.chars().toArray),
      (as: Chunk[Int]) => new String(as.map(_.toChar).toArray)
    )

    def list[A]: Differ[List[A]] = apply[A].transform(
      (as: List[A]) => Chunk.fromIterable(as),
      (as: Chunk[A]) => as.toList
    )

    def map[K, V]: Differ[Map[K, V]] = apply[(K, V)].transform(
      (m: Map[K, V]) => Chunk.fromIterable(m.toSeq),
      (kvs: Chunk[(K, V)]) => kvs.toMap
    )

    def set[A]: Differ[Set[A]] = apply[A].transform(
      (as: Set[A]) => Chunk.fromIterable(as),
      (as: Chunk[A]) => as.toSet
    )

  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  def fromSchema[A](schema: Schema[A]): Differ[A] = schema match {
    case Schema.Primitive(StandardType.UnitType, _)       => unit
    case Schema.Primitive(StandardType.BinaryType, _)     => binary
    case Schema.Primitive(StandardType.IntType, _)        => numeric[Int]
    case Schema.Primitive(StandardType.ShortType, _)      => numeric[Short]
    case Schema.Primitive(StandardType.DoubleType, _)     => numeric[Double]
    case Schema.Primitive(StandardType.FloatType, _)      => numeric[Float]
    case Schema.Primitive(StandardType.LongType, _)       => numeric[Long]
    case Schema.Primitive(StandardType.CharType, _)       => numeric[Char]
    case Schema.Primitive(StandardType.BoolType, _)       => bool
    case Schema.Primitive(StandardType.BigDecimalType, _) => bigDecimal
    case Schema.Primitive(StandardType.BigIntegerType, _) => bigInt
    case Schema.Primitive(StandardType.StringType, _)     => string
    case Schema.Primitive(StandardType.UUIDType, _) =>
      string.transformOrFail[UUID](
        (uuid: UUID) => Right(uuid.toString),
        (s: String) =>
          try {
            Right(UUID.fromString(s))
          } catch { case e: Throwable => Left(s"$s is not a valid UUID: ${e.getMessage}") }
      )
    case Schema.Primitive(StandardType.ZoneIdType, _) =>
      string.transformOrFail[ZoneId](
        (zoneId: ZoneId) => Right(zoneId.getId),
        (s: String) =>
          try {
            Right(ZoneId.of(s))
          } catch { case e: Throwable => Left(s"$s is not a valid ZoneId: ${e.getMessage}") }
      )
    case Schema.Primitive(StandardType.DayOfWeekType, _)               => dayOfWeek
    case Schema.Primitive(StandardType.PeriodType, _)                  => period
    case Schema.Primitive(StandardType.MonthType, _)                   => month
    case Schema.Primitive(StandardType.MonthDayType, _)                => monthDay
    case Schema.Primitive(StandardType.YearType, _)                    => year
    case Schema.Primitive(StandardType.YearMonthType, _)               => yearMonth
    case Schema.Primitive(tpe @ StandardType.LocalDateType(_), _)      => localDate(tpe)
    case Schema.Primitive(tpe @ StandardType.InstantType(_), _)        => instant(tpe)
    case Schema.Primitive(tpe @ StandardType.Duration(_), _)           => duration(tpe)
    case Schema.Primitive(tpe @ StandardType.LocalTimeType(_), _)      => localTime(tpe)
    case Schema.Primitive(tpe @ StandardType.LocalDateTimeType(_), _)  => localDateTime(tpe)
    case Schema.Primitive(tpe @ StandardType.OffsetTimeType(_), _)     => offsetTime(tpe)
    case Schema.Primitive(tpe @ StandardType.OffsetDateTimeType(_), _) => offsetDateTime(tpe)
    case Schema.Primitive(StandardType.ZonedDateTimeType(fmt), _)      => zonedDateTime(fmt)
    case Schema.Primitive(StandardType.ZoneOffsetType, _)              => zoneOffset
    case Schema.Tuple(leftSchema, rightSchema, _)                      => fromSchema(leftSchema) <*> fromSchema(rightSchema)
    case Schema.Optional(schema, _)                                    => fromSchema(schema).optional
    case Schema.Sequence(schema, g, f, _) =>
      fromSchema(schema).chunk.transform(f, g)
    case Schema.SetSchema(s, _)                                                                  => set(s)
    case Schema.MapSchema(k, v, _)                                                               => map(k, v)
    case Schema.Meta(_, _)                                                                       => (_: A, _: A) => Diff.notComparable[Schema[_]]
    case Schema.EitherSchema(leftSchema, rightSchema, _)                                         => either(fromSchema(leftSchema), fromSchema(rightSchema))
    case s @ Schema.Lazy(_)                                                                      => fromSchema(s.schema)
    case Schema.Transform(schema, g, f, _)                                                       => fromSchema(schema).transformOrFail(f, g)
    case Schema.Fail(_, _)                                                                       => fail
    case s @ Schema.GenericRecord(_, _)                                                          => record(s)
    case s: Schema.CaseClass1[_, A]                                                              => product1(s)
    case s: Schema.CaseClass2[_, _, A]                                                           => product2(s)
    case s: Schema.CaseClass3[_, _, _, A]                                                        => product3(s)
    case s: Schema.CaseClass4[_, _, _, _, A]                                                     => product4(s)
    case s: Schema.CaseClass5[_, _, _, _, _, A]                                                  => product5(s)
    case s: Schema.CaseClass6[_, _, _, _, _, _, A]                                               => product6(s)
    case s: Schema.CaseClass7[_, _, _, _, _, _, _, A]                                            => product7(s)
    case s: Schema.CaseClass8[_, _, _, _, _, _, _, _, A]                                         => product8(s)
    case s: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, A]                                      => product9(s)
    case s: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, A]                                  => product10(s)
    case s: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, A]                               => product11(s)
    case s: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, A]                            => product12(s)
    case s: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, A]                         => product13(s)
    case s: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, A]                      => product14(s)
    case s: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]                   => product15(s)
    case s: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]                => product16(s)
    case s: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]             => product17(s)
    case s: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]          => product18(s)
    case s: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]       => product19(s)
    case s: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]    => product20(s)
    case s: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] => product21(s)
    case s: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
      product22(s)
    case Schema.Enum1(c, _)                                                                                                    => enumN(c)
    case Schema.Enum2(c1, c2, _)                                                                                               => enumN(c1, c2)
    case Schema.Enum3(c1, c2, c3, _)                                                                                           => enumN(c1, c2, c3)
    case Schema.Enum4(c1, c2, c3, c4, _)                                                                                       => enumN(c1, c2, c3, c4)
    case Schema.Enum5(c1, c2, c3, c4, c5, _)                                                                                   => enumN(c1, c2, c3, c4, c5)
    case Schema.Enum6(c1, c2, c3, c4, c5, c6, _)                                                                               => enumN(c1, c2, c3, c4, c5, c6)
    case Schema.Enum7(c1, c2, c3, c4, c5, c6, c7, _)                                                                           => enumN(c1, c2, c3, c4, c5, c6, c7)
    case Schema.Enum8(c1, c2, c3, c4, c5, c6, c7, c8, _)                                                                       => enumN(c1, c2, c3, c4, c5, c6, c7, c8)
    case Schema.Enum9(c1, c2, c3, c4, c5, c6, c7, c8, c9, _)                                                                   => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9)
    case Schema.Enum10(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _)                                                             => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
    case Schema.Enum11(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _)                                                        => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
    case Schema.Enum12(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _)                                                   => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
    case Schema.Enum13(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _)                                              => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
    case Schema.Enum14(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _)                                         => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
    case Schema.Enum15(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _)                                    => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
    case Schema.Enum16(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _)                               => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
    case Schema.Enum17(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _)                          => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
    case Schema.Enum18(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _)                     => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
    case Schema.Enum19(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _)                => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
    case Schema.Enum20(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _)           => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
    case Schema.Enum21(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _)      => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
    case Schema.Enum22(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _) => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
    case Schema.EnumN(cs, _)                                                                                                   => enumN(cs.toSeq: _*)
  }
  //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

  def unit: Differ[Unit] = (_: Unit, _: Unit) => Diff.identical

  def binary: Differ[Chunk[Byte]] = LCSDiff.apply[Byte]

  def bool: Differ[Boolean] =
    (thisBool: Boolean, thatBool: Boolean) =>
      if (thisBool ^ thatBool) Diff.Bool(thisBool ^ thatBool) else Diff.identical

  @nowarn def map[K, V](keySchema: Schema[K], valueSchema: Schema[V]): Differ[Map[K, V]] =
    LCSDiff.map[K, V]

  @nowarn def set[A](schema: Schema[A]): Differ[Set[A]] =
    LCSDiff.set[A]

  def numeric[A](implicit numeric: Numeric[A]): Differ[A] =
    (thisValue: A, thatValue: A) =>
      numeric.minus(thisValue, thatValue) match {
        case distance if distance == numeric.zero => Diff.identical
        case distance                             => Diff.Number(distance)
      }

  val period: Differ[Period] =
    (thisPeriod: Period, thatPeriod: Period) =>
      if (thisPeriod == thatPeriod)
        Diff.identical
      else
        Diff.Temporal(
          List(
            (thisPeriod.getDays - thatPeriod.getDays).toLong,
            (thisPeriod.getMonths - thatPeriod.getMonths).toLong,
            (thisPeriod.getYears - thatPeriod.getYears).toLong
          ),
          StandardType.PeriodType
        )

  val year: Differ[Year] =
    (thisYear: Year, thatYear: Year) =>
      if (thisYear == thatYear)
        Diff.identical
      else
        Diff.Temporal(List[Long]((thisYear.getValue - thatYear.getValue).toLong), StandardType.YearType)

  val yearMonth: Differ[YearMonth] =
    (thisYearMonth: YearMonth, thatYearMonth: YearMonth) =>
      if (thisYearMonth == thatYearMonth)
        Diff.identical
      else
        Diff.Temporal(
          List[Long](
            thisYearMonth.getLong(ChronoField.PROLEPTIC_MONTH) - thatYearMonth.getLong(ChronoField.PROLEPTIC_MONTH)
          ),
          StandardType.YearMonthType
        )

  def localDate(tpe: StandardType.LocalDateType): Differ[LocalDate] =
    (thisLocalDate: LocalDate, thatLocalDate: LocalDate) =>
      if (thisLocalDate == thatLocalDate)
        Diff.identical
      else
        Diff.Temporal(List[Long](thisLocalDate.toEpochDay - thatLocalDate.toEpochDay), tpe)

  def instant(tpe: StandardType.InstantType): Differ[Instant] =
    (thisInstant: Instant, thatInstant: Instant) =>
      if (thisInstant == thatInstant)
        Diff.identical
      else
        Diff.Temporal(
          List[Long](
            thisInstant.getEpochSecond - thatInstant.getEpochSecond,
            (thisInstant.getNano - thatInstant.getNano).toLong
          ),
          tpe
        )

  def duration(tpe: StandardType.Duration): Differ[JDuration] =
    (thisDuration: JDuration, thatDuration: JDuration) =>
      if (thisDuration == thatDuration)
        Diff.identical
      else
        Diff.Temporal(
          List[Long](
            thisDuration.getSeconds - thatDuration.getSeconds,
            (thisDuration.getNano - thatDuration.getNano).toLong
          ),
          tpe
        )

  def localTime(tpe: StandardType.LocalTimeType): Differ[LocalTime] =
    (thisLocalTime: LocalTime, thatLocalTime: LocalTime) =>
      if (thisLocalTime == thatLocalTime)
        Diff.identical
      else
        Diff.Temporal(List[Long](thisLocalTime.toNanoOfDay - thatLocalTime.toNanoOfDay), tpe)

  def localDateTime(tpe: StandardType.LocalDateTimeType): Differ[LocalDateTime] =
    (thisLocalDateTime: LocalDateTime, thatLocalDateTime: LocalDateTime) =>
      if (thisLocalDateTime == thatLocalDateTime)
        Diff.identical
      else
        Diff.Temporal(
          List[Long](
            thisLocalDateTime.toLocalDate.toEpochDay - thatLocalDateTime.toLocalDate.toEpochDay,
            thisLocalDateTime.toLocalTime.toNanoOfDay - thatLocalDateTime.toLocalTime.toNanoOfDay
          ),
          tpe
        )

  def offsetTime(tpe: StandardType.OffsetTimeType): Differ[OffsetTime] =
    (thisOffsetTime: OffsetTime, thatOffsetTime: OffsetTime) =>
      if (thisOffsetTime == thatOffsetTime)
        Diff.identical
      else
        Diff.Temporal(
          List[Long](
            thisOffsetTime.toLocalTime.toNanoOfDay - thatOffsetTime.toLocalTime.toNanoOfDay,
            (thisOffsetTime.getOffset.getTotalSeconds - thatOffsetTime.getOffset.getTotalSeconds).toLong
          ),
          tpe
        )

  def offsetDateTime(tpe: StandardType.OffsetDateTimeType): Differ[OffsetDateTime] =
    (thisOffsetDateTime: OffsetDateTime, thatOffsetDateTime: OffsetDateTime) =>
      if (thisOffsetDateTime == thatOffsetDateTime)
        Diff.identical
      else
        Diff.Temporal(
          List[Long](
            thisOffsetDateTime.toLocalDate.toEpochDay - thatOffsetDateTime.toLocalDate.toEpochDay,
            thisOffsetDateTime.toLocalTime.toNanoOfDay - thatOffsetDateTime.toLocalTime.toNanoOfDay,
            (thisOffsetDateTime.getOffset.getTotalSeconds - thatOffsetDateTime.getOffset.getTotalSeconds).toLong
          ),
          tpe
        )

  def zonedDateTime(formatter: DateTimeFormatter): Differ[JZonedDateTime] =
    (thisZonedDateTime: JZonedDateTime, thatZonedDateTime: JZonedDateTime) =>
      if (thisZonedDateTime == thatZonedDateTime)
        Diff.identical
      else
        Diff.ZonedDateTime(
          localDateTime(StandardType.LocalDateTimeType(formatter))(
            thisZonedDateTime.toLocalDateTime,
            thatZonedDateTime.toLocalDateTime
          ),
          string(thisZonedDateTime.getZone.getId, thatZonedDateTime.getZone.getId)
        )

  val zoneOffset: Differ[ZoneOffset] = (thisOffset: ZoneOffset, thatOffset: ZoneOffset) => {
    if (thisOffset.getTotalSeconds == thatOffset.getTotalSeconds)
      Diff.identical
    else
      Diff.Temporal(
        List[Long]((thatOffset.getTotalSeconds - thisOffset.getTotalSeconds).toLong),
        StandardType.ZoneOffsetType
      )
  }

  val dayOfWeek: Differ[DayOfWeek] =
    (thisDay: DayOfWeek, thatDay: DayOfWeek) =>
      if (thisDay == thatDay)
        Diff.identical
      else
        Diff.Temporal(List[Long]((thatDay.getValue - thisDay.getValue).toLong), StandardType.DayOfWeekType)

  val month: Differ[JMonth] =
    (thisMonth: JMonth, thatMonth: JMonth) =>
      if (thisMonth == thatMonth)
        Diff.identical
      else
        Diff.Temporal(List[Long]((thatMonth.getValue - thisMonth.getValue).toLong), StandardType.MonthType)

  val monthDay: Differ[MonthDay] =
    (thisMonthDay: MonthDay, thatMonthDay: MonthDay) =>
      if (thisMonthDay == thatMonthDay)
        Diff.identical
      else
        Diff.Temporal(
          List[Long](
            ChronoUnit.DAYS.between(thisMonthDay.atYear(2001), thatMonthDay.atYear(2001)),
            ChronoUnit.DAYS.between(thisMonthDay.atYear(2000), thatMonthDay.atYear(2000))
          ),
          StandardType.MonthDayType
        )

  val bigInt: Differ[BigInteger] =
    (thisValue: BigInteger, thatValue: BigInteger) =>
      thisValue.subtract(thatValue) match {
        case BigInteger.ZERO => Diff.identical
        case distance        => Diff.BigInt(distance)
      }

  val bigDecimal: Differ[java.math.BigDecimal] =
    (thisValue: java.math.BigDecimal, thatValue: java.math.BigDecimal) => {
      val thatCtx = new MathContext(thatValue.precision())
      thisValue.round(thatCtx).subtract(thatValue, MathContext.UNLIMITED) match {
        case d if d.compareTo(java.math.BigDecimal.ZERO) == 0 => Diff.identical
        case distance                                         => Diff.BigDecimal(distance, thatValue.precision())
      }
    }

  def tuple[A, B](left: Differ[A], right: Differ[B]): Differ[(A, B)] =
    (thisValue: (A, B), thatValue: (A, B)) =>
      (thisValue, thatValue) match {
        case ((thisA, thisB), (thatA, thatB)) =>
          left(thisA, thatA) <*> right(thisB, thatB)
      }

  def either[A, B](left: Differ[A], right: Differ[B]): Differ[Either[A, B]] =
    instancePartial[Either[A, B]] {
      case (Left(l), Left(r))   => Diff.EitherDiff(Left(left(l, r)))
      case (Right(l), Right(r)) => Diff.EitherDiff(Right(right(l, r)))
    }

  def identical[A]: Differ[A] = (_: A, _: A) => Diff.identical

  def fail[A]: Differ[A] = (_: A, _: A) => Diff.notComparable

  def record(schema: Schema.Record[ListMap[String, _]]): Differ[ListMap[String, _]] =
    (thisValue: ListMap[String, _], thatValue: ListMap[String, _]) =>
      if (!(conformsToStructure(thisValue, schema.structure) && conformsToStructure(thatValue, schema.structure)))
        Diff.notComparable
      else
        thisValue.toList.zip(thatValue.toList).zipWithIndex.map {
          case (((thisKey, thisValue), (_, thatValue)), fieldIndex) =>
            thisKey -> fromSchema(schema.structure(fieldIndex).schema)
              .asInstanceOf[Differ[Any]]
              .apply(thisValue, thatValue)
        } match {
          case diffs if diffs.exists(!_._2.isComparable) =>
            Diff.notComparable
          case diffs =>
            Diff.Record(ListMap.empty ++ diffs, schema).orIdentical
        }

  private def conformsToStructure(map: ListMap[String, _], structure: Chunk[Schema.Field[_]]): Boolean =
    structure.foldRight(true) {
      case (_, false)                  => false
      case (field: Schema.Field[a], _) => map.get(field.label).exists(_.isInstanceOf[a])
    }

  def enumN[Z](cases: Schema.Case[_, Z]*): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      cases
        .foldRight[Option[Diff[Z]]](None) {
          case (_, diff @ Some(_)) => diff
          case (subtype: Schema.Case[a, Z], _) =>
            subtype.deconstruct(thisZ) -> (subtype.deconstruct(thatZ)) match {
              case (Some(thisA), Some(thatA)) =>
                val subtypeDiffer: Differ[Z] =
                  fromSchema(subtype.codec).transform(
                    (z: Z) => subtype.unsafeDeconstruct(z),
                    (a: a) => a.asInstanceOf[Z]
                  )
                Some(subtypeDiffer(thisA.asInstanceOf[Z], thatA.asInstanceOf[Z]))
              case _ => None
            }
        }
        .getOrElse(Diff.notComparable)

  def enumeration(structure: ListMap[String, Schema[_]]): Differ[(String, _)] =
    instancePartial[(String, _)] {
      case ((thisKey, thisValue), (thatKey, thatValue)) if thisKey == thatKey =>
        structure
          .get(thisKey)
          .map(fromSchema(_).asInstanceOf[Differ[Any]](thisValue, thatValue))
          .getOrElse(Diff.notComparable)
          .asInstanceOf[Diff[(String, Any)]]
    }

  val string: Differ[String] = LCSDiff.string

  def instancePartial[A](f: PartialFunction[(A, A), Diff[A]]): Differ[A] =
    new Differ[A] {
      override def apply(thisValue: A, thatValue: A): Diff[A] =
        f.applyOrElse((thisValue, thatValue), (_: (A, A)) => Diff.notComparable)
    }

}

sealed trait Diff[A] { self =>

  /**
   * A symbolic operator for [[zip]].
   */
  def <*>[B](that: Diff[B]): Diff[(A, B)] = self.zip(that)

  def zip[B](that: Diff[B]): Diff[(A, B)] = Diff.Tuple(self, that)

  def patch(a: A): Either[String, A]

  def isIdentical: Boolean = false

  def isComparable: Boolean = true
}

object Diff {

  def identical[A]: Identical[A] = Identical()

  def notComparable[A]: NotComparable[A] = NotComparable()

  final case class Identical[A]() extends Diff[A] {
    override def patch(a: A): Either[String, A] = Right(a)
    override def isIdentical: Boolean           = true
  }

  final case class Bool(xor: Boolean) extends Diff[Boolean] {
    override def patch(a: Boolean): Either[String, Boolean] = Right(a ^ xor)
  }

  final case class Number[A](distance: A)(implicit ev: Numeric[A]) extends Diff[A] {
    override def patch(input: A): Either[String, A] =
      Right(ev.minus(input, distance))
  }

  final case class BigInt(distance: BigInteger) extends Diff[BigInteger] {
    override def patch(input: BigInteger): Either[String, BigInteger] =
      Right(input.subtract(distance))
  }

  final case class BigDecimal(distance: java.math.BigDecimal, precision: Int) extends Diff[java.math.BigDecimal] {
    override def patch(input: java.math.BigDecimal): Either[String, java.math.BigDecimal] = {
      val mc = new MathContext(precision)
      Right(input.round(mc).subtract(distance, mc))
    }
  }

  final case class Temporal[A](distances: List[Long], tpe: StandardType[A]) extends Diff[A] { self =>
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
        case (_: StandardType.LocalDateType, distance :: Nil) =>
          Right(LocalDate.ofEpochDay(a.asInstanceOf[LocalDate].toEpochDay - distance).asInstanceOf[A])
        case (_: StandardType.InstantType, dist1 :: dist2 :: Nil) =>
          Right(
            Instant
              .ofEpochSecond(a.asInstanceOf[Instant].getEpochSecond - dist1, a.asInstanceOf[Instant].getNano() - dist2)
              .asInstanceOf[A]
          )
        case (_: StandardType.LocalTimeType, distance :: Nil) =>
          Right(LocalTime.ofNanoOfDay(a.asInstanceOf[LocalTime].toNanoOfDay - distance).asInstanceOf[A])
        case (_: StandardType.LocalDateTimeType, dist1 :: dist2 :: Nil) =>
          Right {
            LocalDateTime
              .of(
                LocalDate.ofEpochDay(a.asInstanceOf[LocalDateTime].toLocalDate.toEpochDay - dist1),
                LocalTime.ofNanoOfDay(a.asInstanceOf[LocalDateTime].toLocalTime.toNanoOfDay - dist2)
              )
              .asInstanceOf[A]
          }
        case (_: StandardType.OffsetTimeType, dist1 :: dist2 :: Nil) =>
          Right {
            OffsetTime
              .of(
                LocalTime.ofNanoOfDay(a.asInstanceOf[OffsetTime].toLocalTime.toNanoOfDay - dist1),
                ZoneOffset.ofTotalSeconds(a.asInstanceOf[OffsetTime].getOffset.getTotalSeconds - dist2.toInt)
              )
              .asInstanceOf[A]
          }
        case (_: StandardType.OffsetDateTimeType, dist1 :: dist2 :: dist3 :: Nil) =>
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
        case (_: StandardType.Duration, dist1 :: dist2 :: Nil) =>
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
        case (s, _) => Left(s"Cannot apply temporal diff to value with type $s")
      }
  }

  final case class ZonedDateTime(localDateTimeDiff: Diff[java.time.LocalDateTime], zoneIdDiff: Diff[String])
      extends Diff[java.time.ZonedDateTime] {
    override def patch(input: JZonedDateTime): scala.Either[String, JZonedDateTime] =
      for {
        patchedLocalDateTime <- localDateTimeDiff.patch(input.toLocalDateTime)
        patchedZoneId        <- zoneIdDiff.patch(input.getZone.getId)
        patched <- try {
                    Right(JZonedDateTime.of(patchedLocalDateTime, ZoneId.of(patchedZoneId)))
                  } catch {
                    case e: Throwable =>
                      Left(
                        s"Patched ZonedDateTime is not valid. Patched values $patchedLocalDateTime, $patchedZoneId. Error=${e.getMessage}"
                      )
                  }
      } yield patched
  }

  final case class Tuple[A, B](leftDifference: Diff[A], rightDifference: Diff[B]) extends Diff[(A, B)] {
    override def patch(input: (A, B)): Either[String, (A, B)] =
      for {
        l <- leftDifference.patch(input._1)
        r <- rightDifference.patch(input._2)
      } yield (l, r)
  }

  final case class LCS[A](edits: Chunk[Edit[A]]) extends Diff[Chunk[A]] {
    override def patch(as: Chunk[A]): Either[String, Chunk[A]] = {
      import zio.schema.diff.{ Edit => ZEdit }

      @tailrec
      def calc(in: List[A], edits: List[Edit[A]], result: List[A]): Either[String, Chunk[A]] = (in, edits) match {
        case (_ :: _, Nil)                            => Left(s"Incorrect Diff - no instructions for these items: ${in.mkString}.")
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
  }

  final case class Total[A](value: A) extends Diff[A] {
    override def patch(input: A): Either[String, A] = Right(value)
  }

  final case class EitherDiff[A, B](diff: Either[Diff[A], Diff[B]]) extends Diff[Either[A, B]] {
    override def isIdentical: Boolean = diff.fold(_.isIdentical, _.isIdentical)

    override def isComparable: Boolean = diff.fold(_.isComparable, _.isComparable)

    override def patch(input: Either[A, B]): Either[String, Either[A, B]] = (input, diff) match {
      case (Left(_), Right(_)) => Left(s"Cannot apply a right diff to a left value")
      case (Right(_), Left(_)) => Left(s"Cannot apply a left diff to a right value")
      case (Left(in), Left(diff)) =>
        diff.patch(in).map(Left(_))
      case (Right(in), Right(diff)) =>
        diff.patch(in).map(Right(_))
    }
  }

  final case class Transform[A, B](diff: Diff[A], f: A => Either[String, B], g: B => Either[String, A])
      extends Diff[B] {
    override def isIdentical: Boolean = diff.isIdentical

    override def isComparable: Boolean = diff.isComparable

    override def patch(input: B): Either[String, B] =
      for {
        a  <- g(input)
        a1 <- diff.patch(a)
        b  <- f(a1)
      } yield b
  }

  /**
   * Represents diff between incomparable values. For instance Left(1) and Right("a")
   */
  final case class NotComparable[A]() extends Diff[A] {
    override def patch(input: A): Either[String, A] =
      Left(s"Non-comparable diff cannot be applied")

    override def isComparable: Boolean = false
  }

  final case class SchemaMigration(migrations: Chunk[Migration]) extends Diff[Schema[_]] { self =>

    //TODO Probably need to implement this
    override def patch(input: Schema[_]): Either[String, Schema[_]] = Left(s"Schema migrations cannot be applied")

    def orIdentical: Diff[Schema[_]] =
      if (migrations.isEmpty) Diff.identical
      else self
  }

  /**
   * Map of field-level diffs between two records. The map of differences
   * is keyed to the records field names.
   */
  final case class Record[R](differences: ListMap[String, Diff[_]], schema: Schema.Record[R]) extends Diff[R] { self =>
    override def isIdentical: Boolean = differences.forall(_._2.isIdentical)

    override def isComparable: Boolean = differences.forall(_._2.isComparable)

    override def patch(input: R): Either[String, R] = {
      val structure = schema.structure

      val patchedDynamicValue = schema.toDynamic(input) match {
        case DynamicValue.Record(values) =>
          differences.foldLeft[Either[String, ListMap[String, DynamicValue]]](Right(values)) {
            case (Right(record), (key, diff)) =>
              (structure.find(_.label == key).map(_.schema), values.get(key)) match {
                case (Some(schema: Schema[b]), Some(oldValue)) =>
                  val oldVal = oldValue.toTypedValue(schema)
                  oldVal
                    .flatMap(v => diff.asInstanceOf[Diff[Any]].patch(v))
                    .map(v => schema.asInstanceOf[Schema[Any]].toDynamic(v)) match {
                    case Left(error)     => Left(error)
                    case Right(newValue) => Right(record + (key -> newValue))
                  }
                case _ =>
                  Left(s"Values=$values and structure=$structure have incompatible shape.")
              }
            case (Left(string), _) => Left(string)
          }
        case dv => Left(s"Failed to apply record diff. Unexpected dynamic value for record: $dv")
      }

      patchedDynamicValue.flatMap { newValues =>
        schema.fromDynamic(DynamicValue.Record(newValues))
      }
    }

    def orIdentical: Diff[R] =
      if (differences.values.forall(_.isIdentical))
        Diff.identical
      else
        self
  }

}

//scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
private[schema] object ProductDiffer {

  def product1[A, Z](schema: Schema.CaseClass1[A, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record[Z](ListMap(fieldDiffer(schema.field, schema.extractField)(thisZ, thatZ)), schema)
        .orIdentical

  def product2[A1, A2, Z](schema: Schema.CaseClass2[A1, A2, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1, schema.extractField1), fieldDiffer(schema.field2, schema.extractField2)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product3[A1, A2, A3, Z](schema: Schema.CaseClass3[A1, A2, A3, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1, schema.extractField1), fieldDiffer(schema.field2, schema.extractField2), fieldDiffer(schema.field3, schema.extractField3)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product4[A1, A2, A3, A4, Z](schema: Schema.CaseClass4[A1, A2, A3, A4, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1, schema.extractField1), fieldDiffer(schema.field2, schema.extractField2), fieldDiffer(schema.field3, schema.extractField3), fieldDiffer(schema.field4, schema.extractField4)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product5[A1, A2, A3, A4, A5, Z](schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1, schema.extractField1), fieldDiffer(schema.field2, schema.extractField2), fieldDiffer(schema.field3, schema.extractField3), fieldDiffer(schema.field4, schema.extractField4), fieldDiffer(schema.field5, schema.extractField5)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product6[A1, A2, A3, A4, A5, A6, Z](schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1, schema.extractField1), fieldDiffer(schema.field2, schema.extractField2), fieldDiffer(schema.field3, schema.extractField3), fieldDiffer(schema.field4, schema.extractField4), fieldDiffer(schema.field5, schema.extractField5), fieldDiffer(schema.field6, schema.extractField6)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product7[A1, A2, A3, A4, A5, A6, A7, Z](schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Diff
        .Record(
          ListMap.empty ++ Chunk(fieldDiffer(schema.field1, schema.extractField1), fieldDiffer(schema.field2, schema.extractField2), fieldDiffer(schema.field3, schema.extractField3), fieldDiffer(schema.field4, schema.extractField4), fieldDiffer(schema.field5, schema.extractField5), fieldDiffer(schema.field6, schema.extractField6), fieldDiffer(schema.field7, schema.extractField7))
            .map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
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
          ).map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  private def fieldDiffer[A, Z](field: Schema.Field[A], extract: Z => A): (Z, Z) => (String, Diff[A]) =
    (thisZ: Z, thatZ: Z) => field.label -> Differ.fromSchema(field.schema)(extract(thisZ), extract(thatZ))
}
