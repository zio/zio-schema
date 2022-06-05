package zio.schema.diff

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

import scala.annotation.nowarn
import scala.collection.immutable.ListMap

import zio.schema.StandardType.DurationType
import zio.schema.Diff
import zio.schema.diff.Edit
import zio.{ Chunk, ChunkBuilder }
import zio.schema.Schema
import zio.schema.StandardType
import zio.schema.DynamicValue

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
    case Schema.Primitive(StandardType.ByteType, _)       => numeric[Byte]
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
    case Schema.Primitive(StandardType.DurationType, _)                => duration
    case Schema.Primitive(tpe @ StandardType.LocalTimeType(_), _)      => localTime(tpe)
    case Schema.Primitive(tpe @ StandardType.LocalDateTimeType(_), _)  => localDateTime(tpe)
    case Schema.Primitive(tpe @ StandardType.OffsetTimeType(_), _)     => offsetTime(tpe)
    case Schema.Primitive(tpe @ StandardType.OffsetDateTimeType(_), _) => offsetDateTime(tpe)
    case Schema.Primitive(StandardType.ZonedDateTimeType(fmt), _)      => zonedDateTime(fmt)
    case Schema.Primitive(StandardType.ZoneOffsetType, _)              => zoneOffset
    case Schema.Tuple(leftSchema, rightSchema, _)                      => fromSchema(leftSchema) <*> fromSchema(rightSchema)
    case Schema.Optional(schema, _)                                    => fromSchema(schema).optional
    case Schema.Sequence(schema, g, f, _, _) =>
      fromSchema(schema).chunk.transform(f, g)
    case Schema.SetSchema(s, _)                                                                  => set(s)
    case Schema.MapSchema(k, v, _)                                                               => map(k, v)
    case Schema.Meta(_, _)                                                                       => (_: A, _: A) => Diff.notComparable[Schema[_]]
    case Schema.EitherSchema(leftSchema, rightSchema, _)                                         => either(fromSchema(leftSchema), fromSchema(rightSchema))
    case s @ Schema.Lazy(_)                                                                      => fromSchema(s.schema)
    case Schema.Transform(schema, g, f, _, _)                                                    => fromSchema(schema).transformOrFail(f, g)
    case Schema.Fail(_, _)                                                                       => fail
    case s @ Schema.GenericRecord(_, _)                                                          => record(s)
    case s: Schema.CaseClass0[A]                                                                 => product0(s)
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
    case Schema.Dynamic(_)                                                                                                     => Differ.dynamicValue
    case s @ Schema.SemiDynamic(_, _)                                                                                          => Differ.semiDynamic(s)
  }
  //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

  def unit: Differ[Unit] = (_: Unit, _: Unit) => Diff.identical

  def binary: Differ[Chunk[Byte]] = LCSDiff.apply[Byte]

  //TODO We can probably actually diff DynamicValues properly
  def dynamicValue: Differ[DynamicValue] = new Differ[DynamicValue] {
    def apply(thisValue: DynamicValue, thatValue: DynamicValue): Diff[DynamicValue] = Diff.notComparable[DynamicValue]
  }

  def semiDynamic[A](schema: Schema.SemiDynamic[A]): Differ[(A, Schema[A])] = {
    val _ = schema
    new Differ[(A, Schema[A])] {
      def apply(thisValue: (A, Schema[A]), thatValue: (A, Schema[A])): Diff[(A, Schema[A])] = {
        val valueDiffer                     = Differ.fromSchema(thisValue._2)
        val schemaDiffer: Differ[Schema[A]] = (_: Schema[A], _: Schema[A]) => Diff.identical[Schema[A]]

        valueDiffer.zip(schemaDiffer).apply(thisValue, thatValue)
      }
    }
  }

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

  def duration: Differ[JDuration] =
    (thisDuration: JDuration, thatDuration: JDuration) =>
      if (thisDuration == thatDuration)
        Diff.identical
      else
        Diff.Temporal(
          List[Long](
            thisDuration.getSeconds - thatDuration.getSeconds,
            (thisDuration.getNano - thatDuration.getNano).toLong
          ),
          DurationType
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

