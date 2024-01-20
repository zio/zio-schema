package zio.schema

import java.math.{ BigInteger, MathContext }
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

import zio.schema.diff.Edit
import zio.{ Chunk, ChunkBuilder }

trait Differ[A] { self =>

  def apply(thisValue: A, thatValue: A): Patch[A]

  /**
   * A symbolic operator for [[zip]].
   */
  def <*>[B](that: Differ[B]): Differ[(A, B)] = self.zip(that)

  def zip[B](that: Differ[B]): Differ[(A, B)] = Differ.tuple(self, that)

  def transform[B](f: B => A, g: A => B): Differ[B] =
    (thisValue: B, thatValue: B) =>
      Patch.Transform(self(f(thisValue), f(thatValue)), g.andThen(Right(_)), f.andThen(Right(_)))

  def transformOrFail[B](f: B => Either[String, A], g: A => Either[String, B]): Differ[B] =
    (thisValue: B, thatValue: B) =>
      f(thisValue) -> f(thatValue) match {
        case (Right(l), Right(r)) => Patch.Transform(self(l, r), g, f)
        case _                    => Patch.notComparable
      }

  def chunk: Differ[Chunk[A]] = Differ.LCSDiff[A]

  def optional: Differ[Option[A]] = Differ.instancePartial {
    case (Some(l), Some(r)) =>
      Patch.Transform[A, Option[A]](
        self(l, r),
        (a: A) => Right(Some(a)),
        (a: Option[A]) => a.map(Right(_)).getOrElse(Left("Patch cannot be applied to None value"))
      )
    case (Some(_), None) => Patch.Total(None)
    case (None, Some(r)) => Patch.Total(Some(r))
    case (None, None)    => Patch.identical
  }
}

object Differ {
  import ProductDiffer._

  private[schema] object LCSDiff {

    def apply[A]: Differ[Chunk[A]] = new Differ[Chunk[A]] {
      override def apply(original: Chunk[A], modified: Chunk[A]): Patch[Chunk[A]] = {
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

        if (isIdentical(edits)) Patch.identical else Patch.LCS(edits)

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
      (s: String) => Chunk.fromIterable(s.toList.map(_.toInt)),
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
    case Schema.Primitive(StandardType.ByteType, _)       => numeric[Byte]
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
    case Schema.Primitive(StandardType.DayOfWeekType, _)      => dayOfWeek
    case Schema.Primitive(StandardType.PeriodType, _)         => period
    case Schema.Primitive(StandardType.MonthType, _)          => month
    case Schema.Primitive(StandardType.MonthDayType, _)       => monthDay
    case Schema.Primitive(StandardType.YearType, _)           => year
    case Schema.Primitive(StandardType.YearMonthType, _)      => yearMonth
    case Schema.Primitive(StandardType.LocalDateType, _)      => localDate
    case Schema.Primitive(StandardType.InstantType, _)        => instant
    case Schema.Primitive(StandardType.DurationType, _)       => duration
    case Schema.Primitive(StandardType.LocalTimeType, _)      => localTime
    case Schema.Primitive(StandardType.LocalDateTimeType, _)  => localDateTime
    case Schema.Primitive(StandardType.OffsetTimeType, _)     => offsetTime
    case Schema.Primitive(StandardType.OffsetDateTimeType, _) => offsetDateTime
    case Schema.Primitive(StandardType.ZonedDateTimeType, _)  => zonedDateTime
    case Schema.Primitive(StandardType.ZoneOffsetType, _)     => zoneOffset
    case Schema.Tuple2(leftSchema, rightSchema, _)            => fromSchema(leftSchema) <*> fromSchema(rightSchema)
    case Schema.Optional(schema, _)                           => fromSchema(schema).optional
    case Schema.Sequence(schema, g, f, _, _) =>
      fromSchema(schema).chunk.transform(f, g)
    case Schema.Set(s, _)                                                                        => set(s)
    case Schema.Map(k, v, _)                                                                     => map(k, v)
    case Schema.Either(leftSchema, rightSchema, _)                                               => either(fromSchema(leftSchema), fromSchema(rightSchema))
    case Schema.Fallback(leftSchema, rightSchema, _, _)                                          => fallback(fromSchema(leftSchema), fromSchema(rightSchema))
    case s @ Schema.Lazy(_)                                                                      => fromSchema(s.schema)
    case Schema.Transform(schema, g, f, _, _)                                                    => fromSchema(schema).transformOrFail(f, g)
    case Schema.Fail(_, _)                                                                       => fail
    case s @ Schema.GenericRecord(_, _, _)                                                       => record(s)
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
    case Schema.Enum1(_, c, _)                                                                                                    => enumN(c)
    case Schema.Enum2(_, c1, c2, _)                                                                                               => enumN(c1, c2)
    case Schema.Enum3(_, c1, c2, c3, _)                                                                                           => enumN(c1, c2, c3)
    case Schema.Enum4(_, c1, c2, c3, c4, _)                                                                                       => enumN(c1, c2, c3, c4)
    case Schema.Enum5(_, c1, c2, c3, c4, c5, _)                                                                                   => enumN(c1, c2, c3, c4, c5)
    case Schema.Enum6(_, c1, c2, c3, c4, c5, c6, _)                                                                               => enumN(c1, c2, c3, c4, c5, c6)
    case Schema.Enum7(_, c1, c2, c3, c4, c5, c6, c7, _)                                                                           => enumN(c1, c2, c3, c4, c5, c6, c7)
    case Schema.Enum8(_, c1, c2, c3, c4, c5, c6, c7, c8, _)                                                                       => enumN(c1, c2, c3, c4, c5, c6, c7, c8)
    case Schema.Enum9(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, _)                                                                   => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9)
    case Schema.Enum10(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _)                                                             => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
    case Schema.Enum11(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _)                                                        => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
    case Schema.Enum12(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _)                                                   => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
    case Schema.Enum13(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _)                                              => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
    case Schema.Enum14(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _)                                         => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
    case Schema.Enum15(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _)                                    => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
    case Schema.Enum16(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _)                               => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
    case Schema.Enum17(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _)                          => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
    case Schema.Enum18(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _)                     => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
    case Schema.Enum19(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _)                => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
    case Schema.Enum20(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _)           => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
    case Schema.Enum21(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _)      => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
    case Schema.Enum22(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _) => enumN(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
    case Schema.EnumN(_, cs, _)                                                                                                   => enumN(cs.toSeq: _*)
    case Schema.Dynamic(_)                                                                                                        => Differ.dynamicValue
  }
  //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

  def unit: Differ[Unit] = (_: Unit, _: Unit) => Patch.identical

  def binary: Differ[Chunk[Byte]] = LCSDiff.apply[Byte]

  //TODO We can probably actually diff DynamicValues properly
  def dynamicValue: Differ[DynamicValue] = new Differ[DynamicValue] {
    def apply(thisValue: DynamicValue, thatValue: DynamicValue): Patch[DynamicValue] = Patch.notComparable[DynamicValue]
  }

  def bool: Differ[Boolean] =
    (thisBool: Boolean, thatBool: Boolean) =>
      if (thisBool ^ thatBool) Patch.Bool(thisBool ^ thatBool) else Patch.identical

  @nowarn def map[K, V](keySchema: Schema[K], valueSchema: Schema[V]): Differ[Map[K, V]] =
    LCSDiff.map[K, V]

  @nowarn def set[A](schema: Schema[A]): Differ[Set[A]] =
    LCSDiff.set[A]

  def numeric[A](implicit numeric: Numeric[A]): Differ[A] =
    (thisValue: A, thatValue: A) =>
      numeric.minus(thisValue, thatValue) match {
        case distance if distance == numeric.zero => Patch.identical
        case distance                             => Patch.Number(distance)
      }

  val period: Differ[Period] =
    (thisPeriod: Period, thatPeriod: Period) =>
      if (thisPeriod == thatPeriod)
        Patch.identical
      else
        Patch.Temporal(
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
        Patch.identical
      else
        Patch.Temporal(List[Long]((thisYear.getValue - thatYear.getValue).toLong), StandardType.YearType)

  val yearMonth: Differ[YearMonth] =
    (thisYearMonth: YearMonth, thatYearMonth: YearMonth) =>
      if (thisYearMonth == thatYearMonth)
        Patch.identical
      else
        Patch.Temporal(
          List[Long](
            thisYearMonth.getLong(ChronoField.PROLEPTIC_MONTH) - thatYearMonth.getLong(ChronoField.PROLEPTIC_MONTH)
          ),
          StandardType.YearMonthType
        )

  def localDate: Differ[LocalDate] =
    (thisLocalDate: LocalDate, thatLocalDate: LocalDate) =>
      if (thisLocalDate == thatLocalDate)
        Patch.identical
      else
        Patch.Temporal(List[Long](thisLocalDate.toEpochDay - thatLocalDate.toEpochDay), StandardType.LocalDateType)

  def instant: Differ[Instant] =
    (thisInstant: Instant, thatInstant: Instant) =>
      if (thisInstant == thatInstant)
        Patch.identical
      else
        Patch.Temporal(
          List[Long](
            thisInstant.getEpochSecond - thatInstant.getEpochSecond,
            (thisInstant.getNano - thatInstant.getNano).toLong
          ),
          StandardType.InstantType
        )

  def duration: Differ[JDuration] =
    (thisDuration: JDuration, thatDuration: JDuration) =>
      if (thisDuration == thatDuration)
        Patch.identical
      else
        Patch.Temporal(
          List[Long](
            thisDuration.getSeconds - thatDuration.getSeconds,
            (thisDuration.getNano - thatDuration.getNano).toLong
          ),
          StandardType.DurationType
        )

  def localTime: Differ[LocalTime] =
    (thisLocalTime: LocalTime, thatLocalTime: LocalTime) =>
      if (thisLocalTime == thatLocalTime)
        Patch.identical
      else
        Patch.Temporal(List[Long](thisLocalTime.toNanoOfDay - thatLocalTime.toNanoOfDay), StandardType.LocalTimeType)

  def localDateTime: Differ[LocalDateTime] =
    (thisLocalDateTime: LocalDateTime, thatLocalDateTime: LocalDateTime) =>
      if (thisLocalDateTime == thatLocalDateTime)
        Patch.identical
      else
        Patch.Temporal(
          List[Long](
            thisLocalDateTime.toLocalDate.toEpochDay - thatLocalDateTime.toLocalDate.toEpochDay,
            thisLocalDateTime.toLocalTime.toNanoOfDay - thatLocalDateTime.toLocalTime.toNanoOfDay
          ),
          StandardType.LocalDateTimeType
        )

  def offsetTime: Differ[OffsetTime] =
    (thisOffsetTime: OffsetTime, thatOffsetTime: OffsetTime) =>
      if (thisOffsetTime == thatOffsetTime)
        Patch.identical
      else
        Patch.Temporal(
          List[Long](
            thisOffsetTime.toLocalTime.toNanoOfDay - thatOffsetTime.toLocalTime.toNanoOfDay,
            (thisOffsetTime.getOffset.getTotalSeconds - thatOffsetTime.getOffset.getTotalSeconds).toLong
          ),
          StandardType.OffsetTimeType
        )

  def offsetDateTime: Differ[OffsetDateTime] =
    (thisOffsetDateTime: OffsetDateTime, thatOffsetDateTime: OffsetDateTime) =>
      if (thisOffsetDateTime == thatOffsetDateTime)
        Patch.identical
      else
        Patch.Temporal(
          List[Long](
            thisOffsetDateTime.toLocalDate.toEpochDay - thatOffsetDateTime.toLocalDate.toEpochDay,
            thisOffsetDateTime.toLocalTime.toNanoOfDay - thatOffsetDateTime.toLocalTime.toNanoOfDay,
            (thisOffsetDateTime.getOffset.getTotalSeconds - thatOffsetDateTime.getOffset.getTotalSeconds).toLong
          ),
          StandardType.OffsetDateTimeType
        )

  def zonedDateTime: Differ[JZonedDateTime] =
    (thisZonedDateTime: JZonedDateTime, thatZonedDateTime: JZonedDateTime) =>
      if (thisZonedDateTime == thatZonedDateTime)
        Patch.identical
      else
        Patch.ZonedDateTime(
          localDateTime(
            thisZonedDateTime.toLocalDateTime,
            thatZonedDateTime.toLocalDateTime
          ),
          string(thisZonedDateTime.getZone.getId, thatZonedDateTime.getZone.getId)
        )

  val zoneOffset: Differ[ZoneOffset] = (thisOffset: ZoneOffset, thatOffset: ZoneOffset) => {
    if (thisOffset.getTotalSeconds == thatOffset.getTotalSeconds)
      Patch.identical
    else
      Patch.Temporal(
        List[Long]((thatOffset.getTotalSeconds - thisOffset.getTotalSeconds).toLong),
        StandardType.ZoneOffsetType
      )
  }

  val dayOfWeek: Differ[DayOfWeek] =
    (thisDay: DayOfWeek, thatDay: DayOfWeek) =>
      if (thisDay == thatDay)
        Patch.identical
      else
        Patch.Temporal(List[Long]((thatDay.getValue - thisDay.getValue).toLong), StandardType.DayOfWeekType)

  val month: Differ[JMonth] =
    (thisMonth: JMonth, thatMonth: JMonth) =>
      if (thisMonth == thatMonth)
        Patch.identical
      else
        Patch.Temporal(List[Long]((thatMonth.getValue - thisMonth.getValue).toLong), StandardType.MonthType)

  val monthDay: Differ[MonthDay] =
    (thisMonthDay: MonthDay, thatMonthDay: MonthDay) =>
      if (thisMonthDay == thatMonthDay)
        Patch.identical
      else
        Patch.Temporal(
          List[Long](
            ChronoUnit.DAYS.between(thisMonthDay.atYear(2001), thatMonthDay.atYear(2001)),
            ChronoUnit.DAYS.between(thisMonthDay.atYear(2000), thatMonthDay.atYear(2000))
          ),
          StandardType.MonthDayType
        )

  val bigInt: Differ[BigInteger] =
    (thisValue: BigInteger, thatValue: BigInteger) =>
      thisValue.subtract(thatValue) match {
        case BigInteger.ZERO => Patch.identical
        case distance        => Patch.BigInt(distance)
      }

  val bigDecimal: Differ[java.math.BigDecimal] =
    (thisValue: java.math.BigDecimal, thatValue: java.math.BigDecimal) => {
      val thatCtx = new MathContext(thatValue.precision())
      thisValue.round(thatCtx).subtract(thatValue, MathContext.UNLIMITED) match {
        case d if d.compareTo(java.math.BigDecimal.ZERO) == 0 => Patch.identical
        case distance                                         => Patch.BigDecimal(distance, thatValue.precision())
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
      case (Left(l), Left(r))   => Patch.EitherDiff(Left(left(l, r)))
      case (Right(l), Right(r)) => Patch.EitherDiff(Right(right(l, r)))
    }

  def fallback[A, B](left: Differ[A], right: Differ[B]): Differ[Fallback[A, B]] =
    instancePartial[Fallback[A, B]] {
      case (Fallback.Left(l), Fallback.Left(r))           => Patch.Fallback(Fallback.Left(left(l, r)))
      case (Fallback.Right(l), Fallback.Right(r))         => Patch.Fallback(Fallback.Right(right(l, r)))
      case (Fallback.Both(l1, r1), Fallback.Both(l2, r2)) => Patch.Fallback(Fallback.Both(left(l1, l2), right(r1, r2)))
    }

  def identical[A]: Differ[A] = (_: A, _: A) => Patch.identical

  def fail[A]: Differ[A] = (_: A, _: A) => Patch.notComparable

  def record(schema: Schema.Record[ListMap[String, _]]): Differ[ListMap[String, _]] =
    (thisValue: ListMap[String, _], thatValue: ListMap[String, _]) =>
      if (!(conformsToStructure(thisValue, schema.fields) && conformsToStructure(thatValue, schema.fields)))
        Patch.notComparable
      else
        thisValue.toList.zip(thatValue.toList).zipWithIndex.map {
          case (((thisKey, thisValue), (_, thatValue)), fieldIndex) =>
            thisKey -> fromSchema(schema.fields(fieldIndex).schema)
              .asInstanceOf[Differ[Any]]
              .apply(thisValue, thatValue)
        } match {
          case diffs if diffs.exists(!_._2.isComparable) =>
            Patch.notComparable
          case diffs =>
            Patch.Record(ListMap.empty ++ diffs, schema).orIdentical
        }

  private def conformsToStructure[Z](map: ListMap[String, _], structure: Chunk[Schema.Field[Z, _]]): Boolean =
    structure.foldRight(true) {
      case (_, false)                     => false
      case (field: Schema.Field[Z, a], _) => map.get(field.name).exists(_.isInstanceOf[a])
    }

  def enumN[Z](cases: Schema.Case[Z, _]*): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      cases
        .foldRight[Option[Patch[Z]]](None) {
          case (_, diff @ Some(_)) => diff
          case (subtype: Schema.Case[Z, a], _) =>
            subtype.deconstructOption(thisZ) -> (subtype.deconstructOption(thatZ)) match {
              case (Some(thisA), Some(thatA)) =>
                val subtypeDiffer: Differ[Z] =
                  fromSchema(subtype.schema).transform(
                    (z: Z) => subtype.deconstruct(z),
                    (a: a) => a.asInstanceOf[Z]
                  )
                Some(subtypeDiffer(thisA.asInstanceOf[Z], thatA.asInstanceOf[Z]))
              case _ => None
            }
        }
        .getOrElse(Patch.notComparable)

  def enumeration(structure: ListMap[String, Schema[_]]): Differ[(String, _)] =
    instancePartial[(String, _)] {
      case ((thisKey, thisValue), (thatKey, thatValue)) if thisKey == thatKey =>
        structure
          .get(thisKey)
          .map(fromSchema(_).asInstanceOf[Differ[Any]](thisValue, thatValue))
          .getOrElse(Patch.notComparable)
          .asInstanceOf[Patch[(String, Any)]]
    }

  val string: Differ[String] = LCSDiff.string

  def instancePartial[A](f: PartialFunction[(A, A), Patch[A]]): Differ[A] =
    new Differ[A] {
      override def apply(thisValue: A, thatValue: A): Patch[A] =
        f.applyOrElse((thisValue, thatValue), (_: (A, A)) => Patch.notComparable)
    }

}

//scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
private[schema] object ProductDiffer {

  def product0[Z](schema: Schema.CaseClass0[Z]): Differ[Z] = {
    val _ = schema
    (_: Z, _: Z) => Patch.identical
  }

  def product1[A, Z](schema: Schema.CaseClass1[A, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record[Z](ListMap(fieldDiffer(schema.field)(thisZ, thatZ)), schema)
        .orIdentical

  def product2[A1, A2, Z](schema: Schema.CaseClass2[A1, A2, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1), fieldDiffer(schema.field2)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product3[A1, A2, A3, Z](schema: Schema.CaseClass3[A1, A2, A3, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1), fieldDiffer(schema.field2), fieldDiffer(schema.field3)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product4[A1, A2, A3, A4, Z](schema: Schema.CaseClass4[A1, A2, A3, A4, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1), fieldDiffer(schema.field2), fieldDiffer(schema.field3), fieldDiffer(schema.field4)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product5[A1, A2, A3, A4, A5, Z](schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1), fieldDiffer(schema.field2), fieldDiffer(schema.field3), fieldDiffer(schema.field4), fieldDiffer(schema.field5)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product6[A1, A2, A3, A4, A5, A6, Z](schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1), fieldDiffer(schema.field2), fieldDiffer(schema.field3), fieldDiffer(schema.field4), fieldDiffer(schema.field5), fieldDiffer(schema.field6)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product7[A1, A2, A3, A4, A5, A6, A7, Z](schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(
          ListMap.empty ++ Chunk(fieldDiffer(schema.field1), fieldDiffer(schema.field2), fieldDiffer(schema.field3), fieldDiffer(schema.field4), fieldDiffer(schema.field5), fieldDiffer(schema.field6), fieldDiffer(schema.field7))
            .map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  def product8[A1, A2, A3, A4, A5, A6, A7, A8, Z](schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1), fieldDiffer(schema.field2), fieldDiffer(schema.field3), fieldDiffer(schema.field4), fieldDiffer(schema.field5), fieldDiffer(schema.field6), fieldDiffer(schema.field7), fieldDiffer(schema.field8)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1), fieldDiffer(schema.field2), fieldDiffer(schema.field3), fieldDiffer(schema.field4), fieldDiffer(schema.field5), fieldDiffer(schema.field6), fieldDiffer(schema.field7), fieldDiffer(schema.field8), fieldDiffer(schema.field9)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1), fieldDiffer(schema.field2), fieldDiffer(schema.field3), fieldDiffer(schema.field4), fieldDiffer(schema.field5), fieldDiffer(schema.field6), fieldDiffer(schema.field7), fieldDiffer(schema.field8), fieldDiffer(schema.field9), fieldDiffer(schema.field10)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(ListMap.empty ++ Chunk(fieldDiffer(schema.field1), fieldDiffer(schema.field2), fieldDiffer(schema.field3), fieldDiffer(schema.field4), fieldDiffer(schema.field5), fieldDiffer(schema.field6), fieldDiffer(schema.field7), fieldDiffer(schema.field8), fieldDiffer(schema.field9), fieldDiffer(schema.field10), fieldDiffer(schema.field11)).map(_.apply(thisZ, thatZ)), schema)
        .orIdentical

  def product12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(
          ListMap.empty ++ Chunk(fieldDiffer(schema.field1), fieldDiffer(schema.field2), fieldDiffer(schema.field3), fieldDiffer(schema.field4), fieldDiffer(schema.field5), fieldDiffer(schema.field6), fieldDiffer(schema.field7), fieldDiffer(schema.field8), fieldDiffer(schema.field9), fieldDiffer(schema.field10), fieldDiffer(schema.field11), fieldDiffer(schema.field12)).map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  def product13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(
          ListMap.empty ++ Chunk(fieldDiffer(schema.field1), fieldDiffer(schema.field2), fieldDiffer(schema.field3), fieldDiffer(schema.field4), fieldDiffer(schema.field5), fieldDiffer(schema.field6), fieldDiffer(schema.field7), fieldDiffer(schema.field8), fieldDiffer(schema.field9), fieldDiffer(schema.field10), fieldDiffer(schema.field11), fieldDiffer(schema.field12), fieldDiffer(schema.field13))
            .map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  def product14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1),
            fieldDiffer(schema.field2),
            fieldDiffer(schema.field3),
            fieldDiffer(schema.field4),
            fieldDiffer(schema.field5),
            fieldDiffer(schema.field6),
            fieldDiffer(schema.field7),
            fieldDiffer(schema.field8),
            fieldDiffer(schema.field9),
            fieldDiffer(schema.field10),
            fieldDiffer(schema.field11),
            fieldDiffer(schema.field12),
            fieldDiffer(schema.field13),
            fieldDiffer(schema.field14)
          ).map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  def product15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1),
            fieldDiffer(schema.field2),
            fieldDiffer(schema.field3),
            fieldDiffer(schema.field4),
            fieldDiffer(schema.field5),
            fieldDiffer(schema.field6),
            fieldDiffer(schema.field7),
            fieldDiffer(schema.field8),
            fieldDiffer(schema.field9),
            fieldDiffer(schema.field10),
            fieldDiffer(schema.field11),
            fieldDiffer(schema.field12),
            fieldDiffer(schema.field13),
            fieldDiffer(schema.field14),
            fieldDiffer(schema.field15)
          ).map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  def product16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1),
            fieldDiffer(schema.field2),
            fieldDiffer(schema.field3),
            fieldDiffer(schema.field4),
            fieldDiffer(schema.field5),
            fieldDiffer(schema.field6),
            fieldDiffer(schema.field7),
            fieldDiffer(schema.field8),
            fieldDiffer(schema.field9),
            fieldDiffer(schema.field10),
            fieldDiffer(schema.field11),
            fieldDiffer(schema.field12),
            fieldDiffer(schema.field13),
            fieldDiffer(schema.field14),
            fieldDiffer(schema.field15),
            fieldDiffer(schema.field16)
          ).map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  def product17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1),
            fieldDiffer(schema.field2),
            fieldDiffer(schema.field3),
            fieldDiffer(schema.field4),
            fieldDiffer(schema.field5),
            fieldDiffer(schema.field6),
            fieldDiffer(schema.field7),
            fieldDiffer(schema.field8),
            fieldDiffer(schema.field9),
            fieldDiffer(schema.field10),
            fieldDiffer(schema.field11),
            fieldDiffer(schema.field12),
            fieldDiffer(schema.field13),
            fieldDiffer(schema.field14),
            fieldDiffer(schema.field15),
            fieldDiffer(schema.field16),
            fieldDiffer(schema.field17)
          ).map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  def product18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1),
            fieldDiffer(schema.field2),
            fieldDiffer(schema.field3),
            fieldDiffer(schema.field4),
            fieldDiffer(schema.field5),
            fieldDiffer(schema.field6),
            fieldDiffer(schema.field7),
            fieldDiffer(schema.field8),
            fieldDiffer(schema.field9),
            fieldDiffer(schema.field10),
            fieldDiffer(schema.field11),
            fieldDiffer(schema.field12),
            fieldDiffer(schema.field13),
            fieldDiffer(schema.field14),
            fieldDiffer(schema.field15),
            fieldDiffer(schema.field16),
            fieldDiffer(schema.field17),
            fieldDiffer(schema.field18)
          ).map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  def product19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](schema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1),
            fieldDiffer(schema.field2),
            fieldDiffer(schema.field3),
            fieldDiffer(schema.field4),
            fieldDiffer(schema.field5),
            fieldDiffer(schema.field6),
            fieldDiffer(schema.field7),
            fieldDiffer(schema.field8),
            fieldDiffer(schema.field9),
            fieldDiffer(schema.field10),
            fieldDiffer(schema.field11),
            fieldDiffer(schema.field12),
            fieldDiffer(schema.field13),
            fieldDiffer(schema.field14),
            fieldDiffer(schema.field15),
            fieldDiffer(schema.field16),
            fieldDiffer(schema.field17),
            fieldDiffer(schema.field18),
            fieldDiffer(schema.field19)
          ).map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  def product20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](schema: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1),
            fieldDiffer(schema.field2),
            fieldDiffer(schema.field3),
            fieldDiffer(schema.field4),
            fieldDiffer(schema.field5),
            fieldDiffer(schema.field6),
            fieldDiffer(schema.field7),
            fieldDiffer(schema.field8),
            fieldDiffer(schema.field9),
            fieldDiffer(schema.field10),
            fieldDiffer(schema.field11),
            fieldDiffer(schema.field12),
            fieldDiffer(schema.field13),
            fieldDiffer(schema.field14),
            fieldDiffer(schema.field15),
            fieldDiffer(schema.field16),
            fieldDiffer(schema.field17),
            fieldDiffer(schema.field18),
            fieldDiffer(schema.field19),
            fieldDiffer(schema.field20)
          ).map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  def product21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](schema: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1),
            fieldDiffer(schema.field2),
            fieldDiffer(schema.field3),
            fieldDiffer(schema.field4),
            fieldDiffer(schema.field5),
            fieldDiffer(schema.field6),
            fieldDiffer(schema.field7),
            fieldDiffer(schema.field8),
            fieldDiffer(schema.field9),
            fieldDiffer(schema.field10),
            fieldDiffer(schema.field11),
            fieldDiffer(schema.field12),
            fieldDiffer(schema.field13),
            fieldDiffer(schema.field14),
            fieldDiffer(schema.field15),
            fieldDiffer(schema.field16),
            fieldDiffer(schema.field17),
            fieldDiffer(schema.field18),
            fieldDiffer(schema.field19),
            fieldDiffer(schema.field20),
            fieldDiffer(schema.field21)
          ).map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  def product22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](schema: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z]): Differ[Z] =
    (thisZ: Z, thatZ: Z) =>
      Patch
        .Record(
          ListMap.empty ++ Chunk(
            fieldDiffer(schema.field1),
            fieldDiffer(schema.field2),
            fieldDiffer(schema.field3),
            fieldDiffer(schema.field4),
            fieldDiffer(schema.field5),
            fieldDiffer(schema.field6),
            fieldDiffer(schema.field7),
            fieldDiffer(schema.field8),
            fieldDiffer(schema.field9),
            fieldDiffer(schema.field10),
            fieldDiffer(schema.field11),
            fieldDiffer(schema.field12),
            fieldDiffer(schema.field13),
            fieldDiffer(schema.field14),
            fieldDiffer(schema.field15),
            fieldDiffer(schema.field16),
            fieldDiffer(schema.field17),
            fieldDiffer(schema.field18),
            fieldDiffer(schema.field19),
            fieldDiffer(schema.field20),
            fieldDiffer(schema.field21),
            fieldDiffer(schema.field22)
          ).map(_.apply(thisZ, thatZ)),
          schema
        )
        .orIdentical

  private def fieldDiffer[A, Z](field: Schema.Field[Z, A]): (Z, Z) => (String, Patch[A]) =
    (thisZ: Z, thatZ: Z) => field.name -> Differ.fromSchema(field.schema)(field.get(thisZ), field.get(thatZ))
}
