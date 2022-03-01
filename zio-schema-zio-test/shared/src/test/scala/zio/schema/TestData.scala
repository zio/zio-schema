package zio.schema

import java.time.format.DateTimeFormatter

import zio.Chunk

object TestData {

  val unitSchema: Schema[Unit]                       = Schema.Primitive(StandardType.UnitType)
  val stringSchema: Schema[String]                   = Schema.Primitive(StandardType.StringType)
  val booleanSchema: Schema[Boolean]                 = Schema.Primitive(StandardType.BoolType)
  val shortSchema: Schema[Short]                     = Schema.Primitive(StandardType.ShortType)
  val intSchema: Schema[Int]                         = Schema.Primitive(StandardType.IntType)
  val longSchema: Schema[Long]                       = Schema.Primitive(StandardType.LongType)
  val floatSchema: Schema[Float]                     = Schema.Primitive(StandardType.FloatType)
  val doubleSchema: Schema[Double]                   = Schema.Primitive(StandardType.DoubleType)
  val binarySchema: Schema[Chunk[Byte]]              = Schema.Primitive(StandardType.BinaryType)
  val charSchema: Schema[Char]                       = Schema.Primitive(StandardType.CharType)
  val bigDecimalSchema: Schema[java.math.BigDecimal] = Schema.Primitive(StandardType.BigDecimalType)
  val bigIntegerSchema: Schema[java.math.BigInteger] = Schema.Primitive(StandardType.BigIntegerType)
  val monthSchema: Schema[java.time.Month]           = Schema.Primitive(StandardType.MonthType)
  val monthDaySchema: Schema[java.time.MonthDay]     = Schema.Primitive(StandardType.MonthDayType)
  val periodSchema: Schema[java.time.Period]         = Schema.Primitive(StandardType.PeriodType)
  val dayOfWeekSchema: Schema[java.time.DayOfWeek]   = Schema.Primitive(StandardType.DayOfWeekType)
  val yearSchema: Schema[java.time.Year]             = Schema.Primitive(StandardType.YearType)
  val yearMonthSchema: Schema[java.time.YearMonth]   = Schema.Primitive(StandardType.YearMonthType)
  val zoneIdSchema: Schema[java.time.ZoneId]         = Schema.Primitive(StandardType.ZoneIdType)
  val zoneOffsetSchema: Schema[java.time.ZoneOffset] = Schema.Primitive(StandardType.ZoneOffsetType)

  val instantSchema: Schema[java.time.Instant] =
    Schema.Primitive(StandardType.InstantType(DateTimeFormatter.ISO_INSTANT))

  val localDateSchema: Schema[java.time.LocalDate] =
    Schema.Primitive(StandardType.LocalDateType(DateTimeFormatter.ISO_LOCAL_DATE))

  val localTimeSchema: Schema[java.time.LocalTime] =
    Schema.Primitive(StandardType.LocalTimeType(DateTimeFormatter.ISO_LOCAL_TIME))

  val localDateTimeSchema: Schema[java.time.LocalDateTime] =
    Schema.Primitive(StandardType.LocalDateTimeType(DateTimeFormatter.ISO_LOCAL_DATE_TIME))

  val offsetTimeSchema: Schema[java.time.OffsetTime] =
    Schema.Primitive(StandardType.OffsetTimeType(DateTimeFormatter.ISO_OFFSET_TIME))

  val offsetDateTimeSchema: Schema[java.time.OffsetDateTime] =
    Schema.Primitive(StandardType.OffsetDateTimeType(DateTimeFormatter.ISO_OFFSET_DATE_TIME))

  val zonedDateTimeSchema: Schema[java.time.ZonedDateTime] =
    Schema.Primitive(StandardType.ZonedDateTimeType(DateTimeFormatter.ISO_ZONED_DATE_TIME))

  val uuidSchema: Schema[java.util.UUID] = Schema.Primitive(StandardType.UUIDType)

  val eitherSchema: Schema[Either[String, Unit]] = Schema.EitherSchema(stringSchema, unitSchema)
  val tupleSchema: Schema[(String, Unit)]        = Schema.Tuple(stringSchema, unitSchema)
  val listSchema: Schema[List[Int]]              = Schema.list(intSchema)
  val mapSchema: Schema[Map[Int, String]]        = Schema.map(intSchema, stringSchema)
  val optionalSchema: Schema[Option[Int]]        = Schema.Optional(intSchema)

  val transformSchema: Schema[String] =
    intSchema.transformOrFail[String]((int: Int) => Right(int.toString), (_: String) => Left("error"))
  val failSchema: Schema[Unit] = Schema.Fail[Unit]("failed")
  val lazySchema: Schema[Int]  = Schema.Lazy(() => intSchema)

  sealed trait Enum2

  object Enum2 {
    case object c1 extends Enum2
    case object c2 extends Enum2

    val schema: Schema.Enum2[c1.type, c2.type, Enum2] = DeriveSchema.gen[Enum2]
  }

  sealed trait Enum23

  object Enum23 {
    case object C1  extends Enum23
    case object C2  extends Enum23
    case object C3  extends Enum23
    case object C4  extends Enum23
    case object C5  extends Enum23
    case object C6  extends Enum23
    case object C7  extends Enum23
    case object C8  extends Enum23
    case object C9  extends Enum23
    case object C10 extends Enum23
    case object C11 extends Enum23
    case object C12 extends Enum23
    case object C13 extends Enum23
    case object C14 extends Enum23
    case object C15 extends Enum23
    case object C16 extends Enum23
    case object C17 extends Enum23
    case object C18 extends Enum23
    case object C19 extends Enum23
    case object C20 extends Enum23
    case object C21 extends Enum23
    case object C22 extends Enum23
    case object C23 extends Enum23

    implicit lazy val schema: Schema.EnumN[Enum23, CaseSet.Aux[Enum23]] = DeriveSchema.gen[Enum23]

  }

  final case class CaseClass1(a1: String)

  object CaseClass1 {
    implicit lazy val schema: Schema[CaseClass1] = DeriveSchema.gen[CaseClass1]
  }

  final case class CaseClass2(a1: String, a2: Long)

  object CaseClass2 {
    implicit lazy val schema: Schema[CaseClass2] = DeriveSchema.gen[CaseClass2]
  }

  final case class CaseClass3(a1: String, a2: Long, a3: Boolean)

  object CaseClass3 {
    implicit lazy val schema: Schema[CaseClass3] = DeriveSchema.gen[CaseClass3]
  }

  final case class CaseClass4(a1: String, a2: Long, a3: Boolean, a4: Int = 4)

  object CaseClass4 {
    implicit lazy val schema: Schema[CaseClass4] = DeriveSchema.gen[CaseClass4]
  }

  final case class CaseClass5(a1: String, a2: Long, a3: Boolean, a4: Int = 4, a5: Int = 5)

  object CaseClass5 {
    implicit lazy val schema: Schema[CaseClass5] = DeriveSchema.gen[CaseClass5]
  }

  final case class CaseClass6(a1: String, a2: Long, a3: Boolean, a4: Int = 4, a5: Int = 5, a6: Int = 6)

  object CaseClass6 {
    implicit lazy val schema: Schema[CaseClass6] = DeriveSchema.gen[CaseClass6]
  }

  final case class CaseClass7(a1: String, a2: Long, a3: Boolean, a4: Int = 4, a5: Int = 5, a6: Int = 6, a7: Int = 7)

  object CaseClass7 {
    implicit lazy val schema: Schema[CaseClass7] = DeriveSchema.gen[CaseClass7]
  }

  final case class CaseClass8(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8
  )

  object CaseClass8 {
    implicit lazy val schema: Schema[CaseClass8] = DeriveSchema.gen[CaseClass8]
  }

  final case class CaseClass9(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9
  )

  object CaseClass9 {
    implicit lazy val schema: Schema[CaseClass9] = DeriveSchema.gen[CaseClass9]
  }

  final case class CaseClass10(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9,
    a10: Int = 10
  )

  object CaseClass10 {
    implicit lazy val schema: Schema[CaseClass10] = DeriveSchema.gen[CaseClass10]
  }

  final case class CaseClass11(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9,
    a10: Int = 10,
    a11: Int = 11
  )

  object CaseClass11 {
    implicit lazy val schema: Schema[CaseClass11] = DeriveSchema.gen[CaseClass11]
  }

  final case class CaseClass12(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9,
    a10: Int = 10,
    a11: Int = 11,
    a12: Int = 12
  )

  object CaseClass12 {
    implicit lazy val schema: Schema[CaseClass12] = DeriveSchema.gen[CaseClass12]
  }

  final case class CaseClass13(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9,
    a10: Int = 10,
    a11: Int = 11,
    a12: Int = 12,
    a13: Int = 13
  )

  object CaseClass13 {
    implicit lazy val schema: Schema[CaseClass13] = DeriveSchema.gen[CaseClass13]
  }

  final case class CaseClass14(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9,
    a10: Int = 10,
    a11: Int = 11,
    a12: Int = 12,
    a13: Int = 13,
    a14: Int = 14
  )

  object CaseClass14 {
    implicit lazy val schema: Schema[CaseClass14] = DeriveSchema.gen[CaseClass14]
  }

  final case class CaseClass15(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9,
    a10: Int = 10,
    a11: Int = 11,
    a12: Int = 12,
    a13: Int = 13,
    a14: Int = 14,
    a15: Int = 15
  )

  object CaseClass15 {
    implicit lazy val schema: Schema[CaseClass15] = DeriveSchema.gen[CaseClass15]
  }

  final case class CaseClass16(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9,
    a10: Int = 10,
    a11: Int = 11,
    a12: Int = 12,
    a13: Int = 13,
    a14: Int = 14,
    a15: Int = 15,
    a16: Int = 16
  )

  object CaseClass16 {
    implicit lazy val schema: Schema[CaseClass16] = DeriveSchema.gen[CaseClass16]
  }

  final case class CaseClass17(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9,
    a10: Int = 10,
    a11: Int = 11,
    a12: Int = 12,
    a13: Int = 13,
    a14: Int = 14,
    a15: Int = 15,
    a16: Int = 16,
    a17: Int = 17
  )

  object CaseClass17 {
    implicit lazy val schema: Schema[CaseClass17] = DeriveSchema.gen[CaseClass17]
  }

  final case class CaseClass18(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9,
    a10: Int = 10,
    a11: Int = 11,
    a12: Int = 12,
    a13: Int = 13,
    a14: Int = 14,
    a15: Int = 15,
    a16: Int = 16,
    a17: Int = 17,
    a18: Int = 18
  )

  object CaseClass18 {
    implicit lazy val schema: Schema[CaseClass18] = DeriveSchema.gen[CaseClass18]
  }

  final case class CaseClass19(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9,
    a10: Int = 10,
    a11: Int = 11,
    a12: Int = 12,
    a13: Int = 13,
    a14: Int = 14,
    a15: Int = 15,
    a16: Int = 16,
    a17: Int = 17,
    a18: Int = 18,
    a19: Int = 19
  )

  object CaseClass19 {
    implicit lazy val schema: Schema[CaseClass19] = DeriveSchema.gen[CaseClass19]
  }

  final case class CaseClass20(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9,
    a10: Int = 10,
    a11: Int = 11,
    a12: Int = 12,
    a13: Int = 13,
    a14: Int = 14,
    a15: Int = 15,
    a16: Int = 16,
    a17: Int = 17,
    a18: Int = 18,
    a19: Int = 19,
    a20: Int = 20
  )

  object CaseClass20 {
    implicit lazy val schema: Schema[CaseClass20] = DeriveSchema.gen[CaseClass20]
  }

  final case class CaseClass21(
    a1: String,
    a2: Long,
    a3: Boolean,
    a4: Int = 4,
    a5: Int = 5,
    a6: Int = 6,
    a7: Int = 7,
    a8: Int = 8,
    a9: Int = 9,
    a10: Int = 10,
    a11: Int = 11,
    a12: Int = 12,
    a13: Int = 13,
    a14: Int = 14,
    a15: Int = 15,
    a16: Int = 16,
    a17: Int = 17,
    a18: Int = 18,
    a19: Int = 19,
    a20: Int = 20,
    a21: Int = 21
  )

  object CaseClass21 {
    implicit lazy val schema: Schema[CaseClass21] = DeriveSchema.gen[CaseClass21]
  }

  final case class CaseClass22(
    f1: CaseClass1,
    f2: CaseClass2,
    f3: CaseClass3,
    f4: CaseClass4,
    f5: CaseClass5,
    f6: CaseClass6,
    f7: CaseClass7,
    f8: CaseClass8,
    f9: CaseClass9,
    f10: CaseClass10,
    f11: CaseClass11,
    f12: CaseClass12,
    f13: CaseClass13,
    f14: CaseClass14,
    f15: CaseClass15,
    f16: CaseClass16,
    f17: CaseClass17,
    f18: CaseClass18,
    f19: CaseClass19,
    f20: CaseClass20,
    f21: CaseClass21,
    f22: Int = 22
  )

  object CaseClass22 {
    val schema: Schema[CaseClass22] = DeriveSchema.gen[CaseClass22]
  }
}
