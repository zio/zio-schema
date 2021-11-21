package zio.schema

import java.time.temporal.ChronoUnit
import java.time.{
  DayOfWeek,
  Instant,
  LocalDate,
  Month,
  MonthDay,
  OffsetDateTime,
  OffsetTime,
  Period,
  Year,
  YearMonth,
  ZoneId,
  ZoneOffset,
  ZonedDateTime
}
import java.util.UUID

import zio.Chunk
import zio.random.Random
import zio.test.{ Gen, Sized }

object types {

  //scalafmt: { maxColumn = 400 }
  sealed trait Arities

  object Arities extends DefaultJavaTimeSchemas {
    implicit val durationSchema: Schema[java.time.Duration] = Schema.primitive(StandardType.Duration(ChronoUnit.SECONDS))

    case object Arity0         extends Arities
    case class Arity1(f1: Int) extends Arities

    object Arity1 {
      implicit lazy val schema: Schema[Arity1] = DeriveSchema.gen
    }
    case class Arity2(f1: Arity1, f2: String) extends Arities

    object Arity2 {
      implicit lazy val schema: Schema[Arity2] = DeriveSchema.gen
    }
    case class Arity3(f1: Arity1, f2: Arity2, f3: Long) extends Arities

    object Arity3 {
      implicit lazy val schema: Schema[Arity3] = DeriveSchema.gen
    }
    case class Arity4(f1: Arity1, f2: Arity2, f3: Arity3, f4: Short) extends Arities

    object Arity4 {
      implicit lazy val schema: Schema[Arity4] = DeriveSchema.gen
    }
    case class Arity5(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Double) extends Arities

    object Arity5 {
      implicit lazy val schema: Schema[Arity5] = DeriveSchema.gen
    }
    case class Arity6(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Float) extends Arities

    object Arity6 {
      implicit lazy val schema: Schema[Arity6] = DeriveSchema.gen
    }
    case class Arity7(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Chunk[Byte]) extends Arities

    object Arity7 {
      implicit lazy val schema: Schema[Arity7] = DeriveSchema.gen
    }
    case class Arity8(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Char) extends Arities

    object Arity8 {
      implicit lazy val schema: Schema[Arity8] = DeriveSchema.gen
    }
    case class Arity9(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: UUID) extends Arities

    object Arity9 {
      implicit lazy val schema: Schema[Arity9] = DeriveSchema.gen
    }
    case class Arity10(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: java.math.BigDecimal) extends Arities

    object Arity10 {
      implicit lazy val schema: Schema[Arity10] = DeriveSchema.gen
    }
    case class Arity11(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: java.math.BigInteger) extends Arities

    object Arity11 {
      implicit lazy val schema: Schema[Arity11] = DeriveSchema.gen
    }
    case class Arity12(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: DayOfWeek) extends Arities

    object Arity12 {
      implicit lazy val schema: Schema[Arity12] = DeriveSchema.gen
    }
    case class Arity13(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: Arity12, f13: Month) extends Arities

    object Arity13 {
      implicit lazy val schema: Schema[Arity13] = DeriveSchema.gen
    }
    case class Arity14(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: Arity12, f13: Arity13, f14: MonthDay) extends Arities

    object Arity14 {
      implicit lazy val schema: Schema[Arity14] = DeriveSchema.gen
    }
    case class Arity15(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: Arity12, f13: Arity13, f14: Arity14, f15: Period) extends Arities

    object Arity15 {
      implicit lazy val schema: Schema[Arity15] = DeriveSchema.gen
    }
    case class Arity16(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: Arity12, f13: Arity13, f14: Arity14, f15: Arity15, f16: Year) extends Arities

    object Arity16 {
      implicit lazy val schema: Schema[Arity16] = DeriveSchema.gen
    }
    case class Arity17(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: Arity12, f13: Arity13, f14: Arity14, f15: Arity15, f16: Arity16, f17: YearMonth) extends Arities

    object Arity17 {
      implicit lazy val schema: Schema[Arity17] = DeriveSchema.gen
    }
    case class Arity18(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: Arity12, f13: Arity13, f14: Arity14, f15: Arity15, f16: Arity16, f17: Arity17, f18: ZoneId) extends Arities

    object Arity18 {
      implicit lazy val schema: Schema[Arity18] = DeriveSchema.gen
    }
    case class Arity19(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: Arity12, f13: Arity13, f14: Arity14, f15: Arity15, f16: Arity16, f17: Arity17, f18: Arity18, f19: ZoneOffset) extends Arities

    object Arity19 {
      implicit lazy val schema: Schema[Arity19] = DeriveSchema.gen
    }
    case class Arity20(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: Arity12, f13: Arity13, f14: Arity14, f15: Arity15, f16: Arity16, f17: Arity17, f18: Arity18, f19: Arity19, f20: Instant) extends Arities

    object Arity20 {
      implicit lazy val schema: Schema[Arity20] = DeriveSchema.gen
    }
    case class Arity21(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: Arity12, f13: Arity13, f14: Arity14, f15: Arity15, f16: Arity16, f17: Arity17, f18: Arity18, f19: Arity19, f20: Arity20, f21: LocalDate) extends Arities

    object Arity21 {
      implicit lazy val schema: Schema[Arity21] = DeriveSchema.gen
    }
    case class Arity22(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: Arity12, f13: Arity13, f14: Arity14, f15: Arity15, f16: Arity16, f17: Arity17, f18: Arity18, f19: Arity19, f20: Arity20, f21: Arity21, f22: ZonedDateTime) extends Arities

    object Arity22 {
      implicit lazy val schema: Schema[Arity22] = DeriveSchema.gen
    }
    case class Arity23(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: Arity12, f13: Arity13, f14: Arity14, f15: Arity15, f16: Arity16, f17: Arity17, f18: Arity18, f19: Arity19, f20: Arity20, f21: Arity21, f22: Arity22, f23: OffsetTime) extends Arities

    object Arity23 {
      implicit lazy val schema: Schema[Arity23] = DeriveSchema.gen
    }
    case class Arity24(f1: Arity1, f2: Arity2, f3: Arity3, f4: Arity4, f5: Arity5, f6: Arity6, f7: Arity7, f8: Arity8, f9: Arity9, f10: Arity10, f11: Arity11, f12: Arity12, f13: Arity13, f14: Arity14, f15: Arity15, f16: Arity16, f17: Arity17, f18: Arity18, f19: Arity19, f20: Arity20, f21: Arity21, f22: Arity22, f23: Arity23, f24: OffsetDateTime) extends Arities

    object Arity24 {
      implicit lazy val schema: Schema[Arity24] = DeriveSchema.gen
    }

    implicit lazy val schema: Schema[Arities] = DeriveSchema.gen

    implicit val genBytes: Gen[Random with Sized, Chunk[Byte]] = Gen.chunkOf(Gen.anyByte)
  }
  //scalafmt: { maxColumn = 120 }

  sealed trait Recursive

  object Recursive {
    case class RecursiveOption(level: Int, r: Option[Recursive]) extends Recursive

    object RecursiveOption {
      implicit lazy val schema: Schema[RecursiveOption] = DeriveSchema.gen
    }
    case class RecursiveList(level: String, r: List[Recursive]) extends Recursive

    object RecursiveList {
      implicit lazy val schema: Schema[RecursiveList] = DeriveSchema.gen
    }
    case class RecursiveEither(level: Long, r: Either[String, Recursive]) extends Recursive

    object RecursiveEither {
      implicit lazy val schema: Schema[RecursiveEither] = DeriveSchema.gen
    }

    implicit lazy val schema: Schema[Recursive] = DeriveSchema.gen
  }

  case class EitherVariants(
    f1: Either[Option[String], Int],
    f2: Either[List[String], String],
    f3: Either[Option[Int], List[String]],
    f4: Either[List[Option[Int]], Option[List[String]]],
    f5: Either[Either[Int, String], Option[Int]],
    f6: Either[Arities, Arities],
    f7: Either[Recursive, Recursive]
  )

  object EitherVariants {
    implicit lazy val schema: Schema[EitherVariants] = DeriveSchema.gen
  }

  case class OptionVariants(
    f1: Option[Int],
    f2: Option[List[String]],
    f3: Option[Either[String, Int]],
    f4: Option[Recursive],
    f5: Option[Arities]
  )

  object OptionVariants {
    implicit lazy val schema: Schema[OptionVariants] = DeriveSchema.gen
  }

  case class SequenceVariants(
    f1: List[String],
    f2: Chunk[String],
    f3: List[Option[String]],
    f4: Chunk[Option[String]],
    f5: List[Option[String]],
    f6: Chunk[Option[String]],
    f7: List[List[String]],
    f8: Chunk[Chunk[String]],
    f9: List[Either[String, Int]],
    f10: Chunk[Either[Option[String], Int]],
    f11: List[Either[Option[String], List[Int]]],
    f12: Chunk[Either[Option[String], List[Int]]],
    f13: List[Option[Either[String, Chunk[Int]]]],
    f14: Chunk[Option[Either[String, List[Int]]]],
    f15: List[Arities],
    f16: Chunk[Arities],
    f17: List[Recursive],
    f18: Chunk[Recursive]
  )

  object SequenceVariants {
    implicit lazy val schema: Schema[SequenceVariants] = DeriveSchema.gen
  }

  type SchemaAndValue[A]     = (Schema[A], A)
  type SchemaAndValues[A]    = (Schema[A], List[A])
  type SchemaAndValuePair[A] = (Schema[A], (A, A))

  val anySchema: Gen[Random with Sized, Schema[_]] =
    Gen.oneOf(
      Gen.const(Schema[SequenceVariants]),
      Gen.const(Schema[OptionVariants]),
      Gen.const(Schema[EitherVariants]),
      Gen.const(Schema[Recursive]),
      Gen.const(Schema[Arities])
    )

  def anySchemaAndValue: Gen[Random with Sized, SchemaAndValue[_]] =
    for {
      schema       <- anySchema
      dynamicValue <- DynamicValueGen.anyDynamicValueOfSchema(schema)
    } yield schema.asInstanceOf[Schema[Any]] -> dynamicValue.toTypedValue(schema).toOption.get

  def anySchemaAndValuePair: Gen[Random with Sized, SchemaAndValuePair[_]] =
    for {
      schema        <- anySchema
      dynamicValue1 <- DynamicValueGen.anyDynamicValueOfSchema(schema)
      dynamicValue2 <- DynamicValueGen.anyDynamicValueOfSchema(schema)
    } yield schema.asInstanceOf[Schema[Any]] -> (dynamicValue1.toTypedValue(schema).toOption.get -> dynamicValue2
      .toTypedValue(schema)
      .toOption
      .get)

  def anySchemaAndValues(n: Int): Gen[Random with Sized, SchemaAndValues[_]] =
    for {
      schema        <- anySchema
      dynamicValues <- Gen.listOfN(n)(DynamicValueGen.anyDynamicValueOfSchema(schema))
    } yield schema.asInstanceOf[Schema[Any]] -> dynamicValues.map(_.toTypedValue(schema).toOption.get)

}
