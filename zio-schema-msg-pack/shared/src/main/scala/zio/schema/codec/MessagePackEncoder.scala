package zio.schema.codec

import java.time._
import java.util.UUID
import scala.collection.immutable.ListMap
import org.msgpack.core.MessagePack
import zio.Chunk
import zio.prelude.data.Optional.AllValuesAreNullable
import zio.schema.{DynamicValue, Schema, StandardType}

private[codec] class MessagePackEncoder {
  private val packer = MessagePack.newDefaultBufferPacker()

  def encode[A](schema: Schema[A], value: A): Chunk[Byte] = {
    encodeValue(schema, value)
    val chunk = Chunk.fromArray(packer.toByteArray)
    packer.close()
    chunk
  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private def encodeValue[A](schema: Schema[A], value: A): Unit =
    (schema, value) match {
      case (Schema.GenericRecord(_, structure, _), v: Map[String, _])      => encodeRecord(structure.toChunk, v)
      case (Schema.Sequence(element, _, g, _, _), v)                       => encodeSequence(element, g(v))
      case (mapSchema: Schema.Map[_, _], map: Map[_, _])                   => encodeMap(mapSchema.asInstanceOf[Schema.Map[Any, Any]], map.asInstanceOf[scala.collection.immutable.Map[Any, Any]])
      case (setSchema: Schema.Set[_], set: Set[_])                         => encodeSet(setSchema.asInstanceOf[Schema.Set[Any]].elementSchema, set.asInstanceOf[scala.collection.immutable.Set[Any]])
      case (Schema.Transform(schema, _, g, _, _), _)                       => g(value).map(v => encodeValue(schema, v)): Unit
      case (Schema.Primitive(standardType, _), v)                          => encodePrimitive(standardType, v)
      case (Schema.Tuple2(left, right, _), v @ (_, _))                     => encodeTuple(left, right, v)
      case (optSchema: Schema.Optional[_], v: Option[_])                   => encodeOptional(optSchema.asInstanceOf[Schema.Optional[Any]].schema, v.asInstanceOf[Option[Any]])
      case (eitherSchema: Schema.Either[_, _], v: Either[_, _]) => encodeEither(eitherSchema.asInstanceOf[Schema.Either[Any, Any]].left, eitherSchema.asInstanceOf[Schema.Either[Any, Any]].right, v.asInstanceOf[scala.Either[Any, Any]])
      case (lzy @ Schema.Lazy(_), v)                                       => encodeValue(lzy.schema, v)
      //  case (Schema.Meta(ast, _), _)                                        => encodeValue(fieldNumber, Schema[MetaSchema], ast)
      case (Schema.CaseClass0(_, _, _), _)         => encodePrimitive(StandardType.UnitType, ())
      case (Schema.CaseClass1(_, f, _, _), v)      => encodeCaseClass(v, f)
      case (Schema.CaseClass2(_, f1, f2, _, _), v) => encodeCaseClass(v, f1, f2)
      case (Schema.CaseClass3(_, f1, f2, f3, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3)
      case (Schema.CaseClass4(_, f1, f2, f3, f4, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4)
      case (Schema.CaseClass5(_, f1, f2, f3, f4, f5, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5)
      case (Schema.CaseClass6(_, f1, f2, f3, f4, f5, f6, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6)
      case (Schema.CaseClass7(_, f1, f2, f3, f4, f5, f6, f7, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7)
      case (Schema.CaseClass8(_, f1, f2, f3, f4, f5, f6, f7, f8, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8)
      case (Schema.CaseClass9(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9)
      case (Schema.CaseClass10(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
      case (Schema.CaseClass11(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
      case (Schema.CaseClass12(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
      case (Schema.CaseClass13(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
      case (Schema.CaseClass14(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)
      case (Schema.CaseClass15(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)
      case (Schema.CaseClass16(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
      case (Schema.CaseClass17(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)
      case (Schema.CaseClass18(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)
      case (Schema.CaseClass19(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, _, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19)
      case (Schema.CaseClass20(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, _), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20)
      case (Schema.CaseClass21(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, tail), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, tail._1)
      case (Schema.CaseClass22(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, tail), v) =>
        encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, tail._1, tail._2)
      case (Schema.Enum1(_, c, _), v)                          => encodeEnum(v, c)
      case (Schema.Enum2(_, c1, c2, _), v)                     => encodeEnum(v, c1, c2)
      case (Schema.Enum3(_, c1, c2, c3, _), v)                 => encodeEnum(v, c1, c2, c3)
      case (Schema.Enum4(_, c1, c2, c3, c4, _), v)             => encodeEnum(v, c1, c2, c3, c4)
      case (Schema.Enum5(_, c1, c2, c3, c4, c5, _), v)         => encodeEnum(v, c1, c2, c3, c4, c5)
      case (Schema.Enum6(_, c1, c2, c3, c4, c5, c6, _), v)     => encodeEnum(v, c1, c2, c3, c4, c5, c6)
      case (Schema.Enum7(_, c1, c2, c3, c4, c5, c6, c7, _), v) => encodeEnum(v, c1, c2, c3, c4, c5, c6, c7)
      case (Schema.Enum8(_, c1, c2, c3, c4, c5, c6, c7, c8, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8)
      case (Schema.Enum9(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9)
      case (Schema.Enum10(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
      case (Schema.Enum11(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
      case (Schema.Enum12(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
      case (Schema.Enum13(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
      case (Schema.Enum14(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
      case (Schema.Enum15(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
      case (Schema.Enum16(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
      case (Schema.Enum17(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
      case (Schema.Enum18(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
      case (Schema.Enum19(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
      case (Schema.Enum20(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
      case (Schema.Enum21(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
      case (Schema.Enum22(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _), v) =>
        encodeEnum(v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
      case (Schema.EnumN(_, cs, _), v) => encodeEnum(v, cs.toSeq: _*)
      case (Schema.Dynamic(_), v)      => encodeValue(DynamicValue.schema, v)
      case (_, _)                      => ()
    }

  private def encodeEnum[Z](value: Z, cases: Schema.Case[Z, _]*): Unit = {
    val fieldIndex = cases.indexWhere(c => c.deconstructOption(value).isDefined)
    if (fieldIndex >= 0) {
      packer.packInt(fieldIndex)
      val subtypeCase = cases(fieldIndex)
      encodeValue(subtypeCase.schema.asInstanceOf[Schema[Any]], subtypeCase.deconstruct(value))
    }
  }

  private def encodeEither[A, B](left: Schema[A], right: Schema[B], either: Either[A, B]): Unit = {
    packer.packMapHeader(1)
    either match {
      case Left(value) =>
        packer.packString("left")
        encodeValue(left, value)
      case Right(value) =>
        packer.packString("right")
        encodeValue(right, value)
    }
  }

  private def encodeCaseClass[Z](value: Z, fields: (Schema.Field[Z, _])*): Unit =
    writeStructure(fields.map { field =>
      (field, field.get(value))
    })

  private def encodeTuple[A, B](left: Schema[A], right: Schema[B], tuple: (A, B)): Unit = {
    packer.packArrayHeader(2)
    encodeValue(left, tuple._1)
    encodeValue(right, tuple._2)
  }

  private def encodePrimitive[A](standardType: StandardType[A], value: A): Unit =
    (standardType, value) match {
      case (StandardType.UnitType, _) =>
        packer.packNil()
        ()
      case (StandardType.StringType, str: String) =>
        packer.packString(str)
        ()
      case (StandardType.BoolType, b: Boolean) =>
        packer.packBoolean(b)
        ()
      case (StandardType.ByteType, v: Byte) =>
        packer.packByte(v)
        ()
      case (StandardType.ShortType, v: Short) =>
        packer.packShort(v)
        ()
      case (StandardType.IntType, v: Int) =>
        packer.packInt(v)
        ()
      case (StandardType.LongType, v: Long) =>
        packer.packLong(v)
        ()
      case (StandardType.FloatType, v: Float) =>
        packer.packFloat(v)
        ()
      case (StandardType.DoubleType, v: Double) =>
        packer.packDouble(v)
        ()
      case (StandardType.BigIntegerType, v: java.math.BigInteger) =>
        packer.packBigInteger(v)
        ()
      case (StandardType.BigDecimalType, v: java.math.BigDecimal) =>
        val unscaled  = v.unscaledValue()
        val precision = v.precision()
        val scale     = v.scale()
        encodeRecord(MessagePackCodec.bigDecimalStructure, ListMap("unscaled" -> unscaled, "precision" -> precision, "scale" -> scale))
      case (StandardType.BinaryType, bytes: Chunk[Byte]) =>
        packer.packBinaryHeader(bytes.size)
        packer.writePayload(bytes.toArray)
        ()
      case (StandardType.CharType, c: Char) =>
        packer.packString(c.toString)
        ()
      case (StandardType.UUIDType, u: UUID) =>
        packer.packString(u.toString)
        ()
      case (StandardType.DayOfWeekType, v: DayOfWeek) =>
        packer.packByte(v.getValue.toByte)
        ()
      case (StandardType.MonthType, v: Month) =>
        packer.packByte(v.getValue.toByte)
        ()
      case (StandardType.MonthDayType, v: MonthDay) =>
        encodeRecord(MessagePackCodec.monthDayStructure, ListMap("month" -> v.getMonthValue, "day" -> v.getDayOfMonth))
      case (StandardType.PeriodType, v: Period) =>
        encodeRecord(MessagePackCodec.periodStructure, ListMap("years" -> v.getYears, "months" -> v.getMonths, "days" -> v.getDays))
      case (StandardType.YearType, v: Year) =>
        packer.packInt(v.getValue)
        ()
      case (StandardType.YearMonthType, v: YearMonth) =>
        encodeRecord(MessagePackCodec.yearMonthStructure, ListMap("year" -> v.getYear, "month" -> v.getMonthValue))
      case (StandardType.ZoneIdType, v: ZoneId) =>
        packer.packString(v.getId)
        ()
      case (StandardType.ZoneOffsetType, v: ZoneOffset) =>
        packer.packInt(v.getTotalSeconds)
        ()
      case (StandardType.DurationType, v: Duration) =>
        encodeRecord(MessagePackCodec.durationStructure, ListMap("seconds" -> v.getSeconds, "nanos" -> v.getNano))
      case (StandardType.InstantType, v: Instant) =>
        packer.packString(v.toString)
        ()
      case (StandardType.LocalDateType, v: LocalDate) =>
        packer.packString(v.toString)
        ()
      case (StandardType.LocalTimeType, v: LocalTime) =>
        packer.packString(v.toString)
        ()
      case (StandardType.LocalDateTimeType, v: LocalDateTime) =>
        packer.packString(v.toString)
        ()
      case (StandardType.OffsetTimeType, v: OffsetTime) =>
        packer.packString(v.toString)
        ()
      case (StandardType.OffsetDateTimeType, v: OffsetDateTime) =>
        packer.packString(v.toString)
        ()
      case (StandardType.ZonedDateTimeType, v: ZonedDateTime) =>
        packer.packString(v.toString)
        ()
      case (_, _) =>
        throw new NotImplementedError(s"No encoder for $standardType")
    }

  def encodeSequence[A](schema: Schema[A], v: Chunk[A]): Unit = {
    packer.packArrayHeader(v.size)
    v.foreach(encodeValue(schema, _))
  }

  def encodeMap[K, V](schema: Schema.Map[K, V], v: Map[K, V]): Unit = {
    packer.packMapHeader(v.size)
    v.foreach {
      case (k, v) =>
        encodeValue(schema.keySchema, k)
        encodeValue(schema.valueSchema, v)
    }
  }

  def encodeSet[A](schema: Schema[A], v: scala.collection.immutable.Set[A]): Unit = {
    packer.packArrayHeader(v.size)
    v.foreach(encodeValue(schema, _))
  }

  def encodeOptional[A](schema: Schema[A], v: Option[A]): Unit = {
    packer.packArrayHeader(v.toList.size)
    v.foreach(encodeValue(schema, _))
  }

  def encodeRecord(structure: Seq[Schema.Field[_, _]], data: ListMap[String, _]): Unit =
    writeStructure(structure.map(schema => (schema, data(schema.name))))

  private def writeStructure(fields: Seq[(Schema.Field[_, _], Any)]): Unit = {
    packer.packMapHeader(fields.size)
    fields.foreach {
      case (fieldSchema: Schema.Field[_, Any], value) =>
        packer.packString(fieldSchema.name)
        encodeValue(fieldSchema.schema, value)
    }
  }

}
