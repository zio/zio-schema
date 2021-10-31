package zio.schema.codec

import zio.schema._
import zio.stream.ZTransducer
import zio.{ Chunk, ZIO }
import zio.schema.Schema.Meta
import zio.schema.ast.SchemaAst

import java.nio.charset.StandardCharsets
import scala.collection.immutable.ListMap
import java.lang
import java.nio.{ ByteBuffer, ByteOrder }
import scala.annotation.tailrec

object AvroCodec extends Codec {

  override def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte] =
    ZTransducer.fromPush(
      (opt: Option[Chunk[A]]) =>
        ZIO.succeed(opt.map(values => values.flatMap(Encoder.encode(schema, _))).getOrElse(Chunk.empty))
    )

  override def encode[A](schema: Schema[A]): A => Chunk[Byte] = a => Encoder.encode(schema, a)

  override def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A] =
    ZTransducer.fromPush(
      (opt: Option[Chunk[Byte]]) =>
        ZIO.fromEither(opt.map(chunk => Decoder.decode(schema, chunk).map(Chunk(_))).getOrElse(Right(Chunk.empty)))
    )

  override def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A] =
    ch =>
      if (ch.isEmpty)
        Left("No bytes to decode")
      else
        Decoder.decode(schema, ch)

  object Encoder {
    private val boolFalseChunk: Chunk[Byte] = Chunk[Byte](0)
    private val boolTrueChunk: Chunk[Byte]  = Chunk[Byte](1)

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    def encode[A](schema: Schema[A], value: A): Chunk[Byte] = schema match {
      case Schema.Sequence(element, _, g) => encodeSequence(element, g(value))
      case Schema.Transform(codec, _, g)  => g(value).map(encode(codec, _)).getOrElse(Chunk.empty)
      case Schema.Primitive(standardType) => encodePrimitive(standardType, value)
      case Schema.Optional(codec) =>
        value match {
          case v: Option[_] => encodeOptional(codec, v)
          case _            => Chunk.empty
        }
      case Schema.Tuple(left, right) =>
        value match {
          case v @ (_, _) => encodeTuple(left, right, v)
          case _          => Chunk.empty
        }
      case Schema.EitherSchema(left, right) =>
        value match {
          case v: Either[_, _] => encodeEither(left, right, v)
          case _               => Chunk.empty
        }
      case lzy @ Schema.Lazy(_) => encode(lzy.schema, value)
      case Meta(ast)            => encode(Schema[SchemaAst], ast)
      case Schema.Enum1(c1) =>
        encodeCase(value, c1)
      case Schema.Enum2(c1, c2) =>
        encodeCase(value, c1, c2)
      case Schema.Enum3(c1, c2, c3) =>
        encodeCase(value, c1, c2, c3)
      case Schema.Enum4(c1, c2, c3, c4) =>
        encodeCase(value, c1, c2, c3, c4)
      case Schema.Enum5(c1, c2, c3, c4, c5) =>
        encodeCase(value, c1, c2, c3, c4, c5)
      case Schema.Enum6(c1, c2, c3, c4, c5, c6) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6)
      case Schema.Enum7(c1, c2, c3, c4, c5, c6, c7) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7)
      case Schema.Enum8(c1, c2, c3, c4, c5, c6, c7, c8) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8)
      case Schema.Enum9(c1, c2, c3, c4, c5, c6, c7, c8, c9) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9)
      case Schema.Enum10(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
      case Schema.Enum11(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
      case Schema.Enum12(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
      case Schema.Enum13(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
      case Schema.Enum14(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
      case Schema.Enum15(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
      case Schema.Enum16(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
      case Schema.Enum17(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
      case Schema.Enum18(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
      case Schema.Enum19(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
      case Schema.Enum20(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
      case Schema.Enum21(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
      case Schema.Enum22(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22) =>
        encodeCase(value, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
      case Schema.EnumN(caseSet) => encodeCase(value, caseSet.toSeq: _*)
      case Schema.CaseClass1(_, f1, _, ext1) =>
        encodeCaseClass(value, f1 -> ext1)
      case Schema.CaseClass2(_, f1, f2, _, ext1, ext2) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2)
      case Schema.CaseClass3(_, f1, f2, f3, _, ext1, ext2, ext3) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3)
      case Schema.CaseClass4(_, f1, f2, f3, f4, _, ext1, ext2, ext3, ext4) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4)
      case Schema.CaseClass5(_, f1, f2, f3, f4, f5, _, ext1, ext2, ext3, ext4, ext5) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5)
      case Schema.CaseClass6(_, f1, f2, f3, f4, f5, f6, _, ext1, ext2, ext3, ext4, ext5, ext6) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6)
      case Schema.CaseClass7(_, f1, f2, f3, f4, f5, f6, f7, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7)
      case Schema.CaseClass8(_, f1, f2, f3, f4, f5, f6, f7, f8, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8)
      case Schema.CaseClass9(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9)
      case Schema.CaseClass10(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10)
      case Schema.CaseClass11(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11)
      case Schema.CaseClass12(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12)
      case Schema.CaseClass13(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13)
      case Schema.CaseClass14(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14)
      case Schema.CaseClass15(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15)
      case Schema.CaseClass16(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16)
      case Schema.CaseClass17(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17)
      case Schema.CaseClass18(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18)
      case Schema.CaseClass19(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19)
      case Schema.CaseClass20(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, ext20) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19, f20 -> ext20)
      case Schema.CaseClass21(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, ext20, ext21) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19, f20 -> ext20, f21 -> ext21)
      case Schema.CaseClass22(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, ext20, ext21, ext22) =>
        encodeCaseClass(value, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19, f20 -> ext20, f21 -> ext21, f22 -> ext22)
      case Schema.GenericRecord(fieldSet) => encodeGenericRecord(value, fieldSet.toChunk)
      case _: Schema.Fail[_]              => Chunk.empty
    }

    private def encodePrimitive[A](standardType: StandardType[A], v: A): Chunk[Byte] = standardType match {
      case StandardType.UnitType       => Chunk.empty
      case StandardType.StringType     => encodeString(v)
      case StandardType.BoolType       => encodeBoolean(v)
      case StandardType.ShortType      => encodeInt(v.toInt)
      case StandardType.IntType        => encodeInt(v)
      case StandardType.LongType       => encodeLong(v)
      case StandardType.FloatType      => encodeFloat(v)
      case StandardType.DoubleType     => encodeDouble(v)
      case StandardType.BinaryType     => encodeBinary(v)
      case StandardType.CharType       => encodeString(v.toString)
      case StandardType.UUIDType       => encodeUUID(v)
      case StandardType.BigDecimalType => encodeDecimal(v)
      case StandardType.BigIntegerType => encodeBigInteger(v)
      case StandardType.DayOfWeekType  => encodeInt(v.ordinal())
      case StandardType.Month          => encodeInt(v.ordinal())
      case StandardType.MonthDay =>
        encodeFields(monthDayStructure(), ListMap("month" -> v.getMonthValue(), "day" -> v.getDayOfMonth()))
      case StandardType.Period =>
        encodeFields(periodStructure(), ListMap("years" -> v.getYears(), "months" -> v.getMonths(), "days" -> v.getDays()))
      case StandardType.Year => encodeInt(v.getValue())
      case StandardType.YearMonth =>
        encodeFields(yearMonthStructure(), ListMap("year" -> v.getYear, "month" -> v.getMonthValue))
      case StandardType.ZoneId     => encodeString(v.getId())
      case StandardType.ZoneOffset => encodeInt(v.getTotalSeconds())
      case StandardType.Duration(_) =>
        encodeFields(durationStructure(), ListMap("seconds" -> v.getSeconds(), "nanos" -> v.getNano()))
      case StandardType.Instant(_)                => encodeLong(v.toEpochMilli())
      case StandardType.LocalDate(formatter)      => encodeString(v.format(formatter))
      case StandardType.LocalTime(formatter)      => encodeString(v.format(formatter))
      case StandardType.LocalDateTime(formatter)  => encodeString(v.format(formatter))
      case StandardType.OffsetTime(formatter)     => encodeString(v.format(formatter))
      case StandardType.OffsetDateTime(formatter) => encodeString(v.format(formatter))
      case StandardType.ZonedDateTime(formatter)  => encodeString(v.format(formatter))
    }

    @tailrec
    def encodeVarLong(n: Long, acc: Chunk[Byte]): Chunk[Byte] =
      if (n > 0x7F) {
        encodeVarLong(n >>> 7, acc :+ ((n | 0x80) & 0xFF).toByte)
      } else {
        acc ++ Chunk(n.toByte)
      }

    def encodeInt(i: Int): Chunk[Byte] = {
      val n = (i << 1) ^ (i >> 31)
      if ((n & ~0x7F) != 0) {
        ((n | 0x80) & 0xFF).toByte +: encodeVarLong(n.toLong >>> 7, Chunk.empty)
      } else {
        Chunk(n.toByte)
      }
    }

    def encodeLong(l: Long): Chunk[Byte] = {
      val n = (l << 1) ^ (l >> 63)
      if ((n & ~0x7F) != 0) {
        ((n | 0x80) & 0xFF).toByte +: encodeVarLong(n >>> 7, Chunk.empty)
      } else {
        Chunk(n.toByte)
      }
    }

    private def encodeString(value: String): Chunk[Byte] = {
      val encoded = Chunk.fromArray(value.getBytes(StandardCharsets.UTF_8))
      encodeInt(encoded.size) ++ encoded
    }

    def encodeBoolean(value: Boolean): Chunk[Byte] = if (value) boolTrueChunk else boolFalseChunk

    def encodeFloat(value: Float): Chunk[Byte] = {
      val intBits: Int = lang.Float.floatToRawIntBits(value)
      val bytes: Array[Byte] =
        ByteBuffer.allocate(lang.Integer.BYTES).order(ByteOrder.LITTLE_ENDIAN).putInt(intBits).array
      Chunk.fromArray(bytes)
    }

    def encodeDouble(value: Double): Chunk[Byte] = {
      val longBits: Long = lang.Double.doubleToRawLongBits(value)
      val bytes: Array[Byte] =
        ByteBuffer.allocate(lang.Long.BYTES).order(ByteOrder.LITTLE_ENDIAN).putLong(longBits).array
      Chunk.fromArray(bytes)
    }

    def encodeBinary(value: Chunk[Byte]): Chunk[Byte] = encodeInt(value.size) ++ value

    def encodeUUID(value: java.util.UUID): Chunk[Byte] = encodeString(value.toString)

    def encodeDecimal(value: BigDecimal): Chunk[Byte] =
      Chunk.fromArray(value.bigDecimal.unscaledValue().toByteArray()) // The scala is fixed in the corresponding AvroSchema. TODO: is it an issue that different values (different scale) yield different schemas?

    def encodeBigInteger(value: BigInt): Chunk[Byte] = encodeDecimal(BigDecimal(value))

    def tupleSchema[A, B](first: Schema[A], second: Schema[B]): Schema[ListMap[String, _]] =
      Schema.record(Schema.Field("first", first), Schema.Field("second", second))

    def singleSchema[A](codec: Schema[A]): Schema[ListMap[String, _]] = Schema.record(Schema.Field("value", codec))

    def optionEmptyStructure() = Schema.Primitive(StandardType.UnitType)

    def monthDayStructure(): Seq[Schema.Field[Int]] =
      Seq(Schema.Field("month", Schema.Primitive(StandardType.IntType)), Schema.Field("day", Schema.Primitive(StandardType.IntType)))

    def periodStructure(): Seq[Schema.Field[Int]] = Seq(Schema.Field("years", Schema.Primitive(StandardType.IntType)), Schema.Field("months", Schema.Primitive(StandardType.IntType)), Schema.Field("days", Schema.Primitive(StandardType.IntType)))

    def yearMonthStructure(): Seq[Schema.Field[Int]] =
      Seq(Schema.Field("year", Schema.Primitive(StandardType.IntType)), Schema.Field("month", Schema.Primitive(StandardType.IntType)))

    def durationStructure(): Seq[Schema.Field[_]] =
      Seq(Schema.Field("seconds", Schema.Primitive(StandardType.LongType)), Schema.Field("nanos", Schema.Primitive(StandardType.IntType)))

    def encodeFields(schema: Seq[Schema.Field[_]], value: ListMap[String, _]): Chunk[Byte] =
      Chunk
        .fromIterable(schema.map {
          case Schema.Field(label, schema, _) =>
            value
              .get(label)
              .map(value => encode(schema.asInstanceOf[Schema[Any]], value))
              .getOrElse(Chunk.empty)
        })
        .flatten

    private def encodeSequence[A](element: Schema[A], sequence: Chunk[A]): Chunk[Byte] = {
      val encoded = sequence.flatMap(value => encode(element, value))
      encodeInt(sequence.size) ++ encoded ++ encodeLong(0L)
    }

    private def encodeOptional[A](schema: Schema[A], value: Option[A]): Chunk[Byte] =
      encodeEither(optionEmptyStructure(), schema, value.toRight(()))

    private def encodeTuple[A, B](left: Schema[A], right: Schema[B], tuple: (A, B)): Chunk[Byte] =
      encode(tupleSchema(left, right), ListMap[String, Any]("first" -> tuple._1, "second" -> tuple._2))

    private def encodeEither[A, B](left: Schema[A], right: Schema[B], either: Either[A, B]): Chunk[Byte] =
      either match {
        case Left(value)  => encodeBoolean(false) ++ encode(left, value)
        case Right(value) => encodeBoolean(true) ++ encode(right, value)
      }

    def encodeCase[Z](v: Z, cases: Schema.Case[_, Z]*): Chunk[Byte] = {
      val fieldIndex = cases.indexWhere(c => c.deconstruct(v).isDefined)
      val encoded = Chunk.fromIterable(if (fieldIndex == -1) {
        Chunk.empty
      } else {
        val subtypeCase = cases(fieldIndex)
        encode(subtypeCase.codec.asInstanceOf[Schema[Any]], subtypeCase.unsafeDeconstruct(v))
      })
      encodeInt(fieldIndex) ++ encoded
    }

    private def encodeGenericRecord(value: Map[String, _], structure: Seq[Schema.Field[_]]): Chunk[Byte] =
      Chunk
        .fromIterable(structure.map {
          case Schema.Field(label, schema, _) =>
            value
              .get(label)
              .map(value => encode(schema.asInstanceOf[Schema[Any]], value))
              .getOrElse(Chunk.empty)
        })
        .flatten

    private def encodeCaseClass[Z](value: Z, fields: (Schema.Field[_], Z => Any)*): Chunk[Byte] =
      Chunk.fromIterable(fields.map { case (Schema.Field(_, schema, _), ext) => encode(schema.asInstanceOf[Schema[Any]], ext(value)) }).flatten
  }

  object Decoder {
    def decode[A](schema: Schema[A], chunk: Chunk[Byte]): Either[String, A] = ???
  }
}
