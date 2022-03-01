package zio.schema.codec

import java.nio.CharBuffer
import java.nio.charset.StandardCharsets

import scala.collection.immutable.ListMap

import zio.json.JsonCodec._
import zio.json.JsonDecoder.{ JsonError, UnsafeJson }
import zio.json.internal.{ Lexer, RetractReader, StringMatrix, Write }
import zio.json.{ JsonCodec => ZJsonCodec, JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }
import zio.schema.Schema.EitherSchema
import zio.schema.ast.SchemaAst
import zio.schema.{ StandardType, _ }
import zio.stream.ZTransducer
import zio.{ Chunk, ChunkBuilder, ZIO }

object JsonCodec extends Codec {

  override def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte] =
    ZTransducer.fromPush(
      (opt: Option[Chunk[A]]) =>
        ZIO
          .effect(opt.map(values => values.flatMap(Encoder.encode(schema, _))).getOrElse(Chunk.empty))
          .catchAll(_ => ZIO.succeed(Chunk.empty))
    )

  override def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A] =
    ZTransducer.utfDecode >>> ZTransducer.fromFunctionM(
      (s: String) => ZIO.fromEither(Decoder.decode(schema, s))
    )

  override def encode[A](schema: Schema[A]): A => Chunk[Byte] = Encoder.encode(schema, _)

  override def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A] =
    (chunk: Chunk[Byte]) => Decoder.decode(schema, new String(chunk.toArray, Encoder.CHARSET))

  object Codecs {
    protected[codec] val unitEncoder: JsonEncoder[Unit] =
      (_: Unit, _: Option[Int], out: Write) => out.write("{}")

    protected[codec] val unitDecoder: JsonDecoder[Unit] =
      (trace: List[JsonDecoder.JsonError], in: RetractReader) => {
        Lexer.char(trace, in, '{')
        Lexer.char(trace, in, '}')
        ()
      }

    protected[codec] val unitCodec: ZJsonCodec[Unit] = ZJsonCodec(unitEncoder, unitDecoder)

    protected[codec] def failDecoder[A](message: String): JsonDecoder[A] =
      (trace: List[JsonDecoder.JsonError], _: RetractReader) => throw UnsafeJson(JsonError.Message(message) :: trace)

    private[codec] def primitiveCodec[A](standardType: StandardType[A]): ZJsonCodec[A] =
      standardType match {
        case StandardType.UnitType              => unitCodec
        case StandardType.StringType            => ZJsonCodec.string
        case StandardType.BoolType              => ZJsonCodec.boolean
        case StandardType.ShortType             => ZJsonCodec.short
        case StandardType.IntType               => ZJsonCodec.int
        case StandardType.LongType              => ZJsonCodec.long
        case StandardType.FloatType             => ZJsonCodec.float
        case StandardType.DoubleType            => ZJsonCodec.double
        case StandardType.BinaryType            => ZJsonCodec.chunk(ZJsonCodec.byte)
        case StandardType.CharType              => ZJsonCodec.char
        case StandardType.BigIntegerType        => ZJsonCodec.bigInteger
        case StandardType.BigDecimalType        => ZJsonCodec.bigDecimal
        case StandardType.UUIDType              => ZJsonCodec.uuid
        case StandardType.DayOfWeekType         => ZJsonCodec.dayOfWeek // ZJsonCodec[java.time.DayOfWeek]
        case StandardType.Duration(_)           => ZJsonCodec.duration //ZJsonCodec[java.time.Duration]
        case StandardType.InstantType(_)        => ZJsonCodec.instant //ZJsonCodec[java.time.Instant]
        case StandardType.LocalDateType(_)      => ZJsonCodec.localDate //ZJsonCodec[java.time.LocalDate]
        case StandardType.LocalDateTimeType(_)  => ZJsonCodec.localDateTime //ZJsonCodec[java.time.LocalDateTime]
        case StandardType.LocalTimeType(_)      => ZJsonCodec.localTime //ZJsonCodec[java.time.LocalTime]
        case StandardType.MonthType             => ZJsonCodec.month //ZJsonCodec[java.time.Month]
        case StandardType.MonthDayType          => ZJsonCodec.monthDay //ZJsonCodec[java.time.MonthDay]
        case StandardType.OffsetDateTimeType(_) => ZJsonCodec.offsetDateTime //ZJsonCodec[java.time.OffsetDateTime]
        case StandardType.OffsetTimeType(_)     => ZJsonCodec.offsetTime //ZJsonCodec[java.time.OffsetTime]
        case StandardType.PeriodType            => ZJsonCodec.period //ZJsonCodec[java.time.Period]
        case StandardType.YearType              => ZJsonCodec.year //ZJsonCodec[java.time.Year]
        case StandardType.YearMonthType         => ZJsonCodec.yearMonth //ZJsonCodec[java.time.YearMonth]
        case StandardType.ZonedDateTimeType(_)  => ZJsonCodec.zonedDateTime //ZJsonCodec[java.time.ZonedDateTime]
        case StandardType.ZoneIdType            => ZJsonCodec.zoneId //ZJsonCodec[java.time.ZoneId]
        case StandardType.ZoneOffsetType        => ZJsonCodec.zoneOffset //ZJsonCodec[java.time.ZoneOffset]
      }
  }

  object Encoder {

    import Codecs._
    import JsonEncoder.{ bump, pad }
    import ProductEncoder._

    private[codec] val CHARSET = StandardCharsets.UTF_8

    final def encode[A](schema: Schema[A], value: A): Chunk[Byte] =
      charSequenceToByteChunk(schemaEncoder(schema).encodeJson(value, None))

    private[codec] def charSequenceToByteChunk(chars: CharSequence): Chunk[Byte] = {
      val bytes = CHARSET.newEncoder().encode(CharBuffer.wrap(chars))
      Chunk.fromByteBuffer(bytes)
    }

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[codec] def schemaEncoder[A](schema: Schema[A]): JsonEncoder[A] = schema match {
      case Schema.Primitive(standardType, _)   => primitiveCodec(standardType)
      case Schema.Sequence(schema, _, g, _, _) => JsonEncoder.chunk(schemaEncoder(schema)).contramap(g)
      case Schema.MapSchema(ks, vs, _) =>
        JsonEncoder.chunk(schemaEncoder(ks).both(schemaEncoder(vs))).contramap(m => Chunk.fromIterable(m))
      case Schema.SetSchema(s, _) =>
        JsonEncoder.chunk(schemaEncoder(s)).contramap(m => Chunk.fromIterable(m))
      case Schema.Transform(c, _, g, _, _)                       => transformEncoder(c, g)
      case Schema.Tuple(l, r, _)                                 => JsonEncoder.tuple2(schemaEncoder(l), schemaEncoder(r))
      case Schema.Optional(schema, _)                            => JsonEncoder.option(schemaEncoder(schema))
      case Schema.Fail(_, _)                                     => unitEncoder.contramap(_ => ())
      case Schema.GenericRecord(structure, _)                    => recordEncoder(structure.toChunk)
      case EitherSchema(left, right, _)                          => JsonEncoder.either(schemaEncoder(left), schemaEncoder(right))
      case l @ Schema.Lazy(_)                                    => schemaEncoder(l.schema)
      case Schema.Meta(_, _)                                     => astEncoder
      case Schema.CaseClass1(f, _, ext, _)                       => caseClassEncoder(f -> ext)
      case Schema.CaseClass2(f1, f2, _, ext1, ext2, _)           => caseClassEncoder(f1 -> ext1, f2 -> ext2)
      case Schema.CaseClass3(f1, f2, f3, _, ext1, ext2, ext3, _) => caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3)
      case Schema.CaseClass4(f1, f2, f3, f4, _, ext1, ext2, ext3, ext4, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4)
      case Schema.CaseClass5(f1, f2, f3, f4, f5, _, ext1, ext2, ext3, ext4, ext5, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5)
      case Schema.CaseClass6(f1, f2, f3, f4, f5, f6, _, ext1, ext2, ext3, ext4, ext5, ext6, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6)
      case Schema.CaseClass7(f1, f2, f3, f4, f5, f6, f7, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7)
      case Schema.CaseClass8(f1, f2, f3, f4, f5, f6, f7, f8, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8)
      case Schema
            .CaseClass9(f1, f2, f3, f4, f5, f6, f7, f8, f9, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9)
      case Schema.CaseClass10(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10)
      case Schema.CaseClass11(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11)
      case Schema.CaseClass12(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12)
      case Schema.CaseClass13(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13)
      case Schema.CaseClass14(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14)
      case Schema.CaseClass15(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15)
      case Schema.CaseClass16(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16)
      case Schema.CaseClass17(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17)
      case Schema.CaseClass18(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18)
      case Schema.CaseClass19(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19)
      case Schema.CaseClass20(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, ext20, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19, f20 -> ext20)
      case Schema.CaseClass21(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, ext20, ext21, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19, f20 -> ext20, f21 -> ext21)
      case Schema.CaseClass22(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, ext20, ext21, ext22, _) =>
        caseClassEncoder(f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19, f20 -> ext20, f21 -> ext21, f22 -> ext22)
      case Schema.Enum1(c, _)                                  => enumEncoder(c)
      case Schema.Enum2(c1, c2, _)                             => enumEncoder(c1, c2)
      case Schema.Enum3(c1, c2, c3, _)                         => enumEncoder(c1, c2, c3)
      case Schema.Enum4(c1, c2, c3, c4, _)                     => enumEncoder(c1, c2, c3, c4)
      case Schema.Enum5(c1, c2, c3, c4, c5, _)                 => enumEncoder(c1, c2, c3, c4, c5)
      case Schema.Enum6(c1, c2, c3, c4, c5, c6, _)             => enumEncoder(c1, c2, c3, c4, c5, c6)
      case Schema.Enum7(c1, c2, c3, c4, c5, c6, c7, _)         => enumEncoder(c1, c2, c3, c4, c5, c6, c7)
      case Schema.Enum8(c1, c2, c3, c4, c5, c6, c7, c8, _)     => enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8)
      case Schema.Enum9(c1, c2, c3, c4, c5, c6, c7, c8, c9, _) => enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9)
      case Schema.Enum10(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
      case Schema.Enum11(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
      case Schema.Enum12(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
      case Schema.Enum13(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
      case Schema.Enum14(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
      case Schema.Enum15(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
      case Schema.Enum16(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
      case Schema.Enum17(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
      case Schema.Enum18(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
      case Schema.Enum19(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
      case Schema
            .Enum20(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
      case Schema
            .Enum21(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
      case Schema.Enum22(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _) =>
        enumEncoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
      case Schema.EnumN(cs, _) => enumEncoder(cs.toSeq: _*)
    }
    //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

    private val astEncoder: JsonEncoder[Schema[_]] =
      (schema: Schema[_], indent: Option[Int], out: Write) =>
        schemaEncoder(Schema[SchemaAst]).unsafeEncode(SchemaAst.fromSchema(schema), indent, out)

    private def transformEncoder[A, B](schema: Schema[A], g: B => Either[String, A]): JsonEncoder[B] = {
      (b: B, indent: Option[Int], out: Write) =>
        g(b) match {
          case Left(_)  => ()
          case Right(a) => schemaEncoder(schema).unsafeEncode(a, indent, out)
        }
    }

    private def enumEncoder[Z](cases: Schema.Case[_, Z]*): JsonEncoder[Z] =
      (value: Z, indent: Option[Int], out: Write) => {
        val fieldIndex = cases.indexWhere(c => c.deconstruct(value).isDefined)
        if (fieldIndex > -1) {
          val case_ = cases(fieldIndex)
          out.write('{')
          val indent_ = bump(indent)
          pad(indent_, out)
          string.unsafeEncode(JsonFieldEncoder.string.unsafeEncodeField(case_.id), indent_, out)
          if (indent.isEmpty) out.write(':')
          else out.write(" : ")
          schemaEncoder(case_.codec.asInstanceOf[Schema[Any]]).unsafeEncode(case_.unsafeDeconstruct(value), indent, out)
          out.write('}')
        } else {
          out.write("{}")
        }
      }

    private def recordEncoder(structure: Seq[Schema.Field[_]]): JsonEncoder[ListMap[String, _]] = {
      (value: ListMap[String, _], indent: Option[Int], out: Write) =>
        {
          if (structure.isEmpty) {
            out.write("{}")
          } else {
            out.write('{')
            val indent_ = bump(indent)
            pad(indent_, out)
            var first = true
            structure.foreach {
              case Schema.Field(k, a, _) =>
                val enc = schemaEncoder(a.asInstanceOf[Schema[Any]])
                if (first)
                  first = false
                else {
                  out.write(',')
                  if (indent.isDefined)
                    JsonEncoder.pad(indent_, out)
                }
                string.unsafeEncode(JsonFieldEncoder.string.unsafeEncodeField(k), indent_, out)
                if (indent.isEmpty) out.write(':')
                else out.write(" : ")
                enc.unsafeEncode(value(k), indent_, out)
            }
            pad(indent, out)
            out.write('}')
          }
        }
    }
  }

  object Decoder {

    import Codecs._
    import ProductDecoder._

    final def decode[A](schema: Schema[A], json: String): Either[String, A] =
      schemaDecoder(schema).decodeJson(json)

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[codec] def schemaDecoder[A](schema: Schema[A]): JsonDecoder[A] = schema match {
      case Schema.Primitive(standardType, _)   => primitiveCodec(standardType)
      case Schema.Optional(codec, _)           => JsonDecoder.option(schemaDecoder(codec))
      case Schema.Tuple(left, right, _)        => JsonDecoder.tuple2(schemaDecoder(left), schemaDecoder(right))
      case Schema.Transform(codec, f, _, _, _) => schemaDecoder(codec).mapOrFail(f)
      case Schema.Sequence(codec, f, _, _, _)  => JsonDecoder.chunk(schemaDecoder(codec)).map(f)
      case Schema.MapSchema(ks, vs, _) =>
        JsonDecoder.chunk(schemaDecoder(ks) <*> schemaDecoder(vs)).map(entries => entries.toList.toMap)
      case Schema.SetSchema(s, _)                                                                   => JsonDecoder.chunk(schemaDecoder(s)).map(entries => entries.toSet)
      case Schema.Fail(message, _)                                                                  => failDecoder(message)
      case Schema.GenericRecord(structure, _)                                                       => recordDecoder(structure.toChunk)
      case Schema.EitherSchema(left, right, _)                                                      => JsonDecoder.either(schemaDecoder(left), schemaDecoder(right))
      case l @ Schema.Lazy(_)                                                                       => schemaDecoder(l.schema)
      case Schema.Meta(_, _)                                                                        => astDecoder
      case s @ Schema.CaseClass1(_, _, _, _)                                                        => caseClass1Decoder(s)
      case s @ Schema.CaseClass2(_, _, _, _, _, _)                                                  => caseClass2Decoder(s)
      case s @ Schema.CaseClass3(_, _, _, _, _, _, _, _)                                            => caseClass3Decoder(s)
      case s @ Schema.CaseClass4(_, _, _, _, _, _, _, _, _, _)                                      => caseClass4Decoder(s)
      case s @ Schema.CaseClass5(_, _, _, _, _, _, _, _, _, _, _, _)                                => caseClass5Decoder(s)
      case s @ Schema.CaseClass6(_, _, _, _, _, _, _, _, _, _, _, _, _, _)                          => caseClass6Decoder(s)
      case s @ Schema.CaseClass7(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                    => caseClass7Decoder(s)
      case s @ Schema.CaseClass8(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)              => caseClass8Decoder(s)
      case s @ Schema.CaseClass9(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)        => caseClass9Decoder(s)
      case s @ Schema.CaseClass10(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => caseClass10Decoder(s)
      case s @ Schema.CaseClass11(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass11Decoder(s)
      case s @ Schema.CaseClass12(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass12Decoder(s)
      case s @ Schema.CaseClass13(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass13Decoder(s)
      case s @ Schema
            .CaseClass14(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass14Decoder(s)
      case s @ Schema
            .CaseClass15(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass15Decoder(s)
      case s @ Schema.CaseClass16(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass16Decoder(s)
      case s @ Schema.CaseClass17(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass17Decoder(s)
      case s @ Schema.CaseClass18(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass18Decoder(s)
      case s @ Schema.CaseClass19(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass19Decoder(s)
      case s @ Schema.CaseClass20(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass20Decoder(s)
      case s @ Schema.CaseClass21(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass21Decoder(s)
      case s @ Schema.CaseClass22(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass22Decoder(s)
      case Schema.Enum1(c, _)                                  => enumDecoder(c)
      case Schema.Enum2(c1, c2, _)                             => enumDecoder(c1, c2)
      case Schema.Enum3(c1, c2, c3, _)                         => enumDecoder(c1, c2, c3)
      case Schema.Enum4(c1, c2, c3, c4, _)                     => enumDecoder(c1, c2, c3, c4)
      case Schema.Enum5(c1, c2, c3, c4, c5, _)                 => enumDecoder(c1, c2, c3, c4, c5)
      case Schema.Enum6(c1, c2, c3, c4, c5, c6, _)             => enumDecoder(c1, c2, c3, c4, c5, c6)
      case Schema.Enum7(c1, c2, c3, c4, c5, c6, c7, _)         => enumDecoder(c1, c2, c3, c4, c5, c6, c7)
      case Schema.Enum8(c1, c2, c3, c4, c5, c6, c7, c8, _)     => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8)
      case Schema.Enum9(c1, c2, c3, c4, c5, c6, c7, c8, c9, _) => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9)
      case Schema.Enum10(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
      case Schema.Enum11(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
      case Schema.Enum12(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
      case Schema.Enum13(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
      case Schema.Enum14(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
      case Schema.Enum15(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
      case Schema.Enum16(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
      case Schema.Enum17(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
      case Schema.Enum18(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
      case Schema.Enum19(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
      case Schema
            .Enum20(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
      case Schema.Enum21(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
      case Schema.Enum22(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _) =>
        enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
      case Schema.EnumN(cs, _) => enumDecoder(cs.toSeq: _*)
    }
    //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

    private def astDecoder: JsonDecoder[Schema[_]] =
      schemaDecoder(Schema[SchemaAst]).map(ast => ast.toSchema)

    private def enumDecoder[Z](cases: Schema.Case[_, Z]*): JsonDecoder[Z] = {
      (trace: List[JsonError], in: RetractReader) =>
        {
          Lexer.char(trace, in, '{')
          if (Lexer.firstField(trace, in)) {
            val subtype = Lexer.string(trace, in).toString
            val trace_  = JsonError.ObjectAccess(subtype) :: trace
            Lexer.char(trace_, in, ':')
            cases.find(_.id == subtype) match {
              case Some(c) =>
                val decoded = schemaDecoder(c.codec).unsafeDecode(trace_, in).asInstanceOf[Z]
                Lexer.nextField(trace, in)
                decoded
              case None =>
                throw UnsafeJson(JsonError.Message("unrecognized subtype") :: trace_)
            }
          } else {
            throw UnsafeJson(JsonError.Message("missing subtype") :: trace)
          }
        }
    }

    private def recordDecoder(structure: Seq[Schema.Field[_]]): JsonDecoder[ListMap[String, Any]] = {
      (trace: List[JsonError], in: RetractReader) =>
        {
          val builder: ChunkBuilder[(String, Any)] = zio.ChunkBuilder.make[(String, Any)](structure.size)
          Lexer.char(trace, in, '{')
          if (Lexer.firstField(trace, in)) {
            do {
              val field = Lexer.string(trace, in).toString
              structure.find(_.label == field) match {
                case Some(Schema.Field(label, schema, _)) =>
                  val trace_ = JsonError.ObjectAccess(label) :: trace
                  Lexer.char(trace_, in, ':')
                  val value = schemaDecoder(schema).unsafeDecode(trace_, in)
                  builder += ((JsonFieldDecoder.string.unsafeDecodeField(trace_, label), value))
                case None =>
                  Lexer.skipValue(trace, in)

              }
            } while (Lexer.nextField(trace, in))
          }
          (ListMap.newBuilder[String, Any] ++= builder.result()).result()
        }
    }

  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private[codec] object ProductEncoder {
    import JsonEncoder.{ bump, pad }

    private[codec] def caseClassEncoder[Z](fields: (Schema.Field[_], Z => Any)*): JsonEncoder[Z] = { (a: Z, indent: Option[Int], out: Write) =>
      {
        out.write('{')
        val indent_ = bump(indent)
        pad(indent_, out)
        var first = true
        fields.foreach {
          case (Schema.Field(key, schema, _), ext) =>
            val enc = Encoder.schemaEncoder(schema.asInstanceOf[Schema[Any]])
            if (first)
              first = false
            else {
              out.write(',')
              if (indent.isDefined)
                JsonEncoder.pad(indent_, out)
            }

            string.unsafeEncode(JsonFieldEncoder.string.unsafeEncodeField(key), indent_, out)
            if (indent.isEmpty) out.write(':')
            else out.write(" : ")
            enc.unsafeEncode(ext(a), indent_, out)
        }
        pad(indent, out)
        out.write('}')
      }
    }
  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private[codec] object ProductDecoder {
    import Decoder.schemaDecoder

    private[codec] def caseClass1Decoder[A, Z](schema: Schema.CaseClass1[A, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field)
        schema.construct(buffer(0).asInstanceOf[A])
      }
    }

    private[codec] def caseClass2Decoder[A1, A2, Z](schema: Schema.CaseClass2[A1, A2, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2])
      }
    }

    private[codec] def caseClass3Decoder[A1, A2, A3, Z](schema: Schema.CaseClass3[A1, A2, A3, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3])
      }
    }

    private[codec] def caseClass4Decoder[A1, A2, A3, A4, Z](schema: Schema.CaseClass4[A1, A2, A3, A4, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] =
          unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4])
      }
    }

    private[codec] def caseClass5Decoder[A1, A2, A3, A4, A5, Z](schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] =
          unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5])
      }
    }

    private[codec] def caseClass6Decoder[A1, A2, A3, A4, A5, A6, Z](schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6])
      }
    }

    private[codec] def caseClass7Decoder[A1, A2, A3, A4, A5, A6, A7, Z](schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7])
      }
    }

    private[codec] def caseClass8Decoder[A1, A2, A3, A4, A5, A6, A7, A8, Z](schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8])
      }
    }

    private[codec] def caseClass9Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9])
      }
    }

    private[codec] def caseClass10Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10])
      }
    }

    private[codec] def caseClass11Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11])
      }
    }

    private[codec] def caseClass12Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12])
      }
    }

    private[codec] def caseClass13Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12], buffer(12).asInstanceOf[A13])
      }
    }

    private[codec] def caseClass14Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14)
        schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14]
        )
      }
    }

    private[codec] def caseClass15Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15)
        schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15]
        )
      }
    }

    private[codec] def caseClass16Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16)
        schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16]
        )
      }
    }

    private[codec] def caseClass17Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17)
        schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16],
          buffer(16).asInstanceOf[A17]
        )
      }
    }

    private[codec] def caseClass18Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18)
        schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16],
          buffer(16).asInstanceOf[A17],
          buffer(17).asInstanceOf[A18]
        )
      }
    }

    private[codec] def caseClass19Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](schema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19)
        schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16],
          buffer(16).asInstanceOf[A17],
          buffer(17).asInstanceOf[A18],
          buffer(18).asInstanceOf[A19]
        )
      }
    }

    private[codec] def caseClass20Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](schema: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20)
        schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16],
          buffer(16).asInstanceOf[A17],
          buffer(17).asInstanceOf[A18],
          buffer(18).asInstanceOf[A19],
          buffer(19).asInstanceOf[A20]
        )
      }
    }

    private[codec] def caseClass21Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](schema: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20, schema.field21)
        schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16],
          buffer(16).asInstanceOf[A17],
          buffer(17).asInstanceOf[A18],
          buffer(18).asInstanceOf[A19],
          buffer(19).asInstanceOf[A20],
          buffer(20).asInstanceOf[A21]
        )
      }
    }

    private[codec] def caseClass22Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](schema: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z]): JsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] =
          unsafeDecodeFields(trace, in, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20, schema.field21, schema.field22)
        schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16],
          buffer(16).asInstanceOf[A17],
          buffer(17).asInstanceOf[A18],
          buffer(18).asInstanceOf[A19],
          buffer(19).asInstanceOf[A20],
          buffer(20).asInstanceOf[A21],
          buffer(21).asInstanceOf[A22]
        )
      }
    }

    private def unsafeDecodeFields(trace: List[JsonError], in: RetractReader, fields: Schema.Field[_]*): Array[Any] = {
      val len: Int                  = fields.length
      val buffer                    = Array.ofDim[Any](len)
      val matrix                    = new StringMatrix(fields.map(_.label).toArray)
      val spans: Array[JsonError]   = fields.map(_.label).toArray.map(JsonError.ObjectAccess(_))
      val schemas: Array[Schema[_]] = fields.map(_.schema).toArray
      Lexer.char(trace, in, '{')
      if (Lexer.firstField(trace, in)) {
        do {
          var trace_ = trace
          val field  = Lexer.field(trace, in, matrix)
          if (field != -1) {
            trace_ = spans(field) :: trace
            if (buffer(field) != null)
              throw UnsafeJson(JsonError.Message("duplicate") :: trace)
            else
              buffer(field) = schemaDecoder(schemas(field)).unsafeDecode(trace_, in)
          } else Lexer.skipValue(trace_, in)
        } while (Lexer.nextField(trace, in))
      }

      var i = 0
      while (i < len) {
        if (buffer(i) == null)
          buffer(i) = schemaDecoder(schemas(i)).unsafeDecodeMissing(spans(i) :: trace)
        i += 1
      }
      buffer
    }
  }

}
