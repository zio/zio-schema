package zio.schema.codec

import java.nio.CharBuffer
import java.nio.charset.StandardCharsets

import scala.collection.immutable.ListMap

import zio.json.JsonCodec._
import zio.json.JsonDecoder.{ JsonError, UnsafeJson }
import zio.json.ast.Json
import zio.json.internal.{ Lexer, RecordingReader, RetractReader, StringMatrix, Write }
import zio.json.{
  JsonCodec => ZJsonCodec,
  JsonDecoder => ZJsonDecoder,
  JsonEncoder => ZJsonEncoder,
  JsonFieldDecoder,
  JsonFieldEncoder
}
import zio.schema._
import zio.schema.annotation._
import zio.schema.codec.DecodeError.ReadError
import zio.stream.ZPipeline
import zio.{ Cause, Chunk, ChunkBuilder, NonEmptyChunk, ZIO }

object JsonCodec {
  type DiscriminatorTuple = Chunk[(discriminatorName, String)]

  implicit def zioJsonBinaryCodec[A](implicit jsonCodec: ZJsonCodec[A]): BinaryCodec[A] =
    new BinaryCodec[A] {
      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] =
        jsonCodec
          .decodeJson(
            new String(whole.toArray, JsonEncoder.CHARSET)
          )
          .left
          .map(failure => DecodeError.ReadError(Cause.empty, failure))

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
        ZPipeline.fromChannel(
          ZPipeline.utfDecode.channel.mapError(cce => ReadError(Cause.fail(cce), cce.getMessage))
        ) >>>
          ZPipeline.groupAdjacentBy[String, Unit](_ => ()) >>>
          ZPipeline.map[(Unit, NonEmptyChunk[String]), String] {
            case (_, fragments) => fragments.mkString
          } >>>
          ZPipeline.mapZIO { (s: String) =>
            ZIO
              .fromEither(jsonCodec.decodeJson(s))
              .mapError(failure => DecodeError.ReadError(Cause.empty, failure))
          }

      override def encode(value: A): Chunk[Byte] =
        JsonEncoder.charSequenceToByteChunk(jsonCodec.encodeJson(value, None))

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
        ZPipeline.mapChunks(
          _.flatMap(encode)
        )
    }

  implicit def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {
      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] =
        JsonDecoder.decode(
          schema,
          new String(whole.toArray, JsonEncoder.CHARSET)
        )

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
        ZPipeline.fromChannel(
          ZPipeline.utfDecode.channel.mapError(cce => ReadError(Cause.fail(cce), cce.getMessage))
        ) >>>
          ZPipeline.groupAdjacentBy[String, Unit](_ => ()) >>>
          ZPipeline.map[(Unit, NonEmptyChunk[String]), String] {
            case (_, fragments) => fragments.mkString
          } >>>
          ZPipeline.mapZIO { (s: String) =>
            ZIO.fromEither(JsonDecoder.decode(schema, s))
          }

      override def encode(value: A): Chunk[Byte] =
        JsonEncoder.encode(schema, value)

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
        ZPipeline.mapChunks(
          _.flatMap(encode)
        )
    }

  def jsonEncoder[A](schema: Schema[A]): ZJsonEncoder[A] =
    JsonEncoder.schemaEncoder(schema)

  def jsonDecoder[A](schema: Schema[A]): ZJsonDecoder[A] =
    JsonDecoder.schemaDecoder(schema)

  def jsonCodec[A](schema: Schema[A]): ZJsonCodec[A] =
    ZJsonCodec(jsonEncoder(schema), jsonDecoder(schema))

  object Codecs {
    protected[codec] val unitEncoder: ZJsonEncoder[Unit] =
      (_: Unit, _: Option[Int], out: Write) => out.write("{}")

    private[codec] val unitDecoder: ZJsonDecoder[Unit] =
      (trace: List[ZJsonDecoder.JsonError], in: RetractReader) => {
        Lexer.char(trace, in, '{')
        Lexer.char(trace, in, '}')
        ()
      }

    protected[codec] val unitCodec: ZJsonCodec[Unit] = ZJsonCodec(unitEncoder, unitDecoder)

    protected[codec] def failDecoder[A](message: String): ZJsonDecoder[A] =
      (trace: List[ZJsonDecoder.JsonError], _: RetractReader) => throw UnsafeJson(JsonError.Message(message) :: trace)

    private[codec] def primitiveCodec[A](standardType: StandardType[A]): ZJsonCodec[A] =
      standardType match {
        case StandardType.UnitType           => unitCodec
        case StandardType.StringType         => ZJsonCodec.string
        case StandardType.BoolType           => ZJsonCodec.boolean
        case StandardType.ByteType           => ZJsonCodec.byte
        case StandardType.ShortType          => ZJsonCodec.short
        case StandardType.IntType            => ZJsonCodec.int
        case StandardType.LongType           => ZJsonCodec.long
        case StandardType.FloatType          => ZJsonCodec.float
        case StandardType.DoubleType         => ZJsonCodec.double
        case StandardType.BinaryType         => ZJsonCodec.chunk(ZJsonCodec.byte.encoder, ZJsonCodec.byte.decoder)
        case StandardType.CharType           => ZJsonCodec.char
        case StandardType.BigIntegerType     => ZJsonCodec.bigInteger
        case StandardType.BigDecimalType     => ZJsonCodec.bigDecimal
        case StandardType.UUIDType           => ZJsonCodec.uuid
        case StandardType.DayOfWeekType      => ZJsonCodec.dayOfWeek // ZJsonCodec[java.time.DayOfWeek]
        case StandardType.DurationType       => ZJsonCodec.duration //ZJsonCodec[java.time.Duration]
        case StandardType.InstantType        => ZJsonCodec.instant //ZJsonCodec[java.time.Instant]
        case StandardType.LocalDateType      => ZJsonCodec.localDate //ZJsonCodec[java.time.LocalDate]
        case StandardType.LocalDateTimeType  => ZJsonCodec.localDateTime //ZJsonCodec[java.time.LocalDateTime]
        case StandardType.LocalTimeType      => ZJsonCodec.localTime //ZJsonCodec[java.time.LocalTime]
        case StandardType.MonthType          => ZJsonCodec.month //ZJsonCodec[java.time.Month]
        case StandardType.MonthDayType       => ZJsonCodec.monthDay //ZJsonCodec[java.time.MonthDay]
        case StandardType.OffsetDateTimeType => ZJsonCodec.offsetDateTime //ZJsonCodec[java.time.OffsetDateTime]
        case StandardType.OffsetTimeType     => ZJsonCodec.offsetTime //ZJsonCodec[java.time.OffsetTime]
        case StandardType.PeriodType         => ZJsonCodec.period //ZJsonCodec[java.time.Period]
        case StandardType.YearType           => ZJsonCodec.year //ZJsonCodec[java.time.Year]
        case StandardType.YearMonthType      => ZJsonCodec.yearMonth //ZJsonCodec[java.time.YearMonth]
        case StandardType.ZonedDateTimeType  => ZJsonCodec.zonedDateTime //ZJsonCodec[java.time.ZonedDateTime]
        case StandardType.ZoneIdType         => ZJsonCodec.zoneId //ZJsonCodec[java.time.ZoneId]
        case StandardType.ZoneOffsetType     => ZJsonCodec.zoneOffset //ZJsonCodec[java.time.ZoneOffset]
      }
  }

  object JsonEncoder {

    import Codecs._
    import ProductEncoder._
    import ZJsonEncoder.{ bump, pad }

    private[codec] val CHARSET = StandardCharsets.UTF_8

    final def encode[A](schema: Schema[A], value: A): Chunk[Byte] =
      charSequenceToByteChunk(schemaEncoder(schema).encodeJson(value, None))

    private[codec] def charSequenceToByteChunk(chars: CharSequence): Chunk[Byte] = {
      val bytes = CHARSET.newEncoder().encode(CharBuffer.wrap(chars))
      Chunk.fromByteBuffer(bytes)
    }

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[codec] def schemaEncoder[A](schema: Schema[A], discriminatorTuple: DiscriminatorTuple = Chunk.empty): ZJsonEncoder[A] =
      schema match {
        case Schema.Primitive(standardType, _)   => primitiveCodec(standardType).encoder
        case Schema.Sequence(schema, _, g, _, _) => ZJsonEncoder.chunk(schemaEncoder(schema, discriminatorTuple)).contramap(g)
        case Schema.Map(ks, vs, _)               => mapEncoder(ks, vs, discriminatorTuple)
        case Schema.Set(s, _) =>
          ZJsonEncoder.chunk(schemaEncoder(s, discriminatorTuple)).contramap(m => Chunk.fromIterable(m))
        case Schema.Transform(c, _, g, _, _)        => transformEncoder(c, g)
        case Schema.Tuple2(l, r, _)                 => ZJsonEncoder.tuple2(schemaEncoder(l, discriminatorTuple), schemaEncoder(r, discriminatorTuple))
        case Schema.Optional(schema, _)             => ZJsonEncoder.option(schemaEncoder(schema, discriminatorTuple))
        case Schema.Fail(_, _)                      => unitEncoder.contramap(_ => ())
        case Schema.GenericRecord(_, structure, _)  => recordEncoder(structure.toChunk)
        case Schema.Either(left, right, _)          => ZJsonEncoder.either(schemaEncoder(left, discriminatorTuple), schemaEncoder(right, discriminatorTuple))
        case l @ Schema.Lazy(_)                     => schemaEncoder(l.schema, discriminatorTuple)
        case Schema.CaseClass0(_, _, _)             => caseClassEncoder(schema, discriminatorTuple)
        case Schema.CaseClass1(_, f, _, _)          => caseClassEncoder(schema, discriminatorTuple, f)
        case Schema.CaseClass2(_, f1, f2, _, _)     => caseClassEncoder(schema, discriminatorTuple, f1, f2)
        case Schema.CaseClass3(_, f1, f2, f3, _, _) => caseClassEncoder(schema, discriminatorTuple, f1, f2, f3)
        case Schema.CaseClass4(_, f1, f2, f3, f4, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4)
        case Schema.CaseClass5(_, f1, f2, f3, f4, f5, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5)
        case Schema.CaseClass6(_, f1, f2, f3, f4, f5, f6, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6)
        case Schema.CaseClass7(_, f1, f2, f3, f4, f5, f6, f7, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7)
        case Schema.CaseClass8(_, f1, f2, f3, f4, f5, f6, f7, f8, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8)
        case Schema
              .CaseClass9(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9)
        case Schema.CaseClass10(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
        case Schema.CaseClass11(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
        case Schema.CaseClass12(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
        case Schema.CaseClass13(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
        case Schema.CaseClass14(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)
        case Schema.CaseClass15(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)
        case Schema.CaseClass16(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
        case Schema.CaseClass17(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)
        case Schema.CaseClass18(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)
        case Schema.CaseClass19(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, _, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19)
        case Schema.CaseClass20(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, _) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20)
        case Schema.CaseClass21(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, tail) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, tail._1)
        case Schema.CaseClass22(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, tail) =>
          caseClassEncoder(schema, discriminatorTuple, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, tail._1, tail._2)
        case e @ Schema.Enum1(_, c, _)                                  => enumEncoder(e, c)
        case e @ Schema.Enum2(_, c1, c2, _)                             => enumEncoder(e, c1, c2)
        case e @ Schema.Enum3(_, c1, c2, c3, _)                         => enumEncoder(e, c1, c2, c3)
        case e @ Schema.Enum4(_, c1, c2, c3, c4, _)                     => enumEncoder(e, c1, c2, c3, c4)
        case e @ Schema.Enum5(_, c1, c2, c3, c4, c5, _)                 => enumEncoder(e, c1, c2, c3, c4, c5)
        case e @ Schema.Enum6(_, c1, c2, c3, c4, c5, c6, _)             => enumEncoder(e, c1, c2, c3, c4, c5, c6)
        case e @ Schema.Enum7(_, c1, c2, c3, c4, c5, c6, c7, _)         => enumEncoder(e, c1, c2, c3, c4, c5, c6, c7)
        case e @ Schema.Enum8(_, c1, c2, c3, c4, c5, c6, c7, c8, _)     => enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8)
        case e @ Schema.Enum9(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, _) => enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9)
        case e @ Schema.Enum10(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
        case e @ Schema.Enum11(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
        case e @ Schema.Enum12(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
        case e @ Schema.Enum13(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
        case e @ Schema.Enum14(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
        case e @ Schema.Enum15(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
        case e @ Schema.Enum16(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
        case e @ Schema.Enum17(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
        case e @ Schema.Enum18(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
        case e @ Schema.Enum19(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
        case e @ Schema
              .Enum20(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
        case e @ Schema
              .Enum21(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
        case e @ Schema.Enum22(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _) =>
          enumEncoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
        case e @ Schema.EnumN(_, cs, _) => enumEncoder(e, cs.toSeq: _*)
        case d @ Schema.Dynamic(_)      => dynamicEncoder(d)
        case _ =>
          if (schema eq null)
            throw new Exception(s"A captured schema is null, most likely due to wrong field initialization order")
          else
            throw new Exception(s"Missing a handler for encoding of schema ${schema.toString()}.")
      }
    //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

    private[codec] def jsonFieldEncoder[A](schema: Schema[A]): Option[JsonFieldEncoder[A]] =
      schema match {
        case Schema.Primitive(StandardType.StringType, _) => Option(JsonFieldEncoder.string)
        case Schema.Primitive(StandardType.LongType, _)   => Option(JsonFieldEncoder.long)
        case Schema.Primitive(StandardType.IntType, _)    => Option(JsonFieldEncoder.int)
        case Schema.Lazy(inner)                           => jsonFieldEncoder(inner())
        case _                                            => None
      }

    private[codec] def mapEncoder[K, V](
      ks: Schema[K],
      vs: Schema[V],
      discriminatorTuple: DiscriminatorTuple
    ): ZJsonEncoder[Map[K, V]] = {
      val valueEncoder = JsonEncoder.schemaEncoder(vs)
      jsonFieldEncoder(ks) match {
        case Some(jsonFieldEncoder) =>
          ZJsonEncoder.keyValueIterable(jsonFieldEncoder, valueEncoder).contramap(a => Chunk.fromIterable(a).toMap)
        case None =>
          ZJsonEncoder
            .chunk(schemaEncoder(ks, discriminatorTuple).zip(schemaEncoder(vs, discriminatorTuple)))
            .contramap(m => Chunk.fromIterable(m))
      }
    }

    private def dynamicEncoder(schema: Schema.Dynamic): ZJsonEncoder[DynamicValue] = {
      val directMapping = schema.annotations.exists {
        case directDynamicMapping() => true
        case _                      => false
      }

      if (directMapping) {
        new ZJsonEncoder[DynamicValue] { directEncoder =>
          override def unsafeEncode(value: DynamicValue, indent: Option[Int], out: Write): Unit =
            value match {
              case DynamicValue.Record(_, values) =>
                if (values.isEmpty) {
                  out.write("{}")
                } else {
                  out.write('{')
                  val indent_ = bump(indent)
                  pad(indent_, out)
                  var first = true
                  values.foreach {
                    case (key, value) =>
                      if (first)
                        first = false
                      else {
                        out.write(',')
                        if (indent.isDefined)
                          ZJsonEncoder.pad(indent_, out)
                      }
                      string.encoder.unsafeEncode(JsonFieldEncoder.string.unsafeEncodeField(key), indent_, out)
                      if (indent.isEmpty) out.write(':')
                      else out.write(" : ")
                      directEncoder.unsafeEncode(value, indent_, out)
                  }
                  pad(indent, out)
                  out.write('}')
                }
              case DynamicValue.Enumeration(_, _) =>
                throw new Exception(s"DynamicValue.Enumeration is not supported in directDynamicMapping mode")
              case DynamicValue.Sequence(values) =>
                ZJsonEncoder.chunk(directEncoder).unsafeEncode(values, indent, out)
              case DynamicValue.Dictionary(_) =>
                throw new Exception(s"DynamicValue.Dictionary is not supported in directDynamicMapping mode")
              case DynamicValue.SetValue(values) =>
                ZJsonEncoder.set(directEncoder).unsafeEncode(values, indent, out)
              case DynamicValue.Primitive(value, standardType) =>
                primitiveCodec(standardType).encoder.unsafeEncode(value, indent, out)
              case DynamicValue.Singleton(_) =>
                out.write("{}")
              case DynamicValue.SomeValue(value) =>
                directEncoder.unsafeEncode(value, indent, out)
              case DynamicValue.NoneValue =>
                out.write("null")
              case DynamicValue.Tuple(_, _) =>
                throw new Exception(s"DynamicValue.Tuple is not supported in directDynamicMapping mode")
              case DynamicValue.LeftValue(_) =>
                throw new Exception(s"DynamicValue.LeftValue is not supported in directDynamicMapping mode")
              case DynamicValue.RightValue(_) =>
                throw new Exception(s"DynamicValue.RightValue is not supported in directDynamicMapping mode")
              case DynamicValue.DynamicAst(_) =>
                throw new Exception(s"DynamicValue.DynamicAst is not supported in directDynamicMapping mode")
              case DynamicValue.Error(message) =>
                throw new Exception(message)
            }
        }
      } else {
        schemaEncoder(DynamicValue.schema)
      }
    }

    private def transformEncoder[A, B](schema: Schema[A], g: B => Either[String, A]): ZJsonEncoder[B] =
      new ZJsonEncoder[B] {
        private lazy val innerEncoder = schemaEncoder(schema)

        override def unsafeEncode(b: B, indent: Option[Int], out: Write): Unit =
          g(b) match {
            case Left(_)  => ()
            case Right(a) => innerEncoder.unsafeEncode(a, indent, out)
          }

        override def isNothing(b: B): Boolean =
          g(b) match {
            case Left(_)  => false
            case Right(a) => innerEncoder.isNothing(a)
          }
      }

    private def enumEncoder[Z](parentSchema: Schema.Enum[Z], cases: Schema.Case[Z, _]*): ZJsonEncoder[Z] =
      // if all cases are CaseClass0, encode as a String
      if (cases.forall(_.schema.isInstanceOf[Schema.CaseClass0[_]])) {
        val caseMap: Map[Z, String] = cases
          .filterNot(_.annotations.exists(_.isInstanceOf[transientCase]))
          .map(
            case_ =>
              case_.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct() ->
                case_.annotations.collectFirst { case caseName(name) => name }.getOrElse(case_.id)
          )
          .toMap
        ZJsonEncoder.string.contramap(caseMap(_))
      } else { (value: Z, indent: Option[Int], out: Write) =>
        {
          val nonTransientCase =
            try cases.collectFirst {
              case c @ Schema.Case(_, _, _, _, _, annotations) if annotations.collectFirst {
                    case _: transientCase => ()
                  }.isEmpty && c.deconstructOption(value).isDefined =>
                c
            } catch {
              case ex: Throwable => throw new RuntimeException(s"Failed to encode enum type $parentSchema", ex)
            }

          nonTransientCase match {
            case Some(case_) =>
              val caseName = case_.annotations.collectFirst {
                case name: caseName => name.name
              }.getOrElse(case_.id)

              val discriminatorChunk = parentSchema.annotations.collect {
                case d: discriminatorName => (d, caseName)
              }
              val noDiscriminators = parentSchema.annotations.exists {
                case noDiscriminator() => true
                case _                 => false
              }

              if (discriminatorChunk.isEmpty && !noDiscriminators) out.write('{')
              val indent_ = bump(indent)
              pad(indent_, out)

              if (discriminatorChunk.isEmpty && !noDiscriminators) {
                string.encoder.unsafeEncode(JsonFieldEncoder.string.unsafeEncodeField(caseName), indent_, out)
                if (indent.isEmpty) out.write(':')
                else out.write(" : ")
              }

              schemaEncoder(
                case_.schema.asInstanceOf[Schema[Any]],
                discriminatorTuple = if (noDiscriminators) Chunk.empty else discriminatorChunk
              ).unsafeEncode(case_.deconstruct(value), indent, out)

              if (discriminatorChunk.isEmpty && !noDiscriminators) out.write('}')
            case None =>
              out.write("{}")
          }
        }
      }

    private def recordEncoder[Z](structure: Seq[Schema.Field[Z, _]]): ZJsonEncoder[ListMap[String, _]] = {
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
              case Schema.Field(k, a, _, _, _, _) =>
                val enc = schemaEncoder(a.asInstanceOf[Schema[Any]])
                if (first)
                  first = false
                else {
                  out.write(',')
                  if (indent.isDefined)
                    ZJsonEncoder.pad(indent_, out)
                }
                string.encoder.unsafeEncode(JsonFieldEncoder.string.unsafeEncodeField(k), indent_, out)
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

  object JsonDecoder {

    import Codecs._
    import ProductDecoder._

    final def decode[A](schema: Schema[A], json: String): Either[DecodeError, A] =
      schemaDecoder(schema).decodeJson(json) match {
        case Left(value)  => Left(ReadError(Cause.empty, value))
        case Right(value) => Right(value)
      }

    def x[A](dec: ZJsonDecoder[A]): Unit = dec match {
      case _: ZJsonDecoder[_] =>
    }

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[codec] def schemaDecoder[A](schema: Schema[A], hasDiscriminator: Boolean = false): ZJsonDecoder[A] = schema match {
      case Schema.Primitive(standardType, _)     => primitiveCodec(standardType).decoder
      case Schema.Optional(codec, _)             => ZJsonDecoder.option(schemaDecoder(codec, hasDiscriminator))
      case Schema.Tuple2(left, right, _)         => ZJsonDecoder.tuple2(schemaDecoder(left, hasDiscriminator), schemaDecoder(right, hasDiscriminator))
      case Schema.Transform(codec, f, _, _, _)   => schemaDecoder(codec, hasDiscriminator).mapOrFail(f)
      case Schema.Sequence(codec, f, _, _, _)    => ZJsonDecoder.chunk(schemaDecoder(codec, hasDiscriminator)).map(f)
      case Schema.Map(ks, vs, _)                 => mapDecoder(ks, vs, hasDiscriminator)
      case Schema.Set(s, _)                      => ZJsonDecoder.chunk(schemaDecoder(s, hasDiscriminator)).map(entries => entries.toSet)
      case Schema.Fail(message, _)               => failDecoder(message)
      case Schema.GenericRecord(_, structure, _) => recordDecoder(structure.toChunk)
      case Schema.Either(left, right, _)         => ZJsonDecoder.either(schemaDecoder(left, hasDiscriminator), schemaDecoder(right, hasDiscriminator))
      case l @ Schema.Lazy(_)                    => schemaDecoder(l.schema, hasDiscriminator)
      //case Schema.Meta(_, _)                                                                           => astDecoder
      case s @ Schema.CaseClass0(_, _, _)                                => caseClass0Decoder(s)
      case s @ Schema.CaseClass1(_, _, _, _)                             => caseClass1Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass2(_, _, _, _, _)                          => caseClass2Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass3(_, _, _, _, _, _)                       => caseClass3Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass4(_, _, _, _, _, _, _)                    => caseClass4Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass5(_, _, _, _, _, _, _, _)                 => caseClass5Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass6(_, _, _, _, _, _, _, _, _)              => caseClass6Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass7(_, _, _, _, _, _, _, _, _, _)           => caseClass7Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass8(_, _, _, _, _, _, _, _, _, _, _)        => caseClass8Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass9(_, _, _, _, _, _, _, _, _, _, _, _)     => caseClass9Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass10(_, _, _, _, _, _, _, _, _, _, _, _, _) => caseClass10Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass11(_, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass11Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass12(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass12Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass13(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass13Decoder(hasDiscriminator, s)
      case s @ Schema
            .CaseClass14(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass14Decoder(hasDiscriminator, s)
      case s @ Schema
            .CaseClass15(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass15Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass16(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass16Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass17(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass17Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass18(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass18Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass19(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass19Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass20(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass20Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass21(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass21Decoder(hasDiscriminator, s)
      case s @ Schema.CaseClass22(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass22Decoder(hasDiscriminator, s)
      case e @ Schema.Enum1(_, c, _)                                  => enumDecoder(e, c)
      case e @ Schema.Enum2(_, c1, c2, _)                             => enumDecoder(e, c1, c2)
      case e @ Schema.Enum3(_, c1, c2, c3, _)                         => enumDecoder(e, c1, c2, c3)
      case e @ Schema.Enum4(_, c1, c2, c3, c4, _)                     => enumDecoder(e, c1, c2, c3, c4)
      case e @ Schema.Enum5(_, c1, c2, c3, c4, c5, _)                 => enumDecoder(e, c1, c2, c3, c4, c5)
      case e @ Schema.Enum6(_, c1, c2, c3, c4, c5, c6, _)             => enumDecoder(e, c1, c2, c3, c4, c5, c6)
      case e @ Schema.Enum7(_, c1, c2, c3, c4, c5, c6, c7, _)         => enumDecoder(e, c1, c2, c3, c4, c5, c6, c7)
      case e @ Schema.Enum8(_, c1, c2, c3, c4, c5, c6, c7, c8, _)     => enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8)
      case e @ Schema.Enum9(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, _) => enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9)
      case e @ Schema.Enum10(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
      case e @ Schema.Enum11(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
      case e @ Schema.Enum12(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
      case e @ Schema.Enum13(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
      case e @ Schema.Enum14(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
      case e @ Schema.Enum15(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
      case e @ Schema.Enum16(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
      case e @ Schema.Enum17(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
      case e @ Schema.Enum18(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
      case e @ Schema.Enum19(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
      case e @ Schema
            .Enum20(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
      case e @ Schema.Enum21(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
      case e @ Schema.Enum22(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _) =>
        enumDecoder(e, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
      case e @ Schema.EnumN(_, cs, _) => enumDecoder(e, cs.toSeq: _*)
      case d @ Schema.Dynamic(_)      => dynamicDecoder(d)
      case _                          => throw new Exception(s"Missing a handler for decoding of schema ${schema.toString()}.")
    }
    //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

    private[codec] def mapDecoder[K, V](
      ks: Schema[K],
      vs: Schema[V],
      hasDiscriminator: Boolean
    ): ZJsonDecoder[Map[K, V]] = {
      val valueDecoder = JsonDecoder.schemaDecoder(vs)
      jsonFieldDecoder(ks) match {
        case Some(jsonFieldDecoder) =>
          ZJsonDecoder.keyValueChunk(jsonFieldDecoder, valueDecoder).map(a => Chunk.fromIterable(a).toMap)
        case None =>
          ZJsonDecoder
            .chunk(schemaDecoder(ks, hasDiscriminator).zip(schemaDecoder(vs, hasDiscriminator)))
            .map(_.toList.toMap)
      }
    }

    private[codec] def jsonFieldDecoder[A](schema: Schema[A]): Option[JsonFieldDecoder[A]] =
      schema match {
        case Schema.Primitive(StandardType.StringType, _) => Option(JsonFieldDecoder.string)
        case Schema.Primitive(StandardType.LongType, _)   => Option(JsonFieldDecoder.long)
        case Schema.Primitive(StandardType.IntType, _)    => Option(JsonFieldDecoder.int)
        case Schema.Lazy(inner)                           => jsonFieldDecoder(inner())
        case _                                            => None
      }

    private def dynamicDecoder(schema: Schema.Dynamic): ZJsonDecoder[DynamicValue] = {
      val directMapping = schema.annotations.exists {
        case directDynamicMapping() => true
        case _                      => false
      }

      if (directMapping) {
        Json.decoder.map(jsonToDynamicValue)
      } else {
        schemaDecoder(DynamicValue.schema)
      }
    }

    private def jsonToDynamicValue(json: Json): DynamicValue =
      json match {
        case Json.Obj(fields) =>
          DynamicValue.Record(
            TypeId.Structural,
            ListMap(fields.map { case (k, v) => k -> jsonToDynamicValue(v) }: _*)
          )
        case Json.Arr(elements) => DynamicValue.Sequence(elements.map(jsonToDynamicValue))
        case Json.Bool(value)   => DynamicValue.Primitive(value, StandardType.BoolType)
        case Json.Str(value)    => DynamicValue.Primitive(value, StandardType.StringType)
        case Json.Num(value)    => DynamicValue.Primitive(value, StandardType.BigDecimalType)
        case Json.Null          => DynamicValue.NoneValue
      }

    private def enumDecoder[Z](parentSchema: Schema.Enum[Z], cases: Schema.Case[Z, _]*): ZJsonDecoder[Z] = {
      val caseNameAliases = cases.flatMap {
        case Schema.Case(name, _, _, _, _, annotations) =>
          annotations.flatMap {
            case a: caseNameAliases => a.aliases.toList.map(_ -> name)
            case cn: caseName       => List(cn.name -> name)
            case _                  => Nil
          }
      }.toMap

      // if all cases are CaseClass0, decode as String
      if (cases.forall(_.schema.isInstanceOf[Schema.CaseClass0[_]])) {
        val caseMap: Map[String, Z] =
          cases.map(case_ => case_.id -> case_.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct()).toMap
        ZJsonDecoder.string.mapOrFail(
          s =>
            caseMap.get(caseNameAliases.get(s).getOrElse(s)) match {
              case Some(z) => Right(z)
              case None    => Left("unrecognized string")
            }
        )
      } else {

        val noDiscriminators = parentSchema.annotations.exists {
          case noDiscriminator() => true
          case _                 => false
        }

        (trace: List[JsonError], in: RetractReader) => {
          if (noDiscriminators) {
            val rr                = RecordingReader(in)
            val it                = cases.iterator
            var result: Option[Z] = None

            while (result.isEmpty && it.hasNext) {
              val c = it.next()
              try {
                val decoded = schemaDecoder(c.schema).unsafeDecode(trace, rr).asInstanceOf[Z]
                result = Some(decoded)
              } catch {
                case _: Exception =>
                  rr.rewind()
              }
            }

            result match {
              case Some(value) => value
              case None        => throw UnsafeJson(JsonError.Message("none of the subtypes could decode the data") :: trace)
            }

          } else {
            Lexer.char(trace, in, '{')

            if (Lexer.firstField(trace, in)) {
              val subtypeOrDiscriminator = deAliasCaseName(Lexer.string(trace, in).toString, caseNameAliases)
              val (subtype, trace_, hasDiscriminator) = parentSchema.annotations.collect {
                case d: discriminatorName if d.tag == subtypeOrDiscriminator =>
                  val trace_ = JsonError.ObjectAccess(subtypeOrDiscriminator) :: trace
                  Lexer.char(trace_, in, ':')
                  val discriminator = Lexer.string(trace, in).toString
                  // Perform a second de-aliasing because the first one would resolve the discriminator key instead.
                  val innerSubtype = deAliasCaseName(discriminator, caseNameAliases)
                  (innerSubtype, JsonError.ObjectAccess(innerSubtype) :: trace_, true)
              }.headOption.getOrElse {
                val trace_ = JsonError.ObjectAccess(subtypeOrDiscriminator) :: trace
                Lexer.char(trace_, in, ':')
                (subtypeOrDiscriminator, trace_, false)
              }
              cases.find(_.id == subtype) match {
                case Some(c) =>
                  val decoded = schemaDecoder(c.schema, hasDiscriminator).unsafeDecode(trace_, in).asInstanceOf[Z]
                  if (!hasDiscriminator) Lexer.nextField(trace, in)
                  decoded
                case None =>
                  throw UnsafeJson(JsonError.Message("unrecognized subtype") :: trace_)
              }
            } else {
              throw UnsafeJson(JsonError.Message("missing subtype") :: trace)
            }
          }
        }
      }
    }

    private def deAliasCaseName(alias: String, caseNameAliases: Map[String, String]): String =
      caseNameAliases.getOrElse(alias, alias)

    private def recordDecoder[Z](structure: Seq[Schema.Field[Z, _]]): ZJsonDecoder[ListMap[String, Any]] = {
      (trace: List[JsonError], in: RetractReader) =>
        {
          val builder: ChunkBuilder[(String, Any)] = zio.ChunkBuilder.make[(String, Any)](structure.size)
          Lexer.char(trace, in, '{')
          if (Lexer.firstField(trace, in)) {
            while ({
              val field = Lexer.string(trace, in).toString
              structure.find(_.name == field) match {
                case Some(Schema.Field(label, schema, _, _, _, _)) =>
                  val trace_ = JsonError.ObjectAccess(label) :: trace
                  Lexer.char(trace_, in, ':')
                  val value = schemaDecoder(schema).unsafeDecode(trace_, in)
                  builder += ((JsonFieldDecoder.string.unsafeDecodeField(trace_, label), value))
                case None =>
                  Lexer.char(trace, in, ':')
                  Lexer.skipValue(trace, in)

              }
              (Lexer.nextField(trace, in))
            }) {
              ()
            }
          }
          (ListMap.newBuilder[String, Any] ++= builder.result()).result()
        }
    }

  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private[codec] object ProductEncoder {
    import ZJsonEncoder.{ bump, pad }

    private[codec] def caseClassEncoder[Z](parentSchema: Schema[_], discriminatorTuple: DiscriminatorTuple, fields: Schema.Field[Z, _]*): ZJsonEncoder[Z] = { (a: Z, indent: Option[Int], out: Write) =>
      {
        out.write('{')
        val indent_ = bump(indent)
        pad(indent_, out)
        var first = true
        discriminatorTuple.foreach { discriminator =>
          val (tag, caseTpeName) = discriminator
          first = false
          string.encoder.unsafeEncode(JsonFieldEncoder.string.unsafeEncodeField(tag.tag), indent_, out)
          if (indent.isEmpty) out.write(':')
          else out.write(" : ")
          string.encoder.unsafeEncode(JsonFieldEncoder.string.unsafeEncodeField(caseTpeName), indent_, out)
        }
        val nonTransientFields = fields.filter {
          case Schema.Field(_, _, annotations, _, _, _) if annotations.collectFirst { case a: transientField => a }.isDefined => false
          case _ => true
        }
        nonTransientFields.foreach {
          case s: Schema.Field[Z, _] =>
            val enc =
              try {
                JsonEncoder.schemaEncoder(s.schema)
              } catch {
                case e: Throwable => throw new RuntimeException(s"Failed to encode field '${s.name}' in $parentSchema'", e)
              }
            if (!enc.isNothing(s.get(a))) {
              if (first)
                first = false
              else {
                out.write(',')
                if (indent.isDefined)
                  ZJsonEncoder.pad(indent_, out)
              }

              string.encoder.unsafeEncode(JsonFieldEncoder.string.unsafeEncodeField(s.name), indent_, out)
              if (indent.isEmpty) out.write(':')
              else out.write(" : ")
              enc.unsafeEncode(s.get(a), indent_, out)
            }
        }
        pad(indent, out)
        out.write('}')
      }
    }
  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private[codec] object ProductDecoder {
    import zio.schema.codec.JsonCodec.JsonDecoder.schemaDecoder

    private[codec] def caseClass0Decoder[Z](schema: Schema.CaseClass0[Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      val _ = Codecs.unitDecoder.unsafeDecode(trace, in)
      schema.defaultConstruct()
    }

    private[codec] def caseClass1Decoder[A, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass1[A, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field)
        schema.defaultConstruct(buffer(0).asInstanceOf[A])
      }
    }

    private[codec] def caseClass2Decoder[A1, A2, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass2[A1, A2, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2])
      }
    }

    private[codec] def caseClass3Decoder[A1, A2, A3, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass3[A1, A2, A3, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3])
      }
    }

    private[codec] def caseClass4Decoder[A1, A2, A3, A4, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass4[A1, A2, A3, A4, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] =
          unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4])
      }
    }

    private[codec] def caseClass5Decoder[A1, A2, A3, A4, A5, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] =
          unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5])
      }
    }

    private[codec] def caseClass6Decoder[A1, A2, A3, A4, A5, A6, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6])
      }
    }

    private[codec] def caseClass7Decoder[A1, A2, A3, A4, A5, A6, A7, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7])
      }
    }

    private[codec] def caseClass8Decoder[A1, A2, A3, A4, A5, A6, A7, A8, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8])
      }
    }

    private[codec] def caseClass9Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9])
      }
    }

    private[codec] def caseClass10Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10])
      }
    }

    private[codec] def caseClass11Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11])
      }
    }

    private[codec] def caseClass12Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12])
      }
    }

    private[codec] def caseClass13Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12], buffer(12).asInstanceOf[A13])
      }
    }

    private[codec] def caseClass14Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14)
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

    private[codec] def caseClass15Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15)
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

    private[codec] def caseClass16Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16)
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

    private[codec] def caseClass17Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17)
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

    private[codec] def caseClass18Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18)
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

    private[codec] def caseClass19Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19)
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

    private[codec] def caseClass20Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] = unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20)
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

    private[codec] def caseClass21Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] =
          unsafeDecodeFields(hasDiscriminator, trace, in, schema, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20, schema.field21)
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

    private[codec] def caseClass22Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](hasDiscriminator: Boolean, schema: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z]): ZJsonDecoder[Z] = { (trace: List[JsonError], in: RetractReader) =>
      {
        val buffer: Array[Any] =
          unsafeDecodeFields(
            hasDiscriminator,
            trace,
            in,
            schema,
            schema.field1,
            schema.field2,
            schema.field3,
            schema.field4,
            schema.field5,
            schema.field6,
            schema.field7,
            schema.field8,
            schema.field9,
            schema.field10,
            schema.field11,
            schema.field12,
            schema.field13,
            schema.field14,
            schema.field15,
            schema.field16,
            schema.field17,
            schema.field18,
            schema.field19,
            schema.field20,
            schema.field21,
            schema.field22
          )
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

    private def unsafeDecodeFields[Z](hasDiscriminator: Boolean, trace: List[JsonError], in: RetractReader, caseClassSchema: Schema[Z], fields: Schema.Field[Z, _]*): Array[Any] = {
      val len: Int                  = fields.length
      val buffer                    = Array.ofDim[Any](len)
      val fieldNames                = fields.map(_.name.asInstanceOf[String]).toArray
      val spans: Array[JsonError]   = fields.map(_.name.asInstanceOf[String]).toArray.map(JsonError.ObjectAccess(_))
      val schemas: Array[Schema[_]] = fields.map(_.schema).toArray
      val fieldAliases = fields.flatMap {
        case Schema.Field(name, _, annotations, _, _, _) =>
          val aliases = annotations.collectFirst { case a: fieldNameAliases => a.aliases }.getOrElse(Nil)
          aliases.map(_ -> fieldNames.indexOf(name)) :+ (name -> fieldNames.indexOf(name))
      }.toMap
      val aliasesMatrix     = fieldAliases.keys.toArray ++ fieldNames
      val rejectExtraFields = caseClassSchema.annotations.collectFirst({ case _: rejectExtraFields => () }).isDefined

      val hasMore = if (!hasDiscriminator) {
        Lexer.char(trace, in, '{')
        true
      } else Lexer.nextField(trace, in)
      if (hasMore && Lexer.firstField(trace, in)) {
        while ({
          var trace_   = trace
          val fieldRaw = Lexer.field(trace, in, new StringMatrix(aliasesMatrix))
          val field    = if (fieldRaw != -1) fieldAliases.getOrElse(aliasesMatrix(fieldRaw), -1) else -1
          if (field != -1) {
            trace_ = spans(field) :: trace
            if (buffer(field) != null)
              throw UnsafeJson(JsonError.Message("duplicate") :: trace)
            else
              buffer(field) = schemaDecoder(schemas(field)).unsafeDecode(trace_, in)
          } else {
            if (rejectExtraFields)
              throw UnsafeJson(JsonError.Message("extra field") :: trace)
            else
              Lexer.skipValue(trace_, in)
          }
          (Lexer.nextField(trace, in))
        }) { () }
      }

      var i = 0
      while (i < len) {
        if (buffer(i) == null) {
          val optionalAnnotation          = fields(i).annotations.collectFirst { case a: optionalField        => a }
          val transientFieldAnnotation    = fields(i).annotations.collectFirst { case a: transientField       => a }
          val fieldDefaultValueAnnotation = fields(i).annotations.collectFirst { case a: fieldDefaultValue[_] => a }

          if (optionalAnnotation.isDefined || transientFieldAnnotation.isDefined)
            buffer(i) = schemas(i).defaultValue.toOption.get
          else if (fieldDefaultValueAnnotation.isDefined)
            buffer(i) = fieldDefaultValueAnnotation.get.value
          else
            buffer(i) = schemaDecoder(schemas(i)).unsafeDecodeMissing(spans(i) :: trace)

        }
        i += 1
      }
      buffer
    }
  }

}
