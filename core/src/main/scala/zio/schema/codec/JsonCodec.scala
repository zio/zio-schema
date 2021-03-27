package zio.schema.codec

import java.nio.CharBuffer
import java.nio.charset.StandardCharsets

import zio.json.JsonCodec._
import zio.json.JsonDecoder.{ JsonError, UnsafeJson }
import zio.json.internal.{ Lexer, RetractReader, Write }
import zio.json.{ JsonCodec => ZJsonCodec, JsonDecoder, JsonEncoder, JsonFieldDecoder, JsonFieldEncoder }
import zio.schema.{ StandardType, _ }
import zio.stream.ZTransducer
import zio.{ Chunk, ChunkBuilder, ZIO }

object JsonCodec extends Codec {

  override def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte] =
    ZTransducer.fromPush(
      (opt: Option[Chunk[A]]) =>
        ZIO.succeed(opt.map(values => values.flatMap(Encoder.encode(schema, _))).getOrElse(Chunk.empty))
    )

  override def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A] = schema match {
    case Schema.Primitive(StandardType.UnitType) =>
      ZTransducer.fromPush(_ => ZIO.succeed(Chunk.unit))
    case _ =>
      ZTransducer.utfDecode >>> ZTransducer.fromFunctionM(
        (s: String) => ZIO.fromEither(Decoder.decode(schema, s))
      )
  }

  override def encode[A](schema: Schema[A]): A => Chunk[Byte] = Encoder.encode(schema, _)

  override def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A] =
    (chunk: Chunk[Byte]) => Decoder.decode(schema, new String(chunk.toArray, Encoder.CHARSET))

  object Codecs {
    protected[codec] val unitEncoder: JsonEncoder[Unit] = new JsonEncoder[Unit] {
      override def unsafeEncode(a: Unit, indent: Option[Int], out: Write): Unit = ()
      override def isNothing(a: Unit): Boolean                                  = true
    }
    protected[codec] val unitDecoder: JsonDecoder[Unit] =
      (_: List[JsonDecoder.JsonError], _: RetractReader) => ()

    protected[codec] val unitCodec: ZJsonCodec[Unit] = ZJsonCodec(unitEncoder, unitDecoder)

    protected[codec] def failDecoder[A](message: String): JsonDecoder[A] =
      (trace: List[JsonDecoder.JsonError], _: RetractReader) => throw UnsafeJson(JsonError.Message(message) :: trace)

    private[codec] def primitiveCodec[A](standardType: StandardType[A]): ZJsonCodec[A] =
      standardType match {
        case StandardType.UnitType          => unitCodec
        case StandardType.StringType        => ZJsonCodec.string
        case StandardType.BoolType          => ZJsonCodec.boolean
        case StandardType.ShortType         => ZJsonCodec.short
        case StandardType.IntType           => ZJsonCodec.int
        case StandardType.LongType          => ZJsonCodec.long
        case StandardType.FloatType         => ZJsonCodec.float
        case StandardType.DoubleType        => ZJsonCodec.double
        case StandardType.BinaryType        => ZJsonCodec.chunk(ZJsonCodec.byte)
        case StandardType.CharType          => ZJsonCodec.char
        case StandardType.BigIntegerType    => ZJsonCodec.bigInteger
        case StandardType.BigDecimalType    => ZJsonCodec.bigDecimal
        case StandardType.DayOfWeekType     => ZJsonCodec.dayOfWeek // ZJsonCodec[java.time.DayOfWeek]
        case StandardType.Duration(_)       => ZJsonCodec.duration //ZJsonCodec[java.time.Duration]
        case StandardType.Instant(_)        => ZJsonCodec.instant //ZJsonCodec[java.time.Instant]
        case StandardType.LocalDate(_)      => ZJsonCodec.localDate //ZJsonCodec[java.time.LocalDate]
        case StandardType.LocalDateTime(_)  => ZJsonCodec.localDateTime //ZJsonCodec[java.time.LocalDateTime]
        case StandardType.LocalTime(_)      => ZJsonCodec.localTime //ZJsonCodec[java.time.LocalTime]
        case StandardType.Month             => ZJsonCodec.month //ZJsonCodec[java.time.Month]
        case StandardType.MonthDay          => ZJsonCodec.monthDay //ZJsonCodec[java.time.MonthDay]
        case StandardType.OffsetDateTime(_) => ZJsonCodec.offsetDateTime //ZJsonCodec[java.time.OffsetDateTime]
        case StandardType.OffsetTime(_)     => ZJsonCodec.offsetTime //ZJsonCodec[java.time.OffsetTime]
        case StandardType.Period            => ZJsonCodec.period //ZJsonCodec[java.time.Period]
        case StandardType.Year              => ZJsonCodec.year //ZJsonCodec[java.time.Year]
        case StandardType.YearMonth         => ZJsonCodec.yearMonth //ZJsonCodec[java.time.YearMonth]
        case StandardType.ZonedDateTime(_)  => ZJsonCodec.zonedDateTime //ZJsonCodec[java.time.ZonedDateTime]
        case StandardType.ZoneId            => ZJsonCodec.zoneId //ZJsonCodec[java.time.ZoneId]
        case StandardType.ZoneOffset        => ZJsonCodec.zoneOffset //ZJsonCodec[java.time.ZoneOffset]
      }
  }

  object Encoder {
    import Codecs._
    import JsonEncoder.{ bump, pad }

    private[codec] val CHARSET = StandardCharsets.UTF_8

    final def encode[A](schema: Schema[A], value: A): Chunk[Byte] =
      charSequenceToByteChunk(schemaEncoder(schema, value).encodeJson(value, None))

    private[codec] def charSequenceToByteChunk(chars: CharSequence): Chunk[Byte] = {
      val bytes = CHARSET.newEncoder().encode(CharBuffer.wrap(chars))
      Chunk.fromByteBuffer(bytes)
    }

    private def schemaEncoder[A](schema: Schema[A], value: A): JsonEncoder[A] = schema match {
      case Schema.Primitive(standardType) => primitiveCodec(standardType)
      case Schema.Sequence(schema)        => JsonEncoder.chunk(schemaEncoder(schema, value))
      case Schema.Transform(c, _, g)      => transformEncoder(c, value, g)
      case s @ Schema.Tuple(_, _)         => tupleEncoder(s, value)
      case s @ Schema.Optional(_)         => optionEncoder(s, value)
      case Schema.Fail(_)                 => unitEncoder.contramap(_ => ())
      case Schema.Record(structure)       => recordEncoder(structure, value)
      case Schema.Enumeration(structure)  => enumEncoder(structure, value)
    }

    private def tupleEncoder[A, B](schema: Schema.Tuple[A, B], value: (A, B)): JsonEncoder[(A, B)] =
      schemaEncoder(schema.left, value._1).both(schemaEncoder(schema.right, value._2))

    private def optionEncoder[A](schema: Schema.Optional[A], value: Option[A]): JsonEncoder[Option[A]] = value match {
      case Some(v) => JsonEncoder.option(schemaEncoder(schema.codec, v))
      case None =>
        (_: Option[A], _: Option[Int], out: Write) => out.write("null")
    }

    private def transformEncoder[A, B](schema: Schema[A], value: B, g: B => Either[String, A]): JsonEncoder[B] = {
      (_: B, indent: Option[Int], out: Write) =>
        g(value) match {
          case Left(_)  => ()
          case Right(a) => schemaEncoder(schema, a).unsafeEncode(a, indent, out)
        }
    }

    private def recordEncoder(structure: Map[String, Schema[_]], value: Map[String, _]): JsonEncoder[Map[String, _]] = {
      (_: Map[String, _], indent: Option[Int], out: Write) =>
        {
          if (structure.isEmpty) {
            out.write("{}")
          } else {
            out.write('{')
            val indent_ = bump(indent)
            pad(indent_, out)
            var first = true
            structure.foreach {
              case (k, a) =>
                val enc = schemaEncoder(a.asInstanceOf[Schema[Any]], value(k))
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

    private def enumEncoder(structure: Map[String, Schema[_]], value: Map[String, _]): JsonEncoder[Map[String, _]] = {
      (a: Map[String, _], indent: Option[Int], out: Write) =>
        {
          if (structure.isEmpty) {
            out.write("{}")
          } else {
            out.write('{')
            val indent_ = bump(indent)
            pad(indent_, out)
            var first  = true
            val (k, v) = a.toSeq.head
            val enc    = schemaEncoder(structure(k).asInstanceOf[Schema[Any]], value(k))
            if (first)
              first = false
            else {
              out.write(',')
              if (indent.isDefined)
                pad(indent_, out)
            }

            string.unsafeEncode(JsonFieldEncoder.string.unsafeEncodeField(k), indent_, out)
            if (indent.isEmpty) out.write(':')
            else out.write(" : ")
            enc.unsafeEncode(v, indent_, out)
            pad(indent, out)
            out.write('}')
          }
        }
    }
  }

  object Decoder {
    import Codecs._
    final def decode[A](schema: Schema[A], json: String): Either[String, A] =
      schemaDecoder(schema).decodeJson(json)

    private def schemaDecoder[A](schema: Schema[A]): JsonDecoder[A] = schema match {
      case Schema.Primitive(standardType) => primitiveCodec(standardType)
      case Schema.Optional(codec)         => JsonDecoder.option(schemaDecoder(codec))
      case Schema.Tuple(left, right)      => JsonDecoder.tuple2(schemaDecoder(left), schemaDecoder(right))
      case Schema.Transform(codec, f, _)  => schemaDecoder(codec).mapOrFail(f)
      case Schema.Sequence(codec)         => JsonDecoder.chunk(schemaDecoder(codec))
      case Schema.Fail(message)           => failDecoder(message)
      case Schema.Record(structure)       => recordDecoder(structure)
      case Schema.Enumeration(structure)  => enumDecoder(structure)
    }

    private def recordDecoder(structure: Map[String, Schema[_]]): JsonDecoder[Map[String, Any]] = {
      (trace: List[JsonError], in: RetractReader) =>
        {
          val builder: ChunkBuilder[(String, Any)] = zio.ChunkBuilder.make[(String, Any)](structure.size)
          Lexer.char(trace, in, '{')
          if (Lexer.firstField(trace, in))
            do {
              val field  = Lexer.string(trace, in).toString
              val trace_ = JsonError.ObjectAccess(field) :: trace
              Lexer.char(trace_, in, ':')
              val value = schemaDecoder(structure(field)).unsafeDecode(trace_, in)
              builder += ((JsonFieldDecoder.string.unsafeDecodeField(trace_, field), value))
            } while (Lexer.nextField(trace, in))
          builder.result().toMap
        }
    }

    private def enumDecoder(structure: Map[String, Schema[_]]): JsonDecoder[Map[String, Any]] = {
      (trace: List[JsonError], in: RetractReader) =>
        {
          val builder: ChunkBuilder[(String, Any)] = zio.ChunkBuilder.make[(String, Any)](structure.size)
          Lexer.char(trace, in, '{')
          if (Lexer.firstField(trace, in)) {
            val field  = Lexer.string(trace, in).toString
            val trace_ = JsonError.ObjectAccess(field) :: trace
            Lexer.char(trace_, in, ':')
            val value = schemaDecoder(structure(field)).unsafeDecode(trace_, in)
            builder += ((JsonFieldDecoder.string.unsafeDecodeField(trace_, field), value))
          }
          builder.result().toMap
        }
    }
  }
}
