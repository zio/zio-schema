package zio.schema.codec

import java.time.DayOfWeek
import zio.json.JsonEncoder.string
import zio.json.{ JsonDecoder, JsonEncoder }
import zio.schema.{ StandardType, _ }
import zio.stream.ZTransducer
import zio.{ Chunk, ZIO }

object JsonCodec extends Codec {
  override def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte] = schema match {
    case Schema.Record(_)               => ???
    case Schema.Sequence(element)       => encoder(element)
    case Schema.Enumeration(_)          => ???
    case Schema.Transform(codec, _, g)      => encoder(codec).contramapM(a => ZIO.fromEither(g(a)))
    case Schema.Primitive(standardType) => Encoder.primitiveEncoder(standardType)
    case Schema.Tuple(left, right)      => encoder(left) >>> encoder(right)
    case Schema.Optional(_)             => encoder(schema)
  }

  override def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A] = schema match {
    case Schema.Record(_)               => ???
    case Schema.Sequence(_)             => ???
    case Schema.Enumeration(_)          => ???
    case Schema.Transform(_, _, _)      => ???
    case Schema.Primitive(standardType) => Decoder.primitiveDecoder(standardType)
    case Schema.Tuple(_, _)             => ???
    case Schema.Optional(_)             => decoder(schema)
  }

  object Encoder {
    final def primitiveEncoder[A](standardType: StandardType[A]): ZTransducer[Any, Nothing, A, Byte] =
      standardType match {
        case StandardType.UnitType          => unitEncoder
        case StandardType.StringType        => standardEncoder[String]
        case StandardType.BoolType          => standardEncoder[Boolean]
        case StandardType.ShortType         => standardEncoder[Short]
        case StandardType.IntType           => standardEncoder[Int]
        case StandardType.LongType          => standardEncoder[Long]
        case StandardType.FloatType         => standardEncoder[Float]
        case StandardType.DoubleType        => standardEncoder[Double]
        case StandardType.BinaryType        => standardEncoder[Chunk[Byte]]
        case StandardType.CharType          => standardEncoder[Char]
        case StandardType.DayOfWeekType     => standardEncoder[java.time.DayOfWeek]
        case StandardType.Duration(_)       => standardEncoder[java.time.Duration]
        case StandardType.Instant(_)        => standardEncoder[java.time.Instant]
        case StandardType.LocalDate(_)      => standardEncoder[java.time.LocalDate]
        case StandardType.LocalDateTime(_)  => standardEncoder[java.time.LocalDateTime]
        case StandardType.LocalTime(_)      => standardEncoder[java.time.LocalTime]
        case StandardType.Month             => standardEncoder[java.time.Month]
        case StandardType.MonthDay          => standardEncoder[java.time.MonthDay]
        case StandardType.OffsetDateTime(_) => standardEncoder[java.time.OffsetDateTime]
        case StandardType.OffsetTime(_)     => standardEncoder[java.time.OffsetTime]
        case StandardType.Period            => standardEncoder[java.time.Period]
        case StandardType.Year              => standardEncoder[java.time.Year]
        case StandardType.YearMonth         => standardEncoder[java.time.YearMonth]
        case StandardType.ZonedDateTime(_)  => standardEncoder[java.time.ZonedDateTime]
        case StandardType.ZoneId            => standardEncoder[java.time.ZoneId]
        case StandardType.ZoneOffset        => standardEncoder[java.time.ZoneOffset]
      }

    private val unitEncoder: ZTransducer[Any, Nothing, Unit, Nothing] =
      ZTransducer.fromPush(_ => ZIO.succeed(Chunk.empty))

    private def standardEncoder[A](implicit enc: JsonEncoder[A]): ZTransducer[Any, Nothing, A, Byte] =
      ZTransducer
        .fromFunction[A, Chunk[Byte]] { s =>
          charSequenceToByteChunk(enc.encodeJson(s, None))
        }
        .mapChunks(_.flatten)

    private def charSequenceToByteChunk(chars: CharSequence): Chunk[Byte] = {
      val bytes: Seq[Byte] = for (i <- 1 to chars.length) yield chars.charAt(i).toByte
      Chunk.fromIterable(bytes)
    }
  }

  object Decoder {
    final def primitiveDecoder[A](standardType: StandardType[A]): ZTransducer[Any, String, Byte, A] =
      standardType match {
        case StandardType.UnitType          => unitDecoder
        case StandardType.StringType        => standardDecoder[String]
        case StandardType.BoolType          => standardDecoder[Boolean]
        case StandardType.ShortType         => standardDecoder[Short]
        case StandardType.IntType           => standardDecoder[Int]
        case StandardType.LongType          => standardDecoder[Long]
        case StandardType.FloatType         => standardDecoder[Float]
        case StandardType.DoubleType        => standardDecoder[Double]
        case StandardType.BinaryType        => standardDecoder[Chunk[Byte]]
        case StandardType.CharType          => standardDecoder[Char]
        case StandardType.DayOfWeekType     => standardDecoder[DayOfWeek]
        case StandardType.Duration(_)       => standardDecoder[java.time.Duration]
        case StandardType.Instant(_)        => standardDecoder[java.time.Instant]
        case StandardType.LocalDate(_)      => standardDecoder[java.time.LocalDate]
        case StandardType.LocalDateTime(_)  => standardDecoder[java.time.LocalDateTime]
        case StandardType.LocalTime(_)      => standardDecoder[java.time.LocalTime]
        case StandardType.Month             => standardDecoder[java.time.Month]
        case StandardType.MonthDay          => standardDecoder[java.time.MonthDay]
        case StandardType.OffsetDateTime(_) => standardDecoder[java.time.OffsetDateTime]
        case StandardType.OffsetTime(_)     => standardDecoder[java.time.OffsetTime]
        case StandardType.Period            => standardDecoder[java.time.Period]
        case StandardType.Year              => standardDecoder[java.time.Year]
        case StandardType.YearMonth         => standardDecoder[java.time.YearMonth]
        case StandardType.ZonedDateTime(_)  => standardDecoder[java.time.ZonedDateTime]
        case StandardType.ZoneId            => standardDecoder[java.time.ZoneId]
        case StandardType.ZoneOffset        => standardDecoder[java.time.ZoneOffset]
      }

    private val unitDecoder: ZTransducer[Any, String, Byte, Unit] =
      ZTransducer.fromPush(_ => ZIO.succeed(Chunk.empty))

    private def standardDecoder[A](implicit dec: JsonDecoder[A]): ZTransducer[Any, String, Byte, A] =
      ZTransducer.utfDecode.mapChunks(_.flatMap(s => dec.decodeJson(s).toOption))
  }
}
