package zio.schema.codec

import java.math.{BigInteger, MathContext}
import java.time._

import zio.schema.codec.DecodeError.ReadError
import zio.schema.{Schema, StandardType}
import zio.stream.ZPipeline
import zio.{Cause, Chunk}

/**
 * Reads and writes values in the MessagePack binary format, driven by a
 * `Schema`. A record is written as a MessagePack map from field names to
 * values.
 *
 * Use it when you need MessagePack's binary encoding for values that have a
 * `Schema`.
 *
 * {{{
 * import zio.schema.{DeriveSchema, Schema}
 * import zio.schema.codec.{BinaryCodec, MessagePackCodec}
 *
 * final case class Point(x: Int, y: Int)
 * implicit val pointSchema: Schema[Point] = DeriveSchema.gen[Point]
 *
 * val codec: BinaryCodec[Point] = MessagePackCodec.messagePackCodec[Point]
 * val bytes = codec.encode(Point(3, 4))
 * bytes.size
 * // 7
 * codec.decode(bytes)
 * // Right(Point(3,4))
 * }}}
 *
 * @see
 *   [[https://zio.dev/zio-schema/derivations/codecs/message-pack/ MessagePack Codec]]
 */
object MessagePackCodec {

  /**
   * Returns a `BinaryCodec` that reads and writes `A` in MessagePack, using the
   * implicit `Schema[A]`.
   *
   * `decode` fails with `DecodeError.ReadError` (`No bytes to decode`) on an
   * empty chunk. Decoding a record fails when the map holds a different number
   * of fields than the schema, for example
   * `Different than expected number of fields. Expected 3 but received 1`, so a
   * record written with fewer or more fields does not decode.
   *
   * `streamDecoder` decodes each chunk it receives as one value, so a chunk
   * that holds two encoded values yields the first one. `streamEncoder` reuses
   * one encoder for the whole stream, and the bytes it writes for each value
   * start with the bytes of every value encoded before it: encoding
   * `Point(1, 2)` then `Point(3, 4)` writes `Point(1, 2)` twice, then
   * `Point(3, 4)`.
   */
  implicit def messagePackCodec[A](implicit schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {
      override def encode(a: A): Chunk[Byte] =
        new MessagePackEncoder().encode(schema, a)

      override def decode(bytes: Chunk[Byte]): Either[DecodeError, A] =
        if (bytes.isEmpty)
          Left(ReadError(Cause.empty, "No bytes to decode"))
        else
          decodeChunk(bytes)

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] = {
        val encoder = new MessagePackEncoder()
        ZPipeline.mapChunks { chunk =>
          chunk.flatMap(encoder.encode(schema, _))
        }
      }

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
        ZPipeline.mapChunksEither(bytes => decodeChunk(bytes).map(Chunk.single))

      private def decodeChunk(chunk: Chunk[Byte]): Either[DecodeError, A] =
        new MessagePackDecoder(chunk)
          .decode(schema)
          .left
          .map(identity)
    }

  // TODO those are duplicates from ThriftCodec
  /**
   * The record fields a `java.math.BigDecimal` is written as in MessagePack:
   * `unscaled`, `precision` and `scale`.
   */
  val bigDecimalStructure: Seq[Schema.Field[java.math.BigDecimal, _]] =
    Seq(
      Schema.Field(
        "unscaled",
        Schema.Primitive(StandardType.BigIntegerType),
        get0 = _.unscaledValue(),
        set0 = (a, b: BigInteger) => new java.math.BigDecimal(b, a.scale)
      ),
      Schema.Field(
        "precision",
        Schema.Primitive(StandardType.IntType),
        get0 = _.precision(),
        set0 = (a, b: Int) => new java.math.BigDecimal(a.unscaledValue, new MathContext(b))
      ),
      Schema
        .Field("scale", Schema.Primitive(StandardType.IntType), get0 = _.scale(), set0 = (a, b: Int) => a.setScale(b))
    )

  /**
   * The record fields a `java.time.MonthDay` is written as in MessagePack:
   * `month` and `day`.
   */
  val monthDayStructure: Seq[Schema.Field[MonthDay, Int]] =
    Seq(
      Schema.Field(
        "month",
        Schema.Primitive(StandardType.IntType),
        get0 = (v: MonthDay) => v.getMonthValue,
        set0 = (a, b: Int) => a.`with`(Month.of(b))
      ),
      Schema
        .Field(
          "day",
          Schema.Primitive(StandardType.IntType),
          get0 = _.getDayOfMonth,
          set0 = (a, b: Int) => a.withDayOfMonth(b)
        )
    )

  /**
   * The record fields a `java.time.Period` is written as in MessagePack:
   * `years`, `months` and `days`.
   */
  val periodStructure: Seq[Schema.Field[Period, Int]] = Seq(
    Schema
      .Field("years", Schema.Primitive(StandardType.IntType), get0 = _.getYears, set0 = (a, b: Int) => a.withYears(b)),
    Schema.Field(
      "months",
      Schema.Primitive(StandardType.IntType),
      get0 = _.getMonths,
      set0 = (a, b: Int) => a.withMonths(b)
    ),
    Schema.Field("days", Schema.Primitive(StandardType.IntType), get0 = _.getDays, set0 = (a, b: Int) => a.withDays(b))
  )

  /**
   * The record fields a `java.time.YearMonth` is written as in MessagePack:
   * `year` and `month`.
   */
  val yearMonthStructure: Seq[Schema.Field[YearMonth, Int]] =
    Seq(
      Schema.Field(
        "year",
        Schema.Primitive(StandardType.IntType),
        get0 = _.getYear,
        set0 = (a, b: Int) => a.`with`(Year.of(b))
      ),
      Schema.Field(
        "month",
        Schema.Primitive(StandardType.IntType),
        get0 = _.getMonthValue,
        set0 = (a, b: Int) => a.`with`(Month.of(b))
      )
    )

  /**
   * The record fields a `java.time.Duration` is written as in MessagePack:
   * `seconds` and `nanos`.
   */
  val durationStructure: Seq[Schema.Field[Duration, _]] =
    Seq(
      Schema.Field(
        "seconds",
        Schema.Primitive(StandardType.LongType),
        get0 = _.getSeconds,
        set0 = (a, b: Long) => a.plusSeconds(b)
      ),
      Schema
        .Field(
          "nanos",
          Schema.Primitive(StandardType.IntType),
          get0 = _.getNano,
          set0 = (a, b: Int) => a.plusNanos(b.toLong)
        )
    )
}
