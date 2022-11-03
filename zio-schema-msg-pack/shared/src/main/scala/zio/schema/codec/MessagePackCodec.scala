package zio.schema.codec

import java.math.{ BigInteger, MathContext }
import java.time.{ Duration, Month, MonthDay, Period, Year, YearMonth }

import zio.schema.codec.BinaryCodec.{ BinaryDecoder, BinaryEncoder, BinaryStreamDecoder, BinaryStreamEncoder }
import zio.schema.{ Schema, StandardType }
import zio.stream.ZPipeline
import zio.{ Chunk, ZIO }

object MessagePackCodec extends BinaryCodec {
  override def encoderFor[A](schema: Schema[A]): BinaryEncoder[A] = new BinaryEncoder[A] {

    override def encode(value: A): Chunk[Byte] =
      new MessagePackEncoder().encode(schema, value)

    override def streamEncoder: BinaryStreamEncoder[A] = {
      val encoder = new MessagePackEncoder()
      ZPipeline.mapChunks { chunk =>
        chunk.flatMap(encoder.encode(schema, _))
      }
    }
  }

  override def decoderFor[A](schema: Schema[A]): BinaryDecoder[A] = new BinaryDecoder[A] {

    override def decode(chunk: Chunk[Byte]): Either[String, A] =
      if (chunk.isEmpty)
        Left("No bytes to decode")
      else
        decodeChunk(chunk)

    override def streamDecoder: BinaryStreamDecoder[A] =
      ZPipeline.mapChunksZIO { chunk =>
        ZIO.fromEither(
          decodeChunk(chunk).map(Chunk(_))
        )
      }

    private def decodeChunk(chunk: Chunk[Byte]) =
      new MessagePackDecoder(chunk)
        .decode(schema)
        .left
        .map(
          err => s"Error at path /${err.path.mkString(".")}: ${err.error}"
        )
  }

  //TODO those are duplicates from ThriftCodec
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
