package zio.schema.codec

import java.math.{ BigInteger, MathContext }
import java.time.{ Duration, Month, MonthDay, Period, Year, YearMonth }

import zio.schema.{ Schema, StandardType }
import zio.stream.ZPipeline
import zio.{ Chunk, ZIO }

object MessagePackCodec extends Codec {
  override def encoder[A](schema: Schema[A]): ZPipeline[Any, Nothing, A, Byte] = {
    val encoder = new MessagePackEncoder()
    ZPipeline.mapChunks { chunk =>
      chunk.flatMap(encoder.encode(schema, _))
    }
  }

  override def decoder[A](schema: Schema[A]): ZPipeline[Any, String, Byte, A] =
    ZPipeline.mapChunksZIO { chunk =>
      ZIO.fromEither(
        new MessagePackDecoder(chunk)
          .decode(schema)
          .map(Chunk(_))
          .left
          .map(err => s"Error at path /${err.path.mkString(".")}: ${err.error}")
      )
    }

  override def encode[A](schema: Schema[A]): A => Chunk[Byte] =
    a => new MessagePackEncoder().encode(schema, a)

  override def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A] =
    ch =>
      if (ch.isEmpty)
        Left("No bytes to decode")
      else
        new MessagePackDecoder(ch)
          .decode(schema)
          .left
          .map(
            err => s"Error at path /${err.path.mkString(".")}: ${err.error}"
          )

  //TODO those are duplicates from ThriftCodec
  val bigDecimalStructure: Seq[Schema.Field[java.math.BigDecimal, _]] =
    Seq(
      Schema.Field(
        "unscaled",
        Schema.Primitive(StandardType.BigIntegerType),
        get = _.unscaledValue(),
        set = (a, b: BigInteger) => new java.math.BigDecimal(b, a.scale)
      ),
      Schema.Field(
        "precision",
        Schema.Primitive(StandardType.IntType),
        get = _.precision(),
        set = (a, b: Int) => new java.math.BigDecimal(a.unscaledValue, new MathContext(b))
      ),
      Schema
        .Field("scale", Schema.Primitive(StandardType.IntType), get = _.scale(), set = (a, b: Int) => a.setScale(b))
    )

  val monthDayStructure: Seq[Schema.Field[MonthDay, Int]] =
    Seq(
      Schema.Field(
        "month",
        Schema.Primitive(StandardType.IntType),
        get = (v: MonthDay) => v.getMonthValue,
        set = (a, b: Int) => a.`with`(Month.of(b))
      ),
      Schema
        .Field(
          "day",
          Schema.Primitive(StandardType.IntType),
          get = _.getDayOfMonth,
          set = (a, b: Int) => a.withDayOfMonth(b)
        )
    )

  val periodStructure: Seq[Schema.Field[Period, Int]] = Seq(
    Schema
      .Field("years", Schema.Primitive(StandardType.IntType), get = _.getYears, set = (a, b: Int) => a.withYears(b)),
    Schema.Field(
      "months",
      Schema.Primitive(StandardType.IntType),
      get = _.getMonths,
      set = (a, b: Int) => a.withMonths(b)
    ),
    Schema.Field("days", Schema.Primitive(StandardType.IntType), get = _.getDays, set = (a, b: Int) => a.withDays(b))
  )

  val yearMonthStructure: Seq[Schema.Field[YearMonth, Int]] =
    Seq(
      Schema.Field(
        "year",
        Schema.Primitive(StandardType.IntType),
        get = _.getYear,
        set = (a, b: Int) => a.`with`(Year.of(b))
      ),
      Schema.Field(
        "month",
        Schema.Primitive(StandardType.IntType),
        get = _.getMonthValue,
        set = (a, b: Int) => a.`with`(Month.of(b))
      )
    )

  val durationStructure: Seq[Schema.Field[Duration, _]] =
    Seq(
      Schema.Field(
        "seconds",
        Schema.Primitive(StandardType.LongType),
        get = _.getSeconds,
        set = (a, b: Long) => a.plusSeconds(b)
      ),
      Schema
        .Field(
          "nanos",
          Schema.Primitive(StandardType.IntType),
          get = _.getNano,
          set = (a, b: Int) => a.plusNanos(b.toLong)
        )
    )
}
