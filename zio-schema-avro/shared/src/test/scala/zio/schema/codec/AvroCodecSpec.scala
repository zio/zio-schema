package zio.schema.codec

import zio.{ Chunk, ZIO }
import zio.schema.{ Schema, StandardType }
import zio.stream.{ ZSink, ZStream }
import zio.test.Assertion._
import zio.test._

import java.util.UUID
import java.math.BigInteger

object AvroCodecSpec extends DefaultRunnableSpec {

  def spec = suite("AvroCodec Spec")(
    suite("encode")(
      suite("primitives")(
        testM("boolean") {
          for {
            f <- encode(Schema.Primitive(StandardType.BoolType), false).map(toHex(_))
            t <- encode(Schema.Primitive(StandardType.BoolType), true).map(toHex(_))
          } yield assert(f)(equalTo("00")) && assert(t)(equalTo("01"))
        },
        testM("short") {
          for {
            p0  <- encode[Short](Schema.Primitive(StandardType.ShortType), 0).map(toHex(_))
            p1  <- encode[Short](Schema.Primitive(StandardType.ShortType), 1).map(toHex(_))
            n1  <- encode[Short](Schema.Primitive(StandardType.ShortType), -1).map(toHex(_))
            p64 <- encode[Short](Schema.Primitive(StandardType.ShortType), 64).map(toHex(_))
            n64 <- encode[Short](Schema.Primitive(StandardType.ShortType), -64).map(toHex(_))
          } yield assert(p0)(equalTo("00")) && assert(p1)(equalTo("02")) && assert(n1)(equalTo("01")) && assert(p64)(
            equalTo("8001")
          ) && assert(n64)(equalTo("7F"))
        },
        testM("int") {
          for {
            p0       <- encode(Schema.Primitive(StandardType.IntType), 0).map(toHex(_))
            p1       <- encode(Schema.Primitive(StandardType.IntType), 1).map(toHex(_))
            n1       <- encode(Schema.Primitive(StandardType.IntType), -1).map(toHex(_))
            p64      <- encode(Schema.Primitive(StandardType.IntType), 64).map(toHex(_))
            n64      <- encode(Schema.Primitive(StandardType.IntType), -64).map(toHex(_))
            p8124    <- encode(Schema.Primitive(StandardType.IntType), 8124).map(toHex(_))
            p1677664 <- encode(Schema.Primitive(StandardType.IntType), 1677664).map(toHex(_))
          } yield assert(p0)(equalTo("00")) && assert(p1)(equalTo("02")) && assert(n1)(equalTo("01")) && assert(p64)(
            equalTo("8001")
          ) && assert(n64)(equalTo("7F")) && assert(p8124)(equalTo("F87E")) && assert(p1677664)(equalTo("C0E5CC01"))
        },
        testM("long") {
          for {
            p0            <- encode(Schema.Primitive(StandardType.LongType), 0L).map(toHex(_))
            p1            <- encode(Schema.Primitive(StandardType.LongType), 1L).map(toHex(_))
            n1            <- encode(Schema.Primitive(StandardType.LongType), -1L).map(toHex(_))
            p64           <- encode(Schema.Primitive(StandardType.LongType), 64L).map(toHex(_))
            n64           <- encode(Schema.Primitive(StandardType.LongType), -64L).map(toHex(_))
            p8124000      <- encode(Schema.Primitive(StandardType.LongType), 8124000L).map(toHex(_))
            p167766400000 <- encode(Schema.Primitive(StandardType.LongType), 167766400000L).map(toHex(_))
          } yield assert(p0)(equalTo("00")) && assert(p1)(equalTo("02")) && assert(n1)(equalTo("01")) && assert(p64)(
            equalTo("8001")
          ) && assert(n64)(equalTo("7F")) && assert(p8124000)(equalTo("C0D9DF07")) && assert(p167766400000)(
            equalTo("80F0C0FAE109")
          )
        },
        testM("float") {
          for {
            f1 <- encode(Schema.Primitive(StandardType.FloatType), 3.1415f).map(toHex(_))
          } yield assert(f1)(equalTo("560E4940"))
        },
        testM("double") {
          for {
            d1 <- encode(Schema.Primitive(StandardType.DoubleType), 3.1415d).map(toHex(_))
          } yield assert(d1)(equalTo("6F1283C0CA210940"))
        },
        testM("bytes") {
          val c1Bytes = Chunk[Byte](-1, 0, 1, 2)
          for {
            empty <- encode(Schema.Primitive(StandardType.BinaryType), Chunk.empty).map(toHex(_))
            c1    <- encode(Schema.Primitive(StandardType.BinaryType), c1Bytes).map(toHex(_))
          } yield assert(empty)(equalTo("00")) && assert(c1)(equalTo("08FF000102"))
        },
        testM("string") {
          for {
            empty <- encode(Schema.Primitive(StandardType.StringType), "").map(toHex(_))
            s1    <- encode(Schema.Primitive(StandardType.StringType), "avro").map(toHex(_))
          } yield assert(empty)(equalTo("00")) && assert(s1)(equalTo("086176726F"))
        },
        testM("bigDecimal") {
          for {
            t1 <- encode[java.math.BigDecimal](
                   Schema.Primitive(StandardType.BigDecimalType),
                   BigDecimal("3.1415").bigDecimal
                 ).map(toHex(_))
            n1 <- encode[java.math.BigDecimal](
                   Schema.Primitive(StandardType.BigDecimalType),
                   BigDecimal("-3.1415").bigDecimal
                 ).map(toHex(_))
          } yield assert(t1)(equalTo("7AB7")) && assert(n1)(equalTo("8549"))
        },
        testM("bitInteger") {
          for {
            t1 <- encode[BigInteger](Schema.Primitive(StandardType.BigIntegerType), BigInt("31415").bigInteger)
                   .map(toHex(_))
            n1 <- encode[BigInteger](Schema.Primitive(StandardType.BigIntegerType), BigInt("-31415").bigInteger)
                   .map(toHex(_))
          } yield assert(t1)(equalTo("7AB7")) && assert(n1)(equalTo("8549"))
        },
        testM("char") {
          for {
            t1 <- encode(Schema.Primitive(StandardType.CharType), 'a'.charValue()).map(toHex(_))
          } yield assert(t1)(equalTo("0261"))
        },
        testM("dayOfWeek") {
          import java.time.DayOfWeek
          for {
            monday <- encode(Schema.Primitive(StandardType.DayOfWeekType), DayOfWeek.MONDAY).map(toHex(_))
            friday <- encode(Schema.Primitive(StandardType.DayOfWeekType), DayOfWeek.FRIDAY).map(toHex(_))
          } yield assert(monday)(equalTo("00")) && assert(friday)(equalTo("08"))
        },
        testM("month") {
          import java.time.Month
          for {
            jan <- encode(Schema.Primitive(StandardType.Month), Month.JANUARY).map(toHex(_))
            may <- encode(Schema.Primitive(StandardType.Month), Month.MAY).map(toHex(_))
          } yield assert(jan)(equalTo("00")) && assert(may)(equalTo("08"))
        },
        testM("monthDay") {
          import java.time.{ Month, MonthDay }
          for {
            jan4    <- encode(Schema.Primitive(StandardType.MonthDay), MonthDay.of(Month.JANUARY, 4)).map(toHex(_))
            april12 <- encode(Schema.Primitive(StandardType.MonthDay), MonthDay.of(Month.APRIL, 12)).map(toHex(_))
          } yield assert(jan4)(equalTo("0208")) && assert(april12)(equalTo("0818"))
        },
        testM("year") {
          import java.time.Year
          for {
            bc <- encode(Schema.Primitive(StandardType.Year), Year.of(-4)).map(toHex(_))
            ac <- encode(Schema.Primitive(StandardType.Year), Year.of(4)).map(toHex(_))
          } yield assert(bc)(equalTo("07")) && assert(ac)(equalTo("08"))
        },
        testM("yearMonth") {
          import java.time.{ Month, YearMonth }
          for {
            yearMonth <- encode(Schema.Primitive(StandardType.YearMonth), YearMonth.of(4, Month.OCTOBER)).map(toHex(_))
          } yield assert(yearMonth)(equalTo("0814"))
        },
        testM("period") {
          import java.time.Period
          for {
            period <- encode(Schema.Primitive(StandardType.Period), Period.of(1, 2, 3)).map(toHex(_))
          } yield assert(period)(equalTo("020406"))
        },
        testM("unit") {
          for {
            encoded <- encode(Schema.Primitive(StandardType.UnitType), ()).map(toHex(_))
          } yield assert(encoded)(equalTo(""))
        },
        testM("uuid") {
          for {
            encoded <- encode(
                        Schema.Primitive(StandardType.UUIDType),
                        UUID.fromString("d326fea5-94e4-4ba5-988e-3c4f194067a7")
                      ).map(toHex(_))
          } yield assert(encoded)(equalTo("4864333236666561352D393465342D346261352D393838652D336334663139343036376137"))
        },
        testM("zoneId") {
          import java.time.ZoneId
          for {
            encoded <- encode(Schema.Primitive(StandardType.ZoneId), ZoneId.of("CET")).map(toHex(_))
          } yield assert(encoded)(equalTo("06434554"))
        },
        testM("zoneOffset") {
          import java.time.ZoneOffset
          for {
            encoded <- encode(Schema.Primitive(StandardType.ZoneOffset), ZoneOffset.ofHoursMinutes(1, 30)).map(toHex(_))
          } yield assert(encoded)(equalTo("B054"))
        },
        testM("duration") {
          import java.time.Duration
          import java.time.temporal.ChronoUnit
          for {
            encoded <- encode(Schema.Primitive(StandardType.Duration(ChronoUnit.SECONDS)), Duration.ofNanos(2000004L))
                        .map(toHex(_))
          } yield assert(encoded)(equalTo("008892F401"))
        },
        testM("instant") {
          import java.time.Instant
          import java.time.format.DateTimeFormatter
          val formatter = DateTimeFormatter.ISO_DATE_TIME
          for {
            encoded <- encode(Schema.Primitive(StandardType.Instant(formatter)), Instant.ofEpochMilli(12L))
                        .map(toHex(_))
          } yield assert(encoded)(equalTo("18"))
        },
        testM("localDate") {
          import java.time.LocalDate
          import java.time.format.DateTimeFormatter
          val formatter = DateTimeFormatter.ISO_LOCAL_DATE
          for {
            encoded <- encode(Schema.Primitive(StandardType.LocalDate(formatter)), LocalDate.of(2020, 3, 12))
                        .map(toHex(_))
          } yield assert(encoded)(equalTo("14323032302D30332D3132"))
        },
        testM("localDateTime") {
          import java.time.LocalDateTime
          import java.time.format.DateTimeFormatter
          val formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
          for {
            encoded <- encode(
                        Schema.Primitive(StandardType.LocalDateTime(formatter)),
                        LocalDateTime.of(2020, 3, 12, 2, 3, 4)
                      ).map(toHex(_))
          } yield assert(encoded)(equalTo("26323032302D30332D31325430323A30333A3034"))
        },
        testM("localTime") {
          import java.time.LocalTime
          import java.time.format.DateTimeFormatter
          val formatter = DateTimeFormatter.ISO_LOCAL_TIME
          for {
            encoded <- encode(Schema.Primitive(StandardType.LocalTime(formatter)), LocalTime.of(2, 3, 4)).map(toHex(_))
          } yield assert(encoded)(equalTo("1030323A30333A3034"))
        },
        testM("offsetDateTime") {
          import java.time.{ LocalDateTime, OffsetDateTime, ZoneOffset }
          import java.time.format.DateTimeFormatter
          val formatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
          for {
            encoded <- encode(
                        Schema.Primitive(StandardType.OffsetDateTime(formatter)),
                        OffsetDateTime.of(LocalDateTime.of(2020, 3, 12, 2, 3, 4), ZoneOffset.ofHours(2))
                      ).map(toHex(_))
          } yield assert(encoded)(equalTo("32323032302D30332D31325430323A30333A30342B30323A3030"))
        },
        testM("offsetTime") {
          import java.time.{ OffsetTime, ZoneOffset }
          import java.time.format.DateTimeFormatter
          val formatter = DateTimeFormatter.ISO_OFFSET_TIME
          for {
            encoded <- encode(
                        Schema.Primitive(StandardType.OffsetTime(formatter)),
                        OffsetTime.of(2, 3, 4, 0, ZoneOffset.ofHours(2))
                      ).map(toHex(_))
          } yield assert(encoded)(equalTo("1C30323A30333A30342B30323A3030"))
        },
        testM("zonedDateTime") {
          import java.time.{ LocalDateTime, ZoneId, ZonedDateTime }
          import java.time.format.DateTimeFormatter
          val formatter = DateTimeFormatter.ISO_ZONED_DATE_TIME
          for {
            encoded <- encode(
                        Schema.Primitive(StandardType.ZonedDateTime(formatter)),
                        ZonedDateTime.of(LocalDateTime.of(2020, 3, 12, 2, 3, 4), ZoneId.of("UTC"))
                      ).map(toHex(_))
          } yield assert(encoded)(equalTo("32323032302D30332D31325430323A30333A30345A5B5554435D"))
        }
      )
    ),
    suite("decode")(
      )
  )

  def encode[A](schema: Schema[A], input: A): ZIO[Any, Nothing, Chunk[Byte]] =
    ZStream
      .succeed(input)
      .transduce(AvroCodec.encoder(schema))
      .run(ZSink.collectAll)

  def toHex(chunk: Chunk[Byte]): String =
    chunk.toArray.map("%02X".format(_)).mkString
}
