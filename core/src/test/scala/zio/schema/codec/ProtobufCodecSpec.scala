package zio.schema.codec

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{
  DayOfWeek,
  Duration,
  Instant,
  LocalDate,
  LocalDateTime,
  LocalTime,
  Month,
  MonthDay,
  OffsetDateTime,
  OffsetTime,
  Period,
  Year,
  YearMonth,
//  ZoneId,
  ZoneOffset,
  ZonedDateTime
}

import scala.util.Try

import zio.Chunk
import zio.schema.Schema.Primitive
import zio.schema.{ Schema, StandardType }
import zio.stream.{ ZSink, ZStream }
import zio.test.Assertion._
import zio.test._

// TODO: use generators instead of manual encode/decode
object ProtobufCodecSpec extends DefaultRunnableSpec {

  def spec = suite("ProtobufCodec Spec")(
    suite("Should correctly encode")(
      testM("integers") {
        assertM(encode(schemaBasicInt, BasicInt(150)).map(toHex))(
          equalTo("089601")
        )
      },
      testM("strings") {
        assertM(encode(schemaBasicString, BasicString("testing")).map(toHex))(
          equalTo("0A0774657374696E67")
        )
      },
      testM("floats") {
        assertM(encode(schemaBasicFloat, BasicFloat(0.001f)).map(toHex))(
          equalTo("0D6F12833A")
        )
      },
      testM("doubles") {
        assertM(encode(schemaBasicDouble, BasicDouble(0.001)).map(toHex))(
          equalTo("09FCA9F1D24D62503F")
        )
      },
      testM("embedded messages") {
        assertM(encode(schemaEmbedded, Embedded(BasicInt(150))).map(toHex))(
          equalTo("0A03089601")
        )
      },
      testM("packed lists") {
        assertM(encode(schemaPackedList, PackedList(List(3, 270, 86942))).map(toHex))(
          equalTo("0A06038E029EA705")
        )
      },
      testM("unpacked lists") {
        assertM(encode(schemaUnpackedList, UnpackedList(List("foo", "bar", "baz"))).map(toHex))(
          equalTo("0A03666F6F0A036261720A0362617A")
        )
      },
      testM("records") {
        assertM(encode(schemaRecord, Record("Foo", 123)).map(toHex))(
          equalTo("0A03466F6F107B")
        )
      },
      testM("enumerations") {
        assertM(encode(schemaEnumeration, Enumeration(IntValue(482))).map(toHex))(
          equalTo("10E203")
        )
      }
    ),
    suite("Should successfully encode and decode")(
      testM("messages") {
        assertM(encodeAndDecode(schema, message))(
          equalTo(Chunk(message))
        )
      },
      testM("booleans") {
        val value = true
        assertM(encodeAndDecode(Schema[Boolean], value))(
          equalTo(Chunk(value))
        )
      },
      testM("shorts") {
        val value = 5.toShort
        assertM(encodeAndDecode(Schema[Short], value))(
          equalTo(Chunk(value))
        )
      },
      testM("longs") {
        val value = 1000L
        assertM(encodeAndDecode(Schema[Long], value))(
          equalTo(Chunk(value))
        )
      },
      testM("floats") {
        val value = 0.001f
        assertM(encodeAndDecode(Schema[Float], value))(
          equalTo(Chunk(value))
        )
      },
      testM("doubles") {
        val value = 0.001
        assertM(encodeAndDecode(Schema[Double], value))(
          equalTo(Chunk(value))
        )
      },
      testM("bytes") {
        val value = Chunk.fromArray("some bytes".getBytes)
        assertM(encodeAndDecode(Schema[Chunk[Byte]], value))(
          equalTo(Chunk(value))
        )
      },
      testM("chars") {
        val value = 'c'
        assertM(encodeAndDecode(Schema[Char], value))(
          equalTo(Chunk(value))
        )
      },
      testM("day of weeks") {
        val value = DayOfWeek.of(3)
        assertM(encodeAndDecode(Schema[DayOfWeek], value))(
          equalTo(Chunk(value))
        )
      },
      testM("months") {
        val value = Month.of(3)
        assertM(encodeAndDecode(Schema[Month], value))(
          equalTo(Chunk(value))
        )
      },
      testM("month days") {
        val value = MonthDay.of(1, 31)
        assertM(encodeAndDecode(Schema[MonthDay], value))(
          equalTo(Chunk(value))
        )
      },
      testM("periods") {
        val value = Period.of(5, 3, 1)
        assertM(encodeAndDecode(Schema[Period], value))(
          equalTo(Chunk(value))
        )
      },
      testM("years") {
        val value = Year.of(2020)
        assertM(encodeAndDecode(Schema[Year], value))(
          equalTo(Chunk(value))
        )
      },
      testM("year months") {
        val value = YearMonth.of(2020, 5)
        assertM(encodeAndDecode(Schema[YearMonth], value))(
          equalTo(Chunk(value))
        )
      },
      // testM("zone ids") {
      //   val value = java.time.ZoneId.systemDefault()
      //   assertM(encodeAndDecode(Schema[ZoneId], value))(
      //     equalTo(Chunk(value))
      //   )
      // },
      testM("zone offsets") {
        val value = ZoneOffset.ofHours(6)
        assertM(encodeAndDecode(Schema[ZoneOffset], value))(
          equalTo(Chunk(value))
        )
      },
      testM("durations") {
        val value = Duration.ofDays(12)
        assertM(encodeAndDecode(Primitive(StandardType.Duration(ChronoUnit.DAYS)), value))(
          equalTo(Chunk(value))
        )
      },
      testM("instants") {
        val value = Instant.now()
        assertM(encodeAndDecode(Primitive(StandardType.Instant(DateTimeFormatter.ISO_INSTANT)), value))(
          equalTo(Chunk(value))
        )
      },
      testM("local dates") {
        val value = LocalDate.now()
        assertM(encodeAndDecode(Primitive(StandardType.LocalDate(DateTimeFormatter.ISO_LOCAL_DATE)), value))(
          equalTo(Chunk(value))
        )
      },
      testM("local times") {
        val value = LocalTime.now()
        assertM(encodeAndDecode(Primitive(StandardType.LocalTime(DateTimeFormatter.ISO_LOCAL_TIME)), value))(
          equalTo(Chunk(value))
        )
      },
      testM("local date times") {
        val value = LocalDateTime.now()
        assertM(encodeAndDecode(Primitive(StandardType.LocalDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME)), value))(
          equalTo(Chunk(value))
        )
      },
      testM("offset times") {
        val value = OffsetTime.now()
        assertM(encodeAndDecode(Primitive(StandardType.OffsetTime(DateTimeFormatter.ISO_OFFSET_TIME)), value))(
          equalTo(Chunk(value))
        )
      },
      testM("offset date times") {
        val value = OffsetDateTime.now()
        assertM(encodeAndDecode(Primitive(StandardType.OffsetDateTime(DateTimeFormatter.ISO_OFFSET_DATE_TIME)), value))(
          equalTo(Chunk(value))
        )
      },
      testM("zoned date times") {
        val value = ZonedDateTime.now()
        assertM(encodeAndDecode(Primitive(StandardType.ZonedDateTime(DateTimeFormatter.ISO_ZONED_DATE_TIME)), value))(
          equalTo(Chunk(value))
        )
      },
      testM("packed sequences") {
        assertM(encodeAndDecode(schemaPackedList, PackedList(List(3, 270, 86942))))(
          equalTo(Chunk(PackedList(List(3, 270, 86942))))
        )
      },
      testM("non-packed sequences") {
        assertM(encodeAndDecode(schemaUnpackedList, UnpackedList(List("foo", "bar", "baz"))))(
          equalTo(Chunk(UnpackedList(List("foo", "bar", "baz"))))
        )
      },
      testM("enumerations") {
        assertM(encodeAndDecode(schemaEnumeration, Enumeration(BooleanValue(true))))(
          equalTo(Chunk(Enumeration(BooleanValue(true))))
        )
      },
      testM("tuples") {
        val value = (123, "foo")
        assertM(encodeAndDecode(schemaTuple, value))(
          equalTo(Chunk(value))
        )
      },
      testM("optionals") {
        val value = Some(123)
        assertM(encodeAndDecode(Schema.Optional(Schema[Int]), value))(
          equalTo(Chunk(value))
        )
      }
    ),
    suite("Should successfully decode")(
      testM("incomplete messages using default values") {
        assertM(decode(schemaRecord, "107B"))(
          equalTo(Chunk(Record("", 123)))
        )
      },
      testM("incomplete tuples using default values") {
        assertM(decode(schemaTuple, "087B"))(
          equalTo(Chunk((123, "")))
        )
      },
      testM("empty input") {
        assertM(decode(Schema[Int], ""))(
          equalTo(Chunk.empty)
        )
      }
    ),
    suite("Should fail to decode")(
      testM("unknown wire types") {
        assertM(decode(schemaRecord, "0F").run)(
          fails(equalTo("Failed decoding key: unknown wire type"))
        )
      },
      testM("invalid field numbers") {
        assertM(decode(schemaRecord, "00").run)(
          fails(equalTo("Failed decoding key: invalid field number"))
        )
      },
      testM("incomplete length delimited values") {
        assertM(decode(schemaRecord, "0A0346").run)(
          fails(equalTo("Unexpected end of chunk"))
        )
      },
      testM("incomplete var ints") {
        assertM(decode(schemaRecord, "10FF").run)(
          fails(equalTo("Unexpected end of chunk"))
        )
      }
    )
  )

  // some tests are based on https://developers.google.com/protocol-buffers/docs/encoding

  case class BasicInt(value: Int)

  val schemaBasicInt: Schema[BasicInt] = Schema.caseClassN(
    "value" -> Schema[Int]
  )(BasicInt, BasicInt.unapply)

  case class BasicString(value: String)

  val schemaBasicString: Schema[BasicString] = Schema.caseClassN(
    "value" -> Schema[String]
  )(BasicString, BasicString.unapply)

  case class BasicFloat(value: Float)

  val schemaBasicFloat: Schema[BasicFloat] = Schema.caseClassN(
    "value" -> Schema[Float]
  )(BasicFloat, BasicFloat.unapply)

  case class BasicDouble(value: Double)

  val schemaBasicDouble: Schema[BasicDouble] = Schema.caseClassN(
    "value" -> Schema[Double]
  )(BasicDouble, BasicDouble.unapply)

  case class Embedded(embedded: BasicInt)

  val schemaEmbedded: Schema[Embedded] = Schema.caseClassN(
    "embedded" -> schemaBasicInt
  )(Embedded, Embedded.unapply)

  case class PackedList(packed: List[Int])

  val schemaPackedList: Schema[PackedList] = Schema.caseClassN(
    "packed" -> Schema.list(Schema[Int])
  )(PackedList, PackedList.unapply)

  case class UnpackedList(items: List[String])

  val schemaUnpackedList: Schema[UnpackedList] = Schema.caseClassN(
    "unpacked" -> Schema.list(Schema[String])
  )(UnpackedList, UnpackedList.unapply)

  case class Record(name: String, value: Int)

  val schemaRecord: Schema[Record] = Schema.caseClassN(
    "name"  -> Schema[String],
    "value" -> Schema[Int]
  )(Record, Record.unapply)

  val schemaTuple: Schema.Tuple[Int, String] = Schema.Tuple(Schema[Int], Schema[String])

  sealed trait OneOf
  case class StringValue(value: String)   extends OneOf
  case class IntValue(value: Int)         extends OneOf
  case class BooleanValue(value: Boolean) extends OneOf

  val schemaOneOf: Schema[OneOf] = Schema.Transform(
    Schema.enumeration(
      Map(
        "string"  -> Schema[String],
        "int"     -> Schema[Int],
        "boolean" -> Schema[Boolean]
      )
    ),
    (value: Map[String, Any]) => {
      value
        .get("string")
        .map(v => Right(StringValue(v.asInstanceOf[String])))
        .orElse(value.get("int").map(v => Right(IntValue(v.asInstanceOf[Int]))))
        .orElse(value.get("boolean").map(v => Right(BooleanValue(v.asInstanceOf[Boolean]))))
        .getOrElse(Left("No value found"))
    }, { (o: OneOf) =>
      o match {
        case StringValue(v)  => Right(Map("string"  -> v))
        case IntValue(v)     => Right(Map("int"     -> v))
        case BooleanValue(v) => Right(Map("boolean" -> v))
      }
    }
  )

  case class Enumeration(oneOf: OneOf)

  val schemaEnumeration: Schema[Enumeration] =
    Schema.caseClassN("value" -> schemaOneOf)(Enumeration, Enumeration.unapply)

  case class SearchRequest(query: String, pageNumber: Int, resultPerPage: Int)

  val schema: Schema[SearchRequest] = Schema.caseClassN(
    "query"         -> Schema[String],
    "pageNumber"    -> Schema[Int],
    "resultPerPage" -> Schema[Int]
  )(SearchRequest, SearchRequest.unapply)

  val message: SearchRequest = SearchRequest("bitcoins", 1, 100)

  def toHex(chunk: Chunk[Byte]): String =
    chunk.toArray.map("%02X".format(_)).mkString

  def fromHex(hex: String): Chunk[Byte] =
    Try(hex.split("(?<=\\G.{2})").map(Integer.parseInt(_, 16).toByte))
      .map(Chunk.fromArray)
      .getOrElse(Chunk.empty)

  def encode[A](schema: Schema[A], input: A) =
    ZStream
      .succeed(input)
      .transduce(ProtobufCodec.encoder(schema))
      .run(ZSink.collectAll)

  def decode[A](schema: Schema[A], hex: String) =
    ZStream
      .fromChunk(fromHex(hex))
      .transduce(ProtobufCodec.decoder(schema))
      .run(ZSink.collectAll)

  def encodeAndDecode[A](schema: Schema[A], input: A) =
    ZStream
      .succeed(input)
      .transduce(ProtobufCodec.encoder(schema))
      .transduce(ProtobufCodec.decoder(schema))
      .run(ZSink.collectAll)
}
