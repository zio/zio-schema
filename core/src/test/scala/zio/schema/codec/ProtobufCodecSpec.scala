package zio.schema.codec

import zio.schema.Schema.Primitive
import zio.schema.{ Schema, StandardType }
import zio.stream.{ ZSink, ZStream }
import zio.test.Assertion._
import zio.test._
import zio.{ Chunk, ZIO }

import java.time._
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.util.Try

// TODO: use generators instead of manual encode/decode
object ProtobufCodecSpec extends DefaultRunnableSpec {

  def spec = suite("ProtobufCodec Spec")(
    suite("Should correctly encode")(
      testM("integers") {
        for {
          e  <- encode(schemaBasicInt, BasicInt(150)).map(toHex)
          e2 <- encodeNS(schemaBasicInt, BasicInt(150)).map(toHex)
        } yield assert(e)(equalTo("089601")) && assert(e2)(equalTo("089601"))
      },
      testM("strings") {
        for {
          e  <- encode(schemaBasicString, BasicString("testing")).map(toHex)
          e2 <- encodeNS(schemaBasicString, BasicString("testing")).map(toHex)
        } yield assert(e)(equalTo("0A0774657374696E67")) && assert(e2)(equalTo("0A0774657374696E67"))
      },
      testM("floats") {
        for {
          e  <- encode(schemaBasicFloat, BasicFloat(0.001f)).map(toHex)
          e2 <- encodeNS(schemaBasicFloat, BasicFloat(0.001f)).map(toHex)
        } yield assert(e)(equalTo("0D6F12833A")) && assert(e2)(equalTo("0D6F12833A"))
      },
      testM("doubles") {
        for {
          e  <- encode(schemaBasicDouble, BasicDouble(0.001)).map(toHex)
          e2 <- encodeNS(schemaBasicDouble, BasicDouble(0.001)).map(toHex)
        } yield assert(e)(equalTo("09FCA9F1D24D62503F")) && assert(e2)(equalTo("09FCA9F1D24D62503F"))
      },
      testM("embedded messages") {
        for {
          e  <- encode(schemaEmbedded, Embedded(BasicInt(150))).map(toHex)
          e2 <- encodeNS(schemaEmbedded, Embedded(BasicInt(150))).map(toHex)
        } yield assert(e)(equalTo("0A03089601")) && assert(e2)(equalTo("0A03089601"))
      },
      testM("packed lists") {
        for {
          e  <- encode(schemaPackedList, PackedList(List(3, 270, 86942))).map(toHex)
          e2 <- encodeNS(schemaPackedList, PackedList(List(3, 270, 86942))).map(toHex)
        } yield assert(e)(equalTo("0A06038E029EA705")) && assert(e2)(equalTo("0A06038E029EA705"))
      },
      testM("unpacked lists") {
        for {
          e  <- encode(schemaUnpackedList, UnpackedList(List("foo", "bar", "baz"))).map(toHex)
          e2 <- encodeNS(schemaUnpackedList, UnpackedList(List("foo", "bar", "baz"))).map(toHex)
        } yield assert(e)(equalTo("0A03666F6F0A036261720A0362617A")) && assert(e2)(
          equalTo("0A03666F6F0A036261720A0362617A")
        )
      },
      testM("records") {
        for {
          e  <- encode(schemaRecord, Record("Foo", 123)).map(toHex)
          e2 <- encodeNS(schemaRecord, Record("Foo", 123)).map(toHex)
        } yield assert(e)(equalTo("0A03466F6F107B")) && assert(e2)(equalTo("0A03466F6F107B"))
      },
      testM("enumerations") {
        for {
          e  <- encode(schemaEnumeration, Enumeration(IntValue(482))).map(toHex)
          e2 <- encodeNS(schemaEnumeration, Enumeration(IntValue(482))).map(toHex)
        } yield assert(e)(equalTo("0A0310E203")) && assert(e2)(equalTo("0A0310E203"))
      },
      testM("enums unwrapped") {
        for {
          e  <- encode(schemaOneOf, IntValue(482)).map(toHex)
          e2 <- encodeNS(schemaOneOf, IntValue(482)).map(toHex)
        } yield assert(e)(equalTo("10E203")) && assert(e2)(equalTo("10E203"))
      },
      testM("failure") {
        for {
          e  <- encode(schemaFail, StringValue("foo")).map(_.size)
          e2 <- encodeNS(schemaFail, StringValue("foo")).map(_.size)
        } yield assert(e)(equalTo(0)) && assert(e2)(equalTo(0))
      }
    ),
    suite("Should successfully encode and decode")(
      testM("integer") {
        for {
          ed2 <- encodeAndDecodeNS(schemaBasicInt, BasicInt(150))
        } yield assert(ed2)(equalTo(BasicInt(150)))
      },
      testM("integer inside wrapper class") {
        for {
          ed2 <- encodeAndDecodeNS(basicIntWrapperSchema, BasicIntWrapper(BasicInt(150)))
        } yield assert(ed2)(equalTo(BasicIntWrapper(BasicInt(150))))
      },
      testM("two integers") {
        for {
          ed2 <- encodeAndDecodeNS(schemaBasicTwoInts, BasicTwoInts(150, 151))
        } yield assert(ed2)(equalTo(BasicTwoInts(150, 151)))
      },
      testM("two integers inside wrapper class") {
        for {
          ed2 <- encodeAndDecodeNS(basicTwoIntWrapperSchema, BasicTwoIntWrapper(BasicTwoInts(150, 151)))
        } yield assert(ed2)(equalTo(BasicTwoIntWrapper(BasicTwoInts(150, 151))))
      },
      testM("two wrapped integers inside wrapper class") {
        for {
          e2 <- encodeAndDecodeNS(separateWrapper, SeparateWrapper(BasicInt(150), BasicInt(151)))
        } yield assert(e2)(equalTo(SeparateWrapper(BasicInt(150), BasicInt(151))))
      },
      testM("complex product and string and integer") {
        for {
          ed2 <- encodeAndDecodeNS(schema, message)
        } yield assert(ed2)(equalTo(message))
      },
      testM("booleans") {
        val value = true
        for {
          ed  <- encodeAndDecode(Schema[Boolean], value)
          ed2 <- encodeAndDecodeNS(Schema[Boolean], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("shorts") {
        val value = 5.toShort
        for {
          ed  <- encodeAndDecode(Schema[Short], value)
          ed2 <- encodeAndDecodeNS(Schema[Short], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("longs") {
        val value = 1000L
        for {
          ed  <- encodeAndDecode(Schema[Long], value)
          ed2 <- encodeAndDecodeNS(Schema[Long], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("floats") {
        val value = 0.001f
        for {
          ed  <- encodeAndDecode(Schema[Float], value)
          ed2 <- encodeAndDecodeNS(Schema[Float], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("doubles") {
        val value = 0.001
        for {
          ed  <- encodeAndDecode(Schema[Double], value)
          ed2 <- encodeAndDecodeNS(Schema[Double], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("bytes") {
        val value = Chunk.fromArray("some bytes".getBytes)
        for {
          ed  <- encodeAndDecode(Schema[Chunk[Byte]], value)
          ed2 <- encodeAndDecodeNS(Schema[Chunk[Byte]], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("chars") {
        val value = 'c'
        for {
          ed  <- encodeAndDecode(Schema[Char], value)
          ed2 <- encodeAndDecodeNS(Schema[Char], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("day of weeks") {
        val value = DayOfWeek.of(3)
        for {
          ed  <- encodeAndDecode(Schema[DayOfWeek], value)
          ed2 <- encodeAndDecodeNS(Schema[DayOfWeek], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("months") {
        val value = Month.of(3)
        for {
          ed  <- encodeAndDecode(Schema[Month], value)
          ed2 <- encodeAndDecodeNS(Schema[Month], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("month days") {
        val value = MonthDay.of(1, 31)
        for {
          ed  <- encodeAndDecode(Schema[MonthDay], value)
          ed2 <- encodeAndDecodeNS(Schema[MonthDay], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("periods") {
        val value = Period.of(5, 3, 1)
        for {
          ed  <- encodeAndDecode(Schema[Period], value)
          ed2 <- encodeAndDecodeNS(Schema[Period], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("years") {
        val value = Year.of(2020)
        for {
          ed  <- encodeAndDecode(Schema[Year], value)
          ed2 <- encodeAndDecodeNS(Schema[Year], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("year months") {
        val value = YearMonth.of(2020, 5)
        for {
          ed  <- encodeAndDecode(Schema[YearMonth], value)
          ed2 <- encodeAndDecodeNS(Schema[YearMonth], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("zone ids") {
        val value = ZoneId.systemDefault()
        for {
          ed  <- encodeAndDecode(Schema[ZoneId], value)
          ed2 <- encodeAndDecodeNS(Schema[ZoneId], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("zone offsets") {
        val value = ZoneOffset.ofHours(6)
        for {
          ed  <- encodeAndDecode(Schema[ZoneOffset], value)
          ed2 <- encodeAndDecodeNS(Schema[ZoneOffset], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("durations") {
        val value = Duration.ofDays(12)
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.Duration(ChronoUnit.DAYS)), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.Duration(ChronoUnit.DAYS)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("instants") {
        val value = Instant.now()
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.Instant(DateTimeFormatter.ISO_INSTANT)), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.Instant(DateTimeFormatter.ISO_INSTANT)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("local dates") {
        val value = LocalDate.now()
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.LocalDate(DateTimeFormatter.ISO_LOCAL_DATE)), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.LocalDate(DateTimeFormatter.ISO_LOCAL_DATE)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("local times") {
        val value = LocalTime.now()
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.LocalTime(DateTimeFormatter.ISO_LOCAL_TIME)), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.LocalTime(DateTimeFormatter.ISO_LOCAL_TIME)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("local date times") {
        val value = LocalDateTime.now()
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.LocalDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME)), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.LocalDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("offset times") {
        val value = OffsetTime.now()
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.OffsetTime(DateTimeFormatter.ISO_OFFSET_TIME)), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.OffsetTime(DateTimeFormatter.ISO_OFFSET_TIME)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("offset date times") {
        val value            = OffsetDateTime.now()
        val offsetDateSchema = Primitive(StandardType.OffsetDateTime(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
        for {
          ed  <- encodeAndDecode(offsetDateSchema, value)
          ed2 <- encodeAndDecodeNS(offsetDateSchema, value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("zoned date times") {
        val zoneSchema = Primitive(StandardType.ZonedDateTime(DateTimeFormatter.ISO_ZONED_DATE_TIME))
        val now        = ZonedDateTime.now()
        for {
          ed  <- encodeAndDecode(zoneSchema, now)
          ed2 <- encodeAndDecodeNS(zoneSchema, now)
        } yield assert(ed)(equalTo(Chunk(now))) && assert(ed2)(equalTo(now))
      },
      testM("packed sequences") {
        val list = PackedList(List(3, 270, 86942))
        for {
          ed  <- encodeAndDecode(schemaPackedList, list)
          ed2 <- encodeAndDecodeNS(schemaPackedList, list)
        } yield assert(ed)(equalTo(Chunk(list))) && assert(ed2)(equalTo(list))
      },
      testM("non-packed sequences") {
        val list = UnpackedList(List("foo", "bar", "baz"))
        for {
          ed  <- encodeAndDecode(schemaUnpackedList, list)
          ed2 <- encodeAndDecodeNS(schemaUnpackedList, list)
        } yield assert(ed)(equalTo(Chunk(list))) && assert(ed2)(equalTo(list))
      },
      testM("enumerations") {
        for {
          ed  <- encodeAndDecode(schemaEnumeration, Enumeration(BooleanValue(true)))
          ed2 <- encodeAndDecodeNS(schemaEnumeration, Enumeration(IntValue(482)))
        } yield assert(ed)(equalTo(Chunk(Enumeration(BooleanValue(true))))) && assert(ed2)(
          equalTo(Enumeration(IntValue(482)))
        )
      },
      testM("enums unwrapped") {
        for {
          ed  <- encodeAndDecode(schemaOneOf, BooleanValue(true))
          ed2 <- encodeAndDecodeNS(schemaOneOf, BooleanValue(true))
        } yield assert(ed)(equalTo(Chunk(BooleanValue(true)))) && assert(ed2)(
          equalTo(BooleanValue(true))
        )
      },
      testM("enum within enum") {
        val oneOf   = RichSum.AnotherSum(BooleanValue(false))
        val wrapper = RichSum.LongWrapper(150L)
        for {
          ed  <- encodeAndDecode(richSumSchema, wrapper)
          ed2 <- encodeAndDecodeNS(richSumSchema, oneOf)
        } yield assert(ed)(equalTo(Chunk(wrapper))) && assert(ed2)(equalTo(oneOf))
      },
      testM("tuples") {
        val value = (123, "foo")
        for {
          ed  <- encodeAndDecode(schemaTuple, value)
          ed2 <- encodeAndDecodeNS(schemaTuple, value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("either left") {
        val either = Left(9)
        for {
          ed  <- encodeAndDecode(eitherSchema, either)
          ed2 <- encodeAndDecodeNS(eitherSchema, either)
        } yield assert(ed)(equalTo(Chunk(either))) && assert(ed2)(equalTo(either))
      },
      testM("either right") {
        val either = Right("hello")
        for {
          ed  <- encodeAndDecode(eitherSchema, either)
          ed2 <- encodeAndDecodeNS(eitherSchema, either)
        } yield assert(ed)(equalTo(Chunk(either))) && assert(ed2)(equalTo(either))
      },
      testM("either with product type") {
        val eitherLeft = Left(MyRecord(150))
        for {
          ed  <- encodeAndDecode(complexEitherSchema2, eitherLeft)
          ed2 <- encodeAndDecodeNS(complexEitherSchema2, eitherLeft)
        } yield assert(ed)(equalTo(Chunk(eitherLeft))) && assert(ed2)(equalTo(eitherLeft))
      },
      testM("either with sum type") {
        val eitherRight  = Right(BooleanValue(true))
        val eitherRight2 = Right(StringValue("hello"))
        for {
          ed  <- encodeAndDecode(complexEitherSchema, eitherRight2)
          ed2 <- encodeAndDecodeNS(complexEitherSchema, eitherRight)
        } yield assert(ed)(equalTo(Chunk(eitherRight2))) && assert(ed2)(equalTo(eitherRight))
      },
      testM("optionals") {
        val value = Some(123)
        for {
          ed  <- encodeAndDecode(Schema.Optional(Schema[Int]), value)
          ed2 <- encodeAndDecodeNS(Schema.Optional(Schema[Int]), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("complex optionals with sum type") {
        val value = Some(BooleanValue(true))
        for {
          ed  <- encodeAndDecode(Schema.Optional(schemaOneOf), value)
          ed2 <- encodeAndDecodeNS(Schema.Optional(schemaOneOf), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("option within option") {
        val value = Some(Some(true))
        for {
          ed  <- encodeAndDecode(Schema.option(Schema.option(Schema[Boolean])), value)
          ed2 <- encodeAndDecodeNS(Schema.option(Schema.option(Schema[Boolean])), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("product type with inner product type") {
        val richProduct = RichProduct(StringValue("sum_type"), BasicString("string"), Record("value", 47))
        for {
          ed  <- encodeAndDecode(richProductSchema, richProduct)
          ed2 <- encodeAndDecodeNS(richProductSchema, richProduct)
        } yield assert(ed)(equalTo(Chunk(richProduct))) && assert(ed2)(equalTo(richProduct))
      },
      testM("complex sum type with nested product") {
        val richSum = RichSum.Person("hello", 10)
        for {
          ed  <- encodeAndDecode(richSumSchema, richSum)
          ed2 <- encodeAndDecodeNS(richSumSchema, richSum)
        } yield assert(ed)(equalTo(Chunk(richSum))) && assert(ed2)(equalTo(richSum))
      },
      testM("complex sum type with nested long primitive") {
        val long = RichSum.LongWrapper(100L)
        for {
          ed  <- encodeAndDecode(richSumSchema, long)
          ed2 <- encodeAndDecodeNS(richSumSchema, long)
        } yield assert(ed)(equalTo(Chunk(long))) && assert(ed2)(equalTo(long))
      },
      testM("complex either with product type") {
        val either = Left(Record("hello world", 100))
        for {
          ed  <- encodeAndDecode(complexEitherSchema, either)
          ed2 <- encodeAndDecodeNS(complexEitherSchema, either)
        } yield assert(ed)(equalTo(Chunk(either))) && assert(ed2)(equalTo(either))
      },
      testM("complex tuples") {
        val value = (Record("hello world", 100), BooleanValue(true))
        for {
          ed  <- encodeAndDecode(complexTupleSchema, value)
          ed2 <- encodeAndDecodeNS(complexTupleSchema, value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("complex optionals with product type") {
        val value = Some(Record("hello earth", 21))
        for {
          ed  <- encodeAndDecode(Schema.Optional(schemaRecord), value)
          ed2 <- encodeAndDecodeNS(Schema.Optional(schemaRecord), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("optional of product type within optional") {
        val value = Some(Some(Record("hello", 10)))
        for {
          ed  <- encodeAndDecode(Schema.Optional(Schema.Optional(schemaRecord)), value)
          ed2 <- encodeAndDecodeNS(Schema.Optional(Schema.Optional(schemaRecord)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("optional of sum type within optional") {
        val value = Some(Some(BooleanValue(true)))
        for {
          ed  <- encodeAndDecode(Schema.Optional(Schema.Optional(schemaOneOf)), value)
          ed2 <- encodeAndDecodeNS(Schema.Optional(Schema.Optional(schemaOneOf)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("either within either") {
        val either = Right(Left(BooleanValue(true)))
        val schema = Schema.either(Schema[Int], Schema.either(schemaOneOf, Schema[String]))
        for {
          ed  <- encodeAndDecode(schema, either)
          ed2 <- encodeAndDecodeNS(schema, either)
        } yield assert(ed)(equalTo(Chunk(either))) && assert(ed2)(equalTo(either))
      }
    ),
    suite("Should successfully decode")(
      testM("incomplete messages using default values") {
        for {
          e  <- decode(schemaRecord, "107B")
          e2 <- decodeNS(schemaRecord, "107B")
        } yield assert(e)(equalTo(Chunk(Record("", 123)))) && assert(e2)(equalTo(Record("", 123)))
      },
      testM("incomplete tuples using default values") {
        for {
          d  <- decode(schemaTuple, "087B")
          d2 <- decodeNS(schemaTuple, "087B")
        } yield assert(d)(equalTo(Chunk((123, "")))) && assert(d2)(equalTo((123, "")))
      },
      testM("empty input") {
        assertM(decode(Schema[Int], ""))(
          equalTo(Chunk.empty)
        )
      },
      testM("empty input by non streaming variant") {
        assertM(decodeNS(Schema[Int], "").run)(
          fails(equalTo("No bytes to decode"))
        )
      }
    ),
    suite("Should fail to decode")(
      testM("unknown wire types") {
        for {
          d  <- decode(schemaRecord, "0F").run
          d2 <- decodeNS(schemaRecord, "0F").run
        } yield assert(d)(fails(equalTo("Failed decoding key: unknown wire type"))) &&
          assert(d2)(fails(equalTo("Failed decoding key: unknown wire type")))
      },
      testM("invalid field numbers") {
        for {
          d  <- decode(schemaRecord, "00").run
          d2 <- decodeNS(schemaRecord, "00").run
        } yield assert(d)(fails(equalTo("Failed decoding key: invalid field number"))) &&
          assert(d2)(fails(equalTo("Failed decoding key: invalid field number")))
      },
      testM("incomplete length delimited values") {
        for {
          d  <- decode(schemaRecord, "0A0346").run
          d2 <- decodeNS(schemaRecord, "0A0346").run
        } yield assert(d)(fails(equalTo("Unexpected end of chunk"))) &&
          assert(d2)(fails(equalTo("Unexpected end of chunk")))
      },
      testM("incomplete var ints") {
        for {
          d  <- decode(schemaRecord, "10FF").run
          d2 <- decodeNS(schemaRecord, "10FF").run
        } yield assert(d)(fails(equalTo("Unexpected end of chunk"))) &&
          assert(d2)(fails(equalTo("Unexpected end of chunk")))
      },
      testM("fail schemas") {
        for {
          d  <- decode(schemaFail, "0F").run
          d2 <- decodeNS(schemaFail, "0F").run
        } yield assert(d)(fails(equalTo("failing schema"))) && assert(d2)(fails(equalTo("failing schema")))
      }
    )
  )

  // some tests are based on https://developers.google.com/protocol-buffers/docs/encoding

  case class BasicInt(value: Int)

  case class BasicTwoInts(value1: Int, value2: Int)

  val schemaBasicTwoInts: Schema[BasicTwoInts] = Schema.caseClassN(
    "value1" -> Schema[Int],
    "value2" -> Schema[Int]
  )(BasicTwoInts, BasicTwoInts.unapply)

  val schemaBasicInt: Schema[BasicInt] = Schema.caseClassN(
    "value" -> Schema[Int]
  )(BasicInt, BasicInt.unapply)

  case class BasicTwoIntWrapper(basic: BasicTwoInts)
  case class BasicIntWrapper(basic: BasicInt)
  case class SeparateWrapper(basic1: BasicInt, basic2: BasicInt)

  val basicIntWrapperSchema: Schema[BasicIntWrapper] = Schema.caseClassN(
    "basic" -> schemaBasicInt
  )(BasicIntWrapper, BasicIntWrapper.unapply)

  val basicTwoIntWrapperSchema: Schema[BasicTwoIntWrapper] = Schema.caseClassN(
    "basic" -> schemaBasicTwoInts
  )(BasicTwoIntWrapper, BasicTwoIntWrapper.unapply)

  case class BasicString(value: String)

  val schemaBasicString: Schema[BasicString] = Schema.caseClassN(
    "value" -> Schema[String]
  )(BasicString, BasicString.unapply)

  val separateWrapper: Schema[SeparateWrapper] = Schema.caseClassN(
    "basic1" -> schemaBasicInt,
    "basic2" -> schemaBasicInt
  )(SeparateWrapper, SeparateWrapper.unapply)

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
    (value: Map[String, _]) => {
      value
        .get("string")
        .map(v => Right(StringValue(v.asInstanceOf[String])))
        .orElse(value.get("int").map(v => Right(IntValue(v.asInstanceOf[Int]))))
        .orElse(value.get("boolean").map(v => Right(BooleanValue(v.asInstanceOf[Boolean]))))
        .getOrElse(Left("No value found"))
    }, {
      case StringValue(v)  => Right(Map("string"  -> v))
      case IntValue(v)     => Right(Map("int"     -> v))
      case BooleanValue(v) => Right(Map("boolean" -> v))
    }
  )

  case class MyRecord(age: Int)

  val myRecord: Schema[MyRecord] = Schema.caseClassN(
    "age" -> Schema[Int]
  )(MyRecord, MyRecord.unapply)

  val complexTupleSchema: Schema.Tuple[Record, OneOf] = Schema.Tuple(schemaRecord, schemaOneOf)

  val eitherSchema: Schema.EitherSchema[Int, String] = Schema.EitherSchema(Schema[Int], Schema[String])

  val complexEitherSchema: Schema.EitherSchema[Record, OneOf] =
    Schema.EitherSchema(schemaRecord, schemaOneOf)

  val complexEitherSchema2: Schema.EitherSchema[MyRecord, MyRecord] =
    Schema.EitherSchema(myRecord, myRecord)

  case class RichProduct(stringOneOf: OneOf, basicString: BasicString, record: Record)

  val richProductSchema: Schema[RichProduct] = Schema.caseClassN(
    "stringOneOf" -> schemaOneOf,
    "basicString" -> schemaBasicString,
    "record"      -> schemaRecord
  )(RichProduct, RichProduct.unapply)

  sealed trait RichSum

  object RichSum {
    case class Person(name: String, age: Int) extends RichSum
    case class AnotherSum(oneOf: OneOf)       extends RichSum
    case class LongWrapper(long: Long)        extends RichSum
  }

  val personSchema: Schema[RichSum.Person] = Schema.caseClassN(
    ("name" -> Schema[String]),
    ("age"  -> Schema[Int])
  )(RichSum.Person, RichSum.Person.unapply)

  val richSumSchema: Schema[RichSum] = Schema.Transform(
    Schema.enumeration(
      Map(
        "person" -> personSchema,
        "oneOf"  -> schemaOneOf,
        "long"   -> Schema[Long]
      )
    ),
    (value: Map[String, _]) => {
      value
        .get("person")
        .map(v => Right(v.asInstanceOf[RichSum.Person]))
        .orElse(value.get("oneOf").map(v => Right(RichSum.AnotherSum(v.asInstanceOf[OneOf]))))
        .orElse(value.get("long").map(v => Right(RichSum.LongWrapper(v.asInstanceOf[Long]))))
        .getOrElse(Left("No value found"))
    }, {
      case p: RichSum.Person         => Right(Map("person" -> p))
      case RichSum.AnotherSum(oneOf) => Right(Map("oneOf"  -> oneOf))
      case RichSum.LongWrapper(long) => Right(Map("long"   -> long))
    }
  )

  case class Enumeration(oneOf: OneOf)

  val schemaEnumeration: Schema[Enumeration] =
    Schema.caseClassN("value" -> schemaOneOf)(Enumeration, Enumeration.unapply)

  val schemaFail: Schema[StringValue] = Schema.fail("failing schema")

  case class RequestVars(someString: String, second: Int)

  val rvSchema: Schema[RequestVars] = Schema.caseClassN(
    "someString" -> Schema[String],
    "second"     -> Schema[Int]
  )(RequestVars, RequestVars.unapply)

  case class SearchRequest(query: String, pageNumber: RequestVars, resultPerPage: Int)

  val schema: Schema[SearchRequest] = Schema.caseClassN(
    "query"         -> Schema[String],
    "pageNumber"    -> rvSchema,
    "resultPerPage" -> Schema[Int]
  )(SearchRequest, SearchRequest.unapply)

  val message: SearchRequest = SearchRequest("bitcoins", RequestVars("varValue", 1), 100)

  def toHex(chunk: Chunk[Byte]): String =
    chunk.toArray.map("%02X".format(_)).mkString

  def fromHex(hex: String): Chunk[Byte] =
    Try(hex.split("(?<=\\G.{2})").map(Integer.parseInt(_, 16).toByte))
      .map(Chunk.fromArray)
      .getOrElse(Chunk.empty)

  def encode[A](schema: Schema[A], input: A): ZIO[Any, Nothing, Chunk[Byte]] =
    ZStream
      .succeed(input)
      .transduce(ProtobufCodec.encoder(schema))
      .run(ZSink.collectAll)

  //NS == non streaming variant of encode
  def encodeNS[A](schema: Schema[A], input: A): ZIO[Any, Nothing, Chunk[Byte]] =
    ZIO.succeed(ProtobufCodec.encode(schema)(input))

  def decode[A](schema: Schema[A], hex: String): ZIO[Any, String, Chunk[A]] =
    ZStream
      .fromChunk(fromHex(hex))
      .transduce(ProtobufCodec.decoder(schema))
      .run(ZSink.collectAll)

  //NS == non streaming variant of decode
  def decodeNS[A](schema: Schema[A], hex: String): ZIO[Any, String, A] =
    ZIO.succeed(ProtobufCodec.decode(schema)(fromHex(hex))).absolve[String, A]

  def encodeAndDecode[A](schema: Schema[A], input: A) =
    ZStream
      .succeed(input)
      .transduce(ProtobufCodec.encoder(schema))
      .transduce(ProtobufCodec.decoder(schema))
      .run(ZSink.collectAll)

  //NS == non streaming variant of encodeAndDecode
  def encodeAndDecodeNS[A](schema: Schema[A], input: A) =
    ZIO
      .succeed(input)
      .map(a => ProtobufCodec.encode(schema)(a))
      .map(ch => ProtobufCodec.decode(schema)(ch))
      .absolve
}
