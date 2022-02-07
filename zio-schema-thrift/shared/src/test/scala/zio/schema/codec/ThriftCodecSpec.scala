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
  ZoneId,
  ZoneOffset,
  ZonedDateTime
}
import java.util
import java.util.UUID

import scala.util.Try

import org.apache.thrift.TSerializable
import org.apache.thrift.protocol.{ TBinaryProtocol, TField, TType }

import zio.console.putStrLn
import zio.schema.CaseSet.caseOf
import zio.schema.codec.{ generated => g }
import zio.schema.{ CaseSet, DeriveSchema, Schema, SchemaGen, StandardType }
import zio.stream.{ ZSink, ZStream }
import zio.test.Assertion._
import zio.test._
import zio.{ Chunk, Task, ZIO }

// TODO: use generators instead of manual encode/decode

/**
 * Testing data were generated with thrift compiler
 *
 * cd zio-schema-thrift/shared/src/test                                                                                                                                                           ±[●●●][thrift]
 * thrift -r --gen java:generated_annotations=undated -out scala resources/testing-data.thrift
 */
object ThriftCodecSpec extends DefaultRunnableSpec {

  import Schema._

  def spec = suite("ThriftCodec Spec")(
    suite("Should correctly encode")(
      testM("integers") {
        for {
          e   <- encode(schemaBasicInt, BasicInt(150)).map(toHex)
          e2  <- encodeNS(schemaBasicInt, BasicInt(150)).map(toHex)
          res <- write(new g.BasicInt(150))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      testM("strings") {
        for {
          e   <- encode(schemaBasicString, BasicString("testing")).map(toHex)
          e2  <- encodeNS(schemaBasicString, BasicString("testing")).map(toHex)
          res <- write(new g.BasicString("testing"))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      testM("floats") {
        for {
          e  <- encode(schemaBasicFloat, BasicFloat(0.001f)).map(toHex)
          e2 <- encodeNS(schemaBasicFloat, BasicFloat(0.001f)).map(toHex)
          // there is no float in Thrift
          res <- write(new g.BasicDouble(0.001f))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      testM("doubles") {
        for {
          e   <- encode(schemaBasicDouble, BasicDouble(0.001)).map(toHex)
          e2  <- encodeNS(schemaBasicDouble, BasicDouble(0.001)).map(toHex)
          res <- write(new g.BasicDouble(0.001))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      testM("embedded messages") {
        for {
          e   <- encode(schemaEmbedded, Embedded(BasicInt(150))).map(toHex)
          e2  <- encodeNS(schemaEmbedded, Embedded(BasicInt(150))).map(toHex)
          res <- write(new g.Embedded(new g.BasicInt(150)))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      testM("int lists") {
        for {
          e   <- encode(schemaIntList, IntList(List(3, 270, 86942))).map(toHex)
          e2  <- encodeNS(schemaIntList, IntList(List(3, 270, 86942))).map(toHex)
          res <- write(new g.IntList(util.Arrays.asList[java.lang.Integer](3, 270, 86942)))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      testM("string lists") {
        for {
          e   <- encode(schemaStringList, StringList(List("foo", "bar", "baz"))).map(toHex)
          e2  <- encodeNS(schemaStringList, StringList(List("foo", "bar", "baz"))).map(toHex)
          res <- write(new g.StringList(util.Arrays.asList[String]("foo", "bar", "baz")))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      testM("records") {
        for {
          e   <- encode(Record.schemaRecord, Record("Foo", 123)).map(toHex)
          e2  <- encodeNS(Record.schemaRecord, Record("Foo", 123)).map(toHex)
          res <- write(new g.Record("Foo", 123))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      testM("enumerations") {
        for {

          e   <- encode(schemaEnumeration, Enumeration(IntValue(482))).map(toHex)
          e2  <- encodeNS(schemaEnumeration, Enumeration(IntValue(482))).map(toHex)
          res <- write(new g.Enumeration(g.OneOf.intValue(new generated.IntValue(482))))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      testM("enums unwrapped") {
        for {
          e   <- encode(schemaOneOf, IntValue(482)).map(toHex)
          e2  <- encodeNS(schemaOneOf, IntValue(482)).map(toHex)
          res <- write(g.OneOf.intValue(new generated.IntValue(482)))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      testM("map") {
        val m       = MapValue(Map("a" -> Record("Foo", 123), "b" -> Record("Bar", 456)))
        val javaMap = new util.HashMap[String, g.Record]()
        javaMap.put("a", new g.Record("Foo", 123))
        javaMap.put("b", new g.Record("Bar", 456))
        val m2 = new g.MapValue(javaMap)
        for {
          e   <- encodeNS(schemaMapValue, m).map(toHex)
          res <- write(m2)
        } yield assert(e)(equalTo(res))
      },
      testM("set") {
        val m       = SetValue(Set(Record("Foo", 123), Record("Bar", 456)))
        val javaSet = new util.HashSet[g.Record]()
        javaSet.add(new g.Record("Foo", 123))
        javaSet.add(new g.Record("Bar", 456))
        val m2 = new g.SetValue(javaSet)
        for {
          e   <- encodeNS(schemaSetValue, m).map(toHex)
          res <- write(m2)
        } yield assert(e)(equalTo(res))
      },
      testM("failure") {
        for {
          e  <- encode(schemaFail, StringValue("foo")).map(_.size)
          e2 <- encodeNS(schemaFail, StringValue("foo")).map(_.size)
        } yield assert(e)(equalTo(0)) && assert(e2)(equalTo(0))
      }
    ),
    suite("Should successfully encode and decode")(
      testM("empty list") {
        for {
          ed <- encodeAndDecodeNS(Schema.list[Int], List.empty)
        } yield assert(ed)(equalTo(List.empty))
      },
      testM("list of an empty list") {
        for {
          ed <- encodeAndDecodeNS(Schema[List[List[Int]]], List(List.empty))
        } yield assert(ed)(equalTo(List(List.empty)))
      },
      testM("tuple containing empty list & tuple containing list of an empty list") {
        val value: (String, List[List[Int]], String) = ("first string", List(List.empty), "second string")
        val value2: (String, List[Int], String)      = ("first string", List.empty, "second string")
        for {
          ed  <- encodeAndDecodeNS(DeriveSchema.gen[(String, List[List[Int]], String)], value)
          ed2 <- encodeAndDecodeNS(DeriveSchema.gen[(String, List[Int], String)], value2)
        } yield assert(ed)(equalTo(value)) && assert(ed2)(equalTo(value2))
      },
      testM("records") {
        for {
          ed2 <- encodeAndDecodeNS(Record.schemaRecord, Record("hello", 150))
        } yield assert(ed2)(equalTo(Record("hello", 150)))
      },
      testM("records with arity greater than 22") {
        for {
          e <- encode(schemaHighArityRecord, HighArity()).map(toHex)
          res <- write(
                  new generated.HighArity(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22,
                    23, 24)
                )
          ed <- encodeAndDecodeNS(schemaHighArityRecord, HighArity())
        } yield assert(ed)(equalTo(HighArity())) && assert(e)(equalTo(res))
      },
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
          ed2 <- encodeAndDecodeNS(SearchRequest.schema, message)
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
      testM("uuids") {
        val value = UUID.randomUUID
        for {
          ed  <- encodeAndDecode(Schema[UUID], value)
          ed2 <- encodeAndDecodeNS(Schema[UUID], value)
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
          ed  <- encodeAndDecode(Primitive(StandardType.InstantType(DateTimeFormatter.ISO_INSTANT)), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.InstantType(DateTimeFormatter.ISO_INSTANT)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("local dates") {
        val value = LocalDate.now()
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.LocalDateType(DateTimeFormatter.ISO_LOCAL_DATE)), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.LocalDateType(DateTimeFormatter.ISO_LOCAL_DATE)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("local times") {
        val value = LocalTime.now()
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.LocalTimeType(DateTimeFormatter.ISO_LOCAL_TIME)), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.LocalTimeType(DateTimeFormatter.ISO_LOCAL_TIME)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("local date times") {
        val value = LocalDateTime.now()
        for {
          ed <- encodeAndDecode(Primitive(StandardType.LocalDateTimeType(DateTimeFormatter.ISO_LOCAL_DATE_TIME)), value)
          ed2 <- encodeAndDecodeNS(
                  Primitive(StandardType.LocalDateTimeType(DateTimeFormatter.ISO_LOCAL_DATE_TIME)),
                  value
                )
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("offset times") {
        val value = OffsetTime.now()
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.OffsetTimeType(DateTimeFormatter.ISO_OFFSET_TIME)), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.OffsetTimeType(DateTimeFormatter.ISO_OFFSET_TIME)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("offset date times") {
        val value            = OffsetDateTime.now()
        val offsetDateSchema = Primitive(StandardType.OffsetDateTimeType(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
        for {
          ed  <- encodeAndDecode(offsetDateSchema, value)
          ed2 <- encodeAndDecodeNS(offsetDateSchema, value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("zoned date times") {
        val zoneSchema = Primitive(StandardType.ZonedDateTimeType(DateTimeFormatter.ISO_ZONED_DATE_TIME))
        val now        = ZonedDateTime.now()
        for {
          ed  <- encodeAndDecode(zoneSchema, now)
          ed2 <- encodeAndDecodeNS(zoneSchema, now)
        } yield assert(ed)(equalTo(Chunk(now))) && assert(ed2)(equalTo(now))
      },
      testM("primitive sequences") {
        val list = IntList(List(3, 270, 86942))
        for {
          ed  <- encodeAndDecode(schemaIntList, list)
          ed2 <- encodeAndDecodeNS(schemaIntList, list)
        } yield assert(ed)(equalTo(Chunk(list))) && assert(ed2)(equalTo(list))
      },
      testM("empty primitive sequence") {
        val list = IntList(List.empty)
        for {
          ed  <- encodeAndDecode(schemaIntList, list)
          ed2 <- encodeAndDecodeNS(schemaIntList, list)
        } yield assert(ed)(equalTo(Chunk(list))) && assert(ed2)(equalTo(list))
      },
      testM("string sequences") {
        val list = StringList(List("foo", "bar", "baz"))
        for {
          ed  <- encodeAndDecode(schemaStringList, list)
          ed2 <- encodeAndDecodeNS(schemaStringList, list)
        } yield assert(ed)(equalTo(Chunk(list))) && assert(ed2)(equalTo(list))
      },
      testM("empty string sequence") {
        val list = StringList(List.empty)
        for {
          ed  <- encodeAndDecode(schemaStringList, list)
          ed2 <- encodeAndDecodeNS(schemaStringList, list)
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
      testM("enumerations preserving type order") {
        for {
          s1 <- encodeAndDecode(schemaGenericEnumeration, "s")
          i1 <- encodeAndDecode(schemaGenericEnumeration, 1)
          s2 <- encodeAndDecode(schemaGenericEnumerationSorted, "s")
          i2 <- encodeAndDecode(schemaGenericEnumerationSorted, 1)
        } yield assert(s1)(equalTo(s2)) && assert(i1)(equalTo(i2))
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
          ed  <- encodeAndDecode(RichSum.richSumSchema, wrapper)
          ed2 <- encodeAndDecodeNS(RichSum.richSumSchema, oneOf)
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
        val value         = Some(Some(true))
        val valueSomeNone = Some(None)
        val valueNone     = None
        for {
          ed          <- encodeAndDecode(Schema.option(Schema.option(Schema[Boolean])), value)
          ed2         <- encodeAndDecodeNS(Schema.option(Schema.option(Schema[Boolean])), value)
          edSomeNone  <- encodeAndDecode(Schema.option(Schema.option(Schema[Boolean])), valueSomeNone)
          edSomeNone2 <- encodeAndDecodeNS(Schema.option(Schema.option(Schema[Boolean])), valueSomeNone)
          edNone      <- encodeAndDecode(Schema.option(Schema.option(Schema[Boolean])), valueNone)
          edNone2     <- encodeAndDecodeNS(Schema.option(Schema.option(Schema[Boolean])), valueNone)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value)) &&
          assert(edSomeNone)(equalTo(Chunk(valueSomeNone))) && assert(edSomeNone2)(equalTo(valueSomeNone)) &&
          assert(edNone)(equalTo(Chunk(valueNone))) && assert(edNone2)(equalTo(valueNone))
      },
      testM("option with omitted field") {
        // this behavior is useful for decoding classes which were generated by thrift compiler
        val value = ClassWithOption(123, None)
        for {
          bytesFieldOmitted <- writeManually { p =>
                                p.writeFieldBegin(new TField("number", TType.I32, 1))
                                p.writeI32(123)
                                p.writeFieldStop()
                              }
          d <- decodeNS(classWithOptionSchema, bytesFieldOmitted)
          bytesFieldPresent <- writeManually { p =>
                                p.writeFieldBegin(new TField("number", TType.I32, 1))
                                p.writeI32(123)
                                p.writeFieldBegin(new TField("name", TType.STRUCT, 2))
                                p.writeFieldBegin(new TField("", TType.VOID, 1))
                                p.writeFieldStop()
                                p.writeFieldStop()
                              }
          d2 <- decodeNS(classWithOptionSchema, bytesFieldPresent)
        } yield assert(d)(equalTo(value)) && assert(d2)(equalTo(value))
      },
      testM("product type with inner product type") {
        val richProduct = RichProduct(StringValue("sum_type"), BasicString("string"), Record("value", 47))
        for {
          ed  <- encodeAndDecode(richProductSchema, richProduct)
          ed2 <- encodeAndDecodeNS(richProductSchema, richProduct)
        } yield assert(ed)(equalTo(Chunk(richProduct))) &&
          assert(ed2)(equalTo(richProduct))
      },
      testM("complex sum type with nested product") {
        val richSum = RichSum.Person("hello", 10)
        for {
          ed  <- encodeAndDecode(RichSum.richSumSchema, richSum)
          ed2 <- encodeAndDecodeNS(RichSum.richSumSchema, richSum)
        } yield assert(ed)(equalTo(Chunk(richSum))) && assert(ed2)(equalTo(richSum))
      },
      testM("complex sum type with nested long primitive") {
        val long = RichSum.LongWrapper(100L)
        for {
          ed  <- encodeAndDecode(RichSum.richSumSchema, long)
          ed2 <- encodeAndDecodeNS(RichSum.richSumSchema, long)
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
          ed  <- encodeAndDecode(Schema.Optional(Record.schemaRecord), value)
          ed2 <- encodeAndDecodeNS(Schema.Optional(Record.schemaRecord), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      testM("optional of product type within optional") {
        val value = Some(Some(Record("hello", 10)))
        for {
          ed  <- encodeAndDecode(Schema.Optional(Schema.Optional(Record.schemaRecord)), value)
          ed2 <- encodeAndDecodeNS(Schema.Optional(Schema.Optional(Record.schemaRecord)), value)
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
      },
      testM("sequence of products") {
        val richSequence = SequenceOfProduct(
          "hello",
          List(Record("Jan", 30), Record("xxx", 40), Record("Peter", 22)),
          RichSum.LongWrapper(150L)
        )
        for {
          ed  <- encodeAndDecode(sequenceOfProductSchema, richSequence)
          ed2 <- encodeAndDecodeNS(sequenceOfProductSchema, richSequence)
        } yield assert(ed)(equalTo(Chunk(richSequence))) && assert(ed2)(equalTo(richSequence))
      },
      testM("sequence of sums") {
        val richSequence = SequenceOfSum("hello", List(RichSum.LongWrapper(150L), RichSum.LongWrapper(150L)))
        for {
          ed  <- encodeAndDecode(sequenceOfSumSchema, richSequence)
          ed2 <- encodeAndDecodeNS(sequenceOfSumSchema, richSequence)
        } yield assert(ed)(equalTo(Chunk(richSequence))) && assert(ed2)(equalTo(richSequence))
      },
      testM("map of products") {
        val m: Map[Record, MyRecord] = Map(
          Record("AAA", 1) -> MyRecord(1),
          Record("BBB", 2) -> MyRecord(2)
        )
        val mSchema = Schema.map(Record.schemaRecord, myRecord)
        for {
          ed  <- encodeAndDecode(mSchema, m)
          ed2 <- encodeAndDecodeNS(mSchema, m)
        } yield assert(ed)(equalTo(Chunk.succeed(m))) && assert(ed2)(equalTo(m))
      },
      testM("map in record") {
        val m = MapRecord(1, Map(1 -> "aaa", 3 -> "ccc"))
        for {
          ed  <- encodeAndDecode(schemaMapRecord, m)
          ed2 <- encodeAndDecodeNS(schemaMapRecord, m)
        } yield assert(ed)(equalTo(Chunk.succeed(m))) && assert(ed2)(equalTo(m))
      },
      testM("set of products") {
        val set: Set[Record] = Set(Record("AAA", 1), Record("BBB", 2))
        val setSchema        = Schema.set(Record.schemaRecord)

        for {
          ed  <- encodeAndDecode(setSchema, set)
          ed2 <- encodeAndDecodeNS(setSchema, set)
        } yield assert(ed)(equalTo(Chunk.succeed(set))) && assert(ed2)(equalTo(set))
      },
      testM("set in record") {
        val m = SetRecord(1, Set("aaa", "ccc"))
        for {
          ed  <- encodeAndDecode(schemaSetRecord, m)
          ed2 <- encodeAndDecodeNS(schemaSetRecord, m)
        } yield assert(ed)(equalTo(Chunk.succeed(m))) && assert(ed2)(equalTo(m))
      },
      testM("recursive data types") {
        checkM(SchemaGen.anyRecursiveTypeAndValue) {
          case (schema, value) =>
            for {
              ed  <- encodeAndDecode(schema, value)
              ed2 <- encodeAndDecodeNS(schema, value)
            } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
        }
      }
    ),
    suite("Should successfully decode")(
      testM("empty input") {
        assertM(decode(Schema[Int], ""))(
          equalTo(Chunk.empty)
        )
      },
      testM("empty input by non streaming variant") {
        assertM(decodeNS(Schema[Int], "").run)(
          fails(equalTo("No bytes to decode"))
        )
      },
      testM("thrift enum value as an integer") {
        for {
          encoded <- write(new g.EnumValue(g.Color.BLUE))
          ed      <- decodeNS(schemaEnumValue, encoded)
        } yield assert(ed)(equalTo(EnumValue(2)))
      }
    ),
    suite("Should fail to decode")(
      testM("field begin") {
        for {
          d  <- decode(Record.schemaRecord, "0F").run
          d2 <- decodeNS(Record.schemaRecord, "0F").run
        } yield assert(d)(fails(equalTo("Error at path /: Error reading field begin"))) &&
          assert(d2)(fails(equalTo("Error at path /: Error reading field begin")))
      },
      testM("missing value") {
        for {
          bytes <- writeManually { p =>
                    p.writeFieldBegin(new TField("name", TType.STRING, 1))
                    p.writeString("Dan")
                    p.writeFieldStop()
                  }
          d <- decode(Record.schemaRecord, bytes).run
          bytes2 <- writeManually { p =>
                     p.writeFieldBegin(new TField("value", TType.I32, 2))
                     p.writeI32(123)
                     p.writeFieldStop()
                   }
          d2 <- decode(Record.schemaRecord, bytes2).run
        } yield assert(d)(fails(equalTo("Error at path /value: Missing value"))) &&
          assert(d2)(fails(equalTo("Error at path /name: Missing value")))
      },
      testM("unable to decode") {
        for {
          bytes <- writeManually { p =>
                    p.writeFieldBegin(new TField("name", TType.STRING, 1))
                    p.writeString("Dan")
                    p.writeFieldBegin(new TField("value", TType.I32, 2))
                    p.writeFieldStop()
                  }
          d <- decode(Record.schemaRecord, bytes).run
        } yield assert(d)(fails(equalTo("Error at path /fieldId:2: Unable to decode Int")))
      },
      testM("unknown type") {
        for {
          bytes <- writeManually { p =>
                    p.writeFieldBegin(new TField("value", TType.I32, 2))
                    p.writeString("This is number one bullshit")
                    p.writeFieldStop()
                  }
          d <- decode(Record.schemaRecord, bytes).run
        } yield assert(d)(fails(equalTo("Error at path /fieldId:26729: Unknown type 84")))
      }
    )
  )

  def writeManually(f: TBinaryProtocol => Unit): Task[String] = Task {
    val writeRecord = new ChunkTransport.Write()
    f(new TBinaryProtocol(writeRecord))
    toHex(writeRecord.chunk)
  }

  def write(serializable: TSerializable): Task[String] =
    writeManually(serializable.write(_))

  // some tests are based on https://developers.google.com/protocol-buffers/docs/encoding
  case class BasicInt(value: Int)

  case class BasicTwoInts(value1: Int, value2: Int)

  lazy val schemaBasicTwoInts: Schema[BasicTwoInts] = DeriveSchema.gen[BasicTwoInts]

  lazy val schemaBasicInt: Schema[BasicInt] = DeriveSchema.gen[BasicInt]

  case class BasicTwoIntWrapper(basic: BasicTwoInts)

  case class BasicIntWrapper(basic: BasicInt)

  case class SeparateWrapper(basic1: BasicInt, basic2: BasicInt)

  lazy val basicIntWrapperSchema: Schema[BasicIntWrapper] = DeriveSchema.gen[BasicIntWrapper]

  lazy val basicTwoIntWrapperSchema: Schema[BasicTwoIntWrapper] = DeriveSchema.gen[BasicTwoIntWrapper]

  case class BasicString(value: String)

  lazy val schemaBasicString: Schema[BasicString] = DeriveSchema.gen[BasicString]

  lazy val separateWrapper: Schema[SeparateWrapper] = DeriveSchema.gen[SeparateWrapper]

  case class BasicFloat(value: Float)

  lazy val schemaBasicFloat: Schema[BasicFloat] = DeriveSchema.gen[BasicFloat]

  case class BasicDouble(value: Double)

  lazy val schemaBasicDouble: Schema[BasicDouble] = DeriveSchema.gen[BasicDouble]

  case class Embedded(embedded: BasicInt)

  lazy val schemaEmbedded: Schema[Embedded] = DeriveSchema.gen[Embedded]

  case class IntList(items: List[Int])

  lazy val schemaIntList: Schema[IntList] = DeriveSchema.gen[IntList]

  case class StringList(items: List[String])

  lazy val schemaStringList: Schema[StringList] = DeriveSchema.gen[StringList]

  case class EnumValue(value: Int)

  lazy val schemaEnumValue: Schema[EnumValue] = DeriveSchema.gen[EnumValue]

  case class Record(name: String, value: Int)

  object Record {
    implicit val schemaRecord: Schema[Record] = DeriveSchema.gen[Record]
  }

  val schemaTuple: Schema.Tuple[Int, String] = Schema.Tuple(Schema[Int], Schema[String])

  case class MapValue(value: Map[String, Record])

  val schemaMapValue: Schema[MapValue] = DeriveSchema.gen[MapValue]

  case class SetValue(value: Set[Record])

  val schemaSetValue: Schema[SetValue] = DeriveSchema.gen[SetValue]

  sealed trait OneOf

  case class StringValue(value: String) extends OneOf

  case class IntValue(value: Int) extends OneOf

  case class BooleanValue(value: Boolean) extends OneOf

  case class HighArity(
    f1: Int = 1,
    f2: Int = 2,
    f3: Int = 3,
    f4: Int = 4,
    f5: Int = 5,
    f6: Int = 6,
    f7: Int = 7,
    f8: Int = 8,
    f9: Int = 9,
    f10: Int = 10,
    f11: Int = 11,
    f12: Int = 12,
    f13: Int = 13,
    f14: Int = 14,
    f15: Int = 15,
    f16: Int = 16,
    f17: Int = 17,
    f18: Int = 18,
    f19: Int = 19,
    f20: Int = 20,
    f21: Int = 21,
    f22: Int = 22,
    f23: Int = 23,
    f24: Int = 24
  )

  lazy val schemaHighArityRecord: Schema[HighArity] = DeriveSchema.gen[HighArity]

  lazy val schemaOneOf: Schema[OneOf] = DeriveSchema.gen[OneOf]

  case class MyRecord(age: Int)

  lazy val myRecord: Schema[MyRecord] = DeriveSchema.gen[MyRecord]

  case class MapRecord(age: Int, map: Map[Int, String])

  lazy val schemaMapRecord: Schema[MapRecord] = DeriveSchema.gen[MapRecord]

  case class SetRecord(age: Int, set: Set[String])

  lazy val schemaSetRecord: Schema[SetRecord] = DeriveSchema.gen[SetRecord]

  val complexTupleSchema: Schema.Tuple[Record, OneOf] = Schema.Tuple(Record.schemaRecord, schemaOneOf)

  val eitherSchema: Schema.EitherSchema[Int, String] = Schema.EitherSchema(Schema[Int], Schema[String])

  val complexEitherSchema: Schema.EitherSchema[Record, OneOf] =
    Schema.EitherSchema(Record.schemaRecord, schemaOneOf)

  val complexEitherSchema2: Schema.EitherSchema[MyRecord, MyRecord] =
    Schema.EitherSchema(myRecord, myRecord)

  case class RichProduct(stringOneOf: OneOf, basicString: BasicString, record: Record)

  lazy val richProductSchema: Schema[RichProduct] = DeriveSchema.gen[RichProduct]

  sealed trait RichSum

  object RichSum {
    case class Person(name: String, age: Int) extends RichSum

    case class AnotherSum(oneOf: OneOf) extends RichSum

    case class LongWrapper(long: Long) extends RichSum

    implicit val richSumSchema: Schema[RichSum] = DeriveSchema.gen[RichSum]
  }

  case class Enumeration(oneOf: OneOf)

  lazy val schemaEnumeration: Schema[Enumeration] = DeriveSchema.gen[Enumeration]

  lazy val schemaGenericEnumeration: Schema[Any] = Schema.enumeration[Any, CaseSet.Aux[Any]](
    caseOf[String, Any]("string")(_.asInstanceOf[String]) ++ caseOf[Int, Any]("int")(_.asInstanceOf[Int])
  )

  lazy val schemaGenericEnumerationSorted: Schema[Any] = Schema.enumeration[Any, CaseSet.Aux[Any]](
    caseOf[Int, Any]("int")(_.asInstanceOf[Int]) ++ caseOf[String, Any]("string")(_.asInstanceOf[String])
  )

  val schemaFail: Schema[StringValue] = Schema.fail("failing schema")

  case class RequestVars(someString: String, second: Int)

  lazy val rvSchema: Schema[RequestVars] = DeriveSchema.gen[RequestVars]

  case class SearchRequest(query: String, pageNumber: RequestVars, resultPerPage: Int)

  object SearchRequest {
    implicit val schema: Schema[SearchRequest] = DeriveSchema.gen[SearchRequest]
  }

  val message: SearchRequest = SearchRequest("bitcoins", RequestVars("varValue", 1), 100)

  case class ClassWithOption(number: Int, name: Option[String])

  lazy val classWithOptionSchema: Schema[ClassWithOption] = DeriveSchema.gen[ClassWithOption]

  case class SequenceOfProduct(name: String, records: List[Record], richSum: RichSum)

  case class SequenceOfSum(value: String, enums: List[RichSum])

  lazy val sequenceOfProductSchema: Schema[SequenceOfProduct] = DeriveSchema.gen[SequenceOfProduct]

  lazy val sequenceOfSumSchema: Schema[SequenceOfSum] = DeriveSchema.gen[SequenceOfSum]

  def toHex(chunk: Chunk[Byte]): String =
    chunk.toArray.map("%02X".format(_)).mkString

  def fromHex(hex: String): Chunk[Byte] =
    Try(hex.split("(?<=\\G.{2})").map(Integer.parseInt(_, 16).toByte))
      .map(Chunk.fromArray)
      .getOrElse(Chunk.empty)

  def encode[A](schema: Schema[A], input: A): ZIO[Any, Nothing, Chunk[Byte]] =
    ZStream
      .succeed(input)
      .transduce(ThriftCodec.encoder(schema))
      .run(ZSink.collectAll)

  //NS == non streaming variant of encode
  def encodeNS[A](schema: Schema[A], input: A): ZIO[Any, Nothing, Chunk[Byte]] =
    ZIO.succeed(ThriftCodec.encode(schema)(input))

  def decode[A](schema: Schema[A], hex: String): ZIO[Any, String, Chunk[A]] =
    ZStream
      .fromChunk(fromHex(hex))
      .transduce(ThriftCodec.decoder(schema))
      .run(ZSink.collectAll)

  //NS == non streaming variant of decode
  def decodeNS[A](schema: Schema[A], hex: String): ZIO[Any, String, A] =
    ZIO.succeed(ThriftCodec.decode(schema)(fromHex(hex))).absolve[String, A]

  def encodeAndDecode[A](schema: Schema[A], input: A): ZIO[Any, String, Chunk[A]] =
    ZStream
      .succeed(input)
      .transduce(ThriftCodec.encoder(schema))
      .transduce(ThriftCodec.decoder(schema))
      .run(ZSink.collectAll)

  def encodeAndDecode[A](encodeSchema: Schema[A], decodeSchema: Schema[A], input: A): ZIO[Any, String, Chunk[A]] =
    ZStream
      .succeed(input)
      .transduce(ThriftCodec.encoder(encodeSchema))
      .transduce(ThriftCodec.decoder(decodeSchema))
      .run(ZSink.collectAll)

  //NS == non streaming variant of encodeAndDecode
  def encodeAndDecodeNS[A](schema: Schema[A], input: A, print: Boolean = false): ZIO[zio.console.Console, String, A] =
    ZIO
      .succeed(input)
      .tap(value => putStrLn(s"Input Value: $value").when(print).ignore)
      .map(a => ThriftCodec.encode(schema)(a))
      .tap(encoded => putStrLn(s"\nEncoded Bytes:\n${toHex(encoded)}").when(print).ignore)
      .map(ch => ThriftCodec.decode(schema)(ch))
      .absolve

  def encodeAndDecodeNS[A](encodeSchema: Schema[A], decodeSchema: Schema[A], input: A): ZIO[Any, String, A] =
    ZIO
      .succeed(input)
      .map(a => ThriftCodec.encode(encodeSchema)(a))
      .map(ch => ThriftCodec.decode(decodeSchema)(ch))
      .absolve

}
