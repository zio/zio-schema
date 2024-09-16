package zio.schema.codec

import java.time._
import java.util.UUID

import scala.collection.immutable.ListMap
import scala.util.Try

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.module.scala.DefaultScalaModule
import org.msgpack.core.{ MessagePack, MessagePacker }
import org.msgpack.jackson.dataformat.MessagePackFactory

import zio.schema.CaseSet.caseOf
import zio.schema._
import zio.stream.{ ZSink, ZStream }
import zio.test.Assertion._
import zio.test._
import zio.{ Chunk, Console, Scope, Task, ZIO }

object MessagePackCodecSpec extends ZIOSpecDefault {

  import Schema._

  val objectMapper = new ObjectMapper(new MessagePackFactory())
  objectMapper.registerModule(DefaultScalaModule)

  def spec: Spec[TestEnvironment with Scope, Any] = suite("MessagePackCodec Spec")(
    suite("Should correctly encode")(
      test("integers") {
        for {
          e   <- encode(schemaBasicInt, BasicInt(150)).map(toHex)
          e2  <- encodeNS(schemaBasicInt, BasicInt(150)).map(toHex)
          res <- write(BasicInt(150))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      test("strings") {
        for {
          e   <- encode(schemaBasicString, BasicString("testing")).map(toHex)
          e2  <- encodeNS(schemaBasicString, BasicString("testing")).map(toHex)
          res <- write(BasicString("testing"))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      test("floats") {
        for {
          e   <- encode(schemaBasicFloat, BasicFloat(0.001f)).map(toHex)
          e2  <- encodeNS(schemaBasicFloat, BasicFloat(0.001f)).map(toHex)
          res <- write(BasicFloat(0.001f))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      test("doubles") {
        for {
          e   <- encode(schemaBasicDouble, BasicDouble(0.001)).map(toHex)
          e2  <- encodeNS(schemaBasicDouble, BasicDouble(0.001)).map(toHex)
          res <- write(BasicDouble(0.001))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      test("embedded messages") {
        for {
          e   <- encode(schemaEmbedded, Embedded(BasicInt(150))).map(toHex)
          e2  <- encodeNS(schemaEmbedded, Embedded(BasicInt(150))).map(toHex)
          res <- write(Embedded(BasicInt(150)))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      test("int lists") {
        for {
          e   <- encode(schemaIntList, IntList(List(3, 270, 86942))).map(toHex)
          e2  <- encodeNS(schemaIntList, IntList(List(3, 270, 86942))).map(toHex)
          res <- write(IntList(List(3, 270, 86942)))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      test("string lists") {
        for {
          e   <- encode(schemaStringList, StringList(List("foo", "bar", "baz"))).map(toHex)
          e2  <- encodeNS(schemaStringList, StringList(List("foo", "bar", "baz"))).map(toHex)
          res <- write(StringList(List("foo", "bar", "baz")))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      test("records") {
        for {
          e   <- encode(Record.schemaRecord, Record("Foo", 123)).map(toHex)
          e2  <- encodeNS(Record.schemaRecord, Record("Foo", 123)).map(toHex)
          res <- write(Record("Foo", 123))
        } yield assert(e)(equalTo(res)) && assert(e2)(equalTo(res))
      },
      test("map") {
        val m = MapValue(scala.collection.immutable.Map("a" -> Record("Foo", 123), "b" -> Record("Bar", 456)))
        for {
          e   <- encodeNS(schemaMapValue, m).map(toHex)
          res <- write(m)
        } yield assert(e)(equalTo(res))
      },
      test("set") {
        val m = SetValue(scala.collection.immutable.Set(Record("Foo", 123), Record("Bar", 456)))
        for {
          e   <- encodeNS(schemaSetValue, m).map(toHex)
          res <- write(m)
        } yield assert(e)(equalTo(res))
      },
      test("failure") {
        for {
          e  <- encode(schemaFail, StringValue("foo")).map(_.size)
          e2 <- encodeNS(schemaFail, StringValue("foo")).map(_.size)
        } yield assert(e)(equalTo(0)) && assert(e2)(equalTo(0))
      }
    ),
    suite("Should successfully encode and decode")(
      test("empty list") {
        for {
          ed <- encodeAndDecodeNS(Schema.list[Int], List.empty)
        } yield assert(ed)(equalTo(List.empty))
      },
      test("list of an empty list") {
        for {
          ed <- encodeAndDecodeNS(Schema[List[List[Int]]], List(List.empty))
        } yield assert(ed)(equalTo(List(List.empty)))
      },
      test("tuple containing empty list & tuple containing list of an empty list") {
        val value: (String, List[List[Int]], String) = ("first string", List(List.empty), "second string")
        val value2: (String, List[Int], String)      = ("first string", List.empty, "second string")
        for {
          ed  <- encodeAndDecodeNS(DeriveSchema.gen[(String, List[List[Int]], String)], value)
          ed2 <- encodeAndDecodeNS(DeriveSchema.gen[(String, List[Int], String)], value2)
        } yield assert(ed)(equalTo(value)) && assert(ed2)(equalTo(value2))
      },
      test("records") {
        for {
          ed2 <- encodeAndDecodeNS(Record.schemaRecord, Record("hello", 150))
        } yield assert(ed2)(equalTo(Record("hello", 150)))
      },
      test("records with arity greater than 22") {
        for {
          e   <- encode(schemaHighArityRecord, HighArity()).map(toHex)
          res <- write(HighArity())
          ed  <- encodeAndDecodeNS(schemaHighArityRecord, HighArity())
        } yield assert(ed)(equalTo(HighArity())) && assert(e)(equalTo(res))
      },
      test("case classes with arity 22") {
        for {
          e   <- encode(schemaMaxArityCaseClass, MaxArityCaseClass()).map(toHex)
          res <- write(MaxArityCaseClass())
          ed  <- encodeAndDecodeNS(schemaMaxArityCaseClass, MaxArityCaseClass())
        } yield assert(ed)(equalTo(MaxArityCaseClass())) && assert(e)(equalTo(res))
      },
      test("integer") {
        for {
          ed2 <- encodeAndDecodeNS(schemaBasicInt, BasicInt(150))
        } yield assert(ed2)(equalTo(BasicInt(150)))
      },
      test("integer inside wrapper class") {
        for {
          ed2 <- encodeAndDecodeNS(basicIntWrapperSchema, BasicIntWrapper(BasicInt(150)))
        } yield assert(ed2)(equalTo(BasicIntWrapper(BasicInt(150))))
      },
      test("two integers") {
        for {
          ed2 <- encodeAndDecodeNS(schemaBasicTwoInts, BasicTwoInts(150, 151))
        } yield assert(ed2)(equalTo(BasicTwoInts(150, 151)))
      },
      test("two integers inside wrapper class") {
        for {
          ed2 <- encodeAndDecodeNS(basicTwoIntWrapperSchema, BasicTwoIntWrapper(BasicTwoInts(150, 151)))
        } yield assert(ed2)(equalTo(BasicTwoIntWrapper(BasicTwoInts(150, 151))))
      },
      test("two wrapped integers inside wrapper class") {
        for {
          e2 <- encodeAndDecodeNS(separateWrapper, SeparateWrapper(BasicInt(150), BasicInt(151)))
        } yield assert(e2)(equalTo(SeparateWrapper(BasicInt(150), BasicInt(151))))
      },
      test("complex product and string and integer") {
        for {
          ed2 <- encodeAndDecodeNS(SearchRequest.schema, message)
        } yield assert(ed2)(equalTo(message))
      },
      test("booleans") {
        val value = true
        for {
          ed  <- encodeAndDecode(Schema[Boolean], value)
          ed2 <- encodeAndDecodeNS(Schema[Boolean], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("shorts") {
        val value = 5.toShort
        for {
          ed  <- encodeAndDecode(Schema[Short], value)
          ed2 <- encodeAndDecodeNS(Schema[Short], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("longs") {
        val value = 1000L
        for {
          ed  <- encodeAndDecode(Schema[Long], value)
          ed2 <- encodeAndDecodeNS(Schema[Long], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("floats") {
        val value = 0.001f
        for {
          ed  <- encodeAndDecode(Schema[Float], value)
          ed2 <- encodeAndDecodeNS(Schema[Float], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("doubles") {
        val value = 0.001
        for {
          ed  <- encodeAndDecode(Schema[Double], value)
          ed2 <- encodeAndDecodeNS(Schema[Double], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("bytes") {
        val value = Chunk.fromArray("some bytes".getBytes)
        for {
          ed  <- encodeAndDecode(Schema[Chunk[Byte]], value)
          ed2 <- encodeAndDecodeNS(Schema[Chunk[Byte]], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("chars") {
        val value = 'c'
        for {
          ed  <- encodeAndDecode(Schema[Char], value)
          ed2 <- encodeAndDecodeNS(Schema[Char], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("uuids") {
        val value = UUID.randomUUID
        for {
          ed  <- encodeAndDecode(Schema[UUID], value)
          ed2 <- encodeAndDecodeNS(Schema[UUID], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("currencies") {
        val value = java.util.Currency.getInstance("USD")
        for {
          ed  <- encodeAndDecode(Schema[java.util.Currency], value)
          ed2 <- encodeAndDecodeNS(Schema[java.util.Currency], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("day of weeks") {
        val value = DayOfWeek.of(3)
        for {
          ed  <- encodeAndDecode(Schema[DayOfWeek], value)
          ed2 <- encodeAndDecodeNS(Schema[DayOfWeek], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("months") {
        val value = Month.of(3)
        for {
          ed  <- encodeAndDecode(Schema[Month], value)
          ed2 <- encodeAndDecodeNS(Schema[Month], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("month days") {
        val value = MonthDay.of(1, 31)
        for {
          ed  <- encodeAndDecode(Schema[MonthDay], value)
          ed2 <- encodeAndDecodeNS(Schema[MonthDay], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("periods") {
        val value = Period.of(5, 3, 1)
        for {
          ed  <- encodeAndDecode(Schema[Period], value)
          ed2 <- encodeAndDecodeNS(Schema[Period], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("years") {
        val value = Year.of(2020)
        for {
          ed  <- encodeAndDecode(Schema[Year], value)
          ed2 <- encodeAndDecodeNS(Schema[Year], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("year months") {
        val value = YearMonth.of(2020, 5)
        for {
          ed  <- encodeAndDecode(Schema[YearMonth], value)
          ed2 <- encodeAndDecodeNS(Schema[YearMonth], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("zone ids") {
        val value = ZoneId.systemDefault()
        for {
          ed  <- encodeAndDecode(Schema[ZoneId], value)
          ed2 <- encodeAndDecodeNS(Schema[ZoneId], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("zone offsets") {
        val value = ZoneOffset.ofHours(6)
        for {
          ed  <- encodeAndDecode(Schema[ZoneOffset], value)
          ed2 <- encodeAndDecodeNS(Schema[ZoneOffset], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("durations") {
        val value = Duration.ofDays(12)
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.DurationType), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.DurationType), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("instants") {
        val value = Instant.now()
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.InstantType), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.InstantType), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("local dates") {
        val value = LocalDate.now()
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.LocalDateType), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.LocalDateType), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("local times") {
        val value = LocalTime.now()
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.LocalTimeType), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.LocalTimeType), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("local date times") {
        val value = LocalDateTime.now()
        for {
          ed <- encodeAndDecode(Primitive(StandardType.LocalDateTimeType), value)
          ed2 <- encodeAndDecodeNS(
                  Primitive(StandardType.LocalDateTimeType),
                  value
                )
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("offset times") {
        val value = OffsetTime.now()
        for {
          ed  <- encodeAndDecode(Primitive(StandardType.OffsetTimeType), value)
          ed2 <- encodeAndDecodeNS(Primitive(StandardType.OffsetTimeType), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("offset date times") {
        val value            = OffsetDateTime.now()
        val offsetDateSchema = Primitive(StandardType.OffsetDateTimeType)
        for {
          ed  <- encodeAndDecode(offsetDateSchema, value)
          ed2 <- encodeAndDecodeNS(offsetDateSchema, value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("zoned date times") {
        val zoneSchema = Primitive(StandardType.ZonedDateTimeType)
        val now        = ZonedDateTime.now()
        for {
          ed  <- encodeAndDecode(zoneSchema, now)
          ed2 <- encodeAndDecodeNS(zoneSchema, now)
        } yield assert(ed)(equalTo(Chunk(now))) && assert(ed2)(equalTo(now))
      },
      test("primitive sequences") {
        val list = IntList(List(3, 270, 86942))
        for {
          ed  <- encodeAndDecode(schemaIntList, list)
          ed2 <- encodeAndDecodeNS(schemaIntList, list)
        } yield assert(ed)(equalTo(Chunk(list))) && assert(ed2)(equalTo(list))
      },
      test("empty primitive sequence") {
        val list = IntList(List.empty)
        for {
          ed  <- encodeAndDecode(schemaIntList, list)
          ed2 <- encodeAndDecodeNS(schemaIntList, list)
        } yield assert(ed)(equalTo(Chunk(list))) && assert(ed2)(equalTo(list))
      },
      test("string sequences") {
        val list = StringList(List("foo", "bar", "baz"))
        for {
          ed  <- encodeAndDecode(schemaStringList, list)
          ed2 <- encodeAndDecodeNS(schemaStringList, list)
        } yield assert(ed)(equalTo(Chunk(list))) && assert(ed2)(equalTo(list))
      },
      test("empty string sequence") {
        val list = StringList(List.empty)
        for {
          ed  <- encodeAndDecode(schemaStringList, list)
          ed2 <- encodeAndDecodeNS(schemaStringList, list)
        } yield assert(ed)(equalTo(Chunk(list))) && assert(ed2)(equalTo(list))
      },
      test("enumerations") {
        for {
          ed  <- encodeAndDecode(schemaEnumeration, Enumeration(BooleanValue(true)))
          ed2 <- encodeAndDecodeNS(schemaEnumeration, Enumeration(IntValue(482)))
        } yield assert(ed)(equalTo(Chunk(Enumeration(BooleanValue(true))))) && assert(ed2)(
          equalTo(Enumeration(IntValue(482)))
        )
      },
      test("enumerations preserving type order") {
        for {
          s1 <- encodeAndDecode(schemaGenericEnumeration, "s")
          i1 <- encodeAndDecode(schemaGenericEnumeration, 1)
          s2 <- encodeAndDecode(schemaGenericEnumerationSorted, "s")
          i2 <- encodeAndDecode(schemaGenericEnumerationSorted, 1)
        } yield assert(s1)(equalTo(s2)) && assert(i1)(equalTo(i2))
      },
      test("enums unwrapped") {
        for {
          ed  <- encodeAndDecode(schemaOneOf, BooleanValue(true))
          ed2 <- encodeAndDecodeNS(schemaOneOf, BooleanValue(true))
        } yield assert(ed)(equalTo(Chunk(BooleanValue(true)))) && assert(ed2)(
          equalTo(BooleanValue(true))
        )
      },
      test("enum within enum") {
        val oneOf   = RichSum.AnotherSum(BooleanValue(false))
        val wrapper = RichSum.LongWrapper(150L)
        for {
          ed  <- encodeAndDecode(RichSum.richSumSchema, wrapper)
          ed2 <- encodeAndDecodeNS(RichSum.richSumSchema, oneOf)
        } yield assert(ed)(equalTo(Chunk(wrapper))) && assert(ed2)(equalTo(oneOf))
      },
      test("tuples") {
        val value = (123, "foo")
        for {
          ed  <- encodeAndDecode(schemaTuple, value)
          ed2 <- encodeAndDecodeNS(schemaTuple, value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("either left") {
        val either = Left(9)
        for {
          ed  <- encodeAndDecode(eitherSchema, either)
          ed2 <- encodeAndDecodeNS(eitherSchema, either)
        } yield assert(ed)(equalTo(Chunk(either))) && assert(ed2)(equalTo(either))
      },
      test("either right") {
        val either = Right("hello")
        for {
          ed  <- encodeAndDecode(eitherSchema, either)
          ed2 <- encodeAndDecodeNS(eitherSchema, either)
        } yield assert(ed)(equalTo(Chunk(either))) && assert(ed2)(equalTo(either))
      },
      test("either with product type") {
        val eitherLeft = Left(MyRecord(150))
        for {
          ed  <- encodeAndDecode(complexEitherSchema2, eitherLeft)
          ed2 <- encodeAndDecodeNS(complexEitherSchema2, eitherLeft)
        } yield assert(ed)(equalTo(Chunk(eitherLeft))) && assert(ed2)(equalTo(eitherLeft))
      },
      test("either with sum type") {
        val eitherRight  = Right(BooleanValue(true))
        val eitherRight2 = Right(StringValue("hello"))
        for {
          ed  <- encodeAndDecode(complexEitherSchema, eitherRight2)
          ed2 <- encodeAndDecodeNS(complexEitherSchema, eitherRight)
        } yield assert(ed)(equalTo(Chunk(eitherRight2))) && assert(ed2)(equalTo(eitherRight))
      },
      test("fallback left full decode") {
        val fallback = zio.schema.Fallback.Left(9)
        for {
          ed  <- encodeAndDecode(fallbackSchema1, fallback)
          ed2 <- encodeAndDecodeNS(fallbackSchema1, fallback)
        } yield assert(ed)(equalTo(Chunk(fallback))) && assert(ed2)(equalTo(fallback))
      },
      test("fallback left non full decode") {
        val fallback = zio.schema.Fallback.Left(9)
        for {
          ed  <- encodeAndDecode(fallbackSchema2, fallback)
          ed2 <- encodeAndDecodeNS(fallbackSchema2, fallback)
        } yield assert(ed)(equalTo(Chunk(fallback))) && assert(ed2)(equalTo(fallback))
      },
      test("fallback right full decode") {
        val fallback = zio.schema.Fallback.Right("hello")
        for {
          ed  <- encodeAndDecode(fallbackSchema1, fallback)
          ed2 <- encodeAndDecodeNS(fallbackSchema1, fallback)
        } yield assert(ed)(equalTo(Chunk(fallback))) && assert(ed2)(equalTo(fallback))
      },
      test("fallback right non full decode") {
        val fallback = zio.schema.Fallback.Right("hello")
        for {
          ed  <- encodeAndDecode(fallbackSchema2, fallback)
          ed2 <- encodeAndDecodeNS(fallbackSchema2, fallback)
        } yield assert(ed)(equalTo(Chunk(fallback))) && assert(ed2)(equalTo(fallback))
      },
      test("fallback both full decode") {
        val fallback = zio.schema.Fallback.Both(2, "hello")
        for {
          ed  <- encodeAndDecode(fallbackSchema1, fallback)
          ed2 <- encodeAndDecodeNS(fallbackSchema1, fallback)
        } yield assert(ed)(equalTo(Chunk(fallback))) && assert(ed2)(equalTo(fallback))
      },
      test("fallback both non full decode") {
        val fallback = zio.schema.Fallback.Both(2, "hello")
        for {
          ed  <- encodeAndDecode(fallbackSchema2, fallback)
          ed2 <- encodeAndDecodeNS(fallbackSchema2, fallback)
        } yield assert(ed)(equalTo(Chunk(fallback.simplify))) && assert(ed2)(equalTo(fallback.simplify))
      },
      test("fallback with product type") {
        val fallbackLeft = zio.schema.Fallback.Left(MyRecord(150))
        for {
          ed  <- encodeAndDecode(complexFallbackSchema2, fallbackLeft)
          ed2 <- encodeAndDecodeNS(complexFallbackSchema2, fallbackLeft)
        } yield assert(ed)(equalTo(Chunk(fallbackLeft))) && assert(ed2)(equalTo(fallbackLeft))
      },
      test("fallback with sum type") {
        val fallbackRight  = zio.schema.Fallback.Right(BooleanValue(true))
        val fallbackRight2 = zio.schema.Fallback.Right(StringValue("hello"))
        for {
          ed  <- encodeAndDecode(complexFallbackSchema, fallbackRight2)
          ed2 <- encodeAndDecodeNS(complexFallbackSchema, fallbackRight)
        } yield assert(ed)(equalTo(Chunk(fallbackRight2))) && assert(ed2)(equalTo(fallbackRight))
      },
      test("optionals") {
        val value = Some(123)
        for {
          ed  <- encodeAndDecode(Schema.Optional(Schema[Int]), value)
          ed2 <- encodeAndDecodeNS(Schema.Optional(Schema[Int]), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("complex optionals with sum type") {
        val value = Some(BooleanValue(true))
        for {
          ed  <- encodeAndDecode(Schema.Optional(schemaOneOf), value)
          ed2 <- encodeAndDecodeNS(Schema.Optional(schemaOneOf), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("option within option") {
        val value         = Some(Some(true))
        val valueSomeNone = Some(None)
        val valueNone     = None
        val scheme        = Schema.option(Schema.option(Schema[Boolean]))
        for {
          ed          <- encodeAndDecode(scheme, value)
          ed2         <- encodeAndDecodeNS(scheme, value)
          edSomeNone  <- encodeAndDecode(scheme, valueSomeNone)
          edSomeNone2 <- encodeAndDecodeNS(scheme, valueSomeNone)
          edNone      <- encodeAndDecode(scheme, valueNone)
          edNone2     <- encodeAndDecodeNS(scheme, valueNone)
        } yield assert(ed)(equalTo(Chunk(value))) &&
          assert(ed2)(equalTo(value)) &&
          assert(edSomeNone)(equalTo(Chunk(valueSomeNone))) &&
          assert(edSomeNone2)(equalTo(valueSomeNone)) &&
          assert(edNone)(equalTo(Chunk(valueNone))) &&
          assert(edNone2)(equalTo(valueNone))
      },
      test("product type with inner product type") {
        val richProduct = RichProduct(StringValue("sum_type"), BasicString("string"), Record("value", 47))
        for {
          ed  <- encodeAndDecode(richProductSchema, richProduct)
          ed2 <- encodeAndDecodeNS(richProductSchema, richProduct)
        } yield assert(ed)(equalTo(Chunk(richProduct))) &&
          assert(ed2)(equalTo(richProduct))
      },
      test("complex sum type with nested product") {
        val richSum = RichSum.Person("hello", 10)
        for {
          ed  <- encodeAndDecode(RichSum.richSumSchema, richSum)
          ed2 <- encodeAndDecodeNS(RichSum.richSumSchema, richSum)
        } yield assert(ed)(equalTo(Chunk(richSum))) && assert(ed2)(equalTo(richSum))
      },
      test("complex sum type with nested long primitive") {
        val long = RichSum.LongWrapper(100L)
        for {
          ed  <- encodeAndDecode(RichSum.richSumSchema, long)
          ed2 <- encodeAndDecodeNS(RichSum.richSumSchema, long)
        } yield assert(ed)(equalTo(Chunk(long))) && assert(ed2)(equalTo(long))
      },
      test("complex either with product type") {
        val either = Left(Record("hello world", 100))
        for {
          ed  <- encodeAndDecode(complexEitherSchema, either)
          ed2 <- encodeAndDecodeNS(complexEitherSchema, either)
        } yield assert(ed)(equalTo(Chunk(either))) && assert(ed2)(equalTo(either))
      },
      test("complex tuples") {
        val value = (Record("hello world", 100), BooleanValue(true))
        for {
          ed  <- encodeAndDecode(complexTupleSchema, value)
          ed2 <- encodeAndDecodeNS(complexTupleSchema, value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("complex optionals with product type") {
        val value = Some(Record("hello earth", 21))
        for {
          ed  <- encodeAndDecode(Schema.Optional(Record.schemaRecord), value)
          ed2 <- encodeAndDecodeNS(Schema.Optional(Record.schemaRecord), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("optional of product type within optional") {
        val value = Some(Some(Record("hello", 10)))
        for {
          ed  <- encodeAndDecode(Schema.Optional(Schema.Optional(Record.schemaRecord)), value)
          ed2 <- encodeAndDecodeNS(Schema.Optional(Schema.Optional(Record.schemaRecord)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("optional of sum type within optional") {
        val value = Some(Some(BooleanValue(true)))
        for {
          ed  <- encodeAndDecode(Schema.Optional(Schema.Optional(schemaOneOf)), value)
          ed2 <- encodeAndDecodeNS(Schema.Optional(Schema.Optional(schemaOneOf)), value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      },
      test("either within either") {
        val either = Right(Left(BooleanValue(true)))
        val schema = Schema.either(Schema[Int], Schema.either(schemaOneOf, Schema[String]))
        for {
          ed  <- encodeAndDecode(schema, either)
          ed2 <- encodeAndDecodeNS(schema, either)
        } yield assert(ed)(equalTo(Chunk(either))) && assert(ed2)(equalTo(either))
      },
      test("sequence of products") {
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
      test("sequence of sums") {
        val richSequence = SequenceOfSum("hello", List(RichSum.LongWrapper(150L), RichSum.LongWrapper(150L)))
        for {
          ed  <- encodeAndDecode(sequenceOfSumSchema, richSequence)
          ed2 <- encodeAndDecodeNS(sequenceOfSumSchema, richSequence)
        } yield assert(ed)(equalTo(Chunk(richSequence))) && assert(ed2)(equalTo(richSequence))
      },
      test("map of products") {
        val m: scala.collection.immutable.Map[Record, MyRecord] = scala.collection.immutable.Map(
          Record("AAA", 1) -> MyRecord(1),
          Record("BBB", 2) -> MyRecord(2)
        )
        val mSchema = Schema.map(Record.schemaRecord, myRecord)
        for {
          ed  <- encodeAndDecode(mSchema, m)
          ed2 <- encodeAndDecodeNS(mSchema, m)
        } yield assert(ed)(equalTo(Chunk.succeed(m))) && assert(ed2)(equalTo(m))
      },
      test("map in record") {
        val m = MapRecord(1, scala.collection.immutable.Map(1 -> "aaa", 3 -> "ccc"))
        for {
          ed  <- encodeAndDecode(schemaMapRecord, m)
          ed2 <- encodeAndDecodeNS(schemaMapRecord, m)
        } yield assert(ed)(equalTo(Chunk.succeed(m))) && assert(ed2)(equalTo(m))
      },
      test("set of products") {
        val set: scala.collection.immutable.Set[Record] =
          scala.collection.immutable.Set(Record("AAA", 1), Record("BBB", 2))
        val setSchema = Schema.set(Record.schemaRecord)

        for {
          ed  <- encodeAndDecode(setSchema, set)
          ed2 <- encodeAndDecodeNS(setSchema, set)
        } yield assert(ed)(equalTo(Chunk.succeed(set))) && assert(ed2)(equalTo(set))
      },
      test("set in record") {
        val m = SetRecord(1, scala.collection.immutable.Set("aaa", "ccc"))
        for {
          ed  <- encodeAndDecode(schemaSetRecord, m)
          ed2 <- encodeAndDecodeNS(schemaSetRecord, m)
        } yield assert(ed)(equalTo(Chunk.succeed(m))) && assert(ed2)(equalTo(m))
      },
      test("recursive data types") {
        check(SchemaGen.anyRecursiveTypeAndValue) {
          case (schema, value) =>
            for {
              ed  <- encodeAndDecode(schema, value)
              ed2 <- encodeAndDecodeNS(schema, value)
            } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
        }
      },
      suite("dynamic")(
        test("dynamic int") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(StandardType.IntType)
          ) { dynamicValue =>
            assertZIO(encodeAndDecode(Schema.dynamicValue, dynamicValue))(equalTo(Chunk(dynamicValue)))
          }
        },
        test("dynamic instant") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(StandardType.InstantType)
          ) { dynamicValue =>
            assertZIO(encodeAndDecode(Schema.dynamicValue, dynamicValue))(equalTo(Chunk(dynamicValue)))
          }
        },
        test("dynamic zoned date time") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(
              StandardType.ZonedDateTimeType
            )
          ) { dynamicValue =>
            assertZIO(encodeAndDecode(Schema.dynamicValue, dynamicValue))(equalTo(Chunk(dynamicValue)))
          }
        },
        test("dynamic duration") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(StandardType.DurationType)
          ) { dynamicValue =>
            assertZIO(encodeAndDecode(Schema.dynamicValue, dynamicValue))(equalTo(Chunk(dynamicValue)))
          }
        },
        test("dynamic string") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(StandardType.StringType)
          ) { dynamicValue =>
            assertZIO(encodeAndDecodeNS(Schema.dynamicValue, dynamicValue))(equalTo(dynamicValue))
          }
        },
        test("dynamic unit") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(StandardType.UnitType)
          ) { dynamicValue =>
            assertZIO(encodeAndDecode(Schema.dynamicValue, dynamicValue))(equalTo(Chunk(dynamicValue)))
          }
        },
        test("dynamic json") {
          check(
            DynamicValueGen.anyDynamicValueOfSchema(SchemaGen.Json.schema)
          ) { dynamicValue =>
            assertZIO(encodeAndDecode(Schema.dynamicValue, dynamicValue))(equalTo(Chunk(dynamicValue)))
          }
        },
        test("dynamic tuple") {
          check(
            DynamicValueGen.anyDynamicTupleValue(Schema[String], Schema[Int])
          ) { dynamicValue =>
            assertZIO(encodeAndDecode(Schema.dynamicValue, dynamicValue))(equalTo(Chunk(dynamicValue)))
          }
        },
        test("dynamic record") {
          check(
            SchemaGen.anyRecord.flatMap(DynamicValueGen.anyDynamicValueOfSchema)
          ) { dynamicValue =>
            assertZIO(encodeAndDecodeNS(Schema.dynamicValue, dynamicValue))(equalTo(dynamicValue))
          }
        },
        test("dynamic record example") {
          val dynamicValue: DynamicValue = DynamicValue.Record(
            TypeId.Structural,
            ListMap("0" -> DynamicValue.Primitive(new java.math.BigDecimal(0.0), StandardType[java.math.BigDecimal]))
          )
          for {
            dynamicValue2 <- encodeAndDecodeNS(Schema.dynamicValue, dynamicValue)
          } yield assertTrue(dynamicValue == dynamicValue2)
        },
        test("dynamic (string, record)") {
          check(
            SchemaGen.anyRecord.flatMap(record => DynamicValueGen.anyDynamicTupleValue(Schema[String], record))
          ) { dynamicValue =>
            assertZIO(encodeAndDecode(Schema.dynamicValue, dynamicValue))(equalTo(Chunk(dynamicValue)))
          }
        },
        test("dynamic sequence") {
          check(SchemaGen.anyRecord.flatMap(DynamicValueGen.anyDynamicSequence)) { dynamicValue =>
            assertZIO(encodeAndDecode(Schema.dynamicValue, dynamicValue))(equalTo(Chunk(dynamicValue)))
          }
        },
        test("dynamic set") {
          check(SchemaGen.anyRecord.flatMap(DynamicValueGen.anyDynamicSet)) { dynamicValue =>
            assertZIO(encodeAndDecode(Schema.dynamicValue, dynamicValue))(equalTo(Chunk(dynamicValue)))
          }
        }
      )
    ),
    suite("Should successfully decode")(
      test("empty input") {
        assertZIO(decode(Schema[Int], ""))(
          equalTo(Chunk.empty)
        )
      },
      test("empty input by non streaming variant") {
        assertZIO(decodeNS(Schema[Int], "").exit)(
          failsWithA[DecodeError]
        )
      }
    ),
    suite("Should fail to decode")(
      test("field begin") {
        for {
          d  <- decode(Record.schemaRecord, "0F").exit
          d2 <- decodeNS(Record.schemaRecord, "0F").exit
        } yield assert(d)(
          failsWithA[DecodeError]
        ) &&
          assert(d2)(
            failsWithA[DecodeError]
          )
      },
      test("missing value") {
        for {
          bytes <- writeManually { p =>
                    p.packMapHeader(2)
                    p.packString("name")
                    p.packString("Dan")
                    p.packString("value")
                  }
          d <- decode(Record.schemaRecord, bytes).exit
          bytes2 <- writeManually { p =>
                     p.packMapHeader(2)
                     p.packString("name")
                     p.packString("value")
                     p.packInt(123)
                   }
          d2 <- decode(Record.schemaRecord, bytes2).exit
        } yield assert(d)(
          failsWithA[DecodeError]
        ) &&
          assert(d2)(
            failsWithA[DecodeError]
          )
      },
      test("unable to decode") {
        for {
          bytes <- writeManually { p =>
                    p.packMapHeader(2)
                    p.packString("name")
                    p.packString("Dan")
                    p.packString("value")
                    p.packString("no an Int")
                  }
          d <- decode(Record.schemaRecord, bytes).exit
        } yield assert(d)(
          failsWithA[DecodeError]
        )
      },
      test("unknown type") {
        for {
          bytes <- writeManually { p =>
                    p.packMapHeader(2)
                    p.packString("This is number one bullshit")
                  }
          d <- decode(Record.schemaRecord, bytes).exit
        } yield assert(d)(
          failsWithA[DecodeError]
        )
      }
    )
  )

  def writeManually(f: MessagePacker => Any): Task[String] = ZIO.attempt {
    val packer = MessagePack.newDefaultBufferPacker()
    f(packer)
    val hex = toHex(Chunk.fromArray(packer.toByteArray))
    packer.close()
    hex
  }

  def write[A](serializable: A): Task[String] = ZIO.attempt(
    toHex(Chunk.fromArray(objectMapper.writeValueAsBytes(serializable)))
  )
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

  val schemaTuple: Schema.Tuple2[Int, String] = Schema.Tuple2(Schema[Int], Schema[String])

  case class MapValue(value: scala.collection.immutable.Map[String, Record])

  val schemaMapValue: Schema[MapValue] = DeriveSchema.gen[MapValue]

  case class SetValue(value: scala.collection.immutable.Set[Record])

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

  case class MaxArityCaseClass(
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
    f22: Int = 22
  )

  lazy val schemaMaxArityCaseClass: Schema[MaxArityCaseClass] = DeriveSchema.gen[MaxArityCaseClass]

  lazy val schemaHighArityRecord: Schema[HighArity] = DeriveSchema.gen[HighArity]

  lazy val schemaOneOf: Schema[OneOf] = DeriveSchema.gen[OneOf]

  lazy val schemaIntValue: Schema[IntValue] = DeriveSchema.gen[IntValue]

  case class MyRecord(age: Int)

  lazy val myRecord: Schema[MyRecord] = DeriveSchema.gen[MyRecord]

  case class MapRecord(age: Int, map: scala.collection.immutable.Map[Int, String])

  lazy val schemaMapRecord: Schema[MapRecord] = DeriveSchema.gen[MapRecord]

  case class SetRecord(age: Int, set: scala.collection.immutable.Set[String])

  lazy val schemaSetRecord: Schema[SetRecord] = DeriveSchema.gen[SetRecord]

  val complexTupleSchema: Schema.Tuple2[Record, OneOf] = Schema.Tuple2(Record.schemaRecord, schemaOneOf)

  val eitherSchema: Schema.Either[Int, String] = Schema.Either(Schema[Int], Schema[String])

  val complexEitherSchema: Schema.Either[Record, OneOf] =
    Schema.Either(Record.schemaRecord, schemaOneOf)

  val complexEitherSchema2: Schema.Either[MyRecord, MyRecord] =
    Schema.Either(myRecord, myRecord)

  val fallbackSchema1: Schema.Fallback[Int, String] = Schema.Fallback(Schema[Int], Schema[String], true)

  val fallbackSchema2: Schema.Fallback[Int, String] = Schema.Fallback(Schema[Int], Schema[String], false)

  val complexFallbackSchema: Schema.Fallback[Record, OneOf] =
    Schema.Fallback(Record.schemaRecord, schemaOneOf)

  val complexFallbackSchema2: Schema.Fallback[MyRecord, MyRecord] =
    Schema.Fallback(myRecord, myRecord)

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
    TypeId.Structural,
    caseOf[String, Any]("string")(_.asInstanceOf[String])(_.asInstanceOf[Any])(_.isInstanceOf[String]) ++ caseOf[
      Int,
      Any
    ]("int")(_.asInstanceOf[Int])(_.asInstanceOf[Any])(_.isInstanceOf[Int])
  )

  lazy val schemaGenericEnumerationSorted: Schema[Any] = Schema.enumeration[Any, CaseSet.Aux[Any]](
    TypeId.Structural,
    caseOf[Int, Any]("int")(_.asInstanceOf[Int])(_.asInstanceOf[Any])(_.isInstanceOf[Int]) ++ caseOf[String, Any](
      "string"
    )(_.asInstanceOf[String])(_.asInstanceOf[Any])(_.isInstanceOf[String])
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
      .via(MessagePackCodec.messagePackCodec(schema).streamEncoder)
      .run(ZSink.collectAll)

  //NS == non streaming variant of encode
  def encodeNS[A](schema: Schema[A], input: A): ZIO[Any, Nothing, Chunk[Byte]] =
    ZIO.succeed(MessagePackCodec.messagePackCodec(schema).encode(input))

  def decode[A](schema: Schema[A], hex: String): ZIO[Any, DecodeError, Chunk[A]] =
    ZStream
      .fromChunk(fromHex(hex))
      .via(MessagePackCodec.messagePackCodec(schema).streamDecoder)
      .run(ZSink.collectAll)

  //NS == non streaming variant of decode
  def decodeNS[A](schema: Schema[A], hex: String): ZIO[Any, DecodeError, A] =
    ZIO.succeed(MessagePackCodec.messagePackCodec(schema).decode(fromHex(hex))).absolve[DecodeError, A]

  def encodeAndDecode[A](schema: Schema[A], input: A): ZIO[Any, DecodeError, Chunk[A]] =
    ZStream
      .succeed(input)
      .via(MessagePackCodec.messagePackCodec(schema).streamEncoder)
      .via(MessagePackCodec.messagePackCodec(schema).streamDecoder)
      .run(ZSink.collectAll)

  def encodeAndDecode[A](encodeSchema: Schema[A], decodeSchema: Schema[A], input: A): ZIO[Any, DecodeError, Chunk[A]] =
    ZStream
      .succeed(input)
      .via(MessagePackCodec.messagePackCodec(encodeSchema).streamEncoder)
      .via(MessagePackCodec.messagePackCodec(decodeSchema).streamDecoder)
      .run(ZSink.collectAll)

  //NS == non streaming variant of encodeAndDecode
  def encodeAndDecodeNS[A](schema: Schema[A], input: A, print: Boolean = false): ZIO[Any, DecodeError, A] =
    ZIO
      .succeed(input)
      .tap(value => Console.printLine(s"Input Value: $value").when(print).ignore)
      .map(a => MessagePackCodec.messagePackCodec(schema).encode(a))
      .tap(encoded => Console.printLine(s"\nEncoded Bytes:\n${toHex(encoded)}").when(print).ignore)
      .map(ch => MessagePackCodec.messagePackCodec(schema).decode(ch))
      .absolve

  def encodeAndDecodeNS[A](encodeSchema: Schema[A], decodeSchema: Schema[A], input: A): ZIO[Any, DecodeError, A] =
    ZIO
      .succeed(input)
      .map(a => MessagePackCodec.messagePackCodec(encodeSchema).encode(a))
      .map(ch => MessagePackCodec.messagePackCodec(decodeSchema).decode(ch))
      .absolve

}
