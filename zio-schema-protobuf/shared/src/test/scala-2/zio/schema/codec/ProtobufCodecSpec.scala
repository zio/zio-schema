package zio.schema.codec

import java.time._
import java.util.UUID

import scala.collection.immutable.ListMap
import scala.util.Try

import zio.Console._
import zio._
import zio.schema.CaseSet._
import zio.schema.meta.MetaSchema
import zio.schema.{ CaseSet, DeriveSchema, DynamicValue, DynamicValueGen, Schema, SchemaGen, StandardType, TypeId }
import zio.stream.{ ZSink, ZStream }
import zio.test.Assertion._
import zio.test._

// TODO: use generators instead of manual encode/decode
object ProtobufCodecSpec extends ZIOSpecDefault {
  import Schema._

  def spec: Spec[TestConfig with Sized, Any] =
    suite("ProtobufCodec Spec")(
      suite("Should correctly encode")(
        test("integers") {
          for {
            e  <- encode(schemaBasicInt, BasicInt(150)).map(toHex)
            e2 <- encodeNS(schemaBasicInt, BasicInt(150)).map(toHex)
          } yield assert(e)(equalTo("089601")) && assert(e2)(equalTo("089601"))
        },
        test("strings") {
          for {
            e  <- encode(schemaBasicString, BasicString("testing")).map(toHex)
            e2 <- encodeNS(schemaBasicString, BasicString("testing")).map(toHex)
          } yield assert(e)(equalTo("0A0774657374696E67")) && assert(e2)(equalTo("0A0774657374696E67"))
        },
        test("floats") {
          for {
            e  <- encode(schemaBasicFloat, BasicFloat(0.001f)).map(toHex)
            e2 <- encodeNS(schemaBasicFloat, BasicFloat(0.001f)).map(toHex)
          } yield assert(e)(equalTo("0D6F12833A")) && assert(e2)(equalTo("0D6F12833A"))
        },
        test("doubles") {
          for {
            e  <- encode(schemaBasicDouble, BasicDouble(0.001)).map(toHex)
            e2 <- encodeNS(schemaBasicDouble, BasicDouble(0.001)).map(toHex)
          } yield assert(e)(equalTo("09FCA9F1D24D62503F")) && assert(e2)(equalTo("09FCA9F1D24D62503F"))
        },
        test("embedded messages") {
          for {
            e  <- encode(schemaEmbedded, Embedded(BasicInt(150))).map(toHex)
            e2 <- encodeNS(schemaEmbedded, Embedded(BasicInt(150))).map(toHex)
          } yield assert(e)(equalTo("0A03089601")) && assert(e2)(equalTo("0A03089601"))
        },
        test("packed lists") {
          for {
            e  <- encode(schemaPackedList, PackedList(List(3, 270, 86942))).map(toHex)
            e2 <- encodeNS(schemaPackedList, PackedList(List(3, 270, 86942))).map(toHex)
          } yield assert(e)(equalTo("0A081206038E029EA705")) && assert(e2)(equalTo("0A081206038E029EA705"))
        },
        test("unpacked lists") {
          for {
            e  <- encode(schemaUnpackedList, UnpackedList(List("foo", "bar", "baz"))).map(toHex)
            e2 <- encodeNS(schemaUnpackedList, UnpackedList(List("foo", "bar", "baz"))).map(toHex)
          } yield assert(e)(equalTo("0A11120F0A03666F6F12036261721A0362617A")) && assert(e2)(
            equalTo("0A11120F0A03666F6F12036261721A0362617A")
          )
        },
        test("records") {
          for {
            e  <- encode(Record.schemaRecord, Record("Foo", 123)).map(toHex)
            e2 <- encodeNS(Record.schemaRecord, Record("Foo", 123)).map(toHex)
          } yield assert(e)(equalTo("0A03466F6F107B")) && assert(e2)(equalTo("0A03466F6F107B"))
        },
        test("enumerations") {
          for {
            e  <- encode(schemaEnumeration, Enumeration(IntValue(482))).map(toHex)
            e2 <- encodeNS(schemaEnumeration, Enumeration(IntValue(482))).map(toHex)
          } yield assert(e)(equalTo("0A05120308E203")) && assert(e2)(equalTo("0A05120308E203"))
        },
        test("enums unwrapped") {
          for {
            e  <- encode(schemaOneOf, IntValue(482)).map(toHex)
            e2 <- encodeNS(schemaOneOf, IntValue(482)).map(toHex)
          } yield assert(e)(equalTo("120308E203")) && assert(e2)(equalTo("120308E203"))
        },
        test("failure") {
          for {
            e  <- encode(schemaFail, StringValue("foo")).map(_.size).exit
            e2 <- encodeNS(schemaFail, StringValue("foo")).map(_.size).exit
          } yield assert(e)(dies(anything)) && assert(e2)(dies(anything))
        }
      ),
      suite("Should successfully encode and decode")(
        test("empty list") {
          for {
            ed <- encodeAndDecodeNS(Schema[List[List[Int]]], List.empty)
          } yield assert(ed)(equalTo(List.empty))
        },
        test("list of an empty list") {
          for {
            ed <- encodeAndDecodeNS(Schema[List[List[Int]]], List(List.empty))
          } yield assert(ed)(equalTo(List(List.empty)))
        },
        test("case class containing empty list & case class containing list of an empty list") {
          val value2 = Lists(1, List.empty, "second string", List(List.empty))
          for {
            ed2 <- encodeAndDecodeNS(Schema[Lists], value2)
          } yield assert(ed2)(equalTo(value2))
        },
        test("records") {
          for {
            ed2 <- encodeAndDecodeNS(Record.schemaRecord, Record("hello", 150))
          } yield assert(ed2)(equalTo(Record("hello", 150)))
        },
        test("records with arity greater than 22") {
          for {
            ed <- encodeAndDecodeNS(schemaHighArityRecord, HighArity())
          } yield assert(ed)(equalTo(HighArity()))
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
        test("string") {
          for {
            ed2 <- encodeAndDecodeNS(Schema[String], "hello world")
          } yield assert(ed2)(equalTo("hello world"))
        },
        test("empty string") {
          for {
            ed2 <- encodeAndDecodeNS(Schema[String], "")
          } yield assert(ed2)(equalTo(""))
        },
        test("empty string in wrapper class") {
          for {
            ed2 <- encodeAndDecodeNS(schemaBasicString, BasicString(""))
          } yield assert(ed2)(equalTo(BasicString("")))
        },
        test("empty dynamic string") {
          for {
            ed2 <- encodeAndDecodeNS(
                    Schema.dynamicValue,
                    DynamicValue.Primitive("", StandardType.StringType)
                  )
          } yield assert(ed2)(equalTo(DynamicValue.Primitive("", StandardType.StringType)))
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
          val value = java.time.Duration.ofDays(12)
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
            ed <- encodeAndDecode(
                   Primitive(StandardType.LocalDateTimeType),
                   value
                 )
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
        test("packed sequences") {
          val list = PackedList(List(3, 270, 86942))
          for {
            ed  <- encodeAndDecode(schemaPackedList, list)
            ed2 <- encodeAndDecodeNS(schemaPackedList, list)
          } yield assert(ed)(equalTo(Chunk(list))) && assert(ed2)(equalTo(list))
        },
        test("empty packed sequence") {
          val list = PackedList(List.empty)
          for {
            ed  <- encodeAndDecode(schemaPackedList, list)
            ed2 <- encodeAndDecodeNS(schemaPackedList, list)
          } yield assert(ed)(equalTo(Chunk(list))) && assert(ed2)(equalTo(list))
        },
        test("non-packed sequences") {
          val list = UnpackedList(List("foo", "bar", "baz"))
          for {
            ed  <- encodeAndDecode(schemaUnpackedList, list)
            ed2 <- encodeAndDecodeNS(schemaUnpackedList, list)
          } yield assert(ed)(equalTo(Chunk(list))) && assert(ed2)(equalTo(list))
        },
        test("empty non-packed sequence") {
          val list = UnpackedList(List.empty)
          for {
            ed  <- encodeAndDecode(schemaUnpackedList, list)
            ed2 <- encodeAndDecodeNS(schemaUnpackedList, list)
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
        test("enumN within enumN") {
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
        test("optionals") {
          check(Gen.option(Gen.int(Int.MinValue, Int.MaxValue))) { value =>
            for {
              ed  <- encodeAndDecode(Schema.Optional(Schema[Int]), value)
              ed2 <- encodeAndDecodeNS(Schema.Optional(Schema[Int]), value)
            } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
          }
        },
        test("complex optionals with sum type") {
          val value = Some(BooleanValue(true))
          for {
            ed  <- encodeAndDecode(Schema.Optional(schemaOneOf), value)
            ed2 <- encodeAndDecodeNS(Schema.Optional(schemaOneOf), value)
          } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
        },
        test("option within option") {
          val value = Some(Some(true))
          for {
            ed  <- encodeAndDecode(Schema.option(Schema.option(Schema[Boolean])), value)
            ed2 <- encodeAndDecodeNS(Schema.option(Schema.option(Schema[Boolean])), value)
          } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
        },
        test("product type with inner product type") {
          val richProduct = RichProduct(StringValue("sum_type"), BasicString("string"), Record("value", 47))
          for {
            ed  <- encodeAndDecode(richProductSchema, richProduct)
            ed2 <- encodeAndDecodeNS(richProductSchema, richProduct)
          } yield assert(ed)(equalTo(Chunk(richProduct))) && assert(ed2)(equalTo(richProduct))
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
        test("sequence of tuples") {
          for {
            ed <- encodeAndDecodeNS2(Schema[List[(String, Int)]], List("foo" -> 1, "bar" -> 2))
          } yield assertTrue(ed == Right(List("foo" -> 1, "bar" -> 2)))
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
        test("set of products") {
          val set: scala.collection.immutable.Set[Record] =
            scala.collection.immutable.Set(Record("AAA", 1), Record("BBB", 2))
          val setSchema = Schema.set(Record.schemaRecord)

          for {
            ed2 <- encodeAndDecodeNS(setSchema, set)
            ed  <- encodeAndDecode(setSchema, set)
          } yield assert(ed)(equalTo(Chunk.succeed(set))) && assert(ed2)(equalTo(set))
        },
        test("recursive data types") {
          check(SchemaGen.anyRecursiveTypeAndValue) {
            case (schema, value) =>
              for {
                ed <- encodeAndDecode2(schema, value)
//              ed2 <- encodeAndDecodeNS(schema, value)
              } yield assertTrue(ed == Right(Chunk(value))) //&& assert(ed2)(equalTo(value))
          }
        },
        test("deep recursive data types") {
          check(SchemaGen.anyDeepRecursiveTypeAndValue) {
            case (schema, value) =>
              for {
                ed <- encodeAndDecode2(schema, value)
                //              ed2 <- encodeAndDecodeNS(schema, value)
              } yield assertTrue(ed == Right(Chunk(value))) //&& assert(ed2)(equalTo(value))
          }
        } @@ TestAspect.sized(200)
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
        test("unknown wire types") {
          for {
            d  <- decode(Record.schemaRecord, "0F").exit
            d2 <- decodeNS(Record.schemaRecord, "0F").exit
          } yield assert(d)(fails(anything)) &&
            assert(d2)(fails(anything))
        },
        test("invalid field numbers") {
          for {
            d  <- decode(Record.schemaRecord, "00").exit
            d2 <- decodeNS(Record.schemaRecord, "00").exit
          } yield assert(d)(fails(anything)) &&
            assert(d2)(fails(anything))
        },
        test("incomplete length delimited values") {
          for {
            d  <- decode(Record.schemaRecord, "0A0346").exit
            d2 <- decodeNS(Record.schemaRecord, "0A0346").exit
          } yield assert(d)(fails(anything)) &&
            assert(d2)(fails(anything))
        },
        test("incomplete var ints") {
          for {
            d  <- decode(Record.schemaRecord, "10FF").exit
            d2 <- decodeNS(Record.schemaRecord, "10FF").exit
          } yield assert(d)(fails(anything)) &&
            assert(d2)(fails(anything))
        },
        test("fail schemas") {
          for {
            d  <- decode(schemaFail, "0F").exit
            d2 <- decodeNS(schemaFail, "0F").exit
          } yield assert(d)(failsWithA[DecodeError]) && assert(d2)(failsWithA[DecodeError])
        }
      ),
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
          val dynamicValue = DynamicValue.Record(
            TypeId.Structural,
            ListMap("0" -> DynamicValue.Primitive(new java.math.BigDecimal(0.0), StandardType[java.math.BigDecimal]))
          )
          assertZIO(encodeAndDecodeNS(Schema.dynamicValue, dynamicValue))(equalTo(dynamicValue))
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
      ),
      suite("meta schema")(
        test("any meta schema") {
          check(SchemaGen.anySchema) { schema =>
            val metaSchema = schema.ast
            assertZIO(encodeAndDecode(MetaSchema.schema, metaSchema))(equalTo(Chunk(metaSchema)))
          }
        }
      )
    )

  // some tests are based on https://developers.google.com/protocol-buffers/docs/encoding
  case class BasicInt(value: Int)

  case class BasicTwoInts(value1: Int, value2: Int)

  case class Lists(id: Int, ints: List[Int], value: String, intLists: List[List[Int]])

  object Lists {
    implicit val schema: Schema[Lists] = DeriveSchema.gen[Lists]
  }

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

  case class PackedList(packed: List[Int])

  lazy val schemaPackedList: Schema[PackedList] = DeriveSchema.gen[PackedList]

  case class UnpackedList(items: List[String])

  lazy val schemaUnpackedList: Schema[UnpackedList] = DeriveSchema.gen[UnpackedList]

  case class Record(name: String, value: Int)

  object Record {
    implicit val schemaRecord: Schema[Record] = DeriveSchema.gen[Record]

    val genericRecord: Schema[ListMap[String, _]] = Schema.record(
      TypeId.Structural,
      Schema
        .Field(
          "c",
          Schema.Primitive(StandardType.IntType),
          get0 = (p: ListMap[String, _]) => p("c").asInstanceOf[Int],
          set0 = (p, v: Int) => p.updated("c", v)
        ),
      Schema
        .Field(
          "b",
          Schema.Primitive(StandardType.IntType),
          get0 = (p: ListMap[String, _]) => p("b").asInstanceOf[Int],
          set0 = (p, v: Int) => p.updated("b", v)
        ),
      Schema
        .Field(
          "a",
          Schema.Primitive(StandardType.IntType),
          get0 = (p: ListMap[String, _]) => p("a").asInstanceOf[Int],
          set0 = (p, v: Int) => p.updated("a", v)
        )
    )

    val genericRecordSorted: Schema[ListMap[String, _]] = Schema.record(
      TypeId.Structural,
      Schema
        .Field(
          "a",
          Schema.Primitive(StandardType.IntType),
          get0 = (p: ListMap[String, _]) => p("a").asInstanceOf[Int],
          set0 = (p, v: Int) => p.updated("a", v)
        ),
      Schema
        .Field(
          "b",
          Schema.Primitive(StandardType.IntType),
          get0 = (p: ListMap[String, _]) => p("b").asInstanceOf[Int],
          set0 = (p, v: Int) => p.updated("b", v)
        ),
      Schema
        .Field(
          "c",
          Schema.Primitive(StandardType.IntType),
          get0 = (p: ListMap[String, _]) => p("c").asInstanceOf[Int],
          set0 = (p, v: Int) => p.updated("c", v)
        )
    )
  }

  val schemaTuple: Schema.Tuple2[Int, String] = Schema.Tuple2(Schema[Int], Schema[String])

  sealed trait OneOf
  case class StringValue(value: String)   extends OneOf
  case class IntValue(value: Int)         extends OneOf
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

  val complexTupleSchema: Schema.Tuple2[Record, OneOf] = Schema.Tuple2(Record.schemaRecord, schemaOneOf)

  val eitherSchema: Schema.Either[Int, String] = Schema.Either(Schema[Int], Schema[String])

  val complexEitherSchema: Schema.Either[Record, OneOf] =
    Schema.Either(Record.schemaRecord, schemaOneOf)

  val complexEitherSchema2: Schema.Either[MyRecord, MyRecord] =
    Schema.Either(myRecord, myRecord)

  case class RichProduct(stringOneOf: OneOf, basicString: BasicString, record: Record)

  lazy val richProductSchema: Schema[RichProduct] = DeriveSchema.gen[RichProduct]

  sealed trait RichSum

  object RichSum {
    case class Person(name: String, age: Int) extends RichSum
    case class AnotherSum(oneOf: OneOf)       extends RichSum
    case class LongWrapper(long: Long)        extends RichSum

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
    ProtobufCodec
      .protobufCodec(schema)
      .streamEncoder
      .apply(
        ZStream
          .succeed(input)
      )
      .run(ZSink.collectAll)

  //NS == non streaming variant of encode
  def encodeNS[A](schema: Schema[A], input: A): ZIO[Any, Nothing, Chunk[Byte]] =
    ZIO.succeed(ProtobufCodec.protobufCodec(schema).encode(input))

  def decode[A](schema: Schema[A], hex: String): ZIO[Any, DecodeError, Chunk[A]] =
    ProtobufCodec
      .protobufCodec(schema)
      .streamDecoder
      .apply(
        ZStream
          .fromChunk(fromHex(hex))
      )
      .run(ZSink.collectAll)

  //NS == non streaming variant of decode
  def decodeNS[A](schema: Schema[A], hex: String): ZIO[Any, DecodeError, A] =
    ZIO.succeed(ProtobufCodec.protobufCodec(schema).decode(fromHex(hex))).absolve[DecodeError, A]

  def encodeAndDecode[A](schema: Schema[A], input: A): ZIO[Any, DecodeError, Chunk[A]] =
    ProtobufCodec
      .protobufCodec(schema)
      .streamEncoder
      .andThen(ProtobufCodec.protobufCodec(schema).streamDecoder)
      .apply(ZStream.succeed(input))
      .run(ZSink.collectAll)

  def encodeAndDecode2[A](schema: Schema[A], input: A): ZIO[Any, Any, scala.util.Either[DecodeError, Chunk[A]]] =
    ProtobufCodec
      .protobufCodec(schema)
      .streamEncoder
      .andThen(ProtobufCodec.protobufCodec(schema).streamDecoder)
      .apply(ZStream.succeed(input))
      .run(ZSink.collectAll)
      .either
      .tapSome {
        case Left(error) =>
          printLine(s"Failed to encode and decode input $input\nError=$error").orDie
      }

  def encodeAndDecode[A](encodeSchema: Schema[A], decodeSchema: Schema[A], input: A): ZIO[Any, DecodeError, Chunk[A]] =
    ProtobufCodec
      .protobufCodec(encodeSchema)
      .streamEncoder
      .andThen(ProtobufCodec.protobufCodec(decodeSchema).streamDecoder)
      .apply(ZStream.succeed(input))
      .run(ZSink.collectAll)

  //NS == non streaming variant of encodeAndDecode
  def encodeAndDecodeNS[A](schema: Schema[A], input: A, print: Boolean = false): ZIO[Any, DecodeError, A] =
    ZIO
      .succeed(input)
      .tap(value => printLine(s"Input Value: $value").when(print).ignore)
      .map(a => ProtobufCodec.protobufCodec(schema).encode(a))
      .tap(encoded => printLine(s"\nEncoded Bytes (${encoded.size}):\n${toHex(encoded)}").when(print).ignore)
      .map(ch => ProtobufCodec.protobufCodec(schema).decode(ch))
      .absolve

  def encodeAndDecodeNS2[A](
    schema: Schema[A],
    input: A,
    print: Boolean = false
  ): ZIO[Any, DecodeError, scala.util.Either[DecodeError, A]] =
    ZIO
      .succeed(input)
      .tap(value => printLine(s"Input Value: $value").when(print).ignore)
      .map(a => ProtobufCodec.protobufCodec(schema).encode(a))
      .tap(encoded => printLine(s"\nEncoded Bytes:\n${toHex(encoded)}").when(print).ignore)
      .map(ch => ProtobufCodec.protobufCodec(schema).decode(ch))
      .tapSome {
        case Left(err) => printLine(s"Failed to encode and decode value $input\nError = $err").orDie
      }

  def encodeAndDecodeNS[A](encodeSchema: Schema[A], decodeSchema: Schema[A], input: A): ZIO[Any, DecodeError, A] =
    ZIO
      .succeed(input)
      .map(a => ProtobufCodec.protobufCodec(encodeSchema).encode(a))
      .map(ch => ProtobufCodec.protobufCodec(decodeSchema).decode(ch))
      .absolve

}
