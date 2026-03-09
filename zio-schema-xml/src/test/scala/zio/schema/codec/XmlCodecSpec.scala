package zio.schema.codec

import java.time._
import java.util.UUID

import zio.schema.annotation._
import zio.schema.{ DeriveSchema, Schema }
import zio.stream.{ ZSink, ZStream }
import zio.test.Assertion._
import zio.test._
import zio.{ Chunk, Console, Scope, ZIO }

object XmlCodecSpec extends ZIOSpecDefault {

  import Schema._

  def spec: Spec[TestEnvironment with Scope, Any] = suite("XmlCodec Spec")(
    roundTripSuite,
    encodeSuite,
    decodeSuite,
    edgeCaseSuite
  )

  // ===========================================================================
  // Round trip tests (encode -> decode -> assert equality)
  // ===========================================================================
  private val roundTripSuite = suite("Round trip encode/decode")(
    suite("Primitive types")(
      test("Unit") {
        assertRoundTrips(Schema[Unit], ())
      },
      test("String") {
        assertRoundTrips(Schema[String], "hello world")
      },
      test("String with special XML characters") {
        assertRoundTrips(Schema[String], """<tag attr="val">&amp;""")
      },
      test("empty String") {
        assertRoundTrips(Schema[String], "")
      },
      test("Boolean true") {
        assertRoundTrips(Schema[Boolean], true)
      },
      test("Boolean false") {
        assertRoundTrips(Schema[Boolean], false)
      },
      test("Byte") {
        assertRoundTrips(Schema[Byte], 42.toByte)
      },
      test("Short") {
        assertRoundTrips(Schema[Short], 1024.toShort)
      },
      test("Int") {
        assertRoundTrips(Schema[Int], 150)
      },
      test("Int negative") {
        assertRoundTrips(Schema[Int], -42)
      },
      test("Long") {
        assertRoundTrips(Schema[Long], 9876543210L)
      },
      test("Float") {
        assertRoundTrips(Schema[Float], 3.14f)
      },
      test("Double") {
        assertRoundTrips(Schema[Double], 2.718281828)
      },
      test("BigInteger") {
        assertRoundTrips(Schema[java.math.BigInteger], new java.math.BigInteger("123456789012345678901234567890"))
      },
      test("BigDecimal") {
        assertRoundTrips(Schema[java.math.BigDecimal], new java.math.BigDecimal("12345.6789"))
      },
      test("Binary") {
        assertRoundTrips(Schema[Chunk[Byte]], Chunk.fromArray(Array[Byte](1, 2, 3, 4, 5)))
      },
      test("Char") {
        assertRoundTrips(Schema[Char], 'Z')
      },
      test("UUID") {
        val uuid = UUID.randomUUID()
        assertRoundTrips(Schema[UUID], uuid)
      },
      test("Currency") {
        assertRoundTrips(Schema[java.util.Currency], java.util.Currency.getInstance("USD"))
      },
      test("DayOfWeek") {
        assertRoundTrips(Schema[DayOfWeek], DayOfWeek.FRIDAY)
      },
      test("Month") {
        assertRoundTrips(Schema[Month], Month.DECEMBER)
      },
      test("MonthDay") {
        assertRoundTrips(Schema[MonthDay], MonthDay.of(12, 25))
      },
      test("Period") {
        assertRoundTrips(Schema[Period], Period.of(2, 3, 15))
      },
      test("Year") {
        assertRoundTrips(Schema[Year], Year.of(2025))
      },
      test("YearMonth") {
        assertRoundTrips(Schema[YearMonth], YearMonth.of(2025, 6))
      },
      test("ZoneId") {
        assertRoundTrips(Schema[ZoneId], ZoneId.of("America/New_York"))
      },
      test("ZoneOffset") {
        assertRoundTrips(Schema[ZoneOffset], ZoneOffset.ofHours(5))
      },
      test("Duration") {
        assertRoundTrips(Schema[Duration], Duration.ofHours(2).plusMinutes(30))
      },
      test("Instant") {
        assertRoundTrips(Schema[Instant], Instant.parse("2025-01-15T10:30:00Z"))
      },
      test("LocalDate") {
        assertRoundTrips(Schema[LocalDate], LocalDate.of(2025, 6, 15))
      },
      test("LocalTime") {
        assertRoundTrips(Schema[LocalTime], LocalTime.of(14, 30, 0))
      },
      test("LocalDateTime") {
        assertRoundTrips(Schema[LocalDateTime], LocalDateTime.of(2025, 6, 15, 14, 30, 0))
      },
      test("OffsetTime") {
        assertRoundTrips(Schema[OffsetTime], OffsetTime.of(14, 30, 0, 0, ZoneOffset.ofHours(5)))
      },
      test("OffsetDateTime") {
        assertRoundTrips(
          Schema[OffsetDateTime],
          OffsetDateTime.of(2025, 6, 15, 14, 30, 0, 0, ZoneOffset.ofHours(5))
        )
      },
      test("ZonedDateTime") {
        assertRoundTrips(
          Schema[ZonedDateTime],
          ZonedDateTime.of(2025, 6, 15, 14, 30, 0, 0, ZoneId.of("America/New_York"))
        )
      }
    ),
    suite("Records")(
      test("simple case class") {
        assertRoundTrips(BasicInt.schema, BasicInt(150))
      },
      test("case class with String field") {
        assertRoundTrips(BasicString.schema, BasicString("hello"))
      },
      test("case class with multiple fields") {
        assertRoundTrips(Record.schema, Record("test", 42))
      },
      test("nested case class") {
        assertRoundTrips(Embedded.schema, Embedded(BasicInt(150)))
      },
      test("deeply nested") {
        assertRoundTrips(
          SearchRequest.schema,
          SearchRequest("bitcoins", RequestVars("varValue", 1), 100)
        )
      },
      test("high arity case class") {
        assertRoundTrips(schemaHighArity, HighArity())
      },
      test("case class with optional field (Some)") {
        assertRoundTrips(ClassWithOption.schema, ClassWithOption(42, Some("hello")))
      },
      test("case class with optional field (None)") {
        assertRoundTrips(ClassWithOption.schema, ClassWithOption(42, None))
      },
      test("case class with transient field") {
        assertRoundTrips(PersonWithTransientField.schema, PersonWithTransientField("Alice"))
      },
      test("case class with default field value") {
        assertRoundTrips(PersonWithDefaultField.schema, PersonWithDefaultField("Bob", 25))
      },
      test("case class with list field") {
        assertRoundTrips(IntList.schema, IntList(List(1, 2, 3)))
      },
      test("case class with empty list field") {
        assertRoundTrips(IntList.schema, IntList(List.empty))
      },
      test("case class with map field") {
        assertRoundTrips(MapRecord.schema, MapRecord(42, scala.collection.immutable.Map(1 -> "one", 2 -> "two")))
      },
      test("case class with set field") {
        assertRoundTrips(SetRecord.schema, SetRecord(42, scala.collection.immutable.Set("a", "b", "c")))
      },
      test("case object") {
        assertRoundTrips(schemaObject, Singleton)
      }
    ),
    suite("Enums")(
      test("enum - StringValue case") {
        assertRoundTrips(schemaOneOf, StringValue("hello"): OneOf)
      },
      test("enum - IntValue case") {
        assertRoundTrips(schemaOneOf, IntValue(42): OneOf)
      },
      test("enum - BooleanValue case") {
        assertRoundTrips(schemaOneOf, BooleanValue(true): OneOf)
      },
      test("simple enum (case objects only)") {
        assertRoundTrips(Color.schema, Color.Red: Color)
      },
      test("rich sum type") {
        assertRoundTrips(RichSum.schema, RichSum.Person("Alice", 30): RichSum)
      },
      test("rich sum type - another case") {
        assertRoundTrips(RichSum.schema, RichSum.LongWrapper(999L): RichSum)
      },
      test("nested enum in record") {
        assertRoundTrips(Enumeration.schema, Enumeration(StringValue("test")))
      }
    ),
    suite("Collections")(
      test("list of ints") {
        assertRoundTrips(Schema.list[Int], List(1, 2, 3))
      },
      test("empty list") {
        assertRoundTrips(Schema.list[Int], List.empty[Int])
      },
      test("list of strings") {
        assertRoundTrips(Schema.list[String], List("a", "b", "c"))
      },
      test("list of records") {
        assertRoundTrips(Schema.list[Record], List(Record("a", 1), Record("b", 2)))
      },
      test("set of strings") {
        assertRoundTrips(Schema.set[String], scala.collection.immutable.Set("x", "y", "z"))
      },
      test("map of string to int") {
        assertRoundTrips(
          Schema.map[String, Int],
          scala.collection.immutable.Map("a" -> 1, "b" -> 2)
        )
      },
      test("map of string to record") {
        assertRoundTrips(
          Schema.map[String, Record],
          scala.collection.immutable.Map("first" -> Record("a", 1), "second" -> Record("b", 2))
        )
      },
      test("empty map") {
        assertRoundTrips(
          Schema.map[String, Int],
          scala.collection.immutable.Map.empty[String, Int]
        )
      }
    ),
    suite("Tuples")(
      test("tuple of Int and String") {
        assertRoundTrips(schemaTuple, (42, "hello"))
      },
      test("tuple of records") {
        assertRoundTrips(
          Schema.Tuple2(Record.schema, Record.schema),
          (Record("a", 1), Record("b", 2))
        )
      }
    ),
    suite("Either")(
      test("Left value") {
        assertRoundTrips(eitherSchema, Left(42): scala.util.Either[Int, String])
      },
      test("Right value") {
        assertRoundTrips(eitherSchema, Right("hello"): scala.util.Either[Int, String])
      },
      test("complex Either - Left") {
        assertRoundTrips(complexEitherSchema, Left(Record("test", 1)): scala.util.Either[Record, OneOf])
      },
      test("complex Either - Right") {
        assertRoundTrips(
          complexEitherSchema,
          Right(StringValue("abc")): scala.util.Either[Record, OneOf]
        )
      }
    ),
    suite("Optional")(
      test("Some value") {
        assertRoundTrips(Schema.Optional(Schema[Int]), Some(42): Option[Int])
      },
      test("None value") {
        assertRoundTrips(Schema.Optional(Schema[Int]), None: Option[Int])
      },
      test("Some complex value") {
        assertRoundTrips(Schema.Optional(Record.schema), Some(Record("test", 1)): Option[Record])
      }
    ),
    suite("Fallback")(
      test("Fallback Left") {
        assertRoundTrips(
          fallbackSchema,
          zio.schema.Fallback.Left(42): zio.schema.Fallback[Int, String]
        )
      },
      test("Fallback Right") {
        assertRoundTrips(
          fallbackSchema,
          zio.schema.Fallback.Right("hello"): zio.schema.Fallback[Int, String]
        )
      },
      test("Fallback Both") {
        assertRoundTrips(
          Schema.Fallback(Schema[Int], Schema[String], true),
          zio.schema.Fallback.Both(42, "hello"): zio.schema.Fallback[Int, String]
        )
      }
    ),
    suite("Transform")(
      test("transform round trip") {
        val schema: Schema[Int] = Schema[String].transformOrFail(
          s =>
            try Right(s.toInt)
            catch { case _: NumberFormatException => Left(s"Not an int: $s") },
          i => Right(i.toString)
        )
        assertRoundTrips(schema, 42)
      }
    ),
    suite("Streaming")(
      test("stream encode and decode") {
        for {
          result <- ZStream
                      .succeed(Record("test", 42))
                      .via(XmlCodec.xmlCodec(Record.schema).streamEncoder)
                      .via(XmlCodec.xmlCodec(Record.schema).streamDecoder)
                      .run(ZSink.collectAll)
        } yield assert(result)(equalTo(Chunk(Record("test", 42))))
      }
    )
  )

  // ===========================================================================
  // Encode-specific tests
  // ===========================================================================
  private val encodeSuite = suite("Encoding specifics")(
    test("produces valid XML with declaration") {
      for {
        bytes <- ZIO.succeed(XmlCodec.xmlCodec(Schema[Int]).encode(42))
        xml = new String(bytes.toArray, "UTF-8")
      } yield assert(xml)(startsWithString("""<?xml version="1.0" encoding="UTF-8"?>"""))
    },
    test("record field names used as element names") {
      for {
        bytes <- ZIO.succeed(XmlCodec.xmlCodec(Record.schema).encode(Record("Alice", 30)))
        xml = new String(bytes.toArray, "UTF-8")
      } yield assert(xml)(containsString("<name>Alice</name>")) &&
        assert(xml)(containsString("<value>30</value>"))
    },
    test("enum case name used as element name") {
      for {
        bytes <- ZIO.succeed(XmlCodec.xmlCodec(schemaOneOf).encode(StringValue("hello")))
        xml = new String(bytes.toArray, "UTF-8")
      } yield assert(xml)(containsString("<StringValue>"))
    },
    test("optional None omitted in record") {
      for {
        bytes <- ZIO.succeed(XmlCodec.xmlCodec(ClassWithOption.schema).encode(ClassWithOption(42, None)))
        xml = new String(bytes.toArray, "UTF-8")
      } yield assert(xml)(not(containsString("<name>")))
    },
    test("optional Some included in record") {
      for {
        bytes <- ZIO.succeed(XmlCodec.xmlCodec(ClassWithOption.schema).encode(ClassWithOption(42, Some("hi"))))
        xml = new String(bytes.toArray, "UTF-8")
      } yield assert(xml)(containsString("<name>hi</name>"))
    },
    test("XML special characters are escaped") {
      for {
        bytes <- ZIO.succeed(XmlCodec.xmlCodec(Schema[String]).encode("<test>&"))
        xml = new String(bytes.toArray, "UTF-8")
      } yield assert(xml)(containsString("&lt;test&gt;&amp;"))
    },
    test("list elements wrapped in <item>") {
      for {
        bytes <- ZIO.succeed(XmlCodec.xmlCodec(IntList.schema).encode(IntList(List(1, 2))))
        xml = new String(bytes.toArray, "UTF-8")
      } yield assert(xml)(containsString("<item>1</item>")) &&
        assert(xml)(containsString("<item>2</item>"))
    },
    test("map entries wrapped in <entry><key><value>") {
      for {
        bytes <- ZIO.succeed(
                   XmlCodec
                     .xmlCodec(MapRecord.schema)
                     .encode(MapRecord(1, scala.collection.immutable.Map(10 -> "ten")))
                 )
        xml = new String(bytes.toArray, "UTF-8")
      } yield assert(xml)(containsString("<entry>")) &&
        assert(xml)(containsString("<key>10</key>")) &&
        assert(xml)(containsString("<value>ten</value>"))
    },
    test("simple enum encodes as text") {
      for {
        bytes <- ZIO.succeed(XmlCodec.xmlCodec(Color.schema).encode(Color.Red))
        xml = new String(bytes.toArray, "UTF-8")
      } yield assert(xml)(containsString("Red"))
    }
  )

  // ===========================================================================
  // Decode-specific tests
  // ===========================================================================
  private val decodeSuite = suite("Decoding specifics")(
    test("decode empty bytes returns error") {
      val result = XmlCodec.xmlCodec(Schema[Int]).decode(Chunk.empty)
      assert(result)(isLeft)
    },
    test("decode malformed XML returns error") {
      val bytes  = Chunk.fromArray("<not-closed>".getBytes("UTF-8"))
      val result = XmlCodec.xmlCodec(Schema[Int]).decode(bytes)
      assert(result)(isLeft)
    },
    test("decode missing required field returns error") {
      val xml    = """<Record><name>Alice</name></Record>"""
      val bytes  = Chunk.fromArray(xml.getBytes("UTF-8"))
      val result = XmlCodec.xmlCodec(Record.schema).decode(bytes)
      assert(result)(isLeft)
    },
    test("decode wrong type returns error") {
      val xml    = """<value>not-a-number</value>"""
      val bytes  = Chunk.fromArray(xml.getBytes("UTF-8"))
      val result = XmlCodec.xmlCodec(Schema[Int]).decode(bytes)
      assert(result)(isLeft)
    },
    test("decode missing optional field returns None") {
      val xml   = """<ClassWithOption><number>42</number></ClassWithOption>"""
      val bytes = Chunk.fromArray(xml.getBytes("UTF-8"))
      for {
        result <- ZIO.fromEither(XmlCodec.xmlCodec(ClassWithOption.schema).decode(bytes))
      } yield assert(result)(equalTo(ClassWithOption(42, None)))
    },
    test("decode with default field value") {
      val xml   = """<PersonWithDefaultField><name>Bob</name></PersonWithDefaultField>"""
      val bytes = Chunk.fromArray(xml.getBytes("UTF-8"))
      for {
        result <- ZIO.fromEither(XmlCodec.xmlCodec(PersonWithDefaultField.schema).decode(bytes))
      } yield assert(result)(equalTo(PersonWithDefaultField("Bob", 18)))
    }
  )

  // ===========================================================================
  // Edge case tests
  // ===========================================================================
  private val edgeCaseSuite = suite("Edge cases")(
    test("round trip with empty collections in record") {
      assertRoundTrips(
        MapRecord.schema,
        MapRecord(0, scala.collection.immutable.Map.empty)
      )
    },
    test("round trip with nested optional") {
      val schema = DeriveSchema.gen[NestedOptional]
      assertRoundTrips(schema, NestedOptional(Some(Some(42))))
    },
    test("round trip with nested optional None") {
      val schema = DeriveSchema.gen[NestedOptional]
      assertRoundTrips(schema, NestedOptional(None))
    },
    test("round trip with list of enums") {
      assertRoundTrips(
        SequenceOfSum.schema,
        SequenceOfSum("test", List(RichSum.Person("A", 1), RichSum.LongWrapper(2L)))
      )
    },
    test("round trip with list of records") {
      assertRoundTrips(
        SequenceOfProduct.schema,
        SequenceOfProduct("name", List(Record("a", 1), Record("b", 2)), RichSum.Person("c", 3))
      )
    },
    test("round trip with @fieldName annotation") {
      assertRoundTrips(
        PersonWithFieldName.schema,
        PersonWithFieldName("Alice", 30)
      )
    }
  )

  // ===========================================================================
  // Helper: assert encode -> decode round trips to the same value
  // ===========================================================================
  private def assertRoundTrips[A](schema: Schema[A], value: A): ZIO[Any, DecodeError, TestResult] =
    encodeAndDecodeNS(schema, value).map(decoded => assert(decoded)(equalTo(value)))

  private def encodeAndDecodeNS[A](schema: Schema[A], input: A, print: Boolean = false): ZIO[Any, DecodeError, A] =
    ZIO
      .succeed(input)
      .tap(v => Console.printLine(s"Input: $v").when(print).ignore)
      .map(a => XmlCodec.xmlCodec(schema).encode(a))
      .tap(enc => Console.printLine(s"Encoded XML:\n${new String(enc.toArray, "UTF-8")}").when(print).ignore)
      .map(ch => XmlCodec.xmlCodec(schema).decode(ch))
      .absolve

  private def encodeAndDecode[A](schema: Schema[A], input: A): ZIO[Any, DecodeError, Chunk[A]] =
    ZStream
      .succeed(input)
      .via(XmlCodec.xmlCodec(schema).streamEncoder)
      .via(XmlCodec.xmlCodec(schema).streamDecoder)
      .run(ZSink.collectAll)

  // ===========================================================================
  // Test data models
  // ===========================================================================

  case class BasicInt(value: Int)
  object BasicInt {
    implicit val schema: Schema[BasicInt] = DeriveSchema.gen[BasicInt]
  }

  case class BasicString(value: String)
  object BasicString {
    implicit val schema: Schema[BasicString] = DeriveSchema.gen[BasicString]
  }

  case class Record(name: String, value: Int)
  object Record {
    implicit val schema: Schema[Record] = DeriveSchema.gen[Record]
  }

  case class Embedded(embedded: BasicInt)
  object Embedded {
    implicit val schema: Schema[Embedded] = DeriveSchema.gen[Embedded]
  }

  case class IntList(items: List[Int])
  object IntList {
    implicit val schema: Schema[IntList] = DeriveSchema.gen[IntList]
  }

  case class RequestVars(someString: String, second: Int)
  object RequestVars {
    implicit val schema: Schema[RequestVars] = DeriveSchema.gen[RequestVars]
  }

  case class SearchRequest(query: String, pageNumber: RequestVars, resultPerPage: Int)
  object SearchRequest {
    implicit val schema: Schema[SearchRequest] = DeriveSchema.gen[SearchRequest]
  }

  case class ClassWithOption(number: Int, name: Option[String])
  object ClassWithOption {
    implicit val schema: Schema[ClassWithOption] = DeriveSchema.gen[ClassWithOption]
  }

  case class PersonWithTransientField(name: String, @transientField age: Int = 0)
  object PersonWithTransientField {
    implicit val schema: Schema[PersonWithTransientField] = DeriveSchema.gen[PersonWithTransientField]
  }

  case class PersonWithDefaultField(name: String, @fieldDefaultValue(18) age: Int)
  object PersonWithDefaultField {
    implicit val schema: Schema[PersonWithDefaultField] = DeriveSchema.gen[PersonWithDefaultField]
  }

  case class PersonWithFieldName(name: String, @fieldName("years_old") age: Int)
  object PersonWithFieldName {
    implicit val schema: Schema[PersonWithFieldName] = DeriveSchema.gen[PersonWithFieldName]
  }

  case class MapRecord(age: Int, map: scala.collection.immutable.Map[Int, String])
  object MapRecord {
    implicit val schema: Schema[MapRecord] = DeriveSchema.gen[MapRecord]
  }

  case class SetRecord(age: Int, set: scala.collection.immutable.Set[String])
  object SetRecord {
    implicit val schema: Schema[SetRecord] = DeriveSchema.gen[SetRecord]
  }

  case class NestedOptional(value: Option[Option[Int]])
  // No companion - schema derived inline in tests

  sealed trait OneOf
  case class StringValue(value: String)   extends OneOf
  case class IntValue(value: Int)         extends OneOf
  case class BooleanValue(value: Boolean) extends OneOf
  lazy val schemaOneOf: Schema[OneOf] = DeriveSchema.gen[OneOf]

  sealed trait Color
  object Color {
    case object Red   extends Color
    case object Green extends Color
    case object Blue  extends Color

    implicit val schema: Schema[Color] = DeriveSchema.gen[Color]
  }

  sealed trait RichSum
  object RichSum {
    case class Person(name: String, age: Int) extends RichSum
    case class LongWrapper(long: Long)        extends RichSum

    implicit val schema: Schema[RichSum] = DeriveSchema.gen[RichSum]
  }

  case class Enumeration(oneOf: OneOf)
  object Enumeration {
    implicit val schema: Schema[Enumeration] = DeriveSchema.gen[Enumeration]
  }

  case class SequenceOfProduct(name: String, records: List[Record], richSum: RichSum)
  object SequenceOfProduct {
    implicit val schema: Schema[SequenceOfProduct] = DeriveSchema.gen[SequenceOfProduct]
  }

  case class SequenceOfSum(value: String, enums: List[RichSum])
  object SequenceOfSum {
    implicit val schema: Schema[SequenceOfSum] = DeriveSchema.gen[SequenceOfSum]
  }

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
    f22: Int = 22
  )
  lazy val schemaHighArity: Schema[HighArity] = DeriveSchema.gen[HighArity]

  case object Singleton
  implicit val schemaObject: Schema[Singleton.type] = DeriveSchema.gen[Singleton.type]

  val schemaTuple: Schema.Tuple2[Int, String] = Schema.Tuple2(Schema[Int], Schema[String])

  val eitherSchema: Schema.Either[Int, String] = Schema.Either(Schema[Int], Schema[String])

  val complexEitherSchema: Schema.Either[Record, OneOf] =
    Schema.Either(Record.schema, schemaOneOf)

  val fallbackSchema: Schema.Fallback[Int, String] = Schema.Fallback(Schema[Int], Schema[String], true)
}
