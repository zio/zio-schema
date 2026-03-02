package zio.schema.codec.xml

import java.time._
import java.util.UUID

import zio._
import zio.schema._
import zio.schema.annotation._
import zio.stream._
import zio.test._

object XmlCodecSpec extends ZIOSpecDefault {

  // ---------------------------------------------------------------------------
  // Test types
  // ---------------------------------------------------------------------------

  case class SimplePerson(name: String, age: Int)

  object SimplePerson {
    implicit val schema: Schema[SimplePerson] = DeriveSchema.gen
  }

  case class Nested(person: SimplePerson, tag: String)

  object Nested {
    implicit val schema: Schema[Nested] = DeriveSchema.gen
  }

  case class WithOptional(name: String, nickname: Option[String])

  object WithOptional {
    implicit val schema: Schema[WithOptional] = DeriveSchema.gen
  }

  case class WithDefault(name: String, role: String = "user")

  object WithDefault {
    implicit val schema: Schema[WithDefault] = DeriveSchema.gen
  }

  case class WithFieldName(@fieldName("full_name") name: String, age: Int)

  object WithFieldName {
    implicit val schema: Schema[WithFieldName] = DeriveSchema.gen
  }

  case class WithTransient(name: String, @transientField secret: String = "hidden")

  object WithTransient {
    implicit val schema: Schema[WithTransient] = DeriveSchema.gen
  }

  @simpleEnum
  sealed trait SimpleColor

  object SimpleColor {
    case object Red   extends SimpleColor
    case object Green extends SimpleColor
    case object Blue  extends SimpleColor
    implicit val schema: Schema[SimpleColor] = DeriveSchema.gen
  }

  sealed trait Shape

  object Shape {
    case class Circle(radius: Double)     extends Shape
    case class Rect(w: Double, h: Double) extends Shape
    implicit val schema: Schema[Shape] = DeriveSchema.gen
  }

  @discriminatorName("type")
  sealed trait Animal

  object Animal {
    case class Dog(name: String) extends Animal
    case class Cat(name: String) extends Animal
    implicit val schema: Schema[Animal] = DeriveSchema.gen
  }

  @noDiscriminator
  sealed trait Fruit

  object Fruit {
    case class Apple(variety: String) extends Fruit
    case class Banana(length: Int)    extends Fruit
    implicit val schema: Schema[Fruit] = DeriveSchema.gen
  }

  case class WithAttribute(@xmlAttribute id: Int, name: String)

  object WithAttribute {
    implicit val schema: Schema[WithAttribute] = DeriveSchema.gen
  }

  @xmlNamespace("http://example.com/ns", Some("ex"))
  case class WithNs(value: String)

  object WithNs {
    implicit val schema: Schema[WithNs] = DeriveSchema.gen
  }

  @xmlNamespace("http://example.com/ns")
  case class WithDefaultNs(value: String)

  object WithDefaultNs {
    implicit val schema: Schema[WithDefaultNs] = DeriveSchema.gen
  }

  case class WithAttrAndNs(@xmlAttribute @xmlNamespace("http://example.com/ns", Some("ex")) id: Int, name: String)

  object WithAttrAndNs {
    implicit val schema: Schema[WithAttrAndNs] = DeriveSchema.gen
  }

  @xmlNamespace("http://example.com/ns", Some("ex"))
  case class WithMultiFieldNs(name: String, age: Int, active: Boolean)

  object WithMultiFieldNs {
    implicit val schema: Schema[WithMultiFieldNs] = DeriveSchema.gen
  }

  case class WithList(items: List[Int])

  object WithList {
    implicit val schema: Schema[WithList] = DeriveSchema.gen
  }

  case class WithSet(tags: Set[String])

  object WithSet {
    implicit val schema: Schema[WithSet] = DeriveSchema.gen
  }

  case class WithMap(props: Map[String, Int])

  object WithMap {
    implicit val schema: Schema[WithMap] = DeriveSchema.gen
  }

  case class RecursiveTree(value: Int, children: List[RecursiveTree])

  object RecursiveTree {
    implicit lazy val schema: Schema[RecursiveTree] = DeriveSchema.gen
  }

  case class WithEither(choice: Either[String, Int])

  object WithEither {
    implicit val schema: Schema[WithEither] = DeriveSchema.gen
  }

  case class WithFallback(fb: Fallback[String, Int])

  object WithFallback {
    implicit val schema: Schema[WithFallback] = DeriveSchema.gen
  }

  case class WithFallbackFull(fb: Fallback[String, Int])

  object WithFallbackFull {
    implicit val schema: Schema[WithFallbackFull] = {
      val fbSchema = Schema.Fallback(Schema[String], Schema[Int], fullDecode = true)
      Schema.CaseClass1(
        TypeId.parse("zio.schema.codec.xml.XmlCodecSpec.WithFallbackFull"),
        Schema.Field("fb", fbSchema, get0 = _.fb, set0 = (_, v) => WithFallbackFull(v)),
        WithFallbackFull(_)
      )
    }
  }

  case class WithUnit(u: Unit)

  object WithUnit {
    implicit val schema: Schema[WithUnit] = DeriveSchema.gen
  }

  // ---------------------------------------------------------------------------
  // Helpers
  // ---------------------------------------------------------------------------

  private def roundTrip[A: Schema](value: A): Either[zio.schema.codec.DecodeError, A] = {
    val codec   = XmlCodec.schemaBasedBinaryCodec[A]
    val encoded = codec.encode(value)
    codec.decode(encoded)
  }

  private def roundTripAssert[A: Schema](value: A) =
    assertTrue(roundTrip(value) == Right(value))

  // ---------------------------------------------------------------------------
  // Spec
  // ---------------------------------------------------------------------------

  def spec: Spec[Any, Any] =
    suite("XmlCodecSpec")(
      primitivesSuite,
      recordsSuite,
      enumsSuite,
      collectionsSuite,
      eitherTupleOptionalFallbackSuite,
      transformAndLazySuite,
      xmlAnnotationsSuite,
      streamingSuite,
      configurationSuite,
      xmlReaderSuite,
      xmlWriterSuite,
      realWorldXmlSuite
    )

  // ---------------------------------------------------------------------------
  // 1. Primitive round-trips
  // ---------------------------------------------------------------------------

  private val primitivesSuite = suite("primitives")(
    test("String") {
      roundTripAssert("hello world")
    },
    test("Int") {
      roundTripAssert(42)
    },
    test("Long") {
      roundTripAssert(123456789L)
    },
    test("Boolean") {
      roundTripAssert(true) && roundTripAssert(false)
    },
    test("Double") {
      roundTripAssert(3.14)
    },
    test("Float") {
      roundTripAssert(2.718f)
    },
    test("Short") {
      roundTripAssert(Short.MaxValue)
    },
    test("Byte") {
      roundTripAssert(42.toByte)
    },
    test("Char") {
      roundTripAssert('Z')
    },
    test("BigInteger") {
      roundTripAssert(new java.math.BigInteger("123456789012345678901234567890"))
    },
    test("BigDecimal") {
      roundTripAssert(new java.math.BigDecimal("12345.6789"))
    },
    test("UUID") {
      val uuid = UUID.fromString("550e8400-e29b-41d4-a716-446655440000")
      roundTripAssert(uuid)
    },
    test("Binary (Chunk[Byte])") {
      val bytes = Chunk[Byte](1, 2, 3, 4, 5)
      roundTripAssert(bytes)
    },
    test("Unit in record") {
      roundTripAssert(WithUnit(()))
    },
    test("LocalDate") {
      roundTripAssert(LocalDate.of(2024, 6, 15))
    },
    test("LocalDateTime") {
      roundTripAssert(LocalDateTime.of(2024, 6, 15, 10, 30, 0))
    },
    test("LocalTime") {
      roundTripAssert(LocalTime.of(14, 30, 45))
    },
    test("Instant") {
      roundTripAssert(Instant.parse("2024-06-15T10:30:00Z"))
    },
    test("Duration") {
      roundTripAssert(java.time.Duration.ofHours(2).plusMinutes(30))
    },
    test("Period") {
      roundTripAssert(Period.of(1, 6, 15))
    },
    test("ZonedDateTime") {
      roundTripAssert(ZonedDateTime.of(2024, 6, 15, 10, 30, 0, 0, ZoneId.of("Europe/London")))
    },
    test("OffsetDateTime") {
      roundTripAssert(OffsetDateTime.of(2024, 6, 15, 10, 30, 0, 0, ZoneOffset.ofHours(2)))
    },
    test("OffsetTime") {
      roundTripAssert(OffsetTime.of(14, 30, 0, 0, ZoneOffset.ofHours(-5)))
    },
    test("DayOfWeek") {
      roundTripAssert(DayOfWeek.MONDAY)
    },
    test("Month") {
      roundTripAssert(Month.JANUARY)
    },
    test("MonthDay") {
      roundTripAssert(MonthDay.of(12, 25))
    },
    test("Year") {
      roundTripAssert(Year.of(2024))
    },
    test("YearMonth") {
      roundTripAssert(YearMonth.of(2024, 6))
    },
    test("ZoneId") {
      roundTripAssert(ZoneId.of("America/New_York"))
    },
    test("ZoneOffset") {
      roundTripAssert(ZoneOffset.ofHours(5))
    },
    test("Currency") {
      roundTripAssert(java.util.Currency.getInstance("USD"))
    } @@ TestAspect.jvmOnly,
    test("Float NaN round-trip") {
      val codec   = XmlCodec.schemaBasedBinaryCodec[Float]
      val encoded = codec.encode(Float.NaN)
      val result  = codec.decode(encoded)
      assertTrue(result.exists(_.isNaN))
    },
    test("Double NaN round-trip") {
      val codec   = XmlCodec.schemaBasedBinaryCodec[Double]
      val encoded = codec.encode(Double.NaN)
      val result  = codec.decode(encoded)
      assertTrue(result.exists(_.isNaN))
    },
    test("Float boundary values") {
      roundTripAssert(Float.MinValue) && roundTripAssert(Float.MaxValue)
    },
    test("Double boundary values") {
      roundTripAssert(Double.MinValue) && roundTripAssert(Double.MaxValue)
    },
    test("Int boundary values") {
      roundTripAssert(Int.MinValue) && roundTripAssert(Int.MaxValue)
    },
    test("Long boundary values") {
      roundTripAssert(Long.MinValue) && roundTripAssert(Long.MaxValue)
    },
    test("Byte boundary values") {
      roundTripAssert(Byte.MinValue) && roundTripAssert(Byte.MaxValue)
    },
    test("Short boundary values") {
      roundTripAssert(Short.MinValue) && roundTripAssert(Short.MaxValue)
    },
    test("Char special characters") {
      roundTripAssert('\u20AC')
    },
    test("empty String encoding produces empty output") {
      val codec   = XmlCodec.schemaBasedBinaryCodec[String]
      val encoded = codec.encode("")
      assertTrue(encoded.isEmpty || codec.decode(encoded).isLeft)
    },
    test("String with XML special chars") {
      roundTripAssert("Hello <>&\" World")
    },
    test("String with unicode") {
      roundTripAssert("Hello \u4E16\u754C \uD83C\uDF0D")
    },
    test("BigDecimal scientific notation") {
      roundTripAssert(new java.math.BigDecimal("1.23E+10"))
    }
  )

  // ---------------------------------------------------------------------------
  // 2. Record round-trips
  // ---------------------------------------------------------------------------

  private val recordsSuite = suite("records")(
    test("simple case class") {
      roundTripAssert(SimplePerson("Alice", 30))
    },
    test("nested case class") {
      roundTripAssert(Nested(SimplePerson("Bob", 25), "vip"))
    },
    test("optional field Some") {
      roundTripAssert(WithOptional("Alice", Some("Ali")))
    },
    test("optional field None") {
      roundTripAssert(WithOptional("Alice", None))
    },
    test("with @fieldName") {
      roundTripAssert(WithFieldName("Alice Smith", 30))
    },
    test("with @transientField") {
      val original = WithTransient("Alice", "mySecret")
      val result   = roundTrip(original)
      assertTrue(result == Right(WithTransient("Alice", "hidden")))
    }
  )

  // ---------------------------------------------------------------------------
  // 3. Enum / sealed trait round-trips
  // ---------------------------------------------------------------------------

  private val enumsSuite = suite("enums")(
    test("simple enum") {
      roundTripAssert[SimpleColor](SimpleColor.Red) &&
      roundTripAssert[SimpleColor](SimpleColor.Green)
    },
    test("sealed trait with case classes") {
      roundTripAssert[Shape](Shape.Circle(5.0)) &&
      roundTripAssert[Shape](Shape.Rect(3.0, 4.0))
    },
    test("with @discriminatorName") {
      roundTripAssert[Animal](Animal.Dog("Rex")) &&
      roundTripAssert[Animal](Animal.Cat("Whiskers"))
    },
    test("with @noDiscriminator") {
      roundTripAssert[Fruit](Fruit.Apple("Fuji")) &&
      roundTripAssert[Fruit](Fruit.Banana(20))
    }
  )

  // ---------------------------------------------------------------------------
  // 4. Collection round-trips
  // ---------------------------------------------------------------------------

  private val collectionsSuite = suite("collections")(
    test("List[Int]") {
      roundTripAssert(WithList(List(1, 2, 3)))
    },
    test("empty List") {
      roundTripAssert(WithList(List.empty))
    },
    test("Set[String]") {
      roundTripAssert(WithSet(Set("a", "b", "c")))
    },
    test("Map[String, Int]") {
      roundTripAssert(WithMap(Map("x" -> 1, "y" -> 2)))
    },
    test("empty Map") {
      roundTripAssert(WithMap(Map.empty))
    }
  )

  // ---------------------------------------------------------------------------
  // 5. Either, Tuple, Optional, Fallback
  // ---------------------------------------------------------------------------

  private val eitherTupleOptionalFallbackSuite = suite("either, tuple, optional, fallback")(
    test("Either Left") {
      roundTripAssert(WithEither(Left("error")))
    },
    test("Either Right") {
      roundTripAssert(WithEither(Right(42)))
    },
    test("Tuple2") {
      roundTripAssert(("hello", 42))
    },
    test("Option Some") {
      roundTripAssert(Option("hello"))
    },
    test("Option None") {
      roundTripAssert(Option.empty[String])
    },
    test("Fallback Left") {
      roundTripAssert(WithFallback(Fallback.Left("only-left")))
    },
    test("Fallback Right") {
      roundTripAssert(WithFallback(Fallback.Right(99)))
    },
    test("Fallback Both") {
      roundTripAssert(WithFallbackFull(Fallback.Both("text", 77)))
    }
  )

  // ---------------------------------------------------------------------------
  // 6. Transform and Lazy
  // ---------------------------------------------------------------------------

  private val transformAndLazySuite = suite("transform and lazy")(
    test("recursive (lazy) data structure") {
      val tree = RecursiveTree(1, List(RecursiveTree(2, Nil), RecursiveTree(3, List(RecursiveTree(4, Nil)))))
      roundTripAssert(tree)
    }
  )

  // ---------------------------------------------------------------------------
  // 7. XML-specific annotations
  // ---------------------------------------------------------------------------

  private val xmlAnnotationsSuite = suite("XML annotations")(
    test("@xmlAttribute round-trip") {
      roundTripAssert(WithAttribute(42, "Alice"))
    },
    test("@xmlNamespace round-trip") {
      roundTripAssert(WithNs("namespaced"))
    },
    test("@xmlNamespace without prefix (default ns)") {
      val codec   = XmlCodec.schemaBasedBinaryCodec[WithDefaultNs]
      val encoded = new String(codec.encode(WithDefaultNs("test")).toArray, "UTF-8")
      assertTrue(encoded.contains("WithDefaultNs")) &&
      roundTripAssert(WithDefaultNs("test"))
    },
    test("@xmlAttribute with @xmlNamespace combined") {
      val codec   = XmlCodec.schemaBasedBinaryCodec[WithAttrAndNs]
      val encoded = new String(codec.encode(WithAttrAndNs(1, "Alice")).toArray, "UTF-8")
      assertTrue(encoded.contains("id=")) &&
      roundTripAssert(WithAttrAndNs(1, "Alice"))
    },
    test("@xmlNamespace round-trip with multiple fields") {
      roundTripAssert(WithMultiFieldNs("Alice", 30, true))
    }
  )

  // ---------------------------------------------------------------------------
  // 8. Streaming encode/decode
  // ---------------------------------------------------------------------------

  private val streamingSuite = suite("streaming")(
    test("stream round-trip") {
      val codec  = XmlCodec.schemaBasedBinaryCodec[SimplePerson]
      val person = SimplePerson("Stream", 99)
      for {
        result <- ZStream
                   .succeed(person)
                   .via(codec.streamEncoder)
                   .via(codec.streamDecoder)
                   .run(ZSink.collectAll)
      } yield assertTrue(result == Chunk(person))
    }
  )

  // ---------------------------------------------------------------------------
  // 9. Configuration
  // ---------------------------------------------------------------------------

  private val configurationSuite = suite("configuration")(
    test("pretty printing produces indented XML") {
      val config = XmlCodec.Configuration(writerConfig = WriterConfig.pretty)
      val codec  = XmlCodec.schemaBasedBinaryCodec[SimplePerson](config)
      val person = SimplePerson("Pretty", 1)
      val xml    = new String(codec.encode(person).toArray, "UTF-8")
      assertTrue(xml.contains("\n")) && assertTrue(codec.decode(codec.encode(person)) == Right(person))
    },
    test("XML declaration") {
      val config = XmlCodec.Configuration(writerConfig = WriterConfig.withDeclaration)
      val codec  = XmlCodec.schemaBasedBinaryCodec[SimplePerson](config)
      val person = SimplePerson("Declared", 2)
      val xml    = new String(codec.encode(person).toArray, "UTF-8")
      assertTrue(xml.startsWith("<?xml")) && assertTrue(codec.decode(codec.encode(person)) == Right(person))
    }
  )

  // ---------------------------------------------------------------------------
  // 10. XmlReader parser tests
  // ---------------------------------------------------------------------------

  private val xmlReaderSuite = suite("XmlReader")(
    test("parse simple self-closing element") {
      val result = XmlReader.read("<root/>", ReaderConfig.default)
      assertTrue(result == Right(Xml.Element(XmlName("root"), Chunk.empty, Chunk.empty)))
    },
    test("parse element with text") {
      val result = XmlReader.read("<root>hello</root>", ReaderConfig.default)
      assertTrue(result == Right(Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.Text("hello")))))
    },
    test("parse element with attributes") {
      val result = XmlReader.read("""<root attr="value"/>""", ReaderConfig.default)
      assertTrue(
        result == Right(Xml.Element(XmlName("root"), Chunk((XmlName("attr"), "value")), Chunk.empty))
      )
    },
    test("parse nested elements") {
      val result = XmlReader.read("<root><child>text</child></root>", ReaderConfig.default)
      val expected = Xml.Element(
        XmlName("root"),
        Chunk.empty,
        Chunk(Xml.Element(XmlName("child"), Chunk.empty, Chunk(Xml.Text("text"))))
      )
      assertTrue(result == Right(expected))
    },
    test("decode &amp; entity") {
      val result = XmlReader.read("<root>&amp;</root>", ReaderConfig.default)
      assertTrue(result == Right(Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.Text("&")))))
    },
    test("decode &lt; entity") {
      val result = XmlReader.read("<root>&lt;</root>", ReaderConfig.default)
      assertTrue(result == Right(Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.Text("<")))))
    },
    test("decode &gt; entity") {
      val result = XmlReader.read("<root>&gt;</root>", ReaderConfig.default)
      assertTrue(result == Right(Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.Text(">")))))
    },
    test("decode &quot; entity in attribute") {
      val result = XmlReader.read("""<root attr="&quot;"/>""", ReaderConfig.default)
      result match {
        case Right(Xml.Element(_, attrs, _)) =>
          assertTrue(attrs.head._2 == "\"")
        case other =>
          assertTrue(other.toString == "unexpected")
      }
    },
    test("decode &apos; entity in attribute") {
      val result = XmlReader.read("""<root attr="&apos;"/>""", ReaderConfig.default)
      result match {
        case Right(Xml.Element(_, attrs, _)) =>
          assertTrue(attrs.head._2 == "'")
        case other =>
          assertTrue(other.toString == "unexpected")
      }
    },
    test("decode multiple entities") {
      val result = XmlReader.read("<root>&lt;&amp;&gt;</root>", ReaderConfig.default)
      assertTrue(result == Right(Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.Text("<&>")))))
    },
    test("parse CDATA section") {
      val result = XmlReader.read("<root><![CDATA[<>&\"]]></root>", ReaderConfig.default)
      assertTrue(
        result == Right(Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.CData("<>&\""))))
      )
    },
    test("parse comment") {
      val result = XmlReader.read("<root><!-- comment --></root>", ReaderConfig.default)
      assertTrue(
        result == Right(Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.Comment(" comment "))))
      )
    },
    test("parse processing instruction") {
      val result = XmlReader.read("<root><?target data?></root>", ReaderConfig.default)
      assertTrue(
        result == Right(
          Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.ProcessingInstruction("target", "data")))
        )
      )
    },
    test("preserve text whitespace in content") {
      val result = XmlReader.read("<root>  text  </root>", ReaderConfig.default)
      assertTrue(result == Right(Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.Text("  text  ")))))
    },
    test("preserve whitespace when configured") {
      val config = ReaderConfig(preserveWhitespace = true)
      val result = XmlReader.read("<root>  <child/>  </root>", config)
      result match {
        case Right(Xml.Element(_, _, children)) =>
          assertTrue(children.size == 3)
        case other =>
          assertTrue(other.toString == "unexpected")
      }
    },
    test("ignore whitespace-only text nodes") {
      val result = XmlReader.read("<root>  <child/>  </root>", ReaderConfig.default)
      result match {
        case Right(Xml.Element(_, _, children)) =>
          assertTrue(children.size == 1) &&
            assertTrue(children.head.isInstanceOf[Xml.Element])
        case other =>
          assertTrue(other.toString == "unexpected")
      }
    },
    test("reject unclosed element") {
      val result = XmlReader.read("<root>", ReaderConfig.default)
      assertTrue(result.isLeft)
    },
    test("reject mismatched tags") {
      val result = XmlReader.read("<root></other>", ReaderConfig.default)
      assertTrue(result.isLeft)
    },
    test("reject invalid element name") {
      val result = XmlReader.read("<123/>", ReaderConfig.default)
      assertTrue(result.isLeft)
    },
    test("reject element exceeding max depth at root") {
      val config = ReaderConfig(maxDepth = -1)
      val result = XmlReader.read("<root/>", config)
      assertTrue(result.isLeft)
    },
    test("reject exceeding max attributes") {
      val config = ReaderConfig(maxAttributes = 1)
      val result = XmlReader.read("""<root a="1" b="2"/>""", config)
      assertTrue(result.isLeft)
    },
    test("reject text exceeding max length") {
      val config = ReaderConfig(maxTextLength = 3)
      val result = XmlReader.read("<root>12345</root>", config)
      assertTrue(result.isLeft)
    },
    test("error includes line and column info") {
      val result = XmlReader.read("<root></other>", ReaderConfig.default)
      result match {
        case Left(err) =>
          val msg = err.toString
          assertTrue(msg.contains("line") && msg.contains("column"))
        case Right(_) =>
          assertTrue(false)
      }
    },
    test("parse mixed content") {
      val result = XmlReader.read("<root>text1<child/>text2</root>", ReaderConfig.default)
      result match {
        case Right(Xml.Element(_, _, children)) =>
          assertTrue(children.size == 3) &&
            assertTrue(children(0).isInstanceOf[Xml.Text]) &&
            assertTrue(children(1).isInstanceOf[Xml.Element]) &&
            assertTrue(children(2).isInstanceOf[Xml.Text])
        case other =>
          assertTrue(other.toString == "unexpected")
      }
    },
    test("parse element with namespace prefix") {
      val result = XmlReader.read("""<ns:root xmlns:ns="http://example.com"/>""", ReaderConfig.default)
      result match {
        case Right(Xml.Element(name, _, _)) =>
          assertTrue(name.prefix == Some("ns")) &&
            assertTrue(name.localName == "root")
        case other =>
          assertTrue(other.toString == "unexpected")
      }
    }
  )

  // ---------------------------------------------------------------------------
  // 11. XmlWriter serializer tests
  // ---------------------------------------------------------------------------

  private val xmlWriterSuite = suite("XmlWriter")(
    test("write self-closing element") {
      val xml    = Xml.Element(XmlName("root"), Chunk.empty, Chunk.empty)
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(result == "<root/>")
    },
    test("write element with text") {
      val xml    = Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.Text("content")))
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(result == "<root>content</root>")
    },
    test("write element with attributes") {
      val xml = Xml.Element(
        XmlName("root"),
        Chunk((XmlName("id"), "123"), (XmlName("name"), "test")),
        Chunk.empty
      )
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(result == """<root id="123" name="test"/>""")
    },
    test("escape & in text") {
      val xml    = Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.Text("A & B")))
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(result.contains("A &amp; B"))
    },
    test("escape < in text") {
      val xml    = Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.Text("A < B")))
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(result.contains("A &lt; B"))
    },
    test("escape > in text") {
      val xml    = Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.Text("A > B")))
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(result.contains("A &gt; B"))
    },
    test("escape & in attribute") {
      val xml    = Xml.Element(XmlName("root"), Chunk((XmlName("a"), "A & B")), Chunk.empty)
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(result.contains("&amp;"))
    },
    test("escape < in attribute") {
      val xml    = Xml.Element(XmlName("root"), Chunk((XmlName("a"), "A < B")), Chunk.empty)
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(result.contains("&lt;"))
    },
    test("escape quote in attribute") {
      val xml    = Xml.Element(XmlName("root"), Chunk((XmlName("a"), "A\"B")), Chunk.empty)
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(result.contains("&quot;"))
    },
    test("write CDATA") {
      val xml    = Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.CData("data")))
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(result.contains("<![CDATA[data]]>"))
    },
    test("write comment") {
      val xml    = Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.Comment("comment")))
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(result.contains("<!--comment-->"))
    },
    test("write processing instruction") {
      val xml    = Xml.Element(XmlName("root"), Chunk.empty, Chunk(Xml.ProcessingInstruction("target", "data")))
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(result.contains("<?target data?>"))
    },
    test("write with indentation") {
      val xml = Xml.Element(
        XmlName("root"),
        Chunk.empty,
        Chunk(Xml.Element(XmlName("child"), Chunk.empty, Chunk(Xml.Text("text"))))
      )
      val result = XmlWriter.write(xml, WriterConfig(indentStep = 2))
      assertTrue(result.contains("\n"))
    },
    test("write without indentation by default") {
      val xml = Xml.Element(
        XmlName("root"),
        Chunk.empty,
        Chunk(Xml.Element(XmlName("child"), Chunk.empty, Chunk(Xml.Text("text"))))
      )
      val result = XmlWriter.write(xml, WriterConfig.default)
      assertTrue(!result.contains("\n"))
    },
    test("write with XML declaration") {
      val xml    = Xml.Element(XmlName("root"), Chunk.empty, Chunk.empty)
      val result = XmlWriter.write(xml, WriterConfig(includeDeclaration = true))
      assertTrue(result.startsWith("<?xml"))
    },
    test("write with encoding in declaration") {
      val xml    = Xml.Element(XmlName("root"), Chunk.empty, Chunk.empty)
      val result = XmlWriter.write(xml, WriterConfig(includeDeclaration = true, encoding = "UTF-16"))
      assertTrue(result.contains("UTF-16"))
    }
  )

  // ---------------------------------------------------------------------------
  // 12. Real-world XML round-trips
  // ---------------------------------------------------------------------------

  private val realWorldXmlSuite = suite("real-world XML round-trips")(
    test("RSS feed round-trip") {
      val rss =
        """<?xml version="1.0" encoding="UTF-8"?><rss xmlns:atom="http://www.w3.org/2005/Atom" version="2.0"><channel><title><![CDATA[BBC News]]></title><description><![CDATA[BBC News - World]]></description><link>https://www.bbc.co.uk/news/world</link><image><url>https://news.bbcimg.co.uk/nol/shared/img/bbc_news_120x60.gif</url><title>BBC News</title><link>https://www.bbc.co.uk/news/world</link></image><lastBuildDate>Sun, 08 Feb 2026 15:51:39 GMT</lastBuildDate><atom:link href="https://feeds.bbci.co.uk/news/world/rss.xml" rel="self" type="application/rss+xml"/><item><title><![CDATA[Japan's governing party projected to win snap election]]></title><description><![CDATA[A coalition led by current PM is expected to clinch a decisive win.]]></description><link>https://www.bbc.com/news/articles/cx2y7d2z29xo</link><guid isPermaLink="false">https://www.bbc.com/news/articles/cx2y7d2z29xo#0</guid><pubDate>Sun, 08 Feb 2026 14:23:19 GMT</pubDate></item><item><title><![CDATA[Polls close in Thai election]]></title><description><![CDATA[Thai voters choose between sweeping change or more of the same.]]></description><link>https://www.bbc.com/news/articles/cx2jn4z4eq0o</link><guid isPermaLink="false">https://www.bbc.com/news/articles/cx2jn4z4eq0o#0</guid><pubDate>Sun, 08 Feb 2026 14:47:10 GMT</pubDate></item></channel></rss>"""
      val parsed1 = XmlReader.read(rss, ReaderConfig.default)
      parsed1 match {
        case Right(xml1) =>
          val written = XmlWriter.write(xml1, WriterConfig.default)
          val parsed2 = XmlReader.read(written, ReaderConfig.default)
          assertTrue(parsed2 == Right(xml1))
        case Left(err) =>
          assertTrue(err.toString == "unexpected")
      }
    },
    test("Atom feed round-trip") {
      val atom =
        """<?xml version="1.0" encoding="UTF-8"?><feed xmlns="http://www.w3.org/2005/Atom" xml:lang="en-US"><id>tag:github.com,2008:https://github.com/zio/zio/releases</id><link type="text/html" rel="alternate" href="https://github.com/zio/zio/releases"/><link type="application/atom+xml" rel="self" href="https://github.com/zio/zio/releases.atom"/><title>Release notes from zio</title><updated>2025-12-28T03:25:46Z</updated><entry><id>tag:github.com,2008:Repository/134079884/v2.1.24</id><updated>2025-12-29T19:57:55Z</updated><link rel="alternate" type="text/html" href="https://github.com/zio/zio/releases/tag/v2.1.24"/><title>2.1.24</title><content type="html">This release focuses on performance improvements.</content></entry><entry><id>tag:github.com,2008:Repository/134079884/v2.1.23</id><updated>2025-11-15T10:30:00Z</updated><link rel="alternate" type="text/html" href="https://github.com/zio/zio/releases/tag/v2.1.23"/><title>2.1.23</title><content type="html">Bug fixes and stability improvements.</content></entry></feed>"""
      val parsed1 = XmlReader.read(atom, ReaderConfig.default)
      parsed1 match {
        case Right(xml1) =>
          val written = XmlWriter.write(xml1, WriterConfig.default)
          val parsed2 = XmlReader.read(written, ReaderConfig.default)
          assertTrue(parsed2 == Right(xml1))
        case Left(err) =>
          assertTrue(err.toString == "unexpected")
      }
    },
    test("Sitemap round-trip") {
      val sitemap =
        """<?xml version="1.0" encoding="UTF-8"?><urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9"><url><loc>https://www.example.org/</loc><lastmod>2024-01-15</lastmod></url><url><loc>https://www.example.org/about</loc><lastmod>2024-01-10</lastmod></url><url><loc>https://www.example.org/products</loc><lastmod>2024-01-20</lastmod></url><url><loc>https://www.example.org/contact</loc><lastmod>2024-01-05</lastmod></url></urlset>"""
      val parsed1 = XmlReader.read(sitemap, ReaderConfig.default)
      parsed1 match {
        case Right(xml1) =>
          val written = XmlWriter.write(xml1, WriterConfig.default)
          val parsed2 = XmlReader.read(written, ReaderConfig.default)
          assertTrue(parsed2 == Right(xml1))
        case Left(err) =>
          assertTrue(err.toString == "unexpected")
      }
    }
  )
}
