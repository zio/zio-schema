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
      configurationSuite
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
    } @@ TestAspect.jvmOnly
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
}
