package zio.schema.codec

import zio._
import zio.schema._
import zio.test.Assertion._
import zio.test._

object XmlCodecSpec extends ZIOSpecDefault {

  case class Person(name: String, age: Int)

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  case class Address(street: String, city: String, zip: String)

  object Address {
    implicit val schema: Schema[Address] = DeriveSchema.gen[Address]
  }

  case class Company(name: String, address: Address, employees: List[Person])

  object Company {
    implicit val schema: Schema[Company] = DeriveSchema.gen[Company]
  }

  sealed trait Color

  object Color {
    case object Red                          extends Color
    case class Custom(r: Int, g: Int, b: Int) extends Color

    implicit val schema: Schema[Color] = DeriveSchema.gen[Color]
  }

  case class WithOptional(name: String, nickname: Option[String])

  object WithOptional {
    implicit val schema: Schema[WithOptional] = DeriveSchema.gen[WithOptional]
  }

  private def roundTrip[A](value: A)(implicit schema: Schema[A]): Either[DecodeError, A] = {
    val codec   = XmlCodec.xmlCodec[A]
    val encoded = codec.encode(value)
    codec.decode(encoded)
  }

  override def spec: Spec[TestEnvironment, Any] = suite("XmlCodecSpec")(
    suite("primitives")(
      test("String roundtrip") {
        val result = roundTrip("hello world")
        assertTrue(result == Right("hello world"))
      },
      test("Int roundtrip") {
        val result = roundTrip(42)
        assertTrue(result == Right(42))
      },
      test("Long roundtrip") {
        val result = roundTrip(123456789L)
        assertTrue(result == Right(123456789L))
      },
      test("Boolean roundtrip") {
        val result = roundTrip(true)
        assertTrue(result == Right(true))
      },
      test("Double roundtrip") {
        val result = roundTrip(3.14)
        assertTrue(result == Right(3.14))
      },
      test("Float roundtrip") {
        val result = roundTrip(2.5f)
        assertTrue(result == Right(2.5f))
      },
      test("BigDecimal roundtrip") {
        val bd     = new java.math.BigDecimal("123.456")
        val result = roundTrip(bd)(Schema.Primitive(StandardType.BigDecimalType))
        assertTrue(result == Right(bd))
      },
      test("UUID roundtrip") {
        val uuid   = java.util.UUID.randomUUID()
        val result = roundTrip(uuid)(Schema.Primitive(StandardType.UUIDType))
        assertTrue(result == Right(uuid))
      },
      test("String with XML special characters") {
        val str    = """<hello> & "world" 'test'"""
        val result = roundTrip(str)
        assertTrue(result == Right(str))
      }
    ),
    suite("case classes")(
      test("simple case class roundtrip") {
        val person = Person("Alice", 30)
        val result = roundTrip(person)
        assertTrue(result == Right(person))
      },
      test("nested case class roundtrip") {
        val company = Company(
          "ZIO Inc",
          Address("123 Main St", "Springfield", "62701"),
          List(Person("Bob", 25), Person("Carol", 35))
        )
        val result = roundTrip(company)
        assertTrue(result == Right(company))
      }
    ),
    suite("collections")(
      test("List roundtrip") {
        val list   = List(1, 2, 3)
        val result = roundTrip(list)
        assertTrue(result == Right(list))
      },
      test("empty List roundtrip") {
        val list   = List.empty[Int]
        val result = roundTrip(list)
        assertTrue(result == Right(list))
      },
      test("Map roundtrip") {
        val map    = Map("a" -> 1, "b" -> 2)
        val schema = Schema.map[String, Int]
        val result = roundTrip(map)(schema)
        assertTrue(result == Right(map))
      },
      test("Set roundtrip") {
        val set    = Set(1, 2, 3)
        val schema = Schema.set[Int]
        val result = roundTrip(set)(schema)
        assertTrue(result == Right(set))
      }
    ),
    suite("optional")(
      test("Some value roundtrip") {
        val value  = WithOptional("Alice", Some("Al"))
        val result = roundTrip(value)
        assertTrue(result == Right(value))
      },
      test("None value roundtrip") {
        val value  = WithOptional("Bob", None)
        val result = roundTrip(value)
        assertTrue(result == Right(value))
      }
    ),
    suite("either")(
      test("Left roundtrip") {
        val value: Either[String, Int] = Left("error")
        val schema                     = Schema.either[String, Int]
        val result                     = roundTrip(value)(schema)
        assertTrue(result == Right(value))
      },
      test("Right roundtrip") {
        val value: Either[String, Int] = Right(42)
        val schema                     = Schema.either[String, Int]
        val result                     = roundTrip(value)(schema)
        assertTrue(result == Right(value))
      }
    ),
    suite("enum")(
      test("singleton enum roundtrip") {
        val value: Color = Color.Red
        val result       = roundTrip(value)
        assertTrue(result == Right(value))
      },
      test("case class enum roundtrip") {
        val value: Color = Color.Custom(255, 128, 0)
        val result       = roundTrip(value)
        assertTrue(result == Right(value))
      }
    ),
    suite("tuple")(
      test("tuple roundtrip") {
        val value  = ("hello", 42)
        val schema = Schema.tuple2[String, Int]
        val result = roundTrip(value)(schema)
        assertTrue(result == Right(value))
      }
    ),
    suite("temporal types")(
      test("LocalDate roundtrip") {
        val value  = java.time.LocalDate.of(2026, 3, 5)
        val result = roundTrip(value)(Schema.Primitive(StandardType.LocalDateType))
        assertTrue(result == Right(value))
      },
      test("Instant roundtrip") {
        val value  = java.time.Instant.parse("2026-03-05T12:00:00Z")
        val result = roundTrip(value)(Schema.Primitive(StandardType.InstantType))
        assertTrue(result == Right(value))
      },
      test("Duration roundtrip") {
        val value  = java.time.Duration.ofHours(2).plusMinutes(30)
        val result = roundTrip(value)(Schema.Primitive(StandardType.DurationType))
        assertTrue(result == Right(value))
      }
    ),
    suite("encoding format")(
      test("produces valid XML with declaration") {
        val codec   = XmlCodec.xmlCodec[Person]
        val encoded = new String(codec.encode(Person("Alice", 30)).toArray, "UTF-8")
        assertTrue(
          encoded.startsWith("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"),
          encoded.contains("<name>Alice</name>"),
          encoded.contains("<age>30</age>")
        )
      }
    )
  )
}
