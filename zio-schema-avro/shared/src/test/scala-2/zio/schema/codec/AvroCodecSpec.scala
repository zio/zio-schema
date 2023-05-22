package zio.schema.codec

import zio._
import zio.schema.{ DeriveSchema, Schema }
import zio.test._

import java.math.BigInteger
import java.time.{ DayOfWeek, Month, MonthDay, Period, Year, YearMonth, ZoneId, ZoneOffset }
import java.util.UUID

object AvroCodecSpec extends ZIOSpecDefault {

  final case class Person(name: String, age: Int)

  object Person {
    implicit lazy val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  case class Record(name: String, value: Int)

  object Record {
    implicit val schemaRecord: Schema[Record] = DeriveSchema.gen[Record]
  }

  case class Class0()

  object Class0 {
    implicit val schemaClass0: Schema[Class0] = DeriveSchema.gen[Class0]
  }

  case class Class1(value: Int)

  object Class1 {
    implicit val schemaClass0: Schema[Class1] = DeriveSchema.gen[Class1]
  }

  case class Composer(name: String, birthplace: String, compositions: List[String])

  object Composer {
    implicit val schemaComposer: Schema[Composer] = DeriveSchema.gen[Composer]
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
    f22: Int = 22,
    f23: Int = 23,
    f24: Int = 24
  )

  object HighArity {
    implicit val schemaHighArityRecord: Schema[HighArity] = DeriveSchema.gen[HighArity]
  }

  sealed trait OneOf

  object OneOf {
    case class StringValue(value: String) extends OneOf

    case class IntValue(value: Int) extends OneOf

    case class BooleanValue(value: Boolean) extends OneOf

    implicit val schemaOneOf: Schema[OneOf] = DeriveSchema.gen[OneOf]
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Avro Codec Spec")(
    primitiveEncoderSpec,
    collectionsEncoderSpec,
    optionEncoderSpec,
    eitherEncoderSpec,
    tupleEncoderSpec,
    genericRecordEncoderSpec,
    caseClassEncoderSpec,
    enumEncoderSpec
  )
//    test("Encode simple case class") {
//        val person = Person("John", 42)
//        val codec  = AvroCodec.schemaBasedBinaryCodec[Person]
//        val bytes  = codec.encode(person)
//        val decoded: Either[DecodeError, Person] = codec.decode(bytes)
//        assertTrue(decoded.isRight, decoded.getOrElse(Person("fake", 0)) == person)
//    }
//  )

  private val primitiveEncoderSpec = suite("Avro Codec - Encoder primitive spec")(
    test("Encode string") {
      val codec = AvroCodec.schemaBasedBinaryCodec[String]
      val bytes = codec.encode("Hello")
      assertTrue(bytes.length == 6)
    },
    test("Encode Int") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Int]
      val bytes = codec.encode(42)
      assertTrue(bytes.length == 1)
    },
    test("Encode Short") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Short]
      val bytes = codec.encode(42)
      assertTrue(bytes.length == 1)
    },
    test("Encode Long") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Long]
      val bytes = codec.encode(600_000_000_000L)
      assertTrue(bytes.length == 6)
    },
    test("Encode Float") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Float]
      val bytes = codec.encode(42.0f)
      assertTrue(bytes.length == 4)
    },
    test("Encode Double") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Double]
      val bytes = codec.encode(42.0)
      assertTrue(bytes.length == 8)
    },
    test("Encode Boolean") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Boolean]
      val bytes = codec.encode(true)
      assertTrue(bytes.length == 1)
    },
    test("Encode Char") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Char]
      val bytes = codec.encode('a')
      assertTrue(bytes.length == 2)
    },
    test("Encode UUID") {
      val codec = AvroCodec.schemaBasedBinaryCodec[UUID]
      val bytes = codec.encode(UUID.randomUUID())
      assertTrue(bytes.length == 37)
    },
    test("Encode BigDecimal") {
      val codec = AvroCodec.schemaBasedBinaryCodec[BigDecimal]
      val bytes = codec.encode(BigDecimal.valueOf(42.0))
      assertTrue(bytes.length == 12)
    },
    test("Encode BigDecimal") {
      val codec = AvroCodec.schemaBasedBinaryCodec[BigInteger]
      val bytes = codec.encode(BigInteger.valueOf(42))
      assertTrue(bytes.length == 12)
    },
    test("Encode DayOfWeek") {
      val codec = AvroCodec.schemaBasedBinaryCodec[DayOfWeek]
      val bytes = codec.encode(DayOfWeek.FRIDAY)
      assertTrue(bytes.length == 1)
    },
    test("Encode Month") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Month]
      val bytes = codec.encode(Month.APRIL)
      assertTrue(bytes.length == 1)
    },
    test("Encode MonthDay") {
      val codec = AvroCodec.schemaBasedBinaryCodec[MonthDay]
      val bytes = codec.encode(MonthDay.of(4, 1))
      assertTrue(bytes.length == 8)
    },
    test("Encode Period") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Period]
      val bytes = codec.encode(Period.ofDays(4))
      assertTrue(bytes.length == 4)
    },
    test("Encode Year") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Year]
      val bytes = codec.encode(Year.of(2021))
      assertTrue(bytes.length == 2)
    },
    test("Encode YearMonth") {
      val codec = AvroCodec.schemaBasedBinaryCodec[YearMonth]
      val bytes = codec.encode(YearMonth.of(2021, 4))
      assertTrue(bytes.length == 8)
    },
    test("Encode ZoneId") {
      val codec = AvroCodec.schemaBasedBinaryCodec[ZoneId]
      val bytes = codec.encode(ZoneId.systemDefault())
      assertTrue(bytes.length == 17)
    },
    test("Encode ZoneOffset") {
      val codec = AvroCodec.schemaBasedBinaryCodec[ZoneOffset]
      val bytes = codec.encode(ZoneOffset.MAX)
      assertTrue(bytes.length == 3)
    },
    test("Encode Duration") {
      val codec = AvroCodec.schemaBasedBinaryCodec[java.time.Duration]
      val bytes = codec.encode(java.time.Duration.ofDays(4))
      assertTrue(bytes.length == 6)
    },
    test("Encode Instant") {
      val codec = AvroCodec.schemaBasedBinaryCodec[java.time.Instant]
      val bytes = codec.encode(java.time.Instant.now())
      assertTrue(bytes.length == 28)
    },
    test("Encode LocalDate") {
      val codec = AvroCodec.schemaBasedBinaryCodec[java.time.LocalDate]
      val bytes = codec.encode(java.time.LocalDate.now())
      assertTrue(bytes.length == 11)
    },
    test("Encode LocalTime") {
      val codec = AvroCodec.schemaBasedBinaryCodec[java.time.LocalTime]
      val bytes = codec.encode(java.time.LocalTime.now())
      assertTrue(bytes.length == 16)
    },
    test("Encode LocalDateTime") {
      val codec = AvroCodec.schemaBasedBinaryCodec[java.time.LocalDateTime]
      val bytes = codec.encode(java.time.LocalDateTime.now())
      assertTrue(bytes.length == 27)
    },
    test("Encode OffsetTime") {
      val codec = AvroCodec.schemaBasedBinaryCodec[java.time.OffsetTime]
      val bytes = codec.encode(java.time.OffsetTime.now())
      assertTrue(bytes.length == 22)
    },
    test("Encode OffsetDateTime") {
      val codec = AvroCodec.schemaBasedBinaryCodec[java.time.OffsetDateTime]
      val bytes = codec.encode(java.time.OffsetDateTime.now())
      assertTrue(bytes.length == 33)
    },
    test("Encode ZonedDateTime") {
      val codec = AvroCodec.schemaBasedBinaryCodec[java.time.ZonedDateTime]
      val bytes = codec.encode(java.time.ZonedDateTime.now())
      assertTrue(bytes.length == 51)
    },
    test("Encode Unit") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Unit]
      val bytes = codec.encode(())
      assertTrue(bytes.isEmpty)
    }
  )

  private val collectionsEncoderSpec = suite("Avro Codec - Encoder Collection spec")(
    test("Encode Chunk[Byte]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Chunk[Byte]]
      val bytes = codec.encode(Chunk.fromArray(Array[Byte](1, 2, 3)))
      assertTrue(bytes.length == 5)
    },
    test("Encode Chunk[String]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Chunk[String]]
      val bytes = codec.encode(Chunk.fromArray(Array[String]("John", "Adam", "Daniel")))
      assertTrue(bytes.length == 19)
    },
    test("Encode List[String]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[List[String]]
      val bytes = codec.encode(List("John", "Adam", "Daniel"))
      assertTrue(bytes.length == 19)
    },
    test("Encode Set[String]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Set[String]]
      val bytes = codec.encode(Set("John", "Adam", "Daniel"))
      assertTrue(bytes.length == 19)
    },
    test("Encode Map[String, String]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Map[String, String]]
      val bytes = codec.encode(Map("Name" -> "John", "Name" -> "Adam", "Name" -> "Daniel"))
      assertTrue(bytes.length == 14)
    }
  )

  private val optionEncoderSpec = suite("Avro Codec - Encoder Option spec")(
    test("Encode Option[Int]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Option[Int]]
      val bytes = codec.encode(Some(42))
      assertTrue(bytes.length == 2)
    },
    test("Encode List[Option[Int]]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[List[Option[Int]]]
      val bytes = codec.encode(List(Some(42), Some(53), None, Some(64)))
      assertTrue(bytes.length == 10)
    },
    test("Encode Chunk[Option[Int]]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Chunk[Option[Int]]]
      val bytes = codec.encode(Chunk(Some(42), Some(53), None, Some(64)))
      assertTrue(bytes.length == 10)
    }
  )

  private val eitherEncoderSpec = suite("Avro Codec - Encoder Either spec")(
    test("Encode Either[Int, String]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Either[Int, String]]
      val bytes = codec.encode(Right("John"))
      assertTrue(bytes.length == 6)
    },
    test("Encode Either[Int, String]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Either[Int, String]]
      val bytes = codec.encode(Left(42))
      assertTrue(bytes.length == 2)
    },
    test("Encode Either[List[String], Int]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Either[List[String], Int]]
      val bytes = codec.encode(Left(List("John", "Adam", "Daniel")))
      assertTrue(bytes.length == 20)
    }
  )

  private val tupleEncoderSpec = suite("Avro Codec - Encode Tuples spec")(
    test("Encode Tuple2[Int, String]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[(Int, String)]
      val bytes = codec.encode(Tuple2(42, "Test"))
      assertTrue(bytes.length == 6)
    }
  )

  private val genericRecordEncoderSpec = suite("Avro Codec - Encode Generic Record")(
    test("Encode Record") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Record]
      val bytes = codec.encode(Record("John", 42))
      assertTrue(bytes.length == 6)
    },
    test("Encode High Arity") {
      val codec = AvroCodec.schemaBasedBinaryCodec[HighArity]
      val bytes = codec.encode(HighArity())
      assertTrue(bytes.length == 24)
    }
  )

  private val caseClassEncoderSpec = suite("Avro Codec - Encode Case class")(
    test("Encode Case Class 0") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Class0]
      val bytes = codec.encode(Class0())
      assertTrue(bytes.isEmpty)
    },
    test("Encode Case Class 1") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Class1]
      val bytes = codec.encode(Class1(42))
      assertTrue(bytes.length == 1)
    },
    test("Encode Case Class 2") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Record]
      val bytes = codec.encode(Record("John", 42))
      assertTrue(bytes.length == 6)
    },
    test("Encode Case Class 3") {
      val ennio = Composer("ennio morricone", "rome", List("legend of 1900", "ecstasy of gold"))
      val codec = AvroCodec.schemaBasedBinaryCodec[Composer]
      val bytes = codec.encode(ennio)
      val expectedResult = (Array[Byte](30, 101, 110, 110, 105, 111, 32, 109, 111, 114, 114, 105, 99, 111, 110, 101, 8,
          114, 111, 109, 101, 4, 28, 108, 101, 103, 101, 110, 100, 32, 111, 102, 32, 49, 57, 48, 48, 30, 101, 99, 115,
          116, 97, 115, 121, 32, 111, 102, 32, 103, 111, 108, 100, 0))
      assertTrue(bytes == Chunk.fromArray(expectedResult))
    }
  )

  private val enumEncoderSpec = suite("Avro Codec - Encode Enum")(
    test("Encode Enum3") {
      val codec = AvroCodec.schemaBasedBinaryCodec[OneOf]
      val bytes = codec.encode(OneOf.BooleanValue(true))
      assertTrue(bytes.length == 2)
    }
  )

}
