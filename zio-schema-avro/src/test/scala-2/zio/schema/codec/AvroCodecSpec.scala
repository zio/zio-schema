package zio.schema.codec

import java.math.BigInteger
import java.time.{
  DayOfWeek,
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
import java.util.UUID

import org.apache.avro.generic.GenericData

import zio._
import zio.schema.codec.AvroAnnotations.avroEnum
import zio.schema.{ DeriveSchema, Fallback, Schema }
import zio.stream.ZStream
import zio.test._

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

  case class Ingredient(name: String, sugar: Double, fat: Double)
  case class Pizza(name: String, ingredients: List[Ingredient], vegetarian: Boolean, vegan: Boolean, calories: Int)

  object Pizza {
    implicit val schemaIngredient: Schema[Ingredient] = DeriveSchema.gen[Ingredient]
    implicit val schemaPizza: Schema[Pizza]           = DeriveSchema.gen[Pizza]
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

    case object NullValue extends OneOf

    implicit val schemaOneOf: Schema[OneOf] = DeriveSchema.gen[OneOf]
  }

  @avroEnum() sealed trait Enums

  object Enums {
    case object A extends Enums
    case object B extends Enums
    case object C extends Enums
    case object D extends Enums

    case object E extends Enums

    implicit val schemaEnums: Schema[Enums] = DeriveSchema.gen[Enums]
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Avro Codec Spec")(
    primitiveEncoderSpec,
    collectionsEncoderSpec,
    optionEncoderSpec,
    eitherEncoderSpec,
    fallbackEncoderSpec,
    tupleEncoderSpec,
    genericRecordEncoderSpec,
    caseClassEncoderSpec,
    enumEncoderSpec,
    primitiveDecoderSpec,
    optionDecoderSpec,
    eitherDecoderSpec,
    fallbackDecoderSpec,
    tupleDecoderSpec,
    sequenceDecoderSpec,
    genericRecordDecoderSpec,
    enumDecoderSpec,
    streamEncodingDecodingSpec,
    genericRecordEncodeDecodeSpec
  )

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
      val bytes = codec.encode(600000000000L)
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
    test("Encode ZoneOffset") {
      val codec = AvroCodec.schemaBasedBinaryCodec[ZoneOffset]
      val bytes = codec.encode(ZoneOffset.MAX)
      assertTrue(bytes.length == 3)
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
    },
    test("Encode Option[Option[Int]]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Option[Option[Int]]]
      val bytes = codec.encode(Some(Some(42)))
      assertTrue(bytes.length == 3)
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
    },
    test("Encode Either[String, Option[Int]]") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Either[String, Option[Int]]]
      val bytes = codec.encode(Right(Some(42)))
      assertTrue(bytes.length == 3)
    }
  )

  private val fallbackEncoderSpec = suite("Avro Codec - Encoder Fallback spec")(
    test("Encode Fallback.Right") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Fallback[Int, String]]
      val bytes = codec.encode(Fallback.Right("John"))
      assertTrue(bytes.length == 7)
    },
    test("Encode Fallback.Left") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Fallback[Int, String]]
      val bytes = codec.encode(Fallback.Left(42))
      assertTrue(bytes.length == 3)
    },
    test("Encode Fallback.Both") {
      val codec = AvroCodec.schemaBasedBinaryCodec[Fallback[Int, String]]
      val bytes = codec.encode(Fallback.Both(42, "John"))
      assertTrue(bytes.length == 8)
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

  private val primitiveDecoderSpec = suite("Avro Codec - Primitive decoder spec")(
    test("Decode Unit") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Unit]
      val bytes  = codec.encode(())
      val result = codec.decode(bytes)
      assertTrue(result == Right(()))
    },
    test("Decode Boolean") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Boolean]
      val bytes  = codec.encode(true)
      val result = codec.decode(bytes)
      assertTrue(result == Right(true))
    },
    test("Decode String") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[String]
      val bytes  = codec.encode("John")
      val result = codec.decode(bytes)
      assertTrue(result == Right("John"))
    },
    test("Decode Byte") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Byte]
      val bytes  = codec.encode(42.toByte)
      val result = codec.decode(bytes)
      assertTrue(result == Right(42.toByte))
    },
    test("Decode Short") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Short]
      val bytes  = codec.encode(42.toShort)
      val result = codec.decode(bytes)
      assertTrue(result == Right(42.toShort))
    },
    test("Decode Integer") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Int]
      val bytes  = codec.encode(42)
      val result = codec.decode(bytes)
      assertTrue(result == Right(42))
    },
    test("Decode Long") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Long]
      val bytes  = codec.encode(42L)
      val result = codec.decode(bytes)
      assertTrue(result == Right(42L))
    },
    test("Decode Float") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Float]
      val bytes  = codec.encode(42.0f)
      val result = codec.decode(bytes)
      assertTrue(result == Right(42.0f))
    },
    test("Decode Double") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Double]
      val bytes  = codec.encode(42.0)
      val result = codec.decode(bytes)
      assertTrue(result == Right(42.0))
    },
    test("Decode Chunk[Byte]") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Chunk[Byte]]
      val bytes  = codec.encode(Chunk.fromArray(Array[Byte](1, 2, 3)))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Chunk.fromArray(Array[Byte](1, 2, 3))))
    },
    test("Decode Char") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Char]
      val bytes  = codec.encode('a')
      val result = codec.decode(bytes)
      assertTrue(result == Right('a'))
    },
    test("Decode UUID") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[UUID]
      val uuid   = UUID.randomUUID()
      val bytes  = codec.encode(uuid)
      val result = codec.decode(bytes)
      assertTrue(result == Right(uuid))
    },
    test("Decode BigDecimal") {
      val codec      = AvroCodec.schemaBasedBinaryCodec[BigDecimal]
      val bigDecimal = BigDecimal(42)
      val bytes      = codec.encode(bigDecimal)
      val result     = codec.decode(bytes)
      assertTrue(result == Right(bigDecimal))
    },
    test("Decode BigInt") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[BigInt]
      val bigInt = BigInt(42)
      val bytes  = codec.encode(bigInt)
      val result = codec.decode(bytes)
      assertTrue(result == Right(bigInt))
    },
    test("Decode DayOfWeek") {
      val codec     = AvroCodec.schemaBasedBinaryCodec[DayOfWeek]
      val dayOfWeek = DayOfWeek.FRIDAY
      val bytes     = codec.encode(dayOfWeek)
      val result    = codec.decode(bytes)
      assertTrue(result == Right(dayOfWeek))
    },
    test("Decode Month") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Month]
      val month  = Month.APRIL
      val bytes  = codec.encode(month)
      val result = codec.decode(bytes)
      assertTrue(result == Right(month))
    },
    test("Decode MonthDay") {
      val codec    = AvroCodec.schemaBasedBinaryCodec[MonthDay]
      val monthDay = MonthDay.of(1, 1)
      val bytes    = codec.encode(monthDay)
      val result   = codec.decode(bytes)
      assertTrue(result == Right(monthDay))
    },
    test("Decode Period") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Period]
      val period = Period.of(1, 1, 1)
      val bytes  = codec.encode(period)
      val result = codec.decode(bytes)
      assertTrue(result == Right(period))
    },
    test("Decode Year") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Year]
      val year   = Year.of(2020)
      val bytes  = codec.encode(year)
      val result = codec.decode(bytes)
      assertTrue(result == Right(year))
    },
    test("Decode YearMonth") {
      val codec     = AvroCodec.schemaBasedBinaryCodec[YearMonth]
      val yearMonth = YearMonth.of(2020, 1)
      val bytes     = codec.encode(yearMonth)
      val result    = codec.decode(bytes)
      assertTrue(result == Right(yearMonth))
    },
    test("Decode ZoneId") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[ZoneId]
      val zoneId = ZoneId.of("UTC")
      val bytes  = codec.encode(zoneId)
      val result = codec.decode(bytes)
      assertTrue(result == Right(zoneId))
    },
    test("Decode ZoneOffset") {
      val codec      = AvroCodec.schemaBasedBinaryCodec[ZoneOffset]
      val zoneOffset = ZoneOffset.ofHours(1)
      val bytes      = codec.encode(zoneOffset)
      val result     = codec.decode(bytes)
      assertTrue(result == Right(zoneOffset))
    },
    test("Decode Duration") {
      val codec    = AvroCodec.schemaBasedBinaryCodec[Duration]
      val duration = Duration.fromMillis(2000)
      val bytes    = codec.encode(duration)
      val result   = codec.decode(bytes)
      assertTrue(result == Right(duration))
    },
    test("Decode Instant") {
      val codec   = AvroCodec.schemaBasedBinaryCodec[Instant]
      val instant = Instant.now()
      val bytes   = codec.encode(instant)
      val result  = codec.decode(bytes)
      assertTrue(result == Right(instant))
    },
    test("Decode LocalDate") {
      val codec     = AvroCodec.schemaBasedBinaryCodec[LocalDate]
      val localDate = LocalDate.now()
      val bytes     = codec.encode(localDate)
      val result    = codec.decode(bytes)
      assertTrue(result == Right(localDate))
    },
    test("Decode LocalDateTime") {
      val codec         = AvroCodec.schemaBasedBinaryCodec[LocalDateTime]
      val localDateTime = LocalDateTime.now()
      val bytes         = codec.encode(localDateTime)
      val result        = codec.decode(bytes)
      assertTrue(result == Right(localDateTime))
    },
    test("Decode LocalTime") {
      val codec     = AvroCodec.schemaBasedBinaryCodec[LocalTime]
      val localTime = LocalTime.now()
      val bytes     = codec.encode(localTime)
      val result    = codec.decode(bytes)
      assertTrue(result == Right(localTime))
    },
    test("Decode OffsetDateTime") {
      val codec          = AvroCodec.schemaBasedBinaryCodec[OffsetDateTime]
      val offsetDateTime = OffsetDateTime.now()
      val bytes          = codec.encode(offsetDateTime)
      val result         = codec.decode(bytes)
      assertTrue(result == Right(offsetDateTime))
    },
    test("Decode OffsetTime") {
      val codec      = AvroCodec.schemaBasedBinaryCodec[OffsetTime]
      val offsetTime = OffsetTime.now()
      val bytes      = codec.encode(offsetTime)
      val result     = codec.decode(bytes)
      assertTrue(result == Right(offsetTime))
    },
    test("Decode ZonedDateTime") {
      val codec         = AvroCodec.schemaBasedBinaryCodec[ZonedDateTime]
      val zonedDateTime = ZonedDateTime.now()
      val bytes         = codec.encode(zonedDateTime)
      val result        = codec.decode(bytes)
      assertTrue(result == Right(zonedDateTime))
    }
  )

  private val optionDecoderSpec = suite("Avro Codec - Option Decoder spec")(
    test("Decode Option") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Option[Int]]
      val bytes  = codec.encode(Some(42))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Some(42)))
    },
    test("Decode Option[Option[_]]") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Option[Option[Int]]]
      val bytes  = codec.encode(Some(Some(42)))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Some(Some(42))))
    },
    test("Decode Option[Enum]") {
      sealed trait Enum

      object Enum {
        case class Case1() extends Enum
        case class Case2() extends Enum
        implicit val schemaEnum: Schema[Enum] = DeriveSchema.gen[Enum]
      }

      val codec  = AvroCodec.schemaBasedBinaryCodec[Option[Enum]]
      val bytes  = codec.encode(Some(Enum.Case1()))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Some(Enum.Case1())))
    }
  )

  private val eitherDecoderSpec = suite("Avro Codec - Either Decoder spec")(
    test("Decode Either") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Either[String, Int]]
      val bytes  = codec.encode(Right(42))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Right(42)))
    },
    test("Decode Either[List[String], Int]") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Either[List[String], Int]]
      val bytes  = codec.encode(Left(List("John", "Adam", "Daniel")))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Left(List("John", "Adam", "Daniel"))))
    },
    test("Decode Either[String, Option[Int]]") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Either[String, Option[Int]]]
      val bytes  = codec.encode(Right(Some(42)))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Right(Some(42))))
    }
  )

  private val fallbackDecoderSpec = suite("Avro Codec - Fallback Decoder spec")(
    test("Decode Fallback") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Fallback[String, Int]]
      val bytes  = codec.encode(Fallback.Right(42))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Fallback.Right(42)))
    },
    test("Decode Fallback[List[String], Int]") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Fallback[List[String], Int]]
      val bytes  = codec.encode(Fallback.Left(List("John", "Adam", "Daniel")))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Fallback.Left(List("John", "Adam", "Daniel"))))
    },
    test("Decode Fallback.Both full decode") {
      val codec  = AvroCodec.schemaBasedBinaryCodec(Schema.Fallback[String, Int](Schema[String], Schema[Int], true))
      val bytes  = codec.encode(Fallback.Both("hello", 42))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Fallback.Both("hello", 42)))
    },
    test("Decode Fallback.Both non full decode") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Fallback[String, Int]]
      val bytes  = codec.encode(Fallback.Both("hello", 42))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Fallback.Left("hello")))
    }
  )

  private val tupleDecoderSpec = suite("Avro Codec - Tuple Decoder Spec")(
    test("Decode Tuple2") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[(Int, String)]
      val bytes  = codec.encode((42, "42"))
      val result = codec.decode(bytes)
      assertTrue(result == Right((42, "42")))
    }
  )

  private val sequenceDecoderSpec = suite("Avro Codec - Sequence Decoder spec")(
    test("Decode List") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[List[Int]]
      val bytes  = codec.encode(List(42))
      val result = codec.decode(bytes)
      assertTrue(result == Right(List(42)))
    },
    test("Decode Set") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Set[Int]]
      val bytes  = codec.encode(Set(42))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Set(42)))
    },
    test("Decode Map") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Map[String, Int]]
      val bytes  = codec.encode(Map("42" -> 42))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Map("42" -> 42)))
    },
    test("Decode Chunk") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Chunk[String]]
      val bytes  = codec.encode(Chunk("42", "John", "Adam"))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Chunk("42", "John", "Adam")))
    },
    test("Decode Chunk[Option[Int]]") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Chunk[Option[Int]]]
      val bytes  = codec.encode(Chunk(Some(42), Some(53), None, Some(64)))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Chunk(Some(42), Some(53), None, Some(64))))
    }
  )

  private val genericRecordDecoderSpec = suite("Avro Codec - Decode Generic Record")(
    test("Decode Record") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Record]
      val bytes  = codec.encode(Record("John", 42))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Record("John", 42)))
    },
    test("Decode High Arity") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[HighArity]
      val bytes  = codec.encode(HighArity())
      val result = codec.decode(bytes)
      assertTrue(result == Right(HighArity()))
    }
  )

  private val enumDecoderSpec = suite("Avro Codec - Decode enum")(
    test("Decode Enum3") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[OneOf]
      val bytes  = codec.encode(OneOf.BooleanValue(true))
      val result = codec.decode(bytes)
      assertTrue(result == Right(OneOf.BooleanValue(true)))
    },
    test("Decode Enum3 - case object") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[OneOf]
      val bytes  = codec.encode(OneOf.NullValue)
      val result = codec.decode(bytes)
      assertTrue(result == Right(OneOf.NullValue))
    },
    test("Decode Enum5") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Enums]
      val bytes  = codec.encode(Enums.A)
      val result = codec.decode(bytes)
      assertTrue(result == Right(Enums.A))
    },
    test("Decode Person") {
      val codec  = AvroCodec.schemaBasedBinaryCodec[Person]
      val bytes  = codec.encode(Person("John", 42))
      val result = codec.decode(bytes)
      assertTrue(result == Right(Person("John", 42)))
    },
    test("Decode CaseClass3") {
      val ennio  = Composer("ennio morricone", "rome", List("legend of 1900", "ecstasy of gold"))
      val codec  = AvroCodec.schemaBasedBinaryCodec[Composer]
      val bytes  = codec.encode(ennio)
      val result = codec.decode(bytes)
      assertTrue(result == Right(ennio))
    }
  )

  private val streamEncodingDecodingSpec =
    suite("AvroCodec - Stream encode/decode")(test("Encoding/Decoding using streams") {

      val pepperoni =
        Pizza("pepperoni", List(Ingredient("pepperoni", 12, 4.4), Ingredient("onions", 1, 0.4)), false, false, 98)
      val codec = AvroCodec.schemaBasedBinaryCodec[Pizza]
      val resultZIO = ZStream
        .fromIterable(List(pepperoni))
        .via(codec.streamEncoder)
        .via(codec.streamDecoder)
        .runCollect
      for {
        result <- resultZIO
      } yield assertTrue(result == Chunk(pepperoni))

    })

  private val genericRecordEncodeDecodeSpec = suite("AvroCodec - encode/decode Generic Record")(
    test("Encode/Decode") {
      val codec                       = AvroCodec.schemaBasedBinaryCodec[Record]
      val generic: GenericData.Record = codec.encodeGenericRecord(Record("John", 42))
      val result                      = codec.decodeGenericRecord(generic)
      assertTrue(result == Right(Record("John", 42)))
    }
  )

}
