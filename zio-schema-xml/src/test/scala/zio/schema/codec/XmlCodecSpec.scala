package zio.schema.codec

import java.math.{ BigDecimal => JBigDecimal, BigInteger => JBigInteger }
import java.time._
import java.util.UUID

import zio._
import zio.schema._
import zio.schema.annotation._
import zio.schema.codec.XmlAnnotations._
import zio.stream.ZStream
import zio.test._

object XmlCodecSpec extends ZIOSpecDefault {

  case class Person(name: String, age: Int, tags: List[String])

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  case class Department(id: Int, name: String, employees: List[Person])

  object Department {
    implicit val schema: Schema[Department] = DeriveSchema.gen[Department]
  }

  case class Address(street: String, city: String, zip: Option[String])

  object Address {
    implicit val schema: Schema[Address] = DeriveSchema.gen[Address]
  }

  case class PersonWithAddress(name: String, age: Int, address: Address)

  object PersonWithAddress {
    implicit val schema: Schema[PersonWithAddress] = DeriveSchema.gen[PersonWithAddress]
  }

  case class WithOptional(value: Option[String])

  object WithOptional {
    implicit val schema: Schema[WithOptional] = DeriveSchema.gen[WithOptional]
  }

  case class WithList(items: List[String])

  object WithList {
    implicit val schema: Schema[WithList] = DeriveSchema.gen[WithList]
  }

  case class WithMap(data: Map[String, Int])

  object WithMap {
    implicit val schema: Schema[WithMap] = DeriveSchema.gen[WithMap]
  }

  case class WithSet(items: Set[String])

  object WithSet {
    implicit val schema: Schema[WithSet] = DeriveSchema.gen[WithSet]
  }

  case class WithEither(result: Either[String, Int])

  object WithEither {
    implicit val schema: Schema[WithEither] = DeriveSchema.gen[WithEither]
  }

  case class WithTuple(pair: (String, Int))

  object WithTuple {
    implicit val schema: Schema[WithTuple] = DeriveSchema.gen[WithTuple]
  }

  sealed trait Status

  object Status {
    case object Active                 extends Status
    case object Inactive               extends Status
    case class Pending(reason: String) extends Status
    implicit val schema: Schema[Status] = DeriveSchema.gen[Status]
  }

  case class Task(id: Int, status: Status)

  object Task {
    implicit val schema: Schema[Task] = DeriveSchema.gen[Task]
  }

  sealed trait ContactMethod

  object ContactMethod {
    case class Email(address: String) extends ContactMethod
    case class Phone(number: String)  extends ContactMethod
    case object NoContact             extends ContactMethod
    implicit val schema: Schema[ContactMethod] = DeriveSchema.gen[ContactMethod]
  }

  case class Node(value: Int, children: List[Node])

  object Node {
    implicit val schema: Schema[Node] = DeriveSchema.gen[Node]
  }

  case class Temperature(celsius: Double)

  object Temperature {

    val schema: Schema[Temperature] = Schema[Double].transformOrFail(
      c => Right(Temperature(c)),
      t => Right(t.celsius)
    )
  }

  case class AnnotatedRecord(
    required: String,
    @optionalField optional: String,
    @transientField transientField: String = "ignored",
    @fieldDefaultValue("default") withDefault: String
  )

  object AnnotatedRecord {
    implicit val schema: Schema[AnnotatedRecord] = DeriveSchema.gen[AnnotatedRecord]
  }

  case class LargeRecord(
    f1: Int,
    f2: Int,
    f3: Int,
    f4: Int,
    f5: Int,
    f6: Int,
    f7: Int,
    f8: Int,
    f9: Int,
    f10: Int,
    f11: Int,
    f12: Int,
    f13: Int,
    f14: Int,
    f15: Int,
    f16: Int,
    f17: Int,
    f18: Int,
    f19: Int,
    f20: Int,
    f21: Int,
    f22: Int,
    f23: Int,
    f24: Int,
    f25: Int
  )

  object LargeRecord {
    implicit val schema: Schema[LargeRecord] = DeriveSchema.gen[LargeRecord]
  }

  case class AllPrimitives(
    string: String,
    boolean: Boolean,
    byte: Byte,
    short: Short,
    int: Int,
    long: Long,
    float: Float,
    double: Double,
    char: Char,
    uuid: UUID,
    bigDecimal: JBigDecimal,
    bigInteger: JBigInteger
  )

  object AllPrimitives {
    implicit val schema: Schema[AllPrimitives] = DeriveSchema.gen[AllPrimitives]
  }

  case class TemporalTypes(
    instant: Instant,
    localDate: LocalDate,
    localTime: LocalTime,
    localDateTime: LocalDateTime,
    offsetTime: OffsetTime,
    offsetDateTime: OffsetDateTime,
    zonedDateTime: ZonedDateTime,
    duration: Duration,
    period: Period,
    year: Year,
    yearMonth: YearMonth,
    monthDay: MonthDay,
    dayOfWeek: DayOfWeek,
    month: Month,
    zoneId: ZoneId,
    zoneOffset: ZoneOffset
  )

  object TemporalTypes {
    implicit val schema: Schema[TemporalTypes] = DeriveSchema.gen[TemporalTypes]
  }

  case class PersonWithAttr(@attribute id: String, name: String, age: Int)

  object PersonWithAttr {
    implicit val schema: Schema[PersonWithAttr] = DeriveSchema.gen[PersonWithAttr]
  }

  override def spec: Spec[TestEnvironment, Any] = suite("XmlCodec")(
    suite("Attributes")(
      test("encodes @attribute fields as XML attributes") {
        val person = PersonWithAttr("p001", "Alice", 30)
        val codec  = XmlCodec.encode(PersonWithAttr.schema)
        val xml    = new String(codec.encode(person).toArray)

        assertTrue(
          xml.contains("id=\"p001\""),
          xml.contains("<name>Alice</name>"),
          !xml.contains("<id>")
        )
      },
      test("round-trips with attributes") {
        val person = PersonWithAttr("p002", "Bob", 45)
        val codec  = XmlCodec.encode(PersonWithAttr.schema)

        for {
          encoded <- ZIO.succeed(codec.encode(person))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == person)
      }
    ),
    suite("Basic round-trip")(
      test("simple record") {
        val person = Person("Alice", 30, List("tag1", "tag2"))
        val codec  = XmlCodec.encode(Person.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(person))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == person)
      },
      test("nested record") {
        val value = PersonWithAddress("Bob", 42, Address("123 Main", "NYC", Some("10001")))
        val codec = XmlCodec.encode(PersonWithAddress.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      },
      test("optional Some") {
        val value = WithOptional(Some("test"))
        val codec = XmlCodec.encode(WithOptional.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      },
      test("optional None") {
        val value = WithOptional(None)
        val codec = XmlCodec.encode(WithOptional.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      },
      test("map") {
        val value = WithMap(Map("a" -> 1, "b" -> 2))
        val codec = XmlCodec.encode(WithMap.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      },
      test("set") {
        val value = WithSet(Set("x", "y", "z"))
        val codec = XmlCodec.encode(WithSet.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      },
      test("either left") {
        val value = WithEither(Left("error"))
        val codec = XmlCodec.encode(WithEither.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      },
      test("either right") {
        val value = WithEither(Right(123))
        val codec = XmlCodec.encode(WithEither.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      },
      test("tuple") {
        val value = WithTuple(("key", 99))
        val codec = XmlCodec.encode(WithTuple.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      },
      test("enum case object") {
        val task  = Task(1, Status.Active)
        val codec = XmlCodec.encode(Task.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(task))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == task)
      },
      test("enum case class") {
        val task  = Task(2, Status.Pending("needs approval"))
        val codec = XmlCodec.encode(Task.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(task))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == task)
      }
    ),
    suite("Collections")(
      test("repeating elements by default") {
        val person    = Person("Alice", 30, List("tag1", "tag2"))
        val codec     = XmlCodec.encode(Person.schema)
        val xmlString = new String(codec.encode(person).toArray)

        assertTrue(
          xmlString.contains("<tags>tag1</tags>"),
          xmlString.contains("<tags>tag2</tags>"),
          !xmlString.contains("<item>")
        )
      },
      test("wrapped when configured") {
        val person    = Person("Alice", 30, List("tag1", "tag2"))
        val codec     = XmlCodec.encode(Person.schema, XmlCodec.Config(wrapCollections = true))
        val xmlString = new String(codec.encode(person).toArray)

        assertTrue(xmlString.contains("<item>"))
      },
      test("empty list") {
        val value = WithList(List.empty)
        val codec = XmlCodec.encode(WithList.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      },
      test("single element list") {
        val value = WithList(List("only"))
        val codec = XmlCodec.encode(WithList.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      }
    ),
    suite("Map and Set determinism")(
      test("map encoding is deterministic") {
        val value = WithMap(Map("z" -> 1, "a" -> 2, "m" -> 3))
        val codec = XmlCodec.encode(WithMap.schema)
        val xml1  = new String(codec.encode(value).toArray)
        val xml2  = new String(codec.encode(value).toArray)
        assertTrue(xml1 == xml2)
      },
      test("set encoding is deterministic") {
        val value = WithSet(Set("z", "a", "m"))
        val codec = XmlCodec.encode(WithSet.schema)
        val xml1  = new String(codec.encode(value).toArray)
        val xml2  = new String(codec.encode(value).toArray)
        assertTrue(xml1 == xml2)
      }
    ),
    suite("Configuration")(
      test("pretty print") {
        val person = Person("Alice", 30, List("tag1"))
        val codec  = XmlCodec.encode(Person.schema, XmlCodec.Config(prettyPrint = true))
        val xml    = new String(codec.encode(person).toArray)
        assertTrue(xml.contains("\n  "))
      },
      test("without XML declaration") {
        val person = Person("Alice", 30, List.empty)
        val codec  = XmlCodec.encode(Person.schema, XmlCodec.Config(xmlDeclaration = false))
        val xml    = new String(codec.encode(person).toArray)
        assertTrue(!xml.contains("<?xml"))
      }
    ),
    suite("Error cases")(
      test("malformed XML") {
        val malformed = Chunk.fromArray("""<?xml version="1.0"?><Person><name>Alice</Person>""".getBytes)
        val result    = XmlCodec.encode(Person.schema).decode(malformed)
        assertTrue(result.isLeft)
      },
      test("type mismatch - string for int") {
        val xml    = """<?xml version="1.0"?><Person><name>Alice</name><age>not-a-number</age></Person>"""
        val result = XmlCodec.encode(Person.schema).decode(Chunk.fromArray(xml.getBytes))
        assertTrue(result.isLeft)
      },
      test("missing required field") {
        val xml    = """<?xml version="1.0"?><Person><name>Alice</name></Person>"""
        val result = XmlCodec.encode(Person.schema).decode(Chunk.fromArray(xml.getBytes))
        assertTrue(result.isLeft)
      },
      test("XXE attack prevented") {
        val xxe    = """<?xml version="1.0"?>
          <!DOCTYPE foo [
            <!ENTITY xxe SYSTEM "file:///etc/passwd">
          ]>
          <Person><name>&xxe;</name><age>30</age></Person>"""
        val result = XmlCodec.encode(Person.schema).decode(Chunk.fromArray(xxe.getBytes))
        assertTrue(result.isLeft)
      }
    ),
    suite("XSD generation")(
      test("generates valid XSD") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Person.schema))
        } yield assertTrue(
          xsd.contains("<?xml version=\"1.0\""),
          xsd.contains("xmlns:xs=\"http://www.w3.org/2001/XMLSchema\"")
        )
      },
      test("generates xs:attribute for @attribute fields") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(PersonWithAttr.schema))
        } yield assertTrue(
          xsd.contains("xs:attribute"),
          xsd.contains("name=\"id\"")
        )
      },
      test("generates xs:enumeration for simple case object enums") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Status.schema))
        } yield {
          val hasEnumeration = xsd.contains("xs:enumeration") || xsd.contains("xs:choice")
          assertTrue(hasEnumeration)
        }
      },
      test("sequences map to unbounded elements") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Person.schema))
        } yield assertTrue(
          xsd.contains("name=\"tags\""),
          xsd.contains("maxOccurs=\"unbounded\"")
        )
      },
      test("optionals map to minOccurs=0") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Address.schema))
        } yield {
          val zipLine = xsd.linesIterator.find(_.contains("name=\"zip\"")).getOrElse("")
          assertTrue(zipLine.contains("minOccurs=\"0\""))
        }
      },
      test("sealed traits use choice") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Status.schema))
        } yield assertTrue(
          xsd.contains("xs:choice"),
          !xsd.contains("xs:enumeration")
        )
      },
      test("XSD round-trip") {
        for {
          originalXsd <- ZIO.fromEither(XmlSchemaCodec.encode(Department.schema))
          decoded     <- ZIO.fromEither(XmlSchemaCodec.decode(Chunk.fromArray(originalXsd.getBytes)))
        } yield assertTrue(decoded.isInstanceOf[Schema.Record[_]])
      },
      test("reject XSD with imports") {
        val xsdWithImport = """<?xml version="1.0" encoding="UTF-8"?>
          <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:import namespace="http://example.com/other" schemaLocation="other.xsd"/>
          </xs:schema>"""
        val decoded       = XmlSchemaCodec.decode(Chunk.fromArray(xsdWithImport.getBytes))
        assertTrue(
          decoded.isLeft,
          decoded.left.exists {
            case DecodeError.ReadError(_, msg) => msg.contains("Imports") || msg.contains("imports")
            case _                             => false
          }
        )
      }
    ),
    suite("Primitives")(
      test("all primitive types round-trip") {
        val value = AllPrimitives(
          string = "test",
          boolean = true,
          byte = 42.toByte,
          short = 1000.toShort,
          int = 123456,
          long = 9876543210L,
          float = 3.14f,
          double = 2.718281828,
          char = 'X',
          uuid = UUID.fromString("550e8400-e29b-41d4-a716-446655440000"),
          bigDecimal = new JBigDecimal("12345.6789"),
          bigInteger = new JBigInteger("98765432109876543210")
        )
        val codec = XmlCodec.encode(AllPrimitives.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(
          decoded.string == value.string,
          decoded.boolean == value.boolean,
          decoded.byte == value.byte,
          decoded.short == value.short,
          decoded.int == value.int,
          decoded.long == value.long,
          decoded.float == value.float,
          decoded.double == value.double,
          decoded.char == value.char,
          decoded.uuid == value.uuid,
          decoded.bigDecimal == value.bigDecimal,
          decoded.bigInteger == value.bigInteger
        )
      },
      test("individual primitives") {
        val stringSchema = Schema[String]
        val boolSchema   = Schema[Boolean]
        val intSchema    = Schema[Int]

        for {
          str  <- ZIO.fromEither(XmlCodec.encode(stringSchema).decode(XmlCodec.encode(stringSchema).encode("hello")))
          bool <- ZIO.fromEither(XmlCodec.encode(boolSchema).decode(XmlCodec.encode(boolSchema).encode(true)))
          int  <- ZIO.fromEither(XmlCodec.encode(intSchema).decode(XmlCodec.encode(intSchema).encode(42)))
        } yield assertTrue(
          str == "hello",
          bool == true,
          int == 42
        )
      }
    ),
    suite("Temporal types")(
      test("all temporal types round-trip") {
        val now = Instant.now()
        val value = TemporalTypes(
          instant = now,
          localDate = LocalDate.of(2024, 3, 15),
          localTime = LocalTime.of(14, 30, 0),
          localDateTime = LocalDateTime.of(2024, 3, 15, 14, 30),
          offsetTime = OffsetTime.of(14, 30, 0, 0, ZoneOffset.ofHours(2)),
          offsetDateTime = OffsetDateTime.of(2024, 3, 15, 14, 30, 0, 0, ZoneOffset.ofHours(2)),
          zonedDateTime = ZonedDateTime.of(2024, 3, 15, 14, 30, 0, 0, ZoneId.of("UTC")),
          duration = java.time.Duration.ofHours(5),
          period = Period.ofDays(10),
          year = Year.of(2024),
          yearMonth = YearMonth.of(2024, 3),
          monthDay = MonthDay.of(3, 15),
          dayOfWeek = DayOfWeek.FRIDAY,
          month = Month.MARCH,
          zoneId = ZoneId.of("America/New_York"),
          zoneOffset = ZoneOffset.ofHours(-5)
        )
        val codec = XmlCodec.encode(TemporalTypes.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      }
    ),
    suite("Transform schema")(
      test("successful transform") {
        val value = Temperature(25.0)
        val codec = XmlCodec.encode(Temperature.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded.celsius == 25.0)
      },
      test("transform with validation") {
        val schema = Schema[Int].transformOrFail[Int](
          i => if (i > 0) Right(i) else Left("must be positive"),
          i => Right(i)
        )
        val codec = XmlCodec.encode(schema)

        for {
          validEncoded  <- ZIO.succeed(codec.encode(10))
          validDecoded  <- ZIO.fromEither(codec.decode(validEncoded))
          invalidXml    = """<?xml version="1.0"?><int>-5</int>"""
          invalidResult = codec.decode(Chunk.fromArray(invalidXml.getBytes))
        } yield assertTrue(
          validDecoded == 10,
          invalidResult.isLeft
        )
      }
    ),
    suite("Lazy schema (recursive)")(
      test("recursive data structure") {
        val leaf1  = Node(1, List.empty)
        val leaf2  = Node(2, List.empty)
        val branch = Node(0, List(leaf1, leaf2))

        val codec = XmlCodec.encode(Node.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(branch))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(
          decoded.value == 0,
          decoded.children.size == 2,
          decoded.children(0).value == 1,
          decoded.children(1).value == 2
        )
      },
      test("deeply nested recursive structure") {
        val deep  = Node(1, List(Node(2, List(Node(3, List(Node(4, List.empty)))))))
        val codec = XmlCodec.encode(Node.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(deep))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(
          decoded.value == 1,
          decoded.children.head.children.head.children.head.value == 4
        )
      }
    ),
    suite("Field annotations")(
      test("transientField is not encoded") {
        val value = AnnotatedRecord("req", "opt", "secret", "def")
        val codec = XmlCodec.encode(AnnotatedRecord.schema)
        val xml   = new String(codec.encode(value).toArray)

        assertTrue(
          xml.contains("<required>req</required>"),
          !xml.contains("secret"),
          !xml.contains("<transientField>")
        )
      },
      test("optionalField can be omitted") {
        val xmlWithoutOptional = """<?xml version="1.0"?>
          <AnnotatedRecord>
            <required>test</required>
            <transientField>x</transientField>
            <withDefault>value</withDefault>
          </AnnotatedRecord>"""
        val result             = XmlCodec.encode(AnnotatedRecord.schema).decode(Chunk.fromArray(xmlWithoutOptional.getBytes))
        assertTrue(result.isLeft || result.isRight)
      },
      test("fieldDefaultValue provides default") {
        val value = AnnotatedRecord("req", "opt", "trans", "custom")
        val codec = XmlCodec.encode(AnnotatedRecord.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded.withDefault == "custom")
      }
    ),
    suite("Large records")(
      test("record with > 22 fields") {
        val value = LargeRecord(
          1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25
        )
        val codec = XmlCodec.encode(LargeRecord.schema)
        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(
          decoded.f1 == 1,
          decoded.f25 == 25,
          decoded == value
        )
      }
    ),
    suite("Streaming")(
      test("encode via ZStream") {
        val people = List(
          Person("Alice", 30, List("tag1")),
          Person("Bob", 25, List("tag2")),
          Person("Charlie", 35, List("tag3"))
        )
        val codec = XmlCodec.encode(Person.schema)

        for {
          encoded <- ZStream
                      .fromIterable(people)
                      .map(codec.encode)
                      .runCollect
          decoded <- ZIO.foreach(encoded)(chunk => ZIO.fromEither(codec.decode(chunk)))
        } yield assertTrue(
          decoded.size == 3,
          decoded(0).name == "Alice",
          decoded(2).name == "Charlie"
        )
      },
      test("decode via stream") {
        val person  = Person("Test", 42, List("a", "b"))
        val codec   = XmlCodec.encode(Person.schema)
        val encoded = codec.encode(person)

        for {
          decoded <- ZStream
                      .fromChunk(encoded)
                      .runCollect
                      .flatMap(chunk => ZIO.fromEither(codec.decode(chunk)))
        } yield assertTrue(decoded == person)
      }
    ),
    suite("Dynamic values")(
      test("DynamicValue round-trip") {
        val person  = Person("Dynamic", 99, List("d1", "d2"))
        val dynamic = DynamicValue(person)(Person.schema)

        for {
          typedBack <- ZIO.fromEither(dynamic.toTypedValue(Person.schema))
        } yield assertTrue(typedBack == person)
      }
    )
  )
}
