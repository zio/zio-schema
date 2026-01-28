package zio.schema.codec

import zio._
import zio.schema._
import zio.test.Assertion._
import zio.test._

object ScalaXbParitySpec extends ZIOSpecDefault {

  import zio.schema.DeriveSchema

  case class Person(name: String, age: Int, tags: List[String])

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  case class Department(id: Int, name: String, employees: List[Person])

  object Department {
    implicit val schema: Schema[Department] = DeriveSchema.gen[Department]
  }

  case class Container(optionalVal: Option[String])

  object Container {
    implicit val schema: Schema[Container] = DeriveSchema.gen[Container]
  }

  case class PersonWithAge(name: String, age: Int)

  object PersonWithAge {
    implicit val schema: Schema[PersonWithAge] = DeriveSchema.gen[PersonWithAge]
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

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ScalaXbParitySpec")(
    test("sequences map to unbounded XSD elements") {
      for {
        xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Person.schema))
      } yield {

        assert(xsd)(containsString("""name="tags"""")) &&
        assert(xsd)(containsString("""maxOccurs="unbounded"""")) &&
        assert(xsd)(containsString("""type="xs:string""""))
      }
    },
    test("options map to minOccurs=0") {
      for {
        xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Container.schema))
      } yield {
        // <xs:element name="optionalVal" type="xs:string" minOccurs="0" maxOccurs="1"/>
        assert(xsd)(containsString("""name="optionalVal"""")) &&
        assert(xsd)(containsString("""minOccurs="0"""")) &&
        assert(xsd)(containsString("""maxOccurs="1""""))
      }
    },
    test("round trip encodes matching structure") {
      val p = Person("Alice", 30, List("tag1", "tag2"))

      for {
        encoded   <- ZIO.succeed(XmlCodec.encode(Person.schema).encode(p))
        xmlString = new String(encoded.toArray)
      } yield {
        assert(xmlString)(containsString("<tags>tag1</tags>")) &&
        assert(xmlString)(containsString("<tags>tag2</tags>")) &&
        assert(xmlString)(not(containsString("<item>")))
      }
    },
    test("full round trip: encode and decode preserves data") {
      val person = PersonWithAge("Bob", 42)
      val codec  = XmlCodec.encode(PersonWithAge.schema)

      for {
        encoded <- ZIO.succeed(codec.encode(person))
        decoded <- ZIO.fromEither(codec.decode(encoded))
      } yield assertTrue(decoded == person)
    },
    test("Map encoding round trip") {
      val value = WithMap(Map("a" -> 1, "b" -> 2))
      val codec = XmlCodec.encode(WithMap.schema)

      for {
        encoded <- ZIO.succeed(codec.encode(value))
        decoded <- ZIO.fromEither(codec.decode(encoded))
      } yield assertTrue(decoded == value)
    },
    test("Set encoding round trip") {
      val value = WithSet(Set("x", "y", "z"))
      val codec = XmlCodec.encode(WithSet.schema)

      for {
        encoded <- ZIO.succeed(codec.encode(value))
        decoded <- ZIO.fromEither(codec.decode(encoded))
      } yield assertTrue(decoded == value)
    },
    test("Either encoding round trip") {
      val left  = WithEither(Left("error"))
      val right = WithEither(Right(123))
      val codec = XmlCodec.encode(WithEither.schema)

      for {
        encodedL <- ZIO.succeed(codec.encode(left))
        decodedL <- ZIO.fromEither(codec.decode(encodedL))
        encodedR <- ZIO.succeed(codec.encode(right))
        decodedR <- ZIO.fromEither(codec.decode(encodedR))
      } yield assertTrue(decodedL == left, decodedR == right)
    },
    test("Tuple2 encoding round trip") {
      val value = WithTuple(("key", 99))
      val codec = XmlCodec.encode(WithTuple.schema)

      for {
        encoded <- ZIO.succeed(codec.encode(value))
        decoded <- ZIO.fromEither(codec.decode(encoded))
      } yield assertTrue(decoded == value)
    },
    test("config: wrapCollections changes XML structure") {
      val person  = Person("Alice", 30, List("tag1", "tag2"))
      val wrapped = XmlCodec.encode(Person.schema, XmlCodec.Config(wrapCollections = true))

      for {
        encoded   <- ZIO.succeed(wrapped.encode(person))
        xmlString = new String(encoded.toArray)
      } yield assert(xmlString)(containsString("<item>"))
    },
    test("XSD isomorphism check") {
      val xsdEither = XmlSchemaCodec.encode(Department.schema)
      assert(xsdEither.isRight)(isTrue)
    },
    test("generates correct complex types") {
      val xsd = XmlSchemaCodec.encode(Department.schema).getOrElse("")
      assert(xsd)(containsString("complexType name=\"DepartmentType\"")) &&
      assert(xsd)(containsString("complexType name=\"PersonType\""))
    },
    test("sealed traits use XSD choice pattern (not string enum)") {
      for {
        xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Status.schema))
      } yield assertTrue(
        xsd.contains("xs:choice"),
        xsd.contains("""name="Active"""") || xsd.contains("""name="Inactive""""),
        !xsd.contains("xs:enumeration")
      )
    },
    test("sealed trait encoding wraps case in parent element") {
      val task  = Task(1, Status.Active)
      val codec = XmlCodec.encode(Task.schema)

      for {
        encoded   <- ZIO.succeed(codec.encode(task))
        xmlString = new String(encoded.toArray)
      } yield assertTrue(
        xmlString.contains("<status>"),
        xmlString.contains("Active"),
        xmlString.contains("</status>")
      )
    },
    test("sealed trait round-trip with case object") {
      val task  = Task(1, Status.Active)
      val codec = XmlCodec.encode(Task.schema)

      for {
        encoded <- ZIO.succeed(codec.encode(task))
        decoded <- ZIO.fromEither(codec.decode(encoded))
      } yield assertTrue(decoded == task)
    },
    test("sealed trait round-trip with case class") {
      val task  = Task(2, Status.Pending("needs approval"))
      val codec = XmlCodec.encode(Task.schema)

      for {
        encoded <- ZIO.succeed(codec.encode(task))
        decoded <- ZIO.fromEither(codec.decode(encoded))
      } yield assertTrue(decoded == task)
    },
    test("XSD round-trip preserves sealed trait structure") {
      for {
        xsdString <- ZIO.fromEither(XmlSchemaCodec.encode(Status.schema))
        xsdBytes  = Chunk.fromArray(xsdString.getBytes)
        decoded   <- ZIO.fromEither(XmlSchemaCodec.decode(xsdBytes))
      } yield assertTrue(
        decoded.isInstanceOf[Schema.Enum[_]]
      )
    }
  )
}
