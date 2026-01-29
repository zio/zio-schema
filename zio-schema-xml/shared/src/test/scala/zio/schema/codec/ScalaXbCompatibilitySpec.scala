package zio.schema.codec

import zio._
import zio.schema._
import zio.test._

/**
 * Validate that our XML/XSD output matches ScalaXB conventions
 *
 * Collections repeat element names, optionals map to minOccurs=0,
 * sealed traits use xs:choice, and XSD round-trips preserve structure.
 */
object ScalaXbCompatibilitySpec extends ZIOSpecDefault {

  case class Address(street: String, city: String, zip: Option[String])

  object Address {
    implicit val schema: Schema[Address] = DeriveSchema.gen[Address]
  }

  case class Person(name: String, age: Int, address: Address)

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  case class Department(id: Int, name: String, members: List[Person])

  object Department {
    implicit val schema: Schema[Department] = DeriveSchema.gen[Department]
  }

  sealed trait ContactMethod

  object ContactMethod {
    case class Email(address: String) extends ContactMethod
    case class Phone(number: String)  extends ContactMethod
    case object NoContact             extends ContactMethod

    implicit val schema: Schema[ContactMethod] = DeriveSchema.gen[ContactMethod]
  }

  override def spec: Spec[TestEnvironment, Any] =
    suite("ScalaXB Compatibility")(
      test("collections use repeated elements (ScalaXB convention)") {
        val dept = Department(
          1,
          "Engineering",
          List(
            Person("Alice", 30, Address("123 Main", "NYC", Some("10001"))),
            Person("Bob", 25, Address("456 Oak", "LA", None))
          )
        )
        val codec = XmlCodec.encode(Department.schema)
        val xml   = new String(codec.encode(dept).toArray)

        // ScalaXB repeats the collection element name for each item
        val memberCount = xml.split("<members>").length - 1
        assertTrue(
          memberCount == 2,
          xml.contains("<members><name>Alice</name>"),
          xml.contains("<members><name>Bob</name>")
        )
      },
      test("optional fields omitted when empty (ScalaXB default)") {
        val addr  = Address("123 Main", "NYC", None)
        val codec = XmlCodec.encode(Address.schema)
        val xml   = new String(codec.encode(addr).toArray)

        assertTrue(
          !xml.contains("<zip>"),
          xml.contains("<street>123 Main</street>"),
          xml.contains("<city>NYC</city>")
        )
      },
      test("optional fields included when present") {
        val addr  = Address("123 Main", "NYC", Some("10001"))
        val codec = XmlCodec.encode(Address.schema)
        val xml   = new String(codec.encode(addr).toArray)

        assertTrue(
          xml.contains("<zip>10001</zip>")
        )
      },
      test("sealed trait uses choice pattern (XSD standard)") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(ContactMethod.schema))
        } yield assertTrue(
          xsd.contains("xs:choice"),
          xsd.contains("Email") || xsd.contains("Phone"),
          !xsd.contains("xs:enumeration") // Not a string enum
        )
      },
      test("sealed trait XML wraps case in discriminator element") {
        val methods = List(
          ContactMethod.Email("alice@example.com"),
          ContactMethod.Phone("555-1234"),
          ContactMethod.NoContact
        )

        val codec = XmlCodec.encode(ContactMethod.schema)
        val xmls  = methods.map(m => new String(codec.encode(m).toArray))

        assertTrue(
          xmls(0).contains("<Email>") && xmls(0).contains("<address>alice@example.com</address>"),
          xmls(1).contains("<Phone>") && xmls(1).contains("<number>555-1234</number>"),
          xmls(2).contains("<NoContact")
        )
      },
      test("XSD isomorphism: field names preserved") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Person.schema))
        } yield assertTrue(
          xsd.contains("name=\"name\""),
          xsd.contains("name=\"age\""),
          xsd.contains("name=\"address\"")
        )
      },
      test("XSD isomorphism: types mapped correctly") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Person.schema))
        } yield assertTrue(
          xsd.contains("type=\"xs:string\""),
          xsd.contains("type=\"xs:int\""),
          xsd.contains("type=\"AddressType\"")
        )
      },
      test("XSD isomorphism: optional → minOccurs=0") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Address.schema))
        } yield {
          // zip is optional
          val zipLine = xsd.linesIterator.find(_.contains("name=\"zip\"")).getOrElse("")
          assertTrue(
            zipLine.contains("minOccurs=\"0\"")
          )
        }
      },
      test("XSD isomorphism: collection → maxOccurs=unbounded") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Department.schema))
        } yield {
          val membersLine = xsd.linesIterator.find(_.contains("name=\"members\"")).getOrElse("")
          assertTrue(
            membersLine.contains("maxOccurs=\"unbounded\"")
          )
        }
      },
      test("XSD round-trip preserves structure") {
        for {
          originalXsd <- ZIO.fromEither(XmlSchemaCodec.encode(Department.schema))
          decoded     <- ZIO.fromEither(XmlSchemaCodec.decode(Chunk.fromArray(originalXsd.getBytes)))
        } yield {
          assertTrue(decoded.isInstanceOf[Schema.Record[_]])
        }
      },
      test("XML round-trip with complex nested structure") {
        val dept = Department(
          1,
          "Engineering",
          List(
            Person("Alice", 30, Address("123 Main", "NYC", Some("10001"))),
            Person("Bob", 25, Address("456 Oak", "LA", None))
          )
        )
        val codec = XmlCodec.encode(Department.schema)

        for {
          encoded <- ZIO.succeed(codec.encode(dept))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == dept)
      },
      test("XSD can serve as external contract") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Person.schema))
        } yield assertTrue(
          xsd.startsWith("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"),
          xsd.contains("xmlns:xs=\"http://www.w3.org/2001/XMLSchema\""),
          xsd.contains("<xs:schema"),
          xsd.contains("</xs:schema>")
        )
      },
      test("collection encoding matches XSD definition") {
        val dept  = Department(1, "Eng", List(Person("A", 1, Address("S", "C", None))))
        val codec = XmlCodec.encode(Department.schema)
        val xml   = new String(codec.encode(dept).toArray)

        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(Department.schema))
        } yield {
          assertTrue(
            xsd.contains("name=\"members\""),
            xsd.contains("maxOccurs=\"unbounded\""),
            xml.contains("<members>")
          )
        }
      }
    )
}
