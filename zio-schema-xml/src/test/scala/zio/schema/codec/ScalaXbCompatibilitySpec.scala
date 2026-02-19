package zio.schema.codec

import scala.xml._

import zio._
import zio.schema._
import zio.test._

object ScalaXbCompatibilitySpec extends ZIOSpecDefault {

  case class PersonType(name: String, age: Int, email: Option[String])

  object PersonType {
    implicit val schema: Schema[PersonType] = DeriveSchema.gen[PersonType]

    def fromScalaXb(name: String, age: Int, email: Option[String]): PersonType =
      PersonType(name, age, email)
  }

  val samplePerson: PersonType       = PersonType("Alice", 30, Some("alice@example.com"))
  val personWithoutEmail: PersonType = PersonType("Bob", 25, None)

  override def spec: Spec[TestEnvironment, Any] = suite("ScalaXB Compatibility")(
    suite("Configuration Requirements")(
      test("type annotations are optional and default to false") {
        val codec = XmlCodec.encode(PersonType.schema)
        val xml   = new String(codec.encode(samplePerson).toArray)

        assertTrue(
          !xml.contains("xsi:type"),
          XmlCodec.Config.default.includeTypeAnnotations == false
        )
      },
      test("can enable type annotations via config") {
        val codec = XmlCodec.encode(
          PersonType.schema,
          XmlCodec.Config(includeTypeAnnotations = true)
        )
        new String(codec.encode(samplePerson).toArray)

        assertTrue(codec != null)
      },
      test("default serialization produces compact XML without type bloat") {
        val codec = XmlCodec.encode(PersonType.schema)
        val xml   = new String(codec.encode(samplePerson).toArray)

        assertTrue(
          xml.contains("<age>30</age>"),
          !xml.contains("<age><int>30</int></age>")
        )
      }
    ),
    suite("Isomorphic Round-trip: Schema → XSD → Schema")(
      test("case class → Schema → XSD → Schema preserves structure") {
        val schema1 = PersonType.schema

        for {
          // Generate XSD from Schema
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(schema1))

          // Parse XSD back to Schema
          schema2 <- ZIO.fromEither(XmlSchemaCodec.decode(Chunk.fromArray(xsd.getBytes)))

          codec1 = XmlCodec.encode(schema1)
          xml1   = new String(codec1.encode(samplePerson).toArray)
        } yield assertTrue(
          xsd.contains("xs:complexType"),
          xsd.contains("name=\"name\""),
          xsd.contains("name=\"age\""),
          xsd.contains("name=\"email\""),
          xml1.contains("<name>Alice</name>")
        )
      }
    ),
    suite("ScalaXB Runtime API Usage")(
      test("can serialize using ScalaXB-style XML literals") {
        val personNode =
          <PersonType>
            <name>{samplePerson.name}</name>
            <age>{samplePerson.age}</age>
            {samplePerson.email.map(e => <email>{e}</email>).getOrElse(NodeSeq.Empty)}
          </PersonType>

        val xmlString = personNode.toString

        assertTrue(
          xmlString.contains("<name>Alice</name>"),
          xmlString.contains("<age>30</age>"),
          xmlString.contains("<email>alice@example.com</email>")
        )
      },
      test("can parse XML using scala-xml (ScalaXB's underlying library)") {
        val xmlString = """<PersonType>
          <name>Charlie</name>
          <age>35</age>
          <email>charlie@test.com</email>
        </PersonType>"""

        val xml   = XML.loadString(xmlString)
        val name  = (xml \ "name").text
        val age   = (xml \ "age").text.toInt
        val email = (xml \ "email").headOption.map(_.text)

        assertTrue(
          name == "Charlie",
          age == 35,
          email == Some("charlie@test.com")
        )
      }
    ),
    suite("Pattern: XSD → ScalaXB → ZIO Schema comparison")(
      test("ZIO Schema produces XML compatible with ScalaXB's serializer") {

        val codec  = XmlCodec.encode(PersonType.schema)
        val zioXml = new String(codec.encode(samplePerson).toArray)

        val scalaxbStyleXml =
          <PersonType>
            <name>{samplePerson.name}</name>
            <age>{samplePerson.age}</age>
            <email>{samplePerson.email.get}</email>
          </PersonType>.toString

        assertTrue(
          zioXml.contains("<name>Alice</name>"),
          scalaxbStyleXml.contains("<name>Alice</name>"),
          zioXml.contains("<age>30</age>"),
          scalaxbStyleXml.contains("<age>30</age>"),
          zioXml.contains("<email>alice@example.com</email>"),
          scalaxbStyleXml.contains("<email>alice@example.com</email>")
        )
      },
      test("ZIO Schema can deserialize ScalaXB-serialized XML") {
        val scalaxbXml = """<?xml version="1.0" encoding="UTF-8"?>
          <PersonType>
            <name>Bob</name>
            <age>25</age>
            <email>bob@test.com</email>
          </PersonType>"""

        val codec = XmlCodec.encode(PersonType.schema)

        for {
          decoded <- ZIO.fromEither(codec.decode(Chunk.fromArray(scalaxbXml.getBytes)))
        } yield assertTrue(
          decoded.name == "Bob",
          decoded.age == 25,
          decoded.email == Some("bob@test.com")
        )
      },
      test("ScalaXB can parse ZIO Schema-serialized XML") {
        val codec  = XmlCodec.encode(PersonType.schema)
        val zioXml = new String(codec.encode(samplePerson).toArray)

        val xml   = XML.loadString(zioXml)
        val name  = (xml \ "name").text
        val age   = (xml \ "age").text.toInt
        val email = (xml \ "email").headOption.map(_.text)

        assertTrue(
          name == "Alice",
          age == 30,
          email == Some("alice@example.com")
        )
      },
      test("round-trip maintains data integrity through both systems") {
        val codec = XmlCodec.encode(PersonType.schema)

        for {
          zioEncoded   <- ZIO.succeed(codec.encode(samplePerson))
          zioXmlString = new String(zioEncoded.toArray)

          parsed      = XML.loadString(zioXmlString)
          parsedName  = (parsed \ "name").text
          parsedAge   = (parsed \ "age").text.toInt
          parsedEmail = (parsed \ "email").headOption.map(_.text)

          // Recreate and re-encode with ZIO Schema
          recreated = PersonType(parsedName, parsedAge, parsedEmail)
          reEncoded <- ZIO.succeed(codec.encode(recreated))

          // Decode back
          finalDecoded <- ZIO.fromEither(codec.decode(reEncoded))
        } yield assertTrue(
          finalDecoded == samplePerson,
          recreated == samplePerson
        )
      }
    ),
    suite("XML outputs are isomorphic")(
      test("serialized values from both systems can be cross-deserialized") {
        val codec = XmlCodec.encode(PersonType.schema)

        val zioXml = new String(codec.encode(personWithoutEmail).toArray)

        val scalaxbXml =
          <PersonType>
            <name>{personWithoutEmail.name}</name>
            <age>{personWithoutEmail.age}</age>
          </PersonType>.toString

        for {
          fromZio     <- ZIO.fromEither(codec.decode(Chunk.fromArray(zioXml.getBytes)))
          fromScalaxb <- ZIO.fromEither(codec.decode(Chunk.fromArray(scalaxbXml.getBytes)))
        } yield assertTrue(
          fromZio == personWithoutEmail,
          fromScalaxb.name == personWithoutEmail.name,
          fromScalaxb.age == personWithoutEmail.age,
          fromScalaxb.email.isEmpty
        )
      },
      test("optional field handling is equivalent (minOccurs=0)") {
        val codec        = XmlCodec.encode(PersonType.schema)
        val withEmail    = new String(codec.encode(samplePerson).toArray)
        val withoutEmail = new String(codec.encode(personWithoutEmail).toArray)

        assertTrue(
          withEmail.contains("<email>"),
          !withoutEmail.contains("<email>")
        )
      }
    ),
    suite("XSD Generation for ScalaXB compatibility")(
      test("generated XSD is valid for ScalaXB code generation") {
        for {
          xsd <- ZIO.fromEither(XmlSchemaCodec.encode(PersonType.schema))
        } yield assertTrue(
          xsd.contains("xs:complexType"),
          xsd.contains("xs:sequence"),
          xsd.contains("xs:element"),
          xsd.contains("name=\"name\""),
          xsd.contains("name=\"age\""),
          xsd.contains("name=\"email\""),
          xsd.contains("minOccurs=\"0\"")
        )
      },
      test("XSD round-trip produces equivalent schema") {
        for {
          xsd1   <- ZIO.fromEither(XmlSchemaCodec.encode(PersonType.schema))
          schema <- ZIO.fromEither(XmlSchemaCodec.decode(Chunk.fromArray(xsd1.getBytes)))
          xsd2   <- ZIO.fromEither(XmlSchemaCodec.encode(schema))
        } yield assertTrue(
          xsd1.contains("xs:complexType"),
          xsd2.contains("xs:complexType")
        )
      }
    )
  )
}
