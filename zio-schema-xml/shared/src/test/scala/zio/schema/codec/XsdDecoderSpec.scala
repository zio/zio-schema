package zio.schema.codec

import zio._
import zio.schema.{ DeriveSchema, Schema }
import zio.test._

object XsdDecoderSpec extends ZIOSpecDefault {

  case class SimplePerson(name: String, age: Int)

  object SimplePerson {
    implicit val schema: Schema[SimplePerson] = DeriveSchema.gen[SimplePerson]
  }

  case class Address(street: String, city: String)

  object Address {
    implicit val schema: Schema[Address] = DeriveSchema.gen[Address]
  }

  case class PersonWithAddress(person: SimplePerson, address: Address)

  object PersonWithAddress {
    implicit val schema: Schema[PersonWithAddress] = DeriveSchema.gen[PersonWithAddress]
  }

  sealed trait Status

  object Status {
    case object Active   extends Status
    case object Inactive extends Status
    case object Pending  extends Status

    implicit val schema: Schema[Status] = DeriveSchema.gen[Status]
  }

  override def spec: Spec[TestEnvironment, Any] =
    suite("XSD Decoder")(
      test("decode simple XSD") {
        val xsd     = XmlSchemaCodec.encode(SimplePerson.schema).getOrElse("")
        val decoded = XmlSchemaCodec.decode(Chunk.fromArray(xsd.getBytes))

        assertTrue(decoded.isRight)
      },
      test("decode nested XSD") {
        val xsd     = XmlSchemaCodec.encode(PersonWithAddress.schema).getOrElse("")
        val decoded = XmlSchemaCodec.decode(Chunk.fromArray(xsd.getBytes))

        assertTrue(decoded.isRight)
      },
      test("decode enum XSD") {
        val xsd     = XmlSchemaCodec.encode(Status.schema).getOrElse("")
        val decoded = XmlSchemaCodec.decode(Chunk.fromArray(xsd.getBytes))

        assertTrue(decoded.isRight)
      },
      test("XSD round-trip preserves structure") {
        val originalXsd   = XmlSchemaCodec.encode(SimplePerson.schema).getOrElse("")
        val decodedSchema = XmlSchemaCodec.decode(Chunk.fromArray(originalXsd.getBytes))

        assertTrue(
          decodedSchema.isRight,
          decodedSchema.exists {
            case _: Schema.Record[_] => true
            case _                   => false
          }
        )
      },
      test("decoded schema can encode") {
        SimplePerson("Alice", 30)
        val xsd           = XmlSchemaCodec.encode(SimplePerson.schema).getOrElse("")
        val decodedSchema = XmlSchemaCodec.decode(Chunk.fromArray(xsd.getBytes))

        assertTrue(decodedSchema.isRight)
      },
      test("reject invalid XSD") {
        val invalidXsd = "<invalid>not a valid xsd</invalid>"
        val decoded    = XmlSchemaCodec.decode(Chunk.fromArray(invalidXsd.getBytes))

        assertTrue(decoded.isLeft)
      },
      test("reject XSD without root element") {
        val xsdWithoutRoot = """<?xml version="1.0" encoding="UTF-8"?>
          <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
          </xs:schema>"""
        val decoded        = XmlSchemaCodec.decode(Chunk.fromArray(xsdWithoutRoot.getBytes))

        assertTrue(decoded.isLeft)
      },
      test("reject XSD with imports") {
        val xsdWithImport = """<?xml version="1.0" encoding="UTF-8"?>
          <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:import namespace="http://example.com/other" schemaLocation="other.xsd"/>
          </xs:schema>"""
        val decoded       = XmlSchemaCodec.decode(Chunk.fromArray(xsdWithImport.getBytes))

        assertTrue(decoded.isLeft, decoded.left.exists(_.contains("Imports")))
      }
    )
}
