package zio.schema.codec

import zio._
import zio.schema._
import zio.schema.DeriveSchema
import zio.test._

object XmlCodecSpec extends ZIOSpecDefault {

  case class Person(name: String, age: Int)
  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  case class Address(street: String, city: String, zipCode: String)
  object Address {
    implicit val schema: Schema[Address] = DeriveSchema.gen[Address]
  }

  override def spec: Spec[Any, Nothing] = suite("XmlCodecSpec")(
    suite("encoding")(
      test("encode simple case class") {
        val person = Person("John Doe", 30)
        val codec = XmlCodec.schemaBasedBinaryCodec[Person]
        val encoded = codec.encode(person)
        val xml = new String(encoded.toArray, "UTF-8")
        
        assertTrue(xml.contains("Person")) &&
        assertTrue(xml.contains("John Doe")) &&
        assertTrue(xml.contains("30"))
      },
      
      test("encode simple primitives") {
        val codec = XmlCodec.schemaBasedBinaryCodec[String]
        val encoded = codec.encode("Hello World")
        val xml = new String(encoded.toArray, "UTF-8")
        
        assertTrue(xml.contains("Hello World"))
      }
    ),
    
    suite("decoding")(
      test("decode simple case class") {
        val xml = "<Person><name>John Doe</name><age>30</age></Person>"
        val codec = XmlCodec.schemaBasedBinaryCodec[Person]
        val decoded = codec.decode(Chunk.fromArray(xml.getBytes("UTF-8")))
        
        assertTrue(decoded == Right(Person("John Doe", 30)))
      },
      
      test("decode simple primitives") {
        val xml = "<string>Hello World</string>"
        val codec = XmlCodec.schemaBasedBinaryCodec[String]
        val decoded = codec.decode(Chunk.fromArray(xml.getBytes("UTF-8")))
        
        assertTrue(decoded == Right("Hello World"))
      }
    ),
    
    suite("roundtrip")(
      test("simple case class roundtrip") {
        val person = Person("John Doe", 30)
        val codec = XmlCodec.schemaBasedBinaryCodec[Person]
        val encoded = codec.encode(person)
        val decoded = codec.decode(encoded)
        
        assertTrue(decoded == Right(person))
      },
      
      test("string roundtrip") {
        val value = "Hello World"
        val codec = XmlCodec.schemaBasedBinaryCodec[String]
        val encoded = codec.encode(value)
        val decoded = codec.decode(encoded)
        
        assertTrue(decoded == Right(value))
      }
    )
  )
}