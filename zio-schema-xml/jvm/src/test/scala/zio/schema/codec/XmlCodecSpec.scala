package zio.schema.codec

import zio.schema.{ DeriveSchema, Schema }
import zio.test._

object XmlCodecSpec extends ZIOSpecDefault {

  final case class Person(name: String, age: Int)
  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  def spec: Spec[TestEnvironment, Any] =
    suite("XmlCodecSpec")(
      test("encodes case class") {
        val xml = XmlCodec.encode(Person.schema, Person("Alice", 42), XmlCodec.Configuration("person"))
        assertTrue(xml.contains("<name>Alice</name>"), xml.contains("<age>42</age>"))
      },
      test("roundtrip simple strings") {
        val schema = Schema[String]
        val xml    = XmlCodec.encode(schema, "hello")
        val out    = XmlCodec.decode(schema, xml)
        assertTrue(out == Right("hello"))
      }
    )
}
