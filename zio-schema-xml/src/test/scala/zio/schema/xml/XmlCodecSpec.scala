package zio.schema.xml

import zio.test._
import zio.test.Assertion._
import zio.schema._
import zio.ZIO
import scala.xml._

object XmlCodecSpec extends DefaultRunnableSpec {
  case class Person(name: String, age: Int)

  val personSchema: Schema[Person] = Schema.CaseClass2(
    field1 = Schema.Field("name", Schema.Primitive(StandardType.StringType)),
    field2 = Schema.Field("age", Schema.Primitive(StandardType.IntType)),
    construct = (name, age) => Person(name, age),
    extractField1 = _.name,
    extractField2 = _.age
  )

  def spec = suite("XmlCodecSpec")(
    testM("encode and decode string") {
      val schema = Schema.primitive[String]
      val value = "hello"
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    testM("encode and decode int") {
      val schema = Schema.primitive[Int]
      val value = 42
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    testM("encode and decode boolean") {
      val schema = Schema.primitive[Boolean]
      val value = true
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    testM("encode and decode double") {
      val schema = Schema.primitive[Double]
      val value = 3.14
      val xml = XmlCodec.encode(schema, value)
      assertM(XmlCodec.decode(schema, xml))(equalTo(value))
    },
    testM("encode and decode case class") {
      val value = Person("John", 30)
      val xml = XmlCodec.encode(personSchema, value)
      assertM(XmlCodec.decode(personSchema, xml))(equalTo(value))
    }
  )
}