package zio.schema.codec

import zio.schema.DeriveSchema
import zio.test.Assertion.isLeft
import zio.test._

object XmlCodecSpec extends ZIOSpecDefault {
  final case class Person(name: String, age: Int)
  object Person { implicit val schema = DeriveSchema.gen[Person] }

  def spec = suite("XmlCodecSpec")(
    test("encode produces xml payload") {
      val codec = XmlCodec.xmlCodec(Person.schema)
      val out   = new String(codec.encode(Person("Jane", 30)).toArray, "UTF-8")
      assertTrue(out.contains("<value>"))
    },
    test("decode currently returns left") {
      val codec = XmlCodec.xmlCodec(Person.schema)
      assert(codec.decode(zio.Chunk.fromArray("<value/>".getBytes("UTF-8"))))(isLeft(anything))
    }
  )
}
