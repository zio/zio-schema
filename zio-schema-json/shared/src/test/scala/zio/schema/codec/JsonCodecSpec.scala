package zio.schema.codec

import zio.schema._
import zio.test._
import zio.test.Assertion._
import zio.json._ // Ensure zio-json is imported for JsonDecoder/Encoder

object JsonCodecSpec extends ZIOSpecDefault {

  case class Person(name: String, age: Int)
  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  override def spec: Spec[TestEnvironment, Any] =
    suite("JsonCodec")(
      suite("decode should strictly parse the entire input")(
        test("should reject string with trailing characters") {
          val codec = JsonCodec(Schema[String])
          assert(codec.decode("\"foo\"\""))(isLeft(startsWithString("Unexpected trailing characters at position 5: '\"\"'")))
        },
        test("should reject object with trailing characters") {
          val codec = JsonCodec(Person.schema)
          assert(codec.decode("{}}"))(isLeft(startsWithString("Unexpected trailing characters at position 1: '}}'")))
        },
        test("should reject object with extra characters after valid JSON") {
          val codec = JsonCodec(Person.schema)
          val json = "{\"name\":\"test\",\"age\":10}extra"
          assert(codec.decode(json))(isLeft(startsWithString("Unexpected trailing characters at position 25: 'extra'")))
        },
        test("should reject int with trailing characters") {
          val codec = JsonCodec(Schema[Int])
          assert(codec.decode("123abc"))(isLeft(startsWithString("Unexpected trailing characters at position 3: 'abc'")))
        },
        test("should reject boolean with trailing characters") {
          val codec = JsonCodec(Schema[Boolean])
          assert(codec.decode("truefalse"))(isLeft(startsWithString("Unexpected trailing characters at position 4: 'false'")))
        },
        test("should reject null with trailing characters") {
          val codec = JsonCodec(Schema[Option[String]])
          assert(codec.decode("nullnull"))(isLeft(startsWithString("Unexpected trailing characters at position 4: 'null'")))
        },
        test("should accept valid string without trailing characters") {
          val codec = JsonCodec(Schema[String])
          assert(codec.decode("\"foo\""))(isRight(equalTo("foo")))
        },
        test("should accept valid object without trailing characters") {
          val codec = JsonCodec(Person.schema)
          assert(codec.decode("{\"name\":\"test\",\"age\":10}"))(isRight(equalTo(Person("test", 10))))
        },
        test("should accept valid object with leading/trailing whitespace") {
          val codec = JsonCodec(Person.schema)
          assert(codec.decode("  {\"name\":\"test\",\"age\":10}  "))(isRight(equalTo(Person("test", 10))))
        },
        test("should accept valid int without trailing characters") {
          val codec = JsonCodec(Schema[Int])
          assert(codec.decode("123"))(isRight(equalTo(123)))
        },
        test("should accept valid boolean without trailing characters") {
          val codec = JsonCodec(Schema[Boolean])
          assert(codec.decode("true"))(isRight(equalTo(true)))
        },
        test("should accept valid null without trailing characters") {
          val codec = JsonCodec(Schema[Option[String]])
          assert(codec.decode("null"))(isRight(equalTo(None)))
        }
      )
    )
}
