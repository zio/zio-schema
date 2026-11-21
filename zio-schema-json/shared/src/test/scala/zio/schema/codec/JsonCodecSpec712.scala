package zio.schema.codec

import zio.Chunk
import zio.schema._
import zio.test.Assertion._
import zio.test._

/**
 * Regression test for issue #712: the schema-based JSON decoder accepted
 * malformed input with extra characters after a complete value, such as `{}}`
 * or `"foo""`.
 */
object JsonCodecSpec712 extends ZIOSpecDefault {

  final case class Empty()

  object Empty {
    implicit val schema: Schema[Empty] = DeriveSchema.gen[Empty]
  }

  final case class Person(name: String, age: Int)

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  private def decode[A](schema: Schema[A], json: String): Either[DecodeError, A] =
    JsonCodec
      .schemaBasedBinaryCodec[A](JsonCodec.Configuration.default)(schema)
      .decode(Chunk.fromArray(json.getBytes("UTF-8")))

  def spec: Spec[TestEnvironment, Any] =
    suite("JsonCodecSpec712")(
      test("rejects an extra closing brace after an object") {
        assert(decode(Empty.schema, "{}}"))(isLeft)
      },
      test("rejects an extra quote after a string") {
        assert(decode(Schema[String], "\"foo\"\""))(isLeft)
      },
      test("rejects a second value after a record") {
        assert(decode(Person.schema, """{"name":"a","age":1} {"name":"b","age":2}"""))(isLeft)
      },
      test("rejects trailing garbage after a number") {
        assert(decode(Schema[Int], "42x"))(isLeft)
      },
      test("still accepts surrounding whitespace") {
        assert(decode(Person.schema, "  \n{\"name\":\"a\",\"age\":1}\n\t "))(isRight(equalTo(Person("a", 1))))
      },
      test("still accepts a plain string") {
        assert(decode(Schema[String], "\"foo\""))(isRight(equalTo("foo")))
      },
      test("JsonDecoder.decode rejects trailing characters") {
        assert(JsonCodec.JsonDecoder.decode(Empty.schema, "{}}", JsonCodec.Configuration.default))(isLeft)
      }
    )
}
