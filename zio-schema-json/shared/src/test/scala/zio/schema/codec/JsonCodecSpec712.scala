package zio.schema.codec

import zio._
import zio.schema._
import zio.stream.ZStream
import zio.test._

/**
 * Regression test for issue #712: the decoder accepted input with extra
 * characters after a complete JSON value, e.g. `"foo""` or `{}}`.
 */
object JsonCodecSpec712 extends ZIOSpecDefault {

  final case class Person(name: String)

  object Person {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }

  private def decode[A](json: String)(implicit schema: Schema[A]): Either[DecodeError, A] =
    JsonCodec.schemaBasedBinaryCodec[A].decode(Chunk.fromArray(json.getBytes("UTF-8")))

  def spec: Spec[Any, DecodeError] = suite("JsonCodecSpec712")(
    test("rejects trailing characters after a value") {
      assertTrue(
        decode[String]("\"foo\"\"").isLeft,
        decode[String]("\"foo\" x").isLeft,
        decode[Int]("1 2").isLeft,
        decode[Person]("{\"name\":\"a\"}}").isLeft
      )
    },
    test("allows surrounding whitespace") {
      assertTrue(
        decode[String](" \"foo\" \n") == Right("foo"),
        decode[Person]("{\"name\":\"a\"}\r\n") == Right(Person("a"))
      )
    },
    test("rejects trailing characters with the zio-json based codec") {
      val codec = JsonCodec.zioJsonBinaryCodec[String](zio.json.JsonCodec.string)
      assertTrue(
        codec.decode(Chunk.fromArray("\"foo\"\"".getBytes("UTF-8"))).isLeft,
        codec.decode(Chunk.fromArray("\"foo\"".getBytes("UTF-8"))) == Right("foo")
      )
    },
    test("fallback arrays are read to the end") {
      val lenient = Schema.Fallback(Schema[Int], Schema[String])
      val full    = Schema.Fallback(Schema[Int], Schema[String], fullDecode = true)
      assertTrue(
        decode("[30,\"hello\"]")(lenient) == Right(Fallback.Left(30)),
        decode("[30,30]")(full) == Right(Fallback.Left(30)),
        decode("[\"x\",\"hello\"]")(full) == Right(Fallback.Right("hello")),
        decode("[30,\"hello\"] 1")(lenient).isLeft
      )
    },
    test("stream decoding still splits values") {
      ZStream
        .fromIterable("{\"name\":\"a\"} {\"name\":\"b\"}".getBytes("UTF-8"))
        .via(JsonCodec.schemaBasedBinaryCodec[Person].streamDecoder)
        .runCollect
        .map(out => assertTrue(out == Chunk(Person("a"), Person("b"))))
    }
  )
}
