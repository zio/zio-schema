package zio.schema.codec

import zio._
import zio.json.{JsonCodec => ZJsonCodec}
import zio.schema._
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test._

/**
 * Regression tests for https://github.com/zio/zio-schema/issues/712
 *
 * Single-value decoding must reject non-whitespace input that follows a
 * complete JSON value, while stream decoding (several values separated by
 * whitespace / newlines) must keep working.
 */
object JsonTrailingInputSpec extends ZIOSpecDefault {

  final case class Rec(a: Int)
  object Rec {
    implicit val schema: Schema[Rec] = DeriveSchema.gen[Rec]
  }

  private def bytes(s: String): Chunk[Byte] = Chunk.fromArray(s.getBytes("UTF-8"))

  private val malformed: List[(String, String)] = List(
    "record + extra '}'"       -> """{"a":1}}""",
    "record + extra garbage"   -> """{"a":1} x""",
    "record + second value"    -> """{"a":1}{"a":2}""",
    "empty record + extra '}'" -> """{}}"""
  )

  def spec: Spec[TestEnvironment, Any] =
    suite("JsonCodec trailing input (#712)")(
      suite("jsonDecoder(schema).decodeJson")(
        test("rejects trailing characters after a record") {
          val dec = JsonCodec.jsonDecoder(Rec.schema)
          assertTrue(malformed.forall { case (_, json) => dec.decodeJson(json).isLeft })
        },
        test("rejects trailing quote after a string") {
          assert(JsonCodec.jsonDecoder(Schema[String]).decodeJson("\"foo\"\""))(isLeft)
        },
        test("rejects trailing characters after a number") {
          assert(JsonCodec.jsonDecoder(Schema[Int]).decodeJson("123abc"))(isLeft) &&
          assert(JsonCodec.jsonDecoder(Schema[Int]).decodeJson("1 2"))(isLeft)
        },
        test("accepts surrounding whitespace") {
          assert(JsonCodec.jsonDecoder(Rec.schema).decodeJson(" \n{\"a\":1} \n\t "))(isRight(equalTo(Rec(1)))) &&
          assert(JsonCodec.jsonDecoder(Schema[String]).decodeJson("\"foo\"  "))(isRight(equalTo("foo")))
        },
        test("error message mentions trailing input") {
          val res = JsonCodec.jsonDecoder(Rec.schema).decodeJson("""{"a":1}}""")
          assert(res)(isLeft(containsString("trailing")))
        }
      ),
      suite("BinaryCodec.decode")(
        test("schemaBasedBinaryCodec rejects trailing characters") {
          val codec = JsonCodec.schemaBasedBinaryCodec(Rec.schema)
          assertTrue(malformed.forall { case (_, json) => codec.decode(bytes(json)).isLeft }) &&
          assert(codec.decode(bytes("""{"a":1}  """)))(isRight(equalTo(Rec(1))))
        },
        test("zioJsonBinaryCodec rejects trailing characters") {
          implicit val zc: ZJsonCodec[Rec] = JsonCodec.jsonCodec(Rec.schema)
          val codec                        = JsonCodec.zioJsonBinaryCodec[Rec]
          assertTrue(malformed.forall { case (_, json) => codec.decode(bytes(json)).isLeft }) &&
          assert(codec.decode(bytes("""{"a":1}""")))(isRight(equalTo(Rec(1))))
        }
      ),
      suite("streaming stays lenient")(
        test("newline separated values still decode") {
          val codec = JsonCodec.schemaBasedBinaryCodec(Rec.schema)
          ZStream
            .fromChunk(bytes("{\"a\":1}\n{\"a\":2}\n"))
            .via(codec.streamDecoder)
            .runCollect
            .map(out => assert(out)(equalTo(Chunk(Rec(1), Rec(2)))))
        }
      )
    )
}
