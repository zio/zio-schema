package zio.schema.codec

import java.nio.charset.StandardCharsets

import zio.Chunk
import zio.schema.{DeriveSchema, Schema}
import zio.test.Assertion._
import zio.test._

object StrictJsonDecoderSpec extends ZIOSpecDefault {

  final case class SamplePayload(label: String, count: Int)

  object SamplePayload {
    implicit val schema: Schema[SamplePayload] = DeriveSchema.gen[SamplePayload]
  }

  private def decodeString[A](schema: Schema[A], raw: String): Either[String, A] =
    JsonCodec.jsonDecoder(schema).decodeJson(raw)

  private def decodeBinary[A](schema: Schema[A], raw: String): Either[DecodeError, A] =
    JsonCodec.schemaBasedBinaryCodec(schema).decode(Chunk.fromArray(raw.getBytes(StandardCharsets.UTF_8)))

  def spec: Spec[TestEnvironment, Any] =
    suite("StrictJsonDecoderSpec")(
      test("rejects malformed JSON object with extra closing brace") {
        val input = """{"label":"item","count":1}}"""
        assert(decodeString(SamplePayload.schema, input))(isLeft(containsString("expected end of input"))) &&
        assert(decodeBinary(SamplePayload.schema, input))(isLeft)
      },
      test("rejects malformed JSON string with trailing quote or literal") {
        assert(decodeString(Schema[String], """"foo"""""))(isLeft(containsString("expected end of input"))) &&
        assert(decodeString(Schema[Int], "42trailing"))(isLeft(containsString("expected end of input"))) &&
        assert(decodeString(Schema[Boolean], "truefalse"))(isLeft(containsString("expected end of input")))
      },
      test("accepts valid JSON values followed by trailing whitespace") {
        val padded = "  {\"label\": \"ok\", \"count\": 7} \n\t  "
        assert(decodeString(SamplePayload.schema, padded))(isRight(equalTo(SamplePayload("ok", 7)))) &&
        assert(decodeBinary(SamplePayload.schema, padded))(isRight(equalTo(SamplePayload("ok", 7))))
      }
    )
}
