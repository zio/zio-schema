package zio.schema.codec

import zio.Chunk
import zio.schema.{DeriveSchema, Schema}
import zio.test.Assertion._
import zio.test._

object JsonCodecTrailingCharsSpec extends ZIOSpecDefault {

  final case class SampleRecord(value: String)
  object SampleRecord {
    implicit val schema: Schema[SampleRecord] = DeriveSchema.gen[SampleRecord]
  }

  def spec: Spec[TestEnvironment, Any] =
    suite("JsonCodecTrailingCharsSpec")(
      test("rejects malformed JSON object with extra closing brace (#712)") {
        val codec   = JsonCodec.jsonBasedBinaryCodec[SampleRecord]
        val decoded = codec.decode(Chunk.fromArray("""{"value":"ok"}}""".getBytes("UTF-8")))
        assert(decoded)(isLeft)
      },
      test("rejects malformed JSON string literal with extra quote (#712)") {
        val codec   = JsonCodec.jsonBasedBinaryCodec[String]
        val decoded = codec.decode(Chunk.fromArray(""""foo"""".getBytes("UTF-8")))
        assert(decoded)(isLeft)
      },
      test("accepts valid JSON with trailing whitespace (#712)") {
        val codec   = JsonCodec.jsonBasedBinaryCodec[SampleRecord]
        val decoded = codec.decode(Chunk.fromArray("""{"value":"ok"}   
	 """.getBytes("UTF-8")))
        assert(decoded)(isRight(equalTo(SampleRecord("ok"))))
      }
    )
}
