package zio.schema.codec

import java.nio.charset.StandardCharsets

import zio.json.JsonStreamDelimiter
import zio.schema.Schema
import zio.schema.codec.JsonCodec.JsonEncoder.charSequenceToByteChunk
import zio.schema.codec.JsonCodecSpec.AllOptionalFields
import zio.stream.{ ZPipeline, ZStream }
import zio.test.Assertion.{ equalTo, isRight }
import zio.test.TestAspect.timeout
import zio.test.{ Gen, Spec, TestAspect, TestEnvironment, ZIOSpecDefault, assertZIO, check }
import zio.{ Chunk, durationInt }

object JsonCodecJVMSpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment, Any] =
    suite("JsonCodec JVM Spec")(
      decoderSuite
    ) @@ TestAspect.jvmOnly @@ timeout(180.seconds)

  private val decoderSuite = suite("decoding")(
    suite("case class")(
      test("case class with empty option field is decoded by stream") {
        val names     = Gen.option(Gen.elements("John", "Jane", "Jermaine", "Jasmine"))
        val age       = Gen.option(Gen.int(1, 99))
        val person    = names.zipWith(age)(AllOptionalFields(_, _, None))
        val delimiter = Gen.elements(JsonStreamDelimiter.Array, JsonStreamDelimiter.Newline)
        val codec     = JsonCodec.jsonEncoder(AllOptionalFields.schema)

        check(Gen.chunkOfBounded(0, 3)(person), delimiter) {
          (people, delim) =>
            val indent        = if (delim == JsonStreamDelimiter.Array) Some(1) else None
            val encodedPeople = people.map(p => codec.encodeJson(p, indent))
            val encoded = delim match {
              case JsonStreamDelimiter.Array =>
                encodedPeople.mkString("[", ",", "]")
              case JsonStreamDelimiter.Newline =>
                encodedPeople.mkString("", "\n", "\n")
            }

            assertDecodesJsonStream(
              AllOptionalFields.schema,
              people,
              charSequenceToByteChunk(encoded),
              delim
            )
        }
      }
    )
  )

  private def assertDecodesJsonStream[A](
    schema: Schema[A],
    value: Chunk[A],
    chunk: Chunk[Byte],
    delimiter: JsonStreamDelimiter
  ) = {
    val result = ZStream
      .fromChunk(chunk)
      .via(ZPipeline.decodeCharsWith(StandardCharsets.UTF_8))
      .via(JsonCodec.jsonDecoder(schema).decodeJsonPipeline(delimiter))
      .runCollect
      .either
    assertZIO(result)(isRight(equalTo(value)))
  }
}
