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
    suite("decode record with more than 22 fields")(
      test("missing fields in the json payload are populated with their default values") {
        val exampleSchema = zio.schema.DeriveSchema.gen[RecordExample]
        val string        = """{"f1": "test"}"""
        assertDecodesJson(exampleSchema, RecordExample(Some("test")), string)
      },
      test("fail if a field with no default value is missing in the json payload") {
        val exampleSchema = zio.schema.DeriveSchema.gen[RecordExample2]
        val string        = """{"f1": "test"}"""
        assertDecodesJsonFailure(exampleSchema, string)
      }
    ),
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

  private def assertDecodesJson[A](schema: Schema[A], value: A, jsonString: String) = {
    val either = JsonCodec.jsonDecoder(schema).decodeJson(jsonString)
    zio.test.assert(either)(isRight(equalTo(value)))
  }

  private def assertDecodesJsonFailure[A](schema: Schema[A], jsonString: String) = {
    val either = JsonCodec.jsonDecoder(schema).decodeJson(jsonString)
    zio.test.assertTrue(either.isLeft)
  }

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

  case class RecordExample(
    f1: Option[String],
    f2: Option[String] = None,
    f3: Option[String] = None,
    f4: Option[String] = None,
    f5: Option[String] = None,
    f6: Option[String] = None,
    f7: Option[String] = None,
    f8: Option[String] = None,
    f9: Option[String] = None,
    f10: Option[String] = None,
    f11: Option[String] = None,
    f12: Option[String] = None,
    f13: Option[String] = None,
    f14: Option[String] = None,
    f15: Option[String] = None,
    f16: Option[String] = None,
    f17: Option[String] = None,
    f18: Option[String] = None,
    f19: Option[String] = None,
    f20: Option[String] = None,
    f21: Option[String] = None,
    f22: Option[String] = None,
    f23: Option[String] = None
  )

  case class RecordExample2(
    f1: Option[String],
    f2: String,
    f3: Option[String] = None,
    f4: Option[String] = None,
    f5: Option[String] = None,
    f6: Option[String] = None,
    f7: Option[String] = None,
    f8: Option[String] = None,
    f9: Option[String] = None,
    f10: Option[String] = None,
    f11: Option[String] = None,
    f12: Option[String] = None,
    f13: Option[String] = None,
    f14: Option[String] = None,
    f15: Option[String] = None,
    f16: Option[String] = None,
    f17: Option[String] = None,
    f18: Option[String] = None,
    f19: Option[String] = None,
    f20: Option[String] = None,
    f21: Option[String] = None,
    f22: Option[String] = None,
    f23: Option[String] = None
  )

}
