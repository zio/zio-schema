package zio.schema.codec

import zio.schema._
import zio.schema.codec.JsonCodec.JsonEncoder.charSequenceToByteChunk
import zio.test._

/**
 * Regression tests for issue #712:
 * JsonCodec decoder must reject malformed JSON with trailing characters.
 *
 * Previously, the decoder would silently accept inputs like:
 *   - `{}}` — extra `}` after a valid object
 *   - `"foo""` — extra `"` after a valid string
 *   - `null null` — second token after a valid null
 *
 * The fix validates that no non-whitespace characters remain after the root
 * JSON value is parsed.
 */
object JsonCodecSpec712 extends ZIOSpecDefault {

  private def decodeString(json: String): Either[DecodeError, String] =
    JsonCodec.schemaBasedBinaryCodec[String](JsonCodec.Configuration.default)(Schema[String])
      .decode(charSequenceToByteChunk(json))

  private def decodeInt(json: String): Either[DecodeError, Int] =
    JsonCodec.schemaBasedBinaryCodec[Int](JsonCodec.Configuration.default)(Schema[Int])
      .decode(charSequenceToByteChunk(json))

  case class Foo(x: Int)
  implicit val fooSchema: Schema[Foo] = DeriveSchema.gen[Foo]

  private def decodeFoo(json: String): Either[DecodeError, Foo] =
    JsonCodec.schemaBasedBinaryCodec[Foo](JsonCodec.Configuration.default)(fooSchema)
      .decode(charSequenceToByteChunk(json))

  override def spec: Spec[Any, Nothing] = suite("JsonCodec Issue #712 - Trailing character rejection")(
    suite("must reject malformed JSON with trailing non-whitespace")(
      test("object with extra closing brace: {}}") {
        val result = decodeFoo("""{"x":1}}""")
        assertTrue(result.isLeft)
      },
      test("string with extra quote: \"foo\"\"") {
        val result = decodeString(""""foo"""" + "\"")
        assertTrue(result.isLeft)
      },
      test("null followed by another token: null null") {
        val result = JsonCodec.schemaBasedBinaryCodec[Option[String]](JsonCodec.Configuration.default)(Schema[Option[String]])
          .decode(charSequenceToByteChunk("null null"))
        assertTrue(result.isLeft)
      },
      test("integer with trailing garbage: 42abc") {
        val result = decodeInt("42abc")
        assertTrue(result.isLeft)
      },
      test("valid object followed by another object: {}{\"x\":2}") {
        val result = decodeFoo("""{"x":1}{"x":2}""")
        assertTrue(result.isLeft)
      },
      test("boolean with trailing content: true false") {
        val result = JsonCodec.schemaBasedBinaryCodec[Boolean](JsonCodec.Configuration.default)(Schema[Boolean])
          .decode(charSequenceToByteChunk("true false"))
        assertTrue(result.isLeft)
      }
    ),
    suite("must still accept valid JSON (including trailing whitespace)")(
      test("plain string") {
        val result = decodeString(""""hello"""")
        assertTrue(result == Right("hello"))
      },
      test("string with trailing newline") {
        val result = decodeString(""""hello"""" + "\n")
        assertTrue(result == Right("hello"))
      },
      test("string with trailing spaces") {
        val result = decodeString(""""hello"   """)
        assertTrue(result == Right("hello"))
      },
      test("object") {
        val result = decodeFoo("""{"x":42}""")
        assertTrue(result == Right(Foo(42)))
      },
      test("object with trailing whitespace") {
        val result = decodeFoo("""{"x":42}  """)
        assertTrue(result == Right(Foo(42)))
      },
      test("integer") {
        val result = decodeInt("123")
        assertTrue(result == Right(123))
      }
    )
  )
}
