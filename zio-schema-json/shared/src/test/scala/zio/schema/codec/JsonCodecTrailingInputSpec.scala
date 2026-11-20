package zio.schema.codec

import zio._
import zio.schema._
import zio.schema.codec.JsonCodec.JsonEncoder.charSequenceToByteChunk
import zio.stream.ZStream
import zio.test._

object JsonCodecTrailingInputSpec extends ZIOSpecDefault {
  private def binaryCodecs[A](schema: Schema[A]): List[BinaryCodec[A]] =
    List(
      JsonCodec.schemaBasedBinaryCodec(schema),
      JsonCodec.schemaBasedBinaryCodec(JsonCodec.Configuration(treatStreamsAsArrays = true))(schema),
      JsonCodec.zioJsonBinaryCodec(JsonCodec.jsonCodec(schema))
    )

  private def rejectsAll[A](schema: Schema[A], inputs: String*): TestResult =
    assertTrue(
      binaryCodecs(schema).forall(codec => inputs.forall(input => codec.decode(charSequenceToByteChunk(input)).isLeft))
    )

  def spec: Spec[TestEnvironment, Any] = suite("JsonCodec trailing input rejection")(
    test("rejects trailing characters after strings and objects (issue 712)") {
      rejectsAll(Schema[String], "\"foo\"\"", "\"foo\"extra", "\"foo\" \"bar\"") &&
      rejectsAll(Schema[Unit], "{}}", "{}{}", "{} extra")
    },
    test("rejects trailing tokens after primitives and arrays") {
      rejectsAll(Schema[Int], "42x", "42 99", "42]") &&
      rejectsAll(Schema[Boolean], "truefalse", "false}") &&
      rejectsAll(Schema[List[Int]], "[1,2]]", "[1,2] [3]") &&
      rejectsAll(Schema[Option[Int]], "nullx", "null null")
    },
    test("allows standard JSON whitespace after a valid top-level value") {
      assertTrue(binaryCodecs(Schema[Int]).forall { codec =>
        List("42", " 42 ", "42\t\r\n ").forall(input => codec.decode(charSequenceToByteChunk(input)) == Right(42))
      }) && assertTrue(binaryCodecs(Schema[String]).forall { codec =>
        codec.decode(charSequenceToByteChunk("\"foo\" \t\r\n")) == Right("foo")
      })
    },
    test("consumes unselected fallback array elements before checking for EOF") {
      val leftOnly   = Schema.Fallback(Schema[Int], Schema[String])
      val fullDecode = Schema.Fallback(Schema[Int], Schema[String], true)
      assertTrue(binaryCodecs(leftOnly).forall { codec =>
        codec.decode(charSequenceToByteChunk("[30,\"hello\"]")) == Right(Fallback.Left(30))
      }) && assertTrue(binaryCodecs(fullDecode).forall { codec =>
        codec.decode(charSequenceToByteChunk("[30,30]")) == Right(Fallback.Left(30))
      }) && rejectsAll(leftOnly, "[30,\"hello\"]extra") && rejectsAll(fullDecode, "[30,30]]")
    },
    test("preserves multi-element stream decoding across byte chunks") {
      val schemaCodec = JsonCodec.schemaBasedBinaryCodec(Schema[Int])
      val zioCodec    = JsonCodec.zioJsonBinaryCodec(zio.json.JsonCodec.int)
      for {
        results <- ZIO.foreach(List(schemaCodec, zioCodec)) { codec =>
                     ZStream
                       .fromChunk(charSequenceToByteChunk("1\n2\n3"))
                       .rechunk(1)
                       .via(codec.streamDecoder)
                       .runCollect
                   }
      } yield assertTrue(results.forall(_ == Chunk(1, 2, 3)))
    }
  )
}
