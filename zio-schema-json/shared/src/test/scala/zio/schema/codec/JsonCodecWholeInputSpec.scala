package zio.schema.codec

import zio._
import zio.schema._
import zio.schema.codec.JsonCodec.JsonEncoder.charSequenceToByteChunk
import zio.stream.ZStream
import zio.test._

object JsonCodecWholeInputSpec extends ZIOSpecDefault {
  private def codecs[A](schema: Schema[A]): List[BinaryCodec[A]] =
    List(
      JsonCodec.schemaBasedBinaryCodec(schema),
      JsonCodec.schemaBasedBinaryCodec(JsonCodec.Configuration(treatStreamsAsArrays = true))(schema),
      JsonCodec.zioJsonBinaryCodec(JsonCodec.jsonCodec(schema))
    )

  private def rejects[A](schema: Schema[A], inputs: String*): TestResult =
    assertTrue(
      codecs(schema).forall(codec => inputs.forall(input => codec.decode(charSequenceToByteChunk(input)).isLeft))
    )

  def spec: Spec[TestEnvironment, Any] = suite("JsonCodec whole input")(
    test("rejects trailing characters after strings and objects (issue 712)") {
      rejects(Schema[String], "\"foo\"\"", "\"foo\"garbage", "\"foo\" \"bar\"") &&
      rejects(Schema[Unit], "{}}", "{}{}", "{} trailing")
    },
    test("rejects trailing characters after numbers, booleans, arrays and null") {
      rejects(Schema[Int], "123x", "1 2", "1]", "1\nfalse") &&
      rejects(Schema[Boolean], "truefalse", "false}") &&
      rejects(Schema[List[Int]], "[1,2]]", "[1,2] []") &&
      rejects(Schema[Option[Int]], "nullx", "null null")
    },
    test("rejects non-JSON whitespace after a value") {
      rejects(Schema[Int], "1\u0000", "1\u000b", "1\u000c", "1\u00a0")
    },
    test("accepts end of input and the four JSON whitespace characters") {
      assertTrue(codecs(Schema[Int]).forall { codec =>
        List("123", " 123 ", "123\t\r\n ").forall(input => codec.decode(charSequenceToByteChunk(input)) == Right(123))
      }) && assertTrue(codecs(Schema[String]).forall { codec =>
        codec.decode(charSequenceToByteChunk("\"foo\" \t\r\n")) == Right("foo")
      })
    },
    test("preserves malformed and incomplete input errors") {
      rejects(Schema[Int], "", " \t\r\n", "oops") &&
      rejects(Schema[String], "\"unfinished") &&
      rejects(Schema[List[Int]], "[1,2")
    },
    test("preserves nested decoder composition") {
      val schema = Schema[List[Option[Int]]]
      assertTrue(codecs(schema).forall { codec =>
        codec.decode(charSequenceToByteChunk("[1,null,2]")) == Right(List(Some(1), None, Some(2)))
      })
    },
    test("fallbacks consume the unselected value before checking for trailing input") {
      val leftOnly   = Schema.Fallback(Schema[Int], Schema[String])
      val fullDecode = Schema.Fallback(Schema[Int], Schema[String], true)
      assertTrue(codecs(leftOnly).forall { codec =>
        codec.decode(charSequenceToByteChunk("[30,\"hello\"]")) == Right(Fallback.Left(30))
      }) && assertTrue(codecs(fullDecode).forall { codec =>
        codec.decode(charSequenceToByteChunk("[30,30]")) == Right(Fallback.Left(30))
      }) && rejects(leftOnly, "[30,\"hello\"]garbage") && rejects(fullDecode, "[30,30]]")
    },
    test("streams still decode multiple values across byte chunks") {
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
    },
    test("array streams still decode each element") {
      val codec = JsonCodec.schemaBasedBinaryCodec(JsonCodec.Configuration(treatStreamsAsArrays = true))(Schema[Int])
      for {
        result <- ZStream
                    .fromChunk(charSequenceToByteChunk("[1,2,3]"))
                    .rechunk(1)
                    .via(codec.streamDecoder)
                    .runCollect
      } yield assertTrue(result == Chunk(1, 2, 3))
    }
  )
}
