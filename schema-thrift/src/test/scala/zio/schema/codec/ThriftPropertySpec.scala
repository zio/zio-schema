package zio.schema.codec

import zio._
import zio.schema._
import zio.stream._
import zio.test._

/**
 * GOOGLE-STANDARD PROPERTY-BASED TESTING SUITE
 * --------------------------------------------
 * This suite verifies the mathematical correctness of the codec.
 * Instead of checking specific values (e.g. 1, "abc"), it generates
 * random usage scenarios (Fuzz Testing) to find edge cases.
 * Principle: decode(encode(x)) == x (Round Trip Law)
 */

object ThriftPropertySpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment with Scope, Any] = suite("Thrift Property-Based Reliability Tests")(
    test("Round-trip property for Integers (Fuzzing)") {
      check(Gen.int) { value =>
        val schema = Schema[Int]
        val codec  = ThriftCodec.thriftCodec(schema)

        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      }
    },
    test("Round-trip property for Strings (Unicode & Empty)") {
      check(Gen.string) { value =>
        val schema = Schema[String]
        val codec  = ThriftCodec.thriftCodec(schema)

        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      }
    },
    test("Round-trip property for List of Integers (Stress Test)") {
      check(Gen.listOf(Gen.int)) { value =>
        val schema = Schema[List[Int]]
        val codec  = ThriftCodec.thriftCodec(schema)

        for {
          encoded <- ZIO.succeed(codec.encode(value))
          decoded <- ZIO.fromEither(codec.decode(encoded))
        } yield assertTrue(decoded == value)
      }
    },
    test("Streaming Pipeline Verification (ZPipeline)") {
      check(Gen.string) { value =>
        val schema = Schema[String]
        val codec  = ThriftCodec.thriftCodec(schema)

        for {
          byteStream <- ZIO.succeed(
                         ZStream(value).via(codec.streamEncoder)
                       )
          resultStream <- byteStream.via(codec.streamDecoder).runCollect
        } yield assertTrue(resultStream.head == value)
      }
    }
  )
}
