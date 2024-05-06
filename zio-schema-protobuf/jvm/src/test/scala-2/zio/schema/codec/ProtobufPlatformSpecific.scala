package zio.schema.codec

import zio.Chunk
import zio.schema.Schema
import zio.schema.codec.ProtobufCodecSpec.{ encodeAndDecode, encodeAndDecodeNS, test }
import zio.test.Assertion.equalTo
import zio.test.{ Gen, assert, check }

object ProtobufPlatformSpecific {

  @SuppressWarnings(Array(
    "scalafix:ExplicitResultTypes"
  ))
  val platformSpecificEncodeAndDecode = Seq(
    test("currencies") {
      check(Gen.currency) { value =>
        for {
          ed  <- encodeAndDecode(Schema[java.util.Currency], value)
          ed2 <- encodeAndDecodeNS(Schema[java.util.Currency], value)
        } yield assert(ed)(equalTo(Chunk(value))) && assert(ed2)(equalTo(value))
      }
    }
  )
}
