package zio.schema.codec

import zio.schema.codec.JsonCodecSpec.{ assertDecodes, assertEncodesJson, stringify, test }
import zio.schema.{ Schema, StandardType }
import zio.test.{ Gen, Spec, TestEnvironment, check }

object JsonPlatformSpecific {

  val platformSpecificEncoderTests: List[Spec[TestEnvironment, Nothing]] = List(
    test("Currency") {
      assertEncodesJson(
        Schema.Primitive(StandardType.CurrencyType),
        java.util.Currency.getInstance("USD")
      )
    }
  )

  val platformSpecificDecoderTests: List[Spec[TestEnvironment, DecodeError]] = List(
    test("Currency") {
      check(Gen.currency)(
        currency => assertDecodes(Schema[java.util.Currency], currency, stringify(currency.getCurrencyCode))
      )
    }
  )
}
