package zio.schema.codec

import zio.schema.{ Schema, StandardType }
import zio.schema.codec.JsonCodecSpec.{ assertDecodes, assertEncodesJson, stringify, test }
import zio.test.{ Gen, check }

object JsonPlatformSpecific {

  val platformSpecificEncoderTests = Seq(
    test("Currency") {
      assertEncodesJson(
        Schema.Primitive(StandardType.CurrencyType),
        java.util.Currency.getInstance(java.util.Locale.getDefault())
      )
    }
  )

  val platformSpecificDecoderTests = Seq(
    test("Currency") {
      check(Gen.currency)(
        currency => assertDecodes(Schema[java.util.Currency], currency, stringify(currency.getCurrencyCode()))
      )
    }
  )
}
