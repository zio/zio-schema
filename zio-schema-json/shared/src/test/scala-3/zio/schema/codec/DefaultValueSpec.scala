package zio.schema.codec

import zio.Console._
import zio._
import zio.json.{ DeriveJsonEncoder, JsonEncoder }
import zio.schema._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test.*

object DefaultValueSpec extends ZIOSpecDefault {
  implicit val defaultConfig: JsonCodec.Config = JsonCodec.Config.default

  def spec: Spec[TestEnvironment, Any] =
    suite("Custom Spec")(
      customSuite
    ) @@ timeout(90.seconds)

  private val customSuite = suite("custom")(
    suite("default value schema")(
      test("default value at last field") {
        val result = JsonCodec.jsonDecoder(Schema[WithDefaultValue], defaultConfig).decodeJson("""{"orderId": 1}""")
        assertTrue(result.isRight)
      }
    )
  )

  case class WithDefaultValue(orderId: Int, description: String = "desc")

  object WithDefaultValue {
    implicit lazy val schema: Schema[WithDefaultValue] = DeriveSchema.gen[WithDefaultValue]
  }

}
