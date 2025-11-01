package zio.schema

import zio.test.*

object IArraySpec extends ZIOSpecDefault {

  def spec =
    suite("IArraySpec")(
      test("Can derive schema for IArray") {
        val schema = Schema[IArray[Int]]
        assertTrue(true)
      }
    )
}
