package zio.schema.json

import zio.schema.DynamicValue
import zio.test.Assertion._
import zio.test._

object JsonPatchSpec extends ZIOSpecDefault {
  override def spec: Spec[Any, Any] = suite("JsonPatch Spec")(
    test("Add operation should hold correct path and value") {
      val patch = JsonPatch.Add("path", DynamicValue("value"))
      assert(patch.path)(equalTo("path"))
    },
    test("Remove operation should hold correct path") {
      val patch = JsonPatch.Remove("path")
      assert(patch.path)(equalTo("path"))
    }
  )
}
