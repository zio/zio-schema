package zio.schema.json

import scala.collection.immutable.ListMap

import zio.schema.{ DynamicValue, TypeId }
import zio.test._

object JsonPatchSpec extends ZIOSpecDefault {
  override def spec: Spec[Any, Any] = suite("JsonPatch Spec")(
    test("Add: should add field to record") {
      val user   = TypeId.parse("User")
      val data   = ListMap("name" -> DynamicValue("Akshaya"))
      val record = DynamicValue.Record(user, data)

      val patch  = JsonPatch.Add("/age", DynamicValue(25))
      val result = patch.apply(record)

      val expectedData = ListMap("name" -> DynamicValue("Akshaya"), "age" -> DynamicValue(25))
      val expected     = DynamicValue.Record(user, expectedData)

      assertTrue(result == Right(expected))
    },
    test("Remove: should remove field from record") {
      val user   = TypeId.parse("User")
      val data   = ListMap("name" -> DynamicValue("Akshaya"))
      val record = DynamicValue.Record(user, data)

      val patch  = JsonPatch.Remove("/name")
      val result = patch.apply(record)

      val expected = DynamicValue.Record(user, ListMap.empty[String, DynamicValue])

      assertTrue(result == Right(expected))
    },
    test("Failure: target is not a record") {
      val patch  = JsonPatch.Remove("/any")
      val result = patch.apply(DynamicValue("random-string"))

      assertTrue(result == Left("expected record"))
    }
  )
}
