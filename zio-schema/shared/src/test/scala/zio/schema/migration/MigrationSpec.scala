package zio.schema.migration

import zio.Chunk
import zio.schema._
import zio.test._
import zio.test.Assertion._

object MigrationSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with zio.Scope, Any] = suite("MigrationSpec")(
    test("Identity Law: migration.identity should leave value unchanged") {
      val dv = DynamicValue.Primitive("test", StandardType.StringType)
      val migration = Migration.identity[String]
      assert(migration.apply(dv))(isRight(equalTo(dv)))
    },
    test("Structural Reverse Law: migration.reverse.reverse should be equal to original") {
      val action = MigrationAction.RenameField(Chunk.empty, "old", "new")
      val migration = Migration(Chunk(action))
      assert(migration.reverse.actions.head.asInstanceOf[MigrationAction.RenameField].from)(equalTo("new")) &&
      assert(migration.reverse.reverse.actions.head.asInstanceOf[MigrationAction.RenameField].from)(equalTo("old"))
    },
    test("Nested Update: should update value at path") {
      val dv = DynamicValue.Record(TypeId.parse("Person"), scala.collection.immutable.ListMap(
        "address" -> DynamicValue.Record(TypeId.parse("Address"), scala.collection.immutable.ListMap(
          "city" -> DynamicValue.Primitive("Soul", StandardType.StringType)
        ))
      ))
      val action = MigrationAction.RenameField(Chunk("address"), "city", "cityName")
      val result = action.apply(dv)
      result match {
        case Right(DynamicValue.Record(_, values)) =>
          values.get("address") match {
            case Some(DynamicValue.Record(_, inner)) =>
              assert(inner.get("cityName"))(isSome(equalTo(DynamicValue.Primitive("Soul", StandardType.StringType))))
            case _ => assert(true)(isFalse)
          }
        case _ => assert(true)(isFalse)
      }
    }
  )
}
