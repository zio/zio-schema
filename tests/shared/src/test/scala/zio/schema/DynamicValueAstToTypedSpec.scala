package zio.schema

import scala.collection.immutable.ListMap
import zio.test.Assertion._
import zio.test._

object DynamicValueAstToTypedSpec extends ZIOSpecDefault {

  final case class User(name: String, age: Int)
  object User {
    implicit val schema: Schema[User] = DeriveSchema.gen[User]
  }

  def spec: Spec[TestEnvironment, Any] =
    suite("DynamicValueAstToTypedSpec")(
      test("converts DynamicValue.Record back to typed value via schema.ast.toSchema (#511)") {
        val user        = User("John", 30)
        val userDynamic = User.schema.toDynamic(user)
        val fromOrig    = userDynamic.toTypedValue(User.schema)
        val fromAst     = userDynamic.toTypedValue(User.schema.ast.toSchema)

        assert(fromOrig)(isRight(equalTo(user))) &&
        assert(fromAst)(isRight(equalTo(ListMap[String, Any]("name" -> "John", "age" -> 30))))
      },
      test("preserves schema field ordering when DynamicValue.Record keys are reordered (#511)") {
        val reordered = DynamicValue.Record(
          User.schema.asInstanceOf[Schema.Record[User]].id,
          ListMap(
            "age"  -> DynamicValue.Primitive(30, StandardType.IntType),
            "name" -> DynamicValue.Primitive("John", StandardType.StringType)
          )
        )
        assert(reordered.toTypedValue(User.schema))(isRight(equalTo(User("John", 30))))
      }
    )
}
