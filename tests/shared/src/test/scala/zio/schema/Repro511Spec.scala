package zio.schema

import zio.test._

object Repro511Spec extends ZIOSpecDefault {

  final case class User(name: String, age: Int)
  implicit val schema: Schema[User] = DeriveSchema.gen[User]

  def spec: Spec[Environment, Any] =
    test("repro 511") {
      val user        = User("John", 30)
      val userDynamic = schema.toDynamic(user)
      val direct      = userDynamic.toTypedValue(schema)
      val fromAst     = userDynamic.toTypedValue(schema.ast.toSchema)
      println(s"dynamic: $userDynamic")
      println(s"direct: $direct")
      println(s"fromAst: $fromAst")
      assertTrue(direct == Right(user))
    }
}
