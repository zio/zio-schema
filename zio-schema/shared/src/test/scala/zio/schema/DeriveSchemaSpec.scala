package zio.schema

import zio.schema.SchemaAssertions.hasSameSchema
import zio.test._

object DeriveSchemaSpec extends DefaultRunnableSpec {

  sealed case class UserId(id: String)

  sealed case class User(name: String, id: UserId)

  sealed trait Status
  case class Ok(response: List[String]) extends Status
  case class Failed(code: Int, reason: String, additionalExplanation: Option[String], remark: String = "oops")
      extends Status
  case object Pending extends Status

  sealed trait OneOf
  case class StringValue(value: String)   extends OneOf
  case class IntValue(value: Int)         extends OneOf
  case class BooleanValue(value: Boolean) extends OneOf

  override def spec: ZSpec[Environment, Failure] = suite("DeriveSchemaSpec")(
    test("DeriveSchema correctly derives schema for UserId case class") {

      val userIdSchema: Schema[UserId] = DeriveSchema.gen

      val expectedSchema: Schema[UserId] =
        Schema.CaseClass1(
          field = "id" -> Schema.Primitive(StandardType.StringType),
          UserId.apply,
          (uid: UserId) => uid.id
        )

      assert(userIdSchema)(hasSameSchema(expectedSchema))
    },
    test("DeriveSchema correctly derives schema for case class with nested case classes") {
      val derived: Schema[User] = DeriveSchema.gen
      val expected: Schema[User] =
        Schema.CaseClass2(
          field1 = ("name", Schema.Primitive(StandardType.StringType)),
          field2 = (
            "id",
            Schema.CaseClass1(
              field = "id" -> Schema.Primitive(StandardType.StringType),
              UserId.apply,
              (uid: UserId) => uid.id
            )
          ),
          User.apply,
          (u: User) => u.name,
          (u: User) => u.id
        )
      assert(derived)(hasSameSchema(expected))
    },
    test("DeriveSchema correctly derives schema for complex ADT") {

      val statusSchema: Schema[Status] = DeriveSchema.gen[Status]

      val expectedSchema: Schema[Status] =
        Schema.Enum3(
          Schema.Case("Failed", DeriveSchema.gen[Failed], (s: Status) => s.asInstanceOf[Failed]),
          Schema.Case("Ok", DeriveSchema.gen[Ok], (s: Status) => s.asInstanceOf[Ok]),
          Schema.Case(
            "Pending",
            DeriveSchema.gen[Pending.type],
            (s: Status) => s.asInstanceOf[Pending.type]
          )
        )

      assert(statusSchema)(hasSameSchema(expectedSchema))
    }
  )
}
