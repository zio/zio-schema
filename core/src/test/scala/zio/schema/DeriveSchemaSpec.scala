package zio.schema

//import zio.Chunk
import zio.schema.SchemaAssertions.hasSameSchema
import zio.test.{ DefaultRunnableSpec, ZSpec, assert, suite, test }

object DeriveSchemaSpec extends DefaultRunnableSpec {

  sealed case class UserId(id: String)

  sealed case class User(name: String, id: UserId)

  sealed trait Status
  case class Ok(response: List[String]) extends Status
  case class Failed(code: Int, reason: String, additionalExplanation: Option[String], remark: String = "oops")
      extends Status
  case object Pending extends Status

  override def spec: ZSpec[Environment, Failure] = suite("DeriveSchemaSpec")(
    test("DeriveSchema correctly derives schema for UserId case class") {

      val userIdSchema: Schema[UserId] = DeriveSchema.gen

      val expectedSchema: Schema[UserId] =
        Schema.CaseClass1(
          field = "id" -> Schema.Primitive(StandardType.StringType),
          UserId.apply,
          (uid: UserId) => Tuple1(uid.id)
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
              (uid: UserId) => Tuple1(uid.id)
            )
          ),
          User.apply,
          (u: User) => User.unapply(u).get
        )
      assert(derived)(hasSameSchema(expected))
    },
    test("DeriveSchema correctly derives schema for complex ADT") {

      val statusSchema: Schema[Status] = DeriveSchema.gen[Status]

      val expectedSchema: Schema[Status] =
        Schema.Transform[Map[String, Any], Status](
          Schema.Enumeration(
            Map(
              "Ok"      -> DeriveSchema.gen[Ok],
              "Failed"  -> DeriveSchema.gen[Failed],
              "Pending" -> DeriveSchema.gen[Pending.type]
            )
          ),
          _ => Right(Pending), // ignored in equality comparison
          _ => Right(Map("Pending" -> Pending)) // ignored in equality comparison
        )

      assert(statusSchema)(hasSameSchema(expectedSchema))
    }
  )
}
