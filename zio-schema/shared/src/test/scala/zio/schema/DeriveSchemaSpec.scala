package zio.schema

import scala.annotation.Annotation

import zio.Chunk
import zio.schema.SchemaAssertions.hasSameSchema
import zio.test.Assertion._
import zio.test._

object DeriveSchemaSpec extends DefaultRunnableSpec {

  final case class annotation1(value: String) extends Annotation
  final case class annotation2(value: String) extends Annotation

  sealed case class UserId(id: String)

  sealed case class User(name: String, @annotation1("foo") @annotation2("bar") id: UserId)

  sealed trait Status
  case class Ok(response: List[String]) extends Status
  case class Failed(code: Int, reason: String, additionalExplanation: Option[String], remark: String = "oops")
      extends Status
  case object Pending extends Status

  sealed trait OneOf
  case class StringValue(value: String)   extends OneOf
  case class IntValue(value: Int)         extends OneOf
  case class BooleanValue(value: Boolean) extends OneOf

  case object Singleton

  case class Recursive(field: Option[Recursive])

  override def spec: ZSpec[Environment, Failure] = suite("DeriveSchemaSpec")(
    test("DeriveSchema correctly derives schema for UserId case class") {
      val derived: Schema[UserId] = DeriveSchema.gen
      val expected: Schema[UserId] =
        Schema.CaseClass1(
          annotations = Chunk.empty,
          field = Schema.Field("id", Schema.Primitive(StandardType.StringType)),
          UserId.apply,
          (uid: UserId) => uid.id
        )

      assert(derived)(hasSameSchema(expected))
    },
    test("DeriveSchema correctly derivs schema for Singleton case object") {
      val derived: Schema[Singleton.type]             = DeriveSchema.gen[Singleton.type]
      val expected: Schema.CaseObject[Singleton.type] = Schema.CaseObject(Singleton)

      assert(derived)(hasSameSchema(expected))
    },
    test("DeriveSchema correctly derives schema for case class with nested case classes") {
      val derived: Schema[User] = DeriveSchema.gen
      val expected: Schema[User] =
        Schema.CaseClass2(
          annotations = Chunk.empty,
          field1 = Schema.Field("name", Schema.Primitive(StandardType.StringType)),
          field2 = Schema.Field(
            "id",
            Schema.CaseClass1(
              annotations = Chunk.empty,
              field = Schema.Field("id", Schema.Primitive(StandardType.StringType)),
              UserId.apply,
              (uid: UserId) => uid.id
            ),
            Chunk(annotation1("foo"), annotation2("bar"))
          ),
          User.apply,
          (u: User) => u.name,
          (u: User) => u.id
        )
      assert(derived)(hasSameSchema(expected))
    },
    test("DeriveSchema correctly derives schema for complex ADT") {
      val derived: Schema[Status] = DeriveSchema.gen[Status]
      val expected: Schema[Status] =
        Schema.Enum3(
          Schema.Case("Failed", DeriveSchema.gen[Failed], (s: Status) => s.asInstanceOf[Failed]),
          Schema.Case("Ok", DeriveSchema.gen[Ok], (s: Status) => s.asInstanceOf[Ok]),
          Schema.Case(
            "Pending",
            DeriveSchema.gen[Pending.type],
            (s: Status) => s.asInstanceOf[Pending.type]
          )
        )

      assert(derived)(hasSameSchema(expected))
    },
    test("Deriveschema correctly derives for case classes with recursive types") {
      assert(DeriveSchema.gen[Recursive])(anything)
    }
  )
}
