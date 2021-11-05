package zio.schema

import scala.annotation.Annotation

import zio.Chunk
import zio.schema.DeriveSchema._
import zio.schema.SchemaAssertions.hasSameSchema
import zio.test.Assertion._
import zio.test._

object DeriveSchemaSpec extends DefaultRunnableSpec {

  final case class annotation1(value: String) extends Annotation
  final case class annotation2(value: String) extends Annotation
  final case class annotation3(value: String) extends Annotation

  @annotation1("bar")
  sealed case class UserId(id: String)

  @annotation1("foo")
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

  case class DependsOnA(a: DependsOnB)
  case class DependsOnB(b: DependsOnA)

  sealed trait Tree[+A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A)                        extends Tree[A]
  case object Root                                    extends Tree[Nothing]

  @annotation1("foo")
  sealed trait AnnotatedSumType

  @annotation2("bar")
  case class AnnotatedSumType1(value: Int) extends AnnotatedSumType

  @annotation3("baz")
  case class AnnotatedSumType2(value: Int) extends AnnotatedSumType

  object RecursiveAnnotatedSumTypeEnum {
    implicit lazy val schema: Schema[AnnotatedSumType] = DeriveSchema.gen
  }

  sealed trait RecursiveEnum
  case class RecursiveEnum1(label: String, rs: List[RecursiveEnum]) extends RecursiveEnum
  case object RecursiveEnum2                                        extends RecursiveEnum

  object RecursiveEnum {
    implicit lazy val schema: Schema[RecursiveEnum] = DeriveSchema.gen
  }

  override def spec: ZSpec[Environment, Failure] = suite("DeriveSchemaSpec")(
    suite("Derivation")(
      test("correctly derives schema for UserId case class") {
        val derived: Schema[UserId] = Schema[UserId]
        val expected: Schema[UserId] =
          Schema.CaseClass1(
            annotations = Chunk.empty,
            field = Schema.Field("id", Schema.Primitive(StandardType.StringType)),
            UserId.apply,
            (uid: UserId) => uid.id
          )

        assert(derived)(hasSameSchema(expected))
      },
      test("correctly derives schema for Singleton case object") {
        val derived: Schema[Singleton.type]  = Schema[Singleton.type]
        val expected: Schema[Singleton.type] = Schema[Unit].transform(_ => Singleton, _ => ())

        assert(derived)(hasSameSchema(expected))
      },
      test("correctly derives schema for case class with nested case classes") {
        val derived: Schema[User] = Schema[User]
        val expected: Schema[User] =
          Schema.CaseClass2(
            annotations = Chunk.empty,
            field1 = Schema.Field("name", Schema.Primitive(StandardType.StringType)),
            field2 = Schema.Field(
              "id",
              Schema.CaseClass1(
                annotations = Chunk.empty,
                field = Schema.Field("id", Schema.Primitive(StandardType.StringType, Chunk.empty)),
                construct = UserId.apply,
                extractField = (uid: UserId) => uid.id
              ),
              Chunk(annotation1("foo"), annotation2("bar"))
            ),
            User.apply,
            (u: User) => u.name,
            (u: User) => u.id
          )
        assert(derived)(hasSameSchema(expected))
      },
      test("correctly derives schema for annotated sum type") {
        val derived: Schema[AnnotatedSumType] = Schema[AnnotatedSumType]
        lazy val expected: Schema[AnnotatedSumType] =
          Schema.Enum2[AnnotatedSumType1, AnnotatedSumType2, AnnotatedSumType](
            Schema.Case[AnnotatedSumType1, AnnotatedSumType](
              id = "AnnotatedSumType1",
              codec = Schema.CaseClass1(
                annotations = Chunk(annotation2("bar")),
                field = Schema.Field[Int]("value", Schema.Primitive(StandardType.IntType, Chunk.empty), Chunk.empty),
                construct = AnnotatedSumType1.apply,
                extractField = (a: AnnotatedSumType1) => a.value
              ),
              unsafeDeconstruct = _.asInstanceOf[AnnotatedSumType1],
              annotations = Chunk(annotation2("bar"))
            ),
            Schema.Case[AnnotatedSumType2, AnnotatedSumType](
              id = "AnnotatedSumType2",
              codec = Schema.CaseClass1(
                annotations = Chunk(annotation3("baz")),
                field = Schema.Field[Int]("value", Schema.Primitive(StandardType.IntType, Chunk.empty), Chunk.empty),
                construct = AnnotatedSumType2.apply,
                extractField = (a: AnnotatedSumType2) => a.value
              ),
              unsafeDeconstruct = _.asInstanceOf[AnnotatedSumType2],
              annotations = Chunk(annotation3("baz"))
            ),
            Chunk(annotation1("foo"))
          )
        assert(derived)(hasSameSchema(expected))
      },
      test("correctly derives schema for complex ADT") {
        val derived: Schema[Status] = Schema[Status]
        val expected: Schema[Status] =
          Schema.Enum3(
            Schema.Case("Failed", Schema[Failed], (s: Status) => s.asInstanceOf[Failed], Chunk.empty),
            Schema.Case("Ok", Schema[Ok], (s: Status) => s.asInstanceOf[Ok], Chunk.empty),
            Schema.Case(
              "Pending",
              Schema[Pending.type],
              (s: Status) => s.asInstanceOf[Pending.type],
              Chunk.empty
            ),
            Chunk.empty // TODO: Should include test for annotations probably
          )

        assert(derived)(hasSameSchema(expected))
      },
      test("correctly derives for case classes with recursive types") {
        assert(DeriveSchema.gen[Recursive])(anything)
      },
      test("correctly derives mutually recrusive case classes") {
        assert(DeriveSchema.gen[DependsOnA] -> DeriveSchema.gen[DependsOnB])(anything)
      },
      test("correctly derives Tree") {
        assert(DeriveSchema.gen[Tree[Recursive]])(anything)
      },
      test("correctly derives recursive ADT") {
        assert(DeriveSchema.gen[RecursiveEnum])(anything)

      }
    )
  )
}
