package zio.schema

import scala.annotation.Annotation

import zio.Chunk
import zio.test._

object SchemaDerivationSpec extends DefaultRunnableSpec {
  import Assertion._
  import SchemaAssertions._

  final case class annotation1(value: String) extends Annotation
  final case class annotation2(value: String) extends Annotation

  val p = Chunk.apply[Any](new annotation1("foo"), new annotation2("bar"))

  final case class ContainerFields(field1: Option[String], field2: List[String])

  object ContainerFields {
    implicit lazy val schema: Schema[ContainerFields] = SchemaDerivation.gen[ContainerFields]
  }

  sealed case class UserId(id: String)
  sealed case class User(name: String, @annotation1("foo") @annotation2("bar") id: UserId)

  object User {
    implicit lazy val schema: Schema.CaseClass2[String, UserId, User] = SchemaDerivation.gen[User]
  }

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

  case class Recursive(
    field1: Int,
    field2: Option[Recursive] = None,
    field3: List[Recursive] = Nil,
    field4: Set[Recursive] = Set.empty,
    field5: Vector[Recursive] = Vector.empty,
    field6: Chunk[Recursive] = Chunk.empty
  )

  object Recursive {
    implicit lazy val schema: Schema[Recursive] = SchemaDerivation.gen[Recursive]
  }

  case class Cyclic(field1: Long, child: CyclicChild1)

  object Cyclic {
    implicit lazy val schema: Schema.CaseClass2[Long, CyclicChild1, Cyclic] = SchemaDerivation.gen[Cyclic]
  }

  case class CyclicChild1(field1: Int, child: CyclicChild2)
  case class CyclicChild2(field1: String, recursive: Option[Cyclic])

  final case class Arity24(
    a1: String,
    a2: Long,
    a3: Boolean,
    f4: Int = 4,
    f5: Int = 5,
    f6: Int = 6,
    f7: Int = 7,
    f8: Int = 8,
    f9: Int = 9,
    f10: Int = 10,
    f11: Int = 11,
    f12: Int = 12,
    f13: Int = 13,
    f14: Int = 14,
    f15: Int = 15,
    f16: Int = 16,
    f17: Int = 17,
    f18: Int = 18,
    f19: Int = 19,
    f20: Int = 20,
    f21: Int = 21,
    f22: Int = 22,
    f23: Int = 23,
    f24: Int = 24
  )

  object Arity24 {
    implicit lazy val schema: Schema[Arity24] = SchemaDerivation.gen[Arity24]
  }

//
  case class DependsOnA(a: Option[DependsOnB])

  object DependsOnA {
    implicit lazy val schema: Schema[DependsOnA] = SchemaDerivation.gen[DependsOnA]
  }
  case class DependsOnB(b: Option[DependsOnA])

  object DependsOnB {
    implicit lazy val schema: Schema[DependsOnB] = SchemaDerivation.gen[DependsOnB]
  }

  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A)                        extends Tree[A]
  case object Root                                    extends Tree[Nothing]

  sealed trait RecursiveEnum
  case class RecursiveEnum1(label: String, rs: List[RecursiveEnum]) extends RecursiveEnum
  case object RecursiveEnum2                                        extends RecursiveEnum

  // object RecursiveEnum {
  //   implicit lazy val schema: Schema[RecursiveEnum] = gen
  // }

  override def spec: ZSpec[Environment, Failure] = suite("DeriveSchemaSpec")(
    suite("Derivation")(
      test("case class") {
        assert(Schema[User].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("case class with arity > 22") {
        assert(Schema[Arity24].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("recursive data structure") {
        assert(Schema[Recursive].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("cyclic recursion") {
        val c = Cyclic(1, CyclicChild1(2, CyclicChild2("3", None)))
        val _ = Schema[Cyclic].toDynamic(c)
        assert(Schema[Cyclic].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derives schema for UserId case class") {
        println(User.schema.field2.annotations)
        val derived: Schema[UserId] = SchemaDerivation.gen[UserId]
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
        val derived: Schema[Singleton.type]  = SchemaDerivation.gen[Singleton.type]
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
                field = Schema.Field("id", Schema.Primitive(StandardType.StringType)),
                UserId.apply,
                (uid: UserId) => uid.id
              )
//               Chunk(annotation1("foo"), annotation2("bar"))
            ),
            User.apply,
            (u: User) => u.name,
            (u: User) => u.id
          )
        assert(derived)(hasSameSchema(expected))
      },
      // test("correctly derives schema for complex ADT") {
      //   val derived: Schema[Status] = Schema[Status]
      //   val expected: Schema[Status] =
      //     Schema.Enum3(
      //       Schema.Case("Failed", Schema[Failed], (s: Status) => s.asInstanceOf[Failed]),
      //       Schema.Case("Ok", Schema[Ok], (s: Status) => s.asInstanceOf[Ok]),
      //       Schema.Case(
      //         "Pending",
      //         Schema[Pending.type],
      //         (s: Status) => s.asInstanceOf[Pending.type]
      //       )
      //     )

      //   assert(derived)(hasSameSchema(expected))
      // } @@ TestAspect.ignore,
      test("correctly derives for case classes with recursive types") {
        assert(Schema[Recursive].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derives mutually recursive case classes") {
        val a  = DependsOnA(Some(DependsOnB(None)))
        val a0 = Schema[DependsOnA].fromDynamic(Schema[DependsOnA].toDynamic(a))
        assert(Schema[DependsOnA])(anything)
        assert(a0)(isRight(equalTo(a)))

        val b = DependsOnB(Some(DependsOnA(None)))
        val b0 = Schema[DependsOnB].fromDynamic(Schema[DependsOnB].toDynamic(b))
        assert(Schema[DependsOnB])(anything)
        assert(b0)(isRight(equalTo(b)))
      }
      // test("correctly derives Tree") {
      //   assert(gen[Tree[Recursive]])(anything)
      // } @@ TestAspect.ignore,
      // test("correctly derives recursive ADT") {
      //   assert(gen[RecursiveEnum])(anything)

      // } @@ TestAspect.ignore
    )
  )
}
