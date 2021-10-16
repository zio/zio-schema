package zio.schema

import zio.Chunk

import scala.annotation.Annotation

//import zio.Chunk
// import zio.schema.DeriveSchema._
//import zio.schema.SchemaAssertions.hasSameSchema
// import zio.test.Assertion._
import zio.test._

object SchemaDerivationSpec extends DefaultRunnableSpec {
  import Assertion._
//  import SchemaDerivation._

  final case class annotation1(value: String) extends Annotation
  final case class annotation2(value: String) extends Annotation

  sealed case class UserId(id: String)

  object UserId {
    implicit lazy val schema: Schema[UserId] = SchemaDerivation.gen[UserId]
  }

  final case class ContainerFields(field1: Option[String], field2: List[String])

  object ContainerFields {
    implicit lazy val schema: Schema[ContainerFields] = SchemaDerivation.gen[ContainerFields]
  }

  sealed case class User(name: String, @annotation1("foo") @annotation2("bar") id: UserId)

  object User {
    implicit lazy val schema = SchemaDerivation.gen[User]
  }

//  sealed trait Status
//  case class Ok(response: List[String]) extends Status
//  case class Failed(code: Int, reason: String, additionalExplanation: Option[String], remark: String = "oops")
//      extends Status
//  case object Pending extends Status
//
//  sealed trait OneOf
//  case class StringValue(value: String)   extends OneOf
//  case class IntValue(value: Int)         extends OneOf
//  case class BooleanValue(value: Boolean) extends OneOf
//
//  case object Singleton
//  implicit val singletonSchema = gen[Singleton.type]
//
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
//
  case class Cyclic(field1: Long, child: CyclicChild1)
  case class CyclicChild1(field1: Int, child: CyclicChild2)
  case class CyclicChild2(field1: String, recursive: Option[Cyclic])

  object Cyclic {
    implicit lazy val schema = SchemaDerivation.gen[Cyclic]
  }
//
//  case class DependsOnA(a: DependsOnB)
//
//  object DependsOnA {
//    implicit lazy val schema: Schema[DependsOnA] = gen[DependsOnA]
//  }
//  case class DependsOnB(b: DependsOnA)
//
//  object DependsOnB {
//    implicit lazy val schema: Schema[DependsOnB] = gen[DependsOnB]
//  }

  sealed trait Tree[+A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  case class Leaf[A](value: A)                        extends Tree[A]
  case object Root                                    extends Tree[Nothing]

  // sealed trait RecursiveEnum
  // case class RecursiveEnum1(label: String, rs: List[RecursiveEnum]) extends RecursiveEnum
  // case object RecursiveEnum2                                        extends RecursiveEnum

  // object RecursiveEnum {
  //   implicit lazy val schema: Schema[RecursiveEnum] = gen
  // }

  override def spec: ZSpec[Environment, Failure] = suite("DeriveSchemaSpec")(
    suite("Derivation")(
      test("case class") {
        assert(Schema[User].toString)(not(containsString("null")))
      },
      test("recursive data structure") {
        assert(Schema[Recursive].toString)(not(containsString("null")))
      },
      test("cyclic recursion") {
        assert(Schema[Cyclic].toString)(not(containsString("null")))
      }
      // test("correctly derives schema for UserId case class") {
      //   val derived: Schema[UserId] = Schema[UserId]
      //   val expected: Schema[UserId] =
      //     Schema.CaseClass1(
      //       annotations = Chunk.empty,
      //       field = Schema.Field("id", Schema.Primitive(StandardType.StringType)),
      //       UserId.apply,
      //       (uid: UserId) => uid.id
      //     )

      //   assert(derived)(hasSameSchema(expected))
      // },
      // test("correctly derives schema for Singleton case object") {
      //   val derived: Schema[Singleton.type]  = Schema[Singleton.type]
      //   val expected: Schema[Singleton.type] = Schema[Unit].transform(_ => Singleton, _ => ())

      //   assert(derived)(hasSameSchema(expected))
      // } @@ TestAspect.ignore,
      // test("correctly derives schema for case class with nested case classes") {
      //   val derived: Schema[User] = Schema[User]
      //   val expected: Schema[User] =
      //     Schema.CaseClass2(
      //       annotations = Chunk.empty,
      //       field1 = Schema.Field("name", Schema.Primitive(StandardType.StringType)),
      //       field2 = Schema.Field(
      //         "id",
      //         Schema.CaseClass1(
      //           annotations = Chunk.empty,
      //           field = Schema.Field("id", Schema.Primitive(StandardType.StringType)),
      //           UserId.apply,
      //           (uid: UserId) => uid.id
      //         ),
      //         Chunk(annotation1("foo"), annotation2("bar"))
      //       ),
      //       User.apply,
      //       (u: User) => u.name,
      //       (u: User) => u.id
      //     )
      //   assert(derived)(hasSameSchema(expected))
      // } @@ TestAspect.ignore,
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
      // test("correctly derives for case classes with recursive types") {
      //   assert(gen[Recursive])(anything)
      // } @@ TestAspect.ignore,
      // test("correctly derives mutually recrusive case classes") {
      //   assert(gen[DependsOnA] -> gen[DependsOnB])(anything)
      // } @@ TestAspect.ignore,
      // test("correctly derives Tree") {
      //   assert(gen[Tree[Recursive]])(anything)
      // } @@ TestAspect.ignore,
      // test("correctly derives recursive ADT") {
      //   assert(gen[RecursiveEnum])(anything)

      // } @@ TestAspect.ignore
    )
  )
}
