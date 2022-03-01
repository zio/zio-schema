package zio.schema

import scala.annotation.Annotation

import zio.Chunk
import zio.test._

object DeriveSchemaSpec extends DefaultRunnableSpec {
  import Assertion._
  import SchemaAssertions._

  final case class annotation1(value: String) extends Annotation
  final case class annotation2(value: String) extends Annotation
  final class annotation3 extends Annotation {
    override def equals(obj: Any): Boolean =
      obj.isInstanceOf[annotation3]

    override def hashCode(): Int = 123214123
  }
  final class annotation4(val value: Int) extends Annotation {
    override def equals(obj: Any): Boolean =
      obj match {
        case other: annotation4 => value == other.value
        case _                  => false
      }

    override def hashCode(): Int = 45745623 ^ value
  }

  val fo = new annotation1("foo")

  final case class ContainerFields(field1: Option[String], field2: List[String])

  object ContainerFields {
    implicit lazy val schema: Schema[ContainerFields] = DeriveSchema.gen[ContainerFields]
  }

  sealed case class UserId(id: String)

  @annotation3
  sealed case class User(name: String, @annotation1("foo") @annotation2("bar") @annotation3 @annotation4(0) id: UserId)

  object User {
    implicit lazy val schema: Schema.CaseClass2[String, UserId, User] = DeriveSchema.gen[User]
  }

  sealed trait Status

  object Status {
    case class Ok(response: List[String]) extends Status
    case class Failed(code: Int, reason: String, additionalExplanation: Option[String], remark: String = "oops")
        extends Status
    case object Pending extends Status

    implicit lazy val schema: Schema.Enum3[Failed, Ok, zio.schema.DeriveSchemaSpec.Status.Pending.type, Status] =
      DeriveSchema.gen[Status]
  }

  sealed trait OneOf

  object OneOf {
    case class StringValue(value: String)   extends OneOf
    case class IntValue(value: Int)         extends OneOf
    case class BooleanValue(value: Boolean) extends OneOf

    implicit lazy val schema: Schema.Enum3[BooleanValue, IntValue, StringValue, OneOf] = DeriveSchema.gen[OneOf]
  }

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
    implicit lazy val schema: Schema[Recursive] = DeriveSchema.gen[Recursive]
  }

  case class Cyclic(field1: Long, child: CyclicChild1)

  object Cyclic {
    implicit lazy val schema: Schema.CaseClass2[Long, CyclicChild1, Cyclic] = DeriveSchema.gen[Cyclic]
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
    implicit lazy val schema: Schema[Arity24] = DeriveSchema.gen[Arity24]
  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  case class TupleArities(
    arity2: (User, User),
    arity3: (User, User, User),
    arity4: (User, User, User, User),
    arity5: (User, User, User, User, User),
    arity6: (User, User, User, User, User, User),
    arity7: (User, User, User, User, User, User, User),
    arity8: (User, User, User, User, User, User, User, User),
    arity9: (User, User, User, User, User, User, User, User, User),
    arity10: (User, User, User, User, User, User, User, User, User, User),
    arity11: (User, User, User, User, User, User, User, User, User, User, User),
    arity12: (User, User, User, User, User, User, User, User, User, User, User, User),
    arity13: (User, User, User, User, User, User, User, User, User, User, User, User, User),
    arity14: (User, User, User, User, User, User, User, User, User, User, User, User, User, User),
    arity15: (User, User, User, User, User, User, User, User, User, User, User, User, User, User, User),
    arity16: (User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User),
    arity17: (User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User),
    arity18: (User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User),
    arity19: (User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User),
    arity20: (User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User),
    arity21: (User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User),
    arity22: (User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User, User)
  )
  //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

  object TupleArities {
    implicit lazy val schema: Schema[TupleArities] = DeriveSchema.gen[TupleArities]
  }

  case class DependsOnA(a: Option[DependsOnB])

  object DependsOnA {
    implicit lazy val schema: Schema[DependsOnA] = DeriveSchema.gen[DependsOnA]
  }
  case class DependsOnB(b: Option[DependsOnA])

  object DependsOnB {
    implicit lazy val schema: Schema[DependsOnB] = DeriveSchema.gen[DependsOnB]
  }

  sealed trait Tree[+A]

  object Tree {
    case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
    case class Leaf[A](value: A)                        extends Tree[A]
    case object Root                                    extends Tree[Nothing]
  }

  sealed trait RBTree[+A, +B]

  object RBTree {
    case class Branch[A, B](left: RBTree[A, B], right: RBTree[A, B]) extends RBTree[A, B]
    case class RLeaf[A](value: A)                                    extends RBTree[A, Nothing]
    case class BLeaf[B](value: B)                                    extends RBTree[Nothing, B]
  }

  @annotation1("enum") sealed trait AnnotatedEnum

  object AnnotatedEnum {
    @annotation2("case") case class AnnotatedCase(field: String) extends AnnotatedEnum

    object AnnotatedCase {
      implicit val schema: Schema.CaseClass1[String, AnnotatedCase] = DeriveSchema.gen[AnnotatedCase]
    }

    implicit val schema: Schema.Enum1[AnnotatedCase, AnnotatedEnum] = DeriveSchema.gen[AnnotatedEnum]
  }

  @annotation1("enum") sealed trait RecursiveEnum
  @annotation2("case") case class RecursiveEnum1(label: String, rs: List[RecursiveEnum]) extends RecursiveEnum
  case object RecursiveEnum2                                                             extends RecursiveEnum

  object RecursiveEnum {
    implicit lazy val schema: Schema[RecursiveEnum] = DeriveSchema.gen[RecursiveEnum]
  }

  @annotation1("enum") sealed trait Enum23

  object Enum23 {
    @annotation2("case") case object C1 extends Enum23
    case object C2                      extends Enum23
    case object C3                      extends Enum23
    case object C4                      extends Enum23
    case object C5                      extends Enum23
    case object C6                      extends Enum23
    case object C7                      extends Enum23
    case object C8                      extends Enum23
    case object C9                      extends Enum23
    case object C10                     extends Enum23
    case object C11                     extends Enum23
    case object C12                     extends Enum23
    case object C13                     extends Enum23
    case object C14                     extends Enum23
    case object C15                     extends Enum23
    case object C16                     extends Enum23
    case object C17                     extends Enum23
    case object C18                     extends Enum23
    case object C19                     extends Enum23
    case object C20                     extends Enum23
    case object C21                     extends Enum23
    case object C22                     extends Enum23
    case class C23(recursive: Enum23)   extends Enum23

    implicit lazy val schema: Schema.EnumN[Enum23, CaseSet.Aux[Enum23]] = DeriveSchema.gen[Enum23]

  }

  override def spec: ZSpec[Environment, Failure] = suite("DeriveSchemaSpec")(
    suite("Derivation")(
      test("correctly derives case class") {
        assert(Schema[User].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derives case class with arity > 22") {
        assert(Schema[Arity24].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derives recursive data structure") {
        assert(Schema[Recursive].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derives tuple arities from 2 to 22") {
        assert(Schema[TupleArities].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derive mutually recursive data structure") {
        val c = Cyclic(1, CyclicChild1(2, CyclicChild2("3", None)))
        val _ = Schema[Cyclic].toDynamic(c)
        assert(Schema[Cyclic].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derives recursively for case class") {
        val derived: Schema[UserId] = DeriveSchema.gen[UserId]
        val expected: Schema[UserId] =
          Schema.CaseClass1(
            field = Schema.Field("id", Schema.Primitive(StandardType.StringType)),
            UserId.apply,
            (uid: UserId) => uid.id
          )

        assert(derived)(hasSameSchema(expected))
      },
      test("correctly derives for case object") {
        val derived: Schema[Singleton.type]  = DeriveSchema.gen[Singleton.type]
        val expected: Schema[Singleton.type] = Schema.singleton(Singleton)

        assert(derived)(hasSameSchema(expected))
      },
      test("correctly captures annotations on case class") {
        val derived: Schema[User] = Schema[User]
        val expected: Schema[User] = {
          Schema.CaseClass2(
            field1 = Schema.Field("name", Schema.Primitive(StandardType.StringType)),
            field2 = Schema.Field(
              "id",
              Schema.CaseClass1(
                field = Schema.Field("id", Schema.Primitive(StandardType.StringType)),
                UserId.apply,
                (uid: UserId) => uid.id
              ),
              Chunk(annotation1("foo"), annotation2("bar"), new annotation3, new annotation4(0))
            ),
            User.apply,
            (u: User) => u.name,
            (u: User) => u.id,
            annotations = Chunk(new annotation3)
          )
        }
        assert(derived)(hasSameSchema(expected))
      },
      test("correctly derives Enum") {
        val derived: Schema[Status] = Schema[Status]
        val expected: Schema[Status] =
          Schema.Enum3(
            Schema.Case("Failed", DeriveSchema.gen[Status.Failed], (s: Status) => s.asInstanceOf[Status.Failed]),
            Schema.Case("Ok", DeriveSchema.gen[Status.Ok], (s: Status) => s.asInstanceOf[Status.Ok]),
            Schema.Case(
              "Pending",
              DeriveSchema.gen[Status.Pending.type],
              (s: Status) => s.asInstanceOf[Status.Pending.type]
            )
          )

        assert(derived)(hasSameSchema(expected))
      },
      test("correctly capture annotations on Enum and cases") {
        val derived: Schema.Enum1[AnnotatedEnum.AnnotatedCase, AnnotatedEnum] = AnnotatedEnum.schema
        assertTrue(
          derived.annotations == Chunk(annotation1("enum")) && derived.case1.annotations == Chunk(annotation2("case"))
        )
      },
      test("correctly derives mutually recursive case classes") {
        val a  = DependsOnA(Some(DependsOnB(None)))
        val a0 = Schema[DependsOnA].fromDynamic(Schema[DependsOnA].toDynamic(a))
        assert(Schema[DependsOnA])(anything)
        assert(a0)(isRight(equalTo(a)))

        val b  = DependsOnB(Some(DependsOnA(None)))
        val b0 = Schema[DependsOnB].fromDynamic(Schema[DependsOnB].toDynamic(b))
        assert(Schema[DependsOnB])(anything)
        assert(b0)(isRight(equalTo(b)))
      },
      test("correctly derives recursive Enum with type parameters") {
        assert(DeriveSchema.gen[Tree[Recursive]])(anything)
      },
      test("correctly derives recursive Enum with multiple type parameters") {
        assert(DeriveSchema.gen[RBTree[String, Int]])(anything)
      },
      test("correctly derives recursive Enum") {
        assert(Schema[RecursiveEnum].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derives Enum with > 22 cases") {
        assert(Schema[Enum23].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      }
    )
  )
}
