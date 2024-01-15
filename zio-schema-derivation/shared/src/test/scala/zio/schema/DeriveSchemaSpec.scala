package zio.schema

import scala.annotation.Annotation

import zio.Chunk
import zio.schema.annotation.{ fieldName, optionalField, simpleEnum }
import zio.test._

object DeriveSchemaSpec extends ZIOSpecDefault with VersionSpecificDeriveSchemaSpec {
  import Assertion._
  import SchemaAssertions._

  final case class SimpleZero()
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

    implicit lazy val schema: Schema.Enum3[Ok, Failed, zio.schema.DeriveSchemaSpec.Status.Pending.type, Status] =
      DeriveSchema.gen[Status]
  }

  sealed trait OneOf

  object OneOf {
    case class StringValue(value: String)   extends OneOf
    case class IntValue(value: Int)         extends OneOf
    case class BooleanValue(value: Boolean) extends OneOf

    implicit lazy val schema: Schema.Enum3[StringValue, IntValue, BooleanValue, OneOf] = DeriveSchema.gen[OneOf]
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

  @annotation1("Arity24")
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

    implicit def schema[A: Schema]: Schema[Tree[A]] = DeriveSchema.gen[Tree[A]]
  }

  sealed trait RBTree[+A, +B]

  object RBTree {
    case class Branch[A, B](left: RBTree[A, B], right: RBTree[A, B]) extends RBTree[A, B]
    case class RLeaf[A](value: A)                                    extends RBTree[A, Nothing]
    case class BLeaf[B](value: B)                                    extends RBTree[Nothing, B]

    implicit def schema[A: Schema, B: Schema]: Schema[RBTree[A, B]] = DeriveSchema.gen[RBTree[A, B]]
  }

  sealed trait AdtWithTypeParameters[+Param1, +Param2]

  object AdtWithTypeParameters {
    case class A[Param1, Param2](fieldWithParam1: Param1) extends AdtWithTypeParameters[Param1, Param2]
    case class B[Param1, Param2](fieldWithParam2: Param2) extends AdtWithTypeParameters[Param1, Param2]
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

  case class RenamedField(@fieldName("renamed") name: String, @fieldName("number") num: Int)

  object RenamedField {
    implicit lazy val schema: Schema[RenamedField] = DeriveSchema.gen[RenamedField]
  }

  case class ContainsSchema(schema: Schema[User])

  object ContainsSchema {
    implicit val schema: Schema[ContainsSchema] = DeriveSchema.gen[ContainsSchema]
  }

  case class OptionalField(@optionalField name: String, age: Int)

  object OptionalField {
    implicit val schema: Schema[OptionalField] = DeriveSchema.gen[OptionalField]
  }

  @simpleEnum
  sealed trait SimpleEnum1
  case class SimpleClass1() extends SimpleEnum1

  sealed trait SimpleEnum2
  case class SimpleClass2() extends SimpleEnum2

  sealed abstract class AbstractBaseClass(val x: Int)
  final case class ConcreteClass1(override val x: Int, y: Int)    extends AbstractBaseClass(x)
  final case class ConcreteClass2(override val x: Int, s: String) extends AbstractBaseClass(x)

  sealed abstract class AbstractBaseClass2(val x: Int)
  sealed abstract class MiddleClass(override val x: Int, val y: Int)                   extends AbstractBaseClass2(x)
  final case class ConcreteClass3(override val x: Int, override val y: Int, s: String) extends MiddleClass(x, y)

  override def spec: Spec[Environment, Any] = suite("DeriveSchemaSpec")(
    suite("Derivation")(
      test("correctly derives case class 0") {
        val derived: Schema[SimpleZero] = DeriveSchema.gen[SimpleZero]
        val expected: Schema[SimpleZero] =
          Schema.CaseClass0(
            TypeId.parse("zio.schema.DeriveSchemaSpec.SimpleZero"),
            () => SimpleZero()
          )
        assert(derived)(hasSameSchema(expected))
      },
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
            TypeId.parse("zio.schema.DeriveSchemaSpec.UserId"),
            field0 = Schema.Field(
              "id",
              Schema.Primitive(StandardType.StringType),
              get0 = _.id,
              set0 = (a, b: String) => a.copy(id = b)
            ),
            UserId.apply
          )

        assert(derived)(hasSameSchema(expected))
      },
      test("correctly derives for case object") {
        val derived: Schema[Singleton.type] = DeriveSchema.gen[Singleton.type]
        val expected: Schema[Singleton.type] = Schema.CaseClass0(
          TypeId.fromTypeName("zio.schema.DeriveSchemaSpec.Singleton"),
          () => Singleton,
          Chunk.empty
        )

        assert(derived)(hasSameSchema(expected))
      },
      test("correctly captures annotations on case class") {
        val derived = DeriveSchema.gen[User]

        val expected: Schema[User] = {
          Schema.CaseClass2(
            TypeId.parse("zio.schema.DeriveSchemaSpec.User"),
            field01 = Schema.Field(
              "name",
              Schema.Primitive(StandardType.StringType),
              get0 = _.name,
              set0 = (a, b: String) => a.copy(name = b)
            ),
            field02 = Schema.Field(
              "id",
              Schema.CaseClass1(
                TypeId.parse("zio.schema.DeriveSchemaSpec.UserId"),
                field0 = Schema.Field(
                  "id",
                  Schema.Primitive(StandardType.StringType),
                  get0 = (uid: UserId) => uid.id,
                  set0 = (uid: UserId, id: String) => uid.copy(id = id)
                ),
                UserId.apply
              ),
              Chunk(annotation1("foo"), annotation2("bar"), new annotation3, new annotation4(0)),
              get0 = _.id,
              set0 = (a, b: UserId) => a.copy(id = b)
            ),
            User.apply,
            annotations0 = Chunk(new annotation3)
          )
        }
        assert(derived)(hasSameSchema(expected)) &&
        assert(verifyFieldName[derived.Field1]("name"))(isTrue) &&
        assert(verifyFieldName[derived.Field2]("id"))(isTrue)
      },
      test("correctly captures annotations on case class with arity greater than 22") {
        assertTrue(Schema[Arity24].annotations == Chunk(annotation1("Arity24")))
      },
      test("correctly derives Enum") {
        val derived: Schema[Status] = Schema[Status]
        val expected: Schema[Status] =
          Schema.Enum3(
            TypeId.parse("zio.schema.DeriveSchemaSpec.Status"),
            Schema.Case(
              "Ok",
              DeriveSchema.gen[Status.Ok],
              (s: Status) => s.asInstanceOf[Status.Ok],
              (ok: Status.Ok) => ok.asInstanceOf[Status],
              (s: Status) => s.isInstanceOf[Status.Ok]
            ),
            Schema.Case(
              "Failed",
              DeriveSchema.gen[Status.Failed],
              (s: Status) => s.asInstanceOf[Status.Failed],
              (ok: Status.Failed) => ok.asInstanceOf[Status],
              (s: Status) => s.isInstanceOf[Status.Failed]
            ),
            Schema.Case(
              "Pending",
              DeriveSchema.gen[Status.Pending.type],
              (s: Status) => s.asInstanceOf[Status.Pending.type],
              (p: Status.Pending.type) => p.asInstanceOf[Status],
              (s: Status) => s.isInstanceOf[Status.Pending.type]
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
        val derived: Schema[Tree[Recursive]] = DeriveSchema.gen[Tree[Recursive]]
        assert(derived)(anything)
      },
      test("correctly derives generic recursive Enum") {
        assert(Schema[Tree[Recursive]].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derives recursive Enum with multiple type parameters") {
        val derived: Schema[RBTree[String, Int]] = DeriveSchema.gen[RBTree[String, Int]]
        assert(derived)(anything)
      },
      test("correctly derives generic recursive Enum with multiple type parameters") {
        assert(Schema[RBTree[String, Int]].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derives schema with unused type parameters") {
        val derived: Schema[AdtWithTypeParameters[Int, Int]] = DeriveSchema.gen[AdtWithTypeParameters[Int, Int]]
        assert(derived)(anything)
      },
      test("correctly derives recursive Enum") {
        assert(Schema[RecursiveEnum].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derives Enum with > 22 cases") {
        assert(Schema[Enum23].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derives schema for case classes that use schema") {
        assert(Schema[ContainsSchema].toString)(not(containsString("null")) && not(equalTo("$Lazy$")))
      },
      test("correctly derives renaming field when fieldName annotation is present") {
        val derived = DeriveSchema.gen[RenamedField]

        val expected: Schema[RenamedField] = {
          Schema.CaseClass2(
            TypeId.parse("zio.schema.DeriveSchemaSpec.RenamedField"),
            field01 = Schema.Field(
              "renamed",
              Schema.Primitive(StandardType.StringType),
              Chunk(fieldName("renamed")),
              get0 = _.name,
              set0 = (a, b: String) => a.copy(name = b)
            ),
            field02 = Schema.Field(
              "number",
              Schema.Primitive(StandardType.IntType),
              Chunk(fieldName("number")),
              get0 = _.num,
              set0 = (a, b: Int) => a.copy(num = b)
            ),
            RenamedField.apply
          )
        }
        assert(derived)(hasSameSchema(expected)) &&
        assert(verifyFieldName[derived.Field1]("renamed"))(isTrue)
      },
      test("correctly derives optional fields when optional annotation is present") {
        val derived: Schema[OptionalField] = Schema[OptionalField]
        val expected: Schema[OptionalField] = {
          Schema.CaseClass2(
            TypeId.parse("zio.schema.DeriveSchemaSpec.OptionalField"),
            field01 = Schema.Field(
              "name",
              Schema.Primitive(StandardType.StringType),
              Chunk(optionalField()),
              get0 = _.name,
              set0 = (a, b: String) => a.copy(name = b)
            ),
            field02 = Schema.Field(
              "age",
              Schema.Primitive(StandardType.IntType),
              Chunk.empty,
              get0 = _.age,
              set0 = (a, b: Int) => a.copy(age = b)
            ),
            OptionalField.apply
          )
        }
        assert(derived)(hasSameSchema(expected))
      },
      test("correctly derives simpleEnum with annotation") {
        val derived = DeriveSchema.gen[SimpleEnum1]
        assertTrue(derived.annotations == Chunk(simpleEnum(false)))
      },
      test("correctly derives simpleEnum without annotation") {
        val derived = DeriveSchema.gen[SimpleEnum2]
        assertTrue(derived.annotations == Chunk(simpleEnum(true)))
      },
      test("correctly derives schema for abstract sealed class with case class subclasses") {
        val derived = DeriveSchema.gen[AbstractBaseClass]
        val expected: Schema[AbstractBaseClass] =
          Schema.Enum2(
            TypeId.parse("zio.schema.DeriveSchemaSpec.AbstractBaseClass"),
            Schema.Case(
              "ConcreteClass1",
              Schema.CaseClass2(
                TypeId.parse("zio.schema.DeriveSchemaSpec.ConcreteClass1"),
                field01 = Schema.Field[ConcreteClass1, Int](
                  "x",
                  Schema.Primitive(StandardType.IntType),
                  get0 = _.x,
                  set0 = (a, b: Int) => a.copy(x = b)
                ),
                field02 = Schema.Field[ConcreteClass1, Int](
                  "y",
                  Schema.Primitive(StandardType.IntType),
                  get0 = _.y,
                  set0 = (a, b: Int) => a.copy(y = b)
                ),
                ConcreteClass1.apply
              ),
              (a: AbstractBaseClass) => a.asInstanceOf[ConcreteClass1],
              (a: ConcreteClass1) => a.asInstanceOf[AbstractBaseClass],
              (a: AbstractBaseClass) => a.isInstanceOf[ConcreteClass1]
            ),
            Schema.Case(
              "ConcreteClass2",
              Schema.CaseClass2(
                TypeId.parse("zio.schema.DeriveSchemaSpec.ConcreteClass2"),
                field01 = Schema.Field[ConcreteClass2, Int](
                  "x",
                  Schema.Primitive(StandardType.IntType),
                  get0 = _.x,
                  set0 = (a, b: Int) => a.copy(x = b)
                ),
                field02 = Schema.Field[ConcreteClass2, String](
                  "s",
                  Schema.Primitive(StandardType.StringType),
                  get0 = _.s,
                  set0 = (a, b: String) => a.copy(s = b)
                ),
                ConcreteClass2.apply
              ),
              (a: AbstractBaseClass) => a.asInstanceOf[ConcreteClass2],
              (a: ConcreteClass2) => a.asInstanceOf[AbstractBaseClass],
              (a: AbstractBaseClass) => a.isInstanceOf[ConcreteClass2]
            ),
            Chunk.empty
          )
        assert(derived)(hasSameSchema(expected))
      },
      test(
        "correctly derives schema for abstract sealed class with intermediate subclasses, having case class leaf classes"
      ) {
        val derived = DeriveSchema.gen[AbstractBaseClass2]
        val expected: Schema[AbstractBaseClass2] =
          Schema.Enum1[MiddleClass, AbstractBaseClass2](
            TypeId.parse("zio.schema.DeriveSchemaSpec.AbstractBaseClass2"),
            Schema.Case[AbstractBaseClass2, MiddleClass](
              "MiddleClass",
              Schema.Enum1[ConcreteClass3, MiddleClass](
                TypeId.parse("zio.schema.DeriveSchemaSpec.MiddleClass"),
                Schema.Case[MiddleClass, ConcreteClass3](
                  "ConcreteClass3",
                  Schema.CaseClass3(
                    TypeId.parse("zio.schema.DeriveSchemaSpec.ConcreteClass3"),
                    field01 = Schema.Field[ConcreteClass3, Int](
                      "x",
                      Schema.Primitive(StandardType.IntType),
                      get0 = _.x,
                      set0 = (a, b: Int) => a.copy(x = b)
                    ),
                    field02 = Schema.Field[ConcreteClass3, Int](
                      "y",
                      Schema.Primitive(StandardType.IntType),
                      get0 = _.y,
                      set0 = (a, b: Int) => a.copy(y = b)
                    ),
                    field03 = Schema.Field[ConcreteClass3, String](
                      "s",
                      Schema.Primitive(StandardType.StringType),
                      get0 = _.s,
                      set0 = (a, b: String) => a.copy(s = b)
                    ),
                    ConcreteClass3.apply
                  ),
                  (a: MiddleClass) => a.asInstanceOf[ConcreteClass3],
                  (a: ConcreteClass3) => a.asInstanceOf[MiddleClass],
                  (a: MiddleClass) => a.isInstanceOf[ConcreteClass3],
                  Chunk.empty
                ),
                Chunk.empty
              ),
              (a: AbstractBaseClass2) => a.asInstanceOf[MiddleClass],
              (a: MiddleClass) => a.asInstanceOf[AbstractBaseClass2],
              (a: AbstractBaseClass2) => a.isInstanceOf[MiddleClass],
              Chunk.empty
            )
          )
        assert(derived)(hasSameSchema(expected))
      }
    ),
    versionSpecificSuite
  )
}
