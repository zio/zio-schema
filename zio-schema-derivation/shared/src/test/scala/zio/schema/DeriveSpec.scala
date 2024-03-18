package zio.schema

import scala.annotation.nowarn
import scala.reflect.ClassTag

import zio.schema.Deriver.WrappedF
import zio.schema.Schema.Field
import zio.schema.annotation.fieldDefaultValue
import zio.test.{ Spec, TestEnvironment, ZIOSpecDefault, assertTrue }
import zio.{ Chunk, Scope }

@nowarn object DeriveSpec extends ZIOSpecDefault with VersionSpecificDeriveSpec {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Derive")(
      suite("case object")(
        test("can derive new instance for case object") {
          implicit val schema: Schema[CaseObject1.type] = DeriveSchema.gen[CaseObject1.type]
          val tc                                        = Derive.derive[TC, CaseObject1.type](deriver)
          assertTrue(tc.isDerived == true)
        },
        test("can use existing instance for case object") {
          val tc = Derive.derive[TC, CaseObject2Wrapper](deriver)
          assertTrue(tc.isDerived == true, tc.inner.map(_.isDerived).contains(false))
        }
      ),
      suite("case class")(
        test("can derive new instance for simple case class") {
          val tc = Derive.derive[TC, Record1](deriver)
          assertTrue(tc.isDerived == true)
        },
        test("can use existing instance for simple case class") {
          val tc = Derive.derive[TC, Record2Wrapper](deriver)
          assertTrue(tc.isDerived == true, tc.inner.map(_.isDerived).contains(false))
        },
        test("can derive new instance for case class referring another one using an existing instance") {
          val tc = Derive.derive[TC, Record3](deriver)
          assertTrue(
            tc.isDerived == true,
            tc.inner.flatMap(_.inner).exists(_.isDerived == false)
          )
        },
        test("can derive new instance for case class containing sequences") {
          val tc = Derive.derive[TC, Record4](deriver)
          assertTrue(
            tc.isDerived == true,
            tc.inner.flatMap(_.inner).exists(_.isDerived == false)
          )
        },
        test("can derive new instance for case class containing a set") {
          val tc = Derive.derive[TC, Record5](deriver)
          assertTrue(
            tc.isDerived == true,
            tc.inner.flatMap(_.inner).exists(_.isDerived == true)
          )
        },
        test("can derive new instance for case class containing a map and an either") {
          val tc = Derive.derive[TC, Record6](deriver)
          assertTrue(
            tc.isDerived == true,
            tc.inner.flatMap(_.inner).exists(_.isDerived == true)
          )
        },
        test("can derive new instance for case class containing a 3-tuple field") {
          val tc = Derive.derive[TC, Record7](deriver)
          assertTrue(
            tc.isDerived == true
          )
        },
        test("works with with arity > 22") {
          val tc = Derive.derive[TC, Record8](deriver)
          assertTrue(
            tc.isDerived == true
          )
        },
        test("new top level implicit value is not passed as summoned") {
          assertTrue(
            ImplicitDeriveCheck.tc1 ne null,
            ImplicitDeriveCheck.tc1.isDerived == true,
            ImplicitDeriveCheck.tc7 ne null,
            ImplicitDeriveCheck.tc7.isDerived == true,
            ImplicitDeriveCheck.tc7.inner.flatMap(_.inner).exists(_ ne null) == true
          )
        }
      ),
      suite("enum")(
        test("can derive instance for enum") {
          val tc = Derive.derive[TC, Enum1](deriver)
          assertTrue(tc.isDerived == true)
        },
        test("can derive instance for recursive enum") {
          val tc = Derive.derive[TC3, Enum2](recursiveDeriver)
          assertTrue(
            tc.inner.isDefined,
            tc.inner.flatMap(_.inner).isDefined,
            tc.inner.flatMap(_.inner).flatMap(_.inner).isDefined,
            tc.inner.flatMap(_.inner).flatMap(_.inner).flatMap(_.inner).isDefined
          )
        }
      ),
      suite("caching") {
        test("reuses derived instances") {
          val cachedDeriver = deriver.cached
          val tc1           = Derive.derive[TC, Record1](cachedDeriver)
          val tc2           = Derive.derive[TC, Record2](cachedDeriver)
          val tc3           = Derive.derive[TC, Record3](cachedDeriver)
          val tc4           = Derive.derive[TC, Record4](cachedDeriver)
          val tc5           = Derive.derive[TC, Record5](cachedDeriver)
          val tc6           = Derive.derive[TC, Record6](cachedDeriver)
          val tc7           = Derive.derive[TC, Record7](cachedDeriver)
          val tc8           = Derive.derive[TC, Record8](cachedDeriver)

          val eq1 = tc7.inner.get.inner.get eq tc1
          assertTrue(
            eq1, // (pair._1)
            cachedDeriver.asInstanceOf[CachedDeriver[TC]].cache.size == 22,
            tc2 ne null,
            tc3 ne null,
            tc4 ne null,
            tc5 ne null,
            tc6 ne null,
            tc8 ne null
          )
        }
      },
      suite("default implementation") {
        test("default tupleN implementation creates proper record schema") {
          val capturedSchema = Derive.derive[CapturedSchema, RecordWithBigTuple](schemaCapturer)
          val tupleSchema    = capturedSchema.inner.map(_.schema)
          val isARecord      = tupleSchema.get.isInstanceOf[Schema.Record[_]]
          def record: Schema.Record[(String, Int, Double, Record1, Record2, Record3)] =
            tupleSchema.get.asInstanceOf[Schema.Record[(String, Int, Double, Record1, Record2, Record3)]]

          val ex1 = ("test", 1, 100.0, Record1("x", -5), Record2("y", 200), Record3(None))
          val ex2 = record
            .fields(2)
            .asInstanceOf[Field[(String, Int, Double, Record1, Record2, Record3), Double]]
            .set(ex1, 1000.0)
          val r3: Record3 =
            record.fields(5).asInstanceOf[Field[(String, Int, Double, Record1, Record2, Record3), Record3]].get(ex1)

          assertTrue(
            isARecord,
            record.fields.size == 6,
            ex2 == (("test", 1, 1000.0, Record1("x", -5), Record2("y", 200), Record3(None))),
            r3 == Record3(None)
          )
        }
      },
      suite("support for unknown types")(
        test("can pick up existing instance for unknown type") {
          implicit val openTraitTC: TC[OpenTrait] = new TC[OpenTrait] {
            override def isDerived: Boolean   = false
            override def inner: Option[TC[_]] = None
          }
          val tc        = Derive.derive[TC, UnsupportedField1](deriver)
          val refEquals = tc.inner.get eq openTraitTC
          assertTrue(refEquals)
        },
        test("calls deriveUnknown for unknown type") {
          val tc        = Derive.derive[TC, UnsupportedField1](deriver)
          val refEquals = tc.inner.get eq defaultOpenTraitTC
          assertTrue(refEquals)
        }
      ),
      suite("default field values")(
        test("use case class default values") {
          val capturedSchema = Derive.derive[CapturedSchema, RecordWithDefaultValue](schemaCapturer)
          val annotations = capturedSchema.schema
            .asInstanceOf[Schema.Record[RecordWithDefaultValue]]
            .fields(0)
            .annotations
          assertTrue(
            annotations.exists { a =>
              a.isInstanceOf[fieldDefaultValue[_]] &&
              a.asInstanceOf[fieldDefaultValue[Int]].value == 42
            },
            capturedSchema.schema.defaultValue == Right(RecordWithDefaultValue(42, 52))
          )
        },
        test("use case class default values of generic class") {
          val capturedSchema = Derive.derive[CapturedSchema, GenericRecordWithDefaultValue[Int]](schemaCapturer)
          val annotations = capturedSchema.schema
            .asInstanceOf[Schema.Record[GenericRecordWithDefaultValue[Int]]]
            .fields(0)
            .annotations
          assertTrue(
            annotations.exists { a =>
              a.isInstanceOf[fieldDefaultValue[_]] &&
              a.asInstanceOf[fieldDefaultValue[Option[Int]]].value == None
            },
            capturedSchema.schema.defaultValue == Right(GenericRecordWithDefaultValue[Int](None, 52))
          )
        },
        test("prefer field annotations over case class default values") {
          val capturedSchema = Derive.derive[CapturedSchema, RecordWithDefaultValue](schemaCapturer)
          val annotations = capturedSchema.schema
            .asInstanceOf[Schema.Record[RecordWithDefaultValue]]
            .fields(1)
            .annotations
          assertTrue(
            annotations
              .exists(a => a.isInstanceOf[fieldDefaultValue[_]] && a.asInstanceOf[fieldDefaultValue[Int]].value == 52),
            capturedSchema.schema.defaultValue == Right(RecordWithDefaultValue(42, 52))
          )
        }
      ),
      suite("support factory")(
        test("factory") {
          import zio.schema.Factory
          import zio.schema.Factory._
          def createSomeTrait[A: Factory](deriver: Deriver[TC])(implicit schema: Schema[A]): TC[A] =
            implicitly[Factory[A]].derive[TC](deriver)

          implicit val im: Factory[Enum1] = factory[Enum1]

          val tc = createSomeTrait[Enum1](deriver)
          assertTrue(tc.isDerived == true)
        }
      ),
      versionSpecificSuite
    )

  trait TC[A] {
    def isDerived: Boolean
    def inner: Option[TC[_]]
  }

  object CaseObject1

  object CaseObject2
  implicit val co2TC: TC[CaseObject2.type] = new TC[CaseObject2.type] {
    override def isDerived: Boolean   = false
    override def inner: Option[TC[_]] = None
  }

  case class CaseObject2Wrapper(obj: CaseObject2.type)

  object CaseObject2Wrapper {
    implicit val schema: Schema[CaseObject2Wrapper] = DeriveSchema.gen[CaseObject2Wrapper]
  }

  case class Record1(a: String, b: Int)

  object Record1 {
    implicit val schema: Schema[Record1] = DeriveSchema.gen[Record1]
  }

  case class Record2(a: String, b: Int)

  object Record2 {
    implicit val schema: Schema[Record2] = DeriveSchema.gen[Record2]
    implicit val tc: TC[Record2] = new TC[Record2] {
      override def isDerived: Boolean   = false
      override def inner: Option[TC[_]] = None
    }
  }

  case class Record2Wrapper(obj: Record2)

  object Record2Wrapper {
    implicit val schema: Schema[Record2Wrapper] = DeriveSchema.gen[Record2Wrapper]
  }

  case class Record3(r: Option[Record2])

  object Record3 {
    implicit val schema: Schema[Record3] = DeriveSchema.gen[Record3]
  }

  case class Record4(chunk: Chunk[Record2], vector: Vector[Record1])

  object Record4 {
    implicit val schema: Schema[Record4] = DeriveSchema.gen[Record4]
  }

  case class Record5(set: Set[Record4])

  object Record5 {
    implicit val schema: Schema[Record5] = DeriveSchema.gen[Record5]
  }

  case class Record6(map: Map[String, Record1], either: Either[String, Record2])

  object Record6 {
    implicit val schema: Schema[Record6] = DeriveSchema.gen[Record6]
  }

  case class Record7(pair: (Record1, Int), triple: (String, Enum1, Record2))

  object Record7 {
    implicit val schema: Schema[Record7] = DeriveSchema.gen[Record7]
  }

  case class Record8(
    t01: Int,
    t02: Int,
    t03: Int,
    t04: Int,
    t05: Int,
    t06: Int,
    t07: Int,
    t08: Int,
    t09: Int,
    t10: Int,
    t11: Int,
    t12: Int,
    t13: Int,
    t14: Int,
    t15: Int,
    t16: Int,
    t17: Int,
    t18: Int,
    t19: Int,
    t20: Int,
    t21: Int,
    t22: Int,
    t23: Int,
    t24: Int,
    t25: Int
  )

  object Record8 {
    implicit val schema: Schema[Record8] = DeriveSchema.gen[Record8]
  }

  case class RecordWithBigTuple(tuple: (String, Int, Double, Record1, Record2, Record3))

  object RecordWithBigTuple {
    implicit val schema: Schema[RecordWithBigTuple] = DeriveSchema.gen[RecordWithBigTuple]
  }

  case class RecordWithDefaultValue(int: Int = 42, @fieldDefaultValue(52) int2: Int = 42)

  object RecordWithDefaultValue {
    implicit val schema: Schema[RecordWithDefaultValue] = DeriveSchema.gen[RecordWithDefaultValue]
  }

  case class GenericRecordWithDefaultValue[T](int: Option[T] = None, @fieldDefaultValue(52) int2: Int = 42)

  object GenericRecordWithDefaultValue {
    //explicitly Int, because generic implicit definition leads to "Schema derivation exceeded" error
    implicit def schema: Schema[GenericRecordWithDefaultValue[Int]] =
      DeriveSchema.gen[GenericRecordWithDefaultValue[Int]]
  }

  sealed trait Enum1

  object Enum1 {
    case object Enum1A                      extends Enum1
    final case class Enum1B(value: Record2) extends Enum1
    final case class Enum1C(id: Int)        extends Enum1

    object Enum1C {
      implicit val tc: TC[Enum1C] = new TC[Enum1C] {
        override def isDerived: Boolean   = false
        override def inner: Option[TC[_]] = None
      }
    }

    implicit val schema: Schema[Enum1] = DeriveSchema.gen[Enum1]
  }

  sealed trait Enum2

  object Enum2 {
    final case class Next(value: Enum2) extends Enum2
    case object Stop                    extends Enum2

    implicit val schema: Schema[Enum2] = DeriveSchema.gen[Enum2]
  }

  trait ImplicitDeriveCheckBase {
    implicit val tc1: TC[Record1]
    implicit val tc7: TC[Record7]
  }

  object ImplicitDeriveCheck extends ImplicitDeriveCheckBase {
    val d: Deriver[TC]                     = deriver.autoAcceptSummoned
    implicit override val tc1: TC[Record1] = Derive.derive[TC, Record1](d)
    implicit override val tc7: TC[Record7] = Derive.derive[TC, Record7](d)
  }

  trait OpenTrait

  final case class UnsupportedField1(field: OpenTrait)

  @SuppressWarnings(Array("all"))
  object UnsupportedField1 {
    implicit private val openTraitPlaceholderSchema: Schema[OpenTrait] = Schema.fail("OpenTrait")

    implicit val schema: Schema[UnsupportedField1] = DeriveSchema.gen[UnsupportedField1]
  }

  private val defaultOpenTraitTC: TC[UnsupportedField1] = new TC[UnsupportedField1] {
    override def isDerived: Boolean   = true
    override def inner: Option[TC[_]] = None
  }

  val deriver: Deriver[TC] = new Deriver[TC] {
    override def deriveRecord[A](
      record: Schema.Record[A],
      fields: => Chunk[WrappedF[TC, _]],
      summoned: => Option[TC[A]]
    ): TC[A] =
      summoned.getOrElse {
        assert(fields.forall(_.unwrap ne null)) // force evaluation
        new TC[A] {
          override def isDerived: Boolean   = true
          override def inner: Option[TC[_]] = fields.headOption.map(_.unwrap)
        }
      }

    override def deriveTransformedRecord[A, B](
      record: Schema.Record[A],
      transform: Schema.Transform[A, B, _],
      fields: => Chunk[WrappedF[TC, _]],
      summoned: => Option[TC[B]]
    ): TC[B] =
      summoned.getOrElse {
        assert(fields.forall(_.unwrap ne null)) // force evaluation
        new TC[B] {
          override def isDerived: Boolean = true

          override def inner: Option[TC[_]] = fields.headOption.map(_.unwrap)
        }
      }

    override def deriveEnum[A](
      `enum`: Schema.Enum[A],
      cases: => Chunk[WrappedF[TC, _]],
      summoned: => Option[TC[A]]
    ): TC[A] =
      summoned.getOrElse {
        assert(cases.forall(_.unwrap ne null)) // force evaluation
        new TC[A] {
          override def isDerived: Boolean   = true
          override def inner: Option[TC[_]] = cases.headOption.map(_.unwrap)
        }
      }

    override def derivePrimitive[A](st: StandardType[A], summoned: => Option[TC[A]]): TC[A] =
      summoned.getOrElse {
        new TC[A] {
          override def isDerived: Boolean   = true
          override def inner: Option[TC[_]] = None
        }
      }

    override def deriveOption[A](
      option: Schema.Optional[A],
      innerTC: => TC[A],
      summoned: => Option[TC[Option[A]]]
    ): TC[Option[A]] =
      summoned.getOrElse {
        assert(innerTC ne null) // force evaluation
        new TC[Option[A]] {
          override def isDerived: Boolean   = true
          override def inner: Option[TC[_]] = Some(innerTC)
        }
      }

    override def deriveSequence[C[_], A](
      sequence: Schema.Sequence[C[A], A, _],
      innerTC: => TC[A],
      summoned: => Option[TC[C[A]]]
    ): TC[C[A]] =
      summoned.getOrElse {
        assert(innerTC ne null) // force evaluation
        new TC[C[A]] {
          override def isDerived: Boolean   = true
          override def inner: Option[TC[_]] = Some(innerTC)
        }
      }

    override def deriveSet[A](set: Schema.Set[A], innerTC: => TC[A], summoned: => Option[TC[Set[A]]]): TC[Set[A]] =
      summoned.getOrElse {
        assert(innerTC ne null) // force evaluation
        new TC[Set[A]] {
          override def isDerived: Boolean   = true
          override def inner: Option[TC[_]] = Some(innerTC)
        }
      }

    override def deriveMap[K, V](
      map: Schema.Map[K, V],
      key: => TC[K],
      value: => TC[V],
      summoned: => Option[TC[Map[K, V]]]
    ): TC[Map[K, V]] =
      summoned.getOrElse {
        assert(key ne null)
        assert(value ne null) // force evaluation
        new TC[Map[K, V]] {
          override def isDerived: Boolean   = true
          override def inner: Option[TC[_]] = Some(key)
        }
      }

    override def deriveEither[A, B](
      either: Schema.Either[A, B],
      left: => TC[A],
      right: => TC[B],
      summoned: => Option[TC[Either[A, B]]]
    ): TC[Either[A, B]] =
      summoned.getOrElse {
        assert(left ne null)
        assert(right ne null)
        new TC[Either[A, B]] {
          override def isDerived: Boolean   = true
          override def inner: Option[TC[_]] = Some(right)
        }
      }

    override def deriveTupleN[T](
      schemasAndInstances: => Chunk[(Schema[_], WrappedF[TC, _])],
      summoned: => Option[TC[T]]
    ): TC[T] =
      summoned.getOrElse {
        assert(schemasAndInstances.map(_._2).forall(_.unwrap ne null)) // force evaluation

        new TC[T] {
          override def isDerived: Boolean = true

          override def inner: Option[TC[_]] = schemasAndInstances.headOption.map(_._2.unwrap)
        }
      }

    override def deriveUnknown[A: ClassTag](summoned: => Option[TC[A]]): TC[A] =
      summoned.getOrElse {
        if (implicitly[ClassTag[A]] == implicitly[ClassTag[OpenTrait]]) {
          defaultOpenTraitTC.asInstanceOf[TC[A]]
        } else {
          new TC[A] {
            override def isDerived: Boolean   = true
            override def inner: Option[TC[_]] = None
          }
        }
      }
  }

  trait TC2[A] {
    def schema: Schema[A]
    def innerCount: Int
    def hadSummoned: Boolean
  }

  val simpleDeriver: Deriver[TC2] = new Deriver[TC2] {
    override def deriveRecord[A](
      record: Schema.Record[A],
      fields: => Chunk[WrappedF[TC2, _]],
      summoned: => Option[TC2[A]]
    ): TC2[A] =
      new TC2[A] {
        override def schema: Schema[A]    = record
        override def innerCount: Int      = fields.size
        override def hadSummoned: Boolean = summoned.isDefined
      }

    override def deriveEnum[A](
      `enum`: Schema.Enum[A],
      cases: => Chunk[WrappedF[TC2, _]],
      summoned: => Option[TC2[A]]
    ): TC2[A] =
      new TC2[A] {
        override def schema: Schema[A]    = `enum`
        override def innerCount: Int      = cases.size
        override def hadSummoned: Boolean = summoned.isDefined
      }

    override def derivePrimitive[A](st: StandardType[A], summoned: => Option[TC2[A]]): TC2[A] =
      new TC2[A] {
        override def schema: Schema[A]    = Schema.primitive(st)
        override def innerCount: Int      = 0
        override def hadSummoned: Boolean = summoned.isDefined
      }

    override def deriveOption[A](
      option: Schema.Optional[A],
      inner: => TC2[A],
      summoned: => Option[TC2[Option[A]]]
    ): TC2[Option[A]] =
      new TC2[Option[A]] {
        override def schema: Schema[Option[A]] = option
        override def innerCount: Int           = 1
        override def hadSummoned: Boolean      = summoned.isDefined
      }

    override def deriveSequence[C[_], A](
      sequence: Schema.Sequence[C[A], A, _],
      inner: => TC2[A],
      summoned: => Option[TC2[C[A]]]
    ): TC2[C[A]] =
      new TC2[C[A]] {
        override def schema: Schema[C[A]] = sequence
        override def innerCount: Int      = 1
        override def hadSummoned: Boolean = summoned.isDefined
      }

    override def deriveMap[K, V](
      map: Schema.Map[K, V],
      key: => TC2[K],
      value: => TC2[V],
      summoned: => Option[TC2[Map[K, V]]]
    ): TC2[Map[K, V]] =
      new TC2[Map[K, V]] {
        override def schema: Schema[Map[K, V]] = map
        override def innerCount: Int           = 2
        override def hadSummoned: Boolean      = summoned.isDefined
      }

    override def deriveTransformedRecord[A, B](
      record: Schema.Record[A],
      transform: Schema.Transform[A, B, _],
      fields: => Chunk[WrappedF[TC2, _]],
      summoned: => Option[TC2[B]]
    ): TC2[B] =
      new TC2[B] {
        override def schema: Schema[B]    = transform
        override def innerCount: Int      = fields.size
        override def hadSummoned: Boolean = summoned.isDefined
      }

  }

  // Example typeclass supporting Suspend to deal with recursive types
  sealed trait TC3[A] {
    def inner: Option[TC3[_]]
  }

  object TC3 {
    final case class Suspend[A](f: () => TC3[A]) extends TC3[A] {
      def inner: Option[TC3[_]] = f().inner
    }

    final case class Const[A](inner: Option[TC3[_]]) extends TC3[A]
  }

  val recursiveDeriver: Deriver[TC3] = new Deriver[TC3] {
    override def deriveRecord[A](
      record: Schema.Record[A],
      fields: => Chunk[WrappedF[TC3, _]],
      summoned: => Option[TC3[A]]
    ): TC3[A] =
      summoned.getOrElse {
        TC3.Suspend(() => TC3.Const(fields.headOption.map(_.unwrap)))
      }

    override def deriveEnum[A](
      `enum`: Schema.Enum[A],
      cases: => Chunk[WrappedF[TC3, _]],
      summoned: => Option[TC3[A]]
    ): TC3[A] =
      summoned.getOrElse {
        TC3.Suspend(() => TC3.Const(cases.headOption.map(_.unwrap)))
      }

    override def derivePrimitive[A](st: StandardType[A], summoned: => Option[TC3[A]]): TC3[A] =
      summoned.getOrElse {
        TC3.Const(None)
      }

    override def deriveOption[A](
      option: Schema.Optional[A],
      inner: => TC3[A],
      summoned: => Option[TC3[Option[A]]]
    ): TC3[Option[A]] =
      summoned.getOrElse {
        TC3.Suspend(() => TC3.Const(Some(inner)))
      }

    override def deriveSequence[C[_], A](
      sequence: Schema.Sequence[C[A], A, _],
      inner: => TC3[A],
      summoned: => Option[TC3[C[A]]]
    ): TC3[C[A]] =
      summoned.getOrElse {
        TC3.Suspend(() => TC3.Const(Some(inner)))
      }

    override def deriveMap[K, V](
      map: Schema.Map[K, V],
      key: => TC3[K],
      value: => TC3[V],
      summoned: => Option[TC3[Map[K, V]]]
    ): TC3[Map[K, V]] =
      summoned.getOrElse {
        TC3.Suspend(() => TC3.Const(Some(key)))
      }

    override def deriveTransformedRecord[A, B](
      record: Schema.Record[A],
      transform: Schema.Transform[A, B, _],
      fields: => Chunk[WrappedF[TC3, _]],
      summoned: => Option[TC3[B]]
    ): TC3[B] =
      summoned.getOrElse {
        TC3.Suspend(() => TC3.Const(fields.headOption.map(_.unwrap)))
      }
  }

  trait CapturedSchema[T] {
    def schema: Schema[T]
    def inner: Option[CapturedSchema[_]]
  }

  val schemaCapturer: Deriver[CapturedSchema] = new Deriver[CapturedSchema] {
    override def deriveRecord[A](
      record: Schema.Record[A],
      fields: => Chunk[WrappedF[CapturedSchema, _]],
      summoned: => Option[CapturedSchema[A]]
    ): CapturedSchema[A] =
      new CapturedSchema[A] {
        override def schema: Schema[A] = record
        override def inner: Option[CapturedSchema[_]] =
          fields.headOption.map(_.unwrap)
      }

    override def deriveEnum[A](
      `enum`: Schema.Enum[A],
      cases: => Chunk[WrappedF[CapturedSchema, _]],
      summoned: => Option[CapturedSchema[A]]
    ): CapturedSchema[A] =
      new CapturedSchema[A] {
        override def schema: Schema[A] = `enum`
        override def inner: Option[CapturedSchema[_]] =
          cases.headOption.map(_.unwrap)
      }

    override def derivePrimitive[A](st: StandardType[A], summoned: => Option[CapturedSchema[A]]): CapturedSchema[A] =
      new CapturedSchema[A] {
        override def schema: Schema[A]                = Schema.Primitive(st)
        override def inner: Option[CapturedSchema[_]] = None
      }

    override def deriveOption[A](
      option: Schema.Optional[A],
      innerCS: => CapturedSchema[A],
      summoned: => Option[CapturedSchema[Option[A]]]
    ): CapturedSchema[Option[A]] =
      new CapturedSchema[Option[A]] {
        override def schema: Schema[Option[A]]        = option
        override def inner: Option[CapturedSchema[_]] = Some(innerCS)
      }

    override def deriveSequence[C[_], A](
      sequence: Schema.Sequence[C[A], A, _],
      innerCS: => CapturedSchema[A],
      summoned: => Option[CapturedSchema[C[A]]]
    ): CapturedSchema[C[A]] =
      new CapturedSchema[C[A]] {
        override def schema: Schema[C[A]]             = sequence
        override def inner: Option[CapturedSchema[_]] = Some(innerCS)
      }

    override def deriveMap[K, V](
      map: Schema.Map[K, V],
      key: => CapturedSchema[K],
      value: => CapturedSchema[V],
      summoned: => Option[CapturedSchema[Map[K, V]]]
    ): CapturedSchema[Map[K, V]] =
      new CapturedSchema[Map[K, V]] {
        override def schema: Schema[Map[K, V]]        = map
        override def inner: Option[CapturedSchema[_]] = Some(key)
      }

    override def deriveTransformedRecord[A, B](
      record: Schema.Record[A],
      transform: Schema.Transform[A, B, _],
      fields: => Chunk[WrappedF[CapturedSchema, _]],
      summoned: => Option[CapturedSchema[B]]
    ): CapturedSchema[B] =
      new CapturedSchema[B] {
        override def schema: Schema[B] = transform
        override def inner: Option[CapturedSchema[_]] =
          fields.headOption.map(_.unwrap)
      }
  }
}
