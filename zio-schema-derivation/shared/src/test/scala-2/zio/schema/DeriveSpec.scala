package zio.schema

import zio.{ Chunk, Scope }
import zio.test.{ Spec, TestEnvironment, ZIOSpecDefault, assertTrue }

object DeriveSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Derive")(
      suite("case object")(
        test("can derive new instance for case object") {
          implicit val schema = DeriveSchema.gen[CaseObject1.type]
          val tc              = Derive.derive[TC, CaseObject1.type](deriver)
          assertTrue(tc.isDerived == true)
        },
        test("can use existing instance for case object") {
          implicit val schema = DeriveSchema.gen[CaseObject2.type]
          val tc              = Derive.derive[TC, CaseObject2.type](deriver)
          assertTrue(tc.isDerived == false)
        }
      ),
      suite("case class")(
        test("can derive new instance for simple case class") {
          val tc = Derive.derive[TC, Record1](deriver)
          assertTrue(tc.isDerived == true)
        },
        test("can use existing instance for simple case class") {
          val tc = Derive.derive[TC, Record2](deriver)
          assertTrue(tc.isDerived == false)
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
        }
      ),
      suite("enum")(
        test("can derive instance for enum") {
          val tc = Derive.derive[TC, Enum1](deriver)
          assertTrue(tc.isDerived == true)
        }
      )
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

  // TODO: recursive type
  // TODO: >22 fields

  val deriver: Deriver[TC] = new Deriver[TC] {
    override def deriveRecord[A](record: Schema.Record[A], fields: => Chunk[TC[_]], summoned: => Option[TC[A]]): TC[A] =
      summoned.getOrElse {
        assert(fields.forall(_ ne null)) // force evaluation
        new TC[A] {
          override def isDerived: Boolean   = true
          override def inner: Option[TC[_]] = fields.headOption
        }
      }

    override def deriveEnum[A](`enum`: Schema.Enum[A], cases: => Chunk[TC[_]], summoned: => Option[TC[A]]): TC[A] =
      summoned.getOrElse {
        assert(cases.forall(_ ne null)) // force evaluation
        new TC[A] {
          override def isDerived: Boolean   = true
          override def inner: Option[TC[_]] = cases.headOption
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

    override def deriveSet[A](set: Schema.Set[A], innerTC: => TC[A], summoned: Option[TC[Set[A]]]): TC[Set[A]] =
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
      summoned: Option[TC[Map[K, V]]]
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
      summoned: Option[TC[Either[A, B]]]
    ): TC[Either[A, B]] =
      summoned.getOrElse {
        assert(left ne null)
        assert(right ne null)
        new TC[Either[A, B]] {
          override def isDerived: Boolean   = true
          override def inner: Option[TC[_]] = Some(right)
        }
      }

    override def deriveTuple2[A, B](
      tuple: Schema.Tuple2[A, B],
      left: => TC[A],
      right: => TC[B],
      summoned: Option[TC[(A, B)]]
    ): TC[(A, B)] =
      summoned.getOrElse {
        assert(left ne null)
        assert(right ne null)
        new TC[(A, B)] {
          override def isDerived: Boolean = true

          override def inner: Option[TC[_]] = Some(left)
        }
      }

    override def deriveTuple3[A, B, C](
      tuple: Schema.Tuple2[Schema.Tuple2[A, B], C],
      t1: => TC[A],
      t2: => TC[B],
      t3: => TC[C],
      summoned: Option[TC[(A, B, C)]]
    ): TC[(A, B, C)] =
      summoned.getOrElse {
        assert(t1 ne null)
        assert(t2 ne null)
        assert(t3 ne null)
        new TC[(A, B, C)] {
          override def isDerived: Boolean = true

          override def inner: Option[TC[_]] = Some(t1)
        }
      }

    override def deriveTuple4[A, B, C, D](
      tuple: Schema.Tuple2[Schema.Tuple2[Schema.Tuple2[A, B], C], D],
      t1: => TC[A],
      t2: => TC[B],
      t3: => TC[C],
      t4: => TC[D],
      summoned: Option[TC[(A, B, C, D)]]
    ): TC[(A, B, C, D)] =
      summoned.getOrElse {
        assert(t1 ne null)
        assert(t2 ne null)
        assert(t3 ne null)
        assert(t4 ne null)
        new TC[(A, B, C, D)] {
          override def isDerived: Boolean = true

          override def inner: Option[TC[_]] = Some(t1)
        }
      }
  }
}
