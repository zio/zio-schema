package zio.schema

import java.time.temporal.ChronoUnit

import scala.collection.immutable.ListMap

import zio.Chunk

/**
 * A `Schema[A]` describes the structure of some data type `A`, in terms of case classes,
 * enumerations (sealed traits), collections, and various primitive types (including not only
 * Scala's own primitive types, but enhanced with java.time and big integers / decimals).
 *
 * Schemas models the structure of data types as first class values, so they can be introspected,
 * transformed, and combined using ordinary Scala code, without macros, metaprogramming, or codegen.
 *
 * There are implicit schemas provided for all standard Scala types, and you can automatically
 * derive schemas for your own data types by using `DeriveSchema.gen[A]`. Whether you write them
 * by hand by using constructors and operators,
 *
 * {{{
 * final case class Person(name: String, age: Int)
 * object Person {
 *   implicit val personSchema: Schema[Person] = DeriveSchema.gen[Person]
 * }
 * }}}
 */
sealed trait Schema[A] {
  self =>

  /**
   * A symbolic operator for [[optional]].
   */
  def ? : Schema[Option[A]] = self.optional

  /**
   * A symbolic operator for [[zip]].
   */
  def <*>[B](that: Schema[B]): Schema[(A, B)] = self.zip(that)

  /**
   * A symbolic operator for [[orElseEither]].
   */
  def <+>[B](that: Schema[B]): Schema[Either[A, B]] = self.orElseEither(that)

  /**
   * Performs a diff between thisValue and thatValue. See [[zio.schema.Differ]] for details
   * on the default diff algorithms.
   *
   * A custom [[zio.schema.Differ]] can be supplied if the default behavior is not acceptable.
   */
  def diff(thisValue: A, thatValue: A, differ: Option[Differ[A]] = None): Diff = differ match {
    case Some(differ) => differ(thisValue, thatValue)
    case None         => Differ.fromSchema(self)(thisValue, thatValue)
  }

  def fromDynamic(value: DynamicValue): Either[String, A] =
    value.toTypedValue(self)

  /**
   * Returns a new schema that modifies the type produced by this schema to be optional.
   */
  def optional: Schema[Option[A]] = Schema.Optional(self)

  /**
   * Returns a new schema that combines this schema and the specified schema together, modeling
   * their either composition.
   */
  def orElseEither[B](that: Schema[B]): Schema[Either[A, B]] = Schema.EitherSchema(self, that)

  def serializable: Schema[Schema[_]] = Schema.Meta(MetaSchema.fromSchema(self))

  def toDynamic(value: A): DynamicValue =
    DynamicValue.fromSchemaAndValue(self, value)

  /**
   * Transforms this `Schema[A]` into a `Schema[B]`, by supplying two functions that can transform
   * between `A` and `B`, without possibility of failure.
   */
  def transform[B](f: A => B, g: B => A): Schema[B] =
    Schema.Transform[A, B](self, a => Right(f(a)), b => Right(g(b)))

  /**
   * Transforms this `Schema[A]` into a `Schema[B]`, by supplying two functions that can transform
   * between `A` and `B` (possibly failing in some cases).
   */
  def transformOrFail[B](f: A => Either[String, B], g: B => Either[String, A]): Schema[B] =
    Schema.Transform[A, B](self, f, g)

  /**
   * Returns a new schema that combines this schema and the specified schema together, modeling
   * their tuple composition.
   */
  def zip[B](that: Schema[B]): Schema[(A, B)] = Schema.Tuple(self, that)
}

object Schema extends TupleSchemas with RecordSchemas with EnumSchemas {
  def apply[A](implicit schema: Schema[A]): Schema[A] = schema

  def defer[A](schema: => Schema[A]): Schema[A] = Lazy(() => schema)

  def enumeration(structure: ListMap[String, Schema[_]]): Schema[(String, _)] =
    Enumeration(structure)

  def fail[A](message: String): Schema[A] = Fail(message)

  def first[A](codec: Schema[(A, Unit)]): Schema[A] =
    codec.transform[A](_._1, a => (a, ()))

  def record(field: Field[_]*): Schema[ListMap[String, _]] =
    GenericRecord(Chunk.fromIterable(field))

  def second[A](codec: Schema[(Unit, A)]): Schema[A] =
    codec.transform[A](_._2, a => ((), a))

  def singleton[A](instance: A): Schema[A] = Schema[Unit].transform(_ => instance, _ => ())

  implicit val bigDecimal: Schema[BigDecimal] = primitive[java.math.BigDecimal].transform(BigDecimal(_), _.bigDecimal)

  implicit val bigInt: Schema[BigInt] = primitive[java.math.BigInteger].transform(BigInt(_), _.bigInteger)

  implicit val chronoUnit: Schema[ChronoUnit] = Schema[String].transformOrFail(
    {
      case "SECONDS"   => Right(ChronoUnit.SECONDS)
      case "CENTURIES" => Right(ChronoUnit.CENTURIES)
      case "DAYS"      => Right(ChronoUnit.DAYS)
      case "DECADES"   => Right(ChronoUnit.DECADES)
      case "FOREVER"   => Right(ChronoUnit.FOREVER)
      case "HOURS"     => Right(ChronoUnit.HOURS)
      case "MICROS"    => Right(ChronoUnit.MICROS)
      case "MILLIS"    => Right(ChronoUnit.MILLIS)
      case "MINUTES"   => Right(ChronoUnit.MINUTES)
      case "MONTHS"    => Right(ChronoUnit.MONTHS)
      case "NANOS"     => Right(ChronoUnit.NANOS)
      case "WEEKS"     => Right(ChronoUnit.WEEKS)
      case "YEARS"     => Right(ChronoUnit.YEARS)
      case _           => Left("Failed")
    }, {
      case ChronoUnit.SECONDS   => Right("SECONDS")
      case ChronoUnit.CENTURIES => Right("CENTURIES")
      case ChronoUnit.DAYS      => Right("DAYS")
      case ChronoUnit.DECADES   => Right("DECADES")
      case ChronoUnit.FOREVER   => Right("FOREVER")
      case ChronoUnit.HOURS     => Right("HOURS")
      case ChronoUnit.MICROS    => Right("MICROS")
      case ChronoUnit.MILLIS    => Right("MILLIS")
      case ChronoUnit.MINUTES   => Right("MINUTES")
      case ChronoUnit.MONTHS    => Right("MONTHS")
      case ChronoUnit.NANOS     => Right("NANOS")
      case ChronoUnit.WEEKS     => Right("WEEKS")
      case ChronoUnit.YEARS     => Right("YEARS")
      case _                    => Left("Failed")
    }
  )

  implicit val nil: Schema[Nil.type] = Schema[Unit].transform(_ => Nil, _ => ())

  implicit val none: Schema[None.type] = Schema[Unit].transform(_ => None, _ => ())

  implicit def chunk[A](implicit schemaA: Schema[A]): Schema[Chunk[A]] =
    Schema.Sequence(schemaA, identity, identity)

  implicit def either[A, B](left: Schema[A], right: Schema[B]): Schema[Either[A, B]] =
    EitherSchema(left, right)

  implicit def left[A, B](implicit schemaA: Schema[A]): Schema[Left[A, Nothing]] =
    schemaA.transform(Left(_), _.value)

  implicit def list[A](implicit schemaA: Schema[A]): Schema[List[A]] =
    Schema.Sequence(schemaA, _.toList, Chunk.fromIterable(_))

  implicit def option[A](implicit element: Schema[A]): Schema[Option[A]] =
    Optional(element)

  implicit def primitive[A](implicit standardType: StandardType[A]): Schema[A] =
    Primitive(standardType)

  implicit def right[A, B](implicit schemaB: Schema[B]): Schema[Right[Nothing, B]] =
    schemaB.transform(Right(_), _.value)

  implicit def set[A](implicit element: Schema[A]): Schema[Set[A]] =
    chunk(element).transform(_.toSet, Chunk.fromIterable(_))

  implicit def vector[A](implicit element: Schema[A]): Schema[Vector[A]] =
    chunk(element).transform(_.toVector, Chunk.fromIterable(_))

  sealed trait Enum[A] extends Schema[A] {
    def structure: ListMap[String, Schema[_]]
  }

  final case class Enumeration(override val structure: ListMap[String, Schema[_]]) extends Enum[(String, _)]

  sealed trait Record[R] extends Schema[R] {
    def structure: Chunk[Field[_]]
    def annotations: Chunk[Any] = Chunk.empty
    def rawConstruct(values: Chunk[Any]): Either[String, R]
  }

  final case class Sequence[Col[_], A](schemaA: Schema[A], fromChunk: Chunk[A] => Col[A], toChunk: Col[A] => Chunk[A])
      extends Schema[Col[A]]

  final case class Transform[A, B](codec: Schema[A], f: A => Either[String, B], g: B => Either[String, A])
      extends Schema[B] {
    override def serializable: Schema[Schema[_]] = Meta(MetaSchema.fromSchema(codec))
    override def toString: String                = s"Transform($codec)"
  }

  final case class Primitive[A](standardType: StandardType[A]) extends Schema[A]

  final case class Optional[A](codec: Schema[A]) extends Schema[Option[A]]

  final case class Fail[A](message: String) extends Schema[A]

  final case class Tuple[A, B](left: Schema[A], right: Schema[B]) extends Schema[(A, B)]

  final case class EitherSchema[A, B](left: Schema[A], right: Schema[B]) extends Schema[Either[A, B]]

  final case class Lazy[A](private val schema0: () => Schema[A]) extends Schema[A] {
    lazy val schema: Schema[A] = schema0()

    override def toString: String = s"Lazy($schema)"
  }

  final case class Meta(spec: MetaSchema) extends Schema[Schema[_]]
}

sealed trait EnumSchemas { self: Schema.type =>

  sealed case class Case[A <: Z, Z](id: String, codec: Schema[A], unsafeDeconstruct: Z => A) {

    def deconstruct(z: Z): Option[A] =
      try {
        Some(unsafeDeconstruct(z))
      } catch { case _: IllegalArgumentException => None }

    override def toString: String = s"Case($id,$codec)"
  }

  sealed case class Enum1[A <: Z, Z](case1: Case[A, Z]) extends Enum[Z] {
    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id -> case1.codec
      )
  }

  sealed case class Enum2[A1 <: Z, A2 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z]) extends Enum[Z] {
    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id -> case1.codec,
        case2.id -> case2.codec
      )
  }

  sealed case class Enum3[A1 <: Z, A2 <: Z, A3 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z])
      extends Enum[Z] {
    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id -> case1.codec,
        case2.id -> case2.codec,
        case3.id -> case3.codec
      )
  }
  sealed case class EnumN[Z](cases: Seq[Case[_ <: Z, Z]]) extends Enum[Z] {
    override def structure: ListMap[String, Schema[_]] =
      ListMap.empty ++ cases.map(c => c.id -> c.codec)
  }
}

//scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
sealed trait TupleSchemas {
  implicit def tuple2[A, B](implicit c1: Schema[A], c2: Schema[B]): Schema[(A, B)] =
    c1.zip(c2)

  implicit def tuple3[A, B, C](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C]): Schema[(A, B, C)] =
    c1.zip(c2).zip(c3).transform({ case ((a, b), c) => (a, b, c) }, { case (a, b, c) => ((a, b), c) })

  implicit def tuple4[A, B, C, D](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D]): Schema[(A, B, C, D)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .transform({ case (((a, b), c), d) => (a, b, c, d) }, { case (a, b, c, d) => (((a, b), c), d) })

  implicit def tuple5[A, B, C, D, E](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E]): Schema[(A, B, C, D, E)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .transform({ case ((((a, b), c), d), e) => (a, b, c, d, e) }, { case (a, b, c, d, e) => ((((a, b), c), d), e) })

  implicit def tuple6[A, B, C, D, E, F](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F]): Schema[(A, B, C, D, E, F)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .transform({ case (((((a, b), c), d), e), f) => (a, b, c, d, e, f) }, {
        case (a, b, c, d, e, f)                    => (((((a, b), c), d), e), f)
      })

  implicit def tuple7[A, B, C, D, E, F, G](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G]): Schema[(A, B, C, D, E, F, G)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .transform({ case ((((((a, b), c), d), e), f), g) => (a, b, c, d, e, f, g) }, {
        case (a, b, c, d, e, f, g)                      => ((((((a, b), c), d), e), f), g)
      })

  implicit def tuple8[A, B, C, D, E, F, G, H](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G], c8: Schema[H]): Schema[(A, B, C, D, E, F, G, H)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .transform({ case (((((((a, b), c), d), e), f), g), h) => (a, b, c, d, e, f, g, h) }, {
        case (a, b, c, d, e, f, g, h)                        => (((((((a, b), c), d), e), f), g), h)
      })

  implicit def tuple9[A, B, C, D, E, F, G, H, I](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G], c8: Schema[H], c9: Schema[I]): Schema[(A, B, C, D, E, F, G, H, I)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .transform({ case ((((((((a, b), c), d), e), f), g), h), i) => (a, b, c, d, e, f, g, h, i) }, {
        case (a, b, c, d, e, f, g, h, i)                          => ((((((((a, b), c), d), e), f), g), h), i)
      })

  implicit def tuple10[A, B, C, D, E, F, G, H, I, J](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G], c8: Schema[H], c9: Schema[I], c10: Schema[J]): Schema[(A, B, C, D, E, F, G, H, I, J)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .transform({ case (((((((((a, b), c), d), e), f), g), h), i), j) => (a, b, c, d, e, f, g, h, i, j) }, {
        case (a, b, c, d, e, f, g, h, i, j)                            => (((((((((a, b), c), d), e), f), g), h), i), j)
      })

  implicit def tuple11[A, B, C, D, E, F, G, H, I, J, K](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G], c8: Schema[H], c9: Schema[I], c10: Schema[J], c11: Schema[K]): Schema[(A, B, C, D, E, F, G, H, I, J, K)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .zip(c11)
      .transform({ case ((((((((((a, b), c), d), e), f), g), h), i), j), k) => (a, b, c, d, e, f, g, h, i, j, k) }, {
        case (a, b, c, d, e, f, g, h, i, j, k)                              => ((((((((((a, b), c), d), e), f), g), h), i), j), k)
      })

  implicit def tuple12[A, B, C, D, E, F, G, H, I, J, K, L](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G], c8: Schema[H], c9: Schema[I], c10: Schema[J], c11: Schema[K], c12: Schema[L]): Schema[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .zip(c11)
      .zip(c12)
      .transform({ case (((((((((((a, b), c), d), e), f), g), h), i), j), k), l) => (a, b, c, d, e, f, g, h, i, j, k, l) }, {
        case (a, b, c, d, e, f, g, h, i, j, k, l)                                => (((((((((((a, b), c), d), e), f), g), h), i), j), k), l)
      })

  implicit def tuple13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G], c8: Schema[H], c9: Schema[I], c10: Schema[J], c11: Schema[K], c12: Schema[L], c13: Schema[M]): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .zip(c11)
      .zip(c12)
      .zip(c13)
      .transform({
        case ((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m) => (a, b, c, d, e, f, g, h, i, j, k, l, m)
      }, {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m) => ((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m)
      })

  implicit def tuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G], c8: Schema[H], c9: Schema[I], c10: Schema[J], c11: Schema[K], c12: Schema[L], c13: Schema[M], c14: Schema[N]): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .zip(c11)
      .zip(c12)
      .zip(c13)
      .zip(c14)
      .transform({
        case (((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
      }, {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =>
          (((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n)
      })

  implicit def tuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G], c8: Schema[H], c9: Schema[I], c10: Schema[J], c11: Schema[K], c12: Schema[L], c13: Schema[M], c14: Schema[N], c15: Schema[O]): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .zip(c11)
      .zip(c12)
      .zip(c13)
      .zip(c14)
      .zip(c15)
      .transform({
        case ((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
      }, {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =>
          ((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o)
      })

  implicit def tuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G], c8: Schema[H], c9: Schema[I], c10: Schema[J], c11: Schema[K], c12: Schema[L], c13: Schema[M], c14: Schema[N], c15: Schema[O], c16: Schema[P]): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .zip(c11)
      .zip(c12)
      .zip(c13)
      .zip(c14)
      .zip(c15)
      .zip(c16)
      .transform({
        case (((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
      }, {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
          (((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p)
      })

  implicit def tuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G], c8: Schema[H], c9: Schema[I], c10: Schema[J], c11: Schema[K], c12: Schema[L], c13: Schema[M], c14: Schema[N], c15: Schema[O], c16: Schema[P], c17: Schema[Q])
    : Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .zip(c11)
      .zip(c12)
      .zip(c13)
      .zip(c14)
      .zip(c15)
      .zip(c16)
      .zip(c17)
      .transform({
        case ((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
      }, {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
          ((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q)
      })

  implicit def tuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G], c8: Schema[H], c9: Schema[I], c10: Schema[J], c11: Schema[K], c12: Schema[L], c13: Schema[M], c14: Schema[N], c15: Schema[O], c16: Schema[P], c17: Schema[Q], c18: Schema[R])
    : Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .zip(c11)
      .zip(c12)
      .zip(c13)
      .zip(c14)
      .zip(c15)
      .zip(c16)
      .zip(c17)
      .zip(c18)
      .transform({
        case (((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
      }, {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
          (((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r)
      })

  implicit def tuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C], c4: Schema[D], c5: Schema[E], c6: Schema[F], c7: Schema[G], c8: Schema[H], c9: Schema[I], c10: Schema[J], c11: Schema[K], c12: Schema[L], c13: Schema[M], c14: Schema[N], c15: Schema[O], c16: Schema[P], c17: Schema[Q], c18: Schema[R], c19: Schema[S])
    : Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .zip(c11)
      .zip(c12)
      .zip(c13)
      .zip(c14)
      .zip(c15)
      .zip(c16)
      .zip(c17)
      .zip(c18)
      .zip(c19)
      .transform({
        case ((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
      }, {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
          ((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s)
      })

  implicit def tuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D],
    c5: Schema[E],
    c6: Schema[F],
    c7: Schema[G],
    c8: Schema[H],
    c9: Schema[I],
    c10: Schema[J],
    c11: Schema[K],
    c12: Schema[L],
    c13: Schema[M],
    c14: Schema[N],
    c15: Schema[O],
    c16: Schema[P],
    c17: Schema[Q],
    c18: Schema[R],
    c19: Schema[S],
    c20: Schema[T]
  ): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .zip(c11)
      .zip(c12)
      .zip(c13)
      .zip(c14)
      .zip(c15)
      .zip(c16)
      .zip(c17)
      .zip(c18)
      .zip(c19)
      .zip(c20)
      .transform({
        case (((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
      }, {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
          (((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t)
      })

  implicit def tuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D],
    c5: Schema[E],
    c6: Schema[F],
    c7: Schema[G],
    c8: Schema[H],
    c9: Schema[I],
    c10: Schema[J],
    c11: Schema[K],
    c12: Schema[L],
    c13: Schema[M],
    c14: Schema[N],
    c15: Schema[O],
    c16: Schema[P],
    c17: Schema[Q],
    c18: Schema[R],
    c19: Schema[S],
    c20: Schema[T],
    c21: Schema[U]
  ): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .zip(c11)
      .zip(c12)
      .zip(c13)
      .zip(c14)
      .zip(c15)
      .zip(c16)
      .zip(c17)
      .zip(c18)
      .zip(c19)
      .zip(c20)
      .zip(c21)
      .transform({
        case ((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
      }, {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
          ((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u)
      })

  implicit def tuple22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D],
    c5: Schema[E],
    c6: Schema[F],
    c7: Schema[G],
    c8: Schema[H],
    c9: Schema[I],
    c10: Schema[J],
    c11: Schema[K],
    c12: Schema[L],
    c13: Schema[M],
    c14: Schema[N],
    c15: Schema[O],
    c16: Schema[P],
    c17: Schema[Q],
    c18: Schema[R],
    c19: Schema[S],
    c20: Schema[T],
    c21: Schema[U],
    c22: Schema[V]
  ): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .zip(c8)
      .zip(c9)
      .zip(c10)
      .zip(c11)
      .zip(c12)
      .zip(c13)
      .zip(c14)
      .zip(c15)
      .zip(c16)
      .zip(c17)
      .zip(c18)
      .zip(c19)
      .zip(c20)
      .zip(c21)
      .zip(c22)
      .transform({
        case (((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u), v) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
      }, {
        case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
          (((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u), v)
      })
}

//scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
sealed trait RecordSchemas { self: Schema.type =>

  sealed case class Field[A](label: String, schema: Schema[A], annotations: Chunk[Any] = Chunk.empty) {
    override def toString: String = s"Field($label,$schema)"
  }

  sealed case class GenericRecord(override val structure: Chunk[Field[_]]) extends Record[ListMap[String, _]] {
    override def rawConstruct(values: Chunk[Any]): Either[String, ListMap[String, _]] =
      if (values.size == structure.size)
        Right(ListMap(structure.map(_.label).zip(values): _*))
      else
        Left(s"wrong number of values for $structure")
  }

  sealed case class CaseClass1[A, Z](override val annotations: Chunk[Any] = Chunk.empty, field: Field[A], construct: A => Z, extractField: Z => A) extends Record[Z] { self =>
    override def structure: Chunk[Field[_]] = Chunk(field)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 1)
        try {
          Right(construct(values(0).asInstanceOf[A]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")
    override def toString: String = s"CaseClass1(${structure.mkString(",")})"
  }

  sealed case class CaseClass2[A1, A2, Z](override val annotations: Chunk[Any] = Chunk.empty, field1: Field[A1], field2: Field[A2], construct: (A1, A2) => Z, extractField1: Z => A1, extractField2: Z => A2) extends Record[Z] {
    override def structure: Chunk[Field[_]] = Chunk(field1, field2)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 2)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")
    override def toString: String = s"CaseClass2(${structure.mkString(",")})"

  }

  sealed case class CaseClass3[A1, A2, A3, Z](override val annotations: Chunk[Any] = Chunk.empty, field1: Field[A1], field2: Field[A2], field3: Field[A3], construct: (A1, A2, A3) => Z, extractField1: Z => A1, extractField2: Z => A2, extractField3: Z => A3) extends Record[Z] {
    override def structure: Chunk[Field[_]] = Chunk(field1, field2, field3)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 3)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass3(${structure.mkString(",")})"

  }

  sealed case class CaseClass4[A1, A2, A3, A4, Z](override val annotations: Chunk[Any] = Chunk.empty, field1: Field[A1], field2: Field[A2], field3: Field[A3], field4: Field[A4], construct: (A1, A2, A3, A4) => Z, extractField1: Z => A1, extractField2: Z => A2, extractField3: Z => A3, extractField4: Z => A4) extends Record[Z] {
    override def structure: Chunk[Field[_]] = Chunk(field1, field2, field3, field4)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 4)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3], values(3).asInstanceOf[A4]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass4(${structure.mkString(",")})"

  }

  sealed case class CaseClass5[A1, A2, A3, A4, A5, Z](override val annotations: Chunk[Any] = Chunk.empty, field1: Field[A1], field2: Field[A2], field3: Field[A3], field4: Field[A4], field5: Field[A5], construct: (A1, A2, A3, A4, A5) => Z, extractField1: Z => A1, extractField2: Z => A2, extractField3: Z => A3, extractField4: Z => A4, extractField5: Z => A5) extends Record[Z] {
    override def structure: Chunk[Field[_]] = Chunk(field1, field2, field3, field4, field5)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 5)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3], values(3).asInstanceOf[A4], values(4).asInstanceOf[A5]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass5(${structure.mkString(",")})"
  }

  sealed case class CaseClass6[A1, A2, A3, A4, A5, A6, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    construct: (A1, A2, A3, A4, A5, A6) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] = Chunk(field1, field2, field3, field4, field5, field6)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 6)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3], values(3).asInstanceOf[A4], values(4).asInstanceOf[A5], values(5).asInstanceOf[A6]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass6(${structure.mkString(",")})"

  }

  sealed case class CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    construct: (A1, A2, A3, A4, A5, A6, A7) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] = Chunk(field1, field2, field3, field4, field5, field6, field7)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 7)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3], values(3).asInstanceOf[A4], values(4).asInstanceOf[A5], values(5).asInstanceOf[A6], values(6).asInstanceOf[A7]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass7(${structure.mkString(",")})"

  }

  sealed case class CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] = Chunk(field1, field2, field3, field4, field5, field6, field7, field8)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 8)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3], values(3).asInstanceOf[A4], values(4).asInstanceOf[A5], values(5).asInstanceOf[A6], values(6).asInstanceOf[A7], values(7).asInstanceOf[A8]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass8(${structure.mkString(",")})"

  }

  sealed case class CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 9)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3], values(3).asInstanceOf[A4], values(4).asInstanceOf[A5], values(5).asInstanceOf[A6], values(6).asInstanceOf[A7], values(7).asInstanceOf[A8], values(8).asInstanceOf[A9]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass9(${structure.mkString(",")})"

  }

  sealed case class CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 10)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3], values(3).asInstanceOf[A4], values(4).asInstanceOf[A5], values(5).asInstanceOf[A6], values(6).asInstanceOf[A7], values(7).asInstanceOf[A8], values(8).asInstanceOf[A9], values(9).asInstanceOf[A10]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")
    override def toString: String = s"CaseClass10(${structure.mkString(",")})"

  }

  sealed case class CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    field11: Field[A11],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 11)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3], values(3).asInstanceOf[A4], values(4).asInstanceOf[A5], values(5).asInstanceOf[A6], values(6).asInstanceOf[A7], values(7).asInstanceOf[A8], values(8).asInstanceOf[A9], values(9).asInstanceOf[A10], values(10).asInstanceOf[A11]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")
    override def toString: String = s"CaseClass11(${structure.mkString(",")})"

  }

  sealed case class CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    field11: Field[A11],
    field12: Field[A12],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 12)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3], values(3).asInstanceOf[A4], values(4).asInstanceOf[A5], values(5).asInstanceOf[A6], values(6).asInstanceOf[A7], values(7).asInstanceOf[A8], values(8).asInstanceOf[A9], values(9).asInstanceOf[A10], values(10).asInstanceOf[A11], values(11).asInstanceOf[A12]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")
    override def toString: String = s"CaseClass12(${structure.mkString(",")})"

  }

  sealed case class CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    field11: Field[A11],
    field12: Field[A12],
    field13: Field[A13],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12,
    extractField13: Z => A13
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 13)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3], values(3).asInstanceOf[A4], values(4).asInstanceOf[A5], values(5).asInstanceOf[A6], values(6).asInstanceOf[A7], values(7).asInstanceOf[A8], values(8).asInstanceOf[A9], values(9).asInstanceOf[A10], values(10).asInstanceOf[A11], values(11).asInstanceOf[A12], values(12).asInstanceOf[A13]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")
    override def toString: String = s"CaseClass13(${structure.mkString(",")})"

  }

  sealed case class CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    field11: Field[A11],
    field12: Field[A12],
    field13: Field[A13],
    field14: Field[A14],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12,
    extractField13: Z => A13,
    extractField14: Z => A14
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 14)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4],
              values(4).asInstanceOf[A5],
              values(5).asInstanceOf[A6],
              values(6).asInstanceOf[A7],
              values(7).asInstanceOf[A8],
              values(8).asInstanceOf[A9],
              values(9).asInstanceOf[A10],
              values(10).asInstanceOf[A11],
              values(11).asInstanceOf[A12],
              values(12).asInstanceOf[A13],
              values(13).asInstanceOf[A14]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass14(${structure.mkString(",")})"

  }

  sealed case class CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    field11: Field[A11],
    field12: Field[A12],
    field13: Field[A13],
    field14: Field[A14],
    field15: Field[A15],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12,
    extractField13: Z => A13,
    extractField14: Z => A14,
    extractField15: Z => A15
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 15)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4],
              values(4).asInstanceOf[A5],
              values(5).asInstanceOf[A6],
              values(6).asInstanceOf[A7],
              values(7).asInstanceOf[A8],
              values(8).asInstanceOf[A9],
              values(9).asInstanceOf[A10],
              values(10).asInstanceOf[A11],
              values(11).asInstanceOf[A12],
              values(12).asInstanceOf[A13],
              values(13).asInstanceOf[A14],
              values(14).asInstanceOf[A15]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass15(${structure.mkString(",")})"

  }

  sealed case class CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    field11: Field[A11],
    field12: Field[A12],
    field13: Field[A13],
    field14: Field[A14],
    field15: Field[A15],
    field16: Field[A16],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12,
    extractField13: Z => A13,
    extractField14: Z => A14,
    extractField15: Z => A15,
    extractField16: Z => A16
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 16)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4],
              values(4).asInstanceOf[A5],
              values(5).asInstanceOf[A6],
              values(6).asInstanceOf[A7],
              values(7).asInstanceOf[A8],
              values(8).asInstanceOf[A9],
              values(9).asInstanceOf[A10],
              values(10).asInstanceOf[A11],
              values(11).asInstanceOf[A12],
              values(12).asInstanceOf[A13],
              values(13).asInstanceOf[A14],
              values(14).asInstanceOf[A15],
              values(15).asInstanceOf[A16]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass16(${structure.mkString(",")})"

  }

  sealed case class CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    field11: Field[A11],
    field12: Field[A12],
    field13: Field[A13],
    field14: Field[A14],
    field15: Field[A15],
    field16: Field[A16],
    field17: Field[A17],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12,
    extractField13: Z => A13,
    extractField14: Z => A14,
    extractField15: Z => A15,
    extractField16: Z => A16,
    extractField17: Z => A17
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 17)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4],
              values(4).asInstanceOf[A5],
              values(5).asInstanceOf[A6],
              values(6).asInstanceOf[A7],
              values(7).asInstanceOf[A8],
              values(8).asInstanceOf[A9],
              values(9).asInstanceOf[A10],
              values(10).asInstanceOf[A11],
              values(11).asInstanceOf[A12],
              values(12).asInstanceOf[A13],
              values(13).asInstanceOf[A14],
              values(14).asInstanceOf[A15],
              values(15).asInstanceOf[A16],
              values(16).asInstanceOf[A17]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass17(${structure.mkString(",")})"

  }

  sealed case class CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    field11: Field[A11],
    field12: Field[A12],
    field13: Field[A13],
    field14: Field[A14],
    field15: Field[A15],
    field16: Field[A16],
    field17: Field[A17],
    field18: Field[A18],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12,
    extractField13: Z => A13,
    extractField14: Z => A14,
    extractField15: Z => A15,
    extractField16: Z => A16,
    extractField17: Z => A17,
    extractField18: Z => A18
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 18)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4],
              values(4).asInstanceOf[A5],
              values(5).asInstanceOf[A6],
              values(6).asInstanceOf[A7],
              values(7).asInstanceOf[A8],
              values(8).asInstanceOf[A9],
              values(9).asInstanceOf[A10],
              values(10).asInstanceOf[A11],
              values(11).asInstanceOf[A12],
              values(12).asInstanceOf[A13],
              values(13).asInstanceOf[A14],
              values(14).asInstanceOf[A15],
              values(15).asInstanceOf[A16],
              values(16).asInstanceOf[A17],
              values(17).asInstanceOf[A18]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass18(${structure.mkString(",")})"

  }

  sealed case class CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    field11: Field[A11],
    field12: Field[A12],
    field13: Field[A13],
    field14: Field[A14],
    field15: Field[A15],
    field16: Field[A16],
    field17: Field[A17],
    field18: Field[A18],
    field19: Field[A19],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12,
    extractField13: Z => A13,
    extractField14: Z => A14,
    extractField15: Z => A15,
    extractField16: Z => A16,
    extractField17: Z => A17,
    extractField18: Z => A18,
    extractField19: Z => A19
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18, field19)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 19)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4],
              values(4).asInstanceOf[A5],
              values(5).asInstanceOf[A6],
              values(6).asInstanceOf[A7],
              values(7).asInstanceOf[A8],
              values(8).asInstanceOf[A9],
              values(9).asInstanceOf[A10],
              values(10).asInstanceOf[A11],
              values(11).asInstanceOf[A12],
              values(12).asInstanceOf[A13],
              values(13).asInstanceOf[A14],
              values(14).asInstanceOf[A15],
              values(15).asInstanceOf[A16],
              values(16).asInstanceOf[A17],
              values(17).asInstanceOf[A18],
              values(18).asInstanceOf[A19]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass19(${structure.mkString(",")})"

  }

  sealed case class CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    field11: Field[A11],
    field12: Field[A12],
    field13: Field[A13],
    field14: Field[A14],
    field15: Field[A15],
    field16: Field[A16],
    field17: Field[A17],
    field18: Field[A18],
    field19: Field[A19],
    field20: Field[A20],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12,
    extractField13: Z => A13,
    extractField14: Z => A14,
    extractField15: Z => A15,
    extractField16: Z => A16,
    extractField17: Z => A17,
    extractField18: Z => A18,
    extractField19: Z => A19,
    extractField20: Z => A20
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18, field19, field20)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 20)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4],
              values(4).asInstanceOf[A5],
              values(5).asInstanceOf[A6],
              values(6).asInstanceOf[A7],
              values(7).asInstanceOf[A8],
              values(8).asInstanceOf[A9],
              values(9).asInstanceOf[A10],
              values(10).asInstanceOf[A11],
              values(11).asInstanceOf[A12],
              values(12).asInstanceOf[A13],
              values(13).asInstanceOf[A14],
              values(14).asInstanceOf[A15],
              values(15).asInstanceOf[A16],
              values(16).asInstanceOf[A17],
              values(17).asInstanceOf[A18],
              values(18).asInstanceOf[A19],
              values(19).asInstanceOf[A20]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass20(${structure.mkString(",")})"

  }

  sealed case class CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    field11: Field[A11],
    field12: Field[A12],
    field13: Field[A13],
    field14: Field[A14],
    field15: Field[A15],
    field16: Field[A16],
    field17: Field[A17],
    field18: Field[A18],
    field19: Field[A19],
    field20: Field[A20],
    field21: Field[A21],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12,
    extractField13: Z => A13,
    extractField14: Z => A14,
    extractField15: Z => A15,
    extractField16: Z => A16,
    extractField17: Z => A17,
    extractField18: Z => A18,
    extractField19: Z => A19,
    extractField20: Z => A20,
    extractField21: Z => A21
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18, field19, field20, field21)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 21)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4],
              values(4).asInstanceOf[A5],
              values(5).asInstanceOf[A6],
              values(6).asInstanceOf[A7],
              values(7).asInstanceOf[A8],
              values(8).asInstanceOf[A9],
              values(9).asInstanceOf[A10],
              values(10).asInstanceOf[A11],
              values(11).asInstanceOf[A12],
              values(12).asInstanceOf[A13],
              values(13).asInstanceOf[A14],
              values(14).asInstanceOf[A15],
              values(15).asInstanceOf[A16],
              values(16).asInstanceOf[A17],
              values(17).asInstanceOf[A18],
              values(18).asInstanceOf[A19],
              values(19).asInstanceOf[A20],
              values(20).asInstanceOf[A21]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass21(${structure.mkString(",")})"

  }

  sealed case class CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](
    override val annotations: Chunk[Any] = Chunk.empty,
    field1: Field[A1],
    field2: Field[A2],
    field3: Field[A3],
    field4: Field[A4],
    field5: Field[A5],
    field6: Field[A6],
    field7: Field[A7],
    field8: Field[A8],
    field9: Field[A9],
    field10: Field[A10],
    field11: Field[A11],
    field12: Field[A12],
    field13: Field[A13],
    field14: Field[A14],
    field15: Field[A15],
    field16: Field[A16],
    field17: Field[A17],
    field18: Field[A18],
    field19: Field[A19],
    field20: Field[A20],
    field21: Field[A21],
    field22: Field[A22],
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12,
    extractField13: Z => A13,
    extractField14: Z => A14,
    extractField15: Z => A15,
    extractField16: Z => A16,
    extractField17: Z => A17,
    extractField18: Z => A18,
    extractField19: Z => A19,
    extractField20: Z => A20,
    extractField21: Z => A21,
    extractField22: Z => A22
  ) extends Record[Z] {
    override def structure: Chunk[Field[_]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18, field19, field20, field21, field22)
    override def rawConstruct(values: Chunk[Any]): Either[String, Z] =
      if (values.size == 22)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4],
              values(4).asInstanceOf[A5],
              values(5).asInstanceOf[A6],
              values(6).asInstanceOf[A7],
              values(7).asInstanceOf[A8],
              values(8).asInstanceOf[A9],
              values(9).asInstanceOf[A10],
              values(10).asInstanceOf[A11],
              values(11).asInstanceOf[A12],
              values(12).asInstanceOf[A13],
              values(13).asInstanceOf[A14],
              values(14).asInstanceOf[A15],
              values(15).asInstanceOf[A16],
              values(16).asInstanceOf[A17],
              values(17).asInstanceOf[A18],
              values(18).asInstanceOf[A19],
              values(19).asInstanceOf[A20],
              values(20).asInstanceOf[A21],
              values(21).asInstanceOf[A22]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $structure")

    override def toString: String = s"CaseClass22(${structure.mkString(",")})"

  }
}
