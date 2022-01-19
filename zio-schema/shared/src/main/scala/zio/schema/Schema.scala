package zio.schema

import java.time.temporal.ChronoUnit

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.ast._

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

  type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]]

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
   * The default value for a `Schema` of type `A`.
   */
  def defaultValue: Either[String, A]

  /**
   * Chunk of annotations for this schema
   */
  def annotations: Chunk[Any]

  def ast: SchemaAst = SchemaAst.fromSchema(self)

  /**
   * Returns a new schema that with `annotation`
   */
  def annotate(annotation: Any): Schema[A]

  /**
   *  Convert to Schema[B] iff B and A are homomorphic.
   *
   *  This can be used to e.g convert between a case class and it's
   *  "generic" representation as a ListMap[String,_]
   */
  def coerce[B](newSchema: Schema[B]): Either[String, Schema[B]] =
    for {
      f <- self.migrate(newSchema)
      g <- newSchema.migrate(self)
    } yield self.transformOrFail(f, g)

  /**
   * Performs a diff between thisValue and thatValue. See [[zio.schema.Differ]] for details
   * on the default diff algorithms.
   *
   * A custom [[zio.schema.Differ]] can be supplied if the default behavior is not acceptable.
   */
  def diff(thisValue: A, thatValue: A, differ: Option[Differ[A]] = None): Diff[A] = differ match {
    case Some(differ) => differ(thisValue, thatValue)
    case None         => Differ.fromSchema(self)(thisValue, thatValue)
  }

  /**
   * Patch value with a Diff.
   */
  def patch(oldValue: A, diff: Diff[A]): Either[String, A] = diff.patch(oldValue)

  def fromDynamic(value: DynamicValue): Either[String, A] =
    value.toTypedValue(self)

  def makeAccessors(b: AccessorBuilder): Accessors[b.Lens, b.Prism, b.Traversal]

  /**
   *  Generate a homomorphism from A to B iff A and B are homomorphic
   */
  def migrate[B](newSchema: Schema[B]): Either[String, A => Either[String, B]] =
    Migration.derive(SchemaAst.fromSchema(self), SchemaAst.fromSchema(newSchema)).map { transforms => (a: A) =>
      self.toDynamic(a).transform(transforms).flatMap(newSchema.fromDynamic)
    }

  /**
   * Returns a new schema that modifies the type produced by this schema to be optional.
   */
  def optional: Schema[Option[A]] = Schema.Optional(self)

  def ordering: Ordering[A] = SchemaOrdering.ordering(this)

  /**
   * Returns a new schema that combines this schema and the specified schema together, modeling
   * their either composition.
   */
  def orElseEither[B](that: Schema[B]): Schema[Either[A, B]] = Schema.EitherSchema(self, that)

  def repeated: Schema[Chunk[A]] = Schema.chunk(self)

  def serializable: Schema[Schema[A]] =
    Schema
      .Meta(SchemaAst.fromSchema(self))
      .transformOrFail(
        s => s.coerce(self),
        s => Right(s.ast.toSchema)
      )

  def toDynamic(value: A): DynamicValue =
    DynamicValue.fromSchemaAndValue(self, value)

  /**
   * Transforms this `Schema[A]` into a `Schema[B]`, by supplying two functions that can transform
   * between `A` and `B`, without possibility of failure.
   */
  def transform[B](f: A => B, g: B => A): Schema[B] =
    Schema.Transform[A, B](self, a => Right(f(a)), b => Right(g(b)), annotations)

  /**
   * Transforms this `Schema[A]` into a `Schema[B]`, by supplying two functions that can transform
   * between `A` and `B` (possibly failing in some cases).
   */
  def transformOrFail[B](f: A => Either[String, B], g: B => Either[String, A]): Schema[B] =
    Schema.Transform[A, B](self, f, g, annotations)

  /**
   * Returns a new schema that combines this schema and the specified schema together, modeling
   * their tuple composition.
   */
  def zip[B](that: Schema[B]): Schema[(A, B)] = Schema.Tuple(self, that)
}

object Schema extends TupleSchemas with RecordSchemas with EnumSchemas {
  def apply[A](implicit schema: Schema[A]): Schema[A] = schema

  def defer[A](schema: => Schema[A]): Schema[A] = Lazy(() => schema)

  def enumeration[A, C <: CaseSet.Aux[A]](caseSet: C): Schema[A] =
    EnumN(caseSet, Chunk.empty)

  def fail[A](message: String): Schema[A] = Fail(message)

  def first[A](codec: Schema[(A, Unit)]): Schema[A] =
    codec.transform[A](_._1, a => (a, ()))

  def record(field: Field[_]*): Schema[ListMap[String, _]] =
    GenericRecord(
      field.foldRight[FieldSet](FieldSet.Empty)((field, acc) => field :*: acc)
    )

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

  implicit val dynamicValue: Schema[DynamicValue] = DynamicValueSchema()

  implicit def chunk[A](implicit schemaA: Schema[A]): Schema[Chunk[A]] =
    Schema.Sequence[Chunk[A], A](schemaA, identity, identity, Chunk.empty)

  implicit def map[K, V](implicit ks: Schema[K], vs: Schema[V]): Schema[Map[K, V]] =
    Schema.MapSchema(ks, vs, Chunk.empty)

  implicit def set[A](implicit schemaA: Schema[A]): Schema[Set[A]] =
    Schema.SetSchema(schemaA, Chunk.empty)

  implicit def either[A, B](implicit left: Schema[A], right: Schema[B]): Schema[Either[A, B]] =
    EitherSchema(left, right)

  implicit def left[A, B](implicit schemaA: Schema[A]): Schema[Left[A, Nothing]] =
    schemaA.transform(Left(_), _.value)

  implicit def list[A](implicit schemaA: Schema[A]): Schema[List[A]] =
    Schema.Sequence[List[A], A](schemaA, _.toList, Chunk.fromIterable(_), Chunk.empty)

  implicit def option[A](implicit element: Schema[A]): Schema[Option[A]] =
    Optional(element)

  implicit def primitive[A](implicit standardType: StandardType[A]): Schema[A] =
    Primitive(standardType, Chunk.empty)

  implicit def right[A, B](implicit schemaB: Schema[B]): Schema[Right[Nothing, B]] =
    schemaB.transform(Right(_), _.value)

  implicit def vector[A](implicit element: Schema[A]): Schema[Vector[A]] =
    chunk(element).transform(_.toVector, Chunk.fromIterable(_))

  sealed trait Enum[A] extends Schema[A] {
    def structure: ListMap[String, Schema[_]]
  }
  sealed case class Field[A](label: String, schema: Schema[A], annotations: Chunk[Any] = Chunk.empty) {
    override def toString: String = s"Field($label,$schema)"
  }

  sealed trait Record[R] extends Schema[R] { self =>
    def structure: Chunk[Field[_]]
    def rawConstruct(values: Chunk[Any]): Either[String, R]

    def defaultValue: Either[String, R] =
      self.structure
        .map(_.schema.defaultValue)
        .foldLeft[Either[String, Chunk[R]]](Right(Chunk.empty)) {
          case (e @ Left(_), _)              => e
          case (_, Left(e))                  => Left[String, Chunk[R]](e)
          case (Right(values), Right(value)) => Right[String, Chunk[R]](values :+ value.asInstanceOf[R])
        }
        .flatMap(self.rawConstruct)
  }

  sealed trait Collection[Col, Elem] extends Schema[Col]

  final case class Sequence[Col, Elem](
    schemaA: Schema[Elem],
    fromChunk: Chunk[Elem] => Col,
    toChunk: Col => Chunk[Elem],
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Collection[Col, Elem] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = Traversal[Col, Elem]

    override def annotate(annotation: Any): Sequence[Col, Elem] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Col] = schemaA.defaultValue.map(fromChunk.compose(Chunk(_)))

    override def makeAccessors(b: AccessorBuilder): b.Traversal[Col, Elem] = b.makeTraversal(self, schemaA)
    override def toString: String                                          = s"Sequence($schemaA)"

  }

  final case class Transform[A, B](
    codec: Schema[A],
    f: A => Either[String, B],
    g: B => Either[String, A],
    annotations: Chunk[Any]
  ) extends Schema[B] {
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = codec.Accessors[Lens, Prism, Traversal]

    def defaultValue: Either[String, B] = codec.defaultValue.flatMap(f)

    override def makeAccessors(b: AccessorBuilder): codec.Accessors[b.Lens, b.Prism, b.Traversal] =
      codec.makeAccessors(b)

    override def annotate(annotation: Any): Transform[A, B] = copy(annotations = annotations :+ annotation)

    override def serializable: Schema[Schema[B]] = Meta(SchemaAst.fromSchema(codec)).transformOrFail(
      s => s.coerce(codec).flatMap(s1 => Right(s1.transformOrFail(f, g))),
      s => Right(s.transformOrFail(g, f).ast.toSchema)
    )
    override def toString: String = s"Transform($codec)"

  }

  final case class Primitive[A](standardType: StandardType[A], annotations: Chunk[Any] = Chunk.empty)
      extends Schema[A] {
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = Unit

    override def annotate(annotation: Any): Primitive[A] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, A] = standardType.defaultValue

    override def makeAccessors(b: AccessorBuilder): Unit = ()
  }

  final case class Optional[A](codec: Schema[A], annotations: Chunk[Any] = Chunk.empty) extends Schema[Option[A]] {
    self =>

    private[schema] val someCodec: Schema[Some[A]] = codec.transform(a => Some(a), _.get)

    override def annotate(annotation: Any): Optional[A] = copy(annotations = annotations :+ annotation)

    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] =
      (Prism[Option[A], Some[A]], Prism[Option[A], None.type])

    val toEnum: Enum2[Some[A], None.type, Option[A]] = Enum2(
      Case[Some[A], Option[A]]("Some", someCodec, _.asInstanceOf[Some[A]], Chunk.empty),
      Case[None.type, Option[A]]("None", singleton(None), _.asInstanceOf[None.type], Chunk.empty),
      Chunk.empty
    )

    def defaultValue: Either[String, Option[A]] = Right(None)

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Option[A], Some[A]], b.Prism[Option[A], None.type]) =
      b.makePrism(toEnum, toEnum.case1) -> b.makePrism(toEnum, toEnum.case2)

  }

  final case class Fail[A](message: String, annotations: Chunk[Any] = Chunk.empty) extends Schema[A] {
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = Unit

    override def annotate(annotation: Any): Fail[A] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, A] = Left(message)

    override def makeAccessors(b: AccessorBuilder): Unit = ()
  }

  final case class Tuple[A, B](left: Schema[A], right: Schema[B], annotations: Chunk[Any] = Chunk.empty)
      extends Schema[(A, B)] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[(A, B), A], Lens[(A, B), B])

    override def annotate(annotation: Any): Tuple[A, B] = copy(annotations = annotations :+ annotation)

    val toRecord: CaseClass2[A, B, (A, B)] = CaseClass2[A, B, (A, B)](
      field1 = Field("_1", left),
      field2 = Field("_2", right),
      construct = (a, b) => (a, b),
      extractField1 = _._1,
      extractField2 = _._2
    )

    override def defaultValue: Either[String, (A, B)] =
      left.defaultValue.flatMap(a => right.defaultValue.map(b => (a, b)))

    override def makeAccessors(b: AccessorBuilder): (b.Lens[(A, B), A], b.Lens[(A, B), B]) =
      b.makeLens(toRecord, toRecord.field1) -> b.makeLens(toRecord, toRecord.field2)

  }

  final case class EitherSchema[A, B](left: Schema[A], right: Schema[B], annotations: Chunk[Any] = Chunk.empty)
      extends Schema[Either[A, B]] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] =
      (Prism[Either[A, B], Right[Nothing, B]], Prism[Either[A, B], Left[A, Nothing]])

    override def annotate(annotation: Any): EitherSchema[A, B] = copy(annotations = annotations :+ annotation)

    val rightSchema: Schema[Right[Nothing, B]] = right.transform(b => Right(b), _.value)
    val leftSchema: Schema[Left[A, Nothing]]   = left.transform(a => Left(a), _.value)

    val toEnum: Enum2[Right[Nothing, B], Left[A, Nothing], Either[A, B]] = Enum2(
      Case("Right", rightSchema, _.asInstanceOf[Right[Nothing, B]], Chunk.empty),
      Case("Left", leftSchema, _.asInstanceOf[Left[A, Nothing]], Chunk.empty),
      Chunk.empty
    )

    override def defaultValue: Either[String, Either[A, B]] =
      left.defaultValue match {
        case Right(a) => Right(Left(a))
        case _ =>
          right.defaultValue match {
            case Right(b) => Right(Right(b))
            case _        => Left("unable to extract default value for EitherSchema")
          }
      }

    override def makeAccessors(
      b: AccessorBuilder
    ): (b.Prism[Either[A, B], Right[Nothing, B]], b.Prism[Either[A, B], Left[A, Nothing]]) =
      b.makePrism(toEnum, toEnum.case1) -> b.makePrism(toEnum, toEnum.case2)

  }

  final case class Lazy[A](private val schema0: () => Schema[A]) extends Schema[A] {
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = schema.Accessors[Lens, Prism, Traversal]

    override def annotate(annotation: Any): Lazy[A] = Lazy(() => schema0().annotate(annotation))

    lazy val schema: Schema[A] = schema0()

    def defaultValue: Either[String, A] = schema.defaultValue

    override def makeAccessors(b: AccessorBuilder): schema.Accessors[b.Lens, b.Prism, b.Traversal] =
      schema.makeAccessors(b)

    override def toString: String = "$Lazy$"

    override def annotations: Chunk[Any] = schema0().annotations
  }

  final case class Meta(override val ast: SchemaAst, annotations: Chunk[Any] = Chunk.empty) extends Schema[Schema[_]] {

    override def annotate(annotation: Any): Meta = copy(annotations = annotations :+ annotation)

    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = Unit

    override def defaultValue: Either[String, Schema[_]] =
      ast.toSchema.defaultValue.asInstanceOf[Either[String, Schema[_]]]

    override def makeAccessors(b: AccessorBuilder): Unit = ()

  }

  final case class MapSchema[K, V](ks: Schema[K], vs: Schema[V], override val annotations: Chunk[Any])
      extends Collection[Map[K, V], (K, V)] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = Traversal[Map[K, V], (K, V)]

    override def annotate(annotation: Any): MapSchema[K, V] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Map[K, V]] =
      ks.defaultValue.flatMap(defaultKey => vs.defaultValue.map(defaultValue => Map(defaultKey -> defaultValue)))

    override def makeAccessors(b: AccessorBuilder): b.Traversal[Map[K, V], (K, V)] =
      b.makeTraversal(self, ks <*> vs)
  }

  final case class SetSchema[A](as: Schema[A], override val annotations: Chunk[Any]) extends Collection[Set[A], A] {
    self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = Traversal[Set[A], A]

    override def annotate(annotation: Any): SetSchema[A] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Set[A]] =
      as.defaultValue.map(Set(_))

    override def makeAccessors(b: AccessorBuilder): b.Traversal[Set[A], A] =
      b.makeTraversal(self, as)
  }

}

//scalafmt: { maxColumn = 400 }
sealed trait EnumSchemas { self: Schema.type =>

  sealed case class Case[A, Z](id: String, codec: Schema[A], unsafeDeconstruct: Z => A, annotations: Chunk[Any] = Chunk.empty) {

    def deconstruct(z: Z): Option[A] =
      try {
        Some(unsafeDeconstruct(z))
      } catch { case _: Throwable => None }

    override def toString: String = s"Case($id,$codec,$annotations)"
  }

  sealed case class Enum1[A <: Z, Z](case1: Case[A, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = Prism[Z, A]

    override def annotate(annotation: Any): Enum1[A, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): b.Prism[Z, A] = b.makePrism(self, case1)

    override def structure: ListMap[String, Schema[_]] =
      ListMap(case1.id -> case1.codec)
  }

  sealed case class Enum2[A1 <: Z, A2 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2])

    override def annotate(annotation: Any): Enum2[A1, A2, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2]) =
      (b.makePrism(self, case1), b.makePrism(self, case2))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(case1.id -> case1.codec, case2.id -> case2.codec)
  }

  sealed case class Enum3[A1 <: Z, A2 <: Z, A3 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3])

    override def annotate(annotation: Any): Enum3[A1, A2, A3, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(case1.id -> case1.codec, case2.id -> case2.codec, case3.id -> case3.codec)
  }

  sealed case class Enum4[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z], case4: Case[A4, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4])

    override def annotate(annotation: Any): Enum4[A1, A2, A3, A4, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3), b.makePrism(self, case4))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(case1.id -> case1.codec, case2.id -> case2.codec, case3.id -> case3.codec, case4.id -> case4.codec)
  }

  sealed case class Enum5[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z], case4: Case[A4, Z], case5: Case[A5, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5])

    override def annotate(annotation: Any): Enum5[A1, A2, A3, A4, A5, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3), b.makePrism(self, case4), b.makePrism(self, case5))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(case1.id -> case1.codec, case2.id -> case2.codec, case3.id -> case3.codec, case4.id -> case4.codec, case5.id -> case5.codec)
  }

  sealed case class Enum6[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z], case4: Case[A4, Z], case5: Case[A5, Z], case6: Case[A6, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6])

    override def annotate(annotation: Any): Enum6[A1, A2, A3, A4, A5, A6, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3), b.makePrism(self, case4), b.makePrism(self, case5), b.makePrism(self, case6))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(case1.id -> case1.codec, case2.id -> case2.codec, case3.id -> case3.codec, case4.id -> case4.codec, case5.id -> case5.codec, case6.id -> case6.codec)
  }

  sealed case class Enum7[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z], case4: Case[A4, Z], case5: Case[A5, Z], case6: Case[A6, Z], case7: Case[A7, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7])

    override def annotate(annotation: Any): Enum7[A1, A2, A3, A4, A5, A6, A7, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3), b.makePrism(self, case4), b.makePrism(self, case5), b.makePrism(self, case6), b.makePrism(self, case7))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id -> case1.codec,
        case2.id -> case2.codec,
        case3.id -> case3.codec,
        case4.id -> case4.codec,
        case5.id -> case5.codec,
        case6.id -> case6.codec,
        case7.id -> case7.codec
      )
  }

  sealed case class Enum8[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z], case4: Case[A4, Z], case5: Case[A5, Z], case6: Case[A6, Z], case7: Case[A7, Z], case8: Case[A8, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8])

    override def annotate(annotation: Any): Enum8[A1, A2, A3, A4, A5, A6, A7, A8, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3), b.makePrism(self, case4), b.makePrism(self, case5), b.makePrism(self, case6), b.makePrism(self, case7), b.makePrism(self, case8))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id -> case1.codec,
        case2.id -> case2.codec,
        case3.id -> case3.codec,
        case4.id -> case4.codec,
        case5.id -> case5.codec,
        case6.id -> case6.codec,
        case7.id -> case7.codec,
        case8.id -> case8.codec
      )
  }

  sealed case class Enum9[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z], case4: Case[A4, Z], case5: Case[A5, Z], case6: Case[A6, Z], case7: Case[A7, Z], case8: Case[A8, Z], case9: Case[A9, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9])

    override def annotate(annotation: Any): Enum9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3), b.makePrism(self, case4), b.makePrism(self, case5), b.makePrism(self, case6), b.makePrism(self, case7), b.makePrism(self, case8), b.makePrism(self, case9))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id -> case1.codec,
        case2.id -> case2.codec,
        case3.id -> case3.codec,
        case4.id -> case4.codec,
        case5.id -> case5.codec,
        case6.id -> case6.codec,
        case7.id -> case7.codec,
        case8.id -> case8.codec,
        case9.id -> case9.codec
      )
  }
  sealed case class Enum10[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z], case4: Case[A4, Z], case5: Case[A5, Z], case6: Case[A6, Z], case7: Case[A7, Z], case8: Case[A8, Z], case9: Case[A9, Z], case10: Case[A10, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10])

    override def annotate(annotation: Any): Enum10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3), b.makePrism(self, case4), b.makePrism(self, case5), b.makePrism(self, case6), b.makePrism(self, case7), b.makePrism(self, case8), b.makePrism(self, case9), b.makePrism(self, case10))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec
      )
  }
  sealed case class Enum11[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, A11 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z], case4: Case[A4, Z], case5: Case[A5, Z], case6: Case[A6, Z], case7: Case[A7, Z], case8: Case[A8, Z], case9: Case[A9, Z], case10: Case[A10, Z], case11: Case[A11, Z], annotations: Chunk[Any] = Chunk.empty)
      extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10], Prism[Z, A11])

    override def annotate(annotation: Any): Enum11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10], b.Prism[Z, A11]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3), b.makePrism(self, case4), b.makePrism(self, case5), b.makePrism(self, case6), b.makePrism(self, case7), b.makePrism(self, case8), b.makePrism(self, case9), b.makePrism(self, case10), b.makePrism(self, case11))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec,
        case11.id -> case11.codec
      )
  }
  sealed case class Enum12[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, A11 <: Z, A12 <: Z, Z](
    case1: Case[A1, Z],
    case2: Case[A2, Z],
    case3: Case[A3, Z],
    case4: Case[A4, Z],
    case5: Case[A5, Z],
    case6: Case[A6, Z],
    case7: Case[A7, Z],
    case8: Case[A8, Z],
    case9: Case[A9, Z],
    case10: Case[A10, Z],
    case11: Case[A11, Z],
    case12: Case[A12, Z],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10], Prism[Z, A11], Prism[Z, A12])

    override def annotate(annotation: Any): Enum12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10], b.Prism[Z, A11], b.Prism[Z, A12]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3), b.makePrism(self, case4), b.makePrism(self, case5), b.makePrism(self, case6), b.makePrism(self, case7), b.makePrism(self, case8), b.makePrism(self, case9), b.makePrism(self, case10), b.makePrism(self, case11), b.makePrism(self, case12))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec,
        case11.id -> case11.codec,
        case12.id -> case12.codec
      )
  }
  sealed case class Enum13[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, A11 <: Z, A12 <: Z, A13 <: Z, Z](
    case1: Case[A1, Z],
    case2: Case[A2, Z],
    case3: Case[A3, Z],
    case4: Case[A4, Z],
    case5: Case[A5, Z],
    case6: Case[A6, Z],
    case7: Case[A7, Z],
    case8: Case[A8, Z],
    case9: Case[A9, Z],
    case10: Case[A10, Z],
    case11: Case[A11, Z],
    case12: Case[A12, Z],
    case13: Case[A13, Z],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10], Prism[Z, A11], Prism[Z, A12], Prism[Z, A13])

    override def annotate(annotation: Any): Enum13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10], b.Prism[Z, A11], b.Prism[Z, A12], b.Prism[Z, A13]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3), b.makePrism(self, case4), b.makePrism(self, case5), b.makePrism(self, case6), b.makePrism(self, case7), b.makePrism(self, case8), b.makePrism(self, case9), b.makePrism(self, case10), b.makePrism(self, case11), b.makePrism(self, case12), b.makePrism(self, case13))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec,
        case11.id -> case11.codec,
        case12.id -> case12.codec,
        case13.id -> case13.codec
      )
  }

  sealed case class Enum14[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, A11 <: Z, A12 <: Z, A13 <: Z, A14 <: Z, Z](
    case1: Case[A1, Z],
    case2: Case[A2, Z],
    case3: Case[A3, Z],
    case4: Case[A4, Z],
    case5: Case[A5, Z],
    case6: Case[A6, Z],
    case7: Case[A7, Z],
    case8: Case[A8, Z],
    case9: Case[A9, Z],
    case10: Case[A10, Z],
    case11: Case[A11, Z],
    case12: Case[A12, Z],
    case13: Case[A13, Z],
    case14: Case[A14, Z],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10], Prism[Z, A11], Prism[Z, A12], Prism[Z, A13], Prism[Z, A14])

    override def annotate(annotation: Any): Enum14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10], b.Prism[Z, A11], b.Prism[Z, A12], b.Prism[Z, A13], b.Prism[Z, A14]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3), b.makePrism(self, case4), b.makePrism(self, case5), b.makePrism(self, case6), b.makePrism(self, case7), b.makePrism(self, case8), b.makePrism(self, case9), b.makePrism(self, case10), b.makePrism(self, case11), b.makePrism(self, case12), b.makePrism(self, case13), b.makePrism(self, case14))

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec,
        case11.id -> case11.codec,
        case12.id -> case12.codec,
        case13.id -> case13.codec,
        case14.id -> case14.codec
      )
  }
  sealed case class Enum15[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, A11 <: Z, A12 <: Z, A13 <: Z, A14 <: Z, A15 <: Z, Z](
    case1: Case[A1, Z],
    case2: Case[A2, Z],
    case3: Case[A3, Z],
    case4: Case[A4, Z],
    case5: Case[A5, Z],
    case6: Case[A6, Z],
    case7: Case[A7, Z],
    case8: Case[A8, Z],
    case9: Case[A9, Z],
    case10: Case[A10, Z],
    case11: Case[A11, Z],
    case12: Case[A12, Z],
    case13: Case[A13, Z],
    case14: Case[A14, Z],
    case15: Case[A15, Z],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10], Prism[Z, A11], Prism[Z, A12], Prism[Z, A13], Prism[Z, A14], Prism[Z, A15])

    override def annotate(annotation: Any): Enum15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10], b.Prism[Z, A11], b.Prism[Z, A12], b.Prism[Z, A13], b.Prism[Z, A14], b.Prism[Z, A15]) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5),
        b.makePrism(self, case6),
        b.makePrism(self, case7),
        b.makePrism(self, case8),
        b.makePrism(self, case9),
        b.makePrism(self, case10),
        b.makePrism(self, case11),
        b.makePrism(self, case12),
        b.makePrism(self, case13),
        b.makePrism(self, case14),
        b.makePrism(self, case15)
      )

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec,
        case11.id -> case11.codec,
        case12.id -> case12.codec,
        case13.id -> case13.codec,
        case14.id -> case14.codec,
        case15.id -> case15.codec
      )
  }
  sealed case class Enum16[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, A11 <: Z, A12 <: Z, A13 <: Z, A14 <: Z, A15 <: Z, A16 <: Z, Z](
    case1: Case[A1, Z],
    case2: Case[A2, Z],
    case3: Case[A3, Z],
    case4: Case[A4, Z],
    case5: Case[A5, Z],
    case6: Case[A6, Z],
    case7: Case[A7, Z],
    case8: Case[A8, Z],
    case9: Case[A9, Z],
    case10: Case[A10, Z],
    case11: Case[A11, Z],
    case12: Case[A12, Z],
    case13: Case[A13, Z],
    case14: Case[A14, Z],
    case15: Case[A15, Z],
    case16: Case[A16, Z],
    override val annotations: Chunk[Any]
  ) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10], Prism[Z, A11], Prism[Z, A12], Prism[Z, A13], Prism[Z, A14], Prism[Z, A15], Prism[Z, A16])

    override def annotate(annotation: Any): Enum16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10], b.Prism[Z, A11], b.Prism[Z, A12], b.Prism[Z, A13], b.Prism[Z, A14], b.Prism[Z, A15], b.Prism[Z, A16]) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5),
        b.makePrism(self, case6),
        b.makePrism(self, case7),
        b.makePrism(self, case8),
        b.makePrism(self, case9),
        b.makePrism(self, case10),
        b.makePrism(self, case11),
        b.makePrism(self, case12),
        b.makePrism(self, case13),
        b.makePrism(self, case14),
        b.makePrism(self, case15),
        b.makePrism(self, case16)
      )

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec,
        case11.id -> case11.codec,
        case12.id -> case12.codec,
        case13.id -> case13.codec,
        case14.id -> case14.codec,
        case15.id -> case15.codec,
        case16.id -> case16.codec
      )
  }
  sealed case class Enum17[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, A11 <: Z, A12 <: Z, A13 <: Z, A14 <: Z, A15 <: Z, A16 <: Z, A17 <: Z, Z](
    case1: Case[A1, Z],
    case2: Case[A2, Z],
    case3: Case[A3, Z],
    case4: Case[A4, Z],
    case5: Case[A5, Z],
    case6: Case[A6, Z],
    case7: Case[A7, Z],
    case8: Case[A8, Z],
    case9: Case[A9, Z],
    case10: Case[A10, Z],
    case11: Case[A11, Z],
    case12: Case[A12, Z],
    case13: Case[A13, Z],
    case14: Case[A14, Z],
    case15: Case[A15, Z],
    case16: Case[A16, Z],
    case17: Case[A17, Z],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10], Prism[Z, A11], Prism[Z, A12], Prism[Z, A13], Prism[Z, A14], Prism[Z, A15], Prism[Z, A16], Prism[Z, A17])

    override def annotate(annotation: Any): Enum17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10], b.Prism[Z, A11], b.Prism[Z, A12], b.Prism[Z, A13], b.Prism[Z, A14], b.Prism[Z, A15], b.Prism[Z, A16], b.Prism[Z, A17]) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5),
        b.makePrism(self, case6),
        b.makePrism(self, case7),
        b.makePrism(self, case8),
        b.makePrism(self, case9),
        b.makePrism(self, case10),
        b.makePrism(self, case11),
        b.makePrism(self, case12),
        b.makePrism(self, case13),
        b.makePrism(self, case14),
        b.makePrism(self, case15),
        b.makePrism(self, case16),
        b.makePrism(self, case17)
      )

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec,
        case11.id -> case11.codec,
        case12.id -> case12.codec,
        case13.id -> case13.codec,
        case14.id -> case14.codec,
        case15.id -> case15.codec,
        case16.id -> case16.codec,
        case17.id -> case17.codec
      )
  }
  sealed case class Enum18[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, A11 <: Z, A12 <: Z, A13 <: Z, A14 <: Z, A15 <: Z, A16 <: Z, A17 <: Z, A18 <: Z, Z](
    case1: Case[A1, Z],
    case2: Case[A2, Z],
    case3: Case[A3, Z],
    case4: Case[A4, Z],
    case5: Case[A5, Z],
    case6: Case[A6, Z],
    case7: Case[A7, Z],
    case8: Case[A8, Z],
    case9: Case[A9, Z],
    case10: Case[A10, Z],
    case11: Case[A11, Z],
    case12: Case[A12, Z],
    case13: Case[A13, Z],
    case14: Case[A14, Z],
    case15: Case[A15, Z],
    case16: Case[A16, Z],
    case17: Case[A17, Z],
    case18: Case[A18, Z],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10], Prism[Z, A11], Prism[Z, A12], Prism[Z, A13], Prism[Z, A14], Prism[Z, A15], Prism[Z, A16], Prism[Z, A17], Prism[Z, A18])

    override def annotate(annotation: Any): Enum18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10], b.Prism[Z, A11], b.Prism[Z, A12], b.Prism[Z, A13], b.Prism[Z, A14], b.Prism[Z, A15], b.Prism[Z, A16], b.Prism[Z, A17], b.Prism[Z, A18]) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5),
        b.makePrism(self, case6),
        b.makePrism(self, case7),
        b.makePrism(self, case8),
        b.makePrism(self, case9),
        b.makePrism(self, case10),
        b.makePrism(self, case11),
        b.makePrism(self, case12),
        b.makePrism(self, case13),
        b.makePrism(self, case14),
        b.makePrism(self, case15),
        b.makePrism(self, case16),
        b.makePrism(self, case17),
        b.makePrism(self, case18)
      )

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec,
        case11.id -> case11.codec,
        case12.id -> case12.codec,
        case13.id -> case13.codec,
        case14.id -> case14.codec,
        case15.id -> case15.codec,
        case16.id -> case16.codec,
        case17.id -> case17.codec,
        case18.id -> case18.codec
      )
  }
  sealed case class Enum19[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, A11 <: Z, A12 <: Z, A13 <: Z, A14 <: Z, A15 <: Z, A16 <: Z, A17 <: Z, A18 <: Z, A19 <: Z, Z](
    case1: Case[A1, Z],
    case2: Case[A2, Z],
    case3: Case[A3, Z],
    case4: Case[A4, Z],
    case5: Case[A5, Z],
    case6: Case[A6, Z],
    case7: Case[A7, Z],
    case8: Case[A8, Z],
    case9: Case[A9, Z],
    case10: Case[A10, Z],
    case11: Case[A11, Z],
    case12: Case[A12, Z],
    case13: Case[A13, Z],
    case14: Case[A14, Z],
    case15: Case[A15, Z],
    case16: Case[A16, Z],
    case17: Case[A17, Z],
    case18: Case[A18, Z],
    case19: Case[A19, Z],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10], Prism[Z, A11], Prism[Z, A12], Prism[Z, A13], Prism[Z, A14], Prism[Z, A15], Prism[Z, A16], Prism[Z, A17], Prism[Z, A18], Prism[Z, A19])

    override def annotate(annotation: Any): Enum19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10], b.Prism[Z, A11], b.Prism[Z, A12], b.Prism[Z, A13], b.Prism[Z, A14], b.Prism[Z, A15], b.Prism[Z, A16], b.Prism[Z, A17], b.Prism[Z, A18], b.Prism[Z, A19]) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5),
        b.makePrism(self, case6),
        b.makePrism(self, case7),
        b.makePrism(self, case8),
        b.makePrism(self, case9),
        b.makePrism(self, case10),
        b.makePrism(self, case11),
        b.makePrism(self, case12),
        b.makePrism(self, case13),
        b.makePrism(self, case14),
        b.makePrism(self, case15),
        b.makePrism(self, case16),
        b.makePrism(self, case17),
        b.makePrism(self, case18),
        b.makePrism(self, case19)
      )

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec,
        case11.id -> case11.codec,
        case12.id -> case12.codec,
        case13.id -> case13.codec,
        case14.id -> case14.codec,
        case15.id -> case15.codec,
        case16.id -> case16.codec,
        case17.id -> case17.codec,
        case18.id -> case18.codec,
        case19.id -> case19.codec
      )
  }
  sealed case class Enum20[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, A11 <: Z, A12 <: Z, A13 <: Z, A14 <: Z, A15 <: Z, A16 <: Z, A17 <: Z, A18 <: Z, A19 <: Z, A20 <: Z, Z](
    case1: Case[A1, Z],
    case2: Case[A2, Z],
    case3: Case[A3, Z],
    case4: Case[A4, Z],
    case5: Case[A5, Z],
    case6: Case[A6, Z],
    case7: Case[A7, Z],
    case8: Case[A8, Z],
    case9: Case[A9, Z],
    case10: Case[A10, Z],
    case11: Case[A11, Z],
    case12: Case[A12, Z],
    case13: Case[A13, Z],
    case14: Case[A14, Z],
    case15: Case[A15, Z],
    case16: Case[A16, Z],
    case17: Case[A17, Z],
    case18: Case[A18, Z],
    case19: Case[A19, Z],
    case20: Case[A20, Z],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10], Prism[Z, A11], Prism[Z, A12], Prism[Z, A13], Prism[Z, A14], Prism[Z, A15], Prism[Z, A16], Prism[Z, A17], Prism[Z, A18], Prism[Z, A19], Prism[Z, A20])

    override def annotate(annotation: Any): Enum20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10], b.Prism[Z, A11], b.Prism[Z, A12], b.Prism[Z, A13], b.Prism[Z, A14], b.Prism[Z, A15], b.Prism[Z, A16], b.Prism[Z, A17], b.Prism[Z, A18], b.Prism[Z, A19], b.Prism[Z, A20]) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5),
        b.makePrism(self, case6),
        b.makePrism(self, case7),
        b.makePrism(self, case8),
        b.makePrism(self, case9),
        b.makePrism(self, case10),
        b.makePrism(self, case11),
        b.makePrism(self, case12),
        b.makePrism(self, case13),
        b.makePrism(self, case14),
        b.makePrism(self, case15),
        b.makePrism(self, case16),
        b.makePrism(self, case17),
        b.makePrism(self, case18),
        b.makePrism(self, case19),
        b.makePrism(self, case20)
      )

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec,
        case11.id -> case11.codec,
        case12.id -> case12.codec,
        case13.id -> case13.codec,
        case14.id -> case14.codec,
        case15.id -> case15.codec,
        case16.id -> case16.codec,
        case17.id -> case17.codec,
        case18.id -> case18.codec,
        case19.id -> case19.codec,
        case20.id -> case20.codec
      )
  }
  sealed case class Enum21[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, A11 <: Z, A12 <: Z, A13 <: Z, A14 <: Z, A15 <: Z, A16 <: Z, A17 <: Z, A18 <: Z, A19 <: Z, A20 <: Z, A21 <: Z, Z](
    case1: Case[A1, Z],
    case2: Case[A2, Z],
    case3: Case[A3, Z],
    case4: Case[A4, Z],
    case5: Case[A5, Z],
    case6: Case[A6, Z],
    case7: Case[A7, Z],
    case8: Case[A8, Z],
    case9: Case[A9, Z],
    case10: Case[A10, Z],
    case11: Case[A11, Z],
    case12: Case[A12, Z],
    case13: Case[A13, Z],
    case14: Case[A14, Z],
    case15: Case[A15, Z],
    case16: Case[A16, Z],
    case17: Case[A17, Z],
    case18: Case[A18, Z],
    case19: Case[A19, Z],
    case20: Case[A20, Z],
    case21: Case[A21, Z],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10], Prism[Z, A11], Prism[Z, A12], Prism[Z, A13], Prism[Z, A14], Prism[Z, A15], Prism[Z, A16], Prism[Z, A17], Prism[Z, A18], Prism[Z, A19], Prism[Z, A20], Prism[Z, A21])

    override def annotate(annotation: Any): Enum21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(
      b: AccessorBuilder
    ): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10], b.Prism[Z, A11], b.Prism[Z, A12], b.Prism[Z, A13], b.Prism[Z, A14], b.Prism[Z, A15], b.Prism[Z, A16], b.Prism[Z, A17], b.Prism[Z, A18], b.Prism[Z, A19], b.Prism[Z, A20], b.Prism[Z, A21]) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5),
        b.makePrism(self, case6),
        b.makePrism(self, case7),
        b.makePrism(self, case8),
        b.makePrism(self, case9),
        b.makePrism(self, case10),
        b.makePrism(self, case11),
        b.makePrism(self, case12),
        b.makePrism(self, case13),
        b.makePrism(self, case14),
        b.makePrism(self, case15),
        b.makePrism(self, case16),
        b.makePrism(self, case17),
        b.makePrism(self, case18),
        b.makePrism(self, case19),
        b.makePrism(self, case20),
        b.makePrism(self, case21)
      )

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec,
        case11.id -> case11.codec,
        case12.id -> case12.codec,
        case13.id -> case13.codec,
        case14.id -> case14.codec,
        case15.id -> case15.codec,
        case16.id -> case16.codec,
        case17.id -> case17.codec,
        case18.id -> case18.codec,
        case19.id -> case19.codec,
        case20.id -> case20.codec,
        case21.id -> case21.codec
      )
  }
  sealed case class Enum22[A1 <: Z, A2 <: Z, A3 <: Z, A4 <: Z, A5 <: Z, A6 <: Z, A7 <: Z, A8 <: Z, A9 <: Z, A10 <: Z, A11 <: Z, A12 <: Z, A13 <: Z, A14 <: Z, A15 <: Z, A16 <: Z, A17 <: Z, A18 <: Z, A19 <: Z, A20 <: Z, A21 <: Z, A22 <: Z, Z](
    case1: Case[A1, Z],
    case2: Case[A2, Z],
    case3: Case[A3, Z],
    case4: Case[A4, Z],
    case5: Case[A5, Z],
    case6: Case[A6, Z],
    case7: Case[A7, Z],
    case8: Case[A8, Z],
    case9: Case[A9, Z],
    case10: Case[A10, Z],
    case11: Case[A11, Z],
    case12: Case[A12, Z],
    case13: Case[A13, Z],
    case14: Case[A14, Z],
    case15: Case[A15, Z],
    case16: Case[A16, Z],
    case17: Case[A17, Z],
    case18: Case[A18, Z],
    case19: Case[A19, Z],
    case20: Case[A20, Z],
    case21: Case[A21, Z],
    case22: Case[A22, Z],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Prism[Z, A1], Prism[Z, A2], Prism[Z, A3], Prism[Z, A4], Prism[Z, A5], Prism[Z, A6], Prism[Z, A7], Prism[Z, A8], Prism[Z, A9], Prism[Z, A10], Prism[Z, A11], Prism[Z, A12], Prism[Z, A13], Prism[Z, A14], Prism[Z, A15], Prism[Z, A16], Prism[Z, A17], Prism[Z, A18], Prism[Z, A19], Prism[Z, A20], Prism[Z, A21], Prism[Z, A22])

    override def annotate(annotation: Any): Enum22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: Either[String, Z] = case1.codec.defaultValue

    override def makeAccessors(
      b: AccessorBuilder
    ): (b.Prism[Z, A1], b.Prism[Z, A2], b.Prism[Z, A3], b.Prism[Z, A4], b.Prism[Z, A5], b.Prism[Z, A6], b.Prism[Z, A7], b.Prism[Z, A8], b.Prism[Z, A9], b.Prism[Z, A10], b.Prism[Z, A11], b.Prism[Z, A12], b.Prism[Z, A13], b.Prism[Z, A14], b.Prism[Z, A15], b.Prism[Z, A16], b.Prism[Z, A17], b.Prism[Z, A18], b.Prism[Z, A19], b.Prism[Z, A20], b.Prism[Z, A21], b.Prism[Z, A22]) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5),
        b.makePrism(self, case6),
        b.makePrism(self, case7),
        b.makePrism(self, case8),
        b.makePrism(self, case9),
        b.makePrism(self, case10),
        b.makePrism(self, case11),
        b.makePrism(self, case12),
        b.makePrism(self, case13),
        b.makePrism(self, case14),
        b.makePrism(self, case15),
        b.makePrism(self, case16),
        b.makePrism(self, case17),
        b.makePrism(self, case18),
        b.makePrism(self, case19),
        b.makePrism(self, case20),
        b.makePrism(self, case21),
        b.makePrism(self, case22)
      )

    override def structure: ListMap[String, Schema[_]] =
      ListMap(
        case1.id  -> case1.codec,
        case2.id  -> case2.codec,
        case3.id  -> case3.codec,
        case4.id  -> case4.codec,
        case5.id  -> case5.codec,
        case6.id  -> case6.codec,
        case7.id  -> case7.codec,
        case8.id  -> case8.codec,
        case9.id  -> case9.codec,
        case10.id -> case10.codec,
        case11.id -> case11.codec,
        case12.id -> case12.codec,
        case13.id -> case13.codec,
        case14.id -> case14.codec,
        case15.id -> case15.codec,
        case16.id -> case16.codec,
        case17.id -> case17.codec,
        case18.id -> case18.codec,
        case19.id -> case19.codec,
        case20.id -> case20.codec,
        case21.id -> case21.codec,
        case22.id -> case22.codec
      )
  }
  sealed case class EnumN[Z, C <: CaseSet.Aux[Z]](caseSet: C, annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = caseSet.Accessors[Z, Lens, Prism, Traversal]

    override def annotate(annotation: Any): EnumN[Z, C] = copy(annotations = annotations :+ annotation)

    override def structure: ListMap[String, Schema[_]] = caseSet.toMap

    def defaultValue: Either[String, Z] =
      if (caseSet.toSeq.isEmpty)
        Left("cannot access default value for enum with no members")
      else
        caseSet.toSeq.head.codec.defaultValue.asInstanceOf[Either[String, Z]]

    override def makeAccessors(b: AccessorBuilder): caseSet.Accessors[Z, b.Lens, b.Prism, b.Traversal] = caseSet.makeAccessors(self, b)
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

  sealed case class GenericRecord(fieldSet: FieldSet, override val annotations: Chunk[Any] = Chunk.empty) extends Record[ListMap[String, _]] { self =>

    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = fieldSet.Accessors[ListMap[String, _], Lens, Prism, Traversal]

    override def makeAccessors(b: AccessorBuilder): Accessors[b.Lens, b.Prism, b.Traversal] = fieldSet.makeAccessors(self, b)

    override def structure: Chunk[Schema.Field[_]] = fieldSet.toChunk

    override def rawConstruct(values: Chunk[Any]): Either[String, ListMap[String, _]] =
      if (values.size == structure.size)
        Right(ListMap(structure.map(_.label).zip(values): _*))
      else
        Left(s"wrong number of values for $structure")

    /**
     * Returns a new schema that with `annotation`
     */
    override def annotate(annotation: Any): GenericRecord = copy(annotations = annotations :+ annotation)

  }

  sealed case class CaseClass1[A, Z](field: Field[A], construct: A => Z, extractField: Z => A, override val annotations: Chunk[Any] = Chunk.empty) extends Record[Z] { self =>

    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = Lens[Z, A]

    override def annotate(annotation: Any): CaseClass1[A, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): b.Lens[Z, A] = b.makeLens(self, field)

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

  sealed case class CaseClass2[A1, A2, Z](field1: Field[A1], field2: Field[A2], construct: (A1, A2) => Z, extractField1: Z => A1, extractField2: Z => A2, override val annotations: Chunk[Any] = Chunk.empty) extends Record[Z] { self =>

    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2])

    override def annotate(annotation: Any): CaseClass2[A1, A2, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2]) = (b.makeLens(self, field1), b.makeLens(self, field2))

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

  sealed case class CaseClass3[A1, A2, A3, Z](field1: Field[A1], field2: Field[A2], field3: Field[A3], construct: (A1, A2, A3) => Z, extractField1: Z => A1, extractField2: Z => A2, extractField3: Z => A3, override val annotations: Chunk[Any] = Chunk.empty) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3])

    override def annotate(annotation: Any): CaseClass3[A1, A2, A3, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3))

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

  sealed case class CaseClass4[A1, A2, A3, A4, Z](field1: Field[A1], field2: Field[A2], field3: Field[A3], field4: Field[A4], construct: (A1, A2, A3, A4) => Z, extractField1: Z => A1, extractField2: Z => A2, extractField3: Z => A3, extractField4: Z => A4, override val annotations: Chunk[Any] = Chunk.empty) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4])

    override def annotate(annotation: Any): CaseClass4[A1, A2, A3, A4, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4))

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

  sealed case class CaseClass5[A1, A2, A3, A4, A5, Z](field1: Field[A1], field2: Field[A2], field3: Field[A3], field4: Field[A4], field5: Field[A5], construct: (A1, A2, A3, A4, A5) => Z, extractField1: Z => A1, extractField2: Z => A2, extractField3: Z => A3, extractField4: Z => A4, extractField5: Z => A5, override val annotations: Chunk[Any] = Chunk.empty) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5])

    override def annotate(annotation: Any): CaseClass5[A1, A2, A3, A4, A5, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4), b.makeLens(self, field5))

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
    extractField6: Z => A6,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6])

    override def annotate(annotation: Any): CaseClass6[A1, A2, A3, A4, A5, A6, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4), b.makeLens(self, field5), b.makeLens(self, field6))

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
    extractField7: Z => A7,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7])

    override def annotate(annotation: Any): CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4), b.makeLens(self, field5), b.makeLens(self, field6), b.makeLens(self, field7))

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
    extractField8: Z => A8,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8])

    override def annotate(annotation: Any): CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4), b.makeLens(self, field5), b.makeLens(self, field6), b.makeLens(self, field7), b.makeLens(self, field8))

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
    extractField9: Z => A9,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9])

    override def annotate(annotation: Any): CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4), b.makeLens(self, field5), b.makeLens(self, field6), b.makeLens(self, field7), b.makeLens(self, field8), b.makeLens(self, field9))
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
    extractField10: Z => A10,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10])

    override def annotate(annotation: Any): CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4), b.makeLens(self, field5), b.makeLens(self, field6), b.makeLens(self, field7), b.makeLens(self, field8), b.makeLens(self, field9), b.makeLens(self, field10))

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
    extractField11: Z => A11,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10], Lens[Z, A11])

    override def annotate(annotation: Any): CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10], b.Lens[Z, A11]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4), b.makeLens(self, field5), b.makeLens(self, field6), b.makeLens(self, field7), b.makeLens(self, field8), b.makeLens(self, field9), b.makeLens(self, field10), b.makeLens(self, field11))

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
    extractField12: Z => A12,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10], Lens[Z, A11], Lens[Z, A12])

    override def annotate(annotation: Any): CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10], b.Lens[Z, A11], b.Lens[Z, A12]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4), b.makeLens(self, field5), b.makeLens(self, field6), b.makeLens(self, field7), b.makeLens(self, field8), b.makeLens(self, field9), b.makeLens(self, field10), b.makeLens(self, field11), b.makeLens(self, field12))

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
    extractField13: Z => A13,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10], Lens[Z, A11], Lens[Z, A12], Lens[Z, A13])

    override def annotate(annotation: Any): CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10], b.Lens[Z, A11], b.Lens[Z, A12], b.Lens[Z, A13]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4), b.makeLens(self, field5), b.makeLens(self, field6), b.makeLens(self, field7), b.makeLens(self, field8), b.makeLens(self, field9), b.makeLens(self, field10), b.makeLens(self, field11), b.makeLens(self, field12), b.makeLens(self, field13))

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
    extractField14: Z => A14,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10], Lens[Z, A11], Lens[Z, A12], Lens[Z, A13], Lens[Z, A14])

    override def annotate(annotation: Any): CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10], b.Lens[Z, A11], b.Lens[Z, A12], b.Lens[Z, A13], b.Lens[Z, A14]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4), b.makeLens(self, field5), b.makeLens(self, field6), b.makeLens(self, field7), b.makeLens(self, field8), b.makeLens(self, field9), b.makeLens(self, field10), b.makeLens(self, field11), b.makeLens(self, field12), b.makeLens(self, field13), b.makeLens(self, field14))

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
    extractField15: Z => A15,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10], Lens[Z, A11], Lens[Z, A12], Lens[Z, A13], Lens[Z, A14], Lens[Z, A15])

    override def annotate(annotation: Any): CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10], b.Lens[Z, A11], b.Lens[Z, A12], b.Lens[Z, A13], b.Lens[Z, A14], b.Lens[Z, A15]) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5),
        b.makeLens(self, field6),
        b.makeLens(self, field7),
        b.makeLens(self, field8),
        b.makeLens(self, field9),
        b.makeLens(self, field10),
        b.makeLens(self, field11),
        b.makeLens(self, field12),
        b.makeLens(self, field13),
        b.makeLens(self, field14),
        b.makeLens(self, field15)
      )

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
    extractField16: Z => A16,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10], Lens[Z, A11], Lens[Z, A12], Lens[Z, A13], Lens[Z, A14], Lens[Z, A15], Lens[Z, A16])

    override def annotate(annotation: Any): CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10], b.Lens[Z, A11], b.Lens[Z, A12], b.Lens[Z, A13], b.Lens[Z, A14], b.Lens[Z, A15], b.Lens[Z, A16]) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5),
        b.makeLens(self, field6),
        b.makeLens(self, field7),
        b.makeLens(self, field8),
        b.makeLens(self, field9),
        b.makeLens(self, field10),
        b.makeLens(self, field11),
        b.makeLens(self, field12),
        b.makeLens(self, field13),
        b.makeLens(self, field14),
        b.makeLens(self, field15),
        b.makeLens(self, field16)
      )

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
    extractField17: Z => A17,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10], Lens[Z, A11], Lens[Z, A12], Lens[Z, A13], Lens[Z, A14], Lens[Z, A15], Lens[Z, A16], Lens[Z, A17])

    override def annotate(annotation: Any): CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10], b.Lens[Z, A11], b.Lens[Z, A12], b.Lens[Z, A13], b.Lens[Z, A14], b.Lens[Z, A15], b.Lens[Z, A16], b.Lens[Z, A17]) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5),
        b.makeLens(self, field6),
        b.makeLens(self, field7),
        b.makeLens(self, field8),
        b.makeLens(self, field9),
        b.makeLens(self, field10),
        b.makeLens(self, field11),
        b.makeLens(self, field12),
        b.makeLens(self, field13),
        b.makeLens(self, field14),
        b.makeLens(self, field15),
        b.makeLens(self, field16),
        b.makeLens(self, field17)
      )

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
    extractField18: Z => A18,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10], Lens[Z, A11], Lens[Z, A12], Lens[Z, A13], Lens[Z, A14], Lens[Z, A15], Lens[Z, A16], Lens[Z, A17], Lens[Z, A18])

    override def annotate(annotation: Any): CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10], b.Lens[Z, A11], b.Lens[Z, A12], b.Lens[Z, A13], b.Lens[Z, A14], b.Lens[Z, A15], b.Lens[Z, A16], b.Lens[Z, A17], b.Lens[Z, A18]) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5),
        b.makeLens(self, field6),
        b.makeLens(self, field7),
        b.makeLens(self, field8),
        b.makeLens(self, field9),
        b.makeLens(self, field10),
        b.makeLens(self, field11),
        b.makeLens(self, field12),
        b.makeLens(self, field13),
        b.makeLens(self, field14),
        b.makeLens(self, field15),
        b.makeLens(self, field16),
        b.makeLens(self, field17),
        b.makeLens(self, field18)
      )

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
    extractField19: Z => A19,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10], Lens[Z, A11], Lens[Z, A12], Lens[Z, A13], Lens[Z, A14], Lens[Z, A15], Lens[Z, A16], Lens[Z, A17], Lens[Z, A18], Lens[Z, A19])

    override def annotate(annotation: Any): CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10], b.Lens[Z, A11], b.Lens[Z, A12], b.Lens[Z, A13], b.Lens[Z, A14], b.Lens[Z, A15], b.Lens[Z, A16], b.Lens[Z, A17], b.Lens[Z, A18], b.Lens[Z, A19]) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5),
        b.makeLens(self, field6),
        b.makeLens(self, field7),
        b.makeLens(self, field8),
        b.makeLens(self, field9),
        b.makeLens(self, field10),
        b.makeLens(self, field11),
        b.makeLens(self, field12),
        b.makeLens(self, field13),
        b.makeLens(self, field14),
        b.makeLens(self, field15),
        b.makeLens(self, field16),
        b.makeLens(self, field17),
        b.makeLens(self, field18),
        b.makeLens(self, field19)
      )

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
    extractField20: Z => A20,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10], Lens[Z, A11], Lens[Z, A12], Lens[Z, A13], Lens[Z, A14], Lens[Z, A15], Lens[Z, A16], Lens[Z, A17], Lens[Z, A18], Lens[Z, A19], Lens[Z, A20])

    override def annotate(annotation: Any): CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10], b.Lens[Z, A11], b.Lens[Z, A12], b.Lens[Z, A13], b.Lens[Z, A14], b.Lens[Z, A15], b.Lens[Z, A16], b.Lens[Z, A17], b.Lens[Z, A18], b.Lens[Z, A19], b.Lens[Z, A20]) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5),
        b.makeLens(self, field6),
        b.makeLens(self, field7),
        b.makeLens(self, field8),
        b.makeLens(self, field9),
        b.makeLens(self, field10),
        b.makeLens(self, field11),
        b.makeLens(self, field12),
        b.makeLens(self, field13),
        b.makeLens(self, field14),
        b.makeLens(self, field15),
        b.makeLens(self, field16),
        b.makeLens(self, field17),
        b.makeLens(self, field18),
        b.makeLens(self, field19),
        b.makeLens(self, field20)
      )

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
    extractField21: Z => A21,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10], Lens[Z, A11], Lens[Z, A12], Lens[Z, A13], Lens[Z, A14], Lens[Z, A15], Lens[Z, A16], Lens[Z, A17], Lens[Z, A18], Lens[Z, A19], Lens[Z, A20], Lens[Z, A21])

    override def annotate(annotation: Any): CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10], b.Lens[Z, A11], b.Lens[Z, A12], b.Lens[Z, A13], b.Lens[Z, A14], b.Lens[Z, A15], b.Lens[Z, A16], b.Lens[Z, A17], b.Lens[Z, A18], b.Lens[Z, A19], b.Lens[Z, A20], b.Lens[Z, A21]) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5),
        b.makeLens(self, field6),
        b.makeLens(self, field7),
        b.makeLens(self, field8),
        b.makeLens(self, field9),
        b.makeLens(self, field10),
        b.makeLens(self, field11),
        b.makeLens(self, field12),
        b.makeLens(self, field13),
        b.makeLens(self, field14),
        b.makeLens(self, field15),
        b.makeLens(self, field16),
        b.makeLens(self, field17),
        b.makeLens(self, field18),
        b.makeLens(self, field19),
        b.makeLens(self, field20),
        b.makeLens(self, field21)
      )

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
    extractField22: Z => A22,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[Z] { self =>
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = (Lens[Z, A1], Lens[Z, A2], Lens[Z, A3], Lens[Z, A4], Lens[Z, A5], Lens[Z, A6], Lens[Z, A7], Lens[Z, A8], Lens[Z, A9], Lens[Z, A10], Lens[Z, A11], Lens[Z, A12], Lens[Z, A13], Lens[Z, A14], Lens[Z, A15], Lens[Z, A16], Lens[Z, A17], Lens[Z, A18], Lens[Z, A19], Lens[Z, A20], Lens[Z, A21], Lens[Z, A22])

    override def annotate(annotation: Any): CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z] = copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): (b.Lens[Z, A1], b.Lens[Z, A2], b.Lens[Z, A3], b.Lens[Z, A4], b.Lens[Z, A5], b.Lens[Z, A6], b.Lens[Z, A7], b.Lens[Z, A8], b.Lens[Z, A9], b.Lens[Z, A10], b.Lens[Z, A11], b.Lens[Z, A12], b.Lens[Z, A13], b.Lens[Z, A14], b.Lens[Z, A15], b.Lens[Z, A16], b.Lens[Z, A17], b.Lens[Z, A18], b.Lens[Z, A19], b.Lens[Z, A20], b.Lens[Z, A21], b.Lens[Z, A22]) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5),
        b.makeLens(self, field6),
        b.makeLens(self, field7),
        b.makeLens(self, field8),
        b.makeLens(self, field9),
        b.makeLens(self, field10),
        b.makeLens(self, field11),
        b.makeLens(self, field12),
        b.makeLens(self, field13),
        b.makeLens(self, field14),
        b.makeLens(self, field15),
        b.makeLens(self, field16),
        b.makeLens(self, field17),
        b.makeLens(self, field18),
        b.makeLens(self, field19),
        b.makeLens(self, field20),
        b.makeLens(self, field21),
        b.makeLens(self, field22)
      )

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
