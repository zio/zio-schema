package zio.schema

import java.net.{ URI, URL }
import java.time.temporal.ChronoUnit

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

import zio.schema.internal.SourceLocation
import zio.schema.meta._
import zio.schema.validation._
import zio.{ Chunk, Unsafe }

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

  type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]]

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
  def <+>[B](that: Schema[B]): Schema[scala.util.Either[A, B]] = self.orElseEither(that)

  /**
   * The default value for a `Schema` of type `A`.
   */
  def defaultValue: scala.util.Either[String, A]

  /**
   * Chunk of annotations for this schema
   */
  def annotations: Chunk[Any]

  def ast: MetaSchema = MetaSchema.fromSchema(self)

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
   */
  def diff(thisValue: A, thatValue: A): Patch[A] = Differ.fromSchema(self)(thisValue, thatValue)

  /**
   * Patch value with a Patch.
   */
  def patch(oldValue: A, diff: Patch[A]): scala.util.Either[String, A] = diff.patch(oldValue)

  def fromDynamic(value: DynamicValue): scala.util.Either[String, A] =
    value.toTypedValue(self).toEither.left.map(_.toString)

  def makeAccessors(b: AccessorBuilder): Accessors[b.Lens, b.Prism, b.Traversal]

  /**
   *  Generate a homomorphism from A to B iff A and B are homomorphic
   */
  def migrate[B](newSchema: Schema[B]): Either[String, A => scala.util.Either[String, B]] =
    Migration.derive(MetaSchema.fromSchema(self), MetaSchema.fromSchema(newSchema)).map { transforms => (a: A) =>
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
  def orElseEither[B](that: Schema[B]): Schema[scala.util.Either[A, B]] = Schema.Either(self, that)

  def repeated: Schema[Chunk[A]] = Schema.chunk(self)

  def serializable: Schema[Schema[A]] =
    MetaSchema
      .fromSchema(self)
      .asInstanceOf[Schema[Schema[_]]]
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
  def transform[B](f: A => B, g: B => A)(implicit loc: SourceLocation): Schema[B] =
    Schema.Transform[A, B, SourceLocation](self, a => Right(f(a)), b => Right(g(b)), annotations, loc)

  /**
   * Transforms this `Schema[A]` into a `Schema[B]`, by supplying two functions that can transform
   * between `A` and `B` (possibly failing in some cases).
   */
  def transformOrFail[B](f: A => scala.util.Either[String, B], g: B => scala.util.Either[String, A])(
    implicit loc: SourceLocation
  ): Schema[B] =
    Schema.Transform[A, B, SourceLocation](self, f, g, annotations, loc)

  def validate(value: A)(implicit schema: Schema[A]): Chunk[ValidationError] = Schema.validate[A](value)

  /**
   * Returns a new schema that combines this schema and the specified schema together, modeling
   * their tuple composition.
   */
  def zip[B](that: Schema[B]): Schema[(A, B)] = Schema.Tuple2(self, that)
}

object Schema extends SchemaEquality {
  def apply[A](implicit schema: Schema[A]): Schema[A] = schema

  def defer[A](schema: => Schema[A]): Schema[A] = Lazy(() => schema)

  def enumeration[A, C <: CaseSet.Aux[A]](id: TypeId, caseSet: C): Schema[A] =
    EnumN(id, caseSet, Chunk.empty)

  def fail[A](message: String): Schema[A] = Fail(message)

  def first[A](schema: Schema[(A, Unit)]): Schema[A] =
    schema.transform[A](_._1, a => (a, ()))

  @tailrec
  def force[A](schema: Schema[A]): Schema[A] =
    schema match {
      case Schema.Lazy(f) => force(f())
      case _              => schema
    }

  def record(id: TypeId, fields: Field[ListMap[String, _], _]*): Schema[ListMap[String, _]] =
    GenericRecord(id, FieldSet(fields: _*))

  def record(id: TypeId, fieldSet: FieldSet): Schema[ListMap[String, _]] =
    GenericRecord(id, fieldSet)

  def second[A](schema: Schema[(Unit, A)]): Schema[A] =
    schema.transform[A](_._2, a => ((), a))

  def singleton[A](instance: A): Schema[A] = Schema[Unit].transform(_ => instance, _ => ())

  def validate[A](value: A)(implicit schema: Schema[A]): Chunk[ValidationError] = {
    def loop[A](value: A, schema: Schema[A]): Chunk[ValidationError] =
      schema match {
        case Sequence(schema, _, toChunk, _, _) =>
          toChunk(value).flatMap(value => loop(value, schema))
        case Transform(schema, _, g, _, _) =>
          g(value) match {
            case Right(value) => loop(value, schema)
            case Left(error)  => Chunk(ValidationError.Generic(error))
          }
        case Primitive(_, _) => Chunk.empty
        case optional @ Optional(schema, _) =>
          value.asInstanceOf[Option[optional.OptionalType]] match {
            case Some(value) => loop(value, schema)
            case None        => Chunk.empty
          }
        case tuple @ Schema.Tuple2(left, right, _) =>
          loop(tuple.extract1(value), left) ++ loop(tuple.extract2(value), right)
        case l @ Lazy(_) =>
          loop(value, l.schema)
        case Schema.Map(ks, vs, _) =>
          Chunk.fromIterable(value.keys).flatMap(loop(_, ks)) ++ Chunk.fromIterable(value.values).flatMap(loop(_, vs))
        case set @ Schema.Set(as, _) =>
          Chunk.fromIterable(value.asInstanceOf[scala.collection.Set[set.ElementType]]).flatMap(loop(_, as))
        case enumeration: Enum[_] =>
          enumeration.caseOf(value) match {
            case Some(c) =>
              loop(c.deconstruct(value), c.schema.asInstanceOf[Schema[Any]])
            case None =>
              Chunk.empty // TODO could consider this a fatal error
          }
        case record: Record[_] =>
          val fieldValues = Unsafe.unsafe { implicit unsafe =>
            record.deconstruct(value)
          }
          record.fields
            .zip(fieldValues)
            .foldLeft[Chunk[ValidationError]](Chunk.empty) {
              case (acc, (field, fieldValue)) =>
                val validation = field.validation.asInstanceOf[Validation[Any]]
                validation.validate(fieldValue).swap.getOrElse(Chunk.empty) ++ acc ++ loop(
                  fieldValue,
                  field.schema.asInstanceOf[Schema[Any]]
                )
            }
            .reverse
        case either @ Schema.Either(left, right, _) =>
          value.asInstanceOf[scala.util.Either[either.LeftType, either.RightType]] match {
            case Left(value)  => loop(value, left)
            case Right(value) => loop(value, right)
          }
        case Dynamic(_) => Chunk.empty
        case Fail(_, _) => Chunk.empty
      }
    loop(value, schema)
  }

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

  implicit val dynamicValue: Schema[DynamicValue] = Schema.Dynamic()

  implicit def chunk[A](implicit schemaA: Schema[A]): Schema[Chunk[A]] =
    Schema.Sequence[Chunk[A], A, String](schemaA, identity, identity, Chunk.empty, "Chunk")

  implicit def map[K, V](
    implicit keySchema: Schema[K],
    valueSchema: Schema[V]
  ): Schema[scala.collection.immutable.Map[K, V]] =
    Schema.Map(keySchema, valueSchema, Chunk.empty)

  implicit def set[A](implicit schemaA: Schema[A]): Schema[scala.collection.immutable.Set[A]] =
    Schema.Set(schemaA, Chunk.empty)

  implicit def either[A, B](implicit left: Schema[A], right: Schema[B]): Schema[scala.util.Either[A, B]] =
    Schema.Either(left, right)

  implicit def list[A](implicit schemaA: Schema[A]): Schema[List[A]] =
    Schema.Sequence[List[A], A, String](schemaA, _.toList, Chunk.fromIterable(_), Chunk.empty, "List")

  implicit def option[A](implicit element: Schema[A]): Schema[Option[A]] =
    Optional(element)

  implicit def primitive[A](implicit standardType: StandardType[A]): Schema[A] =
    Primitive(standardType, Chunk.empty)

  def toDynamic[A](a: A)(implicit schema: Schema[A]): DynamicValue = schema.toDynamic(a)

  implicit def vector[A](implicit element: Schema[A]): Schema[Vector[A]] =
    Schema.Sequence[Vector[A], A, String](element, _.toVector, Chunk.fromIterable(_), Chunk.empty, "Vector")

  implicit val url: Schema[java.net.URL] =
    Schema[String].transformOrFail(
      string =>
        try {
          Right(new URL(string))
        } catch { case _: Exception => Left(s"Invalid URL: $string") },
      url => Right(url.toString)
    )

  implicit def schemaSchema[A]: Schema[Schema[A]] = Schema[MetaSchema].transform(
    _.toSchema.asInstanceOf[Schema[A]],
    _.ast
  )

  implicit val uri: Schema[java.net.URI] =
    Schema[String].transformOrFail(
      string =>
        try {
          Right(new URI(string))
        } catch { case _: Exception => Left(s"Invalid URI: $string") },
      uri => Right(uri.toString)
    )

  implicit def standardSchema[A]: Schema[StandardType[A]] = Schema[String].transformOrFail[StandardType[A]](
    string =>
      StandardType
        .fromString(string)
        .asInstanceOf[Option[StandardType[A]]]
        .toRight(s"Invalid StandardType tag ${string}"),
    standardType => Right(standardType.tag)
  )

  sealed trait Enum[Z] extends Schema[Z] {
    private lazy val casesMap: scala.collection.immutable.Map[String, Case[Z, _]] = cases.map(c => c.id -> c).toMap
    def id: TypeId

    def cases: Chunk[Case[Z, _]]

    def caseOf(id: String): Option[Case[Z, _]] =
      casesMap.get(id)

    def caseOf(z: Z): Option[Case[Z, _]] = cases.find(_.isCase(z))

    override def toString: String = s"${this.getClass.getSimpleName}($id)"
  }

  sealed trait Field[R, A] {
    type Field <: Singleton with String

    def name: Field
    def schema: Schema[A]
    def annotations: Chunk[Any]
    def validation: Validation[A]
    def get: R => A
    def set: (R, A) => R

    override def toString: String = s"Field($name,$schema)"
  }

  object Field {

    type WithFieldName[R, F <: Singleton with String, A] = Field[R, A] {
      type Field = F
    }

    def apply[R, A](
      name0: String,
      schema0: Schema[A],
      annotations0: Chunk[Any] = Chunk.empty,
      validation0: Validation[A] = Validation.succeed[A],
      get0: R => A,
      set0: (R, A) => R
    ): Field[R, A] = new Field[R, A] {

      override type Field = name0.type

      def name: Field               = name0.asInstanceOf[Field]
      def schema: Schema[A]         = schema0
      def annotations: Chunk[Any]   = annotations0
      def validation: Validation[A] = validation0
      def get: R => A               = get0
      def set: (R, A) => R          = set0
    }

    def unapply[R, A](field: Field[R, A]): Some[(String, Schema[A], Chunk[Any], Validation[A], R => A, (R, A) => R)] =
      Some[(String, Schema[A], Chunk[Any], Validation[A], R => A, (R, A) => R)](
        (field.name, field.schema, field.annotations, field.validation, field.get, field.set)
      )
  }

  sealed trait Record[R] extends Schema[R] {
    self =>

    type Terms
    type FieldNames

    def fields: Chunk[Field[R, _]]

    def construct(fieldValues: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, R]

    def deconstruct(value: R)(implicit unsafe: Unsafe): Chunk[Any]

    def id: TypeId

    def defaultValue: scala.util.Either[String, R] =
      Unsafe.unsafe { implicit unsafe =>
        self.fields
          .map(_.schema.defaultValue)
          .foldLeft[scala.util.Either[String, Chunk[R]]](Right(Chunk.empty)) {
            case (e @ Left(_), _)              => e
            case (_, Left(e))                  => Left[String, Chunk[R]](e)
            case (Right(values), Right(value)) => Right[String, Chunk[R]](values :+ value.asInstanceOf[R])
          }
          .flatMap(self.construct)
      }
  }

  sealed trait Collection[Col, Elem] extends Schema[Col]

  final case class Sequence[Col, Elem, I](
    elementSchema: Schema[Elem],
    fromChunk: Chunk[Elem] => Col,
    toChunk: Col => Chunk[Elem],
    override val annotations: Chunk[Any] = Chunk.empty,
    identity: I
  ) extends Collection[Col, Elem] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Traversal[Col, Elem]

    override def annotate(annotation: Any): Sequence[Col, Elem, I] = copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Col] =
      elementSchema.defaultValue.map(fromChunk.compose(Chunk(_)))

    override def makeAccessors(b: AccessorBuilder): b.Traversal[Col, Elem] = b.makeTraversal(self, elementSchema)

    override def toString: String = s"Sequence($elementSchema, $identity)"

  }

  final case class Transform[A, B, I](
    schema: Schema[A],
    f: A => scala.util.Either[String, B],
    g: B => scala.util.Either[String, A],
    annotations: Chunk[Any],
    identity: I
  ) extends Schema[B] {
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = schema.Accessors[Lens, Prism, Traversal]

    def defaultValue: scala.util.Either[String, B] = schema.defaultValue.flatMap(f)

    override def makeAccessors(b: AccessorBuilder): schema.Accessors[b.Lens, b.Prism, b.Traversal] =
      schema.makeAccessors(b)

    override def annotate(annotation: Any): Transform[A, B, I] = copy(annotations = annotations :+ annotation)

    override def serializable: Schema[Schema[B]] =
      MetaSchema
        .fromSchema(schema)
        .asInstanceOf[Schema[Schema[_]]]
        .transformOrFail(
          s => s.coerce(schema).flatMap(s1 => Right(s1.transformOrFail(f, g))),
          s => Right(s.transformOrFail(g, f).ast.toSchema)
        )

    override def toString: String = s"Transform($schema, $identity)"

  }

  final case class Primitive[A](standardType: StandardType[A], annotations: Chunk[Any] = Chunk.empty)
      extends Schema[A] {
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Unit

    override def annotate(annotation: Any): Primitive[A] = copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, A] = standardType.defaultValue

    override def makeAccessors(b: AccessorBuilder): Unit = ()
  }

  final case class Optional[A](schema: Schema[A], annotations: Chunk[Any] = Chunk.empty) extends Schema[Option[A]] {
    self =>
    type OptionalType = A

    val some = "Some"
    val none = "None"

    private[schema] lazy val someCodec: Schema[Some[A]] =
      schema.transform(a => Some(a), _.get)

    override def annotate(annotation: Any): Optional[A] = copy(annotations = annotations :+ annotation)

    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Prism[some.type, Option[A], Some[A]], Prism[none.type, Option[A], None.type])

    lazy val toEnum: Enum2[Some[A], None.type, Option[A]] = Enum2(
      TypeId.parse("zio.schema.Schema.Optional"),
      Case[Option[A], Some[A]](
        "Some",
        someCodec,
        _.asInstanceOf[Some[A]],
        _.asInstanceOf[Option[A]],
        _.isInstanceOf[Some[A]],
        Chunk.empty
      ),
      Case[Option[A], None.type](
        "None",
        singleton(None),
        _.asInstanceOf[None.type],
        _.asInstanceOf[Option[A]],
        _.isInstanceOf[None.type],
        Chunk.empty
      ),
      Chunk.empty
    )

    def defaultValue: scala.util.Either[String, Option[A]] = Right(None)

    override def makeAccessors(
      b: AccessorBuilder
    ): (b.Prism[some.type, Option[A], Some[A]], b.Prism[none.type, Option[A], None.type]) =
      b.makePrism(toEnum, toEnum.case1) -> b.makePrism(toEnum, toEnum.case2)

  }

  final case class Fail[A](message: String, annotations: Chunk[Any] = Chunk.empty) extends Schema[A] {
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Unit

    override def annotate(annotation: Any): Fail[A] = copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, A] = Left(message)

    override def makeAccessors(b: AccessorBuilder): Unit = ()
  }

  final case class Tuple2[A, B](left: Schema[A], right: Schema[B], annotations: Chunk[Any] = Chunk.empty)
      extends Schema[(A, B)] {
    self =>

    val first  = "_1"
    val second = "_2"
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Lens[first.type, (A, B), A], Lens[second.type, (A, B), B])

    override def annotate(annotation: Any): Tuple2[A, B] = copy(annotations = annotations :+ annotation)

    val toRecord: CaseClass2[A, B, (A, B)] = CaseClass2[A, B, (A, B)](
      id0 = TypeId.parse("zio.schema.Schema.CaseClass2"),
      field01 = Field[(A, B), A]("_1", left, get0 = _._1, set0 = (ab, a) => (a, ab._2)),
      field02 = Field[(A, B), B]("_2", right, get0 = _._2, set0 = (a, b) => (a._1, b)),
      construct0 = (a, b) => (a, b),
      annotations
    )

    override def defaultValue: scala.util.Either[String, (A, B)] =
      left.defaultValue.flatMap(a => right.defaultValue.map(b => (a, b)))

    override def makeAccessors(b: AccessorBuilder): (b.Lens[first.type, (A, B), A], b.Lens[second.type, (A, B), B]) =
      b.makeLens(toRecord, toRecord.field1) -> b.makeLens(toRecord, toRecord.field2)

    def extract1(value: (A, B)): A = value._1
    def extract2(value: (A, B)): B = value._2
  }

  final case class Either[A, B](left: Schema[A], right: Schema[B], annotations: Chunk[Any] = Chunk.empty)
      extends Schema[scala.util.Either[A, B]] {
    self =>
    type LeftType  = A
    type RightType = B

    val leftSingleton  = "Left"
    val rightSingleton = "Right"
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (
        Prism[rightSingleton.type, scala.util.Either[A, B], Right[Nothing, B]],
        Prism[leftSingleton.type, scala.util.Either[A, B], Left[A, Nothing]]
      )

    override def annotate(annotation: Any): Schema.Either[A, B] = copy(annotations = annotations :+ annotation)

    val rightSchema: Schema[Right[Nothing, B]] = right.transform(b => Right(b), _.value)
    val leftSchema: Schema[Left[A, Nothing]]   = left.transform(a => Left(a), _.value)

    val toEnum: Enum2[Right[Nothing, B], Left[A, Nothing], scala.util.Either[A, B]] = Enum2(
      TypeId.parse("zio.schema.Schema.Either"),
      Case(
        "Right",
        rightSchema,
        _.asInstanceOf[Right[Nothing, B]],
        _.asInstanceOf[scala.util.Either[A, B]],
        (e: scala.util.Either[A, B]) => e.isRight,
        Chunk.empty
      ),
      Case(
        "Left",
        leftSchema,
        _.asInstanceOf[Left[A, Nothing]],
        _.asInstanceOf[scala.util.Either[A, B]],
        (e: scala.util.Either[A, B]) => e.isLeft,
        Chunk.empty
      ),
      Chunk.empty
    )

    override def defaultValue: scala.util.Either[String, scala.util.Either[A, B]] =
      left.defaultValue match {
        case Right(a) => Right(Left(a))
        case _ =>
          right.defaultValue match {
            case Right(b) => Right(Right(b))
            case _        => Left("unable to extract default value for Either")
          }
      }

    override def makeAccessors(
      b: AccessorBuilder
    ): (
      b.Prism[rightSingleton.type, scala.util.Either[A, B], Right[Nothing, B]],
      b.Prism[leftSingleton.type, scala.util.Either[A, B], Left[A, Nothing]]
    ) =
      b.makePrism(toEnum, toEnum.case1) -> b.makePrism(toEnum, toEnum.case2)

  }

  final case class Lazy[A](private val schema0: () => Schema[A]) extends Schema[A] {
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = schema.Accessors[Lens, Prism, Traversal]

    override def annotate(annotation: Any): Lazy[A] = Lazy(() => schema0().annotate(annotation))

    lazy val schema: Schema[A] = schema0()

    def defaultValue: scala.util.Either[String, A] = schema.defaultValue

    override def makeAccessors(b: AccessorBuilder): schema.Accessors[b.Lens, b.Prism, b.Traversal] =
      schema.makeAccessors(b)

    override def toString: String = "$Lazy$"

    override def annotations: Chunk[Any] = schema0().annotations
  }

  final case class Map[K, V](
    keySchema: Schema[K],
    valueSchema: Schema[V],
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Collection[scala.collection.immutable.Map[K, V], (K, V)] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      Traversal[scala.collection.immutable.Map[K, V], (K, V)]

    override def annotate(annotation: Any): Map[K, V] = copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, scala.collection.immutable.Map[K, V]] =
      keySchema.defaultValue.flatMap(
        defaultKey =>
          valueSchema.defaultValue.map(defaultValue => scala.collection.immutable.Map(defaultKey -> defaultValue))
      )

    override def makeAccessors(b: AccessorBuilder): b.Traversal[scala.collection.immutable.Map[K, V], (K, V)] =
      b.makeTraversal(self, keySchema <*> valueSchema)
  }

  final case class Set[A](elementSchema: Schema[A], override val annotations: Chunk[Any] = Chunk.empty)
      extends Collection[scala.collection.immutable.Set[A], A] {
    self =>
    type ElementType = A

    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      Traversal[scala.collection.immutable.Set[A], A]

    override def annotate(annotation: Any): Set[A] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, scala.collection.immutable.Set[A]] =
      elementSchema.defaultValue.map(scala.collection.immutable.Set(_))

    override def makeAccessors(b: AccessorBuilder): b.Traversal[scala.collection.immutable.Set[A], A] =
      b.makeTraversal(self, elementSchema)
  }

  final case class Dynamic(override val annotations: Chunk[Any] = Chunk.empty) extends Schema[DynamicValue] {
    def id: TypeId = DynamicValue.typeId

    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Unit

    /**
     * The default value for a `Schema` of type `A`.
     */
    override def defaultValue: scala.util.Either[String, DynamicValue] =
      Right(DynamicValue.NoneValue)

    /**
     * Returns a new schema that with `annotation`
     */
    override def annotate(annotation: Any): Schema[DynamicValue] =
      this.copy(annotations = annotations :+ annotation)

    override def makeAccessors(b: AccessorBuilder): Unit = ()
  }

// # ENUM SCHEMAS

  sealed case class Case[R, A](
    id: String,
    schema: Schema[A],
    private val unsafeDeconstruct: R => A,
    construct: A => R,
    isCase: R => Boolean,
    annotations: Chunk[Any] = Chunk.empty
  ) { self =>

    def deconstruct(r: R): A = unsafeDeconstruct(r)

    def deconstructOption(r: R): Option[A] =
      if (isCase(r)) Some(unsafeDeconstruct(r)) else None

    override def toString: String = s"Case($id,$schema,$annotations)"
  }

  sealed case class Enum1[A, Z](id: TypeId, case1: Case[Z, A], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Prism[case1.id.type, Z, A]

    override def annotate(annotation: Any): Enum1[A, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): b.Prism[case1.id.type, Z, A] = b.makePrism(self, case1)

    override def cases: Chunk[Case[Z, A]] =
      Chunk(case1)
  }

  sealed case class Enum2[A1, A2, Z](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Prism[case1.id.type, Z, A1], Prism[case2.id.type, Z, A2])

    override def annotate(annotation: Any): Enum2[A1, A2, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (b.Prism[case1.id.type, Z, A1], b.Prism[case2.id.type, Z, A2]) =
      (b.makePrism(self, case1), b.makePrism(self, case2))

    override def cases: Chunk[Case[Z, _]] = Chunk(case1, case2)
  }

  sealed case class Enum3[A1, A2, A3, Z](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Prism[case1.id.type, Z, A1], Prism[case2.id.type, Z, A2], Prism[case3.id.type, Z, A3])

    override def annotate(annotation: Any): Enum3[A1, A2, A3, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(
      b: AccessorBuilder
    ): (b.Prism[case1.id.type, Z, A1], b.Prism[case2.id.type, Z, A2], b.Prism[case3.id.type, Z, A3]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3))

    override def cases: Chunk[Case[Z, _]] = Chunk(case1, case2, case3)
  }

  sealed case class Enum4[A1, A2, A3, A4, Z](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (
        Prism[case1.id.type, Z, A1],
        Prism[case2.id.type, Z, A2],
        Prism[case3.id.type, Z, A3],
        Prism[case4.id.type, Z, A4]
      )

    override def annotate(annotation: Any): Enum4[A1, A2, A3, A4, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4]
    ) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3), b.makePrism(self, case4))

    override def cases: Chunk[Case[Z, _]] = Chunk(case1, case2, case3, case4)
  }

  sealed case class Enum5[A1, A2, A3, A4, A5, Z](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (
        Prism[case1.id.type, Z, A1],
        Prism[case2.id.type, Z, A2],
        Prism[case3.id.type, Z, A3],
        Prism[case4.id.type, Z, A4],
        Prism[case5.id.type, Z, A5]
      )

    override def annotate(annotation: Any): Enum5[A1, A2, A3, A4, A5, Z] = copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(
      b: AccessorBuilder
    ): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5]
    ) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5)
      )

    override def cases: Chunk[Case[Z, _]] = Chunk(case1, case2, case3, case4, case5)
  }

  sealed case class Enum6[A1, A2, A3, A4, A5, A6, Z](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (
        Prism[case1.id.type, Z, A1],
        Prism[case2.id.type, Z, A2],
        Prism[case3.id.type, Z, A3],
        Prism[case4.id.type, Z, A4],
        Prism[case5.id.type, Z, A5],
        Prism[case6.id.type, Z, A6]
      )

    override def annotate(annotation: Any): Enum6[A1, A2, A3, A4, A5, A6, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(
      b: AccessorBuilder
    ): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6]
    ) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5),
        b.makePrism(self, case6)
      )

    override def cases: Chunk[Case[Z, _]] = Chunk(case1, case2, case3, case4, case5, case6)
  }

  sealed case class Enum7[A1, A2, A3, A4, A5, A6, A7, Z](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (
        Prism[case1.id.type, Z, A1],
        Prism[case2.id.type, Z, A2],
        Prism[case3.id.type, Z, A3],
        Prism[case4.id.type, Z, A4],
        Prism[case5.id.type, Z, A5],
        Prism[case6.id.type, Z, A6],
        Prism[case7.id.type, Z, A7]
      )

    override def annotate(annotation: Any): Enum7[A1, A2, A3, A4, A5, A6, A7, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7]
    ) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5),
        b.makePrism(self, case6),
        b.makePrism(self, case7)
      )

    override def cases: Chunk[Case[Z, _]] = Chunk(case1, case2, case3, case4, case5, case6, case7)
  }

  sealed case class Enum8[A1, A2, A3, A4, A5, A6, A7, A8, Z](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (
        Prism[case1.id.type, Z, A1],
        Prism[case2.id.type, Z, A2],
        Prism[case3.id.type, Z, A3],
        Prism[case4.id.type, Z, A4],
        Prism[case5.id.type, Z, A5],
        Prism[case6.id.type, Z, A6],
        Prism[case7.id.type, Z, A7],
        Prism[case8.id.type, Z, A8]
      )

    override def annotate(annotation: Any): Enum8[A1, A2, A3, A4, A5, A6, A7, A8, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8]
    ) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5),
        b.makePrism(self, case6),
        b.makePrism(self, case7),
        b.makePrism(self, case8)
      )

    override def cases: Chunk[Case[Z, _]] = Chunk(case1, case2, case3, case4, case5, case6, case7, case8)
  }

  sealed case class Enum9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9]
    )

    override def annotate(annotation: Any): Enum9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9]
    ) =
      (
        b.makePrism(self, case1),
        b.makePrism(self, case2),
        b.makePrism(self, case3),
        b.makePrism(self, case4),
        b.makePrism(self, case5),
        b.makePrism(self, case6),
        b.makePrism(self, case7),
        b.makePrism(self, case8),
        b.makePrism(self, case9)
      )

    override def cases: Chunk[Case[Z, _]] = Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9)
  }

  sealed case class Enum10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10]
    )

    override def annotate(annotation: Any): Enum10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10]
    ) =
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
        b.makePrism(self, case10)
      )

    override def cases: Chunk[Case[Z, _]] = Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10)
  }

  sealed case class Enum11[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    Z
  ](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    case11: Case[Z, A11],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10],
      Prism[case11.id.type, Z, A11]
    )

    override def annotate(annotation: Any): Enum11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10],
      b.Prism[case11.id.type, Z, A11]
    ) =
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
        b.makePrism(self, case11)
      )

    override def cases: Chunk[Case[Z, _]] =
      Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11)
  }

  sealed case class Enum12[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    Z
  ](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    case11: Case[Z, A11],
    case12: Case[Z, A12],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10],
      Prism[case11.id.type, Z, A11],
      Prism[case12.id.type, Z, A12]
    )

    override def annotate(annotation: Any): Enum12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10],
      b.Prism[case11.id.type, Z, A11],
      b.Prism[case12.id.type, Z, A12]
    ) =
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
        b.makePrism(self, case12)
      )

    override def cases: Chunk[Case[Z, _]] = Chunk(
      case1,
      case2,
      case3,
      case4,
      case5,
      case6,
      case7,
      case8,
      case9,
      case10,
      case11,
      case12
    )
  }

  sealed case class Enum13[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    Z
  ](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    case11: Case[Z, A11],
    case12: Case[Z, A12],
    case13: Case[Z, A13],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10],
      Prism[case11.id.type, Z, A11],
      Prism[case12.id.type, Z, A12],
      Prism[case13.id.type, Z, A13]
    )

    override def annotate(annotation: Any): Enum13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10],
      b.Prism[case11.id.type, Z, A11],
      b.Prism[case12.id.type, Z, A12],
      b.Prism[case13.id.type, Z, A13]
    ) =
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
        b.makePrism(self, case13)
      )

    override def cases: Chunk[Case[Z, _]] = Chunk(
      case1,
      case2,
      case3,
      case4,
      case5,
      case6,
      case7,
      case8,
      case9,
      case10,
      case11,
      case12,
      case13
    )
  }

  sealed case class Enum14[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    Z
  ](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    case11: Case[Z, A11],
    case12: Case[Z, A12],
    case13: Case[Z, A13],
    case14: Case[Z, A14],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10],
      Prism[case11.id.type, Z, A11],
      Prism[case12.id.type, Z, A12],
      Prism[case13.id.type, Z, A13],
      Prism[case14.id.type, Z, A14]
    )

    override def annotate(annotation: Any): Enum14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10],
      b.Prism[case11.id.type, Z, A11],
      b.Prism[case12.id.type, Z, A12],
      b.Prism[case13.id.type, Z, A13],
      b.Prism[case14.id.type, Z, A14]
    ) =
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
        b.makePrism(self, case14)
      )

    override def cases: Chunk[Case[Z, _]] = Chunk(
      case1,
      case2,
      case3,
      case4,
      case5,
      case6,
      case7,
      case8,
      case9,
      case10,
      case11,
      case12,
      case13,
      case14
    )
  }

  sealed case class Enum15[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    Z
  ](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    case11: Case[Z, A11],
    case12: Case[Z, A12],
    case13: Case[Z, A13],
    case14: Case[Z, A14],
    case15: Case[Z, A15],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10],
      Prism[case11.id.type, Z, A11],
      Prism[case12.id.type, Z, A12],
      Prism[case13.id.type, Z, A13],
      Prism[case14.id.type, Z, A14],
      Prism[case15.id.type, Z, A15]
    )

    override def annotate(
      annotation: Any
    ): Enum15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10],
      b.Prism[case11.id.type, Z, A11],
      b.Prism[case12.id.type, Z, A12],
      b.Prism[case13.id.type, Z, A13],
      b.Prism[case14.id.type, Z, A14],
      b.Prism[case15.id.type, Z, A15]
    ) =
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

    override def cases: Chunk[Case[Z, _]] = Chunk(
      case1,
      case2,
      case3,
      case4,
      case5,
      case6,
      case7,
      case8,
      case9,
      case10,
      case11,
      case12,
      case13,
      case14,
      case15
    )
  }

  sealed case class Enum16[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    Z
  ](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    case11: Case[Z, A11],
    case12: Case[Z, A12],
    case13: Case[Z, A13],
    case14: Case[Z, A14],
    case15: Case[Z, A15],
    case16: Case[Z, A16],
    override val annotations: Chunk[Any]
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10],
      Prism[case11.id.type, Z, A11],
      Prism[case12.id.type, Z, A12],
      Prism[case13.id.type, Z, A13],
      Prism[case14.id.type, Z, A14],
      Prism[case15.id.type, Z, A15],
      Prism[case16.id.type, Z, A16]
    )

    override def annotate(
      annotation: Any
    ): Enum16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10],
      b.Prism[case11.id.type, Z, A11],
      b.Prism[case12.id.type, Z, A12],
      b.Prism[case13.id.type, Z, A13],
      b.Prism[case14.id.type, Z, A14],
      b.Prism[case15.id.type, Z, A15],
      b.Prism[case16.id.type, Z, A16]
    ) =
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

    override def cases: Chunk[Case[Z, _]] = Chunk(
      case1,
      case2,
      case3,
      case4,
      case5,
      case6,
      case7,
      case8,
      case9,
      case10,
      case11,
      case12,
      case13,
      case14,
      case15,
      case16
    )
  }

  sealed case class Enum17[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    Z
  ](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    case11: Case[Z, A11],
    case12: Case[Z, A12],
    case13: Case[Z, A13],
    case14: Case[Z, A14],
    case15: Case[Z, A15],
    case16: Case[Z, A16],
    case17: Case[Z, A17],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10],
      Prism[case11.id.type, Z, A11],
      Prism[case12.id.type, Z, A12],
      Prism[case13.id.type, Z, A13],
      Prism[case14.id.type, Z, A14],
      Prism[case15.id.type, Z, A15],
      Prism[case16.id.type, Z, A16],
      Prism[case17.id.type, Z, A17]
    )

    override def annotate(
      annotation: Any
    ): Enum17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10],
      b.Prism[case11.id.type, Z, A11],
      b.Prism[case12.id.type, Z, A12],
      b.Prism[case13.id.type, Z, A13],
      b.Prism[case14.id.type, Z, A14],
      b.Prism[case15.id.type, Z, A15],
      b.Prism[case16.id.type, Z, A16],
      b.Prism[case17.id.type, Z, A17]
    ) =
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

    override def cases: Chunk[Case[Z, _]] = Chunk(
      case1,
      case2,
      case3,
      case4,
      case5,
      case6,
      case7,
      case8,
      case9,
      case10,
      case11,
      case12,
      case13,
      case14,
      case15,
      case16,
      case17
    )
  }

  sealed case class Enum18[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    Z
  ](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    case11: Case[Z, A11],
    case12: Case[Z, A12],
    case13: Case[Z, A13],
    case14: Case[Z, A14],
    case15: Case[Z, A15],
    case16: Case[Z, A16],
    case17: Case[Z, A17],
    case18: Case[Z, A18],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10],
      Prism[case11.id.type, Z, A11],
      Prism[case12.id.type, Z, A12],
      Prism[case13.id.type, Z, A13],
      Prism[case14.id.type, Z, A14],
      Prism[case15.id.type, Z, A15],
      Prism[case16.id.type, Z, A16],
      Prism[case17.id.type, Z, A17],
      Prism[case18.id.type, Z, A18]
    )

    override def annotate(
      annotation: Any
    ): Enum18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10],
      b.Prism[case11.id.type, Z, A11],
      b.Prism[case12.id.type, Z, A12],
      b.Prism[case13.id.type, Z, A13],
      b.Prism[case14.id.type, Z, A14],
      b.Prism[case15.id.type, Z, A15],
      b.Prism[case16.id.type, Z, A16],
      b.Prism[case17.id.type, Z, A17],
      b.Prism[case18.id.type, Z, A18]
    ) =
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

    override def cases: Chunk[Case[Z, _]] = Chunk(
      case1,
      case2,
      case3,
      case4,
      case5,
      case6,
      case7,
      case8,
      case9,
      case10,
      case11,
      case12,
      case13,
      case14,
      case15,
      case16,
      case17,
      case18
    )
  }

  sealed case class Enum19[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    Z
  ](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    case11: Case[Z, A11],
    case12: Case[Z, A12],
    case13: Case[Z, A13],
    case14: Case[Z, A14],
    case15: Case[Z, A15],
    case16: Case[Z, A16],
    case17: Case[Z, A17],
    case18: Case[Z, A18],
    case19: Case[Z, A19],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10],
      Prism[case11.id.type, Z, A11],
      Prism[case12.id.type, Z, A12],
      Prism[case13.id.type, Z, A13],
      Prism[case14.id.type, Z, A14],
      Prism[case15.id.type, Z, A15],
      Prism[case16.id.type, Z, A16],
      Prism[case17.id.type, Z, A17],
      Prism[case18.id.type, Z, A18],
      Prism[case19.id.type, Z, A19]
    )

    override def annotate(
      annotation: Any
    ): Enum19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10],
      b.Prism[case11.id.type, Z, A11],
      b.Prism[case12.id.type, Z, A12],
      b.Prism[case13.id.type, Z, A13],
      b.Prism[case14.id.type, Z, A14],
      b.Prism[case15.id.type, Z, A15],
      b.Prism[case16.id.type, Z, A16],
      b.Prism[case17.id.type, Z, A17],
      b.Prism[case18.id.type, Z, A18],
      b.Prism[case19.id.type, Z, A19]
    ) =
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

    override def cases: Chunk[Case[Z, _]] = Chunk(
      case1,
      case2,
      case3,
      case4,
      case5,
      case6,
      case7,
      case8,
      case9,
      case10,
      case11,
      case12,
      case13,
      case14,
      case15,
      case16,
      case17,
      case18,
      case19
    )
  }

  sealed case class Enum20[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    Z
  ](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    case11: Case[Z, A11],
    case12: Case[Z, A12],
    case13: Case[Z, A13],
    case14: Case[Z, A14],
    case15: Case[Z, A15],
    case16: Case[Z, A16],
    case17: Case[Z, A17],
    case18: Case[Z, A18],
    case19: Case[Z, A19],
    case20: Case[Z, A20],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10],
      Prism[case11.id.type, Z, A11],
      Prism[case12.id.type, Z, A12],
      Prism[case13.id.type, Z, A13],
      Prism[case14.id.type, Z, A14],
      Prism[case15.id.type, Z, A15],
      Prism[case16.id.type, Z, A16],
      Prism[case17.id.type, Z, A17],
      Prism[case18.id.type, Z, A18],
      Prism[case19.id.type, Z, A19],
      Prism[case20.id.type, Z, A20]
    )

    override def annotate(
      annotation: Any
    ): Enum20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10],
      b.Prism[case11.id.type, Z, A11],
      b.Prism[case12.id.type, Z, A12],
      b.Prism[case13.id.type, Z, A13],
      b.Prism[case14.id.type, Z, A14],
      b.Prism[case15.id.type, Z, A15],
      b.Prism[case16.id.type, Z, A16],
      b.Prism[case17.id.type, Z, A17],
      b.Prism[case18.id.type, Z, A18],
      b.Prism[case19.id.type, Z, A19],
      b.Prism[case20.id.type, Z, A20]
    ) =
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

    override def cases: Chunk[Case[Z, _]] = Chunk(
      case1,
      case2,
      case3,
      case4,
      case5,
      case6,
      case7,
      case8,
      case9,
      case10,
      case11,
      case12,
      case13,
      case14,
      case15,
      case16,
      case17,
      case18,
      case19,
      case20
    )
  }

  sealed case class Enum21[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21,
    Z
  ](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    case11: Case[Z, A11],
    case12: Case[Z, A12],
    case13: Case[Z, A13],
    case14: Case[Z, A14],
    case15: Case[Z, A15],
    case16: Case[Z, A16],
    case17: Case[Z, A17],
    case18: Case[Z, A18],
    case19: Case[Z, A19],
    case20: Case[Z, A20],
    case21: Case[Z, A21],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10],
      Prism[case11.id.type, Z, A11],
      Prism[case12.id.type, Z, A12],
      Prism[case13.id.type, Z, A13],
      Prism[case14.id.type, Z, A14],
      Prism[case15.id.type, Z, A15],
      Prism[case16.id.type, Z, A16],
      Prism[case17.id.type, Z, A17],
      Prism[case18.id.type, Z, A18],
      Prism[case19.id.type, Z, A19],
      Prism[case20.id.type, Z, A20],
      Prism[case21.id.type, Z, A21]
    )

    override def annotate(
      annotation: Any
    ): Enum21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(
      b: AccessorBuilder
    ): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10],
      b.Prism[case11.id.type, Z, A11],
      b.Prism[case12.id.type, Z, A12],
      b.Prism[case13.id.type, Z, A13],
      b.Prism[case14.id.type, Z, A14],
      b.Prism[case15.id.type, Z, A15],
      b.Prism[case16.id.type, Z, A16],
      b.Prism[case17.id.type, Z, A17],
      b.Prism[case18.id.type, Z, A18],
      b.Prism[case19.id.type, Z, A19],
      b.Prism[case20.id.type, Z, A20],
      b.Prism[case21.id.type, Z, A21]
    ) =
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

    override def cases: Chunk[Case[Z, _]] = Chunk(
      case1,
      case2,
      case3,
      case4,
      case5,
      case6,
      case7,
      case8,
      case9,
      case10,
      case11,
      case12,
      case13,
      case14,
      case15,
      case16,
      case17,
      case18,
      case19,
      case20,
      case21
    )
  }

  sealed case class Enum22[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21,
    A22,
    Z
  ](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    case3: Case[Z, A3],
    case4: Case[Z, A4],
    case5: Case[Z, A5],
    case6: Case[Z, A6],
    case7: Case[Z, A7],
    case8: Case[Z, A8],
    case9: Case[Z, A9],
    case10: Case[Z, A10],
    case11: Case[Z, A11],
    case12: Case[Z, A12],
    case13: Case[Z, A13],
    case14: Case[Z, A14],
    case15: Case[Z, A15],
    case16: Case[Z, A16],
    case17: Case[Z, A17],
    case18: Case[Z, A18],
    case19: Case[Z, A19],
    case20: Case[Z, A20],
    case21: Case[Z, A21],
    case22: Case[Z, A22],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Prism[case1.id.type, Z, A1],
      Prism[case2.id.type, Z, A2],
      Prism[case3.id.type, Z, A3],
      Prism[case4.id.type, Z, A4],
      Prism[case5.id.type, Z, A5],
      Prism[case6.id.type, Z, A6],
      Prism[case7.id.type, Z, A7],
      Prism[case8.id.type, Z, A8],
      Prism[case9.id.type, Z, A9],
      Prism[case10.id.type, Z, A10],
      Prism[case11.id.type, Z, A11],
      Prism[case12.id.type, Z, A12],
      Prism[case13.id.type, Z, A13],
      Prism[case14.id.type, Z, A14],
      Prism[case15.id.type, Z, A15],
      Prism[case16.id.type, Z, A16],
      Prism[case17.id.type, Z, A17],
      Prism[case18.id.type, Z, A18],
      Prism[case19.id.type, Z, A19],
      Prism[case20.id.type, Z, A20],
      Prism[case21.id.type, Z, A21],
      Prism[case22.id.type, Z, A22]
    )

    override def annotate(
      annotation: Any
    ): Enum22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z] =
      copy(annotations = annotations :+ annotation)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(
      b: AccessorBuilder
    ): (
      b.Prism[case1.id.type, Z, A1],
      b.Prism[case2.id.type, Z, A2],
      b.Prism[case3.id.type, Z, A3],
      b.Prism[case4.id.type, Z, A4],
      b.Prism[case5.id.type, Z, A5],
      b.Prism[case6.id.type, Z, A6],
      b.Prism[case7.id.type, Z, A7],
      b.Prism[case8.id.type, Z, A8],
      b.Prism[case9.id.type, Z, A9],
      b.Prism[case10.id.type, Z, A10],
      b.Prism[case11.id.type, Z, A11],
      b.Prism[case12.id.type, Z, A12],
      b.Prism[case13.id.type, Z, A13],
      b.Prism[case14.id.type, Z, A14],
      b.Prism[case15.id.type, Z, A15],
      b.Prism[case16.id.type, Z, A16],
      b.Prism[case17.id.type, Z, A17],
      b.Prism[case18.id.type, Z, A18],
      b.Prism[case19.id.type, Z, A19],
      b.Prism[case20.id.type, Z, A20],
      b.Prism[case21.id.type, Z, A21],
      b.Prism[case22.id.type, Z, A22]
    ) =
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

    override def cases: Chunk[Case[Z, _]] = Chunk(
      case1,
      case2,
      case3,
      case4,
      case5,
      case6,
      case7,
      case8,
      case9,
      case10,
      case11,
      case12,
      case13,
      case14,
      case15,
      case16,
      case17,
      case18,
      case19,
      case20,
      case21,
      case22
    )
  }

  sealed case class EnumN[Z, C <: CaseSet.Aux[Z]](id: TypeId, caseSet: C, annotations: Chunk[Any] = Chunk.empty)
      extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      caseSet.Accessors[Z, Lens, Prism, Traversal]

    override def annotate(annotation: Any): EnumN[Z, C] = copy(annotations = annotations :+ annotation)

    override def cases: Chunk[Case[Z, _]] = Chunk(caseSet.toSeq: _*)

    def defaultValue: scala.util.Either[String, Z] =
      if (caseSet.toSeq.isEmpty)
        Left("cannot access default value for enum with no members")
      else
        caseSet.toSeq.head.schema.defaultValue.asInstanceOf[scala.util.Either[String, Z]]

    override def makeAccessors(b: AccessorBuilder): caseSet.Accessors[Z, b.Lens, b.Prism, b.Traversal] =
      caseSet.makeAccessors(self, b)
  }

  implicit def tuple2[A, B](implicit c1: Schema[A], c2: Schema[B]): Schema[(A, B)] =
    c1.zip(c2)

  implicit def tuple3[A, B, C](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C]): Schema[(A, B, C)] =
    c1.zip(c2).zip(c3).transform({ case ((a, b), c) => (a, b, c) }, { case (a, b, c) => ((a, b), c) })

  implicit def tuple4[A, B, C, D](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D]
  ): Schema[(A, B, C, D)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .transform({ case (((a, b), c), d) => (a, b, c, d) }, { case (a, b, c, d) => (((a, b), c), d) })

  implicit def tuple5[A, B, C, D, E](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D],
    c5: Schema[E]
  ): Schema[(A, B, C, D, E)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .transform({ case ((((a, b), c), d), e) => (a, b, c, d, e) }, { case (a, b, c, d, e) => ((((a, b), c), d), e) })

  implicit def tuple6[A, B, C, D, E, F](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D],
    c5: Schema[E],
    c6: Schema[F]
  ): Schema[(A, B, C, D, E, F)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .transform({ case (((((a, b), c), d), e), f) => (a, b, c, d, e, f) }, {
        case (a, b, c, d, e, f)                    => (((((a, b), c), d), e), f)
      })

  implicit def tuple7[A, B, C, D, E, F, G](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D],
    c5: Schema[E],
    c6: Schema[F],
    c7: Schema[G]
  ): Schema[(A, B, C, D, E, F, G)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .zip(c5)
      .zip(c6)
      .zip(c7)
      .transform({ case ((((((a, b), c), d), e), f), g) => (a, b, c, d, e, f, g) }, {
        case (a, b, c, d, e, f, g)                      => ((((((a, b), c), d), e), f), g)
      })

  implicit def tuple8[A, B, C, D, E, F, G, H](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D],
    c5: Schema[E],
    c6: Schema[F],
    c7: Schema[G],
    c8: Schema[H]
  ): Schema[(A, B, C, D, E, F, G, H)] =
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

  implicit def tuple9[A, B, C, D, E, F, G, H, I](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D],
    c5: Schema[E],
    c6: Schema[F],
    c7: Schema[G],
    c8: Schema[H],
    c9: Schema[I]
  ): Schema[(A, B, C, D, E, F, G, H, I)] =
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

  implicit def tuple10[A, B, C, D, E, F, G, H, I, J](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D],
    c5: Schema[E],
    c6: Schema[F],
    c7: Schema[G],
    c8: Schema[H],
    c9: Schema[I],
    c10: Schema[J]
  ): Schema[(A, B, C, D, E, F, G, H, I, J)] =
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

  implicit def tuple11[A, B, C, D, E, F, G, H, I, J, K](
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
    c11: Schema[K]
  ): Schema[(A, B, C, D, E, F, G, H, I, J, K)] =
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

  implicit def tuple12[A, B, C, D, E, F, G, H, I, J, K, L](
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
    c12: Schema[L]
  ): Schema[(A, B, C, D, E, F, G, H, I, J, K, L)] =
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
      .transform(
        { case (((((((((((a, b), c), d), e), f), g), h), i), j), k), l) => (a, b, c, d, e, f, g, h, i, j, k, l) }, {
          case (a, b, c, d, e, f, g, h, i, j, k, l)                     => (((((((((((a, b), c), d), e), f), g), h), i), j), k), l)
        }
      )

  implicit def tuple13[A, B, C, D, E, F, G, H, I, J, K, L, M](
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
    c13: Schema[M]
  ): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
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
      .transform(
        {
          case ((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m) => (a, b, c, d, e, f, g, h, i, j, k, l, m)
        }, {
          case (a, b, c, d, e, f, g, h, i, j, k, l, m) => ((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m)
        }
      )

  implicit def tuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](
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
    c14: Schema[N]
  ): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
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
      .transform(
        {
          case (((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n) =>
            (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
        }, {
          case (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =>
            (((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n)
        }
      )

  implicit def tuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](
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
    c15: Schema[O]
  ): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
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
      .transform(
        {
          case ((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o) =>
            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
        }, {
          case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =>
            ((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o)
        }
      )

  implicit def tuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](
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
    c16: Schema[P]
  ): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
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
      .transform(
        {
          case (((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p) =>
            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
        }, {
          case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
            (((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p)
        }
      )

  implicit def tuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](
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
    c17: Schema[Q]
  ): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
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
      .transform(
        {
          case ((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q) =>
            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
        }, {
          case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
            ((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q)
        }
      )

  implicit def tuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](
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
    c18: Schema[R]
  ): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
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
      .transform(
        {
          case (((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r) =>
            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
        }, {
          case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
            (((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r)
        }
      )

  implicit def tuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](
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
    c19: Schema[S]
  ): Schema[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
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
      .transform(
        {
          case ((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s) =>
            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
        }, {
          case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
            ((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s)
        }
      )

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
      .transform(
        {
          case (((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t) =>
            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
        }, {
          case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
            (((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t)
        }
      )

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
      .transform(
        {
          case ((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u) =>
            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
        }, {
          case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
            ((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u)
        }
      )

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
      .transform(
        {
          case (
              ((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u),
              v
              ) =>
            (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)
        }, {
          case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
            (((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u), v)
        }
      )
// # RECORD SCHEMAS

  sealed case class GenericRecord(
    id: TypeId,
    fieldSet: FieldSet,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[ListMap[String, _]] { self =>

    type Terms      = fieldSet.Terms
    type FieldNames = fieldSet.FieldNames

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      fieldSet.Accessors[ListMap[String, _], Lens, Prism, Traversal]

    override def makeAccessors(b: AccessorBuilder): Accessors[b.Lens, b.Prism, b.Traversal] =
      fieldSet.makeAccessors(self, b)

    override def fields: Chunk[Schema.Field[ListMap[String, _], _]] = fieldSet.toChunk

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, ListMap[String, _]] =
      if (values.size == fields.size)
        Right(ListMap(fields.map(_.name).zip(values): _*))
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(values: ListMap[String, _])(implicit unsafe: Unsafe): Chunk[Any] =
      Chunk.fromIterable(fields.map(f => values(f.name)))

    /**
     * Returns a new schema that with `annotation`
     */
    override def annotate(annotation: Any): GenericRecord = copy(annotations = annotations :+ annotation)
  }

  sealed trait CaseClass0[Z] extends Record[Z] { self =>

    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Nothing
    override type Terms                                                     = Any
    override type FieldNames                                                = Any

    def id: TypeId
    def defaultConstruct: () => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): Nothing = ???

    override def fields: Chunk[Field[Z, _]] = Chunk.empty

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.isEmpty)
        try {
          Right(defaultConstruct())
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(value)

    override def toString: String = s"CaseClass0($id, ${fields.mkString(",")})"
  }

  object CaseClass0 {

    def apply[Z](
      id0: TypeId,
      defaultConstruct0: () => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass0[Z] = new CaseClass0[Z] {
      override def id: TypeId                = id0
      override def defaultConstruct: () => Z = defaultConstruct0
      override def annotations: Chunk[Any]   = annotations0

      override def annotate(annotation: Any): CaseClass0[Z] =
        CaseClass0(id, defaultConstruct0, annotations :+ annotation)
    }

    def unapply[Z](schema: CaseClass0[Z]): Some[(TypeId, () => Z, Chunk[Any])] =
      Some((schema.id, schema.defaultConstruct, schema.annotations))
  }

  sealed trait CaseClass1[A, Z] extends Record[Z] { self =>

    type Field1 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Lens[Field1, Z, A]

    override type Terms      = (Field1, A)
    override type FieldNames = Field1

    def id: TypeId
    def field: Field.WithFieldName[Z, Field1, A]
    def defaultConstruct: A => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): b.Lens[Field1, Z, A] = b.makeLens(self, field)

    override def fields: Chunk[Field[Z, _]] = Chunk(field)

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 1)
        try {
          Right(defaultConstruct(values(0).asInstanceOf[A]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(field.get(value))
    override def toString: String                                           = s"CaseClass1(${fields.mkString(",")})"
  }

  object CaseClass1 {

    def apply[A, Z](
      id0: TypeId,
      field0: Field[Z, A],
      defaultConstruct0: A => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass1[A, Z] =
      new CaseClass1[A, Z] {

        override def id: TypeId                               = id0
        override def field: Field.WithFieldName[Z, Field1, A] = field0.asInstanceOf[Field.WithFieldName[Z, Field1, A]]
        override def defaultConstruct: A => Z                 = defaultConstruct0
        override def annotations: Chunk[Any]                  = annotations0

        override def annotate(annotation: Any): CaseClass1[A, Z] =
          CaseClass1(id0, field0, defaultConstruct0, annotations0 :+ annotation)
      }

    def unapply[A, Z](
      schema: CaseClass1[A, Z]
    ): Option[(TypeId, Field.WithFieldName[Z, schema.Field1, A], A => Z, Chunk[Any])] =
      Some((schema.id, schema.field, schema.defaultConstruct, schema.annotations))

    type WithFields[F <: Singleton with String, A, Z] =
      CaseClass1[A, Z] {
        type Field1 = F
      }
  }

  sealed trait CaseClass2[A1, A2, Z] extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Lens[Field1, Z, A1], Lens[Field2, Z, A2])

    override type Terms = (Field1, A1) with (Field2, A2)

    override type FieldNames = Field1 with Field2

    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def construct: (A1, A2) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(
      b: AccessorBuilder
    ): (b.Lens[Field1, Z, A1], b.Lens[Field2, Z, A2]) =
      (b.makeLens(self, field1), b.makeLens(self, field2))

    override def fields: Chunk[Field[Z, _]] = Chunk(field1, field2)

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 2)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] =
      Chunk(field1.get(value), field2.get(value))

    override def toString: String = s"CaseClass2($id, ${fields.mkString(",")})"
  }

  object CaseClass2 {

    def apply[A1, A2, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      construct0: (A1, A2) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass2[A1, A2, Z] =
      new CaseClass2[A1, A2, Z] {
        def id: TypeId                                 = id0
        def field1: Field.WithFieldName[Z, Field1, A1] = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2] = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def construct: (A1, A2) => Z                   = construct0
        def annotations: Chunk[Any]                    = annotations0

        def annotate(annotation: Any): CaseClass2[A1, A2, Z] =
          CaseClass2(id0, field01, field02, construct0, annotations0 :+ annotation)
      }

    def unapply[A1, A2, Z](schema: CaseClass2[A1, A2, Z]): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        (A1, A2) => Z,
        Chunk[Any]
      )
    ] =
      Some((schema.id, schema.field1, schema.field2, schema.construct, schema.annotations))

    type WithFields[F1 <: Singleton with String, F2 <: Singleton with String, A1, A2, Z] =
      CaseClass2[A1, A2, Z] {
        type Field1 = F1
        type Field2 = F2
      }
  }

  sealed trait CaseClass3[A1, A2, A3, Z] extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Lens[Field1, Z, A1], Lens[Field2, Z, A2], Lens[Field3, Z, A3])

    override type Terms = (Field1, A1) with (Field2, A2) with (Field3, A3)

    override type FieldNames = Field1 with Field2 with Field3

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def construct: (A1, A2, A3) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(
      b: AccessorBuilder
    ): (b.Lens[Field1, Z, A1], b.Lens[Field2, Z, A2], b.Lens[Field3, Z, A3]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3))

    override def fields: Chunk[Field[Z, _]] = Chunk(field1, field2, field3)

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 3)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] =
      Chunk(field1.get(value), field2.get(value), field3.get(value))
    override def toString: String = s"CaseClass3($id, ${fields.mkString(",")})"
  }

  object CaseClass3 {

    def apply[A1, A2, A3, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      construct0: (A1, A2, A3) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass3[A1, A2, A3, Z] =
      new CaseClass3[A1, A2, A3, Z] {
        def id: TypeId                                 = id0
        def field1: Field.WithFieldName[Z, Field1, A1] = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2] = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3] = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def construct: (A1, A2, A3) => Z               = construct0
        def annotations: Chunk[Any]                    = annotations0

        def annotate(annotation: Any): CaseClass3[A1, A2, A3, Z] =
          CaseClass3(id0, field01, field02, field03, construct0, annotations0 :+ annotation)
      }

    def unapply[A1, A2, A3, Z](schema: CaseClass3[A1, A2, A3, Z]): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        (A1, A2, A3) => Z,
        Chunk[Any]
      )
    ] =
      Some((schema.id, schema.field1, schema.field2, schema.field3, schema.construct, schema.annotations))

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      A1,
      A2,
      A3,
      Z
    ] =
      CaseClass3[A1, A2, A3, Z] {
        type Field1 = F1
        type Field2 = F2
        type Field3 = F3
      }
  }

  sealed trait CaseClass4[A1, A2, A3, A4, Z] extends Record[Z] { self =>
    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Lens[Field1, Z, A1], Lens[Field2, Z, A2], Lens[Field3, Z, A3], Lens[Field4, Z, A4])

    override type Terms = (Field1, A1) with (Field2, A2) with (Field3, A3) with (Field4, A4)

    override type FieldNames = Field1 with Field2 with Field3 with Field4

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def construct: (A1, A2, A3, A4) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4]
    ) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4))

    override def fields: Chunk[Field[Z, _]] = Chunk(field1, field2, field3, field4)

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 4)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] =
      Chunk(field1.get(value), field2.get(value), field3.get(value), field4.get(value))
    override def toString: String = s"CaseClass4($id, ${fields.mkString(",")})"
  }

  object CaseClass4 {

    def apply[A1, A2, A3, A4, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      construct0: (A1, A2, A3, A4) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass4[A1, A2, A3, A4, Z] =
      new CaseClass4[A1, A2, A3, A4, Z] {
        def id: TypeId                                 = id0
        def field1: Field.WithFieldName[Z, Field1, A1] = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2] = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3] = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4] = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def construct: (A1, A2, A3, A4) => Z           = construct0
        def annotations: Chunk[Any]                    = annotations0

        def annotate(annotation: Any): CaseClass4[A1, A2, A3, A4, Z] =
          CaseClass4(id0, field01, field02, field03, field04, construct0, annotations0 :+ annotation)
      }

    def unapply[A1, A2, A3, A4, Z](schema: CaseClass4[A1, A2, A3, A4, Z]): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        (A1, A2, A3, A4) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (schema.id, schema.field1, schema.field2, schema.field3, schema.field4, schema.construct, schema.annotations)
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      Z
    ] =
      CaseClass4[A1, A2, A3, A4, Z] {
        type Field1 = F1
        type Field2 = F2
        type Field3 = F3
        type Field4 = F4
      }
  }

  sealed trait CaseClass5[A1, A2, A3, A4, A5, Z] extends Record[Z] { self =>
    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Lens[Field1, Z, A1], Lens[Field2, Z, A2], Lens[Field3, Z, A3], Lens[Field4, Z, A4], Lens[Field5, Z, A5])

    override type Terms = (Field1, A1) with (Field2, A2) with (Field3, A3) with (Field4, A4) with (Field5, A5)

    override type FieldNames = Field1 with Field2 with Field3 with Field4 with Field5

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]

    def construct: (A1, A2, A3, A4, A5) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5]
    ) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5)
      )

    override def fields: Chunk[Field[Z, _]] = Chunk(field1, field2, field3, field4, field5)

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 5)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4],
              values(4).asInstanceOf[A5]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value)
    )
    override def toString: String = s"CaseClass5($id, ${fields.mkString(",")})"
  }

  object CaseClass5 {

    def apply[A1, A2, A3, A4, A5, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      construct0: (A1, A2, A3, A4, A5) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass5[A1, A2, A3, A4, A5, Z] =
      new CaseClass5[A1, A2, A3, A4, A5, Z] {
        def id: TypeId                                 = id0
        def field1: Field.WithFieldName[Z, Field1, A1] = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2] = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3] = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4] = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5] = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def construct: (A1, A2, A3, A4, A5) => Z       = construct0
        def annotations: Chunk[Any]                    = annotations0

        def annotate(annotation: Any): CaseClass5[A1, A2, A3, A4, A5, Z] =
          CaseClass5(id0, field01, field02, field03, field04, field05, construct0, annotations0 :+ annotation)
      }

    def unapply[A1, A2, A3, A4, A5, Z](schema: CaseClass5[A1, A2, A3, A4, A5, Z]): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        (A1, A2, A3, A4, A5) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      Z
    ] =
      CaseClass5[A1, A2, A3, A4, A5, Z] {
        type Field1 = F1
        type Field2 = F2
        type Field3 = F3
        type Field4 = F4
        type Field5 = F5
      }
  }

  sealed trait CaseClass6[A1, A2, A3, A4, A5, A6, Z] extends Record[Z] { self =>
    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (
        Lens[Field1, Z, A1],
        Lens[Field2, Z, A2],
        Lens[Field3, Z, A3],
        Lens[Field4, Z, A4],
        Lens[Field5, Z, A5],
        Lens[Field6, Z, A6]
      )

    override type Terms =
      (Field1, A1) with (Field2, A2) with (Field3, A3) with (Field4, A4) with (Field5, A5) with (Field6, A6)

    override type FieldNames = Field1 with Field2 with Field3 with Field4 with Field5 with Field6

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]

    def construct: (A1, A2, A3, A4, A5, A6) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6]
    ) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5),
        b.makeLens(self, field6)
      )

    override def fields: Chunk[Field[Z, _]] = Chunk(field1, field2, field3, field4, field5, field6)

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 6)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4],
              values(4).asInstanceOf[A5],
              values(5).asInstanceOf[A6]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value)
    )

    override def toString: String = s"CaseClass6($id, ${fields.mkString(",")})"
  }

  object CaseClass6 {

    def apply[A1, A2, A3, A4, A5, A6, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      construct0: (A1, A2, A3, A4, A5, A6) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass6[A1, A2, A3, A4, A5, A6, Z] =
      new CaseClass6[A1, A2, A3, A4, A5, A6, Z] {
        def id: TypeId                                 = id0
        def field1: Field.WithFieldName[Z, Field1, A1] = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2] = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3] = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4] = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5] = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6] = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def construct: (A1, A2, A3, A4, A5, A6) => Z   = construct0
        def annotations: Chunk[Any]                    = annotations0

        def annotate(annotation: Any): CaseClass6[A1, A2, A3, A4, A5, A6, Z] =
          CaseClass6(id0, field01, field02, field03, field04, field05, field06, construct0, annotations0 :+ annotation)
      }

    def unapply[A1, A2, A3, A4, A5, A6, Z](schema: CaseClass6[A1, A2, A3, A4, A5, A6, Z]): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        (A1, A2, A3, A4, A5, A6) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      Z
    ] =
      CaseClass6[A1, A2, A3, A4, A5, A6, Z] {
        type Field1 = F1
        type Field2 = F2
        type Field3 = F3
        type Field4 = F4
        type Field5 = F5
        type Field6 = F6
      }
  }

  sealed trait CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z] extends Record[Z] { self =>
    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (
        Lens[Field1, Z, A1],
        Lens[Field2, Z, A2],
        Lens[Field3, Z, A3],
        Lens[Field4, Z, A4],
        Lens[Field5, Z, A5],
        Lens[Field6, Z, A6],
        Lens[Field7, Z, A7]
      )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)

    override type FieldNames = Field1 with Field2 with Field3 with Field4 with Field5 with Field6 with Field7

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]

    def construct: (A1, A2, A3, A4, A5, A6, A7) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7]
    ) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5),
        b.makeLens(self, field6),
        b.makeLens(self, field7)
      )

    override def fields: Chunk[Field[Z, _]] = Chunk(field1, field2, field3, field4, field5, field6, field7)

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 7)
        try {
          Right(
            construct(
              values(0).asInstanceOf[A1],
              values(1).asInstanceOf[A2],
              values(2).asInstanceOf[A3],
              values(3).asInstanceOf[A4],
              values(4).asInstanceOf[A5],
              values(5).asInstanceOf[A6],
              values(6).asInstanceOf[A7]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value)
    )

    override def toString: String = s"CaseClass7($id, ${fields.mkString(",")})"
  }

  object CaseClass7 {

    def apply[A1, A2, A3, A4, A5, A6, A7, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      construct0: (A1, A2, A3, A4, A5, A6, A7) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z] =
      new CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z] {
        def id: TypeId                                   = id0
        def field1: Field.WithFieldName[Z, Field1, A1]   = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]   = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]   = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]   = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]   = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]   = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]   = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def construct: (A1, A2, A3, A4, A5, A6, A7) => Z = construct0
        def annotations: Chunk[Any]                      = annotations0

        def annotate(annotation: Any): CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z] =
          CaseClass7(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, Z](schema: CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        (A1, A2, A3, A4, A5, A6, A7) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      Z
    ] =
      CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z] {
        type Field1 = F1
        type Field2 = F2
        type Field3 = F3
        type Field4 = F4
        type Field5 = F5
        type Field6 = F6
        type Field7 = F7
      }
  }

  sealed trait CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z] extends Record[Z] { self =>
    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (
        Lens[Field1, Z, A1],
        Lens[Field2, Z, A2],
        Lens[Field3, Z, A3],
        Lens[Field4, Z, A4],
        Lens[Field5, Z, A5],
        Lens[Field6, Z, A6],
        Lens[Field7, Z, A7],
        Lens[Field8, Z, A8]
      )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]

    def construct: (A1, A2, A3, A4, A5, A6, A7, A8) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8]
    ) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5),
        b.makeLens(self, field6),
        b.makeLens(self, field7),
        b.makeLens(self, field8)
      )

    override def fields: Chunk[Field[Z, _]] = Chunk(field1, field2, field3, field4, field5, field6, field7, field8)

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 8)
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
              values(7).asInstanceOf[A8]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value)
    )

    override def toString: String = s"CaseClass8($id, ${fields.mkString(",")})"
  }

  object CaseClass8 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z] =
      new CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z] {
        def id: TypeId                                       = id0
        def field1: Field.WithFieldName[Z, Field1, A1]       = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]       = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]       = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]       = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]       = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]       = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]       = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]       = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def construct: (A1, A2, A3, A4, A5, A6, A7, A8) => Z = construct0
        def annotations: Chunk[Any]                          = annotations0

        def annotate(annotation: Any): CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z] =
          CaseClass8(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, Z](schema: CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z]): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        (A1, A2, A3, A4, A5, A6, A7, A8) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      Z
    ] =
      CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z] {
        type Field1 = F1
        type Field2 = F2
        type Field3 = F3
        type Field4 = F4
        type Field5 = F5
        type Field6 = F6
        type Field7 = F7
        type Field8 = F8
      }
  }

  sealed trait CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z] extends Record[Z] { self =>
    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (
        Lens[Field1, Z, A1],
        Lens[Field2, Z, A2],
        Lens[Field3, Z, A3],
        Lens[Field4, Z, A4],
        Lens[Field5, Z, A5],
        Lens[Field6, Z, A6],
        Lens[Field7, Z, A7],
        Lens[Field8, Z, A8],
        Lens[Field9, Z, A9]
      )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field8, A9)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]

    def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9]
    ) =
      (
        b.makeLens(self, field1),
        b.makeLens(self, field2),
        b.makeLens(self, field3),
        b.makeLens(self, field4),
        b.makeLens(self, field5),
        b.makeLens(self, field6),
        b.makeLens(self, field7),
        b.makeLens(self, field8),
        b.makeLens(self, field9)
      )
    override def fields: Chunk[Field[Z, _]] =
      Chunk(field1, field2, field3, field4, field5, field6, field7, field8, field9)

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 9)
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
              values(8).asInstanceOf[A9]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value)
    )
    override def toString: String = s"CaseClass9($id, ${fields.mkString(",")})"
  }

  object CaseClass9 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z] =
      new CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z] {
        def id: TypeId                                           = id0
        def field1: Field.WithFieldName[Z, Field1, A1]           = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]           = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]           = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]           = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]           = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]           = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]           = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]           = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]           = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z = construct0
        def annotations: Chunk[Any]                              = annotations0

        def annotate(annotation: Any): CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z] =
          CaseClass9(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](schema: CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      Z
    ] =
      CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z] {
        type Field1 = F1
        type Field2 = F2
        type Field3 = F3
        type Field4 = F4
        type Field5 = F5
        type Field6 = F6
        type Field7 = F7
        type Field8 = F8
        type Field9 = F9
      }
  }

  sealed trait CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z] extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10]
    ) =
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
        b.makeLens(self, field10)
      )

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 10)
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
              values(9).asInstanceOf[A10]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value)
    )

    override def toString: String = s"CaseClass10($id, ${fields.mkString(",")})"
  }

  object CaseClass10 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z] =
      new CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z] {
        def id: TypeId                                                = id0
        def field1: Field.WithFieldName[Z, Field1, A1]                = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]                = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]                = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]                = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]                = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]                = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]                = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]                = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]                = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10]             = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z = construct0
        def annotations: Chunk[Any]                                   = annotations0

        def annotate(annotation: Any): CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z] =
          CaseClass10(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
      schema: CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      Z
    ] =
      CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
      }
  }

  sealed trait CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z] extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String
    type Field11 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10],
      Lens[Field11, Z, A11]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)
      with (Field11, A11)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10
      with Field11

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]
    def field11: Field.WithFieldName[Z, Field11, A11]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10],
      b.Lens[Field11, Z, A11]
    ) =
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
        b.makeLens(self, field11)
      )

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10,
        field11
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 11)
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
              values(10).asInstanceOf[A11]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value),
      field11.get(value)
    )

    override def toString: String = s"CaseClass11($id, ${fields.mkString(",")})"
  }

  object CaseClass11 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      field011: Field[Z, A11],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z] =
      new CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z] {
        def id: TypeId                                                     = id0
        def field1: Field.WithFieldName[Z, Field1, A1]                     = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]                     = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]                     = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]                     = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]                     = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]                     = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]                     = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]                     = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]                     = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10]                  = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def field11: Field.WithFieldName[Z, Field11, A11]                  = field011.asInstanceOf[Field.WithFieldName[Z, Field11, A11]]
        def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z = construct0
        def annotations: Chunk[Any]                                        = annotations0

        def annotate(annotation: Any): CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z] =
          CaseClass11(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            field011,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
      schema: CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        Field.WithFieldName[Z, schema.Field11, A11],
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.field11,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      F11 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      Z
    ] =
      CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
        type Field11 = F11
      }
  }

  sealed trait CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z] extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String
    type Field11 <: Singleton with String
    type Field12 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10],
      Lens[Field11, Z, A11],
      Lens[Field12, Z, A12]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)
      with (Field11, A11)
      with (Field12, A12)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10
      with Field11
      with Field12

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]
    def field11: Field.WithFieldName[Z, Field11, A11]
    def field12: Field.WithFieldName[Z, Field12, A12]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10],
      b.Lens[Field11, Z, A11],
      b.Lens[Field12, Z, A12]
    ) =
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
        b.makeLens(self, field12)
      )

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10,
        field11,
        field12
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 12)
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
              values(11).asInstanceOf[A12]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value),
      field11.get(value),
      field12.get(value)
    )

    override def toString: String = s"CaseClass12($id, ${fields.mkString(",")})"
  }

  object CaseClass12 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      field011: Field[Z, A11],
      field012: Field[Z, A12],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z] =
      new CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z] {
        def id: TypeId                                                          = id0
        def field1: Field.WithFieldName[Z, Field1, A1]                          = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]                          = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]                          = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]                          = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]                          = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]                          = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]                          = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]                          = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]                          = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10]                       = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def field11: Field.WithFieldName[Z, Field11, A11]                       = field011.asInstanceOf[Field.WithFieldName[Z, Field11, A11]]
        def field12: Field.WithFieldName[Z, Field12, A12]                       = field012.asInstanceOf[Field.WithFieldName[Z, Field12, A12]]
        def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z = construct0
        def annotations: Chunk[Any]                                             = annotations0

        def annotate(annotation: Any): CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z] =
          CaseClass12(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            field011,
            field012,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
      schema: CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        Field.WithFieldName[Z, schema.Field11, A11],
        Field.WithFieldName[Z, schema.Field12, A12],
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.field11,
          schema.field12,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      F11 <: Singleton with String,
      F12 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      Z
    ] =
      CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
        type Field11 = F11
        type Field12 = F12
      }
  }

  sealed trait CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z] extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String
    type Field11 <: Singleton with String
    type Field12 <: Singleton with String
    type Field13 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10],
      Lens[Field11, Z, A11],
      Lens[Field12, Z, A12],
      Lens[Field13, Z, A13]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)
      with (Field11, A11)
      with (Field12, A12)
      with (Field13, A13)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10
      with Field11
      with Field12
      with Field13

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]
    def field11: Field.WithFieldName[Z, Field11, A11]
    def field12: Field.WithFieldName[Z, Field12, A12]
    def field13: Field.WithFieldName[Z, Field13, A13]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10],
      b.Lens[Field11, Z, A11],
      b.Lens[Field12, Z, A12],
      b.Lens[Field13, Z, A13]
    ) =
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
        b.makeLens(self, field13)
      )

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10,
        field11,
        field12,
        field13
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 13)
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
              values(12).asInstanceOf[A13]
            )
          )
        } catch {
          case _: Throwable => Left("invalid type in values")
        } else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value),
      field11.get(value),
      field12.get(value),
      field13.get(value)
    )

    override def toString: String = s"CaseClass13($id, ${fields.mkString(",")})"
  }

  object CaseClass13 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      field011: Field[Z, A11],
      field012: Field[Z, A12],
      field013: Field[Z, A13],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z] =
      new CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z] {
        def id: TypeId                                                               = id0
        def field1: Field.WithFieldName[Z, Field1, A1]                               = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]                               = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]                               = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]                               = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]                               = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]                               = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]                               = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]                               = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]                               = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10]                            = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def field11: Field.WithFieldName[Z, Field11, A11]                            = field011.asInstanceOf[Field.WithFieldName[Z, Field11, A11]]
        def field12: Field.WithFieldName[Z, Field12, A12]                            = field012.asInstanceOf[Field.WithFieldName[Z, Field12, A12]]
        def field13: Field.WithFieldName[Z, Field13, A13]                            = field013.asInstanceOf[Field.WithFieldName[Z, Field13, A13]]
        def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z = construct0
        def annotations: Chunk[Any]                                                  = annotations0

        def annotate(annotation: Any): CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z] =
          CaseClass13(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            field011,
            field012,
            field013,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
      schema: CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        Field.WithFieldName[Z, schema.Field11, A11],
        Field.WithFieldName[Z, schema.Field12, A12],
        Field.WithFieldName[Z, schema.Field13, A13],
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.field11,
          schema.field12,
          schema.field13,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      F11 <: Singleton with String,
      F12 <: Singleton with String,
      F13 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      Z
    ] =
      CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
        type Field11 = F11
        type Field12 = F12
        type Field13 = F13
      }
  }

  sealed trait CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z] extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String
    type Field11 <: Singleton with String
    type Field12 <: Singleton with String
    type Field13 <: Singleton with String
    type Field14 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10],
      Lens[Field11, Z, A11],
      Lens[Field12, Z, A12],
      Lens[Field13, Z, A13],
      Lens[Field14, Z, A14]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)
      with (Field11, A11)
      with (Field12, A12)
      with (Field13, A13)
      with (Field14, A14)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10
      with Field11
      with Field12
      with Field13
      with Field14

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]
    def field11: Field.WithFieldName[Z, Field11, A11]
    def field12: Field.WithFieldName[Z, Field12, A12]
    def field13: Field.WithFieldName[Z, Field13, A13]
    def field14: Field.WithFieldName[Z, Field14, A14]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10],
      b.Lens[Field11, Z, A11],
      b.Lens[Field12, Z, A12],
      b.Lens[Field13, Z, A13],
      b.Lens[Field14, Z, A14]
    ) =
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
        b.makeLens(self, field14)
      )

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10,
        field11,
        field12,
        field13,
        field14
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
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
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value),
      field11.get(value),
      field12.get(value),
      field13.get(value),
      field14.get(value)
    )

    override def toString: String = s"CaseClass14($id, ${fields.mkString(",")})"
  }

  object CaseClass14 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      field011: Field[Z, A11],
      field012: Field[Z, A12],
      field013: Field[Z, A13],
      field014: Field[Z, A14],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z] =
      new CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z] {
        def id: TypeId                                                                    = id0
        def field1: Field.WithFieldName[Z, Field1, A1]                                    = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]                                    = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]                                    = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]                                    = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]                                    = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]                                    = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]                                    = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]                                    = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]                                    = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10]                                 = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def field11: Field.WithFieldName[Z, Field11, A11]                                 = field011.asInstanceOf[Field.WithFieldName[Z, Field11, A11]]
        def field12: Field.WithFieldName[Z, Field12, A12]                                 = field012.asInstanceOf[Field.WithFieldName[Z, Field12, A12]]
        def field13: Field.WithFieldName[Z, Field13, A13]                                 = field013.asInstanceOf[Field.WithFieldName[Z, Field13, A13]]
        def field14: Field.WithFieldName[Z, Field14, A14]                                 = field014.asInstanceOf[Field.WithFieldName[Z, Field14, A14]]
        def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z = construct0
        def annotations: Chunk[Any]                                                       = annotations0

        def annotate(annotation: Any): CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z] =
          CaseClass14(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            field011,
            field012,
            field013,
            field014,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
      schema: CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        Field.WithFieldName[Z, schema.Field11, A11],
        Field.WithFieldName[Z, schema.Field12, A12],
        Field.WithFieldName[Z, schema.Field13, A13],
        Field.WithFieldName[Z, schema.Field14, A14],
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.field11,
          schema.field12,
          schema.field13,
          schema.field14,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      F11 <: Singleton with String,
      F12 <: Singleton with String,
      F13 <: Singleton with String,
      F14 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      Z
    ] =
      CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
        type Field11 = F11
        type Field12 = F12
        type Field13 = F13
        type Field14 = F14
      }
  }

  sealed trait CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z] extends Record[Z] {
    self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String
    type Field11 <: Singleton with String
    type Field12 <: Singleton with String
    type Field13 <: Singleton with String
    type Field14 <: Singleton with String
    type Field15 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10],
      Lens[Field11, Z, A11],
      Lens[Field12, Z, A12],
      Lens[Field13, Z, A13],
      Lens[Field14, Z, A14],
      Lens[Field15, Z, A15]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)
      with (Field11, A11)
      with (Field12, A12)
      with (Field13, A13)
      with (Field14, A14)
      with (Field15, A15)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10
      with Field11
      with Field12
      with Field13
      with Field14
      with Field15

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]
    def field11: Field.WithFieldName[Z, Field11, A11]
    def field12: Field.WithFieldName[Z, Field12, A12]
    def field13: Field.WithFieldName[Z, Field13, A13]
    def field14: Field.WithFieldName[Z, Field14, A14]
    def field15: Field.WithFieldName[Z, Field15, A15]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10],
      b.Lens[Field11, Z, A11],
      b.Lens[Field12, Z, A12],
      b.Lens[Field13, Z, A13],
      b.Lens[Field14, Z, A14],
      b.Lens[Field15, Z, A15]
    ) =
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

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10,
        field11,
        field12,
        field13,
        field14,
        field15
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
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
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value),
      field11.get(value),
      field12.get(value),
      field13.get(value),
      field14.get(value),
      field15.get(value)
    )

    override def toString: String = s"CaseClass15($id, ${fields.mkString(",")})"
  }

  object CaseClass15 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      field011: Field[Z, A11],
      field012: Field[Z, A12],
      field013: Field[Z, A13],
      field014: Field[Z, A14],
      field015: Field[Z, A15],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z] =
      new CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z] {
        def id: TypeId                                                                         = id0
        def field1: Field.WithFieldName[Z, Field1, A1]                                         = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]                                         = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]                                         = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]                                         = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]                                         = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]                                         = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]                                         = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]                                         = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]                                         = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10]                                      = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def field11: Field.WithFieldName[Z, Field11, A11]                                      = field011.asInstanceOf[Field.WithFieldName[Z, Field11, A11]]
        def field12: Field.WithFieldName[Z, Field12, A12]                                      = field012.asInstanceOf[Field.WithFieldName[Z, Field12, A12]]
        def field13: Field.WithFieldName[Z, Field13, A13]                                      = field013.asInstanceOf[Field.WithFieldName[Z, Field13, A13]]
        def field14: Field.WithFieldName[Z, Field14, A14]                                      = field014.asInstanceOf[Field.WithFieldName[Z, Field14, A14]]
        def field15: Field.WithFieldName[Z, Field15, A15]                                      = field015.asInstanceOf[Field.WithFieldName[Z, Field15, A15]]
        def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z = construct0
        def annotations: Chunk[Any]                                                            = annotations0

        def annotate(
          annotation: Any
        ): CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z] =
          CaseClass15(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            field011,
            field012,
            field013,
            field014,
            field015,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
      schema: CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        Field.WithFieldName[Z, schema.Field11, A11],
        Field.WithFieldName[Z, schema.Field12, A12],
        Field.WithFieldName[Z, schema.Field13, A13],
        Field.WithFieldName[Z, schema.Field14, A14],
        Field.WithFieldName[Z, schema.Field15, A15],
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.field11,
          schema.field12,
          schema.field13,
          schema.field14,
          schema.field15,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      F11 <: Singleton with String,
      F12 <: Singleton with String,
      F13 <: Singleton with String,
      F14 <: Singleton with String,
      F15 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      Z
    ] =
      CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
        type Field11 = F11
        type Field12 = F12
        type Field13 = F13
        type Field14 = F14
        type Field15 = F15
      }
  }

  sealed trait CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z] extends Record[Z] {
    self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String
    type Field11 <: Singleton with String
    type Field12 <: Singleton with String
    type Field13 <: Singleton with String
    type Field14 <: Singleton with String
    type Field15 <: Singleton with String
    type Field16 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10],
      Lens[Field11, Z, A11],
      Lens[Field12, Z, A12],
      Lens[Field13, Z, A13],
      Lens[Field14, Z, A14],
      Lens[Field15, Z, A15],
      Lens[Field16, Z, A16]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)
      with (Field11, A11)
      with (Field12, A12)
      with (Field13, A13)
      with (Field14, A14)
      with (Field15, A15)
      with (Field16, A16)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10
      with Field11
      with Field12
      with Field13
      with Field14
      with Field15
      with Field16

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]
    def field11: Field.WithFieldName[Z, Field11, A11]
    def field12: Field.WithFieldName[Z, Field12, A12]
    def field13: Field.WithFieldName[Z, Field13, A13]
    def field14: Field.WithFieldName[Z, Field14, A14]
    def field15: Field.WithFieldName[Z, Field15, A15]
    def field16: Field.WithFieldName[Z, Field16, A16]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10],
      b.Lens[Field11, Z, A11],
      b.Lens[Field12, Z, A12],
      b.Lens[Field13, Z, A13],
      b.Lens[Field14, Z, A14],
      b.Lens[Field15, Z, A15],
      b.Lens[Field16, Z, A16]
    ) =
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

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10,
        field11,
        field12,
        field13,
        field14,
        field15,
        field16
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
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
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value),
      field11.get(value),
      field12.get(value),
      field13.get(value),
      field14.get(value),
      field15.get(value),
      field16.get(value)
    )

    override def toString: String = s"CaseClass16($id, ${fields.mkString(",")})"
  }

  object CaseClass16 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      field011: Field[Z, A11],
      field012: Field[Z, A12],
      field013: Field[Z, A13],
      field014: Field[Z, A14],
      field015: Field[Z, A15],
      field016: Field[Z, A16],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z] =
      new CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z] {
        def id: TypeId                                                                              = id0
        def field1: Field.WithFieldName[Z, Field1, A1]                                              = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]                                              = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]                                              = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]                                              = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]                                              = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]                                              = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]                                              = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]                                              = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]                                              = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10]                                           = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def field11: Field.WithFieldName[Z, Field11, A11]                                           = field011.asInstanceOf[Field.WithFieldName[Z, Field11, A11]]
        def field12: Field.WithFieldName[Z, Field12, A12]                                           = field012.asInstanceOf[Field.WithFieldName[Z, Field12, A12]]
        def field13: Field.WithFieldName[Z, Field13, A13]                                           = field013.asInstanceOf[Field.WithFieldName[Z, Field13, A13]]
        def field14: Field.WithFieldName[Z, Field14, A14]                                           = field014.asInstanceOf[Field.WithFieldName[Z, Field14, A14]]
        def field15: Field.WithFieldName[Z, Field15, A15]                                           = field015.asInstanceOf[Field.WithFieldName[Z, Field15, A15]]
        def field16: Field.WithFieldName[Z, Field16, A16]                                           = field016.asInstanceOf[Field.WithFieldName[Z, Field16, A16]]
        def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z = construct0
        def annotations: Chunk[Any]                                                                 = annotations0

        def annotate(
          annotation: Any
        ): CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z] =
          CaseClass16(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            field011,
            field012,
            field013,
            field014,
            field015,
            field016,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](
      schema: CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        Field.WithFieldName[Z, schema.Field11, A11],
        Field.WithFieldName[Z, schema.Field12, A12],
        Field.WithFieldName[Z, schema.Field13, A13],
        Field.WithFieldName[Z, schema.Field14, A14],
        Field.WithFieldName[Z, schema.Field15, A15],
        Field.WithFieldName[Z, schema.Field16, A16],
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.field11,
          schema.field12,
          schema.field13,
          schema.field14,
          schema.field15,
          schema.field16,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      F11 <: Singleton with String,
      F12 <: Singleton with String,
      F13 <: Singleton with String,
      F14 <: Singleton with String,
      F15 <: Singleton with String,
      F16 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      Z
    ] =
      CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
        type Field11 = F11
        type Field12 = F12
        type Field13 = F13
        type Field14 = F14
        type Field15 = F15
        type Field16 = F16
      }
  }

  sealed trait CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]
      extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String
    type Field11 <: Singleton with String
    type Field12 <: Singleton with String
    type Field13 <: Singleton with String
    type Field14 <: Singleton with String
    type Field15 <: Singleton with String
    type Field16 <: Singleton with String
    type Field17 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10],
      Lens[Field11, Z, A11],
      Lens[Field12, Z, A12],
      Lens[Field13, Z, A13],
      Lens[Field14, Z, A14],
      Lens[Field15, Z, A15],
      Lens[Field16, Z, A16],
      Lens[Field17, Z, A17]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)
      with (Field11, A11)
      with (Field12, A12)
      with (Field13, A13)
      with (Field14, A14)
      with (Field15, A15)
      with (Field16, A16)
      with (Field17, A17)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10
      with Field11
      with Field12
      with Field13
      with Field14
      with Field15
      with Field16
      with Field17

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]
    def field11: Field.WithFieldName[Z, Field11, A11]
    def field12: Field.WithFieldName[Z, Field12, A12]
    def field13: Field.WithFieldName[Z, Field13, A13]
    def field14: Field.WithFieldName[Z, Field14, A14]
    def field15: Field.WithFieldName[Z, Field15, A15]
    def field16: Field.WithFieldName[Z, Field16, A16]
    def field17: Field.WithFieldName[Z, Field17, A17]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10],
      b.Lens[Field11, Z, A11],
      b.Lens[Field12, Z, A12],
      b.Lens[Field13, Z, A13],
      b.Lens[Field14, Z, A14],
      b.Lens[Field15, Z, A15],
      b.Lens[Field16, Z, A16],
      b.Lens[Field17, Z, A17]
    ) =
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

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10,
        field11,
        field12,
        field13,
        field14,
        field15,
        field16,
        field17
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
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
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value),
      field11.get(value),
      field12.get(value),
      field13.get(value),
      field14.get(value),
      field15.get(value),
      field16.get(value),
      field17.get(value)
    )

    override def toString: String = s"CaseClass17($id, ${fields.mkString(",")})"
  }

  object CaseClass17 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      field011: Field[Z, A11],
      field012: Field[Z, A12],
      field013: Field[Z, A13],
      field014: Field[Z, A14],
      field015: Field[Z, A15],
      field016: Field[Z, A16],
      field017: Field[Z, A17],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z] =
      new CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z] {
        def id: TypeId                                                                                   = id0
        def field1: Field.WithFieldName[Z, Field1, A1]                                                   = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]                                                   = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]                                                   = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]                                                   = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]                                                   = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]                                                   = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]                                                   = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]                                                   = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]                                                   = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10]                                                = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def field11: Field.WithFieldName[Z, Field11, A11]                                                = field011.asInstanceOf[Field.WithFieldName[Z, Field11, A11]]
        def field12: Field.WithFieldName[Z, Field12, A12]                                                = field012.asInstanceOf[Field.WithFieldName[Z, Field12, A12]]
        def field13: Field.WithFieldName[Z, Field13, A13]                                                = field013.asInstanceOf[Field.WithFieldName[Z, Field13, A13]]
        def field14: Field.WithFieldName[Z, Field14, A14]                                                = field014.asInstanceOf[Field.WithFieldName[Z, Field14, A14]]
        def field15: Field.WithFieldName[Z, Field15, A15]                                                = field015.asInstanceOf[Field.WithFieldName[Z, Field15, A15]]
        def field16: Field.WithFieldName[Z, Field16, A16]                                                = field016.asInstanceOf[Field.WithFieldName[Z, Field16, A16]]
        def field17: Field.WithFieldName[Z, Field17, A17]                                                = field017.asInstanceOf[Field.WithFieldName[Z, Field17, A17]]
        def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z = construct0
        def annotations: Chunk[Any]                                                                      = annotations0

        def annotate(
          annotation: Any
        ): CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z] =
          CaseClass17(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            field011,
            field012,
            field013,
            field014,
            field015,
            field016,
            field017,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](
      schema: CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        Field.WithFieldName[Z, schema.Field11, A11],
        Field.WithFieldName[Z, schema.Field12, A12],
        Field.WithFieldName[Z, schema.Field13, A13],
        Field.WithFieldName[Z, schema.Field14, A14],
        Field.WithFieldName[Z, schema.Field15, A15],
        Field.WithFieldName[Z, schema.Field16, A16],
        Field.WithFieldName[Z, schema.Field17, A17],
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.field11,
          schema.field12,
          schema.field13,
          schema.field14,
          schema.field15,
          schema.field16,
          schema.field17,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      F11 <: Singleton with String,
      F12 <: Singleton with String,
      F13 <: Singleton with String,
      F14 <: Singleton with String,
      F15 <: Singleton with String,
      F16 <: Singleton with String,
      F17 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      Z
    ] =
      CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
        type Field11 = F11
        type Field12 = F12
        type Field13 = F13
        type Field14 = F14
        type Field15 = F15
        type Field16 = F16
        type Field17 = F17
      }
  }

  sealed trait CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]
      extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String
    type Field11 <: Singleton with String
    type Field12 <: Singleton with String
    type Field13 <: Singleton with String
    type Field14 <: Singleton with String
    type Field15 <: Singleton with String
    type Field16 <: Singleton with String
    type Field17 <: Singleton with String
    type Field18 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10],
      Lens[Field11, Z, A11],
      Lens[Field12, Z, A12],
      Lens[Field13, Z, A13],
      Lens[Field14, Z, A14],
      Lens[Field15, Z, A15],
      Lens[Field16, Z, A16],
      Lens[Field17, Z, A17],
      Lens[Field18, Z, A18]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)
      with (Field11, A11)
      with (Field12, A12)
      with (Field13, A13)
      with (Field14, A14)
      with (Field15, A15)
      with (Field16, A16)
      with (Field17, A17)
      with (Field18, A18)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10
      with Field11
      with Field12
      with Field13
      with Field14
      with Field15
      with Field16
      with Field17
      with Field18

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]
    def field11: Field.WithFieldName[Z, Field11, A11]
    def field12: Field.WithFieldName[Z, Field12, A12]
    def field13: Field.WithFieldName[Z, Field13, A13]
    def field14: Field.WithFieldName[Z, Field14, A14]
    def field15: Field.WithFieldName[Z, Field15, A15]
    def field16: Field.WithFieldName[Z, Field16, A16]
    def field17: Field.WithFieldName[Z, Field17, A17]
    def field18: Field.WithFieldName[Z, Field18, A18]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10],
      b.Lens[Field11, Z, A11],
      b.Lens[Field12, Z, A12],
      b.Lens[Field13, Z, A13],
      b.Lens[Field14, Z, A14],
      b.Lens[Field15, Z, A15],
      b.Lens[Field16, Z, A16],
      b.Lens[Field17, Z, A17],
      b.Lens[Field18, Z, A18]
    ) =
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

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10,
        field11,
        field12,
        field13,
        field14,
        field15,
        field16,
        field17,
        field18
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
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
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value),
      field11.get(value),
      field12.get(value),
      field13.get(value),
      field14.get(value),
      field15.get(value),
      field16.get(value),
      field17.get(value),
      field18.get(value)
    )

    override def toString: String = s"CaseClass18($id, ${fields.mkString(",")})"
  }

  object CaseClass18 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      field011: Field[Z, A11],
      field012: Field[Z, A12],
      field013: Field[Z, A13],
      field014: Field[Z, A14],
      field015: Field[Z, A15],
      field016: Field[Z, A16],
      field017: Field[Z, A17],
      field018: Field[Z, A18],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z] =
      new CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z] {
        def id: TypeId                                    = id0
        def field1: Field.WithFieldName[Z, Field1, A1]    = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]    = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]    = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]    = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]    = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]    = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]    = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]    = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]    = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10] = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def field11: Field.WithFieldName[Z, Field11, A11] = field011.asInstanceOf[Field.WithFieldName[Z, Field11, A11]]
        def field12: Field.WithFieldName[Z, Field12, A12] = field012.asInstanceOf[Field.WithFieldName[Z, Field12, A12]]
        def field13: Field.WithFieldName[Z, Field13, A13] = field013.asInstanceOf[Field.WithFieldName[Z, Field13, A13]]
        def field14: Field.WithFieldName[Z, Field14, A14] = field014.asInstanceOf[Field.WithFieldName[Z, Field14, A14]]
        def field15: Field.WithFieldName[Z, Field15, A15] = field015.asInstanceOf[Field.WithFieldName[Z, Field15, A15]]
        def field16: Field.WithFieldName[Z, Field16, A16] = field016.asInstanceOf[Field.WithFieldName[Z, Field16, A16]]
        def field17: Field.WithFieldName[Z, Field17, A17] = field017.asInstanceOf[Field.WithFieldName[Z, Field17, A17]]
        def field18: Field.WithFieldName[Z, Field18, A18] = field018.asInstanceOf[Field.WithFieldName[Z, Field18, A18]]

        def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z =
          construct0
        def annotations: Chunk[Any] = annotations0

        def annotate(
          annotation: Any
        ): CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z] =
          CaseClass18(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            field011,
            field012,
            field013,
            field014,
            field015,
            field016,
            field017,
            field018,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](
      schema: CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        Field.WithFieldName[Z, schema.Field11, A11],
        Field.WithFieldName[Z, schema.Field12, A12],
        Field.WithFieldName[Z, schema.Field13, A13],
        Field.WithFieldName[Z, schema.Field14, A14],
        Field.WithFieldName[Z, schema.Field15, A15],
        Field.WithFieldName[Z, schema.Field16, A16],
        Field.WithFieldName[Z, schema.Field17, A17],
        Field.WithFieldName[Z, schema.Field18, A18],
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.field11,
          schema.field12,
          schema.field13,
          schema.field14,
          schema.field15,
          schema.field16,
          schema.field17,
          schema.field18,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      F11 <: Singleton with String,
      F12 <: Singleton with String,
      F13 <: Singleton with String,
      F14 <: Singleton with String,
      F15 <: Singleton with String,
      F16 <: Singleton with String,
      F17 <: Singleton with String,
      F18 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      Z
    ] =
      CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
        type Field11 = F11
        type Field12 = F12
        type Field13 = F13
        type Field14 = F14
        type Field15 = F15
        type Field16 = F16
        type Field17 = F17
        type Field18 = F18
      }
  }

  sealed trait CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]
      extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String
    type Field11 <: Singleton with String
    type Field12 <: Singleton with String
    type Field13 <: Singleton with String
    type Field14 <: Singleton with String
    type Field15 <: Singleton with String
    type Field16 <: Singleton with String
    type Field17 <: Singleton with String
    type Field18 <: Singleton with String
    type Field19 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10],
      Lens[Field11, Z, A11],
      Lens[Field12, Z, A12],
      Lens[Field13, Z, A13],
      Lens[Field14, Z, A14],
      Lens[Field15, Z, A15],
      Lens[Field16, Z, A16],
      Lens[Field17, Z, A17],
      Lens[Field18, Z, A18],
      Lens[Field19, Z, A19]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)
      with (Field11, A11)
      with (Field12, A12)
      with (Field13, A13)
      with (Field14, A14)
      with (Field15, A15)
      with (Field16, A16)
      with (Field17, A17)
      with (Field18, A18)
      with (Field19, A19)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10
      with Field11
      with Field12
      with Field13
      with Field14
      with Field15
      with Field16
      with Field17
      with Field18
      with Field19

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]
    def field11: Field.WithFieldName[Z, Field11, A11]
    def field12: Field.WithFieldName[Z, Field12, A12]
    def field13: Field.WithFieldName[Z, Field13, A13]
    def field14: Field.WithFieldName[Z, Field14, A14]
    def field15: Field.WithFieldName[Z, Field15, A15]
    def field16: Field.WithFieldName[Z, Field16, A16]
    def field17: Field.WithFieldName[Z, Field17, A17]
    def field18: Field.WithFieldName[Z, Field18, A18]
    def field19: Field.WithFieldName[Z, Field19, A19]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10],
      b.Lens[Field11, Z, A11],
      b.Lens[Field12, Z, A12],
      b.Lens[Field13, Z, A13],
      b.Lens[Field14, Z, A14],
      b.Lens[Field15, Z, A15],
      b.Lens[Field16, Z, A16],
      b.Lens[Field17, Z, A17],
      b.Lens[Field18, Z, A18],
      b.Lens[Field19, Z, A19]
    ) =
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

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10,
        field11,
        field12,
        field13,
        field14,
        field15,
        field16,
        field17,
        field18,
        field19
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
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
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value),
      field11.get(value),
      field12.get(value),
      field13.get(value),
      field14.get(value),
      field15.get(value),
      field16.get(value),
      field17.get(value),
      field18.get(value),
      field19.get(value)
    )

    override def toString: String = s"CaseClass19($id, ${fields.mkString(",")})"
  }

  object CaseClass19 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      field011: Field[Z, A11],
      field012: Field[Z, A12],
      field013: Field[Z, A13],
      field014: Field[Z, A14],
      field015: Field[Z, A15],
      field016: Field[Z, A16],
      field017: Field[Z, A17],
      field018: Field[Z, A18],
      field019: Field[Z, A19],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z] =
      new CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z] {
        def id: TypeId                                    = id0
        def field1: Field.WithFieldName[Z, Field1, A1]    = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]    = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]    = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]    = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]    = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]    = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]    = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]    = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]    = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10] = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def field11: Field.WithFieldName[Z, Field11, A11] = field011.asInstanceOf[Field.WithFieldName[Z, Field11, A11]]
        def field12: Field.WithFieldName[Z, Field12, A12] = field012.asInstanceOf[Field.WithFieldName[Z, Field12, A12]]
        def field13: Field.WithFieldName[Z, Field13, A13] = field013.asInstanceOf[Field.WithFieldName[Z, Field13, A13]]
        def field14: Field.WithFieldName[Z, Field14, A14] = field014.asInstanceOf[Field.WithFieldName[Z, Field14, A14]]
        def field15: Field.WithFieldName[Z, Field15, A15] = field015.asInstanceOf[Field.WithFieldName[Z, Field15, A15]]
        def field16: Field.WithFieldName[Z, Field16, A16] = field016.asInstanceOf[Field.WithFieldName[Z, Field16, A16]]
        def field17: Field.WithFieldName[Z, Field17, A17] = field017.asInstanceOf[Field.WithFieldName[Z, Field17, A17]]
        def field18: Field.WithFieldName[Z, Field18, A18] = field018.asInstanceOf[Field.WithFieldName[Z, Field18, A18]]
        def field19: Field.WithFieldName[Z, Field19, A19] = field019.asInstanceOf[Field.WithFieldName[Z, Field19, A19]]

        def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z =
          construct0
        def annotations: Chunk[Any] = annotations0

        def annotate(
          annotation: Any
        ): CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z] =
          CaseClass19(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            field011,
            field012,
            field013,
            field014,
            field015,
            field016,
            field017,
            field018,
            field019,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](
      schema: CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        Field.WithFieldName[Z, schema.Field11, A11],
        Field.WithFieldName[Z, schema.Field12, A12],
        Field.WithFieldName[Z, schema.Field13, A13],
        Field.WithFieldName[Z, schema.Field14, A14],
        Field.WithFieldName[Z, schema.Field15, A15],
        Field.WithFieldName[Z, schema.Field16, A16],
        Field.WithFieldName[Z, schema.Field17, A17],
        Field.WithFieldName[Z, schema.Field18, A18],
        Field.WithFieldName[Z, schema.Field19, A19],
        (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z,
        Chunk[Any]
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.field11,
          schema.field12,
          schema.field13,
          schema.field14,
          schema.field15,
          schema.field16,
          schema.field17,
          schema.field18,
          schema.field19,
          schema.construct,
          schema.annotations
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      F11 <: Singleton with String,
      F12 <: Singleton with String,
      F13 <: Singleton with String,
      F14 <: Singleton with String,
      F15 <: Singleton with String,
      F16 <: Singleton with String,
      F17 <: Singleton with String,
      F18 <: Singleton with String,
      F19 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      Z
    ] =
      CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
        type Field11 = F11
        type Field12 = F12
        type Field13 = F13
        type Field14 = F14
        type Field15 = F15
        type Field16 = F16
        type Field17 = F17
        type Field18 = F18
        type Field19 = F19
      }
  }

  sealed trait CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z]
      extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String
    type Field11 <: Singleton with String
    type Field12 <: Singleton with String
    type Field13 <: Singleton with String
    type Field14 <: Singleton with String
    type Field15 <: Singleton with String
    type Field16 <: Singleton with String
    type Field17 <: Singleton with String
    type Field18 <: Singleton with String
    type Field19 <: Singleton with String
    type Field20 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10],
      Lens[Field11, Z, A11],
      Lens[Field12, Z, A12],
      Lens[Field13, Z, A13],
      Lens[Field14, Z, A14],
      Lens[Field15, Z, A15],
      Lens[Field16, Z, A16],
      Lens[Field17, Z, A17],
      Lens[Field18, Z, A18],
      Lens[Field19, Z, A19],
      Lens[Field20, Z, A20]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)
      with (Field11, A11)
      with (Field12, A12)
      with (Field13, A13)
      with (Field14, A14)
      with (Field15, A15)
      with (Field16, A16)
      with (Field17, A17)
      with (Field18, A18)
      with (Field19, A19)
      with (Field20, A20)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10
      with Field11
      with Field12
      with Field13
      with Field14
      with Field15
      with Field16
      with Field17
      with Field18
      with Field19
      with Field20

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]
    def field11: Field.WithFieldName[Z, Field11, A11]
    def field12: Field.WithFieldName[Z, Field12, A12]
    def field13: Field.WithFieldName[Z, Field13, A13]
    def field14: Field.WithFieldName[Z, Field14, A14]
    def field15: Field.WithFieldName[Z, Field15, A15]
    def field16: Field.WithFieldName[Z, Field16, A16]
    def field17: Field.WithFieldName[Z, Field17, A17]
    def field18: Field.WithFieldName[Z, Field18, A18]
    def field19: Field.WithFieldName[Z, Field19, A19]
    def field20: Field.WithFieldName[Z, Field20, A20]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10],
      b.Lens[Field11, Z, A11],
      b.Lens[Field12, Z, A12],
      b.Lens[Field13, Z, A13],
      b.Lens[Field14, Z, A14],
      b.Lens[Field15, Z, A15],
      b.Lens[Field16, Z, A16],
      b.Lens[Field17, Z, A17],
      b.Lens[Field18, Z, A18],
      b.Lens[Field19, Z, A19],
      b.Lens[Field20, Z, A20]
    ) =
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

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10,
        field11,
        field12,
        field13,
        field14,
        field15,
        field16,
        field17,
        field18,
        field19,
        field20
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
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
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value),
      field11.get(value),
      field12.get(value),
      field13.get(value),
      field14.get(value),
      field15.get(value),
      field16.get(value),
      field17.get(value),
      field18.get(value),
      field19.get(value),
      field20.get(value)
    )

    override def toString: String = s"CaseClass20($id, ${fields.mkString(",")})"
  }

  object CaseClass20 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      field011: Field[Z, A11],
      field012: Field[Z, A12],
      field013: Field[Z, A13],
      field014: Field[Z, A14],
      field015: Field[Z, A15],
      field016: Field[Z, A16],
      field017: Field[Z, A17],
      field018: Field[Z, A18],
      field019: Field[Z, A19],
      field020: Field[Z, A20],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z] =
      new CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z] {
        def id: TypeId                                    = id0
        def field1: Field.WithFieldName[Z, Field1, A1]    = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]    = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]    = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]    = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]    = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]    = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]    = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]    = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]    = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10] = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def field11: Field.WithFieldName[Z, Field11, A11] = field011.asInstanceOf[Field.WithFieldName[Z, Field11, A11]]
        def field12: Field.WithFieldName[Z, Field12, A12] = field012.asInstanceOf[Field.WithFieldName[Z, Field12, A12]]
        def field13: Field.WithFieldName[Z, Field13, A13] = field013.asInstanceOf[Field.WithFieldName[Z, Field13, A13]]
        def field14: Field.WithFieldName[Z, Field14, A14] = field014.asInstanceOf[Field.WithFieldName[Z, Field14, A14]]
        def field15: Field.WithFieldName[Z, Field15, A15] = field015.asInstanceOf[Field.WithFieldName[Z, Field15, A15]]
        def field16: Field.WithFieldName[Z, Field16, A16] = field016.asInstanceOf[Field.WithFieldName[Z, Field16, A16]]
        def field17: Field.WithFieldName[Z, Field17, A17] = field017.asInstanceOf[Field.WithFieldName[Z, Field17, A17]]
        def field18: Field.WithFieldName[Z, Field18, A18] = field018.asInstanceOf[Field.WithFieldName[Z, Field18, A18]]
        def field19: Field.WithFieldName[Z, Field19, A19] = field019.asInstanceOf[Field.WithFieldName[Z, Field19, A19]]
        def field20: Field.WithFieldName[Z, Field20, A20] = field020.asInstanceOf[Field.WithFieldName[Z, Field20, A20]]

        def construct
          : (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z =
          construct0
        def annotations: Chunk[Any] = annotations0

        def annotate(
          annotation: Any
        ): CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z] =
          CaseClass20(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            field011,
            field012,
            field013,
            field014,
            field015,
            field016,
            field017,
            field018,
            field019,
            field020,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](
      schema: CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        Field.WithFieldName[Z, schema.Field11, A11],
        Field.WithFieldName[Z, schema.Field12, A12],
        Field.WithFieldName[Z, schema.Field13, A13],
        Field.WithFieldName[Z, schema.Field14, A14],
        Field.WithFieldName[Z, schema.Field15, A15],
        Field.WithFieldName[Z, schema.Field16, A16],
        Field.WithFieldName[Z, schema.Field17, A17],
        Field.WithFieldName[Z, schema.Field18, A18],
        Field.WithFieldName[Z, schema.Field19, A19],
        Field.WithFieldName[Z, schema.Field20, A20],
        ((A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z, Chunk[Any])
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.field11,
          schema.field12,
          schema.field13,
          schema.field14,
          schema.field15,
          schema.field16,
          schema.field17,
          schema.field18,
          schema.field19,
          schema.field20,
          (schema.construct, schema.annotations)
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      F11 <: Singleton with String,
      F12 <: Singleton with String,
      F13 <: Singleton with String,
      F14 <: Singleton with String,
      F15 <: Singleton with String,
      F16 <: Singleton with String,
      F17 <: Singleton with String,
      F18 <: Singleton with String,
      F19 <: Singleton with String,
      F20 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      Z
    ] =
      CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
        type Field11 = F11
        type Field12 = F12
        type Field13 = F13
        type Field14 = F14
        type Field15 = F15
        type Field16 = F16
        type Field17 = F17
        type Field18 = F18
        type Field19 = F19
        type Field20 = F20
      }
  }

  sealed trait CaseClass21[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21,
    Z
  ] extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String
    type Field11 <: Singleton with String
    type Field12 <: Singleton with String
    type Field13 <: Singleton with String
    type Field14 <: Singleton with String
    type Field15 <: Singleton with String
    type Field16 <: Singleton with String
    type Field17 <: Singleton with String
    type Field18 <: Singleton with String
    type Field19 <: Singleton with String
    type Field20 <: Singleton with String
    type Field21 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10],
      Lens[Field11, Z, A11],
      Lens[Field12, Z, A12],
      Lens[Field13, Z, A13],
      Lens[Field14, Z, A14],
      Lens[Field15, Z, A15],
      Lens[Field16, Z, A16],
      Lens[Field17, Z, A17],
      Lens[Field18, Z, A18],
      Lens[Field19, Z, A19],
      Lens[Field20, Z, A20],
      Lens[Field21, Z, A21]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)
      with (Field11, A11)
      with (Field12, A12)
      with (Field13, A13)
      with (Field14, A14)
      with (Field15, A15)
      with (Field16, A16)
      with (Field17, A17)
      with (Field18, A18)
      with (Field19, A19)
      with (Field20, A20)
      with (Field21, A21)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10
      with Field11
      with Field12
      with Field13
      with Field14
      with Field15
      with Field16
      with Field17
      with Field18
      with Field19
      with Field20
      with Field21

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]
    def field11: Field.WithFieldName[Z, Field11, A11]
    def field12: Field.WithFieldName[Z, Field12, A12]
    def field13: Field.WithFieldName[Z, Field13, A13]
    def field14: Field.WithFieldName[Z, Field14, A14]
    def field15: Field.WithFieldName[Z, Field15, A15]
    def field16: Field.WithFieldName[Z, Field16, A16]
    def field17: Field.WithFieldName[Z, Field17, A17]
    def field18: Field.WithFieldName[Z, Field18, A18]
    def field19: Field.WithFieldName[Z, Field19, A19]
    def field20: Field.WithFieldName[Z, Field20, A20]
    def field21: Field.WithFieldName[Z, Field21, A21]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10],
      b.Lens[Field11, Z, A11],
      b.Lens[Field12, Z, A12],
      b.Lens[Field13, Z, A13],
      b.Lens[Field14, Z, A14],
      b.Lens[Field15, Z, A15],
      b.Lens[Field16, Z, A16],
      b.Lens[Field17, Z, A17],
      b.Lens[Field18, Z, A18],
      b.Lens[Field19, Z, A19],
      b.Lens[Field20, Z, A20],
      b.Lens[Field21, Z, A21]
    ) =
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

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10,
        field11,
        field12,
        field13,
        field14,
        field15,
        field16,
        field17,
        field18,
        field19,
        field20,
        field21
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
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
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value),
      field11.get(value),
      field12.get(value),
      field13.get(value),
      field14.get(value),
      field15.get(value),
      field16.get(value),
      field17.get(value),
      field18.get(value),
      field19.get(value),
      field20.get(value),
      field21.get(value)
    )

    override def toString: String = s"CaseClass21($id, ${fields.mkString(",")})"
  }

  object CaseClass21 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      field011: Field[Z, A11],
      field012: Field[Z, A12],
      field013: Field[Z, A13],
      field014: Field[Z, A14],
      field015: Field[Z, A15],
      field016: Field[Z, A16],
      field017: Field[Z, A17],
      field018: Field[Z, A18],
      field019: Field[Z, A19],
      field020: Field[Z, A20],
      field021: Field[Z, A21],
      construct0: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z] =
      new CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z] {
        def id: TypeId                                    = id0
        def field1: Field.WithFieldName[Z, Field1, A1]    = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]    = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]    = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]    = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]    = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]    = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]    = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]    = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]    = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10] = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def field11: Field.WithFieldName[Z, Field11, A11] = field011.asInstanceOf[Field.WithFieldName[Z, Field11, A11]]
        def field12: Field.WithFieldName[Z, Field12, A12] = field012.asInstanceOf[Field.WithFieldName[Z, Field12, A12]]
        def field13: Field.WithFieldName[Z, Field13, A13] = field013.asInstanceOf[Field.WithFieldName[Z, Field13, A13]]
        def field14: Field.WithFieldName[Z, Field14, A14] = field014.asInstanceOf[Field.WithFieldName[Z, Field14, A14]]
        def field15: Field.WithFieldName[Z, Field15, A15] = field015.asInstanceOf[Field.WithFieldName[Z, Field15, A15]]
        def field16: Field.WithFieldName[Z, Field16, A16] = field016.asInstanceOf[Field.WithFieldName[Z, Field16, A16]]
        def field17: Field.WithFieldName[Z, Field17, A17] = field017.asInstanceOf[Field.WithFieldName[Z, Field17, A17]]
        def field18: Field.WithFieldName[Z, Field18, A18] = field018.asInstanceOf[Field.WithFieldName[Z, Field18, A18]]
        def field19: Field.WithFieldName[Z, Field19, A19] = field019.asInstanceOf[Field.WithFieldName[Z, Field19, A19]]
        def field20: Field.WithFieldName[Z, Field20, A20] = field020.asInstanceOf[Field.WithFieldName[Z, Field20, A20]]
        def field21: Field.WithFieldName[Z, Field21, A21] = field021.asInstanceOf[Field.WithFieldName[Z, Field21, A21]]

        def construct
          : (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z =
          construct0
        def annotations: Chunk[Any] = annotations0

        def annotate(annotation: Any): CaseClass21[
          A1,
          A2,
          A3,
          A4,
          A5,
          A6,
          A7,
          A8,
          A9,
          A10,
          A11,
          A12,
          A13,
          A14,
          A15,
          A16,
          A17,
          A18,
          A19,
          A20,
          A21,
          Z
        ] =
          CaseClass21(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            field011,
            field012,
            field013,
            field014,
            field015,
            field016,
            field017,
            field018,
            field019,
            field020,
            field021,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](
      schema: CaseClass21[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        Z
      ]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        Field.WithFieldName[Z, schema.Field11, A11],
        Field.WithFieldName[Z, schema.Field12, A12],
        Field.WithFieldName[Z, schema.Field13, A13],
        Field.WithFieldName[Z, schema.Field14, A14],
        Field.WithFieldName[Z, schema.Field15, A15],
        Field.WithFieldName[Z, schema.Field16, A16],
        Field.WithFieldName[Z, schema.Field17, A17],
        Field.WithFieldName[Z, schema.Field18, A18],
        Field.WithFieldName[Z, schema.Field19, A19],
        Field.WithFieldName[Z, schema.Field20, A20],
        (
          Field.WithFieldName[Z, schema.Field21, A21],
          (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z,
          Chunk[Any]
        )
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.field11,
          schema.field12,
          schema.field13,
          schema.field14,
          schema.field15,
          schema.field16,
          schema.field17,
          schema.field18,
          schema.field19,
          schema.field20,
          (schema.field21, schema.construct, schema.annotations)
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      F11 <: Singleton with String,
      F12 <: Singleton with String,
      F13 <: Singleton with String,
      F14 <: Singleton with String,
      F15 <: Singleton with String,
      F16 <: Singleton with String,
      F17 <: Singleton with String,
      F18 <: Singleton with String,
      F19 <: Singleton with String,
      F20 <: Singleton with String,
      F21 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      Z
    ] =
      CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
        type Field11 = F11
        type Field12 = F12
        type Field13 = F13
        type Field14 = F14
        type Field15 = F15
        type Field16 = F16
        type Field17 = F17
        type Field18 = F18
        type Field19 = F19
        type Field20 = F20
        type Field21 = F21
      }
  }

  sealed trait CaseClass22[
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,
    A10,
    A11,
    A12,
    A13,
    A14,
    A15,
    A16,
    A17,
    A18,
    A19,
    A20,
    A21,
    A22,
    Z
  ] extends Record[Z] { self =>

    type Field1 <: Singleton with String
    type Field2 <: Singleton with String
    type Field3 <: Singleton with String
    type Field4 <: Singleton with String
    type Field5 <: Singleton with String
    type Field6 <: Singleton with String
    type Field7 <: Singleton with String
    type Field8 <: Singleton with String
    type Field9 <: Singleton with String
    type Field10 <: Singleton with String
    type Field11 <: Singleton with String
    type Field12 <: Singleton with String
    type Field13 <: Singleton with String
    type Field14 <: Singleton with String
    type Field15 <: Singleton with String
    type Field16 <: Singleton with String
    type Field17 <: Singleton with String
    type Field18 <: Singleton with String
    type Field19 <: Singleton with String
    type Field20 <: Singleton with String
    type Field21 <: Singleton with String
    type Field22 <: Singleton with String

    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = (
      Lens[Field1, Z, A1],
      Lens[Field2, Z, A2],
      Lens[Field3, Z, A3],
      Lens[Field4, Z, A4],
      Lens[Field5, Z, A5],
      Lens[Field6, Z, A6],
      Lens[Field7, Z, A7],
      Lens[Field8, Z, A8],
      Lens[Field9, Z, A9],
      Lens[Field10, Z, A10],
      Lens[Field11, Z, A11],
      Lens[Field12, Z, A12],
      Lens[Field13, Z, A13],
      Lens[Field14, Z, A14],
      Lens[Field15, Z, A15],
      Lens[Field16, Z, A16],
      Lens[Field17, Z, A17],
      Lens[Field18, Z, A18],
      Lens[Field19, Z, A19],
      Lens[Field20, Z, A20],
      Lens[Field21, Z, A21],
      Lens[Field22, Z, A22]
    )

    override type Terms = (Field1, A1)
      with (Field2, A2)
      with (Field3, A3)
      with (Field4, A4)
      with (Field5, A5)
      with (Field6, A6)
      with (Field7, A7)
      with (Field8, A8)
      with (Field10, A10)
      with (Field11, A11)
      with (Field12, A12)
      with (Field13, A13)
      with (Field14, A14)
      with (Field15, A15)
      with (Field16, A16)
      with (Field17, A17)
      with (Field18, A18)
      with (Field19, A19)
      with (Field20, A20)
      with (Field21, A21)
      with (Field22, A22)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9
      with Field10
      with Field11
      with Field12
      with Field13
      with Field14
      with Field15
      with Field16
      with Field17
      with Field18
      with Field19
      with Field20
      with Field21
      with Field22

    def id: TypeId
    def field1: Field.WithFieldName[Z, Field1, A1]
    def field2: Field.WithFieldName[Z, Field2, A2]
    def field3: Field.WithFieldName[Z, Field3, A3]
    def field4: Field.WithFieldName[Z, Field4, A4]
    def field5: Field.WithFieldName[Z, Field5, A5]
    def field6: Field.WithFieldName[Z, Field6, A6]
    def field7: Field.WithFieldName[Z, Field7, A7]
    def field8: Field.WithFieldName[Z, Field8, A8]
    def field9: Field.WithFieldName[Z, Field9, A9]
    def field10: Field.WithFieldName[Z, Field10, A10]
    def field11: Field.WithFieldName[Z, Field11, A11]
    def field12: Field.WithFieldName[Z, Field12, A12]
    def field13: Field.WithFieldName[Z, Field13, A13]
    def field14: Field.WithFieldName[Z, Field14, A14]
    def field15: Field.WithFieldName[Z, Field15, A15]
    def field16: Field.WithFieldName[Z, Field16, A16]
    def field17: Field.WithFieldName[Z, Field17, A17]
    def field18: Field.WithFieldName[Z, Field18, A18]
    def field19: Field.WithFieldName[Z, Field19, A19]
    def field20: Field.WithFieldName[Z, Field20, A20]
    def field21: Field.WithFieldName[Z, Field21, A21]
    def field22: Field.WithFieldName[Z, Field22, A22]

    def construct: (
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      A22
    ) => Z
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4],
      b.Lens[Field5, Z, A5],
      b.Lens[Field6, Z, A6],
      b.Lens[Field7, Z, A7],
      b.Lens[Field8, Z, A8],
      b.Lens[Field9, Z, A9],
      b.Lens[Field10, Z, A10],
      b.Lens[Field11, Z, A11],
      b.Lens[Field12, Z, A12],
      b.Lens[Field13, Z, A13],
      b.Lens[Field14, Z, A14],
      b.Lens[Field15, Z, A15],
      b.Lens[Field16, Z, A16],
      b.Lens[Field17, Z, A17],
      b.Lens[Field18, Z, A18],
      b.Lens[Field19, Z, A19],
      b.Lens[Field20, Z, A20],
      b.Lens[Field21, Z, A21],
      b.Lens[Field22, Z, A22]
    ) =
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

    override def fields: Chunk[Field[Z, _]] =
      Chunk(
        field1,
        field2,
        field3,
        field4,
        field5,
        field6,
        field7,
        field8,
        field9,
        field10,
        field11,
        field12,
        field13,
        field14,
        field15,
        field16,
        field17,
        field18,
        field19,
        field20,
        field21,
        field22
      )

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
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
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Any] = Chunk(
      field1.get(value),
      field2.get(value),
      field3.get(value),
      field4.get(value),
      field5.get(value),
      field6.get(value),
      field7.get(value),
      field8.get(value),
      field9.get(value),
      field10.get(value),
      field11.get(value),
      field12.get(value),
      field13.get(value),
      field14.get(value),
      field15.get(value),
      field16.get(value),
      field17.get(value),
      field18.get(value),
      field19.get(value),
      field20.get(value),
      field21.get(value),
      field22.get(value)
    )

    override def toString: String = s"CaseClass22($id, ${fields.mkString(",")})"
  }

  object CaseClass22 {

    def apply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](
      id0: TypeId,
      field01: Field[Z, A1],
      field02: Field[Z, A2],
      field03: Field[Z, A3],
      field04: Field[Z, A4],
      field05: Field[Z, A5],
      field06: Field[Z, A6],
      field07: Field[Z, A7],
      field08: Field[Z, A8],
      field09: Field[Z, A9],
      field010: Field[Z, A10],
      field011: Field[Z, A11],
      field012: Field[Z, A12],
      field013: Field[Z, A13],
      field014: Field[Z, A14],
      field015: Field[Z, A15],
      field016: Field[Z, A16],
      field017: Field[Z, A17],
      field018: Field[Z, A18],
      field019: Field[Z, A19],
      field020: Field[Z, A20],
      field021: Field[Z, A21],
      field022: Field[Z, A22],
      construct0: (
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        A22
      ) => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass22[
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      A22,
      Z
    ] =
      new CaseClass22[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        A22,
        Z
      ] {
        def id: TypeId                                    = id0
        def field1: Field.WithFieldName[Z, Field1, A1]    = field01.asInstanceOf[Field.WithFieldName[Z, Field1, A1]]
        def field2: Field.WithFieldName[Z, Field2, A2]    = field02.asInstanceOf[Field.WithFieldName[Z, Field2, A2]]
        def field3: Field.WithFieldName[Z, Field3, A3]    = field03.asInstanceOf[Field.WithFieldName[Z, Field3, A3]]
        def field4: Field.WithFieldName[Z, Field4, A4]    = field04.asInstanceOf[Field.WithFieldName[Z, Field4, A4]]
        def field5: Field.WithFieldName[Z, Field5, A5]    = field05.asInstanceOf[Field.WithFieldName[Z, Field5, A5]]
        def field6: Field.WithFieldName[Z, Field6, A6]    = field06.asInstanceOf[Field.WithFieldName[Z, Field6, A6]]
        def field7: Field.WithFieldName[Z, Field7, A7]    = field07.asInstanceOf[Field.WithFieldName[Z, Field7, A7]]
        def field8: Field.WithFieldName[Z, Field8, A8]    = field08.asInstanceOf[Field.WithFieldName[Z, Field8, A8]]
        def field9: Field.WithFieldName[Z, Field9, A9]    = field09.asInstanceOf[Field.WithFieldName[Z, Field9, A9]]
        def field10: Field.WithFieldName[Z, Field10, A10] = field010.asInstanceOf[Field.WithFieldName[Z, Field10, A10]]
        def field11: Field.WithFieldName[Z, Field11, A11] = field011.asInstanceOf[Field.WithFieldName[Z, Field11, A11]]
        def field12: Field.WithFieldName[Z, Field12, A12] = field012.asInstanceOf[Field.WithFieldName[Z, Field12, A12]]
        def field13: Field.WithFieldName[Z, Field13, A13] = field013.asInstanceOf[Field.WithFieldName[Z, Field13, A13]]
        def field14: Field.WithFieldName[Z, Field14, A14] = field014.asInstanceOf[Field.WithFieldName[Z, Field14, A14]]
        def field15: Field.WithFieldName[Z, Field15, A15] = field015.asInstanceOf[Field.WithFieldName[Z, Field15, A15]]
        def field16: Field.WithFieldName[Z, Field16, A16] = field016.asInstanceOf[Field.WithFieldName[Z, Field16, A16]]
        def field17: Field.WithFieldName[Z, Field17, A17] = field017.asInstanceOf[Field.WithFieldName[Z, Field17, A17]]
        def field18: Field.WithFieldName[Z, Field18, A18] = field018.asInstanceOf[Field.WithFieldName[Z, Field18, A18]]
        def field19: Field.WithFieldName[Z, Field19, A19] = field019.asInstanceOf[Field.WithFieldName[Z, Field19, A19]]
        def field20: Field.WithFieldName[Z, Field20, A20] = field020.asInstanceOf[Field.WithFieldName[Z, Field20, A20]]
        def field21: Field.WithFieldName[Z, Field21, A21] = field021.asInstanceOf[Field.WithFieldName[Z, Field21, A21]]
        def field22: Field.WithFieldName[Z, Field22, A22] = field022.asInstanceOf[Field.WithFieldName[Z, Field22, A22]]

        def construct
          : (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => Z =
          construct0
        def annotations: Chunk[Any] = annotations0

        def annotate(annotation: Any): CaseClass22[
          A1,
          A2,
          A3,
          A4,
          A5,
          A6,
          A7,
          A8,
          A9,
          A10,
          A11,
          A12,
          A13,
          A14,
          A15,
          A16,
          A17,
          A18,
          A19,
          A20,
          A21,
          A22,
          Z
        ] =
          CaseClass22(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            field07,
            field08,
            field09,
            field010,
            field011,
            field012,
            field013,
            field014,
            field015,
            field016,
            field017,
            field018,
            field019,
            field020,
            field021,
            field022,
            construct0,
            annotations0 :+ annotation
          )
      }

    def unapply[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](
      schema: CaseClass22[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        A22,
        Z
      ]
    ): Some[
      (
        TypeId,
        Field.WithFieldName[Z, schema.Field1, A1],
        Field.WithFieldName[Z, schema.Field2, A2],
        Field.WithFieldName[Z, schema.Field3, A3],
        Field.WithFieldName[Z, schema.Field4, A4],
        Field.WithFieldName[Z, schema.Field5, A5],
        Field.WithFieldName[Z, schema.Field6, A6],
        Field.WithFieldName[Z, schema.Field7, A7],
        Field.WithFieldName[Z, schema.Field8, A8],
        Field.WithFieldName[Z, schema.Field9, A9],
        Field.WithFieldName[Z, schema.Field10, A10],
        Field.WithFieldName[Z, schema.Field11, A11],
        Field.WithFieldName[Z, schema.Field12, A12],
        Field.WithFieldName[Z, schema.Field13, A13],
        Field.WithFieldName[Z, schema.Field14, A14],
        Field.WithFieldName[Z, schema.Field15, A15],
        Field.WithFieldName[Z, schema.Field16, A16],
        Field.WithFieldName[Z, schema.Field17, A17],
        Field.WithFieldName[Z, schema.Field18, A18],
        Field.WithFieldName[Z, schema.Field19, A19],
        Field.WithFieldName[Z, schema.Field20, A20],
        (
          Field.WithFieldName[Z, schema.Field21, A21],
          Field.WithFieldName[Z, schema.Field22, A22],
          (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => Z,
          Chunk[Any]
        )
      )
    ] =
      Some(
        (
          schema.id,
          schema.field1,
          schema.field2,
          schema.field3,
          schema.field4,
          schema.field5,
          schema.field6,
          schema.field7,
          schema.field8,
          schema.field9,
          schema.field10,
          schema.field11,
          schema.field12,
          schema.field13,
          schema.field14,
          schema.field15,
          schema.field16,
          schema.field17,
          schema.field18,
          schema.field19,
          schema.field20,
          (schema.field21, schema.field22, schema.construct, schema.annotations)
        )
      )

    type WithFields[
      F1 <: Singleton with String,
      F2 <: Singleton with String,
      F3 <: Singleton with String,
      F4 <: Singleton with String,
      F5 <: Singleton with String,
      F6 <: Singleton with String,
      F7 <: Singleton with String,
      F8 <: Singleton with String,
      F9 <: Singleton with String,
      F10 <: Singleton with String,
      F11 <: Singleton with String,
      F12 <: Singleton with String,
      F13 <: Singleton with String,
      F14 <: Singleton with String,
      F15 <: Singleton with String,
      F16 <: Singleton with String,
      F17 <: Singleton with String,
      F18 <: Singleton with String,
      F19 <: Singleton with String,
      F20 <: Singleton with String,
      F21 <: Singleton with String,
      F22 <: Singleton with String,
      A1,
      A2,
      A3,
      A4,
      A5,
      A6,
      A7,
      A8,
      A9,
      A10,
      A11,
      A12,
      A13,
      A14,
      A15,
      A16,
      A17,
      A18,
      A19,
      A20,
      A21,
      A22,
      Z
    ] =
      CaseClass22[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        A22,
        Z
      ] {
        type Field1  = F1
        type Field2  = F2
        type Field3  = F3
        type Field4  = F4
        type Field5  = F5
        type Field6  = F6
        type Field7  = F7
        type Field8  = F8
        type Field9  = F9
        type Field10 = F10
        type Field11 = F11
        type Field12 = F12
        type Field13 = F13
        type Field14 = F14
        type Field15 = F15
        type Field16 = F16
        type Field17 = F17
        type Field18 = F18
        type Field19 = F19
        type Field20 = F20
        type Field21 = F21
        type Field22 = F22
      }
  }
}
