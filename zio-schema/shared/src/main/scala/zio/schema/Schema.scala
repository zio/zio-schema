package zio.schema

import java.net.URI
import java.time.temporal.ChronoUnit

import scala.annotation.tailrec
import scala.collection.immutable.ListMap

import zio.prelude.NonEmptySet
import zio.schema.annotation._
import zio.schema.internal.SourceLocation
import zio.schema.meta._
import zio.schema.validation._
import zio.{Chunk, NonEmptyChunk, Unsafe, prelude}

/**
 * A `Schema[A]` describes the structure of some data type `A`, in terms of case
 * classes, enumerations (sealed traits), collections, and various primitive
 * types (including not only Scala's own primitive types, but enhanced with
 * java.time and big integers / decimals).
 *
 * A schema is an ordinary value, so it can be introspected, transformed, and combined with ordinary Scala code. The codecs (JSON, Protobuf, Avro and others), diffing and patching, conversion to and from [[DynamicValue]], ordering, validation and migration all work from a schema.
 *
 * The companion object provides implicit schemas for the Scala primitives, `java.time` types, `Option`, `Either`, tuples, `List`, `Vector`, `Set`, `Map` and `Chunk`. For your own data types, either build a schema by hand with the constructors and operators of the companion object (for example `Schema.record`, `Schema.enumeration`, `transform` and `zip`), or derive it at compile time with the `DeriveSchema.gen[A]` macro from the zio-schema-derivation module:
 *
 * {{{
 * final case class Person(name: String, age: Int)
 * object Person {
 *   implicit val personSchema: Schema[Person] = DeriveSchema.gen[Person]
 * }
 *
 * val dynamic = Schema[Person].toDynamic(Person("Ann", 30))
 * val person  = Schema[Person].fromDynamic(dynamic)
 * // Right(Person(Ann,30))
 * }}}
 */
sealed trait Schema[A] {
  self =>

  /**
   * The type of the optics that `makeAccessors` builds for this schema, given the lens, prism and traversal types of an [[AccessorBuilder]]. Records give lenses, enumerations give prisms, and collections give a traversal.
   */
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
   * Returns a default value of type `A` built from this schema, or a `Left` with a message when the schema has none. For example, `Schema[Int]` gives `Right(0)`, `Schema[String]` gives `Right("")`, `Schema[Option[Int]]` gives `Right(None)`, `Schema[List[Int]]` gives `Right(List())`, a derived case class schema builds a value from its fields' defaults, and `Schema[NonEmptyChunk[Int]]` gives a `Left`.
   */
  def defaultValue: scala.util.Either[String, A]

  /**
   * Returns this schema with a `fieldDefaultValue(value)` annotation added. Only some schemas read that annotation in `defaultValue`: a primitive schema such as `Schema[Int].default(5)` returns `Right(5)`, while an `Option` schema still returns `Right(None)`, a `List` schema still returns `Right(List())`, and a derived case class schema and a schema made with `transform` ignore it.
   */
  def default(value: A): Schema[A] = self.annotate(fieldDefaultValue(value))

  /**
   * The annotations attached to this schema, both those added with `annotate` and those that schema derivation copies from the Scala annotations on the type.
   */
  def annotations: Chunk[Any]

  /**
   * Returns the [[zio.schema.meta.MetaSchema]] of this schema: a serializable description of its structure, without the functions (constructors, getters, transformations) that the schema holds.
   */
  def ast: MetaSchema = MetaSchema.fromSchema(self)

  /**
   * Returns a copy of this schema with `annotation` added to its annotations.
   */
  def annotate(annotation: Any): Schema[A]

  /**
   * Returns a `Schema[B]` that keeps this schema's structure and converts between `A` and `B` with migrations derived in both directions (see `migrate`), or a `Left` when either migration cannot be derived.
   *
   * It can convert, for example, between a case class and its generic representation as a `ListMap[String, _]`.
   */
  def coerce[B](newSchema: Schema[B]): Either[String, Schema[B]] =
    for {
      f <- self.migrate(newSchema)
      g <- newSchema.migrate(self)
    } yield self.transformOrFail(f, g)

  /**
   * Returns a [[Patch]] that turns `thisValue` into `thatValue`, computed by the [[Differ]] that `Differ.fromSchema` picks for this schema.
   */
  def diff(thisValue: A, thatValue: A): Patch[A] = Differ.fromSchema(self)(thisValue, thatValue)

  /**
   * Applies `diff` to `oldValue`, returning the patched value or a `Left` with a message when the patch does not apply.
   */
  def patch(oldValue: A, diff: Patch[A]): scala.util.Either[String, A] = diff.patch(oldValue)

  /**
   * Converts a [[DynamicValue]] to a value of type `A`, or returns a `Left` with a message when the dynamic value does not match this schema.
   */
  def fromDynamic(value: DynamicValue): scala.util.Either[String, A] =
    value.toTypedValue(self)

  /**
   * Builds the optics (lenses, prisms or traversals) for this schema with the given [[AccessorBuilder]]. The result type is `Accessors`.
   */
  def makeAccessors(b: AccessorBuilder): Accessors[b.Lens, b.Prism, b.Traversal]

  /**
   * Returns a function that converts a value of `A` into a value of `B` through [[DynamicValue]], using the migrations that [[zio.schema.meta.Migration]] derives from the structures of the two schemas, or a `Left` when no such migration can be derived. The function itself returns a `Left` when a value cannot be converted.
   */
  def migrate[B](newSchema: Schema[B]): Either[String, A => scala.util.Either[String, B]] =
    Migration.derive(MetaSchema.fromSchema(self), MetaSchema.fromSchema(newSchema)).map { transforms => (a: A) =>
      self.toDynamic(a).transform(transforms).flatMap(newSchema.fromDynamic)
    }

  /**
   * Returns a new schema that modifies the type produced by this schema to be
   * optional.
   */
  def optional: Schema[Option[A]] = Schema.Optional(self)

  /**
   * Returns an `Ordering` for `A` that follows the structure of this schema: primitives compare with their [[StandardType]], and case class values compare field by field in declaration order.
   */
  def ordering: Ordering[A] = SchemaOrdering.ordering(this)

  /**
   * Returns a new schema that combines this schema and the specified schema
   * together, modeling their either composition.
   */
  def orElseEither[B](that: Schema[B]): Schema[scala.util.Either[A, B]] = Schema.Either(self, that)

  /**
   * Returns a schema for a `Chunk` of values of this schema.
   */
  def repeated: Schema[Chunk[A]] = Schema.chunk(self)

  /**
   * Returns a `Schema[Schema[A]]` built from the schema that `MetaSchema.toSchema` gives for this schema's values (a generic record for a case class), transformed with `coerce` in one direction and `ast.toSchema` in the other. Encoding a schema with it does not encode its structure: encoding `Schema[Person]` with `JsonCodec` through `Schema[Person].serializable` throws a `java.lang.ClassCastException`. To encode and decode schemas, use the implicit `Schema[Schema[A]]` (`schemaSchema`).
   */
  def serializable: Schema[Schema[A]] =
    MetaSchema
      .fromSchema(self)
      .toSchema
      .asInstanceOf[Schema[Schema[_]]]
      .transformOrFail(
        s => s.coerce(self),
        s => Right(s.ast.toSchema)
      )

  /**
   * Converts `value` to a [[DynamicValue]] that follows the structure of this schema. When a transformation of the schema fails, the result contains a `DynamicValue.Error` with the message.
   */
  def toDynamic(value: A): DynamicValue =
    DynamicValue.fromSchemaAndValue(self, value)

  /**
   * Transforms this `Schema[A]` into a `Schema[B]`, by supplying two functions
   * that can transform between `A` and `B`, without possibility of failure.
   */
  def transform[B](f: A => B, g: B => A)(implicit loc: SourceLocation): Schema[B] =
    Schema.Transform[A, B, SourceLocation](self, a => Right(f(a)), b => Right(g(b)), annotations, loc)

  /**
   * Transforms this `Schema[A]` into a `Schema[B]`, by supplying two functions
   * that can transform between `A` and `B` (possibly failing in some cases).
   */
  def transformOrFail[B](f: A => scala.util.Either[String, B], g: B => scala.util.Either[String, A])(implicit
    loc: SourceLocation
  ): Schema[B] =
    Schema.Transform[A, B, SourceLocation](self, f, g, annotations, loc)

  /**
   * Validates `value` with `Schema.validate`, using the implicit `schema` parameter rather than this schema.
   */
  def validate(value: A)(implicit schema: Schema[A]): Chunk[ValidationError] = Schema.validate[A](value)

  /**
   * Returns this schema with a `zio.schema.annotation.validate(validation)` annotation added. `Schema.validate` does not read this annotation: `Schema.validate(-1)(Schema[Int].validation(Validation.greaterThan(0)))` returns an empty chunk. To have a validation checked, put it on a case class field with the `@validate` annotation or pass it to `Schema.Field`.
   */
  def validation(validation: Validation[A]): Schema[A] = annotate(zio.schema.annotation.validate(validation))

  /**
   * Returns a new schema that combines this schema and the specified schema
   * together, modeling their tuple composition.
   */
  def zip[B](that: Schema[B]): Schema[(A, B)] = Schema.Tuple2(self, that)
}

/**
 * Constructors for schemas, the implicit schemas of the standard Scala and `java.time` types, and the case classes that make up the schema tree (`Primitive`, `Record`, `Enum`, `Sequence`, `Transform` and others), which codecs and other interpreters match on.
 */
object Schema extends SchemaPlatformSpecific with SchemaEquality with SchemaVersionSpecific {
  /**
   * Returns the implicit schema for `A` in scope.
   */
  def apply[A](implicit schema: Schema[A]): Schema[A] = schema

  /**
   * Returns a [[Schema.Lazy]] that evaluates `schema` the first time it is needed. Use it to define the schema of a recursive data type.
   */
  def defer[A](schema: => Schema[A]): Schema[A] = Lazy(() => schema)

  /**
   * Returns an [[Schema.EnumN]] schema for a sum type with the given type id and cases.
   */
  def enumeration[A, C <: CaseSet.Aux[A]](id: TypeId, caseSet: C, annotations: Chunk[Any] = Chunk.empty): Schema[A] =
    EnumN(id, caseSet, annotations)

  /**
   * Returns a [[Schema.Fail]] schema, which stands for the absence of schema information for `A`. Its `defaultValue` is `Left(message)`.
   */
  def fail[A](message: String): Schema[A] = Fail(message)

  /**
   * Returns a schema of `A` from a schema of `(A, Unit)`, dropping the `Unit` half.
   */
  def first[A](schema: Schema[(A, Unit)]): Schema[A] =
    schema.transform[A](_._1, a => (a, ()))

  /**
   * Unwraps nested [[Schema.Lazy]] schemas until it reaches a schema that is not lazy, and returns it.
   */
  @tailrec
  def force[A](schema: Schema[A]): Schema[A] =
    schema match {
      case Schema.Lazy(f) => force(f())
      case _              => schema
    }

  /**
   * Returns the [[TypeId]] of the type that `schema` describes: the stored id of a record or enumeration, the Scala or Java type name of a primitive (`scala.Int`, `java.lang.String`), and the collection type name of a sequence (`scala.collection.immutable.List`). A transformed or lazy schema gives the id of its underlying schema, and `Schema.fail` gives `TypeId.Structural`.
   */
  def getTypeId[A](schema: Schema[A]): TypeId =
    schema match {
      case record: Record[_]          => record.id
      case enum0: Enum[_]             => enum0.id
      case Primitive(standardType, _) =>
        val typeName = standardType.tag match {
          case "unit"           => "scala.Unit"
          case "string"         => "java.lang.String"
          case "boolean"        => "scala.Boolean"
          case "byte"           => "scala.Byte"
          case "short"          => "scala.Short"
          case "int"            => "scala.Int"
          case "long"           => "scala.Long"
          case "float"          => "scala.Float"
          case "double"         => "scala.Double"
          case "binary"         => "zio.Chunk"
          case "char"           => "scala.Char"
          case "bigDecimal"     => "java.math.BigDecimal"
          case "bigInteger"     => "java.math.BigInteger"
          case "dayOfWeek"      => "java.time.DayOfWeek"
          case "month"          => "java.time.Month"
          case "monthDay"       => "java.time.MonthDay"
          case "period"         => "java.time.Period"
          case "year"           => "java.time.Year"
          case "yearMonth"      => "java.time.YearMonth"
          case "zoneId"         => "java.time.ZoneId"
          case "zoneOffset"     => "java.time.ZoneOffset"
          case "duration"       => "java.time.Duration"
          case "instant"        => "java.time.Instant"
          case "localDate"      => "java.time.LocalDate"
          case "localTime"      => "java.time.LocalTime"
          case "localDateTime"  => "java.time.LocalDateTime"
          case "offsetTime"     => "java.time.OffsetTime"
          case "offsetDateTime" => "java.time.OffsetDateTime"
          case "zonedDateTime"  => "java.time.ZonedDateTime"
          case "uuid"           => "java.util.UUID"
          case "currency"       => "java.util.Currency"
          case other            => other
        }
        TypeId.parse(typeName)
      case Transform(schema, _, _, _, _)  => getTypeId(schema)
      case Lazy(schema0)                  => getTypeId(schema0())
      case Optional(schema, _)            => TypeId.parse("scala.Option")
      case Sequence(_, _, _, _, identity) =>
        identity match {
          case s: String if s == "Chunk"         => TypeId.parse("zio.Chunk")
          case s: String if s == "List"          => TypeId.parse("scala.collection.immutable.List")
          case s: String if s == "Vector"        => TypeId.parse("scala.collection.immutable.Vector")
          case s: String if s == "Set"           => TypeId.parse("scala.collection.immutable.Set")
          case s: String if s == "NonEmptyChunk" => TypeId.parse("zio.NonEmptyChunk")
          case s: String                         => TypeId.parse(s)
          case _                                 => TypeId.parse("zio.Chunk")
        }
      case NonEmptySequence(_, _, _, _, identity) =>
        identity match {
          case s: String if s == "NonEmptyChunk" => TypeId.parse("zio.NonEmptyChunk")
          case s: String if s == "NonEmptySet"   => TypeId.parse("zio.prelude.NonEmptySet")
          case s: String                         => TypeId.parse(s)
          case _                                 => TypeId.parse("zio.NonEmptyChunk")
        }
      case _: Set[_]            => TypeId.parse("scala.collection.immutable.Set")
      case _: Map[_, _]         => TypeId.parse("scala.collection.immutable.Map")
      case _: NonEmptyMap[_, _] => TypeId.parse("zio.prelude.NonEmptyMap")
      case _: Tuple2[_, _]      => TypeId.parse("scala.Tuple2")
      case _: Either[_, _]      => TypeId.parse("scala.Either")
      case _: Fallback[_, _]    => TypeId.parse("zio.schema.Fallback")
      case _: Dynamic           => TypeId.parse("zio.schema.DynamicValue")
      case Fail(_, _)           => TypeId.Structural
    }

  /**
   * Returns a [[Schema.GenericRecord]] with the given type id and fields, whose values are `ListMap`s from field name to field value.
   *
   * {{{
   * import scala.collection.immutable.ListMap
   *
   * val user: Schema[ListMap[String, _]] = Schema.record(
   *   TypeId.parse("example.User"),
   *   Schema.Field[ListMap[String, _], String](
   *     "name",
   *     Schema[String],
   *     get0 = _("name").asInstanceOf[String],
   *     set0 = (m, v) => m.updated("name", v)
   *   )
   * )
   *
   * val dynamic = user.toDynamic(ListMap("name" -> "Bob"))
   * // Record(Nominal(Chunk(example),Chunk(),User),ListMap(name -> Primitive(Bob,string)))
   * }}}
   */
  def record(id: TypeId, fields: Field[ListMap[String, _], _]*): Schema[ListMap[String, _]] =
    GenericRecord(id, FieldSet(fields: _*))

  /**
   * Returns a [[Schema.GenericRecord]] with the given type id and field set, whose values are `ListMap`s from field name to field value.
   */
  def record(id: TypeId, fieldSet: FieldSet): Schema[ListMap[String, _]] =
    GenericRecord(id, fieldSet)

  /**
   * Returns a schema of `A` from a schema of `(Unit, A)`, dropping the `Unit` half.
   */
  def second[A](schema: Schema[(Unit, A)]): Schema[A] =
    schema.transform[A](_._2, a => ((), a))

  /**
   * Returns a schema with one value, `instance`. It is a `Schema[Unit]` transformed so that decoding always gives `instance`.
   */
  def singleton[A](instance: A): Schema[A] = Schema[Unit].transform(_ => instance, _ => ())

  /**
   * Validates `value` against the implicit schema and returns the errors found, or an empty chunk when there are none.
   *
   * It checks the validation of each case class field (from a `@validate` annotation or the `validation0` of a `Schema.Field`), reports a `ValidationError.MissingField` for a field without a value, and walks into nested records, enumerations, collections, options, tuples, `Either`, `Fallback` and transformations. A transformation whose reverse function fails gives a `ValidationError.Generic` with its message. A validation added to a schema with `Schema#validation` is not checked.
   *
   * {{{
   * import zio.schema.annotation.validate
   * import zio.schema.validation.Validation
   *
   * final case class Account(@validate(Validation.greaterThan(0)) id: Int)
   * object Account {
   *   implicit val schema: Schema[Account] = DeriveSchema.gen[Account]
   * }
   *
   * val errors = Schema.validate(Account(-1))
   * // Chunk(GreaterThan(-1,0))
   * }}}
   */
  def validate[A](value: A)(implicit schema: Schema[A]): Chunk[ValidationError] = {
    def loop[A](value: A, schema: Schema[A]): Chunk[ValidationError] =
      schema match {
        case Sequence(schema, _, toChunk, _, _) =>
          toChunk(value).flatMap(value => loop(value, schema))
        case nes @ NonEmptySequence(schema, _, _, _, _) =>
          nes.toChunk(value).flatMap(value => loop(value, schema))
        case Transform(schema, _, g, _, _) =>
          g(value) match {
            case Right(value) => loop(value, schema)
            case Left(error)  => Chunk(ValidationError.Generic(error))
          }
        case Primitive(_, _)                => Chunk.empty
        case optional @ Optional(schema, _) =>
          value.asInstanceOf[Option[optional.OptionalType]] match {
            case Some(value) => loop(value, schema)
            case None        => Chunk.empty
          }
        case tuple @ Schema.Tuple2(left, right, _) =>
          loop(tuple.extract1(value), left) ++ loop(tuple.extract2(value), right)
        case l @ Lazy(_) =>
          loop(value, l.schema)
        case Schema.NonEmptyMap(ks, vs, _) =>
          Chunk.fromIterable(value.toMap.keys).flatMap(loop(_, ks)) ++ Chunk
            .fromIterable(value.values)
            .flatMap(loop(_, vs))
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
              case (acc, (field, None)) =>
                Chunk(ValidationError.MissingField(field)) ++ acc
              case (acc, (field, Some(fieldValue))) =>
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
        case fallback @ Schema.Fallback(left, right, _, _) =>
          value.asInstanceOf[zio.schema.Fallback[fallback.LeftType, fallback.RightType]] match {
            case zio.schema.Fallback.Left(value)                 => loop(value, left)
            case zio.schema.Fallback.Right(value)                => loop(value, right)
            case zio.schema.Fallback.Both(valueLeft, valueRight) => loop(valueLeft, left) ++ loop(valueRight, right)
          }
        case Dynamic(_) => Chunk.empty
        case Fail(_, _) => Chunk.empty
      }
    loop(value, schema)
  }

  /**
   * The schema of Scala's `BigDecimal`, stored as the `java.math.BigDecimal` primitive.
   */
  implicit val bigDecimal: Schema[BigDecimal] = primitive[java.math.BigDecimal].transform(BigDecimal(_), _.bigDecimal)

  /**
   * The schema of Scala's `BigInt`, stored as the `java.math.BigInteger` primitive.
   */
  implicit val bigInt: Schema[BigInt] = primitive[java.math.BigInteger].transform(BigInt(_), _.bigInteger)

  /**
   * The schema of `java.time.temporal.ChronoUnit`, stored as the unit's upper-case name (`"DAYS"`). It covers `NANOS`, `MICROS`, `MILLIS`, `SECONDS`, `MINUTES`, `HOURS`, `DAYS`, `WEEKS`, `MONTHS`, `YEARS`, `DECADES`, `CENTURIES` and `FOREVER`. Other units (`HALF_DAYS`, `MILLENNIA`, `ERAS`) and other strings fail with the message `"Failed"`.
   */
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
    },
    {
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

  /**
   * The schema of [[DynamicValue]] itself, a [[Schema.Dynamic]]. Codecs use it to encode and decode dynamic values whose structure is not known in advance.
   */
  implicit val dynamicValue: Schema[DynamicValue] = Schema.Dynamic()

  /**
   * The schema of a `zio.Chunk` of `A`, a [[Schema.Sequence]] identified by `"Chunk"`.
   */
  implicit def chunk[A](implicit schemaA: Schema[A]): Schema[Chunk[A]] =
    Schema.Sequence[Chunk[A], A, String](schemaA, identity, identity, Chunk.empty, "Chunk")

  /**
   * The schema of a `zio.NonEmptyChunk` of `A`, a [[Schema.NonEmptySequence]]. Decoding an empty sequence fails, and `defaultValue` is a `Left`.
   */
  implicit def nonEmptyChunk[A](implicit schemaA: Schema[A]): Schema[NonEmptyChunk[A]] =
    Schema.NonEmptySequence[NonEmptyChunk[A], A, String](
      schemaA,
      NonEmptyChunk.fromChunk,
      _.toChunk,
      Chunk.empty,
      "NonEmptyChunk"
    )

  /**
   * The schema of a `zio.prelude.NonEmptySet` of `A`, a [[Schema.NonEmptySequence]]. Decoding an empty sequence fails, and `defaultValue` is a `Left`.
   */
  implicit def nonEmptySet[A](implicit schemaA: Schema[A]): Schema[NonEmptySet[A]] =
    Schema.NonEmptySequence[NonEmptySet[A], A, String](
      schemaA,
      chunk => NonEmptySet.fromSetOption(chunk.toSet),
      _.toNonEmptyChunk.toChunk,
      Chunk.empty,
      "NonEmptySet"
    )

  /**
   * The schema of a `zio.prelude.NonEmptyMap`, a [[Schema.NonEmptyMap]] built from the key and value schemas. Its `defaultValue` is a `Left`.
   */
  implicit def nonEmptyMap[K, V](implicit
    keySchema: Schema[K],
    valueSchema: Schema[V]
  ): Schema[prelude.NonEmptyMap[K, V]] =
    Schema.NonEmptyMap[K, V](keySchema, valueSchema, Chunk.empty)

  /**
   * The schema of an immutable `Map`, a [[Schema.Map]] built from the key and value schemas.
   */
  implicit def map[K, V](implicit
    keySchema: Schema[K],
    valueSchema: Schema[V]
  ): Schema[scala.collection.immutable.Map[K, V]] =
    Schema.Map(keySchema, valueSchema, Chunk.empty)

  /**
   * The schema of an immutable `Set` of `A`, a [[Schema.Set]].
   */
  implicit def set[A](implicit schemaA: Schema[A]): Schema[scala.collection.immutable.Set[A]] =
    Schema.Set(schemaA, Chunk.empty)

  /**
   * The schema of `scala.util.Either[A, B]`, a [[Schema.Either]] built from the left and right schemas.
   */
  implicit def either[A, B](implicit left: Schema[A], right: Schema[B]): Schema[scala.util.Either[A, B]] =
    Schema.Either(left, right)

  /**
   * The schema of `zio.schema.Fallback[A, B]`, a [[Schema.Fallback]] built from the left and right schemas, with `fullDecode` set to `false`.
   */
  implicit def fallback[A, B](implicit left: Schema[A], right: Schema[B]): Schema[zio.schema.Fallback[A, B]] =
    Schema.Fallback(left, right)

  /**
   * The schema of a `List` of `A`, a [[Schema.Sequence]] identified by `"List"`.
   */
  implicit def list[A](implicit schemaA: Schema[A]): Schema[List[A]] =
    Schema.Sequence[List[A], A, String](schemaA, _.toList, Chunk.fromIterable(_), Chunk.empty, "List")

  /**
   * The schema of an `Option` of `A`, a [[Schema.Optional]]. Its `defaultValue` is `Right(None)`.
   */
  implicit def option[A](implicit element: Schema[A]): Schema[Option[A]] =
    Optional(element)

  /**
   * The schema of a primitive type `A`, a [[Schema.Primitive]] built from the implicit [[StandardType]] of `A`.
   */
  implicit def primitive[A](implicit standardType: StandardType[A]): Schema[A] =
    Primitive(standardType, Chunk.empty)

  /**
   * Converts `a` to a [[DynamicValue]] with the implicit schema of `A`. `Schema.toDynamic(42)` gives `Primitive(42,int)`.
   */
  def toDynamic[A](a: A)(implicit schema: Schema[A]): DynamicValue = schema.toDynamic(a)

  /**
   * The schema of a `Vector` of `A`, a [[Schema.Sequence]] identified by `"Vector"`.
   */
  implicit def vector[A](implicit element: Schema[A]): Schema[Vector[A]] =
    Schema.Sequence[Vector[A], A, String](element, _.toVector, Chunk.fromIterable(_), Chunk.empty, "Vector")

  /**
   * The schema of a schema, stored as its [[zio.schema.meta.MetaSchema]]. Decoding gives back a schema with the same structure, without the original functions (constructors and transformations), so a decoded case class schema works with generic values rather than the case class.
   */
  implicit def schemaSchema[A]: Schema[Schema[A]] = Schema[MetaSchema].transform(
    _.toSchema.asInstanceOf[Schema[A]],
    _.ast
  )

  /**
   * The schema of `java.net.URI`, stored as its string form. Decoding a string that is not a valid URI fails with the message `"Invalid URI: "` followed by the input.
   */
  implicit val uri: Schema[java.net.URI] = {
    val f = (string: String) =>
      try {
        Right(new URI(string))
      } catch { case _: Exception => Left(s"Invalid URI: $string") }
    val g = (uri: java.net.URI) => Right(uri.toString)
    Schema[String].transformOrFail(f, g)
  }

  /**
   * The schema of a [[StandardType]], stored as its tag (`"int"`, `"string"`). Decoding a string that is not a known tag throws a `scala.MatchError` from `StandardType.fromString` instead of returning a `Left`.
   */
  implicit def standardSchema[A]: Schema[StandardType[A]] = Schema[String].transformOrFail[StandardType[A]](
    string =>
      StandardType
        .fromString(string)
        .asInstanceOf[Option[StandardType[A]]]
        .toRight(s"Invalid StandardType tag ${string}"),
    standardType => Right(standardType.tag)
  )

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum), made of one [[Schema.Case]] per subtype. `Enum1` to `Enum22` and `EnumN` implement it.
   */
  sealed trait Enum[Z] extends Schema[Z] {
    private lazy val casesMap: scala.collection.immutable.Map[String, Case[Z, _]] =
      cases.map(c => c.caseName -> c).toMap

    /**
     * The type id of the sum type.
     */
    def id: TypeId

    /**
     * The cases of this enumeration, in declaration order.
     */
    def cases: Chunk[Case[Z, _]]

    /**
     * Returns the case whose `caseName` is `id`, or `None`. The lookup uses `caseName`, so with `@caseName("circle")` on `Circle` the case is found by `"circle"` and not by `"Circle"`.
     */
    def caseOf(id: String): Option[Case[Z, _]] =
      casesMap.get(id)

    /**
     * Returns the first case whose `isCase` accepts `z`, or `None`.
     */
    def caseOf(z: Z): Option[Case[Z, _]] = cases.find(_.isCase(z))

    /**
     * The tag of the `@discriminatorName` annotation on this enumeration, if any. Codecs that support it write the case name in a field with this name instead of wrapping the value in an object keyed by the case name.
     */
    val discriminatorName: Option[String] =
      annotations.collectFirst { case d: discriminatorName => d.tag }

    /**
     * Whether this enumeration has the `@noDiscriminator` annotation, which tells codecs that support it to write the case value without any case name.
     */
    val noDiscriminator: Boolean =
      annotations.exists(_.isInstanceOf[noDiscriminator])

    /**
     * The cases that do not have the `@transientCase` annotation.
     */
    lazy val nonTransientCases: Chunk[Case[Z, _]] =
      cases.filterNot(_.transient)

    override def toString: String = s"${this.getClass.getSimpleName}($id)"
  }

  /**
   * A field of type `A` in a record of type `R`: its name, schema, annotations and validation, with a getter and a setter. Build one with `Schema.Field.apply`.
   */
  sealed trait Field[R, A] {
    /**
     * The singleton string type of the field's name.
     */
    type Field <: Singleton with String

    /**
     * The field's name as declared in the Scala type.
     */
    def name: Field
    /**
     * The schema of the field's value.
     */
    def schema: Schema[A]
    /**
     * The annotations on the field, such as `@fieldName`, `@fieldDefaultValue` or `@transientField`.
     */
    def annotations: Chunk[Any]
    /**
     * The validation of the field's value, which `Schema.validate` checks. It is `Validation.succeed` unless the field has a `@validate` annotation or a validation was passed to `Schema.Field.apply`.
     */
    def validation: Validation[A]
    /**
     * Reads the field's value from a record.
     */
    def get: R => A
    /**
     * Returns a copy of a record with the field set to a new value.
     */
    def set: (R, A) => R

    /**
     * The value of the field's `@fieldDefaultValue` annotation if it has one, else the `defaultValue` of its schema, or `None` when neither exists.
     */
    lazy val defaultValue: Option[A] =
      annotations.collectFirst {
        case a if a.isInstanceOf[fieldDefaultValue[_]] => a.asInstanceOf[fieldDefaultValue[A]].value
      }.orElse(schema.defaultValue.toOption)

    /**
     * Whether the field has an `@optionalField` or a `@fieldDefaultValue` annotation. Codecs that support it let such a field be missing from the input.
     */
    val optional: Boolean =
      annotations.exists(a => a.isInstanceOf[optionalField] || a.isInstanceOf[fieldDefaultValue[_]])

    /**
     * Whether the field has the `@transientField` annotation. Codecs that support it skip the field when encoding.
     */
    val transient: Boolean =
      annotations.exists(_.isInstanceOf[transientField])

    /**
     * The name codecs use for the field: the name in its `@fieldName` annotation if it has one, else `name`.
     */
    val fieldName: String = annotations.collectFirst { case f: fieldName =>
      f.name
    }.getOrElse(name)

    /**
     * The alternative names from the field's `@fieldNameAliases` annotations, which decoders that support them also accept.
     */
    val aliases: scala.collection.immutable.Set[String] =
      annotations.foldLeft(scala.collection.immutable.Set.empty[String]) { (acc, annotation) =>
        annotation match {
          case aliases: fieldNameAliases => acc ++ aliases.aliases
          case _                         => acc
        }
      }

    /**
     * `fieldName` together with `aliases`.
     */
    val nameAndAliases: scala.collection.immutable.Set[String] =
      aliases + fieldName

    override def toString: String = s"Field($name,$schema)"
  }

  /**
   * Constructors and an extractor for [[Schema.Field]].
   */
  object Field {

    /**
     * A [[Schema.Field]] whose name has the singleton type `F`. Derived schemas use it so that optics can refer to fields by name.
     */
    type WithFieldName[R, F <: Singleton with String, A] = Field[R, A] {
      type Field = F
    }

    /**
     * Returns a field of a record of type `R` with the given name, value schema, annotations, validation, getter and setter.
     */
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

    /**
     * Extracts the name, schema, annotations, validation, getter and setter of a field.
     */
    def unapply[R, A](field: Field[R, A]): Some[(String, Schema[A], Chunk[Any], Validation[A], R => A, (R, A) => R)] =
      Some[(String, Schema[A], Chunk[Any], Validation[A], R => A, (R, A) => R)](
        (field.name, field.schema, field.annotations, field.validation, field.get, field.set)
      )
  }

  /**
   * The schema of a product type `R` (a case class, a tuple seen as a record, or a `ListMap` for [[Schema.GenericRecord]]), made of [[Schema.Field]]s. `CaseClass0` to `CaseClass22` and `GenericRecord` implement it.
   */
  sealed trait Record[R] extends Schema[R] {
    self =>

    /**
     * The record's fields at the type level: an intersection of `(name, type)` pairs, such as `(Field1, A1) with (Field2, A2)` for a two-field case class.
     */
    type Terms
    /**
     * The record's field names at the type level: an intersection of their singleton string types, such as `Field1 with Field2`.
     */
    type FieldNames

    /**
     * The fields of the record, in declaration order.
     */
    val fields: Chunk[Field[R, _]]

    /**
     * The fields that do not have the `@transientField` annotation.
     */
    lazy val nonTransientFields: Chunk[Field[R, _]] =
      fields.filterNot(_.transient)

    /**
     * Whether the record has the `@rejectExtraFields` annotation, which tells decoders that support it to fail on input fields the record does not have.
     */
    val rejectExtraFields: Boolean =
      annotations.exists(_.isInstanceOf[rejectExtraFields])

    /**
     * Builds a record from its field values in declaration order, or returns a `Left` when the number of values is wrong. The values are not type-checked, hence the `Unsafe` parameter.
     */
    def construct(fieldValues: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, R]

    /**
     * Returns the field values of `value` in declaration order. A `GenericRecord` gives `None` for a field missing from its `ListMap`.
     */
    def deconstruct(value: R)(implicit unsafe: Unsafe): Chunk[Option[Any]]

    /**
     * The type id of the record.
     */
    def id: TypeId

    /**
     * Builds a record from the default of each field: the value of the field's `@fieldDefaultValue` annotation if it has one, else the `defaultValue` of the field's schema. Returns the first `Left` when a field has no default.
     */
    def defaultValue: scala.util.Either[String, R] =
      Unsafe.unsafe { implicit unsafe =>
        self.fields.map { field =>
          field.annotations.collectFirst { case fieldDefaultValue(value) => value } match {
            case Some(value) => Right(value)
            case None        => field.schema.defaultValue
          }
        }.foldLeft[scala.util.Either[String, Chunk[R]]](Right(Chunk.empty)) {
          case (e @ Left(_), _)              => e
          case (_, Left(e))                  => Left[String, Chunk[R]](e)
          case (Right(values), Right(value)) => Right[String, Chunk[R]](values :+ value.asInstanceOf[R])
        }.flatMap(self.construct)
      }
  }

  /**
   * The schema of a collection `Col` of `Elem` values that converts to and from a `Chunk`. [[Schema.Sequence]], [[Schema.NonEmptySequence]], [[Schema.Map]], [[Schema.NonEmptyMap]] and [[Schema.Set]] implement it.
   */
  sealed trait Collection[Col, Elem] extends Schema[Col] {
    /**
     * Builds the collection from a chunk of elements. The non-empty collections throw a `java.lang.IllegalArgumentException` on an empty chunk.
     */
    def fromChunk: Chunk[Elem] => Col
    /**
     * Returns the elements of the collection as a chunk.
     */
    def toChunk: Col => Chunk[Elem]
    /**
     * Returns the empty collection. The non-empty collections throw a `java.lang.IllegalArgumentException`.
     */
    def empty: Col
  }

  /**
   * The schema of a sequence collection `Col` of `Elem` values, such as `List`, `Vector` or `Chunk`. `identity` names the collection type (`"List"`, `"Vector"`, `"Chunk"` for the built-in schemas); `Schema.getTypeId` and `Schema.strictEquality` use it. The `defaultValue` is the empty collection.
   */
  final case class Sequence[Col, Elem, I](
    elementSchema: Schema[Elem],
    fromChunk: Chunk[Elem] => Col,
    toChunk: Col => Chunk[Elem],
    override val annotations: Chunk[Any] = Chunk.empty,
    identity: I
  ) extends Collection[Col, Elem] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Traversal[Col, Elem]

    override def annotate(annotation: Any): Sequence[Col, Elem, I] =
      copy(annotations = (annotations :+ annotation).distinct)

    override def defaultValue: scala.util.Either[String, Col] = Right(empty)

    override def makeAccessors(b: AccessorBuilder): b.Traversal[Col, Elem] = b.makeTraversal(self, elementSchema)

    override def toString: String = s"Sequence($elementSchema, $identity)"

    override def empty: Col = fromChunk(Chunk.empty[Elem])
  }

  /**
   * The schema of `B` built from a schema of `A` and two conversions that may fail, `f` from `A` to `B` and `g` back. `transform` and `transformOrFail` create it with the source location of the call as `identity`, which `Schema.strictEquality` compares.
   */
  final case class Transform[A, B, I](
    schema: Schema[A],
    f: A => scala.util.Either[String, B],
    g: B => scala.util.Either[String, A],
    annotations: Chunk[Any],
    identity: I
  ) extends Schema[B] {
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = schema.Accessors[Lens, Prism, Traversal]

    /**
     * Converts the default value of the underlying schema with `f`. A `fieldDefaultValue` annotation on this schema is not read.
     */
    def defaultValue: scala.util.Either[String, B] = schema.defaultValue.flatMap(f)

    override def makeAccessors(b: AccessorBuilder): schema.Accessors[b.Lens, b.Prism, b.Traversal] =
      schema.makeAccessors(b)

    override def annotate(annotation: Any): Transform[A, B, I] =
      copy(annotations = (annotations :+ annotation).distinct)

    override def serializable: Schema[Schema[B]] =
      MetaSchema
        .fromSchema(schema)
        .toSchema
        .asInstanceOf[Schema[Schema[_]]]
        .transformOrFail(
          s => s.coerce(schema).flatMap(s1 => Right(s1.transformOrFail(f, g))),
          s => Right(s.transformOrFail(g, f).ast.toSchema)
        )

    override def toString: String = s"Transform($schema, $identity)"

  }

  /**
   * The schema of a primitive type `A`, described by its [[StandardType]]. Its `defaultValue` is the value of a `fieldDefaultValue` annotation on the schema if there is one, else the default of the standard type.
   */
  final case class Primitive[A](standardType: StandardType[A], annotations: Chunk[Any] = Chunk.empty)
      extends Schema[A] {
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Unit

    override def annotate(annotation: Any): Primitive[A] = copy(annotations = (annotations :+ annotation).distinct)

    private def annotatedDefaultValue = annotations.collectFirst {
      case a if a.isInstanceOf[fieldDefaultValue[_]] => a.asInstanceOf[fieldDefaultValue[A]].value
    }

    override def defaultValue: scala.util.Either[String, A] =
      annotatedDefaultValue match {
        case Some(value) => Right(value)
        case None        => standardType.defaultValue
      }

    override def makeAccessors(b: AccessorBuilder): Unit = ()
  }

  /**
   * The schema of `Option[A]`, built from the schema of `A`. Its `defaultValue` is `Right(None)`.
   */
  final case class Optional[A](schema: Schema[A], annotations: Chunk[Any] = Chunk.empty) extends Schema[Option[A]] {
    self =>
    /**
     * The type of the value inside the `Option`.
     */
    type OptionalType = A

    /**
     * The name of the `Some` case, `"Some"`.
     */
    val some = "Some"
    /**
     * The name of the `None` case, `"None"`.
     */
    val none = "None"

    private[schema] lazy val someCodec: Schema[Some[A]] =
      schema.transform(a => Some(a), _.get)

    override def annotate(annotation: Any): Optional[A] = copy(annotations = (annotations :+ annotation).distinct)

    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Prism[some.type, Option[A], Some[A]], Prism[none.type, Option[A], None.type])

    /**
     * This schema seen as an enumeration with the cases `Some` and `None`, which `makeAccessors` uses to build the two prisms.
     */
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

    /**
     * Returns `Right(None)`.
     */
    def defaultValue: scala.util.Either[String, Option[A]] = Right(None)

    override def makeAccessors(
      b: AccessorBuilder
    ): (b.Prism[some.type, Option[A], Some[A]], b.Prism[none.type, Option[A], None.type]) =
      b.makePrism(toEnum, toEnum.case1) -> b.makePrism(toEnum, toEnum.case2)

  }

  /**
   * A schema that stands for the absence of schema information for `A`. Its `defaultValue` is `Left(message)`. `Schema.fail` creates it.
   */
  final case class Fail[A](message: String, annotations: Chunk[Any] = Chunk.empty) extends Schema[A] {
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Unit

    override def annotate(annotation: Any): Fail[A] = copy(annotations = (annotations :+ annotation).distinct)

    override def defaultValue: scala.util.Either[String, A] = Left(message)

    override def makeAccessors(b: AccessorBuilder): Unit = ()
  }

  /**
   * The schema of the pair `(A, B)`, built from the schemas of its two elements. `zip` and the implicit tuple schemas create it; larger tuples are nested pairs with a `transform`.
   */
  final case class Tuple2[A, B](left: Schema[A], right: Schema[B], annotations: Chunk[Any] = Chunk.empty)
      extends Schema[(A, B)] {
    self =>

    /**
     * The name of the first element, `"_1"`.
     */
    val first  = "_1"
    /**
     * The name of the second element, `"_2"`.
     */
    val second = "_2"
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Lens[first.type, (A, B), A], Lens[second.type, (A, B), B])

    override def annotate(annotation: Any): Tuple2[A, B] = copy(annotations = (annotations :+ annotation).distinct)

    /**
     * This schema seen as a two-field record with the fields `_1` and `_2`, which `makeAccessors` uses to build the two lenses.
     */
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

    /**
     * Returns the first element of `value`.
     */
    def extract1(value: (A, B)): A = value._1
    /**
     * Returns the second element of `value`.
     */
    def extract2(value: (A, B)): B = value._2
  }

  /**
   * The schema of `scala.util.Either[A, B]`, built from the left and right schemas. Its `defaultValue` is a `Left` of the left schema's default, else a `Right` of the right schema's default.
   */
  final case class Either[A, B](left: Schema[A], right: Schema[B], annotations: Chunk[Any] = Chunk.empty)
      extends Schema[scala.util.Either[A, B]] {
    self =>
    /**
     * The type of the left value.
     */
    type LeftType  = A
    /**
     * The type of the right value.
     */
    type RightType = B

    /**
     * The name of the left case, `"Left"`.
     */
    val leftSingleton  = "Left"
    /**
     * The name of the right case, `"Right"`.
     */
    val rightSingleton = "Right"
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (
        Prism[rightSingleton.type, scala.util.Either[A, B], Right[Nothing, B]],
        Prism[leftSingleton.type, scala.util.Either[A, B], Left[A, Nothing]]
      )

    override def annotate(annotation: Any): Schema.Either[A, B] =
      copy(annotations = (annotations :+ annotation).distinct)

    /**
     * The schema of the `Right` case, the right schema transformed to wrap its value in `Right`.
     */
    val rightSchema: Schema[Right[Nothing, B]] = right.transform(b => Right(b), _.value)
    /**
     * The schema of the `Left` case, the left schema transformed to wrap its value in `Left`.
     */
    val leftSchema: Schema[Left[A, Nothing]]   = left.transform(a => Left(a), _.value)

    /**
     * This schema seen as a `scala.util.Either` enumeration with the cases `Right` and `Left` (in that order), which `makeAccessors` uses to build the two prisms.
     */
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
        case _        =>
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

  /**
   * Schema for `zio.schema.Fallback` type. If `fullDecode` is set to `true`, it
   * will decode `Fallback.Both` from `Fallback.Both`. If set to `false`, it
   * will decode `Fallback.Left` when possible and `Fallback.Right` as second
   * option from a `Fallback.Both`.
   */
  final case class Fallback[A, B](
    left: Schema[A],
    right: Schema[B],
    fullDecode: Boolean = false,
    annotations: Chunk[Any] = Chunk.empty
  ) extends Schema[zio.schema.Fallback[A, B]] {
    self =>
    /**
     * The type of the left value.
     */
    type LeftType  = A
    /**
     * The type of the right value.
     */
    type RightType = B

    /**
     * The name of the left case, `"Left"`.
     */
    val leftSingleton  = "Left"
    /**
     * The name of the right case, `"Right"`.
     */
    val rightSingleton = "Right"
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (
        Prism[rightSingleton.type, scala.util.Either[A, B], Right[Nothing, B]],
        Prism[leftSingleton.type, scala.util.Either[A, B], Left[A, Nothing]]
      )

    override def annotate(annotation: Any): Schema.Fallback[A, B] =
      copy(annotations = (annotations :+ annotation).distinct)

    /**
     * The schema of the `Right` case, the right schema transformed to wrap its value in `Right`.
     */
    val rightSchema: Schema[Right[Nothing, B]] = right.transform(b => Right(b), _.value)
    /**
     * The schema of the `Left` case, the left schema transformed to wrap its value in `Left`.
     */
    val leftSchema: Schema[Left[A, Nothing]]   = left.transform(a => Left(a), _.value)

    /**
     * This schema seen as a `scala.util.Either` enumeration with the cases `Right` and `Left` (in that order), which `makeAccessors` uses to build the two prisms.
     */
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

    /**
     * Returns a [[Schema.Either]] with the same left and right schemas and annotations.
     */
    val toEither: Either[A, B] = Either[A, B](left, right, annotations)

    override def defaultValue: scala.util.Either[String, zio.schema.Fallback[A, B]] =
      (left.defaultValue, right.defaultValue) match {
        case (Right(a), Right(b)) => Right(zio.schema.Fallback.Both(a, b))
        case (Right(a), _)        => Right(zio.schema.Fallback.Left(a))
        case (_, Right(b))        => Right(zio.schema.Fallback.Right(b))
        case _                    => Left("unable to extract default value for Fallback")
      }

    override def makeAccessors(
      b: AccessorBuilder
    ): (
      b.Prism[rightSingleton.type, scala.util.Either[A, B], Right[Nothing, B]],
      b.Prism[leftSingleton.type, scala.util.Either[A, B], Left[A, Nothing]]
    ) =
      b.makePrism(toEnum, toEnum.case1) -> b.makePrism(toEnum, toEnum.case2)

  }

  /**
   * A schema that is computed the first time it is needed, used for recursive data types. `Schema.defer` creates it. Its `annotations`, `defaultValue` and optics come from the underlying schema, and `annotate` annotates the underlying schema.
   */
  final case class Lazy[A](private val schema0: () => Schema[A]) extends Schema[A] {
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = schema.Accessors[Lens, Prism, Traversal]

    override def annotate(annotation: Any): Lazy[A] = Lazy(() => schema0().annotate(annotation))

    /**
     * The underlying schema, computed on first access.
     */
    lazy val schema: Schema[A] = schema0()

    /**
     * Returns the default value of the underlying schema.
     */
    def defaultValue: scala.util.Either[String, A] = schema.defaultValue

    override def makeAccessors(b: AccessorBuilder): schema.Accessors[b.Lens, b.Prism, b.Traversal] =
      schema.makeAccessors(b)

    override def toString: String = "$Lazy$"

    override def annotations: Chunk[Any] = schema0().annotations
  }

  /**
   * The schema of an immutable `Map[K, V]`, built from the key and value schemas. It is a collection of `(K, V)` pairs, and its `defaultValue` is the empty map.
   */
  final case class Map[K, V](
    keySchema: Schema[K],
    valueSchema: Schema[V],
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Collection[scala.collection.immutable.Map[K, V], (K, V)] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      Traversal[scala.collection.immutable.Map[K, V], (K, V)]

    override def annotate(annotation: Any): Map[K, V] = copy(annotations = (annotations :+ annotation).distinct)

    override def defaultValue: scala.util.Either[String, scala.collection.immutable.Map[K, V]] =
      Right(empty)

    override val fromChunk: Chunk[(K, V)] => scala.collection.immutable.Map[K, V] = _.toMap

    override def makeAccessors(b: AccessorBuilder): b.Traversal[scala.collection.immutable.Map[K, V], (K, V)] =
      b.makeTraversal(self, keySchema <*> valueSchema)

    override val toChunk: scala.collection.immutable.Map[K, V] => Chunk[(K, V)] = map => Chunk.fromIterable(map.toList)

    override def empty: scala.collection.immutable.Map[K, V] = scala.collection.immutable.Map.empty[K, V]
  }

  /**
   * The schema of a `zio.prelude.NonEmptyMap[K, V]`, built from the key and value schemas. Its `defaultValue` is `Left("NonEmptyMap has no default value")`, and `fromChunk` and `empty` throw a `java.lang.IllegalArgumentException` for an empty map.
   */
  final case class NonEmptyMap[K, V](
    keySchema: Schema[K],
    valueSchema: Schema[V],
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Collection[prelude.NonEmptyMap[K, V], (K, V)] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      Traversal[prelude.NonEmptyMap[K, V], (K, V)]

    override def annotate(annotation: Any): NonEmptyMap[K, V] =
      copy(annotations = (annotations :+ annotation).distinct)

    override def defaultValue: scala.Either[String, prelude.NonEmptyMap[K, V]] =
      Left("NonEmptyMap has no default value")

    override def fromChunk: Chunk[(K, V)] => prelude.NonEmptyMap[K, V] =
      chunk => fromChunkOption(chunk).getOrElse(throw new IllegalArgumentException("NonEmptyMap cannot be empty"))

    /**
     * Builds a non-empty map from the pairs in `chunk`, or returns `None` when `chunk` is empty.
     */
    def fromChunkOption(chunk: Chunk[(K, V)]): Option[prelude.NonEmptyMap[K, V]] =
      NonEmptyChunk.fromChunk(chunk).map(prelude.NonEmptyMap.fromNonEmptyChunk)

    /**
     * Converts `map` to a non-empty map. Throws a `java.lang.IllegalArgumentException` with the message `"NonEmptyMap cannot be empty"` when `map` is empty.
     */
    def fromMap(map: scala.collection.immutable.Map[K, V]): prelude.NonEmptyMap[K, V] =
      prelude.NonEmptyMap
        .fromMapOption(map)
        .getOrElse(throw new IllegalArgumentException("NonEmptyMap cannot be empty"))

    override def toChunk: prelude.NonEmptyMap[K, V] => Chunk[(K, V)] = map => Chunk.fromIterable(map.toList)

    override def makeAccessors(b: AccessorBuilder): b.Traversal[prelude.NonEmptyMap[K, V], (K, V)] =
      b.makeTraversal(self, keySchema <*> valueSchema)

    override def empty: prelude.NonEmptyMap[K, V] =
      throw new IllegalArgumentException("NonEmptyMap cannot be empty")
  }

  /**
   * The schema of a non-empty sequence collection `Col` of `Elm` values, such as `NonEmptyChunk` or `NonEmptySet`. `fromChunkOption` returns `None` for an empty chunk, and `identity` names the collection type. Its `defaultValue` is a `Left`, and `fromChunk` and `empty` throw a `java.lang.IllegalArgumentException` for an empty collection.
   */
  final case class NonEmptySequence[Col, Elm, I](
    elementSchema: Schema[Elm],
    fromChunkOption: Chunk[Elm] => Option[Col],
    override val toChunk: Col => Chunk[Elm],
    override val annotations: Chunk[Any] = Chunk.empty,
    identity: I
  ) extends Collection[Col, Elm] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Traversal[Col, Elm]

    override val fromChunk: Chunk[Elm] => Col = (chunk: Chunk[Elm]) =>
      fromChunkOption(chunk).getOrElse(
        throw new IllegalArgumentException(s"NonEmptySequence $identity cannot be empty")
      )

    override def annotate(annotation: Any): NonEmptySequence[Col, Elm, I] =
      copy(annotations = (annotations :+ annotation).distinct)

    override def defaultValue: scala.util.Either[String, Col] =
      Left(s"$identity cannot be empty")

    override def makeAccessors(b: AccessorBuilder): b.Traversal[Col, Elm] = b.makeTraversal(self, elementSchema)

    override def toString: String = s"NonEmptySequence($elementSchema, $identity)"

    override def empty: Col = throw new IllegalArgumentException(s"NonEmptySequence $identity cannot be empty")
  }

  /**
   * The schema of an immutable `Set[A]`, built from the element schema. Its `defaultValue` is the empty set.
   */
  final case class Set[A](elementSchema: Schema[A], override val annotations: Chunk[Any] = Chunk.empty)
      extends Collection[scala.collection.immutable.Set[A], A] {
    self =>
    /**
     * The type of the set's elements.
     */
    type ElementType = A

    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      Traversal[scala.collection.immutable.Set[A], A]

    override def annotate(annotation: Any): Set[A] =
      copy(annotations = (annotations :+ annotation).distinct)

    override def defaultValue: scala.util.Either[String, scala.collection.immutable.Set[A]] =
      Right(empty)

    override val fromChunk: Chunk[A] => scala.collection.immutable.Set[A] = _.toSet

    override def makeAccessors(b: AccessorBuilder): b.Traversal[scala.collection.immutable.Set[A], A] =
      b.makeTraversal(self, elementSchema)

    override val toChunk: scala.collection.immutable.Set[A] => Chunk[A] = Chunk.fromIterable(_)

    override def empty: scala.collection.immutable.Set[A] = scala.collection.immutable.Set.empty[A]
  }

  /**
   * The schema of [[DynamicValue]], for data whose structure is only known at run time.
   */
  final case class Dynamic(override val annotations: Chunk[Any] = Chunk.empty) extends Schema[DynamicValue] {
    /**
     * The type id of `zio.schema.DynamicValue`.
     */
    def id: TypeId = DynamicValue.typeId

    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Unit

    /**
     * Returns `Right(DynamicValue.NoneValue)`.
     */
    override def defaultValue: scala.util.Either[String, DynamicValue] =
      Right(DynamicValue.NoneValue)

    /**
     * Returns a copy of this schema with `annotation` added to its annotations, unless it is already there.
     */
    override def annotate(annotation: Any): Schema[DynamicValue] =
      this.copy(annotations = (annotations :+ annotation).distinct)

    override def makeAccessors(b: AccessorBuilder): Unit = ()
  }

// # ENUM SCHEMAS

  /**
   * One case of an enumeration of type `R`, whose values of type `A` are described by `schema`. `id` is the case name in the Scala type, `construct` turns a case value into an `R`, and `isCase` tells whether an `R` belongs to this case.
   */
  sealed case class Case[R, A](
    id: String,
    schema: Schema[A],
    private val unsafeDeconstruct: R => A,
    construct: A => R,
    isCase: R => Boolean,
    annotations: Chunk[Any] = Chunk.empty
  ) { self =>

    /**
     * The name codecs use for the case: the name in its `@caseName` annotation if it has one, else `id`.
     */
    val caseName: String =
      annotations.collectFirst { case name: caseName => name.name }.getOrElse(id)

    /**
     * The alternative names from the case's `@caseNameAliases` annotations, which decoders that support them also accept.
     */
    val caseNameAliases: Seq[String] =
      annotations.collect { case aliases: caseNameAliases => aliases.aliases }.flatten

    /**
     * Casts `r` to this case's type `A` without checking that it belongs to this case; use `deconstructOption` when it may not.
     */
    def deconstruct(r: R): A = unsafeDeconstruct(r)

    /**
     * Returns `r` as this case's type `A` when `isCase(r)` is true, else `None`.
     */
    def deconstructOption(r: R): Option[A] =
      if (isCase(r)) Some(unsafeDeconstruct(r)) else None

    /**
     * Whether the case has the `@transientCase` annotation.
     */
    val transient: Boolean =
      annotations.exists(_.isInstanceOf[transientCase])

    override def toString: String = s"Case($id,$schema,$annotations)"
  }

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 1 case, `case1`, whose value type is `A`. Its `defaultValue` is the default value of `case1`.
   */
  sealed case class Enum1[A, Z](id: TypeId, case1: Case[Z, A], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Prism[case1.id.type, Z, A]

    override def annotate(annotation: Any): Enum1[A, Z] = copy(annotations = (annotations :+ annotation).distinct)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): b.Prism[case1.id.type, Z, A] = b.makePrism(self, case1)

    override def cases: Chunk[Case[Z, A]] =
      Chunk(case1)
  }

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 2 cases, `case1` to `case2` in order, whose value types are `A1` to `A2`. Its `defaultValue` is the default value of `case1`.
   */
  sealed case class Enum2[A1, A2, Z](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Prism[case1.id.type, Z, A1], Prism[case2.id.type, Z, A2])

    override def annotate(annotation: Any): Enum2[A1, A2, Z] = copy(annotations = (annotations :+ annotation).distinct)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(b: AccessorBuilder): (b.Prism[case1.id.type, Z, A1], b.Prism[case2.id.type, Z, A2]) =
      (b.makePrism(self, case1), b.makePrism(self, case2))

    override def cases: Chunk[Case[Z, _]] = Chunk(case1, case2)
  }

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 3 cases, `case1` to `case3` in order, whose value types are `A1` to `A3`. Its `defaultValue` is the default value of `case1`.
   */
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

    override def annotate(annotation: Any): Enum3[A1, A2, A3, Z] =
      copy(annotations = (annotations :+ annotation).distinct)

    override def defaultValue: scala.util.Either[String, Z] = case1.schema.defaultValue.map(case1.construct)

    override def makeAccessors(
      b: AccessorBuilder
    ): (b.Prism[case1.id.type, Z, A1], b.Prism[case2.id.type, Z, A2], b.Prism[case3.id.type, Z, A3]) =
      (b.makePrism(self, case1), b.makePrism(self, case2), b.makePrism(self, case3))

    override def cases: Chunk[Case[Z, _]] = Chunk(case1, case2, case3)
  }

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 4 cases, `case1` to `case4` in order, whose value types are `A1` to `A4`. Its `defaultValue` is the default value of `case1`.
   */
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

    override def annotate(annotation: Any): Enum4[A1, A2, A3, A4, Z] =
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 5 cases, `case1` to `case5` in order, whose value types are `A1` to `A5`. Its `defaultValue` is the default value of `case1`.
   */
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

    override def annotate(annotation: Any): Enum5[A1, A2, A3, A4, A5, Z] =
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 6 cases, `case1` to `case6` in order, whose value types are `A1` to `A6`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 7 cases, `case1` to `case7` in order, whose value types are `A1` to `A7`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 8 cases, `case1` to `case8` in order, whose value types are `A1` to `A8`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 9 cases, `case1` to `case9` in order, whose value types are `A1` to `A9`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 10 cases, `case1` to `case10` in order, whose value types are `A1` to `A10`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 11 cases, `case1` to `case11` in order, whose value types are `A1` to `A11`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 12 cases, `case1` to `case12` in order, whose value types are `A1` to `A12`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 13 cases, `case1` to `case13` in order, whose value types are `A1` to `A13`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 14 cases, `case1` to `case14` in order, whose value types are `A1` to `A14`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 15 cases, `case1` to `case15` in order, whose value types are `A1` to `A15`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 16 cases, `case1` to `case16` in order, whose value types are `A1` to `A16`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 17 cases, `case1` to `case17` in order, whose value types are `A1` to `A17`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 18 cases, `case1` to `case18` in order, whose value types are `A1` to `A18`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 19 cases, `case1` to `case19` in order, whose value types are `A1` to `A19`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 20 cases, `case1` to `case20` in order, whose value types are `A1` to `A20`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 21 cases, `case1` to `case21` in order, whose value types are `A1` to `A21`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type `Z` (a sealed trait or a Scala 3 enum) with 22 cases, `case1` to `case22` in order, whose value types are `A1` to `A22`. Its `defaultValue` is the default value of `case1`.
   */
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
      copy(annotations = (annotations :+ annotation).distinct)

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

  /**
   * The schema of a sum type with any number of cases, held in a [[CaseSet]]. Derivation uses it for sum types with more than 22 cases, and `Schema.enumeration` creates it.
   */
  sealed case class EnumN[Z, C <: CaseSet.Aux[Z]](id: TypeId, caseSet: C, annotations: Chunk[Any] = Chunk.empty)
      extends Enum[Z] {
    self =>
    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      caseSet.Accessors[Z, Lens, Prism, Traversal]

    override def annotate(annotation: Any): EnumN[Z, C] = copy(annotations = (annotations :+ annotation).distinct)

    override def cases: Chunk[Case[Z, _]] = Chunk(caseSet.toSeq: _*)

    /**
     * Returns the default value of the first case's schema, or a `Left` when there are no cases.
     */
    def defaultValue: scala.util.Either[String, Z] =
      if (caseSet.toSeq.isEmpty)
        Left("cannot access default value for enum with no members")
      else
        caseSet.toSeq.head.schema.defaultValue.asInstanceOf[scala.util.Either[String, Z]]

    override def makeAccessors(b: AccessorBuilder): caseSet.Accessors[Z, b.Lens, b.Prism, b.Traversal] =
      caseSet.makeAccessors(self, b)
  }

  /**
   * The schema of a pair, a [[Schema.Tuple2]] of the two element schemas.
   */
  implicit def tuple2[A, B](implicit c1: Schema[A], c2: Schema[B]): Schema[(A, B)] =
    c1.zip(c2)

  /**
   * The schema of a 3-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple3[A, B, C](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C]): Schema[(A, B, C)] =
    c1.zip(c2).zip(c3).transform({ case ((a, b), c) => (a, b, c) }, { case (a, b, c) => ((a, b), c) })

  /**
   * The schema of a 4-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple4[A, B, C, D](implicit
    c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D]
  ): Schema[(A, B, C, D)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .transform({ case (((a, b), c), d) => (a, b, c, d) }, { case (a, b, c, d) => (((a, b), c), d) })

  /**
   * The schema of a 5-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple5[A, B, C, D, E](implicit
    c1: Schema[A],
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

  /**
   * The schema of a 6-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple6[A, B, C, D, E, F](implicit
    c1: Schema[A],
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
      .transform(
        { case (((((a, b), c), d), e), f) => (a, b, c, d, e, f) },
        { case (a, b, c, d, e, f) =>
          (((((a, b), c), d), e), f)
        }
      )

  /**
   * The schema of a 7-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple7[A, B, C, D, E, F, G](implicit
    c1: Schema[A],
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
      .transform(
        { case ((((((a, b), c), d), e), f), g) => (a, b, c, d, e, f, g) },
        { case (a, b, c, d, e, f, g) =>
          ((((((a, b), c), d), e), f), g)
        }
      )

  /**
   * The schema of a 8-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple8[A, B, C, D, E, F, G, H](implicit
    c1: Schema[A],
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
      .transform(
        { case (((((((a, b), c), d), e), f), g), h) => (a, b, c, d, e, f, g, h) },
        { case (a, b, c, d, e, f, g, h) =>
          (((((((a, b), c), d), e), f), g), h)
        }
      )

  /**
   * The schema of a 9-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple9[A, B, C, D, E, F, G, H, I](implicit
    c1: Schema[A],
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
      .transform(
        { case ((((((((a, b), c), d), e), f), g), h), i) => (a, b, c, d, e, f, g, h, i) },
        { case (a, b, c, d, e, f, g, h, i) =>
          ((((((((a, b), c), d), e), f), g), h), i)
        }
      )

  /**
   * The schema of a 10-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple10[A, B, C, D, E, F, G, H, I, J](implicit
    c1: Schema[A],
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
      .transform(
        { case (((((((((a, b), c), d), e), f), g), h), i), j) => (a, b, c, d, e, f, g, h, i, j) },
        { case (a, b, c, d, e, f, g, h, i, j) =>
          (((((((((a, b), c), d), e), f), g), h), i), j)
        }
      )

  /**
   * The schema of a 11-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple11[A, B, C, D, E, F, G, H, I, J, K](implicit
    c1: Schema[A],
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
      .transform(
        { case ((((((((((a, b), c), d), e), f), g), h), i), j), k) => (a, b, c, d, e, f, g, h, i, j, k) },
        { case (a, b, c, d, e, f, g, h, i, j, k) =>
          ((((((((((a, b), c), d), e), f), g), h), i), j), k)
        }
      )

  /**
   * The schema of a 12-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple12[A, B, C, D, E, F, G, H, I, J, K, L](implicit
    c1: Schema[A],
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
        { case (((((((((((a, b), c), d), e), f), g), h), i), j), k), l) => (a, b, c, d, e, f, g, h, i, j, k, l) },
        { case (a, b, c, d, e, f, g, h, i, j, k, l) =>
          (((((((((((a, b), c), d), e), f), g), h), i), j), k), l)
        }
      )

  /**
   * The schema of a 13-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple13[A, B, C, D, E, F, G, H, I, J, K, L, M](implicit
    c1: Schema[A],
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
        { case ((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m)
        },
        { case (a, b, c, d, e, f, g, h, i, j, k, l, m) =>
          ((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m)
        }
      )

  /**
   * The schema of a 14-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple14[A, B, C, D, E, F, G, H, I, J, K, L, M, N](implicit
    c1: Schema[A],
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
        { case (((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
        },
        { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n) =>
          (((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n)
        }
      )

  /**
   * The schema of a 15-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple15[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O](implicit
    c1: Schema[A],
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
        { case ((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
        },
        { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) =>
          ((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o)
        }
      )

  /**
   * The schema of a 16-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple16[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P](implicit
    c1: Schema[A],
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
        { case (((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)
        },
        { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =>
          (((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p)
        }
      )

  /**
   * The schema of a 17-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple17[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q](implicit
    c1: Schema[A],
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
        { case ((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)
        },
        { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) =>
          ((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q)
        }
      )

  /**
   * The schema of a 18-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple18[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R](implicit
    c1: Schema[A],
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
        { case (((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)
        },
        { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) =>
          (((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r)
        }
      )

  /**
   * The schema of a 19-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple19[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S](implicit
    c1: Schema[A],
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
        { case ((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)
        },
        { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) =>
          ((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s)
        }
      )

  /**
   * The schema of a 20-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple20[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T](implicit
    c1: Schema[A],
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
        { case (((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)
        },
        { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) =>
          (((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t)
        }
      )

  /**
   * The schema of a 21-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple21[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U](implicit
    c1: Schema[A],
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
        { case ((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u) =>
          (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)
        },
        { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) =>
          ((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u)
        }
      )

  /**
   * The schema of a 22-element tuple, built by zipping the element schemas into nested [[Schema.Tuple2]]s and transforming the result into a flat tuple.
   */
  implicit def tuple22[A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V](implicit
    c1: Schema[A],
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
        },
        { case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) =>
          (((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u), v)
        }
      )
// # RECORD SCHEMAS

  /**
   * A record schema whose values are `ListMap`s from field name to field value, for records that have no Scala case class. `Schema.record` creates it, and a schema decoded from its [[zio.schema.meta.MetaSchema]] uses it for case classes.
   */
  sealed case class GenericRecord(
    id: TypeId,
    fieldSet: FieldSet,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[ListMap[String, _]] { self =>

    /**
     * The record's fields at the type level, taken from the field set.
     */
    type Terms      = fieldSet.Terms
    /**
     * The record's field names at the type level, taken from the field set.
     */
    type FieldNames = fieldSet.FieldNames

    /**
     * One lens per field, as given by the field set.
     */
    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      fieldSet.Accessors[ListMap[String, _], Lens, Prism, Traversal]

    override def makeAccessors(b: AccessorBuilder): Accessors[b.Lens, b.Prism, b.Traversal] =
      fieldSet.makeAccessors(self, b)

    override val fields: Chunk[Schema.Field[ListMap[String, _], _]] = fieldSet.toChunk

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, ListMap[String, _]] =
      if (values.size == fields.size)
        Right(ListMap(fields.map(_.name).zip(values): _*))
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(values: ListMap[String, _])(implicit unsafe: Unsafe): Chunk[Option[Any]] =
      Chunk.fromIterable(fields.map(f => values.get(f.name)))

    /**
     * Returns a copy of this schema with `annotation` added to its annotations, unless it is already there.
     */
    override def annotate(annotation: Any): GenericRecord = copy(annotations = (annotations :+ annotation).distinct)
  }

  /**
   * The schema of a case object or a case class with no fields. Decoding calls `defaultConstruct` to create the value.
   */
  sealed trait CaseClass0[Z] extends Record[Z] { self =>

    override type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Nothing
    override type Terms                                                     = Any
    override type FieldNames                                                = Any

    /**
     * The type id of the case class or case object.
     */
    def id: TypeId
    /**
     * Creates the value, for example by returning the case object.
     */
    def defaultConstruct: () => Z
    /**
     * The annotations on the case class or case object.
     */
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): Nothing = ???

    override val fields: Chunk[Field[Z, _]] = Chunk.empty

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.isEmpty)
        try {
          Right(defaultConstruct())
        } catch {
          case _: Throwable => Left("invalid type in values")
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk()

    override def toString: String = s"CaseClass0($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass0]].
   */
  object CaseClass0 {

    /**
     * Returns a [[Schema.CaseClass0]] with the given type id, value constructor and annotations.
     */
    def apply[Z](
      id0: TypeId,
      defaultConstruct0: () => Z,
      annotations0: Chunk[Any] = Chunk.empty
    ): CaseClass0[Z] = new CaseClass0[Z] {
      override def id: TypeId                = id0
      override def defaultConstruct: () => Z = defaultConstruct0
      override def annotations: Chunk[Any]   = annotations0

      override def annotate(annotation: Any): CaseClass0[Z] =
        CaseClass0(id, defaultConstruct0, ((annotations :+ annotation).distinct).distinct)
    }

    /**
     * Extracts the type id, value constructor and annotations of a [[Schema.CaseClass0]].
     */
    def unapply[Z](schema: CaseClass0[Z]): Some[(TypeId, () => Z, Chunk[Any])] =
      Some((schema.id, schema.defaultConstruct, schema.annotations))
  }

  /**
   * The schema of a case class `Z` with 1 field, whose type is `A`. `CaseClass1.apply` builds it from the type id, the field schema and a constructor function from `A` to `Z`.
   */
  sealed trait CaseClass1[A, Z] extends Record[Z] { self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String

    /**
     * The optics of this schema: a lens from `Z` to its field of type `A`.
     */
    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Lens[Field1, Z, A]

    override type Terms      = (Field1, A)
    override type FieldNames = Field1

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of the field, whose value type is `A`.
     */
    def field: Field.WithFieldName[Z, Field1, A]
    /**
     * Creates the case class from its field value.
     */
    def defaultConstruct: A => Z
    /**
     * The annotations on the case class.
     */
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): b.Lens[Field1, Z, A] = b.makeLens(self, field)

    override val fields: Chunk[Field[Z, _]] = Chunk(field)

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 1)
        try {
          Right(defaultConstruct(values(0).asInstanceOf[A]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(Some(field.get(value)))
    override def toString: String                                                   = s"CaseClass1(${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass1]].
   */
  object CaseClass1 {

    /**
     * Returns a [[Schema.CaseClass1]] with the given type id, field schema, constructor function and annotations.
     */
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
          CaseClass1(id0, field0, defaultConstruct0, ((annotations0 :+ annotation).distinct).distinct)
      }

    /**
     * Extracts the type id, field schema, constructor function and annotations of a [[Schema.CaseClass1]].
     */
    def unapply[A, Z](
      schema: CaseClass1[A, Z]
    ): Option[(TypeId, Field.WithFieldName[Z, schema.Field1, A], A => Z, Chunk[Any])] =
      Some((schema.id, schema.field, schema.defaultConstruct, schema.annotations))

    /**
     * A [[Schema.CaseClass1]] whose field name type `Field1` is fixed to `F`.
     */
    type WithFields[F <: Singleton with String, A, Z] =
      CaseClass1[A, Z] {
        type Field1 = F
      }
  }

  /**
   * The schema of a case class `Z` with 2 fields, whose types are `A1` to `A2` in field order. `CaseClass2.apply` builds it from the type id, the 2 field schemas and a constructor function that takes the 2 field values and returns `Z`.
   */
  sealed trait CaseClass2[A1, A2, Z] extends Record[Z] { self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 2 lenses, one per field in order, each from `Z` to the field type.
     */
    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Lens[Field1, Z, A1], Lens[Field2, Z, A2])

    override type Terms = (Field1, A1) with (Field2, A2)

    override type FieldNames = Field1 with Field2

    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * Creates the case class from its 2 field values, in field order.
     */
    def construct: (A1, A2) => Z
    /**
     * The annotations on the case class.
     */
    def annotations: Chunk[Any]

    override def makeAccessors(
      b: AccessorBuilder
    ): (b.Lens[Field1, Z, A1], b.Lens[Field2, Z, A2]) =
      (b.makeLens(self, field1), b.makeLens(self, field2))

    override val fields: Chunk[Field[Z, _]] = Chunk(field1, field2)

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 2)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] =
      Chunk(Some(field1.get(value)), Some(field2.get(value)))

    override def toString: String = s"CaseClass2($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass2]].
   */
  object CaseClass2 {

    /**
     * Returns a [[Schema.CaseClass2]] with the given type id, 2 field schemas in order, constructor function and annotations.
     */
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
          CaseClass2(id0, field01, field02, construct0, (annotations0 :+ annotation).distinct)
      }

    /**
     * Extracts the type id, 2 field schemas in order, constructor function and annotations of a [[Schema.CaseClass2]].
     */
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

    /**
     * A [[Schema.CaseClass2]] whose field name types `Field1` to `Field2` are fixed to `F1` to `F2`.
     */
    type WithFields[F1 <: Singleton with String, F2 <: Singleton with String, A1, A2, Z] =
      CaseClass2[A1, A2, Z] {
        type Field1 = F1
        type Field2 = F2
      }
  }

  /**
   * The schema of a case class `Z` with 3 fields, whose types are `A1` to `A3` in field order. `CaseClass3.apply` builds it from the type id, the 3 field schemas and a constructor function that takes the 3 field values and returns `Z`.
   */
  sealed trait CaseClass3[A1, A2, A3, Z] extends Record[Z] { self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 3 lenses, one per field in order, each from `Z` to the field type.
     */
    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Lens[Field1, Z, A1], Lens[Field2, Z, A2], Lens[Field3, Z, A3])

    override type Terms = (Field1, A1) with (Field2, A2) with (Field3, A3)

    override type FieldNames = Field1 with Field2 with Field3

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * Creates the case class from its 3 field values, in field order.
     */
    def construct: (A1, A2, A3) => Z
    /**
     * The annotations on the case class.
     */
    def annotations: Chunk[Any]

    override def makeAccessors(
      b: AccessorBuilder
    ): (b.Lens[Field1, Z, A1], b.Lens[Field2, Z, A2], b.Lens[Field3, Z, A3]) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3))

    override val fields: Chunk[Field[Z, _]] = Chunk(field1, field2, field3)

    override def construct(values: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, Z] =
      if (values.size == 3)
        try {
          Right(construct(values(0).asInstanceOf[A1], values(1).asInstanceOf[A2], values(2).asInstanceOf[A3]))
        } catch {
          case _: Throwable => Left("invalid type in values")
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] =
      Chunk(Some(field1.get(value)), Some(field2.get(value)), Some(field3.get(value)))
    override def toString: String = s"CaseClass3($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass3]].
   */
  object CaseClass3 {

    /**
     * Returns a [[Schema.CaseClass3]] with the given type id, 3 field schemas in order, constructor function and annotations.
     */
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
          CaseClass3(id0, field01, field02, field03, construct0, (annotations0 :+ annotation).distinct)
      }

    /**
     * Extracts the type id, 3 field schemas in order, constructor function and annotations of a [[Schema.CaseClass3]].
     */
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

    /**
     * A [[Schema.CaseClass3]] whose field name types `Field1` to `Field3` are fixed to `F1` to `F3`.
     */
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

  /**
   * The schema of a case class `Z` with 4 fields, whose types are `A1` to `A4` in field order. `CaseClass4.apply` builds it from the type id, the 4 field schemas and a constructor function that takes the 4 field values and returns `Z`.
   */
  sealed trait CaseClass4[A1, A2, A3, A4, Z] extends Record[Z] { self =>
    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 4 lenses, one per field in order, each from `Z` to the field type.
     */
    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Lens[Field1, Z, A1], Lens[Field2, Z, A2], Lens[Field3, Z, A3], Lens[Field4, Z, A4])

    override type Terms = (Field1, A1) with (Field2, A2) with (Field3, A3) with (Field4, A4)

    override type FieldNames = Field1 with Field2 with Field3 with Field4

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * Creates the case class from its 4 field values, in field order.
     */
    def construct: (A1, A2, A3, A4) => Z
    /**
     * The annotations on the case class.
     */
    def annotations: Chunk[Any]

    override def makeAccessors(b: AccessorBuilder): (
      b.Lens[Field1, Z, A1],
      b.Lens[Field2, Z, A2],
      b.Lens[Field3, Z, A3],
      b.Lens[Field4, Z, A4]
    ) =
      (b.makeLens(self, field1), b.makeLens(self, field2), b.makeLens(self, field3), b.makeLens(self, field4))

    override val fields: Chunk[Field[Z, _]] = Chunk(field1, field2, field3, field4)

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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] =
      Chunk(Some(field1.get(value)), Some(field2.get(value)), Some(field3.get(value)), Some(field4.get(value)))
    override def toString: String = s"CaseClass4($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass4]].
   */
  object CaseClass4 {

    /**
     * Returns a [[Schema.CaseClass4]] with the given type id, 4 field schemas in order, constructor function and annotations.
     */
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
          CaseClass4(id0, field01, field02, field03, field04, construct0, (annotations0 :+ annotation).distinct)
      }

    /**
     * Extracts the type id, 4 field schemas in order, constructor function and annotations of a [[Schema.CaseClass4]].
     */
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

    /**
     * A [[Schema.CaseClass4]] whose field name types `Field1` to `Field4` are fixed to `F1` to `F4`.
     */
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

  /**
   * The schema of a case class `Z` with 5 fields, whose types are `A1` to `A5` in field order. `CaseClass5.apply` builds it from the type id, the 5 field schemas and a constructor function that takes the 5 field values and returns `Z`.
   */
  sealed trait CaseClass5[A1, A2, A3, A4, A5, Z] extends Record[Z] { self =>
    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 5 lenses, one per field in order, each from `Z` to the field type.
     */
    type Accessors[Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Lens[Field1, Z, A1], Lens[Field2, Z, A2], Lens[Field3, Z, A3], Lens[Field4, Z, A4], Lens[Field5, Z, A5])

    override type Terms = (Field1, A1) with (Field2, A2) with (Field3, A3) with (Field4, A4) with (Field5, A5)

    override type FieldNames = Field1 with Field2 with Field3 with Field4 with Field5

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]

    /**
     * Creates the case class from its 5 field values, in field order.
     */
    def construct: (A1, A2, A3, A4, A5) => Z
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] = Chunk(field1, field2, field3, field4, field5)

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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value))
    )
    override def toString: String = s"CaseClass5($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass5]].
   */
  object CaseClass5 {

    /**
     * Returns a [[Schema.CaseClass5]] with the given type id, 5 field schemas in order, constructor function and annotations.
     */
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
          CaseClass5(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            construct0,
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 5 field schemas in order, constructor function and annotations of a [[Schema.CaseClass5]].
     */
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

    /**
     * A [[Schema.CaseClass5]] whose field name types `Field1` to `Field5` are fixed to `F1` to `F5`.
     */
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

  /**
   * The schema of a case class `Z` with 6 fields, whose types are `A1` to `A6` in field order. `CaseClass6.apply` builds it from the type id, the 6 field schemas and a constructor function that takes the 6 field values and returns `Z`.
   */
  sealed trait CaseClass6[A1, A2, A3, A4, A5, A6, Z] extends Record[Z] { self =>
    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 6 lenses, one per field in order, each from `Z` to the field type.
     */
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]

    /**
     * Creates the case class from its 6 field values, in field order.
     */
    def construct: (A1, A2, A3, A4, A5, A6) => Z
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] = Chunk(field1, field2, field3, field4, field5, field6)

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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value))
    )

    override def toString: String = s"CaseClass6($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass6]].
   */
  object CaseClass6 {

    /**
     * Returns a [[Schema.CaseClass6]] with the given type id, 6 field schemas in order, constructor function and annotations.
     */
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
          CaseClass6(
            id0,
            field01,
            field02,
            field03,
            field04,
            field05,
            field06,
            construct0,
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 6 field schemas in order, constructor function and annotations of a [[Schema.CaseClass6]].
     */
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

    /**
     * A [[Schema.CaseClass6]] whose field name types `Field1` to `Field6` are fixed to `F1` to `F6`.
     */
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

  /**
   * The schema of a case class `Z` with 7 fields, whose types are `A1` to `A7` in field order. `CaseClass7.apply` builds it from the type id, the 7 field schemas and a constructor function that takes the 7 field values and returns `Z`.
   */
  sealed trait CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z] extends Record[Z] { self =>
    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 7 lenses, one per field in order, each from `Z` to the field type.
     */
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]

    /**
     * Creates the case class from its 7 field values, in field order.
     */
    def construct: (A1, A2, A3, A4, A5, A6, A7) => Z
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] = Chunk(field1, field2, field3, field4, field5, field6, field7)

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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value))
    )

    override def toString: String = s"CaseClass7($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass7]].
   */
  object CaseClass7 {

    /**
     * Returns a [[Schema.CaseClass7]] with the given type id, 7 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 7 field schemas in order, constructor function and annotations of a [[Schema.CaseClass7]].
     */
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

    /**
     * A [[Schema.CaseClass7]] whose field name types `Field1` to `Field7` are fixed to `F1` to `F7`.
     */
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

  /**
   * The schema of a case class `Z` with 8 fields, whose types are `A1` to `A8` in field order. `CaseClass8.apply` builds it from the type id, the 8 field schemas and a constructor function that takes the 8 field values and returns `Z`.
   */
  sealed trait CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z] extends Record[Z] { self =>
    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 8 lenses, one per field in order, each from `Z` to the field type.
     */
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]

    /**
     * Creates the case class from its 8 field values, in field order.
     */
    def construct: (A1, A2, A3, A4, A5, A6, A7, A8) => Z
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] = Chunk(field1, field2, field3, field4, field5, field6, field7, field8)

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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value))
    )

    override def toString: String = s"CaseClass8($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass8]].
   */
  object CaseClass8 {

    /**
     * Returns a [[Schema.CaseClass8]] with the given type id, 8 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 8 field schemas in order, constructor function and annotations of a [[Schema.CaseClass8]].
     */
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

    /**
     * A [[Schema.CaseClass8]] whose field name types `Field1` to `Field8` are fixed to `F1` to `F8`.
     */
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

  /**
   * The schema of a case class `Z` with 9 fields, whose types are `A1` to `A9` in field order. `CaseClass9.apply` builds it from the type id, the 9 field schemas and a constructor function that takes the 9 field values and returns `Z`.
   */
  sealed trait CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z] extends Record[Z] { self =>
    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 9 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)

    override type FieldNames = Field1
      with Field2
      with Field3
      with Field4
      with Field5
      with Field6
      with Field7
      with Field8
      with Field9

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]

    /**
     * Creates the case class from its 9 field values, in field order.
     */
    def construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z
    /**
     * The annotations on the case class.
     */
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
    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value))
    )
    override def toString: String = s"CaseClass9($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass9]].
   */
  object CaseClass9 {

    /**
     * Returns a [[Schema.CaseClass9]] with the given type id, 9 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 9 field schemas in order, constructor function and annotations of a [[Schema.CaseClass9]].
     */
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

    /**
     * A [[Schema.CaseClass9]] whose field name types `Field1` to `Field9` are fixed to `F1` to `F9`.
     */
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

  /**
   * The schema of a case class `Z` with 10 fields, whose types are `A1` to `A10` in field order. `CaseClass10.apply` builds it from the type id, the 10 field schemas and a constructor function that takes the 10 field values and returns `Z`.
   */
  sealed trait CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z] extends Record[Z] { self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 10 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]

    /**
     * Creates the case class from its 10 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value))
    )

    override def toString: String = s"CaseClass10($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass10]].
   */
  object CaseClass10 {

    /**
     * Returns a [[Schema.CaseClass10]] with the given type id, 10 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 10 field schemas in order, constructor function and annotations of a [[Schema.CaseClass10]].
     */
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

    /**
     * A [[Schema.CaseClass10]] whose field name types `Field1` to `Field10` are fixed to `F1` to `F10`.
     */
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

  /**
   * The schema of a case class `Z` with 11 fields, whose types are `A1` to `A11` in field order. `CaseClass11.apply` builds it from the type id, the 11 field schemas and a constructor function that takes the 11 field values and returns `Z`.
   */
  sealed trait CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z] extends Record[Z] { self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String
    /**
     * The name of field 11 as a singleton string type.
     */
    type Field11 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 11 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]
    /**
     * The schema of field 11, whose value type is `A11`.
     */
    def field11: Field.WithFieldName[Z, Field11, A11]

    /**
     * Creates the case class from its 11 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value)),
      Some(field11.get(value))
    )

    override def toString: String = s"CaseClass11($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass11]].
   */
  object CaseClass11 {

    /**
     * Returns a [[Schema.CaseClass11]] with the given type id, 11 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 11 field schemas in order, constructor function and annotations of a [[Schema.CaseClass11]].
     */
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

    /**
     * A [[Schema.CaseClass11]] whose field name types `Field1` to `Field11` are fixed to `F1` to `F11`.
     */
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

  /**
   * The schema of a case class `Z` with 12 fields, whose types are `A1` to `A12` in field order. `CaseClass12.apply` builds it from the type id, the 12 field schemas and a constructor function that takes the 12 field values and returns `Z`.
   */
  sealed trait CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z] extends Record[Z] { self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String
    /**
     * The name of field 11 as a singleton string type.
     */
    type Field11 <: Singleton with String
    /**
     * The name of field 12 as a singleton string type.
     */
    type Field12 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 12 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]
    /**
     * The schema of field 11, whose value type is `A11`.
     */
    def field11: Field.WithFieldName[Z, Field11, A11]
    /**
     * The schema of field 12, whose value type is `A12`.
     */
    def field12: Field.WithFieldName[Z, Field12, A12]

    /**
     * Creates the case class from its 12 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value)),
      Some(field11.get(value)),
      Some(field12.get(value))
    )

    override def toString: String = s"CaseClass12($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass12]].
   */
  object CaseClass12 {

    /**
     * Returns a [[Schema.CaseClass12]] with the given type id, 12 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 12 field schemas in order, constructor function and annotations of a [[Schema.CaseClass12]].
     */
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

    /**
     * A [[Schema.CaseClass12]] whose field name types `Field1` to `Field12` are fixed to `F1` to `F12`.
     */
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

  /**
   * The schema of a case class `Z` with 13 fields, whose types are `A1` to `A13` in field order. `CaseClass13.apply` builds it from the type id, the 13 field schemas and a constructor function that takes the 13 field values and returns `Z`.
   */
  sealed trait CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z] extends Record[Z] { self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String
    /**
     * The name of field 11 as a singleton string type.
     */
    type Field11 <: Singleton with String
    /**
     * The name of field 12 as a singleton string type.
     */
    type Field12 <: Singleton with String
    /**
     * The name of field 13 as a singleton string type.
     */
    type Field13 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 13 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]
    /**
     * The schema of field 11, whose value type is `A11`.
     */
    def field11: Field.WithFieldName[Z, Field11, A11]
    /**
     * The schema of field 12, whose value type is `A12`.
     */
    def field12: Field.WithFieldName[Z, Field12, A12]
    /**
     * The schema of field 13, whose value type is `A13`.
     */
    def field13: Field.WithFieldName[Z, Field13, A13]

    /**
     * Creates the case class from its 13 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value)),
      Some(field11.get(value)),
      Some(field12.get(value)),
      Some(field13.get(value))
    )

    override def toString: String = s"CaseClass13($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass13]].
   */
  object CaseClass13 {

    /**
     * Returns a [[Schema.CaseClass13]] with the given type id, 13 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 13 field schemas in order, constructor function and annotations of a [[Schema.CaseClass13]].
     */
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

    /**
     * A [[Schema.CaseClass13]] whose field name types `Field1` to `Field13` are fixed to `F1` to `F13`.
     */
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

  /**
   * The schema of a case class `Z` with 14 fields, whose types are `A1` to `A14` in field order. `CaseClass14.apply` builds it from the type id, the 14 field schemas and a constructor function that takes the 14 field values and returns `Z`.
   */
  sealed trait CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z] extends Record[Z] { self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String
    /**
     * The name of field 11 as a singleton string type.
     */
    type Field11 <: Singleton with String
    /**
     * The name of field 12 as a singleton string type.
     */
    type Field12 <: Singleton with String
    /**
     * The name of field 13 as a singleton string type.
     */
    type Field13 <: Singleton with String
    /**
     * The name of field 14 as a singleton string type.
     */
    type Field14 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 14 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]
    /**
     * The schema of field 11, whose value type is `A11`.
     */
    def field11: Field.WithFieldName[Z, Field11, A11]
    /**
     * The schema of field 12, whose value type is `A12`.
     */
    def field12: Field.WithFieldName[Z, Field12, A12]
    /**
     * The schema of field 13, whose value type is `A13`.
     */
    def field13: Field.WithFieldName[Z, Field13, A13]
    /**
     * The schema of field 14, whose value type is `A14`.
     */
    def field14: Field.WithFieldName[Z, Field14, A14]

    /**
     * Creates the case class from its 14 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value)),
      Some(field11.get(value)),
      Some(field12.get(value)),
      Some(field13.get(value)),
      Some(field14.get(value))
    )

    override def toString: String = s"CaseClass14($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass14]].
   */
  object CaseClass14 {

    /**
     * Returns a [[Schema.CaseClass14]] with the given type id, 14 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 14 field schemas in order, constructor function and annotations of a [[Schema.CaseClass14]].
     */
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

    /**
     * A [[Schema.CaseClass14]] whose field name types `Field1` to `Field14` are fixed to `F1` to `F14`.
     */
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

  /**
   * The schema of a case class `Z` with 15 fields, whose types are `A1` to `A15` in field order. `CaseClass15.apply` builds it from the type id, the 15 field schemas and a constructor function that takes the 15 field values and returns `Z`.
   */
  sealed trait CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z] extends Record[Z] {
    self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String
    /**
     * The name of field 11 as a singleton string type.
     */
    type Field11 <: Singleton with String
    /**
     * The name of field 12 as a singleton string type.
     */
    type Field12 <: Singleton with String
    /**
     * The name of field 13 as a singleton string type.
     */
    type Field13 <: Singleton with String
    /**
     * The name of field 14 as a singleton string type.
     */
    type Field14 <: Singleton with String
    /**
     * The name of field 15 as a singleton string type.
     */
    type Field15 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 15 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]
    /**
     * The schema of field 11, whose value type is `A11`.
     */
    def field11: Field.WithFieldName[Z, Field11, A11]
    /**
     * The schema of field 12, whose value type is `A12`.
     */
    def field12: Field.WithFieldName[Z, Field12, A12]
    /**
     * The schema of field 13, whose value type is `A13`.
     */
    def field13: Field.WithFieldName[Z, Field13, A13]
    /**
     * The schema of field 14, whose value type is `A14`.
     */
    def field14: Field.WithFieldName[Z, Field14, A14]
    /**
     * The schema of field 15, whose value type is `A15`.
     */
    def field15: Field.WithFieldName[Z, Field15, A15]

    /**
     * Creates the case class from its 15 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value)),
      Some(field11.get(value)),
      Some(field12.get(value)),
      Some(field13.get(value)),
      Some(field14.get(value)),
      Some(field15.get(value))
    )

    override def toString: String = s"CaseClass15($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass15]].
   */
  object CaseClass15 {

    /**
     * Returns a [[Schema.CaseClass15]] with the given type id, 15 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 15 field schemas in order, constructor function and annotations of a [[Schema.CaseClass15]].
     */
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

    /**
     * A [[Schema.CaseClass15]] whose field name types `Field1` to `Field15` are fixed to `F1` to `F15`.
     */
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

  /**
   * The schema of a case class `Z` with 16 fields, whose types are `A1` to `A16` in field order. `CaseClass16.apply` builds it from the type id, the 16 field schemas and a constructor function that takes the 16 field values and returns `Z`.
   */
  sealed trait CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z] extends Record[Z] {
    self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String
    /**
     * The name of field 11 as a singleton string type.
     */
    type Field11 <: Singleton with String
    /**
     * The name of field 12 as a singleton string type.
     */
    type Field12 <: Singleton with String
    /**
     * The name of field 13 as a singleton string type.
     */
    type Field13 <: Singleton with String
    /**
     * The name of field 14 as a singleton string type.
     */
    type Field14 <: Singleton with String
    /**
     * The name of field 15 as a singleton string type.
     */
    type Field15 <: Singleton with String
    /**
     * The name of field 16 as a singleton string type.
     */
    type Field16 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 16 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]
    /**
     * The schema of field 11, whose value type is `A11`.
     */
    def field11: Field.WithFieldName[Z, Field11, A11]
    /**
     * The schema of field 12, whose value type is `A12`.
     */
    def field12: Field.WithFieldName[Z, Field12, A12]
    /**
     * The schema of field 13, whose value type is `A13`.
     */
    def field13: Field.WithFieldName[Z, Field13, A13]
    /**
     * The schema of field 14, whose value type is `A14`.
     */
    def field14: Field.WithFieldName[Z, Field14, A14]
    /**
     * The schema of field 15, whose value type is `A15`.
     */
    def field15: Field.WithFieldName[Z, Field15, A15]
    /**
     * The schema of field 16, whose value type is `A16`.
     */
    def field16: Field.WithFieldName[Z, Field16, A16]

    /**
     * Creates the case class from its 16 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value)),
      Some(field11.get(value)),
      Some(field12.get(value)),
      Some(field13.get(value)),
      Some(field14.get(value)),
      Some(field15.get(value)),
      Some(field16.get(value))
    )

    override def toString: String = s"CaseClass16($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass16]].
   */
  object CaseClass16 {

    /**
     * Returns a [[Schema.CaseClass16]] with the given type id, 16 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 16 field schemas in order, constructor function and annotations of a [[Schema.CaseClass16]].
     */
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

    /**
     * A [[Schema.CaseClass16]] whose field name types `Field1` to `Field16` are fixed to `F1` to `F16`.
     */
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

  /**
   * The schema of a case class `Z` with 17 fields, whose types are `A1` to `A17` in field order. `CaseClass17.apply` builds it from the type id, the 17 field schemas and a constructor function that takes the 17 field values and returns `Z`.
   */
  sealed trait CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]
      extends Record[Z] { self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String
    /**
     * The name of field 11 as a singleton string type.
     */
    type Field11 <: Singleton with String
    /**
     * The name of field 12 as a singleton string type.
     */
    type Field12 <: Singleton with String
    /**
     * The name of field 13 as a singleton string type.
     */
    type Field13 <: Singleton with String
    /**
     * The name of field 14 as a singleton string type.
     */
    type Field14 <: Singleton with String
    /**
     * The name of field 15 as a singleton string type.
     */
    type Field15 <: Singleton with String
    /**
     * The name of field 16 as a singleton string type.
     */
    type Field16 <: Singleton with String
    /**
     * The name of field 17 as a singleton string type.
     */
    type Field17 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 17 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]
    /**
     * The schema of field 11, whose value type is `A11`.
     */
    def field11: Field.WithFieldName[Z, Field11, A11]
    /**
     * The schema of field 12, whose value type is `A12`.
     */
    def field12: Field.WithFieldName[Z, Field12, A12]
    /**
     * The schema of field 13, whose value type is `A13`.
     */
    def field13: Field.WithFieldName[Z, Field13, A13]
    /**
     * The schema of field 14, whose value type is `A14`.
     */
    def field14: Field.WithFieldName[Z, Field14, A14]
    /**
     * The schema of field 15, whose value type is `A15`.
     */
    def field15: Field.WithFieldName[Z, Field15, A15]
    /**
     * The schema of field 16, whose value type is `A16`.
     */
    def field16: Field.WithFieldName[Z, Field16, A16]
    /**
     * The schema of field 17, whose value type is `A17`.
     */
    def field17: Field.WithFieldName[Z, Field17, A17]

    /**
     * Creates the case class from its 17 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value)),
      Some(field11.get(value)),
      Some(field12.get(value)),
      Some(field13.get(value)),
      Some(field14.get(value)),
      Some(field15.get(value)),
      Some(field16.get(value)),
      Some(field17.get(value))
    )

    override def toString: String = s"CaseClass17($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass17]].
   */
  object CaseClass17 {

    /**
     * Returns a [[Schema.CaseClass17]] with the given type id, 17 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 17 field schemas in order, constructor function and annotations of a [[Schema.CaseClass17]].
     */
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

    /**
     * A [[Schema.CaseClass17]] whose field name types `Field1` to `Field17` are fixed to `F1` to `F17`.
     */
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

  /**
   * The schema of a case class `Z` with 18 fields, whose types are `A1` to `A18` in field order. `CaseClass18.apply` builds it from the type id, the 18 field schemas and a constructor function that takes the 18 field values and returns `Z`.
   */
  sealed trait CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]
      extends Record[Z] { self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String
    /**
     * The name of field 11 as a singleton string type.
     */
    type Field11 <: Singleton with String
    /**
     * The name of field 12 as a singleton string type.
     */
    type Field12 <: Singleton with String
    /**
     * The name of field 13 as a singleton string type.
     */
    type Field13 <: Singleton with String
    /**
     * The name of field 14 as a singleton string type.
     */
    type Field14 <: Singleton with String
    /**
     * The name of field 15 as a singleton string type.
     */
    type Field15 <: Singleton with String
    /**
     * The name of field 16 as a singleton string type.
     */
    type Field16 <: Singleton with String
    /**
     * The name of field 17 as a singleton string type.
     */
    type Field17 <: Singleton with String
    /**
     * The name of field 18 as a singleton string type.
     */
    type Field18 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 18 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]
    /**
     * The schema of field 11, whose value type is `A11`.
     */
    def field11: Field.WithFieldName[Z, Field11, A11]
    /**
     * The schema of field 12, whose value type is `A12`.
     */
    def field12: Field.WithFieldName[Z, Field12, A12]
    /**
     * The schema of field 13, whose value type is `A13`.
     */
    def field13: Field.WithFieldName[Z, Field13, A13]
    /**
     * The schema of field 14, whose value type is `A14`.
     */
    def field14: Field.WithFieldName[Z, Field14, A14]
    /**
     * The schema of field 15, whose value type is `A15`.
     */
    def field15: Field.WithFieldName[Z, Field15, A15]
    /**
     * The schema of field 16, whose value type is `A16`.
     */
    def field16: Field.WithFieldName[Z, Field16, A16]
    /**
     * The schema of field 17, whose value type is `A17`.
     */
    def field17: Field.WithFieldName[Z, Field17, A17]
    /**
     * The schema of field 18, whose value type is `A18`.
     */
    def field18: Field.WithFieldName[Z, Field18, A18]

    /**
     * Creates the case class from its 18 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value)),
      Some(field11.get(value)),
      Some(field12.get(value)),
      Some(field13.get(value)),
      Some(field14.get(value)),
      Some(field15.get(value)),
      Some(field16.get(value)),
      Some(field17.get(value)),
      Some(field18.get(value))
    )

    override def toString: String = s"CaseClass18($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass18]].
   */
  object CaseClass18 {

    /**
     * Returns a [[Schema.CaseClass18]] with the given type id, 18 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 18 field schemas in order, constructor function and annotations of a [[Schema.CaseClass18]].
     */
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

    /**
     * A [[Schema.CaseClass18]] whose field name types `Field1` to `Field18` are fixed to `F1` to `F18`.
     */
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

  /**
   * The schema of a case class `Z` with 19 fields, whose types are `A1` to `A19` in field order. `CaseClass19.apply` builds it from the type id, the 19 field schemas and a constructor function that takes the 19 field values and returns `Z`.
   */
  sealed trait CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]
      extends Record[Z] { self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String
    /**
     * The name of field 11 as a singleton string type.
     */
    type Field11 <: Singleton with String
    /**
     * The name of field 12 as a singleton string type.
     */
    type Field12 <: Singleton with String
    /**
     * The name of field 13 as a singleton string type.
     */
    type Field13 <: Singleton with String
    /**
     * The name of field 14 as a singleton string type.
     */
    type Field14 <: Singleton with String
    /**
     * The name of field 15 as a singleton string type.
     */
    type Field15 <: Singleton with String
    /**
     * The name of field 16 as a singleton string type.
     */
    type Field16 <: Singleton with String
    /**
     * The name of field 17 as a singleton string type.
     */
    type Field17 <: Singleton with String
    /**
     * The name of field 18 as a singleton string type.
     */
    type Field18 <: Singleton with String
    /**
     * The name of field 19 as a singleton string type.
     */
    type Field19 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 19 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]
    /**
     * The schema of field 11, whose value type is `A11`.
     */
    def field11: Field.WithFieldName[Z, Field11, A11]
    /**
     * The schema of field 12, whose value type is `A12`.
     */
    def field12: Field.WithFieldName[Z, Field12, A12]
    /**
     * The schema of field 13, whose value type is `A13`.
     */
    def field13: Field.WithFieldName[Z, Field13, A13]
    /**
     * The schema of field 14, whose value type is `A14`.
     */
    def field14: Field.WithFieldName[Z, Field14, A14]
    /**
     * The schema of field 15, whose value type is `A15`.
     */
    def field15: Field.WithFieldName[Z, Field15, A15]
    /**
     * The schema of field 16, whose value type is `A16`.
     */
    def field16: Field.WithFieldName[Z, Field16, A16]
    /**
     * The schema of field 17, whose value type is `A17`.
     */
    def field17: Field.WithFieldName[Z, Field17, A17]
    /**
     * The schema of field 18, whose value type is `A18`.
     */
    def field18: Field.WithFieldName[Z, Field18, A18]
    /**
     * The schema of field 19, whose value type is `A19`.
     */
    def field19: Field.WithFieldName[Z, Field19, A19]

    /**
     * Creates the case class from its 19 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value)),
      Some(field11.get(value)),
      Some(field12.get(value)),
      Some(field13.get(value)),
      Some(field14.get(value)),
      Some(field15.get(value)),
      Some(field16.get(value)),
      Some(field17.get(value)),
      Some(field18.get(value)),
      Some(field19.get(value))
    )

    override def toString: String = s"CaseClass19($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass19]].
   */
  object CaseClass19 {

    /**
     * Returns a [[Schema.CaseClass19]] with the given type id, 19 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 19 field schemas in order, constructor function and annotations of a [[Schema.CaseClass19]].
     */
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

    /**
     * A [[Schema.CaseClass19]] whose field name types `Field1` to `Field19` are fixed to `F1` to `F19`.
     */
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

  /**
   * The schema of a case class `Z` with 20 fields, whose types are `A1` to `A20` in field order. `CaseClass20.apply` builds it from the type id, the 20 field schemas and a constructor function that takes the 20 field values and returns `Z`.
   */
  sealed trait CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z]
      extends Record[Z] { self =>

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String
    /**
     * The name of field 11 as a singleton string type.
     */
    type Field11 <: Singleton with String
    /**
     * The name of field 12 as a singleton string type.
     */
    type Field12 <: Singleton with String
    /**
     * The name of field 13 as a singleton string type.
     */
    type Field13 <: Singleton with String
    /**
     * The name of field 14 as a singleton string type.
     */
    type Field14 <: Singleton with String
    /**
     * The name of field 15 as a singleton string type.
     */
    type Field15 <: Singleton with String
    /**
     * The name of field 16 as a singleton string type.
     */
    type Field16 <: Singleton with String
    /**
     * The name of field 17 as a singleton string type.
     */
    type Field17 <: Singleton with String
    /**
     * The name of field 18 as a singleton string type.
     */
    type Field18 <: Singleton with String
    /**
     * The name of field 19 as a singleton string type.
     */
    type Field19 <: Singleton with String
    /**
     * The name of field 20 as a singleton string type.
     */
    type Field20 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 20 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]
    /**
     * The schema of field 11, whose value type is `A11`.
     */
    def field11: Field.WithFieldName[Z, Field11, A11]
    /**
     * The schema of field 12, whose value type is `A12`.
     */
    def field12: Field.WithFieldName[Z, Field12, A12]
    /**
     * The schema of field 13, whose value type is `A13`.
     */
    def field13: Field.WithFieldName[Z, Field13, A13]
    /**
     * The schema of field 14, whose value type is `A14`.
     */
    def field14: Field.WithFieldName[Z, Field14, A14]
    /**
     * The schema of field 15, whose value type is `A15`.
     */
    def field15: Field.WithFieldName[Z, Field15, A15]
    /**
     * The schema of field 16, whose value type is `A16`.
     */
    def field16: Field.WithFieldName[Z, Field16, A16]
    /**
     * The schema of field 17, whose value type is `A17`.
     */
    def field17: Field.WithFieldName[Z, Field17, A17]
    /**
     * The schema of field 18, whose value type is `A18`.
     */
    def field18: Field.WithFieldName[Z, Field18, A18]
    /**
     * The schema of field 19, whose value type is `A19`.
     */
    def field19: Field.WithFieldName[Z, Field19, A19]
    /**
     * The schema of field 20, whose value type is `A20`.
     */
    def field20: Field.WithFieldName[Z, Field20, A20]

    /**
     * Creates the case class from its 20 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value)),
      Some(field11.get(value)),
      Some(field12.get(value)),
      Some(field13.get(value)),
      Some(field14.get(value)),
      Some(field15.get(value)),
      Some(field16.get(value)),
      Some(field17.get(value)),
      Some(field18.get(value)),
      Some(field19.get(value)),
      Some(field20.get(value))
    )

    override def toString: String = s"CaseClass20($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass20]].
   */
  object CaseClass20 {

    /**
     * Returns a [[Schema.CaseClass20]] with the given type id, 20 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 20 field schemas in order, constructor function and annotations of a [[Schema.CaseClass20]]. The last element of the tuple is the nested tuple `(construct, annotations)`.
     */
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

    /**
     * A [[Schema.CaseClass20]] whose field name types `Field1` to `Field20` are fixed to `F1` to `F20`.
     */
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

  /**
   * The schema of a case class `Z` with 21 fields, whose types are `A1` to `A21` in field order. `CaseClass21.apply` builds it from the type id, the 21 field schemas and a constructor function that takes the 21 field values and returns `Z`.
   */
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

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String
    /**
     * The name of field 11 as a singleton string type.
     */
    type Field11 <: Singleton with String
    /**
     * The name of field 12 as a singleton string type.
     */
    type Field12 <: Singleton with String
    /**
     * The name of field 13 as a singleton string type.
     */
    type Field13 <: Singleton with String
    /**
     * The name of field 14 as a singleton string type.
     */
    type Field14 <: Singleton with String
    /**
     * The name of field 15 as a singleton string type.
     */
    type Field15 <: Singleton with String
    /**
     * The name of field 16 as a singleton string type.
     */
    type Field16 <: Singleton with String
    /**
     * The name of field 17 as a singleton string type.
     */
    type Field17 <: Singleton with String
    /**
     * The name of field 18 as a singleton string type.
     */
    type Field18 <: Singleton with String
    /**
     * The name of field 19 as a singleton string type.
     */
    type Field19 <: Singleton with String
    /**
     * The name of field 20 as a singleton string type.
     */
    type Field20 <: Singleton with String
    /**
     * The name of field 21 as a singleton string type.
     */
    type Field21 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 21 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]
    /**
     * The schema of field 11, whose value type is `A11`.
     */
    def field11: Field.WithFieldName[Z, Field11, A11]
    /**
     * The schema of field 12, whose value type is `A12`.
     */
    def field12: Field.WithFieldName[Z, Field12, A12]
    /**
     * The schema of field 13, whose value type is `A13`.
     */
    def field13: Field.WithFieldName[Z, Field13, A13]
    /**
     * The schema of field 14, whose value type is `A14`.
     */
    def field14: Field.WithFieldName[Z, Field14, A14]
    /**
     * The schema of field 15, whose value type is `A15`.
     */
    def field15: Field.WithFieldName[Z, Field15, A15]
    /**
     * The schema of field 16, whose value type is `A16`.
     */
    def field16: Field.WithFieldName[Z, Field16, A16]
    /**
     * The schema of field 17, whose value type is `A17`.
     */
    def field17: Field.WithFieldName[Z, Field17, A17]
    /**
     * The schema of field 18, whose value type is `A18`.
     */
    def field18: Field.WithFieldName[Z, Field18, A18]
    /**
     * The schema of field 19, whose value type is `A19`.
     */
    def field19: Field.WithFieldName[Z, Field19, A19]
    /**
     * The schema of field 20, whose value type is `A20`.
     */
    def field20: Field.WithFieldName[Z, Field20, A20]
    /**
     * The schema of field 21, whose value type is `A21`.
     */
    def field21: Field.WithFieldName[Z, Field21, A21]

    /**
     * Creates the case class from its 21 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value)),
      Some(field11.get(value)),
      Some(field12.get(value)),
      Some(field13.get(value)),
      Some(field14.get(value)),
      Some(field15.get(value)),
      Some(field16.get(value)),
      Some(field17.get(value)),
      Some(field18.get(value)),
      Some(field19.get(value)),
      Some(field20.get(value)),
      Some(field21.get(value))
    )

    override def toString: String = s"CaseClass21($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass21]].
   */
  object CaseClass21 {

    /**
     * Returns a [[Schema.CaseClass21]] with the given type id, 21 field schemas in order, constructor function and annotations.
     */
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
      new CaseClass21[
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 21 field schemas in order, constructor function and annotations of a [[Schema.CaseClass21]]. The last element of the tuple is the nested tuple `(field21, construct, annotations)`.
     */
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

    /**
     * A [[Schema.CaseClass21]] whose field name types `Field1` to `Field21` are fixed to `F1` to `F21`.
     */
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

  /**
   * The schema of a case class `Z` with 22 fields, whose types are `A1` to `A22` in field order. `CaseClass22.apply` builds it from the type id, the 22 field schemas and a constructor function that takes the 22 field values and returns `Z`.
   */
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

    /**
     * The name of field 1 as a singleton string type.
     */
    type Field1 <: Singleton with String
    /**
     * The name of field 2 as a singleton string type.
     */
    type Field2 <: Singleton with String
    /**
     * The name of field 3 as a singleton string type.
     */
    type Field3 <: Singleton with String
    /**
     * The name of field 4 as a singleton string type.
     */
    type Field4 <: Singleton with String
    /**
     * The name of field 5 as a singleton string type.
     */
    type Field5 <: Singleton with String
    /**
     * The name of field 6 as a singleton string type.
     */
    type Field6 <: Singleton with String
    /**
     * The name of field 7 as a singleton string type.
     */
    type Field7 <: Singleton with String
    /**
     * The name of field 8 as a singleton string type.
     */
    type Field8 <: Singleton with String
    /**
     * The name of field 9 as a singleton string type.
     */
    type Field9 <: Singleton with String
    /**
     * The name of field 10 as a singleton string type.
     */
    type Field10 <: Singleton with String
    /**
     * The name of field 11 as a singleton string type.
     */
    type Field11 <: Singleton with String
    /**
     * The name of field 12 as a singleton string type.
     */
    type Field12 <: Singleton with String
    /**
     * The name of field 13 as a singleton string type.
     */
    type Field13 <: Singleton with String
    /**
     * The name of field 14 as a singleton string type.
     */
    type Field14 <: Singleton with String
    /**
     * The name of field 15 as a singleton string type.
     */
    type Field15 <: Singleton with String
    /**
     * The name of field 16 as a singleton string type.
     */
    type Field16 <: Singleton with String
    /**
     * The name of field 17 as a singleton string type.
     */
    type Field17 <: Singleton with String
    /**
     * The name of field 18 as a singleton string type.
     */
    type Field18 <: Singleton with String
    /**
     * The name of field 19 as a singleton string type.
     */
    type Field19 <: Singleton with String
    /**
     * The name of field 20 as a singleton string type.
     */
    type Field20 <: Singleton with String
    /**
     * The name of field 21 as a singleton string type.
     */
    type Field21 <: Singleton with String
    /**
     * The name of field 22 as a singleton string type.
     */
    type Field22 <: Singleton with String

    /**
     * The optics of this schema: a tuple of 22 lenses, one per field in order, each from `Z` to the field type.
     */
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
      with (Field9, A9)
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

    /**
     * The type id of the case class.
     */
    def id: TypeId
    /**
     * The schema of field 1, whose value type is `A1`.
     */
    def field1: Field.WithFieldName[Z, Field1, A1]
    /**
     * The schema of field 2, whose value type is `A2`.
     */
    def field2: Field.WithFieldName[Z, Field2, A2]
    /**
     * The schema of field 3, whose value type is `A3`.
     */
    def field3: Field.WithFieldName[Z, Field3, A3]
    /**
     * The schema of field 4, whose value type is `A4`.
     */
    def field4: Field.WithFieldName[Z, Field4, A4]
    /**
     * The schema of field 5, whose value type is `A5`.
     */
    def field5: Field.WithFieldName[Z, Field5, A5]
    /**
     * The schema of field 6, whose value type is `A6`.
     */
    def field6: Field.WithFieldName[Z, Field6, A6]
    /**
     * The schema of field 7, whose value type is `A7`.
     */
    def field7: Field.WithFieldName[Z, Field7, A7]
    /**
     * The schema of field 8, whose value type is `A8`.
     */
    def field8: Field.WithFieldName[Z, Field8, A8]
    /**
     * The schema of field 9, whose value type is `A9`.
     */
    def field9: Field.WithFieldName[Z, Field9, A9]
    /**
     * The schema of field 10, whose value type is `A10`.
     */
    def field10: Field.WithFieldName[Z, Field10, A10]
    /**
     * The schema of field 11, whose value type is `A11`.
     */
    def field11: Field.WithFieldName[Z, Field11, A11]
    /**
     * The schema of field 12, whose value type is `A12`.
     */
    def field12: Field.WithFieldName[Z, Field12, A12]
    /**
     * The schema of field 13, whose value type is `A13`.
     */
    def field13: Field.WithFieldName[Z, Field13, A13]
    /**
     * The schema of field 14, whose value type is `A14`.
     */
    def field14: Field.WithFieldName[Z, Field14, A14]
    /**
     * The schema of field 15, whose value type is `A15`.
     */
    def field15: Field.WithFieldName[Z, Field15, A15]
    /**
     * The schema of field 16, whose value type is `A16`.
     */
    def field16: Field.WithFieldName[Z, Field16, A16]
    /**
     * The schema of field 17, whose value type is `A17`.
     */
    def field17: Field.WithFieldName[Z, Field17, A17]
    /**
     * The schema of field 18, whose value type is `A18`.
     */
    def field18: Field.WithFieldName[Z, Field18, A18]
    /**
     * The schema of field 19, whose value type is `A19`.
     */
    def field19: Field.WithFieldName[Z, Field19, A19]
    /**
     * The schema of field 20, whose value type is `A20`.
     */
    def field20: Field.WithFieldName[Z, Field20, A20]
    /**
     * The schema of field 21, whose value type is `A21`.
     */
    def field21: Field.WithFieldName[Z, Field21, A21]
    /**
     * The schema of field 22, whose value type is `A22`.
     */
    def field22: Field.WithFieldName[Z, Field22, A22]

    /**
     * Creates the case class from its 22 field values, in field order.
     */
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
    /**
     * The annotations on the case class.
     */
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

    override val fields: Chunk[Field[Z, _]] =
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
        }
      else
        Left(s"wrong number of values for $fields")

    override def deconstruct(value: Z)(implicit unsafe: Unsafe): Chunk[Option[Any]] = Chunk(
      Some(field1.get(value)),
      Some(field2.get(value)),
      Some(field3.get(value)),
      Some(field4.get(value)),
      Some(field5.get(value)),
      Some(field6.get(value)),
      Some(field7.get(value)),
      Some(field8.get(value)),
      Some(field9.get(value)),
      Some(field10.get(value)),
      Some(field11.get(value)),
      Some(field12.get(value)),
      Some(field13.get(value)),
      Some(field14.get(value)),
      Some(field15.get(value)),
      Some(field16.get(value)),
      Some(field17.get(value)),
      Some(field18.get(value)),
      Some(field19.get(value)),
      Some(field20.get(value)),
      Some(field21.get(value)),
      Some(field22.get(value))
    )

    override def toString: String = s"CaseClass22($id, ${fields.mkString(",")})"
  }

  /**
   * Constructor and extractor for [[Schema.CaseClass22]].
   */
  object CaseClass22 {

    /**
     * Returns a [[Schema.CaseClass22]] with the given type id, 22 field schemas in order, constructor function and annotations.
     */
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
            (annotations0 :+ annotation).distinct
          )
      }

    /**
     * Extracts the type id, 22 field schemas in order, constructor function and annotations of a [[Schema.CaseClass22]]. The last element of the tuple is the nested tuple `(field21, field22, construct, annotations)`.
     */
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

    /**
     * A [[Schema.CaseClass22]] whose field name types `Field1` to `Field22` are fixed to `F1` to `F22`.
     */
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
