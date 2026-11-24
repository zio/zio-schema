package zio.schema

import scala.annotation.nowarn
import scala.reflect.ClassTag

import zio.Chunk
import zio.schema.Deriver.{WrappedF, wrap}

/**
 * A `Deriver[F]` builds instances of a type class `F` from the `Schema` of a type. Implement one method per kind of schema, then pass the deriver to `Derive.derive`, which walks the structure of a concrete type at compile time and calls those methods with the instances it built for the nested parts.
 *
 * The abstract methods, which every deriver implements, are:
 *   - `deriveRecord`
 *   - `deriveEnum`
 *   - `derivePrimitive`
 *   - `deriveOption`
 *   - `deriveSequence`
 *   - `deriveMap`
 *   - `deriveTransformedRecord` (for case classes with more than 22 fields)
 *
 * These methods have default implementations that a deriver can override:
 *   - `deriveEither` (calls `deriveEnum` by default)
 *   - `deriveSet` (calls `deriveSequence` by default)
 *   - `deriveTupleN` (calls `deriveRecord` by default)
 *   - `derivePrimitiveAlias` and `deriveUnknown` (return the summoned instance by default)
 *
 * The `cached` method converts this deriver to one that uses a cache of instances shared between macro invocations.
 *
 * Each derive method gets an optional summoned implicit value of the derived type class. It is the deriver's decision whether to use the available instance (as a user defined customization for a given type) or not. `Derive.derive` summons an instance for each nested type, but not for the top-level type it is called with.
 *
 * If the decision is to always accept a summoned value if there is any, call the `autoAcceptSummoned` method, which returns a deriver that does this automatically and only calls this deriver's methods when there is no available implicit for the actual type.
 *
 * {{{
 * import zio.Chunk
 * import zio.schema.Deriver.WrappedF
 *
 * trait FieldCount[A] { def count: Int }
 *
 * def fieldCount[A](n: Int): FieldCount[A] = new FieldCount[A] { def count: Int = n }
 *
 * val counter: Deriver[FieldCount] = new Deriver[FieldCount] {
 *   def deriveRecord[A](record: Schema.Record[A], fields: => Chunk[WrappedF[FieldCount, _]], summoned: => Option[FieldCount[A]]): FieldCount[A] =
 *     fieldCount(fields.map(_.unwrap.count).sum)
 *   def deriveEnum[A](`enum`: Schema.Enum[A], cases: => Chunk[WrappedF[FieldCount, _]], summoned: => Option[FieldCount[A]]): FieldCount[A] =
 *     fieldCount(cases.map(_.unwrap.count).foldLeft(0)(math.max))
 *   def derivePrimitive[A](st: StandardType[A], summoned: => Option[FieldCount[A]]): FieldCount[A] =
 *     fieldCount(1)
 *   def deriveOption[A](option: Schema.Optional[A], inner: => FieldCount[A], summoned: => Option[FieldCount[Option[A]]]): FieldCount[Option[A]] =
 *     fieldCount(inner.count)
 *   def deriveSequence[C[_], A](sequence: Schema.Sequence[C[A], A, _], inner: => FieldCount[A], summoned: => Option[FieldCount[C[A]]]): FieldCount[C[A]] =
 *     fieldCount(inner.count)
 *   def deriveMap[K, V](map: Schema.Map[K, V], key: => FieldCount[K], value: => FieldCount[V], summoned: => Option[FieldCount[Map[K, V]]]): FieldCount[Map[K, V]] =
 *     fieldCount(key.count + value.count)
 *   def deriveTransformedRecord[A, B](record: Schema.Record[A], transform: Schema.Transform[A, B, _], fields: => Chunk[WrappedF[FieldCount, _]], summoned: => Option[FieldCount[B]]): FieldCount[B] =
 *     fieldCount(fields.map(_.unwrap.count).sum)
 * }
 *
 * final case class Point(x: Int, y: Int, label: Option[String])
 *
 * object Point {
 *   implicit val schema: Schema[Point] = DeriveSchema.gen[Point]
 * }
 *
 * val points: Int = Derive.derive[FieldCount, Point](counter).count
 * // 3
 * }}}
 */
trait Deriver[F[_]] extends VersionSpecificDeriver[F] { self =>

  /**
   * A deriver that delegates to this one and stores each instance it builds in a cache, so a later request for the same schema returns the stored instance. The cache identifies a case class or sealed trait by its `TypeId`, which does not include type arguments. Being a `lazy val`, it returns the same cached deriver, and the same cache, on every access.
   */
  lazy val cached: Deriver[F] = {
    val cache = CachedDeriver.createCache[F]
    CachedDeriver.apply(this, cache)
  }

  /**
   * Calls `deriveRecord` when `schema` is a `Schema.Record`, and `deriveUnknown` otherwise. `Derive.derive` calls it for case classes, because the schema in scope may not be a record.
   */
  def tryDeriveRecord[A: ClassTag](
    schema: Schema[A],
    fields: => Chunk[WrappedF[F, _]],
    summoned: => Option[F[A]]
  ): F[A] =
    schema match {
      case record: Schema.Record[A] => deriveRecord(record, fields, summoned)
      case _                        => deriveUnknown(summoned)
    }

  /**
   * Calls `deriveEnum` when `schema` is a `Schema.Enum`, and `deriveUnknown` otherwise. `Derive.derive` calls it for sealed traits, because the schema in scope may not be an enum.
   */
  def tryDeriveEnum[A: ClassTag](
    schema: Schema[A],
    cases: => Chunk[WrappedF[F, _]],
    summoned: => Option[F[A]]
  ): F[A] =
    schema match {
      case e: Schema.Enum[A] => deriveEnum(e, cases, summoned)
      case _                 => deriveUnknown(summoned)
    }

  /**
   * Builds the instance for a case class from its record schema and the instances for its fields, in declaration order. Calling `unwrap` on an element of `fields` gives the instance for that field.
   */
  def deriveRecord[A](record: Schema.Record[A], fields: => Chunk[WrappedF[F, _]], summoned: => Option[F[A]]): F[A]

  /**
   * Builds the instance for a sealed trait (or a Scala 3 enum) from its enum schema and the instances for its cases, in the order of the schema's cases.
   */
  def deriveEnum[A](`enum`: Schema.Enum[A], cases: => Chunk[WrappedF[F, _]], summoned: => Option[F[A]]): F[A]

  /** Builds the instance for a primitive type, such as `String` or `Int`, described by `st`. */
  def derivePrimitive[A](st: StandardType[A], summoned: => Option[F[A]]): F[A]

  /**
   * Builds the instance for a type `A` whose schema is the primitive `st` of an underlying type `U`. `Derive.derive` calls it on Scala 3, for an opaque type whose underlying type is a primitive. By default it calls `deriveUnknown`, which returns the summoned instance and throws a `java.lang.IllegalArgumentException` when there is none.
   */
  @nowarn def derivePrimitiveAlias[A: ClassTag, U](st: StandardType[U], summoned: => Option[F[A]]): F[A] =
    deriveUnknown(summoned)

  /** Builds the instance for `Option[A]` from the instance for `A`. */
  def deriveOption[A](option: Schema.Optional[A], inner: => F[A], summoned: => Option[F[Option[A]]]): F[Option[A]]

  /**
   * Builds the instance for a collection `C[A]`, such as a `List`, `Vector` or `Chunk`, from the instance for its elements.
   */
  def deriveSequence[C[_], A](
    sequence: Schema.Sequence[C[A], A, _],
    inner: => F[A],
    summoned: => Option[F[C[A]]]
  ): F[C[A]]

  /** Builds the instance for `Map[K, V]` from the instances for the keys and the values. */
  def deriveMap[K, V](
    map: Schema.Map[K, V],
    key: => F[K],
    value: => F[V],
    summoned: => Option[F[Map[K, V]]]
  ): F[Map[K, V]]

  /**
   * Builds the instance for `Either[A, B]`. By default it calls `deriveEnum` with `either.toEnum`, whose cases are `Right` and then `Left`, and with the instances in the order `left`, `right`. The first instance in `cases` is therefore the one for `A`, while the first case of the enum is `Right`.
   */
  def deriveEither[A, B](
    either: Schema.Either[A, B],
    left: => F[A],
    right: => F[B],
    summoned: => Option[F[Either[A, B]]]
  ): F[Either[A, B]] =
    deriveEnum(
      either.toEnum,
      Chunk(wrap(left), wrap(right)),
      summoned
    )

  /** Builds the instance for `Set[A]`. By default it calls `deriveSequence` with a sequence schema that converts to and from `Set`. */
  def deriveSet[A](set: Schema.Set[A], inner: => F[A], summoned: => Option[F[Set[A]]]): F[Set[A]] =
    deriveSequence[Set, A](
      Schema.Sequence(set.elementSchema, _.toSet, Chunk.fromIterable, set.annotations, getClass.getName + "#deriveSet"),
      inner,
      summoned
    )

  /**
   * Builds the instance for a tuple `T` from the schema and the instance of each element. By default it calls `deriveRecord` with a record schema that has one field per element.
   */
  def deriveTupleN[T](schemasAndInstances: => Chunk[(Schema[_], WrappedF[F, _])], summoned: => Option[F[T]]): F[T] = {
    val recordSchema = TupleRecordSchema.tupleToRecordSchema[T](schemasAndInstances.map(_._1))
    deriveRecord(
      recordSchema,
      schemasAndInstances.map(_._2),
      summoned
    )
  }

  /**
   * Builds the instance for a case class with more than 22 fields. Its schema is a record of type `A` (a `GenericRecord` on the case class's fields) wrapped in `transform`, which converts it to the case class `B`.
   */
  def deriveTransformedRecord[A, B](
    record: Schema.Record[A],
    transform: Schema.Transform[A, B, _],
    fields: => Chunk[WrappedF[F, _]],
    summoned: => Option[F[B]]
  ): F[B]

  /**
   * Builds the instance for a type that `Derive.derive` does not recognize. By default it returns the summoned instance and throws a `java.lang.IllegalArgumentException` with the message "Cannot derive instance for type " followed by the class name of `A` when there is none.
   */
  def deriveUnknown[A: ClassTag](summoned: => Option[F[A]]): F[A] =
    summoned match {
      case Some(value) => value
      case None        =>
        throw new IllegalArgumentException(
          s"Cannot derive instance for type ${implicitly[ClassTag[A]].runtimeClass.getName}"
        )
    }

  /**
   * Returns a deriver that uses the summoned instance whenever there is one, and calls this deriver's methods only when there is none. It does not override `derivePrimitiveAlias` and `deriveUnknown`, whose default implementations already return the summoned instance.
   */
  def autoAcceptSummoned: Deriver[F] =
    new Deriver[F] {
      final override def deriveRecord[A](
        record: Schema.Record[A],
        fields: => Chunk[WrappedF[F, _]],
        summoned: => Option[F[A]]
      ): F[A] =
        summoned.getOrElse {
          self.deriveRecord(record, fields, summoned)
        }

      final override def deriveEnum[A](
        `enum`: Schema.Enum[A],
        cases: => Chunk[WrappedF[F, _]],
        summoned: => Option[F[A]]
      ): F[A] =
        summoned.getOrElse {
          self.deriveEnum(`enum`, cases, summoned)
        }

      final override def derivePrimitive[A](st: StandardType[A], summoned: => Option[F[A]]): F[A] =
        summoned.getOrElse {
          self.derivePrimitive(st, summoned)
        }

      final override def deriveOption[A](
        option: Schema.Optional[A],
        inner: => F[A],
        summoned: => Option[F[Option[A]]]
      ): F[Option[A]] =
        summoned.getOrElse {
          self.deriveOption(option, inner, summoned)
        }

      final override def deriveSequence[C[_], A](
        sequence: Schema.Sequence[C[A], A, _],
        inner: => F[A],
        summoned: => Option[F[C[A]]]
      ): F[C[A]] =
        summoned.getOrElse {
          self.deriveSequence(sequence, inner, summoned)
        }

      final override def deriveMap[K, V](
        map: Schema.Map[K, V],
        key: => F[K],
        value: => F[V],
        summoned: => Option[F[Map[K, V]]]
      ): F[Map[K, V]] =
        summoned.getOrElse {
          self.deriveMap(map, key, value, summoned)
        }

      final override def deriveEither[A, B](
        either: Schema.Either[A, B],
        left: => F[A],
        right: => F[B],
        summoned: => Option[F[Either[A, B]]]
      ): F[Either[A, B]] =
        summoned.getOrElse {
          self.deriveEither(either, left, right, summoned)
        }

      final override def deriveSet[A](set: Schema.Set[A], inner: => F[A], summoned: => Option[F[Set[A]]]): F[Set[A]] =
        summoned.getOrElse {
          self.deriveSet(set, inner, summoned)
        }

      final override def deriveTransformedRecord[A, B](
        record: Schema.Record[A],
        transform: Schema.Transform[A, B, _],
        fields: => Chunk[WrappedF[F, _]],
        summoned: => Option[F[B]]
      ): F[B] =
        summoned.getOrElse {
          self.deriveTransformedRecord(record, transform, fields, summoned)
        }

      final override def deriveTupleN[T](
        schemasAndInstances: => Chunk[(Schema[_], WrappedF[F, _])],
        summoned: => Option[F[T]]
      ): F[T] =
        summoned.getOrElse {
          self.deriveTupleN(schemasAndInstances, summoned)
        }
    }

}

/** Helpers for `Deriver`: the `WrappedF` wrapper for lazily built instances. */
object Deriver {

  /** A lazily evaluated instance of `F[A]`, which `Derive.derive` uses to pass the instances for fields and cases without building them first. */
  class WrappedF[F[_], A](f: () => F[A]) {
    /** The wrapped instance, built on first access and stored for later ones. */
    lazy val unwrap: F[A] = f()
  }

  /** Wraps the by-name instance `f` in a `WrappedF` without evaluating it. */
  def wrap[F[_], A](f: => F[A]): WrappedF[F, A] = new WrappedF(() => f)
}
