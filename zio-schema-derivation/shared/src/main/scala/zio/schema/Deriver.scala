package zio.schema

import scala.reflect.ClassTag

import zio.Chunk
import zio.schema.Deriver.{ WrappedF, wrap }

/** Deriver builds type class instances based on a Schema.
 *
 * The minimum set of methods to implement are:
 *   - `deriveRecord`
 *   - `deriveEnum`
 *   - `derivePrimitive`
 *   - `deriveOption`
 *   - `deriveSequence`
 *   - `deriveMap`
 *   - `deriveTransformedRecord` (for records with more than 22 fields)
 *
 * In addition to this more methods can be overridden to handle all the supported Schema types of zio-schema:
 *   - `deriveEither` (calls `deriveEnum` by default)
 *   - `deriveSet` (calls `deriveSequence` by default)
 *   - `deriveTupleN` (calls `deriveRecord` by default)
 *
 * The `cached` method converts this deriver to one that uses a cache of instances shared between macro invocations.
 *
 * Each derive methods get an optional summoned implicit value of the derived type class. It is the deriver's
 * decision whether to use the available instance (as a user defined customization for a given type) or not.
 *
 * If the decision is to always accept a summoned value if there is any, use the `Deriver.AutoAcceptSummoned` trait
 * which implements this automatically and only calls the trait's methods in case there is no available implicit for
 * the actual type.
 */
trait Deriver[F[_]] extends VersionSpecificDeriver[F] { self =>

  lazy val cached: Deriver[F] = {
    val cache = CachedDeriver.createCache[F]
    CachedDeriver.apply(this, cache)
  }

  def deriveRecord[A](record: Schema.Record[A], fields: => Chunk[WrappedF[F, _]], summoned: => Option[F[A]]): F[A]

  def deriveEnum[A](`enum`: Schema.Enum[A], cases: => Chunk[WrappedF[F, _]], summoned: => Option[F[A]]): F[A]

  def derivePrimitive[A](st: StandardType[A], summoned: => Option[F[A]]): F[A]
  def deriveOption[A](option: Schema.Optional[A], inner: => F[A], summoned: => Option[F[Option[A]]]): F[Option[A]]

  def deriveSequence[C[_], A](
    sequence: Schema.Sequence[C[A], A, _],
    inner: => F[A],
    summoned: => Option[F[C[A]]]
  ): F[C[A]]

  def deriveMap[K, V](
    map: Schema.Map[K, V],
    key: => F[K],
    value: => F[V],
    summoned: => Option[F[Map[K, V]]]
  ): F[Map[K, V]]

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

  def deriveSet[A](set: Schema.Set[A], inner: => F[A], summoned: => Option[F[Set[A]]]): F[Set[A]] =
    deriveSequence[Set, A](
      Schema.Sequence(set.elementSchema, _.toSet, Chunk.fromIterable, set.annotations, getClass.getName + "#deriveSet"),
      inner,
      summoned
    )

  def deriveTupleN[T](schemasAndInstances: => Chunk[(Schema[_], WrappedF[F, _])], summoned: => Option[F[T]]): F[T] = {
    val recordSchema = TupleRecordSchema.tupleToRecordSchema[T](schemasAndInstances.map(_._1))
    deriveRecord(
      recordSchema,
      schemasAndInstances.map(_._2),
      summoned
    )
  }

  def deriveTransformedRecord[A, B](
    record: Schema.Record[A],
    transform: Schema.Transform[A, B, _],
    fields: => Chunk[WrappedF[F, _]],
    summoned: => Option[F[B]]
  ): F[B]

  def deriveUnknown[A: ClassTag](summoned: => Option[F[A]]): F[A] =
    summoned match {
      case Some(value) => value
      case None =>
        throw new IllegalArgumentException(
          s"Cannot derive instance for type ${implicitly[ClassTag[A]].runtimeClass.getName}"
        )
    }

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

object Deriver {

  class WrappedF[F[_], A](f: () => F[A]) {
    lazy val unwrap: F[A] = f()
  }

  def wrap[F[_], A](f: => F[A]): WrappedF[F, A] = new WrappedF(() => f)
}
