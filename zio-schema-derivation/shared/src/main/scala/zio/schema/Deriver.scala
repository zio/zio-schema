package zio.schema

import scala.annotation.nowarn

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
 * If it is more convenient to handle the different tuple types in separate methods, it is possible to override
 * `deriveTuple2` etc. These methods call `deriveTupleN` by default.
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
trait Deriver[F[_]] {

  lazy val cached: (Deriver[F], CachedDeriver.Cache[F]) = {
    val cache = CachedDeriver.createCache[F]
    (CachedDeriver.apply(this, cache), cache)
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
    val arity        = schemasAndInstances.length
    val recordSchema = tupleToRecordSchema[T](arity, schemasAndInstances)
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

  def deriveTuple2[A, B](
    tuple: Schema.Tuple2[A, B],
    left: => F[A],
    right: => F[B],
    summoned: => Option[F[(A, B)]]
  ): F[(A, B)] =
    deriveTupleN[(A, B)](Chunk(tuple.left -> wrap(left), tuple.right -> wrap(right)), summoned)

  @nowarn def deriveTuple3[A, B, C](
    tuple: Schema.Tuple2[Schema.Tuple2[A, B], C],
    transform: Schema.Transform[((A, B), C), (A, B, C), _],
    t1: => F[A],
    t2: => F[B],
    t3: => F[C],
    summoned: => Option[F[(A, B, C)]]
  ): F[(A, B, C)] =
    deriveTupleN[(A, B, C)](
      Chunk(
        tuple.left.asInstanceOf[Schema.Tuple2[A, B]].left  -> wrap(t1),
        tuple.left.asInstanceOf[Schema.Tuple2[A, B]].right -> wrap(t2),
        tuple.right                                        -> wrap(t3)
      ),
      summoned
    )

  @nowarn def deriveTuple4[A, B, C, D](
    tuple: Schema.Tuple2[Schema.Tuple2[Schema.Tuple2[A, B], C], D],
    transform: Schema.Transform[(((A, B), C), D), (A, B, C, D), _],
    t1: => F[A],
    t2: => F[B],
    t3: => F[C],
    t4: => F[D],
    summoned: => Option[F[(A, B, C, D)]]
  ): F[(A, B, C, D)] =
    deriveTupleN[(A, B, C, D)](
      Chunk(
        tuple.left
          .asInstanceOf[Schema.Tuple2[Schema.Tuple2[A, B], C]]
          .left
          .asInstanceOf[Schema.Tuple2[A, B]]
          .left -> wrap(t1),
        tuple.left
          .asInstanceOf[Schema.Tuple2[Schema.Tuple2[A, B], C]]
          .left
          .asInstanceOf[Schema.Tuple2[A, B]]
          .right                                                             -> wrap(t2),
        tuple.left.asInstanceOf[Schema.Tuple2[Schema.Tuple2[A, B], C]].right -> wrap(t3),
        tuple.right                                                          -> wrap(t4)
      ),
      summoned
    )

  protected def tupleToRecordSchema[T](
    arity: Int,
    schemasAndInstances: => Chunk[(Schema[_], WrappedF[F, _])]
  ): Schema.Record[T] =
    arity match {
      case 2 =>
        Schema
          .CaseClass2[Any, Any, (Any, Any)](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemasAndInstances(0)._1.asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any)) => t._1,
              set0 = (t: (Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemasAndInstances(1)._1.asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any)) => t._2,
              set0 = (t: (Any, Any), v: Any) => t.copy(_2 = v)
            ),
            (t1, t2) => (t1, t2)
          )
          .asInstanceOf[Schema.Record[T]]
      case 3 =>
        Schema
          .CaseClass3[Any, Any, Any, (Any, Any, Any)](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemasAndInstances(0)._1.asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemasAndInstances(1)._1.asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemasAndInstances(2)._1.asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            (t1, t2, t3) => (t1, t2, t3)
          )
          .asInstanceOf[Schema.Record[T]]
      case 4 =>
        Schema
          .CaseClass4[Any, Any, Any, Any, (Any, Any, Any, Any)](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemasAndInstances(0)._1.asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemasAndInstances(1)._1.asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemasAndInstances(2)._1.asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemasAndInstances(3)._1.asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any), v: Any) => t.copy(_4 = v)
            ),
            (t1, t2, t3, t4) => (t1, t2, t3, t4)
          )
          .asInstanceOf[Schema.Record[T]]
      case _ => throw new IllegalArgumentException(s"Unsupported tuple arity: $arity")
    }
}

object Deriver {

  trait AutoAcceptSummoned[F[_]] extends Deriver[F] {
    def deriveRecord[A](record: Schema.Record[A], fields: => Chunk[WrappedF[F, _]]): F[A]
    def deriveEnum[A](`enum`: Schema.Enum[A], cases: => Chunk[WrappedF[F, _]]): F[A]
    def derivePrimitive[A](st: StandardType[A]): F[A]
    def deriveOption[A](option: Schema.Optional[A], inner: => F[A]): F[Option[A]]
    def deriveSequence[C[_], A](sequence: Schema.Sequence[C[A], A, _], inner: => F[A]): F[C[A]]
    def deriveMap[K, V](map: Schema.Map[K, V], key: => F[K], value: => F[V]): F[Map[K, V]]

    def deriveEither[A, B](either: Schema.Either[A, B], left: => F[A], right: => F[B]): F[Either[A, B]] =
      deriveEnum(either.toEnum, Chunk(wrap(left), wrap(right)))

    def deriveSet[A](set: Schema.Set[A], inner: => F[A]): F[Set[A]] =
      deriveSequence[Set, A](
        Schema
          .Sequence(
            set.elementSchema,
            (chunk: Chunk[A]) => chunk.toSet,
            (set: Set[A]) => Chunk.fromIterable(set),
            set.annotations,
            getClass.getName + "#deriveSet"
          ),
        inner
      )

    def deriveTuple2[A, B](tuple: Schema.Tuple2[A, B], left: => F[A], right: => F[B]): F[(A, B)] =
      deriveTupleN[(A, B)](Chunk(tuple.left -> wrap(left), tuple.right -> wrap(right)))

    @nowarn def deriveTuple3[A, B, C](
      tuple: Schema.Tuple2[Schema.Tuple2[A, B], C],
      transform: Schema.Transform[((A, B), C), (A, B, C), _],
      t1: => F[A],
      t2: => F[B],
      t3: => F[C]
    ): F[(A, B, C)] =
      deriveTupleN[(A, B, C)](
        Chunk(
          tuple.left.asInstanceOf[Schema.Tuple2[A, B]].left  -> wrap(t1),
          tuple.left.asInstanceOf[Schema.Tuple2[A, B]].right -> wrap(t2),
          tuple.right                                        -> wrap(t3)
        )
      )

    @nowarn def deriveTuple4[A, B, C, D](
      tuple: Schema.Tuple2[Schema.Tuple2[Schema.Tuple2[A, B], C], D],
      transform: Schema.Transform[(((A, B), C), D), (A, B, C, D), _],
      t1: => F[A],
      t2: => F[B],
      t3: => F[C],
      t4: => F[D]
    ): F[(A, B, C, D)] =
      deriveTupleN[(A, B, C, D)](
        Chunk(
          tuple.left
            .asInstanceOf[Schema.Tuple2[Schema.Tuple2[A, B], C]]
            .left
            .asInstanceOf[Schema.Tuple2[A, B]]
            .left -> wrap(t1),
          tuple.left
            .asInstanceOf[Schema.Tuple2[Schema.Tuple2[A, B], C]]
            .left
            .asInstanceOf[Schema.Tuple2[A, B]]
            .right                                                             -> wrap(t2),
          tuple.left.asInstanceOf[Schema.Tuple2[Schema.Tuple2[A, B], C]].right -> wrap(t3),
          tuple.right                                                          -> wrap(t4)
        )
      )

    def deriveTupleN[T](schemasAndInstances: => Chunk[(Schema[_], WrappedF[F, _])]): F[T] = {
      val arity        = schemasAndInstances.length
      val recordSchema = tupleToRecordSchema[T](arity, schemasAndInstances)
      deriveRecord(
        recordSchema,
        schemasAndInstances.map(_._2)
      )
    }

    def deriveTransformedRecord[A, B](
      record: Schema.Record[A],
      transform: Schema.Transform[A, B, _],
      fields: => Chunk[WrappedF[F, _]]
    ): F[B]

    final override def deriveRecord[A](
      record: Schema.Record[A],
      fields: => Chunk[WrappedF[F, _]],
      summoned: => Option[F[A]]
    ): F[A] =
      summoned.getOrElse {
        deriveRecord(record, fields)
      }

    final override def deriveEnum[A](
      `enum`: Schema.Enum[A],
      cases: => Chunk[WrappedF[F, _]],
      summoned: => Option[F[A]]
    ): F[A] =
      summoned.getOrElse {
        deriveEnum(`enum`, cases)
      }

    final override def derivePrimitive[A](st: StandardType[A], summoned: => Option[F[A]]): F[A] =
      summoned.getOrElse {
        derivePrimitive(st)
      }

    final override def deriveOption[A](
      option: Schema.Optional[A],
      inner: => F[A],
      summoned: => Option[F[Option[A]]]
    ): F[Option[A]] =
      summoned.getOrElse {
        deriveOption(option, inner)
      }

    final override def deriveSequence[C[_], A](
      sequence: Schema.Sequence[C[A], A, _],
      inner: => F[A],
      summoned: => Option[F[C[A]]]
    ): F[C[A]] =
      summoned.getOrElse {
        deriveSequence(sequence, inner)
      }

    final override def deriveMap[K, V](
      map: Schema.Map[K, V],
      key: => F[K],
      value: => F[V],
      summoned: => Option[F[Map[K, V]]]
    ): F[Map[K, V]] =
      summoned.getOrElse {
        deriveMap(map, key, value)
      }

    final override def deriveEither[A, B](
      either: Schema.Either[A, B],
      left: => F[A],
      right: => F[B],
      summoned: => Option[F[Either[A, B]]]
    ): F[Either[A, B]] =
      summoned.getOrElse {
        deriveEither(either, left, right)
      }

    final override def deriveSet[A](set: Schema.Set[A], inner: => F[A], summoned: => Option[F[Set[A]]]): F[Set[A]] =
      summoned.getOrElse {
        deriveSet(set, inner)
      }

    final override def deriveTransformedRecord[A, B](
      record: Schema.Record[A],
      transform: Schema.Transform[A, B, _],
      fields: => Chunk[WrappedF[F, _]],
      summoned: => Option[F[B]]
    ): F[B] =
      summoned.getOrElse {
        deriveTransformedRecord(record, transform, fields)
      }

    final override def deriveTuple2[A, B](
      tuple: Schema.Tuple2[A, B],
      left: => F[A],
      right: => F[B],
      summoned: => Option[F[(A, B)]]
    ): F[(A, B)] =
      summoned.getOrElse {
        deriveTuple2(tuple, left, right)
      }

    final override def deriveTuple3[A, B, C](
      tuple: Schema.Tuple2[Schema.Tuple2[A, B], C],
      transform: Schema.Transform[((A, B), C), (A, B, C), _],
      t1: => F[A],
      t2: => F[B],
      t3: => F[C],
      summoned: => Option[F[(A, B, C)]]
    ): F[(A, B, C)] =
      summoned.getOrElse {
        deriveTuple3(tuple, transform, t1, t2, t3)
      }

    final override def deriveTuple4[A, B, C, D](
      tuple: Schema.Tuple2[Schema.Tuple2[Schema.Tuple2[A, B], C], D],
      transform: Schema.Transform[(((A, B), C), D), (A, B, C, D), _],
      t1: => F[A],
      t2: => F[B],
      t3: => F[C],
      t4: => F[D],
      summoned: => Option[F[(A, B, C, D)]]
    ): F[(A, B, C, D)] =
      summoned.getOrElse {
        deriveTuple4(tuple, transform, t1, t2, t3, t4)
      }

    final override def deriveTupleN[T](
      schemasAndInstances: => Chunk[(Schema[_], WrappedF[F, _])],
      summoned: => Option[F[T]]
    ): F[T] =
      summoned.getOrElse {
        deriveTupleN(schemasAndInstances)
      }
  }

  class WrappedF[F[_], A](f: () => F[A]) {
    lazy val unwrap: F[A] = f()
  }

  def wrap[F[_], A](f: => F[A]): WrappedF[F, A] = new WrappedF(() => f)
}
