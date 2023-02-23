package zio.schema

import java.util.concurrent.{ ConcurrentHashMap, ConcurrentMap }

import scala.reflect.ClassTag

import zio.Chunk
import zio.schema.CachedDeriver.{ Cache, CacheKey }
import zio.schema.Deriver.WrappedF

private[schema] class CachedDeriver[F[_]] private (deriver: Deriver[F], val cache: Cache[F]) extends Deriver[F] {

  override def deriveRecord[A](
    record: Schema.Record[A],
    fields: => Chunk[WrappedF[F, _]],
    summoned: => Option[F[A]]
  ): F[A] =
    cached(record) {
      deriver.deriveRecord(record, fields, summoned)
    }

  override def deriveEnum[A](`enum`: Schema.Enum[A], cases: => Chunk[WrappedF[F, _]], summoned: => Option[F[A]]): F[A] =
    cached(`enum`) {
      deriver.deriveEnum(`enum`, cases, summoned)
    }

  override def derivePrimitive[A](st: StandardType[A], summoned: => Option[F[A]]): F[A] =
    cached(Schema.primitive(st)) {
      deriver.derivePrimitive(st, summoned)
    }

  override def derivePrimitiveAlias[A: ClassTag, U](st: StandardType[U], summoned: => Option[F[A]]): F[A] = {
    val key = CacheKey.PrimitiveAlias(st, implicitly[ClassTag[A]])
    cache.get(key) match {
      case None    => cache.put(key, deriver.derivePrimitiveAlias[A, U](st, summoned))
      case Some(g) => g
    }
  }

  override def deriveOption[A](
    option: Schema.Optional[A],
    inner: => F[A],
    summoned: => Option[F[Option[A]]]
  ): F[Option[A]] =
    cached(option) {
      deriver.deriveOption(option, inner, summoned)
    }

  override def deriveSequence[C[_], A](
    sequence: Schema.Sequence[C[A], A, _],
    inner: => F[A],
    summoned: => Option[F[C[A]]]
  ): F[C[A]] =
    cached(sequence) {
      deriver.deriveSequence(sequence, inner, summoned)
    }

  override def deriveMap[K, V](
    map: Schema.Map[K, V],
    key: => F[K],
    value: => F[V],
    summoned: => Option[F[Map[K, V]]]
  ): F[Map[K, V]] =
    cached(map) {
      deriver.deriveMap(map, key, value, summoned)
    }

  override def deriveEither[A, B](
    either: Schema.Either[A, B],
    left: => F[A],
    right: => F[B],
    summoned: => Option[F[Either[A, B]]]
  ): F[Either[A, B]] =
    cached(either) {
      deriver.deriveEither(either, left, right, summoned)
    }

  override def deriveSet[A](set: Schema.Set[A], inner: => F[A], summoned: => Option[F[Set[A]]]): F[Set[A]] =
    cached(set) {
      deriver.deriveSet(set, inner, summoned)
    }

  override def deriveTransformedRecord[A, B](
    record: Schema.Record[A],
    transform: Schema.Transform[A, B, _],
    fields: => Chunk[WrappedF[F, _]],
    summoned: => Option[F[B]]
  ): F[B] =
    cached(transform) {
      deriver.deriveTransformedRecord(record, transform, fields, summoned)
    }

  override def deriveTupleN[T](
    schemasAndInstances: => Chunk[(Schema[_], WrappedF[F, _])],
    summoned: => Option[F[T]]
  ): F[T] = {
    val zipped = zippedTupleSchema(schemasAndInstances.map(_._1))
    cached(zipped.asInstanceOf[Schema[T]]) {
      deriver.deriveTupleN(schemasAndInstances, summoned)
    }
  }

  private def cached[A](schema: Schema[A])(f: => F[A]): F[A] = {
    val key = CacheKey.fromSchema(schema)
    cache.get(key) match {
      case None    => cache.put(key, f)
      case Some(g) => g
    }
  }

  private def zippedTupleSchema(schemas: Chunk[Schema[_]]): Schema[_] =
    schemas.reduceLeft((a, b) => a.zip(b))
}

private[schema] object CachedDeriver {
  sealed trait CacheKey[A]

  object CacheKey {
    final case class Primitive[A](standardType: StandardType[A])                                extends CacheKey[A]
    final case class PrimitiveAlias[A, U](standardType: StandardType[U], classTag: ClassTag[A]) extends CacheKey[A]
    final case class WithId[A](typeId: TypeId)                                                  extends CacheKey[A]
    final case class WithIdentityObject[A](id: Any)                                             extends CacheKey[A]
    final case class Optional[A](key: CacheKey[A])                                              extends CacheKey[A]
    final case class Either[A, B](leftKey: CacheKey[A], rightKey: CacheKey[B])                  extends CacheKey[Either[A, B]]
    final case class Tuple2[A, B](leftKey: CacheKey[A], rightKey: CacheKey[B])                  extends CacheKey[(A, B)]
    final case class Set[A](element: CacheKey[A])                                               extends CacheKey[Set[A]]
    final case class Map[K, V](key: CacheKey[K], valuew: CacheKey[V])                           extends CacheKey[Map[K, V]]
    final case class Misc[A](schema: Schema[A])                                                 extends CacheKey[A]

    def fromStandardType[A](st: StandardType[A]): CacheKey[A] = Primitive(st)

    def fromSchema[A](schema: Schema[A]): CacheKey[A] =
      schema match {
        case e: Schema.Enum[_]             => WithId(e.id)
        case record: Schema.Record[_]      => WithId(record.id)
        case seq: Schema.Sequence[_, _, _] => WithIdentityObject(seq.identity)
        case set: Schema.Set[_]            => Set(fromSchema(set.elementSchema)).asInstanceOf[CacheKey[A]]
        case map: Schema.Map[_, _] =>
          Map(fromSchema(map.keySchema), fromSchema(map.valueSchema)).asInstanceOf[CacheKey[A]]
        case Schema.Transform(_, _, _, _, identity) => WithIdentityObject(identity)
        case Schema.Primitive(standardType, _)      => fromStandardType(standardType)
        case optional: Schema.Optional[_]           => Optional(fromSchema(optional.schema)).asInstanceOf[CacheKey[A]]
        case tuple: Schema.Tuple2[_, _] =>
          Tuple2(fromSchema(tuple.left), fromSchema(tuple.right)).asInstanceOf[CacheKey[A]]
        case either: Schema.Either[_, _] =>
          Either(fromSchema(either.leftSchema), fromSchema(either.rightSchema)).asInstanceOf[CacheKey[A]]
        case Schema.Lazy(schema0) => fromSchema(schema0())
        case Schema.Dynamic(_)    => Misc(schema)
        case Schema.Fail(_, _)    => Misc(schema)
      }
  }

  class Cache[F[_]] private[CachedDeriver] (map: ConcurrentMap[CacheKey[_], Any]) {

    def get[A](schema: CacheKey[A]): Option[F[A]] =
      Option(map.get(schema)).map(_.asInstanceOf[F[A]])

    def put[A](schema: CacheKey[A], value: F[A]): F[A] = {
      map.put(schema, value)
      value
    }

    def size: Int = map.size()
  }

  private[schema] def apply[F[_]](deriver: Deriver[F], cache: Cache[F]): Deriver[F] =
    new CachedDeriver[F](deriver, cache)

  def createCache[F[_]]: Cache[F] =
    new Cache[F](new ConcurrentHashMap[CacheKey[_], Any]())
}
