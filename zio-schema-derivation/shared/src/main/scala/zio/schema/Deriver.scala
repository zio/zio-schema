package zio.schema

import zio.Chunk

trait Deriver[F[_]] {
  def deriveRecord[A](record: Schema.Record[A], fields: => Chunk[F[_]], summoned: => Option[F[A]]): F[A]
  def deriveEnum[A](`enum`: Schema.Enum[A], cases: => Chunk[F[_]], summoned: => Option[F[A]]): F[A]
  def derivePrimitive[A](st: StandardType[A], summoned: => Option[F[A]]): F[A]
  def deriveOption[A](option: Schema.Optional[A], inner: => F[A], summoned: => Option[F[Option[A]]]): F[Option[A]]

  def deriveEither[A, B](
    either: Schema.Either[A, B],
    left: => F[A],
    right: => F[B],
    summoned: Option[F[Either[A, B]]]
  ): F[Either[A, B]]

  def deriveSequence[C[_], A](
    sequence: Schema.Sequence[C[A], A, _],
    inner: => F[A],
    summoned: => Option[F[C[A]]]
  ): F[C[A]]

  def deriveSet[A](set: Schema.Set[A], inner: => F[A], summoned: Option[F[Set[A]]]): F[Set[A]]
  def deriveMap[K, V](map: Schema.Map[K, V], key: => F[K], value: => F[V], summoned: Option[F[Map[K, V]]]): F[Map[K, V]]

  def deriveTuple2[A, B](
    tuple: Schema.Tuple2[A, B],
    left: => F[A],
    right: => F[B],
    summoned: Option[F[(A, B)]]
  ): F[(A, B)]

  def deriveTuple3[A, B, C](
    tuple: Schema.Tuple2[Schema.Tuple2[A, B], C],
    t1: => F[A],
    t2: => F[B],
    t3: => F[C],
    summoned: Option[F[(A, B, C)]]
  ): F[(A, B, C)]

  def deriveTuple4[A, B, C, D](
    tuple: Schema.Tuple2[Schema.Tuple2[Schema.Tuple2[A, B], C], D],
    t1: => F[A],
    t2: => F[B],
    t3: => F[C],
    t4: => F[D],
    summoned: Option[F[(A, B, C, D)]]
  ): F[(A, B, C, D)]

}

// TODO: caching deriver
// TODO: simpler interface for auto-accepting summoned
