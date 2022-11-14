package zio.schema

import zio.Chunk

trait Deriver[F[_]] {
  lazy val cached: (Deriver[F], CachedDeriver.Cache[F]) = {
    val cache = CachedDeriver.createCache[F]
    (CachedDeriver.apply(this, cache), cache)
  }

  def deriveRecord[A](record: Schema.Record[A], fields: => Chunk[F[_]], summoned: => Option[F[A]]): F[A]

  def deriveEnum[A](`enum`: Schema.Enum[A], cases: => Chunk[F[_]], summoned: => Option[F[A]]): F[A]

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
      Chunk(left, right),
      summoned
    )

  def deriveSet[A](set: Schema.Set[A], inner: => F[A], summoned: => Option[F[Set[A]]]): F[Set[A]] =
    deriveSequence[Set, A](
      Schema.Sequence(set.elementSchema, _.toSet, Chunk.fromIterable, set.annotations, getClass.getName + "#deriveSet"),
      inner,
      summoned
    )

  def deriveTupleN[T](schemasAndInstances: => Chunk[(Schema[_], F[_])], summoned: => Option[F[T]]): F[T] = {
    val arity = schemasAndInstances.length
    val recordSchema: Schema.Record[T] =
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
    deriveRecord(
      recordSchema,
      schemasAndInstances.map(_._2),
      summoned
    )
  }

  def deriveTransformedRecord[A, B](
    record: Schema.Record[A],
    transform: Schema.Transform[A, B, _],
    fields: => Chunk[F[_]],
    summoned: => Option[F[B]]
  ): F[B]

  def deriveTuple2[A, B](
    tuple: Schema.Tuple2[A, B],
    left: => F[A],
    right: => F[B],
    summoned: => Option[F[(A, B)]]
  ): F[(A, B)] =
    deriveTupleN[(A, B)](Chunk(tuple.left -> left, tuple.right -> right), summoned)

  def deriveTuple3[A, B, C](
    tuple: Schema.Tuple2[Schema.Tuple2[A, B], C],
    transform: Schema.Transform[((A, B), C), (A, B, C), _],
    t1: => F[A],
    t2: => F[B],
    t3: => F[C],
    summoned: => Option[F[(A, B, C)]]
  ): F[(A, B, C)] =
    deriveTupleN[(A, B, C)](
      Chunk(
        tuple.left.asInstanceOf[Schema.Tuple2[A, B]].left  -> t1,
        tuple.left.asInstanceOf[Schema.Tuple2[A, B]].right -> t2,
        tuple.right                                        -> t3
      ),
      summoned
    )

  def deriveTuple4[A, B, C, D](
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
          .left -> t1,
        tuple.left
          .asInstanceOf[Schema.Tuple2[Schema.Tuple2[A, B], C]]
          .left
          .asInstanceOf[Schema.Tuple2[A, B]]
          .right                                                             -> t2,
        tuple.left.asInstanceOf[Schema.Tuple2[Schema.Tuple2[A, B], C]].right -> t3,
        tuple.right                                                          -> t4
      ),
      summoned
    )
}

// TODO: caching deriver
// TODO: simpler interface for auto-accepting summoned
