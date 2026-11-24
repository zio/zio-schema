package zio.schema

/** Adds the Scala 3 `derive` method to `Deriver`. On Scala 2 it has no members. */
trait VersionSpecificDeriver[F[_]] { self: Deriver[F] =>
  /** Derives the instance `F[A]` with this deriver, the same as `Derive.derive[F, A](this)`. Scala 3 only. */
  inline def derive[A](implicit schema: Schema[A]): F[A] = Derive.derive[F, A](self)(schema)
}
