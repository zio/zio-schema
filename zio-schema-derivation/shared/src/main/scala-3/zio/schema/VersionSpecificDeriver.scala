package zio.schema

trait VersionSpecificDeriver[F[_]] { self: Deriver[F] =>
  inline def derive[A](implicit schema: Schema[A]): F[A] = Derive.derive[F, A](self)(schema)
}
