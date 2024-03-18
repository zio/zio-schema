package zio.schema

/**
 * Useful to create factory methods.
 * 
 * import Factory._
 * def createSomeTrait[A: Factory](deriver: Deriver[SomeTrait])(implicit schema: Schema[A]): SomeTrait[A] = 
 *    implicitly[Factory[A]].derive[SomeTrait](deriver)
 * 
 */
trait Factory[A] {
  def derive[F[_]](deriver: Deriver[F])(implicit schema: Schema[A]): F[A]
}

object Factory {

  inline implicit def factory[A]: Factory[A] = new Factory[A] {
    override def derive[F[_]](deriver: Deriver[F])(implicit schema: Schema[A]): F[A] = Derive.derive[F, A](deriver)(schema)
  }
}