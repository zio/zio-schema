package zio.schema

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

/**
 * A type class that derives an instance of any type class for `A` with a `Deriver`. `Derive.derive` is a macro, so it needs a concrete type where it is called. A generic method that requires a `Factory[A]`, for example as a context bound, moves that call to its caller, where the compiler derives the `Factory[A]` for the concrete type.
 *
 * On Scala 2, the code that the `Factory` macro generates refers to `Factory`, `Deriver`, `Schema` and `Derive` by their short names, so the place where a `Factory` is derived needs `import zio.schema._`.
 *
 * {{{
 * trait Show[A] { def show(a: A): String }
 *
 * def deriveShow[A: Factory: Schema](deriver: Deriver[Show]): Show[A] =
 *   implicitly[Factory[A]].derive[Show](deriver)
 * }}}
 */
trait Factory[A] {
  /** Derives the instance `F[A]` with `deriver`, by calling `Derive.derive[F, A]`. */
  def derive[F[_]](deriver: Deriver[F])(implicit schema: Schema[A]): F[A]
}

/** Provides the implicit `Factory[A]`, which the compiler finds without an import. */
object Factory {

  /** Builds a `Factory[A]` for the concrete type `A`. On Scala 2 it is a whitebox macro; on Scala 3 it is an inline method. */
  implicit def factory[A]: Factory[A] = macro factoryImpl[A]

  /** The Scala 2 macro implementation of `factory`. */
  def factoryImpl[A: c.WeakTypeTag](
    c: whitebox.Context
  ): c.Tree = {
    import c.universe._

    val tpeA = weakTypeOf[A]

    q"""
        new Factory[$tpeA] {
            override def derive[F[_]](deriver: Deriver[F])(implicit schema: Schema[$tpeA]): F[$tpeA] = Derive.derive[F, $tpeA](deriver)
                
        }
    
    """
  }
}
