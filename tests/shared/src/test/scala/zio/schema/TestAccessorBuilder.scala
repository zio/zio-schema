package zio.schema
import zio.schema

class TestAccessorBuilder extends AccessorBuilder {
  override type Lens[F, S, A]   = TestAccessorBuilder.Lens[F, S, A]
  override type Prism[F, S, A]  = TestAccessorBuilder.Prism[F, S, A]
  override type Traversal[S, A] = TestAccessorBuilder.Traversal[S, A]

  override def makeLens[F, S, A](product: Schema.Record[S], term: schema.Schema.Field[A]): Lens[F, S, A] =
    TestAccessorBuilder.Lens(product, term)

  override def makePrism[F, S, A](sum: Schema.Enum[S], term: schema.Schema.Case[A, S]): Prism[F, S, A] =
    TestAccessorBuilder.Prism(sum, term)

  override def makeTraversal[S, A](collection: Schema.Collection[S, A], element: Schema[A]): Traversal[S, A] =
    TestAccessorBuilder.Traversal(collection, element)
}

object TestAccessorBuilder {

  case class Lens[F, S, A](s: Schema.Record[S], a: Schema.Field[A])

  case class Prism[F, S, A](s: Schema.Enum[S], a: Schema.Case[A, S])

  case class Traversal[S, A](s: Schema.Collection[S, A], a: Schema[A])
}
