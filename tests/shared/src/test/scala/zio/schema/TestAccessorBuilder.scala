package zio.schema
import zio.schema

class TestAccessorBuilder extends AccessorBuilder {
  override type Lens[S, A]      = TestAccessorBuilder.Lens[S, A]
  override type Prism[S, A]     = TestAccessorBuilder.Prism[S, A]
  override type Traversal[S, A] = TestAccessorBuilder.Traversal[S, A]

  override def makeLens[S, A](product: Schema.Record[S], term: schema.Schema.Field[A]): Lens[S, A] =
    TestAccessorBuilder.Lens(product, term)

  override def makePrism[S, A](sum: Schema.Enum[S], term: schema.Schema.Case[A, S]): Prism[S, A] =
    TestAccessorBuilder.Prism(sum, term)

  override def makeTraversal[S, A](collection: Schema.Collection[S, A], element: Schema[A]): Traversal[S, A] =
    TestAccessorBuilder.Traversal(collection, element)
}

object TestAccessorBuilder {

  case class Lens[S, A](s: Schema.Record[S], a: Schema.Field[A])

  case class Prism[S, A](s: Schema.Enum[S], a: Schema.Case[A, S])

  case class Traversal[S, A](s: Schema.Collection[S, A], a: Schema[A])
}
