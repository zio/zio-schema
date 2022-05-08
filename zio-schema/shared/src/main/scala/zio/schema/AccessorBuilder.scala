package zio.schema

trait AccessorBuilder {
  type Lens[F, S, A]
  type Prism[S, A]
  type Traversal[S, A]

  def makeLens[F, S, A](product: Schema.Record[S], term: Schema.Field[A]): Lens[F, S, A]

  def makePrism[S, A](sum: Schema.Enum[S], term: Schema.Case[A, S]): Prism[S, A]

  def makeTraversal[S, A](collection: Schema.Collection[S, A], element: Schema[A]): Traversal[S, A]
}
