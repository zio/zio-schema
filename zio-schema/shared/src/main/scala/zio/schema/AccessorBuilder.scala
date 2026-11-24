package zio.schema

/**
 * Builds optics (lenses, prisms and traversals) for the parts of a schema, in
 * whatever optics library the implementation targets. `Schema#makeAccessors`
 * calls it with each field of a record, each case of an enum and the element of
 * a collection, and returns the results as the schema's `Accessors` type. The
 * `zio-schema-optics` module provides `ZioOpticsBuilder` for zio-optics.
 */
trait AccessorBuilder {

  /**
   * The lens type, focusing on field `A` of record `S`; `F` is the field's name
   * as a singleton type.
   */
  type Lens[F, S, A]

  /**
   * The prism type, focusing on case `A` of enum `S`; `F` is the case's name as
   * a singleton type.
   */
  type Prism[F, S, A]

  /**
   * The traversal type, focusing on the elements `A` of collection `S`.
   */
  type Traversal[S, A]

  // TODO should we restrict F <: Singleton with String
  /**
   * Returns a lens that gets and sets the field `term` of the record described
   * by `product`.
   */
  def makeLens[F, S, A](product: Schema.Record[S], term: Schema.Field[S, A]): Lens[F, S, A]

  /**
   * Returns a prism that selects the case `term` of the enum described by
   * `sum`.
   */
  def makePrism[F, S, A](sum: Schema.Enum[S], term: Schema.Case[S, A]): Prism[F, S, A]

  /**
   * Returns a traversal over the elements of the collection described by
   * `collection`, whose elements have the schema `element`.
   */
  def makeTraversal[S, A](collection: Schema.Collection[S, A], element: Schema[A]): Traversal[S, A]
}
