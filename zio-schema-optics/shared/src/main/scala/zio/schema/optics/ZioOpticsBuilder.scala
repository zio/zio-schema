package zio.schema.optics

import zio.optics._
import zio.schema.{ AccessorBuilder, _ }
import zio.{ Chunk, ChunkBuilder }

/**
 * This is an example implementation demonstrating how to derive zio-optics using
 * a zio-schema AccessorBuilder.
 *
 * Because we go through DynamicValues, this implementation is not at all performant
 * so shouldn't be used where performance is potentially an issue.
 */
object ZioOpticsBuilder extends AccessorBuilder {

  type Lens[S, A]         = Optic[S, S, A, OpticFailure, OpticFailure, A, S]
  type Prism[S, A]        = ZPrism[S, S, A, A]
  type Traversal[S[_], A] = ZTraversal[S[A], S[A], A, A]

  override def makeLens[S, A](
    product: Schema.Record[S],
    term: Schema.Field[A]
  ): Optic[S, S, A, OpticFailure, OpticFailure, A, S] =
    Optic(
      getOptic = ZioOpticsBuilder.makeLensGet(product, term),
      setOptic = ZioOpticsBuilder.makeLensSet(product, term)
    )

  override def makePrism[S, A](
    sum: Schema.Enum[S],
    term: Schema.Case[A, S]
  ): ZPrism[S, S, A, A] =
    ZPrism(
      get = ZioOpticsBuilder.makePrismGet(term),
      set = (piece: A) => Right(piece.asInstanceOf[S])
    )

  override def makeTraversal[S[_], A](
    collection: Schema.Sequence[S, A],
    element: Schema[A]
  ): Optic[S[A], S[A], Chunk[A], OpticFailure, OpticFailure, Chunk[A], S[A]] =
    ZTraversal(
      ZioOpticsBuilder.makeTraversalGet(collection),
      ZioOpticsBuilder.makeTraversalSet(collection)
    )

  private[optics] def makeLensGet[S, A](
    product: Schema.Record[S],
    term: Schema.Field[A]
  ): S => Either[(OpticFailure, S), A] = { whole: S =>
    product.toDynamic(whole) match {
      case DynamicValue.Record(values) =>
        values
          .get(term.label)
          .map { dynamicField =>
            term.schema.fromDynamic(dynamicField) match {
              case Left(error)  => Left(OpticFailure(error) -> whole)
              case Right(value) => Right(value)
            }
          }
          .getOrElse(Left(OpticFailure(s"No term found with label ${term.label}") -> whole))
      case _ => Left(OpticFailure(s"Unexpected dynamic value for whole") -> whole)
    }
  }

  private[optics] def makeLensSet[S, A](
    product: Schema.Record[S],
    term: Schema.Field[A]
  ): A => S => Either[(OpticFailure, S), S] = { piece: A => whole: S =>
    product.toDynamic(whole) match {
      case DynamicValue.Record(values) =>
        val updated = values.updated(term.label, term.schema.toDynamic(piece))
        product.fromDynamic(DynamicValue.Record(updated)) match {
          case Left(error)  => Left(OpticFailure(error) -> whole)
          case Right(value) => Right(value)
        }
      case _ => Left(OpticFailure(s"Unexpected dynamic value for whole") -> whole)
    }
  }

  private[optics] def makePrismGet[S, A](
    term: Schema.Case[A, S]
  ): S => Either[(OpticFailure, S), A] = { whole: S =>
    term.deconstruct(whole) match {
      case Some(a) => Right(a)
      case None    => Left(OpticFailure(s"Cannot deconstruct to term ${term.id}") -> whole)
    }
  }

  private[optics] def makeTraversalGet[S[_], A](
    collection: Schema.Sequence[S, A]
  ): S[A] => Either[(OpticFailure, S[A]), Chunk[A]] = { whole: S[A] =>
    Right(collection.toChunk(whole))
  }

  private[optics] def makeTraversalSet[S[_], A](
    collection: Schema.Sequence[S, A]
  ): Chunk[A] => S[A] => Either[(OpticFailure, S[A]), S[A]] = { (piece: Chunk[A]) => (whole: S[A]) =>
    val builder       = ChunkBuilder.make[A]()
    val leftIterator  = collection.toChunk(whole).iterator
    val rightIterator = piece.iterator
    while (leftIterator.hasNext && rightIterator.hasNext) {
      val _ = leftIterator.next()
      builder += rightIterator.next()
    }
    while (leftIterator.hasNext) {
      builder += leftIterator.next()
    }
    Right(collection.fromChunk(builder.result()))
  }

}
