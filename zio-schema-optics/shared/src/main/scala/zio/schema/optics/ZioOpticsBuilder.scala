package zio.schema.optics

import scala.collection.immutable.ListMap

import zio.optics._
import zio.schema._
import zio.{ Chunk, ChunkBuilder }

/**
 * This is an example implementation demonstrating how to derive zio-optics using
 * a zio-schema AccessorBuilder.
 *
 * Because we go through DynamicValues, this implementation is not at all performant
 * so shouldn't be used where performance is potentially an issue.
 */
object ZioOpticsBuilder extends AccessorBuilder {

  type Lens[F, S, A]   = Optic[S, S, A, OpticFailure, OpticFailure, A, S]
  type Prism[F, S, A]  = ZPrism[S, S, A, A]
  type Traversal[S, A] = ZTraversal[S, S, A, A]

  override def makeLens[F, S, A](
    product: Schema.Record[S],
    term: Schema.Field[S, A]
  ): Optic[S, S, A, OpticFailure, OpticFailure, A, S] =
    Optic(
      getOptic = ZioOpticsBuilder.makeLensGet(product, term),
      setOptic = ZioOpticsBuilder.makeLensSet(product, term)
    )

  override def makePrism[F, S, A](
    sum: Schema.Enum[S],
    term: Schema.Case[S, A]
  ): ZPrism[S, S, A, A] =
    ZPrism(
      get = ZioOpticsBuilder.makePrismGet(term),
      set = (piece: A) => zio.prelude.Validation.succeed(piece.asInstanceOf[S])
    )

  override def makeTraversal[S, A](
    collection: Schema.Collection[S, A],
    element: Schema[A]
  ): Optic[S, S, Chunk[A], OpticFailure, OpticFailure, Chunk[A], S] =
    collection match {
      case seq @ Schema.Sequence(_, _, _, _, _) =>
        ZTraversal[S, S, A, A](
          ZioOpticsBuilder.makeSeqTraversalGet(seq),
          ZioOpticsBuilder.makeSeqTraversalSet(seq)
        )
      case Schema.Map(_: Schema[k], _: Schema[v], _) =>
        ZTraversal(
          ZioOpticsBuilder.makeMapTraversalGet[k, v],
          ZioOpticsBuilder.makeMapTraversalSet[k, v]
        )
      case Schema.Set(_, _) =>
        ZTraversal(
          ZioOpticsBuilder.makeSetTraversalGet[A],
          ZioOpticsBuilder.makeSetTraversalSet[A]
        )
    }

  private[optics] def makeLensGet[S, A](
    product: Schema.Record[S],
    term: Schema.Field[S, A]
  ): S => zio.prelude.Validation[(OpticFailure, S), A] = { (whole: S) =>
    product.toDynamic(whole) match {
      case DynamicValue.Record(_, values) =>
        values
          .get(term.name)
          .map { dynamicField =>
            term.schema.fromDynamic(dynamicField) match {
              case Left(error)  => Left(OpticFailure(error) -> whole)
              case zio.prelude.Validation.succeed(value) => zio.prelude.Validation.succeed(value)
            }
          }
          .getOrElse(Left(OpticFailure(s"No term found with label ${term.name}") -> whole))
      case _ => Left(OpticFailure(s"Unexpected dynamic value for whole") -> whole)
    }
  }

  private[optics] def makeLensSet[S, A](
    product: Schema.Record[S],
    term: Schema.Field[S, A]
  ): A => S => zio.prelude.Validation[(OpticFailure, S), S] = { (piece: A) => (whole: S) =>
    product.toDynamic(whole) match {
      case DynamicValue.Record(name, values) =>
        val updated = spliceRecord(values, term.name, term.name -> term.schema.toDynamic(piece))
        product.fromDynamic(DynamicValue.Record(name, updated)) match {
          case Left(error)  => Left(OpticFailure(error) -> whole)
          case zio.prelude.Validation.succeed(value) => zio.prelude.Validation.succeed(value)
        }
      case _ => Left(OpticFailure(s"Unexpected dynamic value for whole") -> whole)
    }
  }

  private[optics] def makePrismGet[S, A](
    term: Schema.Case[S, A]
  ): S => zio.prelude.Validation[(OpticFailure, S), A] = { (whole: S) =>
    term.deconstructOption(whole) match {
      case Some(a) => zio.prelude.Validation.succeed(a)
      case None    => Left(OpticFailure(s"Cannot deconstruct to term ${term.id}") -> whole)
    }
  }

  private[optics] def makeSeqTraversalGet[S, A](
    collection: Schema.Sequence[S, A, _]
  ): S => zio.prelude.Validation[(OpticFailure, S), Chunk[A]] = { (whole: S) =>
    zio.prelude.Validation.succeed(collection.toChunk(whole))
  }

  private[optics] def makeSeqTraversalSet[S, A](
    collection: Schema.Sequence[S, A, _]
  ): Chunk[A] => S => zio.prelude.Validation[(OpticFailure, S), S] = { (piece: Chunk[A]) => (whole: S) =>
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
    zio.prelude.Validation.succeed(collection.fromChunk(builder.result()))
  }

  private[optics] def makeMapTraversalGet[K, V](whole: Map[K, V]): zio.prelude.Validation[(OpticFailure, Map[K, V]), Chunk[(K, V)]] =
    zio.prelude.Validation.succeed(Chunk.fromIterable(whole))

  private[optics] def makeMapTraversalSet[K, V]
    : Chunk[(K, V)] => Map[K, V] => zio.prelude.Validation[(OpticFailure, Map[K, V]), Map[K, V]] = {
    (piece: Chunk[(K, V)]) => (whole: Map[K, V]) =>
      zio.prelude.Validation.succeed(whole ++ piece.toList)
  }

  private[optics] def makeSetTraversalGet[A](whole: Set[A]): zio.prelude.Validation[(OpticFailure, Set[A]), Chunk[A]] =
    zio.prelude.Validation.succeed(Chunk.fromIterable(whole))

  private[optics] def makeSetTraversalSet[A]: Chunk[A] => Set[A] => zio.prelude.Validation[(OpticFailure, Set[A]), Set[A]] = {
    (piece: Chunk[A]) => (whole: Set[A]) =>
      zio.prelude.Validation.succeed(whole ++ piece.toSet)
  }

  private def spliceRecord(
    fields: ListMap[String, DynamicValue],
    label: String,
    splicedField: (String, DynamicValue)
  ): ListMap[String, DynamicValue] =
    fields.foldLeft[ListMap[String, DynamicValue]](ListMap.empty) {
      case (acc, (nextLabel, _)) if nextLabel == label => acc + splicedField
      case (acc, nextField)                            => acc + nextField
    }

}
