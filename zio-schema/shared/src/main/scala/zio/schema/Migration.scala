package zio.schema

import zio.{ Chunk, ChunkBuilder }

sealed trait Migration { self =>

  def migrate(value: DynamicValue): Either[String, DynamicValue] =
    self match {
      case Migration.Require(path)  => Migration.tryRequire(value, path.toList)
      case Migration.Optional(path) => Migration.tryMakeOptional(value, path.toList)
      case Migration.ChangeType(path, _) =>
        Left(
          s"Cannot change type of node at path ${Migration.renderPath(path)}: No type conversion is available"
        )
      case Migration.DeleteNode(path) => Migration.tryDeleteNode(value, path.toList)
      case Migration.AddNode(path, _) =>
        Left(s"Cannot add node at path ${Migration.renderPath(path)}: No default value is available")
      case Migration.Relabel(path, transform) => Migration.tryRelabel(value, path.toList, transform)
      case Migration.IncrementDimensions(path, n) =>
        Migration.tryIncrementDimension(value, path.toList, n)
      case Migration.DecrementDimensions(path, n) =>
        Migration.tryDecrementDimensions(value, path.toList, n)
      case Migration.UpdateFail(path, newMessage) =>
        Migration.tryUpdateFail(value, path.toList, newMessage)
      case Migration.Recursive(_, _) => ???
    }
}

object Migration {
  final case class UpdateFail(path: Chunk[String], message: String) extends Migration

  final case class Optional(path: Chunk[String]) extends Migration

  final case class Require(path: Chunk[String]) extends Migration

  final case class ChangeType(path: Chunk[String], value: StandardType[_]) extends Migration

  final case class AddNode(path: Chunk[String], node: SchemaAst) extends Migration

  final case class DeleteNode(path: Chunk[String]) extends Migration

  final case class Relabel(path: Chunk[String], tranform: LabelTransformation) extends Migration

  final case class IncrementDimensions(path: Chunk[String], n: Int) extends Migration

  final case class DecrementDimensions(path: Chunk[String], n: Int) extends Migration

  final case class Recursive(refPath: Chunk[String], migrations: Chunk[Migration]) extends Migration

  def derive(from: SchemaAst, to: SchemaAst): Either[String, Chunk[Migration]] = {
    def go(
      acc: Chunk[Migration],
      path: Chunk[String],
      fromSubtree: SchemaAst,
      toSubtree: SchemaAst
    ): Either[String, Chunk[Migration]] = (fromSubtree, toSubtree) match {
      case (f: SchemaAst.FailNode, t: SchemaAst.FailNode) =>
        Right(
          if (f.message == t.message)
            acc
          else
            acc ++ transformShape(path, f, t) :+ UpdateFail(path, t.message)
        )
      case (f @ SchemaAst.Product(_, ffields, _, _), t @ SchemaAst.Product(_, tfields, _, _)) =>
        matchedSubtrees(ffields, tfields).map {
          case ((nextPath, fs), (_, ts)) => go(Chunk.empty, path :+ nextPath, fs, ts)
        }.reduce[Either[String, Chunk[Migration]]] {
            case (err @ Left(_), Right(_)) => err
            case (Right(_), err @ Left(_)) => err
            case (Left(e1), Left(e2))      => Left(s"$e1;\n$e2")
            case (Right(t1), Right(t2))    => Right(t1 ++ t2)
          }
          .map(
            _ ++ transformShape(path, f, t) ++ insertions(path, ffields, tfields) ++ deletions(path, ffields, tfields)
          )
      case (f @ SchemaAst.Sum(_, fcases, _, _), t @ SchemaAst.Sum(_, tcases, _, _)) =>
        matchedSubtrees(fcases, tcases).map {
          case ((nextPath, fs), (_, ts)) => go(Chunk.empty, path :+ nextPath, fs, ts)
        }.reduce[Either[String, Chunk[Migration]]] {
            case (err @ Left(_), Right(_)) => err
            case (Right(_), err @ Left(_)) => err
            case (Left(e1), Left(e2))      => Left(s"$e1;\n$e2")
            case (Right(t1), Right(t2))    => Right(t1 ++ t2)
          }
          .map(_ ++ transformShape(path, f, t) ++ insertions(path, fcases, tcases) ++ deletions(path, fcases, tcases))
      case (f @ SchemaAst.Value(ftype, _, _, _), t @ SchemaAst.Value(ttype, _, _, _)) if ttype != ftype =>
        Right(acc ++ transformShape(path, f, t) :+ ChangeType(path, ttype))
      case (f @ SchemaAst.Value(_, _, _, _), t @ SchemaAst.Value(_, _, _, _)) =>
        Right(acc ++ transformShape(path, f, t))
      case (SchemaAst.Ref(fromRef, _, _, _), SchemaAst.Ref(toRef, _, _, _)) if fromRef == toRef => ???
      // Right(acc ++ transformShape(path, f, t))
      case (f, t) => Left(s"Subtrees at path ${renderPath(path)} are not homomorphic: $f cannot be mapped to $t")
    }

    go(Chunk.empty, Chunk.empty, from, to)
  }

  /**
   * Represents a valid label transformation.
   *
   * Not currently implemented but we can use this type to encode
   * unambiguous string transformations applued to field and case labels.
   * For example, convering from snake to camel case (or vica versa)
   */
  sealed trait LabelTransformation {
    def apply(label: String): Either[String, String]
  }

  object LabelTransformation {}

  private def matchedSubtrees(
    from: Chunk[SchemaAst.Labelled],
    to: Chunk[SchemaAst.Labelled]
  ): Chunk[(SchemaAst.Labelled, SchemaAst.Labelled)] =
    from.map {
      case fromNode @ (label, _) => to.find(_._1 == label).map(toNode => fromNode -> toNode)
    }.collect {
      case Some(pair) => pair
    }

  private def insertions(
    path: Chunk[String],
    from: Chunk[SchemaAst.Labelled],
    to: Chunk[SchemaAst.Labelled]
  ): Chunk[Migration] =
    to.foldRight[Chunk[Migration]](Chunk.empty) {
      case ((nodeLabel, _), acc) if from.exists(_._1 == nodeLabel) => acc
      case ((nodeLabel, ast), acc)                                 => acc :+ AddNode(path :+ nodeLabel, ast)
    }

  private def deletions(
    path: Chunk[String],
    from: Chunk[SchemaAst.Labelled],
    to: Chunk[SchemaAst.Labelled]
  ): Chunk[Migration] =
    from.foldRight[Chunk[Migration]](Chunk.empty) {
      case ((nodeLabel, _), acc) if !to.exists(_._1 == nodeLabel) => acc :+ DeleteNode(path :+ nodeLabel)
      case (_, acc)                                               => acc
    }

  private def transformShape(path: Chunk[String], from: SchemaAst, to: SchemaAst): Chunk[Migration] = {
    val builder = ChunkBuilder.make[Migration]()

    if (from.optional && !to.optional)
      builder += Require(path)

    if (to.optional && !from.optional)
      builder += Optional(path)

    if (to.dimensions > from.dimensions)
      builder += IncrementDimensions(path, to.dimensions - from.dimensions)

    if (from.dimensions > to.dimensions)
      builder += DecrementDimensions(path, from.dimensions - to.dimensions)

    builder.result()
  }

  private def updateLeaf(
    value: DynamicValue,
    path: List[String],
    trace: Chunk[String] = Chunk.empty
  )(op: (String, DynamicValue) => Either[String, Option[(String, DynamicValue)]]): Either[String, DynamicValue] =
    (value, path) match {
      case (DynamicValue.Record(values), leafLabel :: Nil) if values.keySet.contains(leafLabel) =>
        op(leafLabel, values(leafLabel)).map {
          case Some((newLeafLabel, newLeafValue)) if newLeafLabel == leafLabel =>
            DynamicValue.Record(values + (leafLabel -> newLeafValue))
          case Some((newLeafLabel, newLeafValue)) =>
            DynamicValue.Record(values - leafLabel + (newLeafLabel -> newLeafValue))
          case None =>
            DynamicValue.Record(values - leafLabel)
        }
      case (DynamicValue.Record(values), nextLabel :: remainder) if values.keySet.contains(nextLabel) =>
        updateLeaf(values(nextLabel), remainder, trace :+ nextLabel)(op).map { updatedValue =>
          DynamicValue.Record(values + (nextLabel -> updatedValue))
        }
      case (DynamicValue.Record(_), nextLabel :: _) =>
        Left(s"Expected label $nextLabel not found at path ${renderPath(trace)}")
      case (v @ DynamicValue.Enumeration((caseLabel, _)), nextLabel :: _) if caseLabel != nextLabel =>
        Right(v)
      case (DynamicValue.Enumeration((caseLabel, caseValue)), nextLabel :: Nil) if caseLabel == nextLabel =>
        op(caseLabel, caseValue).flatMap {
          case Some(newCase) => Right(DynamicValue.Enumeration(newCase))
          case None =>
            Left(
              s"Failed to update leaf node at path ${renderPath(trace :+ nextLabel)}: Cannot remove instantiated case"
            )
        }
      case (DynamicValue.Enumeration((caseLabel, caseValue)), nextLabel :: remainder) if caseLabel == nextLabel =>
        updateLeaf(caseValue, remainder, trace :+ nextLabel)(op).map { updatedValue =>
          DynamicValue.Enumeration(nextLabel -> updatedValue)
        }
      case _ =>
        Left(s"Failed to update leaf at path ${renderPath(trace ++ path)}: Unexpected node at ${renderPath(trace)}")
    }

  protected[schema] def tryUpdateFail(
    value: DynamicValue,
    path: List[String],
    newMessage: String
  ): Either[String, DynamicValue] =
    (path, value) match {
      case (Nil, DynamicValue.Error(_)) => Right(DynamicValue.Error(newMessage))
      case (Nil, _)                     => Left(s"Failed to update fail message at root. Unexpected type")
      case _ =>
        updateLeaf(value, path) { (label, value) =>
          value match {
            case DynamicValue.Error(_) => Right(Some(label -> DynamicValue.Error(newMessage)))
            case _                     => Left(s"Failed to update fail message at ${renderPath(path)}. Unexpected type")
          }
        }
    }

  protected[schema] def tryIncrementDimension(
    value: DynamicValue,
    path: List[String],
    n: Int
  ): Either[String, DynamicValue] =
    path match {
      case Nil =>
        Right(
          (0 until n).foldRight(value) {
            case (_, acc) =>
              DynamicValue.Sequence(Chunk(acc))
          }
        )
      case _ =>
        updateLeaf(value, path) { (label, v) =>
          Right(
            Some(
              (0 until n).foldRight(label -> v) {
                case (_, (_, acc)) =>
                  label -> DynamicValue.Sequence(Chunk(acc))
              }
            )
          )

        }
    }

  protected[schema] def tryDecrementDimensions(
    value: DynamicValue,
    path: List[String],
    n: Int
  ): Either[String, DynamicValue] =
    path match {
      case Nil =>
        (0 until n).foldRight[Either[String, DynamicValue]](Right(value)) {
          case (_, error @ Left(_))                                          => error
          case (_, Right(DynamicValue.Sequence(values))) if values.size == 1 => Right(values(0))
          case _ =>
            Left(
              s"Failed to decrement dimensions for node at path ${renderPath(path)}: Can only decrement dimensions on a sequence with one element"
            )
        }
      case _ =>
        updateLeaf(value, path) { (label, value) =>
          (0 until n)
            .foldRight[Either[String, DynamicValue]](Right(value)) {
              case (_, error @ Left(_))                                          => error
              case (_, Right(DynamicValue.Sequence(values))) if values.size == 1 => Right(values(0))
              case _ =>
                Left(
                  s"Failed to decrement dimensions for node at path ${renderPath(path)}: Can only decrement dimensions on a sequence with one element"
                )
            }
            .map(updatedValue => Some(label -> updatedValue))
        }
    }

  protected[schema] def tryRequire(
    value: DynamicValue,
    path: List[String]
  ): Either[String, DynamicValue] =
    (value, path) match {
      case (DynamicValue.SomeValue(v), Nil) => Right(v)
      case (DynamicValue.NoneValue, Nil) =>
        Left(
          s"Failed to require node: Optional value was None"
        )
      case _ =>
        updateLeaf(value, path) {
          case (label, DynamicValue.SomeValue(v)) =>
            Right(Some(label -> v))
          case (_, DynamicValue.NoneValue) =>
            Left(s"Failed to require leaf at path ${renderPath(path)}: Optional value was not available")
          case _ =>
            Left(s"Failed to require leaf at path ${renderPath(path)}: Expected optional value at lead")
        }
    }

  protected[schema] def tryRelabel(
    value: DynamicValue,
    path: List[String],
    transformation: LabelTransformation
  ): Either[String, DynamicValue] =
    path match {
      case Nil => Left(s"Cannot relabel node: Path was empty")
      case _ =>
        updateLeaf(value, path) { (label, value) =>
          transformation(label).fold(
            error =>
              Left(s"Failed to relabel node at path ${renderPath(path)}: Relabel transform failed with error $error"),
            newLabel => Right(Some(newLabel -> value))
          )
        }
    }

  protected[schema] def tryMakeOptional(
    value: DynamicValue,
    path: List[String]
  ): Either[String, DynamicValue] =
    (value, path) match {
      case (value, Nil) => Right(DynamicValue.SomeValue(value))
      case _ =>
        updateLeaf(value, path) {
          case (label, value) =>
            Right(Some(label -> DynamicValue.SomeValue(value)))
        }
    }

  protected[schema] def tryDeleteNode(
    value: DynamicValue,
    path: List[String]
  ): Either[String, DynamicValue] =
    path match {
      case Nil => Left(s"Cannot delete node: Path was empty")
      case _ =>
        updateLeaf(value, path)((_, _) => Right(None))
    }

  private def renderPath(path: Iterable[String]): String = path.mkString("/")

}
