package zio.schema.ast

import scala.collection.immutable.ListMap

import zio.schema.{ DynamicValue, StandardType }
import zio.{ Chunk, ChunkBuilder }

sealed trait Migration { self =>

  def path: NodePath

  def migrate(value: DynamicValue): Either[String, DynamicValue] =
    self match {
      case Migration.Require(path)  => Migration.require(value, path.toList)
      case Migration.Optional(path) => Migration.makeOptional(value, path.toList)
      case Migration.ChangeType(path, _) =>
        Left(
          s"Cannot change type of node at path ${path.render}: No type conversion is available"
        )
      case Migration.DeleteNode(path) => Migration.deleteNode(value, path.toList)
      case Migration.AddCase(_, _)    => Right(value)
      case Migration.AddNode(path, _) =>
        Left(s"Cannot add node at path ${path.render}: No default value is available")
      case Migration.Relabel(path, transform) => Migration.relabel(value, path.toList, transform)
      case Migration.IncrementDimensions(path, n) =>
        Migration.incrementDimension(value, path.toList, n)
      case Migration.DecrementDimensions(path, n) =>
        Migration.decrementDimensions(value, path.toList, n)
      case Migration.UpdateFail(path, newMessage) =>
        Migration.updateFail(value, path.toList, newMessage)
      case m @ Migration.Recursive(_, _, _) =>
        Migration.migrateRecursive(value, m)
    }
}

object Migration {
  final case class UpdateFail(override val path: NodePath, message: String) extends Migration

  final case class Optional(override val path: NodePath) extends Migration

  final case class Require(override val path: NodePath) extends Migration

  final case class ChangeType(override val path: NodePath, value: StandardType[_]) extends Migration

  final case class AddNode(override val path: NodePath, node: SchemaAst) extends Migration

  final case class AddCase(override val path: NodePath, node: SchemaAst) extends Migration

  final case class DeleteNode(override val path: NodePath) extends Migration

  final case class Relabel(override val path: NodePath, tranform: LabelTransformation) extends Migration

  final case class IncrementDimensions(override val path: NodePath, n: Int) extends Migration

  final case class DecrementDimensions(override val path: NodePath, n: Int) extends Migration

  final case class Recursive(override val path: NodePath, relativeNodePath: NodePath, relativeMigration: Migration)
      extends Migration

  def derive(from: SchemaAst, to: SchemaAst): Either[String, Chunk[Migration]] = {
    def go(
      acc: Chunk[Migration],
      path: NodePath,
      fromSubtree: SchemaAst,
      toSubtree: SchemaAst,
      ignoreRefs: Boolean
    ): Either[String, Chunk[Migration]] = {

      def goProduct(
        f: SchemaAst,
        t: SchemaAst,
        ffields: Chunk[(String, SchemaAst)],
        tfields: Chunk[(String, SchemaAst)]
      ): Either[String, Chunk[Migration]] =
        matchedSubtrees(ffields, tfields).map {
          case ((nextPath, fs), (_, ts)) => go(acc, path / nextPath, fs, ts, ignoreRefs)
        }.foldRight[Either[String, Chunk[Migration]]](Right(Chunk.empty)) {
            case (err @ Left(_), Right(_)) => err
            case (Right(_), err @ Left(_)) => err
            case (Left(e1), Left(e2))      => Left(s"$e1;\n$e2")
            case (Right(t1), Right(t2))    => Right(t1 ++ t2)
          }
          .map(
            _ ++ acc ++ transformShape(path, f, t) ++ insertions(path, ffields, tfields) ++ deletions(
              path,
              ffields,
              tfields
            )
          )

      def goSum(
        f: SchemaAst,
        t: SchemaAst,
        fcases: Chunk[(String, SchemaAst)],
        tcases: Chunk[(String, SchemaAst)]
      ): Either[String, Chunk[Migration]] =
        matchedSubtrees(fcases, tcases).map {
          case ((nextPath, fs), (_, ts)) => go(acc, path / nextPath, fs, ts, ignoreRefs)
        }.foldRight[Either[String, Chunk[Migration]]](Right(Chunk.empty)) {
            case (err @ Left(_), Right(_)) => err
            case (Right(_), err @ Left(_)) => err
            case (Left(e1), Left(e2))      => Left(s"$e1;\n$e2")
            case (Right(t1), Right(t2))    => Right(t1 ++ t2)
          }
          .map(
            _ ++ acc ++ transformShape(path, f, t) ++ caseInsertions(path, fcases, tcases) ++ deletions(
              path,
              fcases,
              tcases
            )
          )

      (fromSubtree, toSubtree) match {
        case (f: SchemaAst.FailNode, t: SchemaAst.FailNode) =>
          Right(
            if (f.message == t.message)
              Chunk.empty
            else
              transformShape(path, f, t) :+ UpdateFail(path, t.message)
          )
        case (f @ SchemaAst.Product(_, ffields, _, _), t @ SchemaAst.Product(_, tfields, _, _)) =>
          goProduct(f, t, ffields, tfields)
        case (f @ SchemaAst.Tuple(_, fleft, fright, _, _), t @ SchemaAst.Tuple(_, tleft, tright, _, _)) =>
          val ffields = Chunk("left" -> fleft, "right" -> fright)
          val tfields = Chunk("left" -> tleft, "right" -> tright)
          goProduct(f, t, ffields, tfields)
        case (f @ SchemaAst.Product(_, ffields, _, _), t @ SchemaAst.Tuple(_, tleft, tright, _, _)) =>
          val tfields = Chunk("left" -> tleft, "right" -> tright)
          goProduct(f, t, ffields, tfields)
        case (f @ SchemaAst.Tuple(_, fleft, fright, _, _), t @ SchemaAst.Product(_, tfields, _, _)) =>
          val ffields = Chunk("left" -> fleft, "right" -> fright)
          goProduct(f, t, ffields, tfields)
        case (f @ SchemaAst.Sum(_, fcases, _, _), t @ SchemaAst.Sum(_, tcases, _, _)) =>
          goSum(f, t, fcases, tcases)
        case (f @ SchemaAst.Either(_, fleft, fright, _, _), t @ SchemaAst.Either(_, tleft, tright, _, _)) =>
          val fcases = Chunk("left" -> fleft, "right" -> fright)
          val tcases = Chunk("left" -> tleft, "right" -> tright)
          goSum(f, t, fcases, tcases)
        case (f @ SchemaAst.Sum(_, fcases, _, _), t @ SchemaAst.Either(_, tleft, tright, _, _)) =>
          val tcases = Chunk("left" -> tleft, "right" -> tright)
          goSum(f, t, fcases, tcases)
        case (f @ SchemaAst.Either(_, fleft, fright, _, _), t @ SchemaAst.Sum(_, tcases, _, _)) =>
          val fcases = Chunk("left" -> fleft, "right" -> fright)
          goSum(f, t, fcases, tcases)
        case (f @ SchemaAst.Value(ftype, _, _, _), t @ SchemaAst.Value(ttype, _, _, _)) if ttype != ftype =>
          Right(transformShape(path, f, t) :+ ChangeType(path, ttype))
        case (f @ SchemaAst.Value(_, _, _, _), t @ SchemaAst.Value(_, _, _, _)) =>
          Right(transformShape(path, f, t))
        case (f @ SchemaAst.Ref(fromRef, nodePath, _, _), t @ SchemaAst.Ref(toRef, _, _, _)) if fromRef == toRef =>
          if (ignoreRefs) Right(Chunk.empty)
          else {
            val recursiveMigrations = acc
              .filter(_.path.isSubpathOf(fromRef))
              .map(relativize(fromRef, nodePath.relativeTo(fromRef)))
            Right(recursiveMigrations ++ transformShape(path, f, t))
          }
        case (f, t) => Left(s"Subtrees at path ${renderPath(path)} are not homomorphic: $f cannot be mapped to $t")
      }
    }

    for {
      ignoringRefs <- go(Chunk.empty, NodePath.root, from, to, ignoreRefs = true)
      withRefs     <- go(ignoringRefs, NodePath.root, from, to, ignoreRefs = false)
    } yield (withRefs ++ ignoringRefs).distinct
  }

  private def relativize(refPath: NodePath, relativeNodePath: NodePath)(migration: Migration): Migration =
    migration match {
      case m: UpdateFail =>
        Recursive(refPath, relativeNodePath, m.copy(path = m.path.relativeTo(refPath)))
      case m: Optional =>
        Recursive(refPath, relativeNodePath, m.copy(path = m.path.relativeTo(refPath)))
      case m: Require =>
        Recursive(refPath, relativeNodePath, m.copy(path = m.path.relativeTo(refPath)))
      case m: ChangeType =>
        Recursive(refPath, relativeNodePath, m.copy(path = m.path.relativeTo(refPath)))
      case m: AddNode =>
        Recursive(refPath, relativeNodePath, m.copy(path = m.path.relativeTo(refPath)))
      case m: AddCase =>
        Recursive(refPath, relativeNodePath, m.copy(path = m.path.relativeTo(refPath)))
      case m: DeleteNode =>
        Recursive(refPath, relativeNodePath, m.copy(path = m.path.relativeTo(refPath)))
      case m: Relabel =>
        Recursive(refPath, relativeNodePath, m.copy(path = m.path.relativeTo(refPath)))
      case m: IncrementDimensions =>
        Recursive(refPath, relativeNodePath, m.copy(path = m.path.relativeTo(refPath)))
      case m: DecrementDimensions =>
        Recursive(refPath, relativeNodePath, m.copy(path = m.path.relativeTo(refPath)))
      case m: Recursive =>
        Recursive(refPath, relativeNodePath, m.copy(path = m.path.relativeTo(refPath)))
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
    path: NodePath,
    from: Chunk[SchemaAst.Labelled],
    to: Chunk[SchemaAst.Labelled]
  ): Chunk[Migration] =
    to.foldRight[Chunk[Migration]](Chunk.empty) {
      case ((nodeLabel, _), acc) if from.exists(_._1 == nodeLabel) => acc
      case ((nodeLabel, ast), acc)                                 => acc :+ AddNode(path / nodeLabel, ast)
    }

  private def caseInsertions(
    path: NodePath,
    from: Chunk[SchemaAst.Labelled],
    to: Chunk[SchemaAst.Labelled]
  ): Chunk[Migration] =
    to.foldRight[Chunk[Migration]](Chunk.empty) {
      case ((nodeLabel, _), acc) if from.exists(_._1 == nodeLabel) => acc
      case ((nodeLabel, ast), acc)                                 => acc :+ AddCase(path / nodeLabel, ast)
    }

  private def deletions(
    path: NodePath,
    from: Chunk[SchemaAst.Labelled],
    to: Chunk[SchemaAst.Labelled]
  ): Chunk[Migration] =
    from.foldRight[Chunk[Migration]](Chunk.empty) {
      case ((nodeLabel, _), acc) if !to.exists(_._1 == nodeLabel) => acc :+ DeleteNode(path / nodeLabel)
      case (_, acc)                                               => acc
    }

  private def transformShape(path: NodePath, from: SchemaAst, to: SchemaAst): Chunk[Migration] = {
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
      case (DynamicValue.Transform(value), _) =>
        updateLeaf(value, path, trace)(op).map(DynamicValue.Transform(_))
      case (DynamicValue.SomeValue(value), _) =>
        updateLeaf(value, path, trace)(op).map(DynamicValue.SomeValue(_))
      case (DynamicValue.NoneValue, _) => Right(DynamicValue.NoneValue)
      case (DynamicValue.Sequence(values), _) =>
        values
          .map(v => updateLeaf(v, path, trace)(op))
          .foldRight[Either[String, DynamicValue.Sequence]](Right(DynamicValue.Sequence(Chunk.empty))) {
            case (Left(e1), Left(e2)) => Left(s"$e1;\n$e2")
            case (Left(e), Right(_))  => Left(e)
            case (Right(_), Left(e))  => Left(e)
            case (Right(DynamicValue.Sequence(v1s)), Right(DynamicValue.Sequence(v2s))) =>
              Right(DynamicValue.Sequence(v1s ++ v2s))
            case (Right(v1), Right(DynamicValue.Sequence(v2s))) => Right(DynamicValue.Sequence(v1 +: v2s))
          }
      case (DynamicValue.Tuple(l, r), "left" :: remainder) =>
        updateLeaf(l, remainder, trace :+ "left")(op).map(newLeft => DynamicValue.Tuple(newLeft, r))
      case (DynamicValue.Tuple(l, r), "right" :: remainder) =>
        updateLeaf(r, remainder, trace :+ "right")(op).map(newRight => DynamicValue.Tuple(l, newRight))
      case (DynamicValue.LeftValue(l), "left" :: remainder) =>
        updateLeaf(l, remainder, trace :+ "left")(op).map(DynamicValue.LeftValue(_))
      case (value @ DynamicValue.LeftValue(_), "right" :: _) =>
        Right(value)
      case (DynamicValue.RightValue(r), "right" :: remainder) =>
        updateLeaf(r, remainder, trace :+ "right")(op).map(DynamicValue.RightValue(_))
      case (value @ DynamicValue.RightValue(_), "left" :: _) =>
        Right(value)
      case (DynamicValue.Record(values), leafLabel :: Nil) if values.keySet.contains(leafLabel) =>
        op(leafLabel, values(leafLabel)).map {
          case Some((newLeafLabel, newLeafValue)) =>
            DynamicValue.Record(spliceRecord(values, leafLabel, newLeafLabel -> newLeafValue))
          case None => DynamicValue.Record(values - leafLabel)
        }
      case (DynamicValue.Record(values), nextLabel :: remainder) if values.keySet.contains(nextLabel) =>
        updateLeaf(values(nextLabel), remainder, trace :+ nextLabel)(op).map { updatedValue =>
          DynamicValue.Record(spliceRecord(values, nextLabel, nextLabel -> updatedValue))
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

  private def spliceRecord(
    fields: ListMap[String, DynamicValue],
    label: String,
    splicedField: (String, DynamicValue)
  ): ListMap[String, DynamicValue] =
    fields.foldLeft[ListMap[String, DynamicValue]](ListMap.empty) {
      case (acc, (nextLabel, _)) if nextLabel == label => acc + splicedField
      case (acc, nextField)                            => acc + nextField
    }

  private def materializeRecursive(depth: Int)(migration: Recursive): Migration = {
    def appendRecursiveN(n: Int)(relativePath: NodePath): NodePath =
      (0 until n).foldRight(NodePath.root)((_, path) => path / relativePath)

    migration match {
      case Recursive(refPath, relativeNodePath, m: UpdateFail) =>
        m.copy(path = refPath / appendRecursiveN(depth)(relativeNodePath) / m.path)
      case Recursive(refPath, relativeNodePath, m: Optional) =>
        m.copy(path = refPath / appendRecursiveN(depth)(relativeNodePath) / m.path)
      case Recursive(refPath, relativeNodePath, m: Require) =>
        m.copy(path = refPath / appendRecursiveN(depth)(relativeNodePath) / m.path)
      case Recursive(refPath, relativeNodePath, m: ChangeType) =>
        m.copy(path = refPath / appendRecursiveN(depth)(relativeNodePath) / m.path)
      case Recursive(refPath, relativeNodePath, m: AddNode) =>
        m.copy(path = refPath / appendRecursiveN(depth)(relativeNodePath) / m.path)
      case Recursive(refPath, relativeNodePath, m: AddCase) =>
        m.copy(path = refPath / appendRecursiveN(depth)(relativeNodePath) / m.path)
      case Recursive(refPath, relativeNodePath, m: DeleteNode) =>
        m.copy(path = refPath / appendRecursiveN(depth)(relativeNodePath) / m.path)
      case Recursive(refPath, relativeNodePath, m: Relabel) =>
        m.copy(path = refPath / appendRecursiveN(depth)(relativeNodePath) / m.path)
      case Recursive(refPath, relativeNodePath, m: IncrementDimensions) =>
        m.copy(path = refPath / appendRecursiveN(depth)(relativeNodePath) / m.path)
      case Recursive(refPath, relativeNodePath, m: DecrementDimensions) =>
        m.copy(path = refPath / appendRecursiveN(depth)(relativeNodePath) / m.path)
      case Recursive(refPath, relativeNodePath, m: Recursive) =>
        m.copy(path = refPath / appendRecursiveN(depth)(relativeNodePath) / m.path)
    }
  }

  protected[schema] def migrateRecursive(value: DynamicValue, migration: Recursive): Either[String, DynamicValue] = {
    def go(lastValue: DynamicValue, depth: Int): Either[String, DynamicValue] =
      materializeRecursive(depth)(migration).migrate(lastValue).flatMap { thisValue =>
        if (thisValue == lastValue)
          Right(thisValue)
        else go(thisValue, depth + 1)
      }

    go(value, 1)
  }

  protected[schema] def updateFail(
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

  protected[schema] def incrementDimension(
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

  protected[schema] def decrementDimensions(
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

  protected[schema] def require(
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

  protected[schema] def relabel(
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

  protected[schema] def makeOptional(
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

  protected[schema] def deleteNode(
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
