package zio.schema.meta

import scala.collection.immutable.ListMap

import zio.prelude.{ Validation, ZValidation }
import zio.schema.meta.ExtensibleMetaSchema.Labelled
import zio.schema.{ DynamicValue, StandardType }
import zio.{ Chunk, ChunkBuilder }

sealed trait Migration { self =>

  def path: NodePath

  def migrate(value: DynamicValue): Validation[String, DynamicValue] =
    self match {
      case Migration.Require(path)  => Migration.require(value, path.toList)
      case Migration.Optional(path) => Migration.makeOptional(value, path.toList)
      case Migration.ChangeType(path, _) =>
        Validation.fail(
          s"Cannot change type of node at path ${path.render}: No type conversion is available"
        )
      case Migration.DeleteNode(path) => Migration.deleteNode(value, path.toList)
      case Migration.AddCase(_, _)    => Validation.succeed(value)
      case Migration.AddNode(path, _) =>
        Validation.fail(s"Cannot add node at path ${path.render}: No default value is available")
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

  final case class AddNode(override val path: NodePath, node: MetaSchema) extends Migration

  final case class AddCase(override val path: NodePath, node: MetaSchema) extends Migration

  final case class DeleteNode(override val path: NodePath) extends Migration

  final case class Relabel(override val path: NodePath, tranform: LabelTransformation) extends Migration

  final case class IncrementDimensions(override val path: NodePath, n: Int) extends Migration

  final case class DecrementDimensions(override val path: NodePath, n: Int) extends Migration

  final case class Recursive(override val path: NodePath, relativeNodePath: NodePath, relativeMigration: Migration)
      extends Migration

  def derive(from: MetaSchema, to: MetaSchema): zio.prelude.Validation[String, Chunk[Migration]] = {
    def go(
      acc: Chunk[Migration],
      path: NodePath,
      fromSubtree: MetaSchema,
      toSubtree: MetaSchema,
      ignoreRefs: Boolean
    ): zio.prelude.Validation[String, Chunk[Migration]] = {

      def goProduct(
        f: MetaSchema,
        t: MetaSchema,
        ffields: Chunk[MetaSchema.Labelled],
        tfields: Chunk[MetaSchema.Labelled]
      ): zio.prelude.Validation[String, Chunk[Migration]] =
        matchedSubtrees(ffields, tfields).map {
          case (Labelled(nextPath, fs), Labelled(_, ts)) => go(acc, path / nextPath, fs, ts, ignoreRefs)
        }.validateAll()
          .map(_.flatten)
          .map(
            _ ++ acc ++ transformShape(path, f, t) ++ insertions(path, ffields, tfields) ++ deletions(
              path,
              ffields,
              tfields
            )
          )

      def goSum(
        f: MetaSchema,
        t: MetaSchema,
        fcases: Chunk[MetaSchema.Labelled],
        tcases: Chunk[MetaSchema.Labelled]
      ): zio.prelude.Validation[String, Chunk[Migration]] =
        matchedSubtrees(fcases, tcases).map {
          case (Labelled(nextPath, fs), Labelled(_, ts)) => go(acc, path / nextPath, fs, ts, ignoreRefs)
        }.validateAll()
          .map(_.flatten)
          .map(
            _ ++ acc ++ transformShape(path, f, t) ++ caseInsertions(path, fcases, tcases) ++ deletions(
              path,
              fcases,
              tcases
            )
          )

      (fromSubtree, toSubtree) match {
        case (f @ ExtensibleMetaSchema.FailNode(_, _, _), t @ ExtensibleMetaSchema.FailNode(_, _, _)) =>
          Validation.succeed(
            if (f.message == t.message)
              Chunk.empty
            else
              transformShape(path, f, t) :+ UpdateFail(path, t.message)
          )
        case (f @ ExtensibleMetaSchema.Product(_, _, ffields, _), t @ ExtensibleMetaSchema.Product(_, _, tfields, _)) =>
          goProduct(f, t, ffields, tfields)
        case (
            f @ ExtensibleMetaSchema.Tuple(_, fleft, fright, _),
            t @ ExtensibleMetaSchema.Tuple(_, tleft, tright, _)
            ) =>
          val ffields = Chunk(Labelled("left", fleft), Labelled("right", fright))
          val tfields = Chunk(Labelled("left", tleft), Labelled("right", tright))
          goProduct(f, t, ffields, tfields)
        case (
            f @ ExtensibleMetaSchema.Product(_, _, ffields, _),
            t @ ExtensibleMetaSchema.Tuple(_, tleft, tright, _)
            ) =>
          val tfields = Chunk(Labelled("left", tleft), Labelled("right", tright))
          goProduct(f, t, ffields, tfields)
        case (
            f @ ExtensibleMetaSchema.Tuple(_, fleft, fright, _),
            t @ ExtensibleMetaSchema.Product(_, _, tfields, _)
            ) =>
          val ffields = Chunk(Labelled("left", fleft), Labelled("right", fright))
          goProduct(f, t, ffields, tfields)
        case (f @ ExtensibleMetaSchema.ListNode(fitem, _, _), t @ ExtensibleMetaSchema.ListNode(titem, _, _)) =>
          val ffields = Chunk(Labelled("item", fitem))
          val tfields = Chunk(Labelled("item", titem))
          goProduct(f, t, ffields, tfields)
        case (ExtensibleMetaSchema.ListNode(fitem, _, _), titem) =>
          derive(fitem, titem).map(migrations => DecrementDimensions(titem.path, 1) +: migrations)
        case (fitem, ExtensibleMetaSchema.ListNode(titem, _, _)) =>
          derive(fitem, titem).map(migrations => IncrementDimensions(titem.path, 1) +: migrations)
        case (
            f @ ExtensibleMetaSchema.Dictionary(fkeys, fvalues, _, _),
            t @ ExtensibleMetaSchema.Dictionary(tkeys, tvalues, _, _)
            ) =>
          val ffields = Chunk(Labelled("keys", fkeys), Labelled("values", fvalues))
          val tfields = Chunk(Labelled("keys", tkeys), Labelled("values", tvalues))
          goProduct(f, t, ffields, tfields)
        case (f @ ExtensibleMetaSchema.Sum(_, _, fcases, _), t @ ExtensibleMetaSchema.Sum(_, _, tcases, _)) =>
          goSum(f, t, fcases, tcases)
        case (
            f @ ExtensibleMetaSchema.Either(_, fleft, fright, _),
            t @ ExtensibleMetaSchema.Either(_, tleft, tright, _)
            ) =>
          val fcases = Chunk(Labelled("left", fleft), Labelled("right", fright))
          val tcases = Chunk(Labelled("left", tleft), Labelled("right", tright))
          goSum(f, t, fcases, tcases)
        case (f @ ExtensibleMetaSchema.Sum(_, _, fcases, _), t @ ExtensibleMetaSchema.Either(_, tleft, tright, _)) =>
          val tcases = Chunk(Labelled("left", tleft), Labelled("right", tright))
          goSum(f, t, fcases, tcases)
        case (f @ ExtensibleMetaSchema.Either(_, fleft, fright, _), t @ ExtensibleMetaSchema.Sum(_, _, tcases, _)) =>
          val fcases = Chunk(Labelled("left", fleft), Labelled("right", fright))
          goSum(f, t, fcases, tcases)
        case (f @ ExtensibleMetaSchema.Value(ftype, _, _), t @ ExtensibleMetaSchema.Value(ttype, _, _))
            if ttype != ftype =>
          Validation.succeed(transformShape(path, f, t) :+ ChangeType(path, ttype))
        case (f @ ExtensibleMetaSchema.Value(_, _, _), t @ ExtensibleMetaSchema.Value(_, _, _)) =>
          Validation.succeed(transformShape(path, f, t))
        case (f @ ExtensibleMetaSchema.Ref(fromRef, nodePath, _), t @ ExtensibleMetaSchema.Ref(toRef, _, _))
            if fromRef == toRef =>
          if (ignoreRefs) Validation.succeed(Chunk.empty)
          else {
            val recursiveMigrations = acc
              .filter(_.path.isSubpathOf(fromRef))
              .map(relativize(fromRef, nodePath.relativeTo(fromRef)))
            Validation.succeed(recursiveMigrations ++ transformShape(path, f, t))
          }
        case (f, t) =>
          Validation.fail(s"Subtrees at path ${renderPath(path)} are not homomorphic: $f cannot be mapped to $t")
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
   * unambiguous string transformations applied to field and case labels.
   * For example, converting from snake to camel case (or vica versa)
   */
  sealed trait LabelTransformation {
    def apply(label: String): zio.prelude.Validation[String, String]
  }

  object LabelTransformation {}

  private def matchedSubtrees(
    from: Chunk[MetaSchema.Labelled],
    to: Chunk[MetaSchema.Labelled]
  ): Chunk[(MetaSchema.Labelled, MetaSchema.Labelled)] =
    from.map {
      case fromNode @ Labelled(label, _) => to.find(_.label == label).map(toNode => fromNode -> toNode)
    }.collect {
      case Some(pair) => pair
    }

  private def insertions(
    path: NodePath,
    from: Chunk[MetaSchema.Labelled],
    to: Chunk[MetaSchema.Labelled]
  ): Chunk[Migration] =
    to.foldRight[Chunk[Migration]](Chunk.empty) {
      case (Labelled(nodeLabel, _), acc) if from.exists(_.label == nodeLabel) => acc
      case (Labelled(nodeLabel, ast), acc)                                    => acc :+ AddNode(path / nodeLabel, ast)
    }

  private def caseInsertions(
    path: NodePath,
    from: Chunk[MetaSchema.Labelled],
    to: Chunk[MetaSchema.Labelled]
  ): Chunk[Migration] =
    to.foldRight[Chunk[Migration]](Chunk.empty) {
      case (Labelled(nodeLabel, _), acc) if from.exists(_.label == nodeLabel) => acc
      case (Labelled(nodeLabel, ast), acc)                                    => acc :+ AddCase(path / nodeLabel, ast)
    }

  private def deletions(
    path: NodePath,
    from: Chunk[MetaSchema.Labelled],
    to: Chunk[MetaSchema.Labelled]
  ): Chunk[Migration] =
    from.foldRight[Chunk[Migration]](Chunk.empty) {
      case (Labelled(nodeLabel, _), acc) if !to.exists(_.label == nodeLabel) => acc :+ DeleteNode(path / nodeLabel)
      case (_, acc)                                                          => acc
    }

  private def transformShape(path: NodePath, from: MetaSchema, to: MetaSchema): Chunk[Migration] = {
    val builder = ChunkBuilder.make[Migration]()

    if (from.optional && !to.optional)
      builder += Require(path)

    if (to.optional && !from.optional)
      builder += Optional(path)

    builder.result()
  }

  private def updateLeaf(
    value: DynamicValue,
    path: List[String],
    trace: Chunk[String] = Chunk.empty
  )(
    op: (String, DynamicValue) => zio.prelude.Validation[String, Option[(String, DynamicValue)]]
  ): zio.prelude.Validation[String, DynamicValue] = {
    (value, path) match {
      case (DynamicValue.SomeValue(value), _) =>
        updateLeaf(value, path, trace)(op).map(DynamicValue.SomeValue)
      case (DynamicValue.NoneValue, _) => Validation.succeed(DynamicValue.NoneValue)
      case (DynamicValue.Sequence(values), "item" :: remainder) =>
        values.zipWithIndex.map { case (v, idx) => updateLeaf(v, remainder, trace :+ s"item[$idx]")(op) }
          .validateAll()
          .map(DynamicValue.Sequence)
      case (DynamicValue.Tuple(l, r), "left" :: remainder) =>
        updateLeaf(l, remainder, trace :+ "left")(op).map(newLeft => DynamicValue.Tuple(newLeft, r))
      case (DynamicValue.Tuple(l, r), "right" :: remainder) =>
        updateLeaf(r, remainder, trace :+ "right")(op).map(newRight => DynamicValue.Tuple(l, newRight))
      case (DynamicValue.LeftValue(l), "left" :: remainder) =>
        updateLeaf(l, remainder, trace :+ "left")(op).map(DynamicValue.LeftValue)
      case (value @ DynamicValue.LeftValue(_), "right" :: _) =>
        Validation.succeed(value)
      case (DynamicValue.RightValue(r), "right" :: remainder) =>
        updateLeaf(r, remainder, trace :+ "right")(op).map(DynamicValue.RightValue)
      case (value @ DynamicValue.RightValue(_), "left" :: _) =>
        Validation.succeed(value)
      case (DynamicValue.Record(name, values), leafLabel :: Nil) if values.keySet.contains(leafLabel) =>
        op(leafLabel, values(leafLabel)).map {
          case Some((newLeafLabel, newLeafValue)) =>
            DynamicValue.Record(name, spliceRecord(values, leafLabel, newLeafLabel -> newLeafValue))
          case None => DynamicValue.Record(name, values - leafLabel)
        }
      case (DynamicValue.Record(name, values), nextLabel :: remainder) if values.keySet.contains(nextLabel) =>
        updateLeaf(values(nextLabel), remainder, trace :+ nextLabel)(op).map { updatedValue =>
          DynamicValue.Record(name, spliceRecord(values, nextLabel, nextLabel -> updatedValue))
        }
      case (DynamicValue.Record(_, _), nextLabel :: _) =>
        Validation.fail(s"Expected label $nextLabel not found at path ${renderPath(trace)}")
      case (v @ DynamicValue.Enumeration(_, (caseLabel, _)), nextLabel :: _) if caseLabel != nextLabel =>
        Validation.succeed(v)
      case (DynamicValue.Enumeration(id, (caseLabel, caseValue)), nextLabel :: Nil) if caseLabel == nextLabel =>
        op(caseLabel, caseValue).flatMap {
          case Some(newCase) => Validation.succeed(DynamicValue.Enumeration(id, newCase))
          case None =>
            Validation.fail(
              s"Failed to update leaf node at path ${renderPath(trace :+ nextLabel)}: Cannot remove instantiated case"
            )
        }
      case (DynamicValue.Enumeration(id, (caseLabel, caseValue)), nextLabel :: remainder) if caseLabel == nextLabel =>
        updateLeaf(caseValue, remainder, trace :+ nextLabel)(op).map { updatedValue =>
          DynamicValue.Enumeration(id, nextLabel -> updatedValue)
        }
      case (DynamicValue.Dictionary(entries), "keys" :: Nil) =>
        entries
          .map(_._1)
          .zipWithIndex
          .map {
            case (k, idx) =>
              op(s"key[$idx]", k).flatMap {
                case Some((_, migrated)) => Validation.succeed(migrated)
                case None                => Validation.fail(s"invalid update at $path, cannot remove map key")
              }
          }
          .validateAll()
          .map { keys =>
            DynamicValue.Dictionary(keys.zip(entries.map(_._2)))
          }
      case (DynamicValue.Dictionary(entries), "keys" :: remainder) =>
        entries
          .map(_._1)
          .zipWithIndex
          .map {
            case (k, idx) =>
              updateLeaf(k, remainder, trace :+ s"key[$idx]")(op)
          }
          .validateAll()
          .map { keys =>
            DynamicValue.Dictionary(keys.zip(entries.map(_._2)))
          }
      case (DynamicValue.Dictionary(entries), "values" :: Nil) =>
        entries
          .map(_._2)
          .zipWithIndex
          .map {
            case (k, idx) =>
              op(s"key[$idx]", k).flatMap {
                case Some((_, migrated)) => Validation.succeed(migrated)
                case None                => Validation.fail(s"invalid update at $path, cannot remove map value")
              }
          }
          .validateAll()
          .map { values =>
            DynamicValue.Dictionary(entries.map(_._1).zip(values))
          }
      case (DynamicValue.Dictionary(entries), "values" :: remainder) =>
        entries
          .map(_._2)
          .zipWithIndex
          .map {
            case (k, idx) =>
              updateLeaf(k, remainder, trace :+ s"value[$idx]")(op)
          }
          .validateAll()
          .map { values =>
            DynamicValue.Dictionary(entries.map(_._1).zip(values))
          }
      case _ =>
        Validation.fail(
          s"Failed to update leaf at path ${renderPath(trace ++ path)}: Unexpected node at ${renderPath(trace)}"
        )
    }
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

  protected[schema] def migrateRecursive(
    value: DynamicValue,
    migration: Recursive
  ): zio.prelude.Validation[String, DynamicValue] = {
    def go(lastValue: DynamicValue, depth: Int): zio.prelude.Validation[String, DynamicValue] =
      materializeRecursive(depth)(migration).migrate(lastValue).flatMap { thisValue =>
        if (thisValue == lastValue)
          Validation.succeed(thisValue)
        else go(thisValue, depth + 1)
      }

    go(value, 1)
  }

  protected[schema] def updateFail(
    value: DynamicValue,
    path: List[String],
    newMessage: String
  ): zio.prelude.Validation[String, DynamicValue] =
    (path, value) match {
      case (Nil, DynamicValue.Error(_)) => Validation.succeed(DynamicValue.Error(newMessage))
      case (Nil, _)                     => Validation.fail(s"Failed to update fail message at root. Unexpected type")
      case _ =>
        updateLeaf(value, path) { (label, value) =>
          value match {
            case DynamicValue.Error(_) => Validation.succeed(Some(label -> DynamicValue.Error(newMessage)))
            case _                     => Validation.fail(s"Failed to update fail message at ${renderPath(path)}. Unexpected type")
          }
        }
    }

  protected[schema] def incrementDimension(
    value: DynamicValue,
    path: List[String],
    n: Int
  ): zio.prelude.Validation[String, DynamicValue] =
    path match {
      case Nil =>
        Validation.succeed(
          (0 until n).foldRight(value) {
            case (_, acc) =>
              DynamicValue.Sequence(Chunk(acc))
          }
        )
      case _ =>
        updateLeaf(value, path) { (label, v) =>
          Validation.succeed(
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
  ): Validation[String, DynamicValue] =
    path match {
      case Nil =>
        Validation.fromEither((0 until n).foldRight[Either[String, DynamicValue]](Right(value)) {
          case (_, error @ Left(_))                                          => error
          case (_, Right(DynamicValue.Sequence(values))) if values.size == 1 => Right(values(0))
          case _ =>
            Left(
              s"Failed to decrement dimensions for node at path ${renderPath(path)}: Can only decrement dimensions on a sequence with one element"
            )
        })
      case _ =>
        updateLeaf(value, path) { (label, value) =>
          Validation.fromEither(
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
          )
        }
    }

  protected[schema] def require(
    value: DynamicValue,
    path: List[String]
  ): Validation[String, DynamicValue] =
    (value, path) match {
      case (DynamicValue.SomeValue(v), Nil) => Validation.succeed(v)
      case (DynamicValue.NoneValue, Nil) =>
        Validation.fail(
          s"Failed to require node: Optional value was None"
        )
      case _ =>
        updateLeaf(value, path) {
          case (label, DynamicValue.SomeValue(v)) =>
            Validation.succeed(Some(label -> v))
          case (_, DynamicValue.NoneValue) =>
            Validation.fail(s"Failed to require leaf at path ${renderPath(path)}: Optional value was not available")
          case _ =>
            Validation.fail(s"Failed to require leaf at path ${renderPath(path)}: Expected optional value at lead")
        }
    }

  protected[schema] def relabel(
    value: DynamicValue,
    path: List[String],
    transformation: LabelTransformation
  ): zio.prelude.Validation[String, DynamicValue] =
    path match {
      case Nil => Validation.fail(s"Cannot relabel node: Path was empty")
      case _ =>
        updateLeaf(value, path) { (label, value) =>
          transformation(label).fold(
            error =>
              Validation.fail(
                s"Failed to relabel node at path ${renderPath(path)}: Relabel transform failed with error $error"
              ),
            newLabel => Validation.succeed(Some(newLabel -> value))
          )
        }
    }

  protected[schema] def makeOptional(
    value: DynamicValue,
    path: List[String]
  ): zio.prelude.Validation[String, DynamicValue] =
    (value, path) match {
      case (value, Nil) => Validation.succeed(DynamicValue.SomeValue(value))
      case _ =>
        updateLeaf(value, path) {
          case (label, value) =>
            Validation.succeed(Some(label -> DynamicValue.SomeValue(value)))
        }
    }

  protected[schema] def deleteNode(
    value: DynamicValue,
    path: List[String]
  ): zio.prelude.Validation[String, DynamicValue] =
    path match {
      case Nil => Validation.fail(s"Cannot delete node: Path was empty")
      case _ =>
        updateLeaf(value, path)((_, _) => Validation.succeed(None))
    }

  private def renderPath(path: Iterable[String]): String = path.mkString("/")

  implicit private class ChunkExt[+W, +E, +A](c: Chunk[ZValidation[W, E, A]]) {
    def validateAll(): ZValidation[W, E, Chunk[A]] = Validation.validateAll(c)
  }

}
