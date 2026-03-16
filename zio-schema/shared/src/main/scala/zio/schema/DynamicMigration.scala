package zio.schema

import zio.Chunk
import scala.collection.immutable.ListMap

case class DynamicMigration(actions: Chunk[MigrationAction]) {

  def apply(value: DynamicValue): Either[MigrationError, DynamicValue] = {
    actions.foldLeft[Either[MigrationError, DynamicValue]](Right(value)) {
      case (acc, action) =>
        acc.flatMap { currentVal =>
          Interpreter.evaluate(action, currentVal)
        }
    }
  }

  def ++(that: DynamicMigration): DynamicMigration =
    DynamicMigration(this.actions ++ that.actions)

  def reverse: DynamicMigration =
    DynamicMigration(this.actions.reverse.map(_.reverse))
}

object Interpreter {
  import MigrationAction._
  import DynamicOptic._

  def evaluate(action: MigrationAction, value: DynamicValue): Either[MigrationError, DynamicValue] = {
    action match {
      case AddField(at, default) =>
        val defaultValue = default.toDynamic
        insertAtPath(value, at, defaultValue)

      case DropField(at, _) =>
        removeAtPath(value, at)

      case Rename(at, to) =>
        at match {
          case Field(parent, oldName) =>
            getAtPath(value, at).flatMap { v =>
              removeAtPath(value, at).flatMap { removed =>
                insertAtPath(removed, Field(parent, to), v)
              }
            }
          case _ => Left(MigrationError("Rename only supported for fields", at))
        }

      case TransformValue(at, transform) =>
        updateAtPath(value, at)(_ => Right(transform.toDynamic))

      case Mandate(at, default) =>
        updateAtPath(value, at) {
          case DynamicValue.SomeValue(v) => Right(v)
          case DynamicValue.NoneValue    => Right(default.toDynamic)
          case _                         => Left(MigrationError("Expected Optional value", at))
        }

      case Optionalize(at) =>
        updateAtPath(value, at)(v => Right(DynamicValue.SomeValue(v)))

      case ChangeType(at, converter) =>
        // In a real implementation, converter would be applied to the value. 
        // For now, we just replace.
        updateAtPath(value, at)(_ => Right(converter.toDynamic))

      case RenameCase(at, from, to) =>
        updateAtPath(value, at) {
          case DynamicValue.Enumeration(id, (caseName, caseValue)) if caseName == from =>
            Right(DynamicValue.Enumeration(id, (to, caseValue)))
          case other => Right(other) // No-op if case doesn't match
        }

      case TransformCase(at, actions) =>
        updateAtPath(value, at) {
          case DynamicValue.Enumeration(id, (caseName, caseValue)) =>
            actions.foldLeft[Either[MigrationError, DynamicValue]](Right(caseValue)) {
              case (acc, act) => acc.flatMap(evaluate(act, _))
            }.map(newCaseValue => DynamicValue.Enumeration(id, (caseName, newCaseValue)))
          case _ => Left(MigrationError("Expected Enumeration", at))
        }
      
      case _ => Right(value) // Placeholder for other actions
    }
  }

  private def getAtPath(value: DynamicValue, path: DynamicOptic): Either[MigrationError, DynamicValue] = {
    path match {
      case Root => Right(value)
      case Field(parent, name) =>
        getAtPath(value, parent).flatMap {
          case DynamicValue.Record(_, values) =>
            values.get(name) match {
              case Some(v) => Right(v)
              case None    => Left(MigrationError(s"Field $name not found", path))
            }
          case _ => Left(MigrationError("Expected Record", parent))
        }
      case Case(parent, name) =>
        getAtPath(value, parent).flatMap {
          case DynamicValue.Enumeration(_, (caseName, caseValue)) if caseName == name =>
            Right(caseValue)
          case _ => Left(MigrationError(s"Case $name not found", parent))
        }
      case Each(_) => Left(MigrationError("getAtPath not supported for Each", path))
    }
  }

  private def updateAtPath(value: DynamicValue, path: DynamicOptic)(f: DynamicValue => Either[MigrationError, DynamicValue]): Either[MigrationError, DynamicValue] = {
    path match {
      case Root => f(value)
      case Field(parent, name) =>
        updateAtPath(value, parent) {
          case DynamicValue.Record(id, values) =>
            values.get(name) match {
              case Some(v) => f(v).map(newV => DynamicValue.Record(id, values + (name -> newV)))
              case None    => Left(MigrationError(s"Field $name not found", path))
            }
          case _ => Left(MigrationError("Expected Record", parent))
        }
      case Case(parent, name) =>
        updateAtPath(value, parent) {
          case DynamicValue.Enumeration(id, (caseName, caseValue)) if caseName == name =>
            f(caseValue).map(newCaseValue => DynamicValue.Enumeration(id, (caseName, newCaseValue)))
          case other => Right(other)
        }
      case Each(parent) =>
        updateAtPath(value, parent) {
          case DynamicValue.Sequence(values) =>
            values.map(v => f(v)).foldLeft[Either[MigrationError, Chunk[DynamicValue]]](Right(Chunk.empty)) {
              case (Right(acc), Right(v)) => Right(acc :+ v)
              case (Left(err), _)         => Left(err)
              case (_, Left(err))         => Left(err)
            }.map(DynamicValue.Sequence(_))
          case _ => Left(MigrationError("Expected Sequence", parent))
        }
    }
  }

  private def insertAtPath(value: DynamicValue, path: DynamicOptic, newValue: DynamicValue): Either[MigrationError, DynamicValue] = {
    path match {
      case Root => Right(newValue)
      case Field(parent, name) =>
        updateAtPath(value, parent) {
          case DynamicValue.Record(id, values) =>
            Right(DynamicValue.Record(id, values + (name -> newValue)))
          case _ => Left(MigrationError("Expected Record", parent))
        }
      case _ => Left(MigrationError("Insert only supported for fields", path))
    }
  }

  private def removeAtPath(value: DynamicValue, path: DynamicOptic): Either[MigrationError, DynamicValue] = {
    path match {
      case Root => Left(MigrationError("Cannot remove root", path))
      case Field(parent, name) =>
        updateAtPath(value, parent) {
          case DynamicValue.Record(id, values) =>
            Right(DynamicValue.Record(id, values - name))
          case _ => Left(MigrationError("Expected Record", parent))
        }
      case _ => Left(MigrationError("Remove only supported for fields", path))
    }
  }
}
