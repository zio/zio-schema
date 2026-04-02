package zio.schema

sealed trait MigrationAction {
  def at: DynamicOptic
  def reverse: MigrationAction
}

object MigrationAction {

  case class AddField(at: DynamicOptic, default: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = DropField(at, default)
  }

  case class DropField(at: DynamicOptic, defaultForReverse: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = AddField(at, defaultForReverse)
  }

  case class Rename(at: DynamicOptic, to: String) extends MigrationAction {
    def reverse: MigrationAction = {
      val newAt = at match {
        case DynamicOptic.Field(parent, _) => DynamicOptic.Field(parent, to)
        case other => other // Should probably not happen for Rename on fields
      }
      val oldName = at match {
        case DynamicOptic.Field(_, name) => name
        case _                           => ""
      }
      Rename(newAt, oldName)
    }
  }

  case class TransformValue(at: DynamicOptic, transform: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = this // Simplification for now, some transforms are invertible
  }

  case class Mandate(at: DynamicOptic, default: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = Optionalize(at)
  }

  case class Optionalize(at: DynamicOptic) extends MigrationAction {
    def reverse: MigrationAction = Mandate(at, SchemaExpr.DefaultValue)
  }

  case class ChangeType(at: DynamicOptic, converter: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = this // Simplification
  }

  case class RenameCase(at: DynamicOptic, from: String, to: String) extends MigrationAction {
    def reverse: MigrationAction = RenameCase(at, to, from)
  }

  case class TransformCase(at: DynamicOptic, actions: Vector[MigrationAction]) extends MigrationAction {
    def reverse: MigrationAction = TransformCase(at, actions.reverse.map(_.reverse))
  }

  case class TransformElements(at: DynamicOptic, transform: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = this
  }

  case class TransformKeys(at: DynamicOptic, transform: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = this
  }

  case class Join(at: DynamicOptic, sourcePaths: Chunk[DynamicOptic], combiner: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = Split(at, sourcePaths, combiner) // Simplification
  }

  case class Split(at: DynamicOptic, targetPaths: Chunk[DynamicOptic], splitter: SchemaExpr[_]) extends MigrationAction {
    def reverse: MigrationAction = Join(at, targetPaths, splitter) // Simplification
  }
}

case class MigrationError(message: String, path: DynamicOptic)
