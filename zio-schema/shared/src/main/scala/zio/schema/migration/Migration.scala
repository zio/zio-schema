package zio.schema.migration

import zio.Chunk
import zio.schema._

/**
 * ZIO #519: Pure Algebraic Migration System
 * Represents a collection of atomic changes between two schema versions.
 */
case class Migration[A, B](actions: Chunk[MigrationAction]) {
  
  /**
   * Chains two migrations together.
   */
  def ++[C](other: Migration[B, C]): Migration[A, C] =
    Migration(actions ++ other.actions)

  /**
   * Generates a structural reverse migration.
   * Note: Some information might be lossy (e.g. DropField), so we use default values.
   */
  def reverse: Migration[B, A] =
    Migration(actions.reverse.map(_.reverseAction))

  /**
   * Applies the migration to a DynamicValue.
   */
  def apply(dv: DynamicValue): Either[MigrationError, DynamicValue] = {
    actions.foldLeft[Either[MigrationError, DynamicValue]](Right(dv)) { (acc, action) =>
      acc.flatMap(action.apply)
    }
  }
}

object Migration {
  def identity[A]: Migration[A, A] = Migration(Chunk.empty)
}

sealed trait MigrationError
object MigrationError {
  case class FieldNotFound(path: Chunk[String]) extends MigrationError
  case class SerializationError(msg: String) extends MigrationError
  case class TypeMismatch(expected: String, actual: String) extends MigrationError
}
