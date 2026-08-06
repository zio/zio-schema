package zio.schema.migration

import zio.Chunk
import zio.schema._

/**
 * Represents atomic structural changes between schema versions.
 */
sealed trait MigrationAction {
  def apply(dv: DynamicValue): Either[MigrationError, DynamicValue]
  def reverseAction: MigrationAction
}

object MigrationAction {

  /**
   * Adds a field to a record.
   */
  case class AddField(path: Chunk[String], schema: Schema[_], default: DynamicValue) extends MigrationAction {
    def apply(dv: DynamicValue): Either[MigrationError, DynamicValue] = 
      updateAtPath(dv, path.init) {
        case DynamicValue.Record(id, values) => DynamicValue.Record(id, values + (path.last -> default))
        case other => other
      }
    def reverseAction: MigrationAction = DropField(path, default)
  }

  /**
   * Removes a field from a record. Requires a default value for reversibility.
   */
  case class DropField(path: Chunk[String], defaultForReverse: DynamicValue) extends MigrationAction {
    def apply(dv: DynamicValue): Either[MigrationError, DynamicValue] = 
      updateAtPath(dv, path.init) {
        case DynamicValue.Record(id, values) => DynamicValue.Record(id, values - path.last)
        case other => other
      }
    def reverseAction: MigrationAction = AddField(path, Schema.primitive[String], defaultForReverse) 
  }

  /**
   * Renames a field at a given path.
   */
  case class RenameField(path: Chunk[String], from: String, to: String) extends MigrationAction {
    def apply(dv: DynamicValue): Either[MigrationError, DynamicValue] = 
      updateAtPath(dv, path) {
        case DynamicValue.Record(id, values) =>
          values.get(from) match {
            case Some(value) => DynamicValue.Record(id, (values - from) + (to -> value))
            case None => DynamicValue.Record(id, values)
          }
        case other => other
      }
    def reverseAction: MigrationAction = RenameField(path, to, from)
  }

  /**
   * Transforms a field value using a ZIO Schema Expression.
   */
  case class TransformValue(path: Chunk[String], transformation: SchemaExpr) extends MigrationAction {
    def apply(dv: DynamicValue): Either[MigrationError, DynamicValue] = 
      updateAtPath(dv, path) { dv =>
        transformation.apply(dv)
      }
    def reverseAction: MigrationAction = TransformValue(path, transformation) 
  }

  trait SchemaExpr {
    def apply(dv: DynamicValue): DynamicValue
  }

  private def updateAtPath(dv: DynamicValue, path: Chunk[String])(f: DynamicValue => DynamicValue): Either[MigrationError, DynamicValue] = {
    def loop(current: DynamicValue, remaining: Chunk[String]): DynamicValue = {
      if (remaining.isEmpty) f(current)
      else current match {
        case DynamicValue.Record(id, values) =>
          val head = remaining.head
          values.get(head) match {
            case Some(nested) => DynamicValue.Record(id, values + (head -> loop(nested, remaining.tail)))
            case None => current
          }
        case _ => current
      }
    }
    Right(loop(dv, path))
  }
}
