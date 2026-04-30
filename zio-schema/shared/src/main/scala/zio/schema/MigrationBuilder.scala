package zio.schema

import zio.Chunk

class MigrationBuilder[A, B](
  sourceSchema: Schema[A],
  targetSchema: Schema[B],
  actions: Chunk[MigrationAction]
) {

  // Record operations

  def addField(at: DynamicOptic, default: SchemaExpr[_]): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ MigrationAction.AddField(at, default))

  def dropField(at: DynamicOptic, defaultForReverse: SchemaExpr[_] = SchemaExpr.DefaultValue): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ MigrationAction.DropField(at, defaultForReverse))

  def renameField(from: DynamicOptic, to: String): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ MigrationAction.Rename(from, to))

  def transformField(at: DynamicOptic, transform: SchemaExpr[_]): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ MigrationAction.TransformValue(at, transform))

  def mandateField(at: DynamicOptic, default: SchemaExpr[_]): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ MigrationAction.Mandate(at, default))

  def optionalizeField(at: DynamicOptic): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ MigrationAction.Optionalize(at))

  def changeFieldType(at: DynamicOptic, converter: SchemaExpr[_]): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ MigrationAction.ChangeType(at, converter))

  // Enum operations

  def renameCase(at: DynamicOptic, from: String, to: String): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ MigrationAction.RenameCase(at, from, to))

  def transformCase(at: DynamicOptic, caseMigration: Vector[MigrationAction]): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ MigrationAction.TransformCase(at, caseMigration))

  // Collections

  def transformElements(at: DynamicOptic, transform: SchemaExpr[_]): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ MigrationAction.TransformElements(at, transform))

  // Maps

  def transformKeys(at: DynamicOptic, transform: SchemaExpr[_]): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ MigrationAction.TransformKeys(at, transform))

  def transformValues(at: DynamicOptic, transform: SchemaExpr[_]): MigrationBuilder[A, B] =
    new MigrationBuilder(sourceSchema, targetSchema, actions :+ MigrationAction.TransformValues(at, transform))

  def build: Migration[A, B] = Migration(DynamicMigration(actions), sourceSchema, targetSchema)

  def buildPartial: Migration[A, B] = build
}

object MigrationBuilder {
  def apply[A, B](implicit source: Schema[A], target: Schema[B]): MigrationBuilder[A, B] =
    new MigrationBuilder(source, target, Chunk.empty)
}
