package zio.schema

case class Migration[A, B](
  dynamicMigration: DynamicMigration,
  sourceSchema: Schema[A],
  targetSchema: Schema[B]
) {
  def apply(value: A): Either[MigrationError, B] = {
    val dynamicValue = sourceSchema.toDynamic(value)
    dynamicMigration.apply(dynamicValue).flatMap { migratedDynamic =>
      migratedDynamic.toTypedValue(targetSchema).left.map(err => MigrationError(err, DynamicOptic.Root))
    }
  }

  def ++[C](that: Migration[B, C]): Migration[A, C] =
    Migration(this.dynamicMigration ++ that.dynamicMigration, this.sourceSchema, that.targetSchema)

  def andThen[C](that: Migration[B, C]): Migration[A, C] = this ++ that

  def reverse: Migration[B, A] =
    Migration(this.dynamicMigration.reverse, targetSchema, sourceSchema)
}

object Migration {
  def identity[A](implicit schema: Schema[A]): Migration[A, A] =
    Migration(DynamicMigration(zio.Chunk.empty), schema, schema)
}
