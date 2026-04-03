package zio.schema

import zio.Chunk
import zio.schema.Schema._

/**
 * ZIO Schema Migration System
 * Provides functionality to migrate data between different versions of a Schema.
 */
sealed trait SchemaMigration { self =>
  def apply(schema: Schema[_]): Either[String, Schema[_]]
}

object SchemaMigration {

  // Step 1: Add a new field to a Record
  final case class AddField(name: String, fieldSchema: Schema[_]) extends SchemaMigration {
    def apply(schema: Schema[_]): Either[String, Schema[_]] = schema match {
      case record: Record[_] => 
        val updatedFields = record.fields :+ Field(name, fieldSchema)
        Right(Schema.record(updatedFields: _*))
      case _ => Left(s"Target is not a Record: $schema")
    }
  }

  // Step 2: Remove an existing field from a Record
  final case class RemoveField(name: String) extends SchemaMigration {
    def apply(schema: Schema[_]): Either[String, Schema[_]] = schema match {
      case record: Record[_] =>
        val updatedFields = record.fields.filterNot(_.name == name)
        Right(Schema.record(updatedFields: _*))
      case _ => Left(s"Target is not a Record: $schema")
    }
  }

  // Step 3: Rename a field while keeping the schema intact
  final case class RenameField(oldName: String, newName: String) extends SchemaMigration {
    def apply(schema: Schema[_]): Either[String, Schema[_]] = schema match {
      case record: Record[_] =>
        val updatedFields = record.fields.map { field =>
          if (field.name == oldName) field.copy(name = newName) else field
        }
        Right(Schema.record(updatedFields: _*))
      case _ => Left(s"Target is not a Record: $schema")
    }
  }

  // Step 4: Batch multiple migrations together
  final case class Combined(migrations: Chunk[SchemaMigration]) extends SchemaMigration {
    def apply(schema: Schema[_]): Either[String, Schema[_]] =
      migrations.foldLeft[Either[String, Schema[_]]](Right(schema)) { (acc, migration) =>
        acc.flatMap(migration.apply)
      }
  }

  /**
   * Generates a migration by comparing two Record schemas.
   */
  def diff(from: Schema[_], to: Schema[_]): Either[String, SchemaMigration] = {
    (from, to) match {
      case (f: Record[_], t: Record[_]) =>
        val fFields = f.fields.map(_.name).toSet
        val tFields = t.fields.map(_.name).toSet

        val removed = (fFields -- tFields).map(RemoveField)
        val added = (tFields -- fFields).map(name => 
          AddField(name, t.fields.find(_.name == name).get.schema)
        )
        
        Right(Combined(Chunk.fromIterable(removed ++ added)))
        
      case _ => Left("Schema diffing is currently only supported for Record types.")
    }
  }
}
