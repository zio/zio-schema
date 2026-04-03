package zio.schema

import zio.Chunk
import zio.schema.DynamicValue._
import zio.schema.Schema._

sealed trait SchemaMigration { self =>
  def apply(schema: Schema[_]): Either[String, Schema[_]]
  def transform(value: DynamicValue): DynamicValue
}

object SchemaMigration {

  final case class Combined(migrations: Chunk[SchemaMigration]) extends SchemaMigration {
    def apply(schema: Schema[_]): Either[String, Schema[_]] =
      migrations.foldLeft[Either[String, Schema[_]]](Right(schema))((acc, m) => acc.flatMap(m.apply))

    def transform(value: DynamicValue): DynamicValue =
      migrations.foldLeft(value)((v, m) => m.transform(v))
  }

  def diff(from: Schema[_], to: Schema[_]): Either[String, SchemaMigration] = {
    (from, to) match {
      case (f: Record[_], t: Record[_]) =>
        val fFields = f.fields.map(field => field.id -> field).toMap
        val tFields = t.fields.map(field => field.id -> field).toMap

        val removedNames = fFields.keySet -- tFields.keySet
        val addedNames   = tFields.keySet -- fFields.keySet
        val commonNames  = fFields.keySet intersect tFields.keySet

        var matchedOld = Set.empty[String]
        var matchedNew = Set.empty[String]

        // 1. Intelligent Rename Detection (Forced Evaluation)
        val renames = (for {
          oldId <- removedNames.toList if !matchedOld.contains(oldId)
          newId <- addedNames.toList   if !matchedNew.contains(newId) && fFields(oldId).schema == tFields(newId).schema
        } yield {
          matchedOld += oldId
          matchedNew += newId
          RenameField(oldId, newId)
        }).toList

        // 2. Actual Removals and Additions
        val removals  = (removedNames -- matchedOld).map(RemoveField(_)).toList
        val additions = (addedNames -- matchedNew).map(id => AddField(id, tFields(id).schema, DynamicValue.None)).toList

        // 3. Deep Recursive Diffing for Common Fields
        val recursions = commonNames.flatMap { id =>
          diff(fFields(id).schema, tFields(id).schema) match {
            case Right(Combined(m)) if m.isEmpty => None
            case Right(m) => Some(m)
            case _ => None
          }
        }.toList

        Right(Combined(Chunk.fromIterable(removals ++ additions ++ renames ++ recursions)))

      case (f, t) if f == t => Right(Combined(Chunk.empty))
      case _ => Left("Schemas are structurally incompatible for migration")
    }
  }

  final case class RenameField(oldName: String, newName: String) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = s match {
      case r: Record[_] => 
        val updatedFields = r.fields.map(f => if (f.id == oldName) f.copy(id = newName) else f)
        Right(Schema.record(updatedFields: _*))
      case _ => Left("Target must be a Record for renaming")
    }
    def transform(v: DynamicValue): DynamicValue = v match {
      case DynamicValue.Record(id, fields) => 
        fields.get(oldName).map(data => DynamicValue.Record(id, (fields - oldName).updated(newName, data))).getOrElse(v)
      case other => other
    }
  }

  final case class AddField(name: String, schema: Schema[_], default: DynamicValue) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = s match {
      case r: Record[_] => 
        if (r.fields.exists(_.id == name)) Left(s"Field $name already exists")
        else Right(Schema.record(r.fields :+ Field(name, schema): _*))
      case _ => Left("Target must be a Record for field addition")
    }
    def transform(v: DynamicValue): DynamicValue = v match {
      case DynamicValue.Record(id, fields) => DynamicValue.Record(id, fields.updated(name, default))
      case other => other
    }
  }

  final case class RemoveField(name: String) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = s match {
      case r: Record[_] => Right(Schema.record(r.fields.filterNot(_.id == name): _*))
      case _ => Left("Target must be a Record for field removal")
    }
    def transform(v: DynamicValue): DynamicValue = v match {
      case DynamicValue.Record(id, fields) => DynamicValue.Record(id, fields.removed(name))
      case other => other
    }
  }
}
