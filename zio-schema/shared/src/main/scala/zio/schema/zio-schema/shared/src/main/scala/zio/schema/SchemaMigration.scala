package zio.schema

import zio.Chunk
import zio.schema.DynamicValue._
import zio.schema.Schema._

object SchemaMigration {
  
  def diff(from: Schema[_], to: Schema[_], path: String = "", seen: Set[(Schema[_], Schema[_])] = Set.empty): Either[String, SchemaMigration] = {
    if (seen.contains((from, to)) || from == to) Right(Combined(Chunk.empty))
    else {
      val nextSeen = seen + ((from, to))
      (from, to) match {
        case (f: Record[_], t: Record[_]) =>
          val fFields = f.fields.map(f => f.id -> f).toMap
          val tFields = t.fields.map(f => f.id -> f).toMap

          val removedIds = fFields.keySet -- tFields.keySet
          val addedIds   = tFields.keySet -- fFields.keySet
          
          val renames = for {
            rId <- removedIds.toList
            aId <- addedIds.toList
            if fFields(rId).schema == tFields(aId).schema
          } yield RenameField(rId, aId)

          val actualRemoved = removedIds -- renames.map(_.from)
          val actualAdded   = addedIds -- renames.map(_.to)

          val addedMigs = actualAdded.foldLeft[Either[String, Chunk[SchemaMigration]]](Right(Chunk.empty)) { (acc, id) =>
            for {
              currentAcc <- acc
              schema = tFields(id).schema
              default <- schema.defaultValue.orElse(if(schema.isInstanceOf[Optional[_]]) Some(DynamicValue.None) else None)
                .toRight(s"No default for $id")
            } yield currentAcc :+ AddField(id, schema, default)
          }

          for {
            a <- addedMigs
            r = Chunk.fromIterable(actualRemoved.map(RemoveField(_)))
            rn = Chunk.fromIterable(renames)
            u <- (fFields.keySet intersect tFields.keySet).foldLeft[Either[String, Chunk[SchemaMigration]]](Right(Chunk.empty)) { (acc, id) =>
              for {
                curr <- acc
                m <- diff(fFields(id).schema, tFields(id).schema, s"$path.$id", nextSeen)
                combined = m match {
                  case Combined(c) if c.nonEmpty => Some(UpdateNestedField(id, m))
                  case _ => None
                }
              } yield curr ++ Chunk.fromIterable(combined)
            }
          } yield Combined(r ++ a ++ rn ++ u)

        case _ => Left(s"Structural mismatch at $path")
      }
    }
  }

  sealed trait SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]]
    def transform(v: DynamicValue): DynamicValue
  }

  final case class UpdateNestedField(name: String, migration: SchemaMigration) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = Right(s) 
    def transform(v: DynamicValue): DynamicValue = v match {
      case DynamicValue.Record(id, fields) => 
        // FIXED: Now defaults to DynamicValue.None for safety if field is missing
        val updated = fields.get(name).map(migration.transform).getOrElse(DynamicValue.None)
        DynamicValue.Record(id, fields.updated(name, updated))
      case _ => v
    }
  }

  final case class RenameField(from: String, to: String) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = Right(s) 
    def transform(v: DynamicValue): DynamicValue = v match {
      case DynamicValue.Record(id, fields) => 
        fields.get(from).map(d => DynamicValue.Record(id, fields.removed(from).updated(to, d))).getOrElse(v)
      case _ => v
    }
  }

  final case class AddField(name: String, schema: Schema[_], default: DynamicValue) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = Right(s)
    def transform(v: DynamicValue): DynamicValue = v match {
      case DynamicValue.Record(id, fields) => DynamicValue.Record(id, fields.updated(name, default))
      case _ => v
    }
  }

  final case class RemoveField(name: String) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = Right(s)
    def transform(v: DynamicValue): DynamicValue = v match {
      case DynamicValue.Record(id, fields) => DynamicValue.Record(id, fields.removed(name))
      case _ => v
    }
  }

  final case class Combined(migrations: Chunk[SchemaMigration]) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = migrations.foldLeft[Either[String, Schema[_]]](Right(s))((acc, m) => acc.flatMap(m.apply))
    def transform(v: DynamicValue): DynamicValue = migrations.foldLeft(v)((acc, m) => m.transform(acc))
  }
}
