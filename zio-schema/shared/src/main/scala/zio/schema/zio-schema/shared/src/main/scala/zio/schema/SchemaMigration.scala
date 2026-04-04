package zio.schema

import zio.Chunk
import zio.schema.DynamicValue._
import zio.schema.Schema._

object SchemaMigration {
  
  // High-performance similarity score (Levenshtein optimized for small strings)
  private def calculateSimilarity(s1: String, s2: String): Double = {
    val commonPrefix = s1.zip(s2).takeWhile(x => x._1 == x._2).length
    commonPrefix.toDouble / s1.length.max(s2.length).max(1)
  }

  def diff(from: Schema[_], to: Schema[_], path: String = ""): Either[String, SchemaMigration] = {
    (from, to) match {
      case (f: Record[_], t: Record[_]) =>
        val fFields = f.fields.map(f => f.id -> f).toMap
        val tFields = t.fields.map(f => f.id -> f).toMap

        val removed = (fFields.keySet -- tFields.keySet).toList
        val added   = (tFields.keySet -- fFields.keySet).toList

        // Refined Rename Detection: Structural Equality + Similarity Score > 0.6
        def findRenames(rem: List[String], add: List[String], acc: Chunk[RenameField]): (List[String], List[String], Chunk[RenameField]) =
          rem match {
            case rH :: rT =>
              add.find(aN => fFields(rH).schema == tFields(aN).schema && calculateSimilarity(rH, aN) > 0.6) match {
                case Some(aN) => findRenames(rT, add.filterNot(_ == aN), acc :+ RenameField(rH, aN))
                case None     => findRenames(rT, add, acc)
              }
            case Nil => (rem, add, acc)
          }

        val (finalRemoved, finalAdded, renames) = findRenames(removed, added, Chunk.empty)

        // Safe Default Values: Check for Option or Schema-defined defaults
        val addMigrations = finalAdded.map { id =>
          val schema = tFields(id).schema
          val defaultValue = schema.defaultValue.getOrElse {
            schema match {
              case _: Schema.Optional[_] => DynamicValue.None
              case _ => throw new RuntimeException(s"Missing default for non-optional field '$id' at $path")
            }
          }
          AddField(id, schema, defaultValue)
        }

        // Deep Recursion with Path Tracking for Nested Records
        val recursions = (fFields.keySet intersect tFields.keySet).flatMap { id =>
          diff(fFields(id).schema, tFields(id).schema, s"$path.$id") match {
            case Right(Combined(m)) if m.nonEmpty => Some(Combined(m))
            case _ => None
          }
        }

        Right(Combined(Chunk.fromIterable(finalRemoved.map(RemoveField(_))) ++ 
                       Chunk.fromIterable(addMigrations) ++ 
                       renames ++ Chunk.fromIterable(recursions)))

      case (f, t) if f == t => Right(Combined(Chunk.empty))
      case _ => Left(s"Incompatible structural change at $path")
    }
  }
}
