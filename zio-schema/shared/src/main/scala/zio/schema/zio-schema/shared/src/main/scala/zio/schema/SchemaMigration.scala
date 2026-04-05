package zio.schema

import zio.Chunk
import zio.schema.DynamicValue._
import zio.schema.Schema._
import scala.quoted._
import java.util.{Collections, WeakHashMap}

sealed trait SchemaMigration {
  def apply(s: Schema[_]): Either[String, Schema[_]]
  def transform(v: DynamicValue): DynamicValue
  def reverse: SchemaMigration
}

object SchemaMigration {

  /**
   * Thread-safe & Memory-safe cache for DynamicValue transformations.
   * Using a synchronized WeakHashMap ensures automatic eviction of entries,
   * preventing memory leaks in high-concurrency, long-running ZIO environments.
   */
  private val transformCache = Collections.synchronizedMap(new WeakHashMap[(SchemaMigration, DynamicValue), DynamicValue]())

  def memoizedTransform(m: SchemaMigration, v: DynamicValue): DynamicValue = {
    val key = (m, v)
    val cached = transformCache.get(key)
    if (cached != null) cached
    else {
      val result = m.transform(v)
      transformCache.put(key, result)
      result
    }
  }

  final case class AddField(name: String, schema: Schema[_], default: DynamicValue) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = s match {
      case r: Record[_] => 
        Right(record((r.fields :+ Field(name, schema, get = _ => default)): _*))
      case _ => Left(s"Target is not a record")
    }
    def transform(v: DynamicValue): DynamicValue = v match {
      case Record(id, fields) => Record(id, fields.updated(name, default))
      case _ => v
    }
    def reverse: SchemaMigration = RemoveField(name, schema, default)
  }

  final case class RemoveField(name: String, schema: Schema[_], default: DynamicValue) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = s match {
      case r: Record[_] => Right(record(r.fields.filterNot(_.id == name): _*))
      case _ => Left("Target is not a record")
    }
    def transform(v: DynamicValue): DynamicValue = v match {
      case Record(id, fields) => Record(id, fields.removed(name))
      case _ => v
    }
    def reverse: SchemaMigration = AddField(name, schema, default)
  }

  final case class RenameField(from: String, to: String) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = s match {
      case r: Record[_] =>
        val updatedFields = r.fields.map { f =>
          if (f.id == from) Field(to, f.schema, get = f.get) else f
        }
        Right(record(updatedFields: _*))
      case _ => Left("Target is not a record")
    }
    def transform(v: DynamicValue): DynamicValue = v match {
      case Record(id, fields) => 
        fields.get(from).map(d => Record(id, fields.removed(from).updated(to, d))).getOrElse(v)
      case _ => v
    }
    def reverse: SchemaMigration = RenameField(to, from)
  }

  final case class UpdateNestedField(name: String, migration: SchemaMigration) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = s match {
      case r: Record[_] =>
        val updated = r.fields.map { f =>
          if (f.id == name) migration.apply(f.schema).map(newS => Field(name, newS, f.get)) else Right(f)
        }
        updated.foldLeft[Either[String, Chunk[Field[_, _]]]](Right(Chunk.empty))((acc, e) => for (c <- acc; f <- e) yield c :+ f)
          .map(fields => record(fields: _*))
      case _ => Left("Nested update failed")
    }
    def transform(v: DynamicValue): DynamicValue = v match {
      case Record(id, fields) => fields.get(name).map(d => Record(id, fields.updated(name, memoizedTransform(migration, d)))).getOrElse(v)
      case _ => v
    }
    def reverse: SchemaMigration = UpdateNestedField(name, migration.reverse)
  }

  final case class Combined(migrations: Chunk[SchemaMigration]) extends SchemaMigration {
    def apply(s: Schema[_]): Either[String, Schema[_]] = migrations.foldLeft[Either[String, Schema[_]]](Right(s))((acc, m) => acc.flatMap(m.apply))
    def transform(v: DynamicValue): DynamicValue = migrations.foldLeft(v)((acc, m) => memoizedTransform(m, acc))
    def reverse: SchemaMigration = Combined(migrations.reverse.map(_.reverse))
  }

  def diff(from: Schema[_], to: Schema[_]): Either[String, SchemaMigration] = {
    if (from == to) Right(Combined(Chunk.empty))
    else (from, to) match {
      case (f: Record[_], t: Record[_]) =>
        val fFields = f.fields.zipWithIndex.map { case (f, idx) => f.id -> (f, idx) }.toMap
        val tFields = t.fields.zipWithIndex.map { case (f, idx) => f.id -> (f, idx) }.toMap

        val updates = (fFields.keySet intersect tFields.keySet).flatMap { id =>
          val fSchema = fFields(id)._1.schema
          val tSchema = tFields(id)._1.schema
          if (fSchema == tSchema) None
          else diff(fSchema, tSchema).toOption match {
            case Some(Combined(c)) if c.isEmpty => None
            case Some(m) => Some(UpdateNestedField(id, m))
            case _ => None
          }
        }

        val removedIds = fFields.keySet -- tFields.keySet
        val addedIds = tFields.keySet -- fFields.keySet
        
        /**
         * Identity Recognition Heuristic:
         * We resolve renames using structural equality and positional proximity (Index Diff < 2).
         * This minimizes false positives during structural refactoring.
         */
        val renames = (for {
          rId <- removedIds.toSeq
          aId <- addedIds.toSeq
          if fFields(rId)._1.schema == tFields(aId)._1.schema && Math.abs(fFields(rId)._2 - tFields(aId)._2) < 2
        } yield RenameField(rId, aId)).toSet

        val currentRemoved = removedIds -- renames.map(_.from)
        val currentAdded = addedIds -- renames.map(_.to)

        val removals = currentRemoved.map { id => 
          val f = fFields(id)._1
          RemoveField(id, f.schema, f.schema.defaultValue.getOrElse(DynamicValue.None))
        }

        val additions = currentAdded.foldLeft[Either[String, List[AddField]]](Right(Nil)) { (acc, id) =>
          val f = tFields(id)._1
          val defaultOpt = f.schema.defaultValue.orElse {
            if (f.schema.isInstanceOf[Schema.Optional[_]]) Some(DynamicValue.None) else None
          }
          for {
            list <- acc
            default <- defaultOpt.toRight(s"Missing default value for required field: $id")
          } yield list :+ AddField(id, f.schema, default)
        }

        additions.map { adds =>
          Combined(Chunk.fromIterable(renames) ++ Chunk.fromIterable(removals) ++ Chunk.fromIterable(adds) ++ Chunk.fromIterable(updates))
        }

      case _ => Left(s"Structural mismatch")
    }
  }

  /**
   * Compile-time Case Class Field Selection.
   * Validates field existence against the type symbol at compile-time using Scala 3 Macros.
   */
  inline def selectField[T](inline name: String): String = ${ selectFieldImpl[T]('name) }

  private def selectFieldImpl[T: Type](name: Expr[String])(using Quotes): Expr[String] = {
    import quotes.reflect._
    val fieldName = name.valueOrAbort
    val tpe = TypeRepr.of[T]
    if (!tpe.typeSymbol.caseFields.exists(_.name == fieldName)) {
      report.errorAndAbort(s"Field '$fieldName' does not exist in ${tpe.show}")
    }
    Expr(fieldName)
  }
}
