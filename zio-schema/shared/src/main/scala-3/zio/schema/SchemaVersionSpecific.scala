package zio.schema

import zio.*
import scala.reflect.ClassTag

/**
 * Implicit schemas that exist only on some Scala versions. On Scala 2 it is empty; on Scala 3 it provides the schema of `IArray`.
 */
trait SchemaVersionSpecific {
  /**
   * The schema of a Scala 3 `IArray` of `A`, a [[Schema.Sequence]] identified by `"IArray"`. Scala 3 only.
   */
  implicit def iArray[A: ClassTag](implicit schemaA: Schema[A]): Schema[IArray[A]] =
    Schema.Sequence[IArray[A], A, String](schemaA, IArray.from, Chunk.fromIterable, Chunk.empty, "IArray")

}
