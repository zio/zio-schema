package zio.schema

import zio.*
import scala.reflect.ClassTag

trait SchemaVersionSpecific {
  implicit def iArray[A: ClassTag](implicit schemaA: Schema[A]): Schema[IArray[A]] =
    Schema.Sequence[IArray[A], A, String](schemaA, IArray.from, Chunk.fromIterable, Chunk.empty, "IArray")

}
