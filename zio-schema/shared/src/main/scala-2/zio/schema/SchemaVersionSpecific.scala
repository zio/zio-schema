package zio.schema

/**
 * Implicit schemas that exist only on some Scala versions. On Scala 2 it is
 * empty; on Scala 3 it provides the schema of `IArray`.
 */
trait SchemaVersionSpecific {}
