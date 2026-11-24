package zio.schema

/**
 * Adds the Scala 3 `derive` method to `Deriver`. On Scala 2 it has no members.
 */
trait VersionSpecificDeriver[F[_]] {}
