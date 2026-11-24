package zio.schema

/**
 * Platform-specific schemas that the [[Schema]] companion object inherits. On
 * the JVM it adds an implicit schema for `java.net.URL`; on Scala.js and Scala
 * Native it adds nothing.
 */
trait SchemaPlatformSpecific {}
