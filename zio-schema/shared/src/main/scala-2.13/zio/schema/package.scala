package zio

/**
 * Type aliases that differ between Scala versions. On Scala 3, it also holds extension methods that merge the two sides of a [[Fallback]].
 */
package object schema {
  /**
   * The bound used for singleton field-name types such as `Schema.Field#Field`. It is `scala.Singleton` on Scala 2.13 and 3, and `Any` on Scala 2.12, which has no `Singleton` type.
   */
  type Singleton = scala.Singleton
}
