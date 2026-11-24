package zio.schema

/**
 * Platform-specific schemas that the [[Schema]] companion object inherits. On
 * the JVM it adds an implicit schema for `java.net.URL`; on Scala.js and Scala
 * Native it adds nothing.
 */
trait SchemaPlatformSpecific {

  /**
   * A schema for `java.net.URL`, encoded as its string form. Decoding parses
   * the string with `java.net.URI` and `toURL`, and fails with `Invalid URL: `
   * followed by the input when they reject it (for example
   * `Invalid URL: foo:bar`). Available on the JVM only.
   */
  implicit val url: Schema[java.net.URL] = {
    val f = (string: String) =>
      try {
        Right(new java.net.URI(string).toURL)
      } catch { case _: Exception => Left(s"Invalid URL: $string") }
    val g = (url: java.net.URL) => Right(url.toString)
    Schema[String].transformOrFail(f, g)
  }

}
