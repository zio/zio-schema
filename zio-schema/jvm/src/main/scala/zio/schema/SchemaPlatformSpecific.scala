package zio.schema

trait SchemaPlatformSpecific {

  implicit val url: Schema[java.net.URL] = {
    val f = (string: String) =>
      try {
        Right(new java.net.URI(string).toURL)
      } catch { case _: Exception => Left(s"Invalid URL: $string") }
    val g = (url: java.net.URL) => Right(url.toString)
    Schema[String].transformOrFail(f, g)
  }

}
