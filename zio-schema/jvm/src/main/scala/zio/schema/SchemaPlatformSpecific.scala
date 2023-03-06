package zio.schema

import java.net.URL

trait SchemaPlatformSpecific {

  implicit val url: Schema[java.net.URL] =
    Schema[String].transformOrFail(
      string =>
        try {
          Right(new URL(string))
        } catch {
          case _: Exception => Left(s"Invalid URL: $string")
        },
      url => Right(url.toString)
    )
}
