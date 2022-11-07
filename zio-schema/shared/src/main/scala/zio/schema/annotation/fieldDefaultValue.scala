package zio.schema.annotation

final case class fieldDefaultValue[A](value: A) extends scala.annotation.StaticAnnotation
