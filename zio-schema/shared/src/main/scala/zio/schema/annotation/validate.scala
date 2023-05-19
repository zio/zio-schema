package zio.schema.annotation

import zio.schema.validation.SchemaValidation

final case class validate[A](validation: SchemaValidation[A]) extends scala.annotation.StaticAnnotation
