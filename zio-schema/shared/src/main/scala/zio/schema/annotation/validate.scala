package zio.schema.annotation

import zio.schema.validation.Validation

final case class validate[A](validation: Validation[A]) extends scala.annotation.StaticAnnotation
