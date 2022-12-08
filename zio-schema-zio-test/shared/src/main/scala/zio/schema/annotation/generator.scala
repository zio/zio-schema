package zio.schema.annotation

import zio.test.{ Gen, Sized }

final case class generator[A](gen: Gen[Sized, A]) extends scala.annotation.StaticAnnotation
