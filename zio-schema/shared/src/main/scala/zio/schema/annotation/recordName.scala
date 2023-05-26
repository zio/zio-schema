package zio.schema.annotation

import scala.annotation.StaticAnnotation

final case class recordName(name: String) extends StaticAnnotation
