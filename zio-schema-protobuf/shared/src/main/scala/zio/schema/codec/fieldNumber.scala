package zio.schema.codec

import scala.annotation.StaticAnnotation

final case class fieldNumber(n: Int) extends StaticAnnotation
