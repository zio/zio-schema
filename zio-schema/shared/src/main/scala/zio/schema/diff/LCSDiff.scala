package zio.schema.diff

sealed trait Edit[A]

object Edit {
  case class Insert[A](value: A) extends Edit[A]
  case class Delete[A](value: A) extends Edit[A]
  case class Keep[A](value: A)   extends Edit[A]
}
