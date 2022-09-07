package zio.schema.diff

sealed trait Edit[A]

object Edit {

  def invert[A](edit: Edit[A]): Edit[A] = edit match {
    case Insert(value) => Delete(value)
    case Delete(value) => Insert(value)
    case Keep(value)   => Keep(value)
  }
  final case class Insert[A](value: A) extends Edit[A]
  final case class Delete[A](value: A) extends Edit[A]
  final case class Keep[A](value: A)   extends Edit[A]
}
