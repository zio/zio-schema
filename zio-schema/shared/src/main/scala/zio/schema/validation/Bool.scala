package zio.schema.validation

sealed trait Bool[A] { self =>
  def &&(that: Bool[A]): Bool[A] = Bool.And(self, that)
  def ||(that: Bool[A]): Bool[A] = Bool.Or(self, that)
  def unary_! : Bool[A]          = Bool.Not(self)
}

object Bool {
  final case class And[A](left: Bool[A], right: Bool[A]) extends Bool[A]
  final case class Or[A](left: Bool[A], right: Bool[A])  extends Bool[A]
  final case class Leaf[A](value: A)                     extends Bool[A]
  final case class Not[A](value: Bool[A])                extends Bool[A]
}
