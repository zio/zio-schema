package zio.schema.validation

/**
 * A boolean expression tree whose leaves hold values of type `A`, built with `&&`, `||` and `!`. `Validation` keeps its predicates in a `Bool[Predicate[A]]`.
 */
sealed trait Bool[A] { self =>
  /** Returns a `Bool.And` of this expression and `that`. */
  def &&(that: Bool[A]): Bool[A] = Bool.And(self, that)
  /** Returns a `Bool.Or` of this expression and `that`. */
  def ||(that: Bool[A]): Bool[A] = Bool.Or(self, that)
  /** Returns a `Bool.Not` that negates this expression. */
  def unary_! : Bool[A]          = Bool.Not(self)

  /** Returns the same tree with `f` applied to the value of each leaf. */
  def map[B](f: A => B): Bool[B] = self match {
    case Bool.And(left, right) => Bool.And(left.map(f), right.map(f))
    case Bool.Or(left, right)  => Bool.Or(left.map(f), right.map(f))
    case Bool.Leaf(value)      => Bool.Leaf(f(value))
    case Bool.Not(value)       => Bool.Not(value.map(f))
  }
}

/** The node types of a `Bool` expression. */
object Bool {
  /** The conjunction of `left` and `right`. */
  final case class And[A](left: Bool[A], right: Bool[A]) extends Bool[A]
  /** The disjunction of `left` and `right`. */
  final case class Or[A](left: Bool[A], right: Bool[A])  extends Bool[A]
  /** A leaf that holds one `value`, such as a `Predicate`. */
  final case class Leaf[A](value: A)                     extends Bool[A]
  /** The negation of `value`. */
  final case class Not[A](value: Bool[A])                extends Bool[A]
}
