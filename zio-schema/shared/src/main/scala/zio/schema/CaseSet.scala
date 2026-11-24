package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.Schema._

/**
 * The cases of an enum schema as a type-level list, built from
 * [[CaseSet.Cons]] cells that end in [[CaseSet.Empty]]. `Schema.enumeration`
 * and `Schema.EnumN` take a `CaseSet` to describe an enum with any number of
 * cases; [[CaseSet.caseOf]] builds a one-case set and `++` joins sets.
 */
sealed trait CaseSet { self =>

  import CaseSet.:+:

  /** The type of the enum whose cases this set holds. */
  type EnumType

  /** The type of the tuple of prisms that [[makeAccessors]] returns, one per case. */
  type Accessors[Whole, Lens[_, _, _], Prism[_, _, _], Traversal[_, _]]

  /** Returns a set with `head` as its first case, followed by the cases of this set. */
  def :+:[A](head: Case[EnumType, A]): A :+: CaseSet.Aux[EnumType]

  // def ++[That](that: That)(implicit append: Append[EnumType, self.type, That]): append.Out

  /** Returns the schema of each case, keyed by case id, in case order. */
  def toMap: ListMap[String, Schema[_]]

  /** Returns the cases in order. */
  def toSeq: Seq[Case[EnumType, _]]

  /** Builds one prism per case with `b`, as nested pairs ending in `Unit`. */
  def makeAccessors(whole: Enum[EnumType], b: AccessorBuilder): Accessors[EnumType, b.Lens, b.Prism, b.Traversal]

}

/** The cells of a [[CaseSet]] and the ways to build one. */
object CaseSet {
  /** A [[CaseSet]] for the enum type `EnumType0`. */
  type Aux[EnumType0] = CaseSet { type EnumType = EnumType0 }

  /** The case set with no cases, which ends every [[CaseSet]]. */
  final case class Empty[Z]() extends CaseSet { self =>
    /** The enum type `Z`. */
    type EnumType = Z

    override type Accessors[Whole, Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Unit

    override def :+:[A](head: Case[EnumType, A]): A :+: Empty[EnumType] = Cons(head, self)

    /** Returns `that`, since this set has no cases. */
    def ++[That](that: That)(implicit append: Append[Z, Empty[Z], That]): append.Out =
      append(self, that)

    override def toMap: ListMap[String, Schema[_]] = ListMap.empty

    override def toSeq: Seq[Case[Z, _]] = Seq.empty

    override def makeAccessors(
      whole: Enum[EnumType],
      b: AccessorBuilder
    ): Accessors[EnumType, b.Lens, b.Prism, b.Traversal] = ()

    override def toString: String = "Empty"
  }

  /** Holds the [[Empty.Aux]] alias. */
  object Empty {
    /** An empty case set for the enum type `Z`. */
    type Aux[Z] = CaseSet.Empty[Z] { type EnumType = Z }
  }

  /** A case set whose first case has the type `A`, followed by the set `T`. */
  sealed trait :+:[A, +T <: CaseSet] extends CaseSet {
    /** The first case. */
    def head: Case[EnumType, A]
  }

  /** A case set cell: the case `head` followed by the cases in `tail`. */
  final case class Cons[A, +T <: CaseSet.Aux[Z], Z](head: Case[Z, A], tail: T) extends :+:[A, T] { self =>
    /** The enum type `Z`. */
    type EnumType = Z

    override type Accessors[Whole, Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Prism[head.id.type, Whole, A], tail.Accessors[Whole, Lens, Prism, Traversal])

    override def :+:[B](head2: Case[Z, B]): Cons[B, Cons[A, T, Z], Z] = Cons(head2, self)

    /** Returns a set with the cases of this set followed by the cases of `that`. */
    def ++[That](that: That)(implicit append: Append[Z, Cons[A, T, Z], That]): append.Out =
      append(self, that)

    override def toMap: ListMap[String, Schema[_]] = ListMap(head.id -> head.schema) ++ tail.toMap

    override def toSeq: Seq[Case[Z, _]] =
      Seq(head) ++ tail.toSeq

    override def makeAccessors(
      whole: Enum[EnumType],
      b: AccessorBuilder
    ): Accessors[EnumType, b.Lens, b.Prism, b.Traversal] =
      (b.makePrism(whole, head), tail.makeAccessors(whole, b))

    override def toString: String = s"$head :+: $tail"
  }
  /** Another name for the [[Cons]] companion, for building and matching sets. */
  val :+: = Cons

  /**
   * Builds a case set from the given cases, in order. The result type is
   * `CaseSet`, so the case types are not tracked.
   */
  def apply[Z](c: Case[Z, _]*): CaseSet = c.foldRight[CaseSet.Aux[Z]](Empty[Z]()) { case (c, cs) =>
    Cons(c, cs)
  }

  /**
   * Builds a one-case set for the subtype `A` of `Z`, with the case id `id`,
   * the implicit `Schema[A]` and no annotations.
   *
   * @param unsafeDeconstruct
   *   casts a `Z` that belongs to this case to `A`
   * @param construct
   *   widens an `A` to `Z`
   * @param isCase
   *   tells whether a `Z` belongs to this case
   */
  def caseOf[A: Schema, Z >: A](
    id: String
  )(unsafeDeconstruct: Z => A)(construct: A => Z)(isCase: Z => Boolean): Cons[A, Empty[Z], Z] =
    Cons(Case(id, Schema[A], unsafeDeconstruct, construct, isCase, Chunk.empty), Empty[Z]())

}

/** Holds the [[Cons0.Aux]] alias. */
object Cons0 {
  /** A [[CaseSet.Cons]] whose `EnumType` is known to be `Z`. */
  type Aux[A, T <: CaseSet.Aux[Z], Z] = CaseSet.Cons[A, T, Z] { type EnumType = Z }
}

/**
 * Evidence that the case sets `Left` and `Right` of the enum type `EnumType`
 * can be joined, used by `CaseSet.Empty#++` and `CaseSet.Cons#++`.
 */
sealed trait Append[EnumType, -Left, -Right] {
  /** The type of the joined case set. */
  type Out <: CaseSet.Aux[EnumType]
  /** Joins `left` and `right`, with the cases of `left` first. */
  def apply(left: Left, right: Right): Out
}

/** Implicit [[Append]] instances. */
object Append extends AppendLowPriority {
  import CaseSet._

  /** An [[Append]] whose result type is `Out0`. */
  type WithOut[EnumType, Left, Right, Out0] = Append[EnumType, Left, Right] { type Out = Out0 }

  /** Joins a [[CaseSet.Cons]] with another set by joining its tail with it. */
  implicit def AppendCons[A, T <: CaseSet.Aux[Z], Z, That <: CaseSet.Aux[Z]](implicit
    append: Append[Z, T, That]
  ): Append.WithOut[Z, Cons[A, T, Z], That, Cons[A, append.Out, Z]] =
    new Append[Z, Cons[A, T, Z], That] {
      override type Out = Cons[A, append.Out, Z]

      def apply(left: Cons[A, T, Z], right: That): Out =
        Cons(left.head, append(left.tail, right))
    }

}

/** Lower-priority [[Append]] instances. */
trait AppendLowPriority extends AppendLowPriority2 {
  /** Joins a set with an empty set on the right, returning the left set. */
  implicit def AppendEmptyRight[T <: CaseSet.Aux[Z], Z]: Append.WithOut[Z, T, CaseSet.Empty[Z], T] =
    new Append[Z, T, CaseSet.Empty[Z]] {
      override type Out = T
      def apply(left: T, right: CaseSet.Empty[Z]): Out = left
    }
}

/** Lowest-priority [[Append]] instances. */
trait AppendLowPriority2 {
  /** Joins an empty set on the left with a set, returning the right set. */
  implicit def AppendEmptyLeft[T <: CaseSet.Aux[Z], Z]: Append.WithOut[Z, CaseSet.Empty[Z], T, T] =
    new Append[Z, CaseSet.Empty[Z], T] {
      override type Out = T
      def apply(left: CaseSet.Empty[Z], right: T): Out = right
    }
}
