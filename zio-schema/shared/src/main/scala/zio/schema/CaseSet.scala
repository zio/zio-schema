package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.Schema._

sealed trait CaseSet { self =>

  import CaseSet.{ :+: }

  type EnumType

  type Accessors[Whole, Lens[_, _], Prism[_, _], Traversal[_, _]]

  def :+:[A](head: Case[A, EnumType]): A :+: CaseSet.Aux[EnumType]

  // def ++[That](that: That)(implicit append: Append[EnumType, self.type, That]): append.Out

  def toMap: ListMap[String, Schema[_]]

  def toSeq: Seq[Case[_, EnumType]]

  def makeAccessors(whole: Enum[EnumType], b: AccessorBuilder): Accessors[EnumType, b.Lens, b.Prism, b.Traversal]

}

object CaseSet {
  type Aux[EnumType0] = CaseSet { type EnumType = EnumType0 }

  final case class Empty[Z]() extends CaseSet { self =>
    type EnumType = Z

    override type Accessors[Whole, Lens[_, _], Prism[_, _], Traversal[_, _]] = Unit

    override def :+:[A](head: Case[A, EnumType]): A :+: Empty[EnumType] = Cons(head, self)

    def ++[That](that: That)(implicit append: Append[Z, Empty[Z], That]): append.Out =
      append(self, that)

    override def toMap: ListMap[String, Schema[_]] = ListMap.empty

    override def toSeq: Seq[Case[_, Z]] = Seq.empty

    override def makeAccessors(
      whole: Enum[EnumType],
      b: AccessorBuilder
    ): Accessors[EnumType, b.Lens, b.Prism, b.Traversal] = ()

    override def toString: String = "Empty"
  }

  object Empty {
    type Aux[Z] = CaseSet.Empty[Z] { type EnumType = Z }
  }

  sealed trait :+:[A, +T <: CaseSet] extends CaseSet {
    def head: Case[A, EnumType]
  }

  final case class Cons[A, +T <: CaseSet.Aux[Z], Z](head: Case[A, Z], tail: T) extends :+:[A, T] { self =>
    type EnumType = Z

    override type Accessors[Whole, Lens[_, _], Prism[_, _], Traversal[_, _]] =
      (Prism[Whole, A], tail.Accessors[Whole, Lens, Prism, Traversal])

    override def :+:[B](head2: Case[B, Z]): Cons[B, Cons[A, T, Z], Z] = Cons(head2, self)

    def ++[That](that: That)(implicit append: Append[Z, Cons[A, T, Z], That]): append.Out =
      append(self, that)

    override def toMap: ListMap[String, Schema[_]] = ListMap(head.id -> head.codec) ++ tail.toMap

    override def toSeq: Seq[Case[_, Z]] =
      Seq(head) ++ tail.toSeq

    override def makeAccessors(
      whole: Enum[EnumType],
      b: AccessorBuilder
    ): Accessors[EnumType, b.Lens, b.Prism, b.Traversal] =
      (b.makePrism(whole, head), tail.makeAccessors(whole, b))

    override def toString: String = s"$head :+: $tail"
  }
  val :+: = Cons

  def apply[Z](c: Case[_, Z]*): CaseSet = c.foldRight[CaseSet.Aux[Z]](Empty[Z]()) {
    case (c, cs) => Cons(c, cs)
  }

  def caseOf[A: Schema, Z >: A](id: String)(unsafeDeconstruct: Z => A): Cons[A, Empty[Z], Z] =
    Cons(Case(id, Schema[A], unsafeDeconstruct, Chunk.empty), Empty[Z]())

}

object Cons0 {
  type Aux[A, T <: CaseSet.Aux[Z], Z] = CaseSet.Cons[A, T, Z] { type EnumType = Z }
}

sealed trait Append[EnumType, -Left, -Right] {
  type Out <: CaseSet.Aux[EnumType]
  def apply(left: Left, right: Right): Out
}

object Append extends AppendLowPriority {
  import CaseSet._

  type WithOut[EnumType, Left, Right, Out0] = Append[EnumType, Left, Right] { type Out = Out0 }

  implicit def AppendCons[A, T <: CaseSet.Aux[Z], Z, That <: CaseSet.Aux[Z]](
    implicit append: Append[Z, T, That]
  ): Append.WithOut[Z, Cons[A, T, Z], That, Cons[A, append.Out, Z]] =
    new Append[Z, Cons[A, T, Z], That] {
      override type Out = Cons[A, append.Out, Z]

      def apply(left: Cons[A, T, Z], right: That): Out =
        Cons(left.head, append(left.tail, right))
    }

}

trait AppendLowPriority extends AppendLowPriority2 {
  implicit def AppendEmptyRight[T <: CaseSet.Aux[Z], Z]: Append.WithOut[Z, T, CaseSet.Empty[Z], T] =
    new Append[Z, T, CaseSet.Empty[Z]] {
      override type Out = T
      def apply(left: T, right: CaseSet.Empty[Z]): Out = left
    }
}

trait AppendLowPriority2 {
  implicit def AppendEmptyLeft[T <: CaseSet.Aux[Z], Z]: Append.WithOut[Z, CaseSet.Empty[Z], T, T] =
    new Append[Z, CaseSet.Empty[Z], T] {
      override type Out = T
      def apply(left: CaseSet.Empty[Z], right: T): Out = right
    }
}
