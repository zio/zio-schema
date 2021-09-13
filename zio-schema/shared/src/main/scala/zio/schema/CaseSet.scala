package zio.schema

import Schema._

import scala.collection.immutable.ListMap

sealed trait CaseSet {

  import CaseSet.{ :+: }

  type EnumType

  type Accessors[Whole, Lens[_, _], Prism[_, _], Traversal[_, _]]
  type Append[That <: CaseSet.Aux[EnumType]] <: CaseSet.Aux[EnumType]

  def :+:[A](head: Case[A, EnumType]): A :+: CaseSet.Aux[EnumType]

  def ++[That <: CaseSet.Aux[EnumType]](that: That): Append[That]

  def toMap: ListMap[String, Schema[_]]

  def toSeq: Seq[Case[_, EnumType]]

  def makeAccessors(whole: Enum[EnumType], b: AccessorBuilder): Accessors[EnumType, b.Lens, b.Prism, b.Traversal]

}

object CaseSet {
  type Aux[EnumType0] = CaseSet { type EnumType = EnumType0 }

  final case class Empty[Z]() extends CaseSet { self =>
    type EnumType = Z

    override type Accessors[Whole, Lens[_, _], Prism[_, _], Traversal[_, _]] = Unit
    override type Append[That <: CaseSet.Aux[EnumType]]                      = That

    override def :+:[A](head: Case[A, EnumType]): A :+: Empty[EnumType] = Cons(head, self)

    override def ++[That <: CaseSet.Aux[EnumType]](that: That): Append[That] = that

    override def toMap: ListMap[String, Schema[_]] = ListMap.empty

    override def toSeq: Seq[Case[_, Z]] = Seq.empty

    override def makeAccessors(
      whole: Enum[EnumType],
      b: AccessorBuilder
    ): Accessors[EnumType, b.Lens, b.Prism, b.Traversal] = ()

    override def toString: String = "Empty"
  }

  sealed trait :+:[A, +T <: CaseSet] extends CaseSet {
    def head: Case[A, EnumType]
  }

  final case class Cons[A, +T <: CaseSet.Aux[Z], Z](head: Case[A, Z], tail: T) extends :+:[A, T] { self =>
    type EnumType = Z

    override type Accessors[Whole, Lens[_, _], Prism[_, _], Traversal[_, _]] =
      (Prism[Whole, A], tail.Accessors[Whole, Lens, Prism, Traversal])

    override type Append[That <: CaseSet.Aux[EnumType]] = Cons[A, tail.Append[That], Z]

    override def :+:[B](head2: Case[B, Z]): Cons[B, Cons[A, T, Z], Z] = Cons(head2, self)

    override def ++[That <: CaseSet.Aux[EnumType]](that: That): Append[That] = Cons(head, tail ++ that)

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

  def caseOf[A: Schema, Z >: A](id: String)(unsafeDeconstruct: Z => A) =
    Cons(Case(id, Schema[A], unsafeDeconstruct), Empty[Z]())

}
