package zio.schema

import zio.Chunk
import zio.schema.Schema._

sealed trait FieldSet {
  type Accessors[Whole, Lens[_, _], Prism[_, _], Traversal[_, _]]
  type Append[That <: FieldSet] <: FieldSet

  def :*:[A](head: Field[A]): FieldSet.Cons[A, FieldSet]

  def ++[That <: FieldSet](that: That): Append[That]

  def toChunk: Chunk[Field[_]]

  def makeAccessors[Whole](whole: Record[Whole], b: AccessorBuilder): Accessors[Whole, b.Lens, b.Prism, b.Traversal]

}

object FieldSet {
  type Empty                   = Empty.type
  type Cons[A, +T <: FieldSet] = :*:[A, T]

  val Cons = :*:

  case object Empty extends FieldSet {
    override type Accessors[Whole, Lens[_, _], Prism[_, _], Traversal[_, _]] = Unit
    override type Append[That <: FieldSet]                                   = That

    override def :*:[A](head: Field[A]): FieldSet.Cons[A, Empty] = Cons(head, Empty)

    override def ++[That <: FieldSet](that: That): Append[That] = that

    override def toChunk: Chunk[Field[_]] = Chunk.empty

    override def makeAccessors[Whole](
      whole: Record[Whole],
      b: AccessorBuilder
    ): Accessors[Whole, b.Lens, b.Prism, b.Traversal] = ()

    override def toString: String = "Empty"
  }

  final case class :*:[A, +T <: FieldSet](head: Field[A], tail: T) extends FieldSet { self =>
    override type Accessors[Whole, Lens[_, _], Prism[_, _], Traversal[_, _]] =
      (Lens[Whole, A], tail.Accessors[Whole, Lens, Prism, Traversal])
    override type Append[That <: FieldSet] = Cons[A, tail.Append[That]]

    override def :*:[B](head2: Field[B]): FieldSet.Cons[B, Cons[A, T]] = Cons(head2, self)

    override def ++[That <: FieldSet](that: That): Append[That] = Cons(head, tail ++ that)

    override def toChunk: Chunk[Field[_]] = head +: tail.toChunk

    override def makeAccessors[Whole](
      whole: Record[Whole],
      b: AccessorBuilder
    ): Accessors[Whole, b.Lens, b.Prism, b.Traversal] =
      (b.makeLens(whole, head), tail.makeAccessors(whole, b))

    override def toString: String = s"$head :*: $tail"
  }

  def field[A: Schema](name: String): A :*: Empty = Field(name, Schema[A]) :*: Empty

}
