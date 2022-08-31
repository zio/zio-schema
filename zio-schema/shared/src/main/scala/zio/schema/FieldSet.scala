package zio.schema

import zio.Chunk
import zio.schema.Schema._

sealed trait FieldSet {
  type Accessors[Whole, Lens[_ <: Singleton with String, _, _], Prism[_, _, _], Traversal[_, _]]
  type Append[That <: FieldSet] <: FieldSet

  type Terms

  def :*:[F <: Singleton with String, A](head: Field[F, A]): FieldSet.Cons[F, A, FieldSet]

  def ++[That <: FieldSet](that: That): Append[That]

  def toChunk: Chunk[Field[_ <: Singleton with String, _]]

  def makeAccessors[Whole](whole: Record[Whole], b: AccessorBuilder): Accessors[Whole, b.Lens, b.Prism, b.Traversal]

}

object FieldSet {
  type Empty                                               = Empty.type
  type Cons[F <: Singleton with String, A, +T <: FieldSet] = :*:[F, A, T]

  val Cons = :*:

  case object Empty extends FieldSet {
    override type Accessors[Whole, Lens[_ <: Singleton with String, _, _], Prism[_, _, _], Traversal[_, _]] = Unit
    override type Append[That <: FieldSet]                                                                  = That

    override type Terms = Any
    override def :*:[F <: Singleton with String, A](head: Field[F, A]): FieldSet.Cons[F, A, Empty] = Cons(head, Empty)

    override def ++[That <: FieldSet](that: That): Append[That] = that

    override def toChunk: Chunk[Field[_ <: Singleton with String, _]] = Chunk.empty

    override def makeAccessors[Whole](
      whole: Record[Whole],
      b: AccessorBuilder
    ): Accessors[Whole, b.Lens, b.Prism, b.Traversal] = ()

    override def toString: String = "Empty"
  }

  final case class :*:[F <: Singleton with String, A, +T <: FieldSet](head: Field[F, A], tail: T) extends FieldSet {
    self =>
    override type Accessors[Whole, Lens[_ <: Singleton with String, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Lens[F, Whole, A], tail.Accessors[Whole, Lens, Prism, Traversal])
    override type Append[That <: FieldSet] = Cons[F, A, tail.Append[That]]

    override type Terms = (F, A) with tail.Terms

    override def :*:[F2 <: Singleton with String, B](head2: Field[F2, B]): FieldSet.Cons[F2, B, Cons[F, A, T]] =
      Cons(head2, self)

    override def ++[That <: FieldSet](that: That): Append[That] = Cons(head, tail ++ that)

    override def toChunk: Chunk[Field[_ <: Singleton with String, _]] = head +: tail.toChunk

    override def makeAccessors[Whole](
      whole: Record[Whole],
      b: AccessorBuilder
    ): Accessors[Whole, b.Lens, b.Prism, b.Traversal] =
      (b.makeLens(whole, head), tail.makeAccessors(whole, b))

    override def toString: String = s"$head :*: $tail"
  }

  def apply(fields: Field[_ <: Singleton with String, _]*): FieldSet =
    fields.foldRight[FieldSet](FieldSet.Empty)((field, acc) => field :*: acc)

  def field[F <: Singleton with String, A: Schema](name: F): :*:[F, A, Empty] = Field[F, A](name, Schema[A]) :*: Empty

}
