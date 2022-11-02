package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.Schema._
import zio.schema.Singleton

sealed trait FieldSet {
  type Accessors[Whole, Lens[_, _, _], Prism[_, _, _], Traversal[_, _]]
  type Append[That <: FieldSet] <: FieldSet

  type Terms
  type FieldNames

  def :*:[A](head: Field[ListMap[String, _], A]): FieldSet.Cons[A, FieldSet]

  def ++[That <: FieldSet](that: That): Append[That]

  def toChunk: Chunk[Field[ListMap[String, _], _]]

  def makeAccessors(
    whole: Record[ListMap[String, _]],
    b: AccessorBuilder
  ): Accessors[ListMap[String, _], b.Lens, b.Prism, b.Traversal]

}

object FieldSet {
  type Empty                   = Empty.type
  type Cons[A, +T <: FieldSet] = :*:[A, T]

  val Cons = :*:

  case object Empty extends FieldSet {
    override type Accessors[Whole, Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Unit
    override type Append[That <: FieldSet]                                         = That

    override type Terms = Any
    override type FieldNames = Any

    override def :*:[A](head: Field[ListMap[String, _], A]): FieldSet.Cons[A, Empty] = Cons(head, Empty)

    override def ++[That <: FieldSet](that: That): Append[That] = that

    override def toChunk: Chunk[Field[ListMap[String, _], _]] = Chunk.empty

    override def makeAccessors(
      whole: Record[ListMap[String, _]],
      b: AccessorBuilder
    ): Accessors[ListMap[String, _], b.Lens, b.Prism, b.Traversal] = ()

    override def toString: String = "Empty"
  }

  final case class :*:[A, +T <: FieldSet](head: Field[ListMap[String, _], A], tail: T) extends FieldSet { self =>
    override type Accessors[Whole, Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] =
      (Lens[head.Field, Whole, A], tail.Accessors[Whole, Lens, Prism, Traversal])
    override type Append[That <: FieldSet] = Cons[A, tail.Append[That]]

    override type Terms = (head.Field, A) with tail.Terms

    override type FieldNames = head.Field with tail.FieldNames

    override def :*:[B](head2: Field[ListMap[String, _], B]): FieldSet.Cons[B, Cons[A, T]] = Cons(head2, self)

    override def ++[That <: FieldSet](that: That): Append[That] = Cons(head, tail ++ that)

    override def toChunk: Chunk[Field[ListMap[String, _], _]] = head +: tail.toChunk

    override def makeAccessors(
      whole: Record[ListMap[String, _]],
      b: AccessorBuilder
    ): Accessors[ListMap[String, _], b.Lens, b.Prism, b.Traversal] =
      (b.makeLens(whole, head), tail.makeAccessors(whole, b))

    override def toString: String = s"$head :*: $tail"
  }

  def apply(fields: Field[ListMap[String, _], _]*): FieldSet =
    fields.foldRight[FieldSet](FieldSet.Empty)((field, acc) => field :*: acc)

  def fromFields(fields: Field.WithFieldName[ListMap[String, _], _ <: Singleton with String, _]*): FieldSet =
    fields.foldRight[FieldSet](FieldSet.Empty)((field, acc) => field :*: acc)

  // def field[A](name: String)(implicit schema: Schema[A]): A :*: Empty =
  //   Field(
  //     name,
  //     schema,
  //     get0 = (p: ListMap[String, _]) => p(name).asInstanceOf[A],
  //     set0 = (p: ListMap[String, _], value: A) => p.updated(name, value)
  //   ) :*: Empty

}
