package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.Schema._

/**
 * The fields of a generic record as a type-level list, built from
 * `FieldSet.:*:` cells that end in [[FieldSet.Empty]]. A generic record stores
 * its values in a `ListMap[String, _]`; `Schema.record` takes a `FieldSet` (or
 * builds one from its fields) to describe such a record.
 */
sealed trait FieldSet {

  /**
   * The type of the tuple of lenses that [[makeAccessors]] returns, one per
   * field.
   */
  type Accessors[Whole, Lens[_, _, _], Prism[_, _, _], Traversal[_, _]]

  /** The type of this field set followed by the fields of `That`. */
  type Append[That <: FieldSet] <: FieldSet

  /** The intersection of `(field name, field type)` pairs, one per field. */
  type Terms

  /** The intersection of the field name types, one per field. */
  type FieldNames

  /**
   * Returns a field set with `head` as its first field, followed by the fields
   * of this set.
   */
  def :*:[A](head: Field[ListMap[String, _], A]): FieldSet.Cons[A, FieldSet]

  /**
   * Returns a field set with the fields of this set followed by the fields of
   * `that`.
   */
  def ++[That <: FieldSet](that: That): Append[That]

  /** Returns the fields in order. */
  def toChunk: Chunk[Field[ListMap[String, _], _]]

  /** Builds one lens per field with `b`, as nested pairs ending in `Unit`. */
  def makeAccessors(
    whole: Record[ListMap[String, _]],
    b: AccessorBuilder
  ): Accessors[ListMap[String, _], b.Lens, b.Prism, b.Traversal]

}

/** The cells of a [[FieldSet]] and the ways to build one. */
object FieldSet {

  /** The type of the empty field set. */
  type Empty = Empty.type

  /** Another name for the `:*:` cell type. */
  type Cons[A, +T <: FieldSet] = :*:[A, T]

  /** Another name for the `:*:` companion. */
  val Cons = :*:

  /** The field set with no fields, which ends every [[FieldSet]]. */
  case object Empty extends FieldSet {
    override type Accessors[Whole, Lens[_, _, _], Prism[_, _, _], Traversal[_, _]] = Unit
    override type Append[That <: FieldSet]                                         = That

    override type Terms      = Any
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

  /** A field set cell: the field `head` followed by the fields in `tail`. */
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

  /** Builds a field set from the given fields, in order. */
  def apply(fields: Field[ListMap[String, _], _]*): FieldSet =
    fields.foldRight[FieldSet](FieldSet.Empty)((field, acc) => field :*: acc)

  /**
   * Builds a field set from fields whose names have singleton string types, in
   * order. The result type is `FieldSet`, as for `apply`.
   */
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
