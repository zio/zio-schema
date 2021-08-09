package zio.schema

import zio.Chunk

sealed trait FieldSet { self =>
  type Head
  def length: Int
  def rawFields: Chunk[Schema.Field[_]]
  def :*:[H](head: Schema.Field[H]): FieldSet = new FieldSet.:*:(head, self)
}

object FieldSet {
  sealed trait Empty extends FieldSet {
    override type Head = Nothing

    override def :*:[H](field: Schema.Field[H]): H :*: Empty = new :*:[H, Empty](field, Empty)
    override def toString: String                            = "Empty"
    override def length: Int                                 = 0
    override def rawFields: Chunk[Schema.Field[_]]           = Chunk.empty
  }
  case object Empty extends Empty

  case class :*:[H, T <: FieldSet](head: Schema.Field[H], tail: T) extends FieldSet {
    override type Head = H

    override def toString: String                  = s"$head :*: $tail"
    override def length: Int                       = tail.length + 1
    override def rawFields: Chunk[Schema.Field[_]] = tail.rawFields.prepended(head)
  }

  def apply(fields: Chunk[Schema.Field[_]]): FieldSet =
    fields.foldRight[FieldSet](Empty)((field, fieldSet) => field :*: fieldSet)

}

sealed trait Struct { self =>
  type Head

  def head: (String, Head)

  def tail: Struct

  def rawValues: Seq[Any]

  def :+:[H](h: (String, H)): Struct = new Struct.:+:(h, self)
}

object Struct {

  sealed trait SNil extends Struct {
    override type Head = Unit

    override def :+:[H](field: (String, H)): H :+: SNil = new :+:[H, SNil](field, SNil)

    override def head: (String, Unit) = throw new NoSuchElementException
    override def tail: Struct         = throw new NoSuchElementException
    override def rawValues: Seq[Any]  = Seq.empty

    override def toString: String = "SNil"
  }

  case object SNil extends SNil

  case class :+:[H, T <: Struct](h: (String, H), t: T) extends Struct { self =>
    override type Head = H

    override def head: (String, H) = h

    override def tail: Struct = t

    override def rawValues: Seq[Any] = t.rawValues.prepended(h._2)

    override def toString: String = s"$h :+: $t"
  }

}
