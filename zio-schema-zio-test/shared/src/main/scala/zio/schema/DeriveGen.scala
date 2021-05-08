package zio.schema

import zio.random.Random
import zio.test._

import zio.schema.Schema.Primitive

object DeriveGen {

  def gen[A](implicit schema: Schema[A]): Gen[Sized with Random, A] =
    schema match {
      case p: Primitive[a] => PrimitiveGen(p.standardType).map(_.asInstanceOf[a])
      case _               => ???
    }
}
