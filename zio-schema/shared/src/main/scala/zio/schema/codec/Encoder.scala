package zio.schema.codec

import zio.stream.ZPipeline

trait Encoder[Whole, Element, -A] {

  def encode(value: A): Whole

  def streamEncoder: ZPipeline[Any, Nothing, A, Element]

}
