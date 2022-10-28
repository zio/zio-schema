package zio.schema.codec

import zio.stream.ZPipeline

trait Decoder[Whole, Element, +A] {

  def decode(whole: Whole): Either[String, A]

  def streamDecoder: ZPipeline[Any, String, Element, A]

}
