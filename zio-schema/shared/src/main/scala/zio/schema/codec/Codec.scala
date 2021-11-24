package zio.schema.codec

import zio.Chunk
import zio.schema._
import zio.stream.ZPipeline

trait Codec {

  def encoder[A](schema: Schema[A]): ZPipeline[Any, Any, Nothing, Nothing, A, Byte]
  def decoder[A](schema: Schema[A]):  ZPipeline[Any, Any, Nothing, String, A, Byte]

  def encode[A](schema: Schema[A]): A => Chunk[Byte]
  def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A]
}
