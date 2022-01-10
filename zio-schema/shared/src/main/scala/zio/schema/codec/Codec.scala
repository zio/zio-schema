package zio.schema.codec

import zio.Chunk
import zio.schema._
import zio.stream.ZPipeline

trait Codec {

  def encoder[A](schema: Schema[A]): ZPipeline[Any, Nothing, A, Byte]
  def decoder[A](schema: Schema[A]): ZPipeline[Any, String, Byte, A]

  def encode[A](schema: Schema[A]): A => Chunk[Byte]
  def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A]
}
