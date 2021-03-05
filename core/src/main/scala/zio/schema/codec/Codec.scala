package zio.schema.codec

import zio.schema._
import zio.stream.ZTransducer
import zio.Chunk

trait Codec {
  def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte]
  def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A]

  def encode[A](schema: Schema[A]): A => Chunk[Byte]
  def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A]
}
