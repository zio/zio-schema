package zio.schema.codec

import zio.schema._
/*
import zio.json.{ JsonDecoder, JsonEncoder }
import zio.schema.Schema
trait Codec {
  def encoder[A](schema: Schema[A]): JsonEncoder[A]
  def decoder[A](schema: Schema[A]): JsonDecoder[A]
}
*/
import zio.stream.ZTransducer

trait Codec {
  def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte]
  def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A]
}
