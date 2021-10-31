package zio.schema.codec

import zio.schema._
import zio.stream.ZTransducer
import zio.Chunk

object AvroSchemaCodec extends Codec {

  override def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte] = ???

  override def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A] = ???

  override def encode[A](schema: Schema[A]): A => Chunk[Byte] = ???

  override def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A] = ???

}
