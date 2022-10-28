package zio.schema.codec

import zio.Chunk
import zio.stream.ZPipeline

trait BinaryCodec extends Codec[Chunk[Byte], Byte]

object BinaryCodec {

  type BinaryEncoder[A] = Encoder[Chunk[Byte], Byte, A]

  type BinaryDecoder[A] = Decoder[Chunk[Byte], Byte, A]

  type BinaryStreamEncoder[A] = ZPipeline[Any, Nothing, A, Byte]

  type BinaryStreamDecoder[A] = ZPipeline[Any, String, Byte, A]

}
