package zio.schema.codec

import zio.Chunk
import zio.schema.Schema

object XmlCodec {
  def xmlCodec[A](schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {
      override def encode(value: A): Chunk[Byte] =
        Chunk.fromArray(s"<value>${value.toString}</value>".getBytes("UTF-8"))

      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] =
        Left(DecodeError.ReadError(zio.Cause.empty, "XML decoding not implemented yet"))

      override def streamEncoder = zio.stream.ZPipeline.mapChunks[A, Chunk[Byte]](_.map(encode)).flattenChunks

      override def streamDecoder =
        zio.stream.ZPipeline.mapChunksZIO[Byte, A](_ => zio.ZIO.fail(DecodeError.ReadError(zio.Cause.empty, "XML stream decoding not implemented yet")))
    }
}
