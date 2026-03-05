package zio.schema.codec

import java.nio.charset.StandardCharsets

import zio.schema.Schema
import zio.stream.ZPipeline
import zio.{ Chunk, Cause }

object XmlCodec {

  implicit def xmlCodec[A](implicit schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {
      override def encode(a: A): Chunk[Byte] =
        Chunk.fromArray(XmlEncoder.encode(schema, a).getBytes(StandardCharsets.UTF_8))

      override def decode(bytes: Chunk[Byte]): Either[DecodeError, A] =
        if (bytes.isEmpty)
          Left(DecodeError.ReadError(Cause.empty, "No bytes to decode"))
        else {
          val xmlStr = new String(bytes.toArray, StandardCharsets.UTF_8)
          XmlDecoder.decode(schema, xmlStr)
        }

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
        ZPipeline.mapChunks { chunk =>
          chunk.flatMap(a => Chunk.fromArray(XmlEncoder.encode(schema, a).getBytes(StandardCharsets.UTF_8)))
        }

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
        ZPipeline.mapChunksEither { bytes =>
          val xmlStr = new String(bytes.toArray, StandardCharsets.UTF_8)
          XmlDecoder.decode(schema, xmlStr).map(Chunk.single)
        }
    }
}
