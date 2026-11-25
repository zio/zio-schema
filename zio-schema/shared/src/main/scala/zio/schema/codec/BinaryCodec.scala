/*
 * Copyright 2020-2022 John A. De Goes and the ZIO Contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package zio.schema.codec

import zio.stream.ZPipeline
import zio.{Chunk, ZIO}

/**
 * A `Codec` that encodes `A` to `Chunk[Byte]` and streams `Byte`s. Format
 * modules provide instances from a `Schema`, for example
 * `JsonCodec.schemaBasedBinaryCodec` and `ProtobufCodec.protobufCodec`.
 */
trait BinaryCodec[A] extends Codec[Chunk[Byte], Byte, A] {

  override def transform[B](ab: A => B, ba: B => A): BinaryCodec[B] =
    BinaryCodec.Transform(this, ab, ba)

  override def transformOrFail[B](ab: A => Either[String, B], ba: B => A): BinaryCodec[B] =
    BinaryCodec.TransformOrFail(this, ab, ba)

}

/**
 * Type aliases for the binary forms of encoders, decoders and stream pipelines.
 */
object BinaryCodec {

  /** An `Encoder` that produces `Chunk[Byte]`. */
  type BinaryEncoder[A] = Encoder[Chunk[Byte], Byte, A]

  /** A `Decoder` that reads `Chunk[Byte]`. */
  type BinaryDecoder[A] = Decoder[Chunk[Byte], Byte, A]

  /** A pipeline that encodes `A` values into a stream of bytes. */
  type BinaryStreamEncoder[A] = ZPipeline[Any, Nothing, A, Byte]

  /**
   * A pipeline that decodes a stream of bytes into `A` values and can fail with
   * a `DecodeError`.
   */
  type BinaryStreamDecoder[A] = ZPipeline[Any, DecodeError, Byte, A]

  final case class Transform[A, B](inner: BinaryCodec[A], ab: A => B, ba: B => A) extends BinaryCodec[B] {
    override def encode(value: B): Chunk[Byte]                       = inner.encode(ba(value))
    override def streamEncoder: ZPipeline[Any, Nothing, B, Byte]     = inner.streamEncoder.contramap(ba)
    override def decode(whole: Chunk[Byte]): Either[DecodeError, B]  = inner.decode(whole).map(ab)
    override def streamDecoder: ZPipeline[Any, DecodeError, Byte, B] = inner.streamDecoder.map(ab)
  }

  final case class TransformOrFail[A, B](
    inner: BinaryCodec[A],
    ab: A => Either[String, B],
    ba: B => A
  ) extends BinaryCodec[B] {

    override def encode(value: B): Chunk[Byte]                   = inner.encode(ba(value))
    override def streamEncoder: ZPipeline[Any, Nothing, B, Byte] = inner.streamEncoder.contramap(ba)

    override def decode(whole: Chunk[Byte]): Either[DecodeError, B] =
      inner.decode(whole).flatMap { a =>
        DecodeError.fromEither(ab(a))
      }
    override def streamDecoder: ZPipeline[Any, DecodeError, Byte, B] =
      inner.streamDecoder.mapZIO { a =>
        ZIO.fromEither(DecodeError.fromEither(ab(a)))
      }

  }

}
