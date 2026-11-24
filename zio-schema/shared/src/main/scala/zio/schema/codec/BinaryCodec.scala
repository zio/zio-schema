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

import zio.Chunk
import zio.stream.ZPipeline

/**
 * A `Codec` that encodes `A` to `Chunk[Byte]` and streams `Byte`s. Format modules provide instances from a `Schema`, for example `JsonCodec.schemaBasedBinaryCodec` and `ProtobufCodec.protobufCodec`.
 */
trait BinaryCodec[A] extends Codec[Chunk[Byte], Byte, A]

/** Type aliases for the binary forms of encoders, decoders and stream pipelines. */
object BinaryCodec {

  /** An `Encoder` that produces `Chunk[Byte]`. */
  type BinaryEncoder[A] = Encoder[Chunk[Byte], Byte, A]

  /** A `Decoder` that reads `Chunk[Byte]`. */
  type BinaryDecoder[A] = Decoder[Chunk[Byte], Byte, A]

  /** A pipeline that encodes `A` values into a stream of bytes. */
  type BinaryStreamEncoder[A] = ZPipeline[Any, Nothing, A, Byte]

  /** A pipeline that decodes a stream of bytes into `A` values and can fail with a `DecodeError`. */
  type BinaryStreamDecoder[A] = ZPipeline[Any, DecodeError, Byte, A]

}
