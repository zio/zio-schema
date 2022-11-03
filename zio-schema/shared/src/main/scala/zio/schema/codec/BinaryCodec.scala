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

trait BinaryCodec extends Codec[Chunk[Byte], Byte]

object BinaryCodec {

  type BinaryEncoder[A] = Encoder[Chunk[Byte], Byte, A]

  type BinaryDecoder[A] = Decoder[Chunk[Byte], Byte, A]

  type BinaryStreamEncoder[A] = ZPipeline[Any, Nothing, A, Byte]

  type BinaryStreamDecoder[A] = ZPipeline[Any, DecodeError, Byte, A]

}
