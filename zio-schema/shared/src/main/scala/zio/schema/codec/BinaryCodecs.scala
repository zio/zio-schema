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
import zio.constraintless._
import zio.stream.ZPipeline

trait BinaryCodecs[Types <: TypeList] extends Codecs[Chunk[Byte], Byte, Types]

object BinaryCodecs {

  def make[Types <: TypeList](implicit instances: Instances[BinaryCodec, Types]): BinaryCodecs[Types] =
    new BinaryCodecs[Types] {
      override def encode[T](value: T)(implicit ev: IsElementOf[T, Types]): Chunk[Byte] =
        instances.withInstance((codec: BinaryCodec[T]) => codec.encode(value))

      override def streamEncoder[T](implicit ev: IsElementOf[T, Types]): ZPipeline[Any, Nothing, T, Byte] =
        instances.withInstance((codec: BinaryCodec[T]) => codec.streamEncoder)

      override def decode[T](
        whole: Chunk[Byte]
      )(implicit ev: IsElementOf[T, Types]): zio.prelude.Validation[DecodeError, T] =
        instances.withInstance((codec: BinaryCodec[T]) => codec.decode(whole))

      override def streamDecoder[T](implicit ev: IsElementOf[T, Types]): ZPipeline[Any, DecodeError, Byte, T] =
        instances.withInstance((codec: BinaryCodec[T]) => codec.streamDecoder)
    }
}
