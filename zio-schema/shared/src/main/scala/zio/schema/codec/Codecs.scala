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

import zio.constraintless._
import zio.stream.ZPipeline

trait Codecs[Whole, Element, Types <: TypeList] {
  def encode[T](value: T)(implicit ev: T IsElementOf Types): Whole
  def streamEncoder[T](implicit ev: T IsElementOf Types): ZPipeline[Any, Nothing, T, Element]
  def decode[T](whole: Whole)(implicit ev: T IsElementOf Types): Either[DecodeError, T]

  def streamDecoder[T](implicit ev: T IsElementOf Types): ZPipeline[Any, DecodeError, Element, T]
}

object Codecs {

  def make[Whole, Element, Types <: TypeList](
    implicit instances: Instances[Codec[Whole, Element, *], Types]
  ): Codecs[Whole, Element, Types] =
    new Codecs[Whole, Element, Types] {

      final override def streamEncoder[T](implicit ev: IsElementOf[T, Types]): ZPipeline[Any, Nothing, T, Element] =
        instances.withInstance((codec: Codec[Whole, Element, T]) => codec.streamEncoder)

      final override def decode[T](whole: Whole)(implicit ev: IsElementOf[T, Types]): Either[DecodeError, T] =
        instances.withInstance((codec: Codec[Whole, Element, T]) => codec.decode(whole))

      final override def streamDecoder[T](implicit ev: IsElementOf[T, Types]): ZPipeline[Any, DecodeError, Element, T] =
        instances.withInstance((codec: Codec[Whole, Element, T]) => codec.streamDecoder)

      final override def encode[T](value: T)(implicit ev: IsElementOf[T, Types]): Whole =
        instances.withInstance((codec: Codec[Whole, Element, T]) => codec.encode(value))
    }
}
