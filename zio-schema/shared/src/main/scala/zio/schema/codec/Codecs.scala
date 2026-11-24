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

/**
 * Codecs for every type in the type list `Types`, all with the same `Whole` and
 * `Element`. Each method selects the codec for `T` from the evidence that `T`
 * is an element of `Types` (zio-constraintless' `IsElementOf`), so one value
 * encodes and decodes several types.
 */
trait Codecs[Whole, Element, Types <: TypeList] {

  /** Returns `value` encoded with the codec for `T`. */
  def encode[T](value: T)(implicit ev: T IsElementOf Types): Whole

  /** Returns the stream encoder of the codec for `T`. */
  def streamEncoder[T](implicit ev: T IsElementOf Types): ZPipeline[Any, Nothing, T, Element]

  /**
   * Decodes `whole` with the codec for `T`, returning a `DecodeError` when it
   * is not a valid encoding.
   */
  def decode[T](whole: Whole)(implicit ev: T IsElementOf Types): Either[DecodeError, T]

  /** Returns the stream decoder of the codec for `T`. */
  def streamDecoder[T](implicit ev: T IsElementOf Types): ZPipeline[Any, DecodeError, Element, T]
}

/** Constructors for `Codecs`. */
object Codecs {

  /**
   * Builds a `Codecs` that delegates to one `Codec` per type in `Types`,
   * collected from the implicit codecs in scope.
   */
  def make[Whole, Element, Types <: TypeList](implicit
    instances: Instances[Codec[Whole, Element, *], Types]
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
