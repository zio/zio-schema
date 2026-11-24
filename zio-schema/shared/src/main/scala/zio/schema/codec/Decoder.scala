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

/**
 * Decodes a `Whole`, such as `Chunk[Byte]`, into a value of type `A`, or turns
 * a stream of `Element`s into a stream of `A` values.
 */
trait Decoder[Whole, Element, +A] {

  /**
   * Returns the value decoded from `whole`, or a `DecodeError` when `whole` is
   * not a valid encoding of `A`.
   */
  def decode(whole: Whole): Either[DecodeError, A]

  /**
   * Returns a pipeline that decodes a stream of `Element`s into `A` values and
   * fails with a `DecodeError` on invalid input.
   */
  def streamDecoder: ZPipeline[Any, DecodeError, Element, A]

}
