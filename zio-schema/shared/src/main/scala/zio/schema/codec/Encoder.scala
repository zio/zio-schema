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

trait Encoder[Whole, Element, -A] {

  def encode(value: A): Whole

  def streamEncoder: ZPipeline[Any, Nothing, A, Element]

  def contramap[B](ba: B => A): Encoder[Whole, Element, B] = Encoder.Contramapped(this, ba)

}

object Encoder {

  final case class Contramapped[Whole, Element, A, B](inner: Encoder[Whole, Element, A], ba: B => A)
      extends Encoder[Whole, Element, B] {
    override def encode(value: B): Whole                            = inner.encode(ba(value))
    override def streamEncoder: ZPipeline[Any, Nothing, B, Element] = inner.streamEncoder.contramap(ba)
  }

}
