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
import zio.ZIO

trait Decoder[Whole, Element, +A] {

  def decode(whole: Whole): Either[DecodeError, A]

  def streamDecoder: ZPipeline[Any, DecodeError, Element, A]

  def map[B](ab: A => B): Decoder[Whole, Element, B]                       = Decoder.Mapped(this, ab)
  def mapOrFail[B](ab: A => Either[String, B]): Decoder[Whole, Element, B] = Decoder.MappedOrFail(this, ab)

}

object Decoder {

  final case class Mapped[Whole, Element, A, B](inner: Decoder[Whole, Element, A], ab: A => B)
      extends Decoder[Whole, Element, B] {
    override def decode(whole: Whole): Either[DecodeError, B]           = inner.decode(whole).map(ab)
    override def streamDecoder: ZPipeline[Any, DecodeError, Element, B] = inner.streamDecoder.map(ab)
  }

  final case class MappedOrFail[Whole, Element, A, B](inner: Decoder[Whole, Element, A], ab: A => Either[String, B])
      extends Decoder[Whole, Element, B] {

    override def decode(whole: Whole): Either[DecodeError, B] =
      inner.decode(whole).flatMap { a =>
        DecodeError.fromEither(ab(a))
      }
    override def streamDecoder: ZPipeline[Any, DecodeError, Element, B] =
      inner.streamDecoder.mapZIO { a =>
        ZIO.fromEither(DecodeError.fromEither(ab(a)))
      }

  }

}
