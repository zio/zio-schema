package zio.schema.codec

import zio.schema.Schema
import zio.stream.ZPipeline

trait Codec[Whole, Element] {

  def encoderFor[A](schema: Schema[A]): Encoder[Whole, Element, A]

  def decoderFor[A](schema: Schema[A]): Decoder[Whole, Element, A]

  final def encoder[A](schema: Schema[A]): ZPipeline[Any, Nothing, A, Element] =
    encoderFor[A](schema).streamEncoder

  final def decoder[A](schema: Schema[A]): ZPipeline[Any, DecodeError, Element, A] =
    decoderFor[A](schema).streamDecoder

  final def encode[A](schema: Schema[A]): A => Whole =
    encoderFor[A](schema).encode

  final def decode[A](schema: Schema[A]): Whole => Either[DecodeError, A] =
    decoderFor[A](schema).decode

}
