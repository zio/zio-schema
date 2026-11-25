package zio.schema.codec

import zio.ZIO
import zio.stream.ZPipeline

/**
 * An `Encoder` and a `Decoder` for the same type `A`, with `Whole` as the
 * encoded form and `Element` as the unit of a stream. `BinaryCodec` fixes these
 * to `Chunk[Byte]` and `Byte`.
 */
trait Codec[Whole, Element, A] extends Encoder[Whole, Element, A] with Decoder[Whole, Element, A] {

  def transform[B](ab: A => B, ba: B => A): Codec[Whole, Element, B] =
    Codec.Transform(this, ab, ba)

  def transformOrFail[B](ab: A => Either[String, B], ba: B => A): Codec[Whole, Element, B] =
    Codec.TransformOrFail(this, ab, ba)

}

object Codec {

  final case class Transform[Whole, Element, A, B](inner: Codec[Whole, Element, A], ab: A => B, ba: B => A)
      extends Codec[Whole, Element, B] {
    override def encode(value: B): Whole                                = inner.encode(ba(value))
    override def streamEncoder: ZPipeline[Any, Nothing, B, Element]     = inner.streamEncoder.contramap(ba)
    override def decode(whole: Whole): Either[DecodeError, B]           = inner.decode(whole).map(ab)
    override def streamDecoder: ZPipeline[Any, DecodeError, Element, B] = inner.streamDecoder.map(ab)
  }

  final case class TransformOrFail[Whole, Element, A, B](
    inner: Codec[Whole, Element, A],
    ab: A => Either[String, B],
    ba: B => A
  ) extends Codec[Whole, Element, B] {

    override def encode(value: B): Whole                            = inner.encode(ba(value))
    override def streamEncoder: ZPipeline[Any, Nothing, B, Element] = inner.streamEncoder.contramap(ba)

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
