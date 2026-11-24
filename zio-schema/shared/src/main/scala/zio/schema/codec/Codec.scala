package zio.schema.codec

/**
 * An `Encoder` and a `Decoder` for the same type `A`, with `Whole` as the
 * encoded form and `Element` as the unit of a stream. `BinaryCodec` fixes these
 * to `Chunk[Byte]` and `Byte`.
 */
trait Codec[Whole, Element, A] extends Encoder[Whole, Element, A] with Decoder[Whole, Element, A]
