---
id: codec-core
title: "Codec Core Types"
description: "Reference for the Encoder, Decoder, Codec, and BinaryCodec traits, and for DecodeError."
keywords:
  - "ZIO Schema"
  - "BinaryCodec"
  - "DecodeError"
  - "Reference"
---

Every format module, such as JSON or Protobuf, builds on the same small set of traits in the `zio.schema.codec` package. Knowing these traits helps when you read a codec's signature or write your own. The format-specific pages, such as [JSON Codecs](../derivations/codecs/json.md), describe how each format is configured.

## Encoder and Decoder

`Encoder[Whole, Element, A]` turns a value of type `A` into the whole output, such as a `Chunk[Byte]`. It also provides `streamEncoder`, which turns a stream of values into a stream of elements. `Decoder[Whole, Element, A]` is the reverse: it turns the whole input into an `A` or a `DecodeError`, and `streamDecoder` does the same for a stream of elements.

```scala
trait Encoder[Whole, Element, -A] {
  def encode(value: A): Whole
  def streamEncoder: ZPipeline[Any, Nothing, A, Element]
}

trait Decoder[Whole, Element, +A] {
  def decode(whole: Whole): Either[DecodeError, A]
  def streamDecoder: ZPipeline[Any, DecodeError, Element, A]
}
```

The `Encoder` is contravariant in `A`, and the `Decoder` is covariant in `A`. That means an encoder for a supertype can encode values of a subtype.

## Codec

`Codec[Whole, Element, A]` combines the two traits, so one value both encodes and decodes:

```scala
trait Codec[Whole, Element, A] extends Encoder[Whole, Element, A] with Decoder[Whole, Element, A]
```

## BinaryCodec

`BinaryCodec[A]` is a `Codec` whose whole form is a `Chunk[Byte]` and whose element form is a `Byte`. Every binary format module returns a `BinaryCodec` from its `schemaBasedBinaryCodec` method or its equivalent. The following example uses JSON, but the calls are the same for every format:

```scala
import zio.Chunk
import zio.schema._
import zio.schema.codec.{BinaryCodec, DecodeError, JsonCodec}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val codec: BinaryCodec[Person] = JsonCodec.schemaBasedBinaryCodec[Person]
```

`encode` produces the bytes, and `decode` reads them back:

```scala
val bytes: Chunk[Byte] = codec.encode(Person("John", 42))
// bytes: Chunk[Byte] = IndexedSeq(
//   123,
//   34,
//   110,
//   97,
//   109,
//   101,
//   34,
//   58,
//   34,
//   74,
//   111,
//   104,
//   110,
//   34,
//   44,
//   34,
//   97,
//   103,
//   101,
//   34,
//   58,
//   52,
//   50,
//   125
// )

codec.decode(bytes)
// res0: Either[DecodeError, Person] = Right(
//   value = Person(name = "John", age = 42)
// )
```

Calling `streamEncoder` and `streamDecoder` on the same codec gives the stream versions. [Integration with ZIO Streams](../integration-with-zio-streams.md) shows them in a complete program.

## DecodeError

`DecodeError` is the failure type of every decoder. It is a sealed trait that extends `Exception`, so it can be printed and logged like any exception. The cases describe what went wrong, for example:

- `MissingField` means that a required field is absent.
- `MalformedField` means that a field has a value of the wrong shape.
- `MalformedFieldWithPath` is a malformed field with the path to it from the root.
- `ExtraFields` reports a field the schema does not know, when the codec rejects extra fields.
- `EmptyContent` means that the input has no content.
- `And` and `Or` combine several errors.

The following input has no `age` field, so decoding fails, and the message names the missing field:

```scala
val incomplete = Chunk.fromArray("""{"name":"John"}""".getBytes("UTF-8"))
// incomplete: Chunk[Byte] = IndexedSeq(
//   123,
//   34,
//   110,
//   97,
//   109,
//   101,
//   34,
//   58,
//   34,
//   74,
//   111,
//   104,
//   110,
//   34,
//   125
// )

codec.decode(incomplete).left.map(_.getMessage)
// res1: Either[String, Person] = Left(value = ".age(missing)")
```

Decoding reports malformed input as a `Left`, so handle that case in the calling code.
