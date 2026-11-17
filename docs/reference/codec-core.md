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

`BinaryCodec[A]` is a `Codec` whose whole form is a `Chunk[Byte]` and whose element form is a `Byte`. The JSON, Protobuf, Avro, MessagePack, Thrift, and XML modules return a `BinaryCodec` from their `schemaBasedBinaryCodec` method or its equivalent. The BSON module is the exception: `BsonSchemaCodec.bsonCodec` returns a `BsonCodec` from the zio-bson library, as described in [BSON Codecs](../derivations/codecs/bson.md). The following example uses JSON, but the calls are the same for every format that returns a `BinaryCodec`:

```scala mdoc:silent
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

```scala mdoc
val bytes: Chunk[Byte] = codec.encode(Person("John", 42))

codec.decode(bytes)
```

Calling `streamEncoder` and `streamDecoder` on the same codec gives the stream versions. [Integration with ZIO Streams](../integration-with-zio-streams.md) shows them in a complete program.

The `BinaryCodec` companion object defines type aliases for the separate parts, which are convenient in signatures:

```scala
object BinaryCodec {
  type BinaryEncoder[A]       = Encoder[Chunk[Byte], Byte, A]
  type BinaryDecoder[A]       = Decoder[Chunk[Byte], Byte, A]
  type BinaryStreamEncoder[A] = ZPipeline[Any, Nothing, A, Byte]
  type BinaryStreamDecoder[A] = ZPipeline[Any, DecodeError, Byte, A]
}
```

## DecodeError

`DecodeError` is the failure type of every decoder in this library. It is a sealed trait that extends `Exception`, so it can be printed and logged like any exception. Its `message` method, which `getMessage` also returns, describes the problem. The cases describe what went wrong:

| Case                                          | Meaning                                                                               |
| --------------------------------------------- | ------------------------------------------------------------------------------------- |
| `ReadError(cause, message)`                   | The input could not be read; many codecs report malformed input this way              |
| `ReadErrorWithPath(path, cause, message)`     | A read error together with the path to the failing value                              |
| `MissingField(schema, message)`               | A required field is absent                                                            |
| `RecordMissingField(record, field, message)`  | A required field of a specific record is absent                                       |
| `MalformedField(schema, message)`             | A field has a value of the wrong shape, or a `transformOrFail` conversion rejected it |
| `MalformedFieldWithPath(path, message)`       | A malformed field together with the path to it from the root                          |
| `ExtraFields(fieldName, message)`             | The input has a field the schema does not know, when the codec rejects extra fields   |
| `EmptyContent(message)`                       | The input has no content                                                              |
| `ValidationError(validation, field, message)` | A value broke a validation rule                                                       |
| `CastError(value, schema)`                    | A `DynamicValue` does not match the schema it is read with                            |
| `MissingCase(key, enumN)`                     | The input names a case that the enumeration does not have                             |
| `IncompatibleShape(values, structure)`        | The fields of a record value do not match the fields of the schema                    |
| `UnsupportedSchema(schema, decoderName)`      | The decoder cannot handle this kind of schema                                         |
| `And(left, right)`, `Or(left, right)`         | Two errors combined; `DecodeError#and` and `DecodeError#or` build them                |

The following input has no `age` field, so decoding fails, and the message names the missing field:

```scala mdoc
val incomplete = Chunk.fromArray("""{"name":"John"}""".getBytes("UTF-8"))

codec.decode(incomplete).left.map(_.getMessage)
```

Decoding reports malformed input as a `Left`, so handle that case in the calling code.
