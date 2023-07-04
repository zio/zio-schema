---
id: codecs
title: "Codecs"
---

Once we have our schema, we can combine it with a codec. A codec is a combination of a schema and a serializer. Unlike codecs in other libraries, a codec in ZIO Schema has no type parameter:

```scala
trait Codec {
  def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte]
  def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A]

  def encode[A](schema: Schema[A]): A => Chunk[Byte]
  def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A]
}
```

It basically says:
- `encoder[A]`: Given a `Schema[A]` it is capable of generating an `Encoder[A]` ( `A => Chunk[Byte]`) for any Schema.
- `decoder[A]`: Given a `Schema[A]` it is capable of generating a `Decoder[A]` ( `Chunk[Byte] => Either[String, A]`) for any Schema.

Example of possible codecs are:

- CSV Codec
- JSON Codec (already available)
- Apache Avro Codec (in progress)
- Apache Thrift Codec (in progress)
- XML Codec
- YAML Codec
- Protobuf Codec (already available)
- QueryString Codec
- etc.
