---
id: custom-codec
title: "Write a Custom Codec"
description: "Implement the Codec trait to read and write a custom text format, including the streaming methods."
keywords:
  - "ZIO Schema"
  - "Codec"
  - "Custom format"
  - "How-to"
---

The format modules cover JSON, Protobuf, Avro, and other common formats. When you need a different format, such as one line of text per record, you can implement the `Codec` trait yourself. A codec has four methods: `encode`, `decode`, `streamEncoder`, and `streamDecoder`. The [Codec Core Types](../reference/codec-core.md) page describes each trait.

This guide writes a codec that stores a `Person` as one line, `name,age`, and reads it back.

## Define the Model

The model is an ordinary case class with a schema, so the rest of the library can use it:

```scala
import zio._
import zio.stream._
import zio.schema._
import zio.schema.codec.{Codec, DecodeError}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}
```

## Implement the Codec

The codec extends `Codec[String, String, Person]`. The first type parameter is the whole output, a `String`. The second is the element type of a stream, which is also a `String`, one line per element. The third is the value type, `Person`.

The following class encodes a person by joining its fields with a comma, and decodes a line by splitting it:

```scala
final class PersonLineCodec extends Codec[String, String, Person] {

  override def encode(person: Person): String =
    s"${person.name},${person.age}"

  override def decode(line: String): Either[DecodeError, Person] =
    line.split(",", 2).toList match {
      case name :: age :: Nil if age.nonEmpty && age.forall(_.isDigit) =>
        Right(Person(name, age.toInt))
      case _ =>
        Left(DecodeError.MalformedField(Schema[Person], s"expected name,age but got: $line"))
    }

  override def streamEncoder: ZPipeline[Any, Nothing, Person, String] =
    ZPipeline.map(encode)

  override def streamDecoder: ZPipeline[Any, DecodeError, String, Person] =
    ZPipeline.mapZIO(line => ZIO.fromEither(decode(line)))
}
```

The `decode` method returns a `Left` for input that does not match the format, so the caller decides how to report the error. The stream methods reuse the single-value methods: `ZPipeline.map` applies `encode` to each element, and `ZPipeline.mapZIO` applies `decode` and turns a `Left` into a failure of the stream.

This codec writes its own format and does not read the schema. Use it when the format is fixed by another system. For a format that should follow the model automatically, use one of the built-in codecs instead.

## Use the Codec

We create the codec and encode a value:

```scala
val lineCodec = new PersonLineCodec
```

```scala
lineCodec.encode(Person("John", 42))
// res0: String = "John,42"
```

Decoding a valid line gives the person back:

```scala
lineCodec.decode("John,42")
// res1: Either[DecodeError, Person] = Right(
//   value = Person(name = "John", age = 42)
// )
```

A line that does not match the format gives a `Left`:

```scala
lineCodec.decode("John").left.map(_.getMessage)
// res2: Either[String, Person] = Left(
//   value = "expected name,age but got: John"
// )
```

To process a stream of people, the streaming methods build a pipeline. The following program encodes two people, decodes the lines back, and collects the result. `Unsafe` runs the stream here so the example prints its result:

```scala
val roundTrip: ZIO[Any, DecodeError, Chunk[Person]] =
  ZStream(Person("John", 42), Person("Jane", 34))
    .via(lineCodec.streamEncoder)
    .via(lineCodec.streamDecoder)
    .runCollect
```

```scala
Unsafe.unsafe { implicit unsafe =>
  Runtime.default.unsafe.run(roundTrip).getOrThrowFiberFailure()
}
// res3: Chunk[Person] = IndexedSeq(
//   Person(name = "John", age = 42),
//   Person(name = "Jane", age = 34)
// )
```

## Where to Go Next

- [Codec Core Types](../reference/codec-core.md) describes the traits this guide implements.
- [Integration with ZIO Streams](../integration-with-zio-streams.md) shows the built-in streaming codecs.
- [Write a Custom Deriver](./custom-deriver.md) shows how to derive your own type class from a schema.
