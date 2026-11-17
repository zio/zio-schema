---
id: thrift
title: "Apache Thrift Codecs"
sidebar_label: "Apache Thrift"
---

## Introduction

Apache Thrift is an open-source framework that allows seamless communication and data sharing between different programming languages and platforms. The `zio-schema-thrift` module derives Apache Thrift codecs from a ZIO Schema. The codecs write Thrift's binary protocol (`TBinaryProtocol`) using the Apache Thrift Java library.

## Installation

To derive Apache Thrift codecs from a ZIO Schema, add the following dependency to your `build.sbt` file. The module is published for the JVM only:

```scala
libraryDependencies += "dev.zio" %% "zio-schema-thrift" % "@VERSION@"
```

## BinaryCodec

The `ThriftCodec` object inside the `zio.schema.codec` package provides `ThriftCodec.thriftCodec`, which allows us to derive Thrift codecs from a ZIO Schema:

```scala
object ThriftCodec {
  implicit def thriftCodec[A](implicit schema: Schema[A]): BinaryCodec[A]
}
```

## Example

The following program encodes a `Person` to Thrift, prints the bytes in hexadecimal, and decodes them back:

```scala mdoc:compile-only
import zio._
import zio.schema.codec._
import zio.schema.{DeriveSchema, Schema}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] =
    DeriveSchema.gen
    
  implicit val thriftCodec: BinaryCodec[Person] =
    ThriftCodec.thriftCodec(schema)
}

object Main extends ZIOAppDefault {
  def run = for {
    _ <- ZIO.debug("Apache Thrift Codec Example:")
    person: Person = Person("John", 42)
    encoded: Chunk[Byte] = Person.thriftCodec.encode(person)
    _ <- ZIO.debug(s"person object encoded to Thrift's binary format: ${toHex(encoded)}")
    decoded <- ZIO.fromEither(Person.thriftCodec.decode(encoded))
    _ <- ZIO.debug(s"Thrift object decoded to Person class: $decoded")
  } yield ()

  def toHex(bytes: Chunk[Byte]): String =
    bytes.map("%02x".format(_)).mkString(" ")
}
```

Here is the output of running the above program:

```
Apache Thrift Codec Example: 
person object encoded to Thrift's binary format: 0b 00 01 00 00 00 04 4a 6f 68 6e 08 00 02 00 00 00 2a 00
Thrift object decoded to Person class: Person(John,42)
```

In the output, `0b 00 01` starts field 1 (`name`, a string), `08 00 02` starts field 2 (`age`, a 32-bit integer), and the final `00` ends the struct.

## Schema Evolution

Thrift identifies fields by their field id. The codec numbers the fields of a case class by position, starting at 1, and it does not read the `@fieldNumber` annotation of the Protobuf module, so the position of a field is its id. Cases of a sealed trait are numbered the same way. Adding a field at the end keeps old data readable only when the new field has a default value or is an `Option` or a sequence; reordering or removing fields changes the meaning of existing data.

When a field is missing from the input, the decoder uses `None` for `Option` fields, an empty sequence for sequence fields such as `List`, and the default value for fields that have one through a Scala default parameter, `@fieldDefaultValue`, or `@optionalField`. Any other missing field makes decoding fail.

A field marked with `@transientField` is not written, and the decoder fills it from its default value.

:::caution
Put transient fields after all other fields. The encoder numbers the remaining fields as if the transient field did not exist, while the decoder counts it, so a `@transientField` placed before another field makes decoding fail.
:::

## Streaming

:::caution
`ThriftCodec#streamEncoder` emits cumulative output: the bytes it emits for element `n` contain the encodings of elements 1 to `n`. The stream decoder also decodes each upstream chunk as one value. To stream Thrift values, encode and decode each element with `BinaryCodec#encode` and `BinaryCodec#decode` instead of the stream pipelines.
:::

To encode a stream of values one element at a time, so that each `Chunk[Byte]` holds exactly one encoded value:

```scala mdoc:compile-only
import zio._
import zio.schema.codec.{BinaryCodec, DecodeError, ThriftCodec}
import zio.schema.{DeriveSchema, Schema}
import zio.stream.ZStream

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen
}

val codec: BinaryCodec[Person] = ThriftCodec.thriftCodec[Person]

val messages: ZStream[Any, Nothing, Chunk[Byte]] =
  ZStream(Person("John", 42), Person("Jane", 34)).map(codec.encode)

val people: ZStream[Any, DecodeError, Person] =
  messages.mapZIO(bytes => ZIO.fromEither(codec.decode(bytes)))
```

The [How to Encode and Decode Streams](../../integration-with-zio-streams.md) guide covers framing for all binary codecs.
