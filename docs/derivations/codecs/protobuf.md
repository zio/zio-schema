---
id: protobuf
title: "Protobuf Codecs"
sidebar_label: "Protobuf"
---

## Introduction

Protocol Buffers (protobuf) is a binary serialization format developed by Google. It is designed for efficient data exchange between different systems and languages. The `zio-schema-protobuf` module derives Protobuf codecs from a ZIO Schema. It runs on the JVM, Scala.js, and Scala Native.

The codec writes the Protobuf wire format, with field numbers and wire types, but it derives the layout from the Scala model instead of a Protobuf schema file. It does not generate Protobuf schema files, and its encodings for `Option`, `Either`, maps, and sealed traits are its own, so data written by other Protobuf implementations decodes only when its layout matches the one the codec derives.

## Installation

To start using Protobuf codecs in ZIO, add the following dependency to your `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-schema-protobuf" % "@VERSION@"
```

For a Scala.js or Scala Native project, use `%%%` instead:

```scala
libraryDependencies += "dev.zio" %%% "zio-schema-protobuf" % "@VERSION@"
```

## BinaryCodec

The `ProtobufCodec` object inside the `zio.schema.codec` package provides `ProtobufCodec.protobufCodec`, which allows us to derive Protobuf codecs from a ZIO Schema:

```scala
object ProtobufCodec {
  implicit def protobufCodec[A](implicit schema: Schema[A]): BinaryCodec[A]
}
```

## Example: BinaryCodec

The following program encodes a `Person` to Protobuf, prints the bytes in hexadecimal, and decodes them back:

```scala mdoc:compile-only
import zio._
import zio.schema.codec._
import zio.schema.{DeriveSchema, Schema}

case class Person(name: String, age: Int)

object Person {
  implicit val schema       : Schema[Person]      =
    DeriveSchema.gen
  implicit val protobufCodec: BinaryCodec[Person] =
    ProtobufCodec.protobufCodec(schema)
}

object Main extends ZIOAppDefault {
  def run = for {
    _ <- ZIO.debug("Protobuf Codec Example:")
    person: Person = Person("John", 42)
    encoded: Chunk[Byte] = Person.protobufCodec.encode(person)
    _ <- ZIO.debug(
      s"person object encoded to Protobuf's binary format: ${toHex(encoded)}"
    )
    decoded <- ZIO.fromEither(Person.protobufCodec.decode(encoded))
    _ <- ZIO.debug(s"Protobuf object decoded to Person class: $decoded")
  } yield ()

  def toHex(bytes: Chunk[Byte]): String =
    bytes.map("%02x".format(_)).mkString(" ")
}
```

Here is the output of running the above program:

```
Protobuf Codec Example:
person object encoded to Protobuf's binary format: 0a 04 4a 6f 68 6e 10 2a
Protobuf object decoded to Person class: Person(John,42)
```

In the output, `0a` starts field 1 (`name`) and `10` starts field 2 (`age`). By default, the fields of a case class are numbered by position, starting at 1.

## Schema Evolution

Protobuf identifies fields by number, not by name, so the rules for changing a model are about numbers. The decoder skips field numbers it does not know, and it fills a missing field from its default value. A Scala default parameter becomes a default value through derivation, and `@fieldDefaultValue` sets one explicitly. `@optionalField` on its own does not make a field optional for this codec.

### Field Numbers

The `@fieldNumber(n)` annotation, from the `zio.schema.codec` package, gives a field an explicit number. Together with default values, it keeps old and new versions of a record compatible, and it lets you reorder fields without changing the format.

Consider the following three versions of a record:

```scala mdoc:silent:reset
import zio.schema.codec.{BinaryCodec, ProtobufCodec, fieldNumber}
import zio.schema.{DeriveSchema, Schema}

final case class RecordV1(x: Int, y: Int)
final case class RecordV2(x: Int = 100, y: Int, z: Int)
final case class RecordV3(@fieldNumber(2) y: Int, @fieldNumber(4) extra: String = "unknown", @fieldNumber(3) z: Int)

implicit val schemaV1: Schema[RecordV1] = DeriveSchema.gen[RecordV1]
implicit val schemaV2: Schema[RecordV2] = DeriveSchema.gen[RecordV2]
implicit val schemaV3: Schema[RecordV3] = DeriveSchema.gen[RecordV3]

val codecV1: BinaryCodec[RecordV1] = ProtobufCodec.protobufCodec[RecordV1]
val codecV2: BinaryCodec[RecordV2] = ProtobufCodec.protobufCodec[RecordV2]
val codecV3: BinaryCodec[RecordV3] = ProtobufCodec.protobufCodec[RecordV3]
```

The decoder of V1 can decode a binary encoded by V2, because it skips field number 3 (`z`). It cannot decode a binary encoded by V3, because V3 has no field number 1 (`x`) and V1 has no default for it:

```scala mdoc
codecV1.decode(codecV2.encode(RecordV2(1, 2, 3)))
codecV1.decode(codecV3.encode(RecordV3(2, "hello", 3))).isLeft
```

The decoder of V2 can decode a binary encoded by V3, because it has a default value for field number 1 (`x`), 100. The decoder of V3 can read V2, using the default for `extra`, but it cannot read V1, because V1 has no field number 3 (`z`):

```scala mdoc
codecV2.decode(codecV3.encode(RecordV3(2, "hello", 3)))
codecV3.decode(codecV2.encode(RecordV2(1, 2, 3)))
codecV3.decode(codecV1.encode(RecordV1(1, 2))).isLeft
```

Field numbers that are not set explicitly default to the field's position, starting at 1. When you mix explicit and positional numbers in one record, make sure no explicit number equals another field's position, because the codec does not detect duplicate numbers.

### Transient Fields and Sealed Traits

A field marked with `@transientField` is not written, and the decoder fills it from its default value.

:::caution
Put transient fields after all other fields. The encoder numbers the remaining fields as if the transient field did not exist, while the decoder counts it, so a `@transientField` placed before another field makes decoding fail.
:::

The cases of a sealed trait are numbered by position, starting at 1, and `@caseName` has no effect. Adding a case at the end keeps old data readable, but reordering or removing cases changes the meaning of existing data.

## Streaming

:::caution
`ProtobufCodec#streamDecoder` decodes each upstream chunk as exactly one message. Protobuf messages carry no length prefix, so when several encoded values arrive in one chunk, or one value is split across chunks, the decoder merges or fails them. Stream Protobuf values only over a transport that keeps message boundaries, and decode each message with `BinaryCodec#decode`.
:::

The following program encodes each `Person` into its own `Chunk[Byte]`, the unit a message transport such as Kafka or a WebSocket carries, and decodes each message on its own:

```scala mdoc:compile-only
import zio._
import zio.schema.codec.{BinaryCodec, DecodeError, ProtobufCodec}
import zio.schema.{DeriveSchema, Schema}
import zio.stream.ZStream

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] =
    DeriveSchema.gen
  implicit val protobufCodec: BinaryCodec[Person] =
    ProtobufCodec.protobufCodec(schema)
}

object Main extends ZIOAppDefault {

  val messages: ZStream[Any, Nothing, Chunk[Byte]] =
    ZStream(Person("John", 42), Person("Jane", 34)).map(Person.protobufCodec.encode)

  val decoded: ZStream[Any, DecodeError, Person] =
    messages.mapZIO(bytes => ZIO.fromEither(Person.protobufCodec.decode(bytes)))

  def run = decoded.runCollect.debug("decoded people")
}
```

The [How to Encode and Decode Streams](../../integration-with-zio-streams.md) guide covers framing for all binary codecs.
