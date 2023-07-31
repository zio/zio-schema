---
id: protobuf
title: "Protobuf Codecs"
sidebar_label: "Protobuf"
---

## Introduction

Protocol Buffers (protobuf) is a binary serialization format developed by Google. It is designed for efficient data exchange between different systems and languages. In this article, we will explore how to derive Protobuf codecs from a ZIO Schema. Protobuf codecs allow us to easily serialize and deserialize data in Protobuf format, making it simple to interact with APIs and data sources that use Protobuf as their data format.

## Installation

To start using Protobuf codecs in ZIO, you need to add the following dependency to your build.sbt file:

```scala
libraryDependencies += "dev.zio" %% "zio-schema-protobuf" % "@VERSION@"
```

## BinaryCodec

The `ProtobufCodec` object inside the `zio.schema.codec` package provides the `protobufCodec` operator which allows us to derive Protobuf codecs from a ZIO Schema:

```scala
object ProtobufCodec {
  implicit def protobufCodec[A](implicit schema: Schema[A]): BinaryCodec[A] = ???
}
```

Let's try an example:

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

```scala
Protobuf Codec Example:
person object encoded to Protobuf's binary format: 0a 04 4a 6f 68 6e 10 2a
Protobuf object decoded to Person class: Person(John,42)
```
