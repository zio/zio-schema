---
id: index
title: "Introduction to ZIO Schema"
sidebar_label: "Introduction"
---

[ZIO Schema](https://github.com/zio/zio-schema) is a [ZIO](https://zio.dev)-based library for modeling the schema of data structures as first-class values.

@PROJECT_BADGES@

## Introduction

ZIO Schema helps us solve some of the most common problems in distributed computing, such as serialization, deserialization, and data migration.

It turns a compile-time construct (the type of a data structure) into a runtime construct (a value that can be read, manipulated, and composed at runtime). A schema is a structure of a data type. ZIO Schema reifies the concept of structure for data types. It makes a high-level description of any data type and makes them first-class values.

Creating a schema for a data type helps us to write codecs for that data type. ZIO Schema ships codecs for JSON, Protobuf, Avro, BSON, MessagePack, Thrift, and XML, and the same schema drives all of them.

## What Problems Does ZIO Schema Solve?

With schema descriptions that can be automatically derived for case classes and sealed traits, _ZIO Schema_ provides powerful features for free:

1. Generic programming without writing macros yourself. The derivation macros inspect your types once, and everything else works on the resulting schema value:
    1. Creating serialization and deserialization codecs for any supported protocol (JSON, Protobuf, etc.)
    2. Deriving an `Ordering` from the structure of the data, and deriving your own type classes with a `Deriver`
    3. Default values for data types
2. Automate ETL (Extract, Transform, Load) pipelines
    1. Diffing: diffing between two values of the same type
    2. Patching: applying a diff to a value to update it
    3. Migration: migrating values from one type to another
3. Computations as data: Not only can we turn types into values, but we can also turn computations into values. This opens up a whole new world of possibilities concerning distributed computing.

When our data structures need to be serialized, deserialized, persisted, or transported across the wire, then _ZIO Schema_ lets us focus on data modeling and automatically tackle all the low-level, messy details for us.

A growing number of ZIO libraries build on _ZIO Schema_, including [ZIO Flow](https://zio.dev/zio-flow), [ZIO Redis](https://zio.dev/zio-redis), [ZIO SQL](https://zio.dev/zio-sql) and [ZIO DynamoDB](https://zio.dev/zio-dynamodb).

## Installation

To use ZIO Schema, we add the core module to our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-schema" % "@VERSION@"
```

ZIO Schema is published for Scala 2.12, 2.13, and 3.

:::caution[Scala 3 Projects Need Scala 3.9 or Later]
The Scala 3 artifacts are compiled with Scala 3.9, so a Scala 3 project must use Scala 3.9 or later to depend on them.
:::

Every other feature lives in its own module; the [Modules](#modules) section lists them all, ready to copy into `build.sbt`.

On Scala 2, schema derivation uses macros that need `scala-reflect` at compile time. Scala 3 has no `scala-reflect` artifact, so a build that cross-compiles adds it only for Scala 2:

```scala
libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, _)) => Seq("org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided)
    case _            => Seq.empty
  }
}
```

[Getting Started](getting-started.md) walks through a first program: deriving a schema, encoding JSON, and validating values.

## Modules

Every other feature lives in its own module, and we add only the modules we use. All modules share the core module's version. Each codec module already depends on `zio-schema-derivation`, so adding a codec also brings in schema derivation.

On the JVM, every module is available. Copy the block and delete the lines you don't need:

```scala
libraryDependencies ++= Seq(
  // Schema, DynamicValue, diff and patch, migration, validation (JVM, Scala.js, Native)
  "dev.zio" %% "zio-schema"            % "@VERSION@",
  // DeriveSchema.gen and the Deriver API for your own type classes (JVM, Scala.js, Native)
  "dev.zio" %% "zio-schema-derivation" % "@VERSION@",
  // JSON codec (JVM, Scala.js, Native)
  "dev.zio" %% "zio-schema-json"       % "@VERSION@",
  // Protocol Buffers codec (JVM, Scala.js, Native)
  "dev.zio" %% "zio-schema-protobuf"   % "@VERSION@",
  // XML codec (JVM, Scala.js, Native)
  "dev.zio" %% "zio-schema-xml"        % "@VERSION@",
  // Apache Avro codec (JVM only)
  "dev.zio" %% "zio-schema-avro"       % "@VERSION@",
  // BSON codec for MongoDB (JVM only)
  "dev.zio" %% "zio-schema-bson"       % "@VERSION@",
  // MessagePack codec (JVM only)
  "dev.zio" %% "zio-schema-msg-pack"   % "@VERSION@",
  // Apache Thrift codec (JVM only)
  "dev.zio" %% "zio-schema-thrift"     % "@VERSION@",
  // Lenses, prisms, and traversals derived from a schema (JVM, Scala.js, Native)
  "dev.zio" %% "zio-schema-optics"     % "@VERSION@",
  // ZIO Test generators (Gen) derived from a schema (JVM, Scala.js, Native)
  "dev.zio" %% "zio-schema-zio-test"   % "@VERSION@"
)
```

In a Scala.js or Scala Native project, the dependencies use the `%%%` operator, and only the cross-platform modules are available. The Avro, BSON, MessagePack, and Thrift codecs have no Scala.js or Scala Native artifacts:

```scala
libraryDependencies ++= Seq(
  // Schema, DynamicValue, diff and patch, migration, validation
  "dev.zio" %%% "zio-schema"            % "@VERSION@",
  // DeriveSchema.gen and the Deriver API for your own type classes
  "dev.zio" %%% "zio-schema-derivation" % "@VERSION@",
  // JSON codec
  "dev.zio" %%% "zio-schema-json"       % "@VERSION@",
  // Protocol Buffers codec
  "dev.zio" %%% "zio-schema-protobuf"   % "@VERSION@",
  // XML codec
  "dev.zio" %%% "zio-schema-xml"        % "@VERSION@",
  // Lenses, prisms, and traversals derived from a schema
  "dev.zio" %%% "zio-schema-optics"     % "@VERSION@",
  // ZIO Test generators (Gen) derived from a schema
  "dev.zio" %%% "zio-schema-zio-test"   % "@VERSION@"
)
```

The build also publishes `zio-schema-macros`, an internal helper that the other modules pull in, so you never add it yourself.

Each add-on module has its own page:

- Codecs: [JSON](derivations/codecs/json.md), [Protocol Buffers](derivations/codecs/protobuf.md), [XML](derivations/codecs/xml.md), [Apache Avro](derivations/codecs/avro.md), [BSON](derivations/codecs/bson.md), [MessagePack](derivations/codecs/message-pack.md), [Apache Thrift](derivations/codecs/thrift.md)
- Derivation: [Automatic Schema Derivation](automatic-schema-derivation.md)
- Optics: [Optics Derivation](derivations/optics-derivation.md)
- ZIO Test: [Derivation of ZIO Test Generators](derivations/zio-test-gen-derivation.md)

## Example

In this example, we derive a schema for `Person` and then encode a `Person` value as a stream of Protocol Buffers bytes. The example needs the `zio-schema-protobuf` module:

```scala mdoc:compile-only
import zio._
import zio.stream._
import zio.schema.codec.{BinaryCodec, ProtobufCodec}
import zio.schema.{DeriveSchema, Schema}

import java.io.IOException

final case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person]    = DeriveSchema.gen
  val protobufCodec: BinaryCodec[Person] = ProtobufCodec.protobufCodec
}

object Main extends ZIOAppDefault {
  def run: ZIO[Any, IOException, Unit] =
    ZStream
      .succeed(Person("John", 43))
      .via(Person.protobufCodec.streamEncoder)
      .runCollect
      .flatMap(x =>
        Console.printLine(s"Encoded data with protobuf codec: ${toHex(x)}")
      )

  def toHex(chunk: Chunk[Byte]): String =
    chunk.map("%02X".format(_)).mkString
}
```

Here is the output of running the above program:

```text
Encoded data with protobuf codec: 0A044A6F686E102B
```

## Resources

- [Zymposium - ZIO Schema](https://www.youtube.com/watch?v=GfNiDaL5aIM) by John A. De Goes, Adam Fraser, and Kit Langton (May 2021)
- [ZIO SCHEMA: A Toolkit For Functional Distributed Computing](https://www.youtube.com/watch?v=lJziseYKvHo&t=481s) by Dan Harris (Functional Scala 2021)
- [Creating Declarative Query Plans With ZIO Schema](https://www.youtube.com/watch?v=ClePN4P9_pg) by Dan Harris (ZIO World 2022)
- [Describing Data...with free applicative functors (and more)](https://www.youtube.com/watch?v=oRLkb6mqvVM) by Kris Nuttycombe (Scala World) on the idea behind the [xenomorph](https://github.com/nuttycom/xenomorph) library
