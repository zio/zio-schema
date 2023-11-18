---
id: index
title: "Introduction to ZIO Schema"
sidebar_label: "Introduction"
---

[ZIO Schema](https://github.com/zio/zio-schema) is a [ZIO](https://zio.dev)-based library for modeling the schema of data structures as first-class values.

@PROJECT_BADGES@

## Introduction

ZIO Schema helps us to solve some of the most common problems in distributed computing, such as serialization, deserialization, and data migration.

It turns a compiled-time construct (the type of a data structure) into a runtime construct (a value that can be read, manipulated, and composed at runtime). A schema is a structure of a data type. ZIO Schema reifies the concept of structure for data types. It makes a high-level description of any data type and makes them first-class values.

Creating a schema for a data type helps us to write codecs for that data type. So this library can be a host of functionalities useful for writing codecs and protocols like JSON, Protobuf, CSV, and so forth.

## What Problems Does ZIO Schema Solve?

With schema descriptions that can be automatically derived for case classes and sealed traits, _ZIO Schema_ will be going to provide powerful features for free:

1. Metaprogramming without macros, reflection, or complicated implicit derivations.
    1. Creating serialization and deserialization codecs for any supported protocol (JSON, Protobuf, etc.)
    2. Deriving standard type classes (`Eq`, `Show`, `Ordering`, etc.) from the structure of the data
    3. Default values for data types
2. Automate ETL (Extract, Transform, Load) pipelines
    1. Diffing: diffing between two values of the same type
    2. Patching: applying a diff to a value to update it
    3. Migration: migrating values from one type to another
3. Computations as data: Not only we can turn types into values, but we can also turn computations into values. This opens up a whole new world of possibilities concerning distributed computing.

When our data structures need to be serialized, deserialized, persisted, or transported across the wire, then _ZIO Schema_ lets us focus on data modeling and automatically tackle all the low-level, messy details for us.

_ZIO Schema_ is used by a growing number of ZIO libraries, including [ZIO Flow](https://zio.dev/zio-flow), [ZIO Redis](https://zio-redis), [ZIO SQL](https://zio.dev/zio-sql) and [ZIO DynamoDB](https://zio.dev/zio-dynamodb).

## Installation

In order to use this library, we need to add the following lines in our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-schema"          % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-schema-avro"     % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-schema-bson"     % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-schema-json"     % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-schema-msg-pack" % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-schema-protobuf" % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-schema-thrift"   % "@VERSION@"
libraryDependencies += "dev.zio" %% "zio-schema-zio-test" % "@VERSION@"

// Required for the automatic generic derivation of schemas
libraryDependencies += "dev.zio" %% "zio-schema-derivation" % "@VERSION@"
libraryDependencies += "org.scala-lang" % "scala-reflect"  % scalaVersion.value % "provided"
```

## Example

In this simple example first, we create a schema for `Person` and then run the _diff_ operation on two instances of the `Person` data type, and finally, we encode a Person instance using _Protobuf_ protocol:

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

```scala
Encoded data with protobuf codec: 0A044A6F686E102B
```

## Resources

- [Zymposium - ZIO Schema](https://www.youtube.com/watch?v=GfNiDaL5aIM) by John A. De Goes, Adam Fraser, and Kit Langton (May 2021)
- [ZIO SCHEMA: A Toolkit For Functional Distributed Computing](https://www.youtube.com/watch?v=lJziseYKvHo&t=481s) by Dan Harris (Functional Scala 2021)
- [Creating Declarative Query Plans With ZIO Schema](https://www.youtube.com/watch?v=ClePN4P9_pg) by Dan Harris (ZIO World 2022)
- [Describing Data...with free applicative functors (and more)](https://www.youtube.com/watch?v=oRLkb6mqvVM) by Kris Nuttycombe (Scala World) on the idea behind the [xenomorph](https://github.com/nuttycom/xenomorph) library
