[//]: # (This file was autogenerated using `zio-sbt-website` plugin via `sbt generateReadme` command.)
[//]: # (So please do not edit it manually. Instead, change "docs/index.md" file or sbt setting keys)
[//]: # (e.g. "readmeDocumentation" and "readmeSupport".)

# ZIO Schema

[ZIO Schema](https://github.com/zio/zio-schema) is a [ZIO](https://zio.dev)-based library for modeling the schema of data structures as first-class values.

[![Development](https://img.shields.io/badge/Project%20Stage-Development-green.svg)](https://github.com/zio/zio/wiki/Project-Stages) ![CI Badge](https://github.com/zio/zio-schema/workflows/CI/badge.svg) [![Sonatype Releases](https://img.shields.io/nexus/r/https/oss.sonatype.org/dev.zio/zio-schema_2.13.svg?label=Sonatype%20Release)](https://oss.sonatype.org/content/repositories/releases/dev/zio/zio-schema_2.13/) [![Sonatype Snapshots](https://img.shields.io/nexus/s/https/oss.sonatype.org/dev.zio/zio-schema_2.13.svg?label=Sonatype%20Snapshot)](https://oss.sonatype.org/content/repositories/snapshots/dev/zio/zio-schema_2.13/) [![javadoc](https://javadoc.io/badge2/dev.zio/zio-schema-docs_2.13/javadoc.svg)](https://javadoc.io/doc/dev.zio/zio-schema-docs_2.13) [![ZIO Schema](https://img.shields.io/github/stars/zio/zio-schema?style=social)](https://github.com/zio/zio-schema)

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
libraryDependencies += "dev.zio" %% "zio-schema"          % "1.7.3"
libraryDependencies += "dev.zio" %% "zio-schema-avro"     % "1.7.3"
libraryDependencies += "dev.zio" %% "zio-schema-bson"     % "1.7.3"
libraryDependencies += "dev.zio" %% "zio-schema-json"     % "1.7.3"
libraryDependencies += "dev.zio" %% "zio-schema-msg-pack" % "1.7.3"
libraryDependencies += "dev.zio" %% "zio-schema-protobuf" % "1.7.3"
libraryDependencies += "dev.zio" %% "zio-schema-thrift"   % "1.7.3"
libraryDependencies += "dev.zio" %% "zio-schema-zio-test" % "1.7.3"

// Required for the automatic generic derivation of schemas
libraryDependencies += "dev.zio" %% "zio-schema-derivation" % "1.7.3"
libraryDependencies += "org.scala-lang" % "scala-reflect"  % scalaVersion.value % "provided"
```

## Example

In this simple example first, we create a schema for `Person` and then run the _diff_ operation on two instances of the `Person` data type, and finally, we encode a Person instance using _Protobuf_ protocol:

```scala
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

## Documentation

Learn more on the [ZIO Schema homepage](https://zio.dev/zio-schema)!

## Contributing

For the general guidelines, see ZIO [contributor's guide](https://zio.dev/contributor-guidelines).
#### TL;DR

Before you submit a PR, make sure your tests are passing, and that the code is properly formatted

```
sbt prepare

sbt test
```

## Code of Conduct

See the [Code of Conduct](https://zio.dev/code-of-conduct)

## Support

Come chat with us on [![Badge-Discord]][Link-Discord].

[Badge-Discord]: https://img.shields.io/discord/629491597070827530?logo=discord "chat on discord"
[Link-Discord]: https://discord.gg/2ccFBr4 "Discord"

## License

[License](LICENSE)
