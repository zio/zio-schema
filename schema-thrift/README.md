# ZIO Schema Thrift

Thrift codec support for ZIO Schema.

## Overview

This module provides automatic Apache Thrift serialization and deserialization for any type with a ZIO Schema. It has been fully migrated to ZIO 2.x, supporting high-performance streaming pipelines and ensuring binary compatibility with standard Thrift protocols.

## Installation

Add to your `build.sbt`:

```scala
libraryDependencies += "dev.zio" %% "zio-schema-thrift" % "<version>"

```

## Usage

### Basic Usage

Define your data models with schemas, then create a Thrift codec:

```scala
import zio.schema._
import zio.schema.codec.ThriftCodec

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

// Create the Thrift codec
val codec = ThriftCodec.thriftCodec(Schema[Person])

// Encode to Thrift binary
val person = Person("Alice", 30)
val bytes: Chunk[Byte] = codec.encode(person)

// Decode from Thrift binary
val decoded: Either[DecodeError, Person] = codec.decode(bytes)

```

### Working with ZIO Streams (ZPipeline)

Since this module is optimized for ZIO 2, you can use `ZPipeline` for efficient stream processing:

```scala
import zio.stream._

val people = ZStream(Person("Bob", 25), Person("Alice", 30))

// Stream Encoding
val byteStream = people.via(codec.streamEncoder)

// Stream Decoding
val resultStream = byteStream.via(codec.streamDecoder)

```

### Complex Types

It handles nested collections and case classes automatically:

```scala
case class Team(id: Int, members: List[Person])

object Team {
  implicit val schema: Schema[Team] = DeriveSchema.gen[Team]
}

val teamCodec = ThriftCodec.thriftCodec(Schema[Team])
val team = Team(1, List(Person("Alice", 30), Person("Bob", 25)))

val encoded = teamCodec.encode(team)
val decoded = teamCodec.decode(encoded)

```

## Thrift Format

Apache Thrift is a binary communication protocol used for RPC and persistent data storage.

* **Compact:** Binary format is efficient and small.
* **Schema-based:** Usually requires IDL, but with ZIO Schema, we derive it from Scala types.
* **Type-safe:** Ensures data integrity during serialization.

### Encoding Conventions

Mapping between ZIO Schema types and Thrift types:

| Scala Type | Thrift Type |
| --- | --- |
| `Unit` | `VOID` |
| `Boolean` | `BOOL` |
| `Byte` | `BYTE` |
| `Short` | `I16` |
| `Int` | `I32` |
| `Long` | `I64` |
| `Double` | `DOUBLE` |
| `String` | `STRING` |
| `Chunk[Byte]` | `BINARY` |
| `List[A]`, `Set[A]` | `LIST` / `SET` |
| `Map[K, V]` | `MAP` |
| `Case Class` | `STRUCT` |
| `Enum` | `STRUCT` |

## Supported Types

All standard ZIO Schema primitive types are supported:

* `Boolean`, `Byte`, `Short`, `Int`, `Long`, `Float`, `Double`, `Char`, `String`
* `BigInt`, `BigDecimal`
* `Unit`, `UUID`
* `Java Time`: `Instant`, `LocalDate`, `LocalTime`, `LocalDateTime`, `ZonedDateTime`, `Duration`, `Period`, `Year`, `MonthDay`, `ZoneId`

## Reliability & Performance

This module includes a comprehensive test suite to ensure stability and speed:

* **Verified Migration:** Checked against ZIO 2 compliance rules (No `ZTransducer` or legacy code).
* **Property Testing:** Validated with fuzz testing to handle edge cases.
* **Performance:** Benchmarked to handle high throughput operations.

## More Information

* [Apache Thrift](https://thrift.apache.org/) - Official Thrift documentation
* [ZIO Schema](https://zio.dev/zio-schema/) - ZIO Schema Homepage