---
id: json
title: "JSON Codecs"
sidebar_label: "JSON"
---

## Introduction

JSON (JavaScript Object Notation) is a widely used data interchange format for transmitting and storing data. ZIO Schema provides `zio-schema-json` module which has functionality to derive JSON codecs from a ZIO Schema. JSON codecs allow us to easily serialize and deserialize data in JSON format. In this article, we will explore how derive JSON codecs using the ZIO Schema.

## Installation

To derive JSON codecs from a ZIO Schema, we need to add the following dependency to our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-schema-json" % @VERSION@
```

## JsonCodec

The `JsonCodec` object inside the `zio.schema.codec` package provides the `jsonCodec` operator which allows us to derive JSON codecs from a ZIO Schema:

```scala
object JsonCodec {
  def jsonCodec[A](schema: Schema[A]): zio.json.JsonCodec[A] = ???
}
```

Let's try an example to see how it works:

```scala mdoc:compile-only
import zio._
import zio.json._
import zio.schema.{DeriveSchema, Schema}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] =
    DeriveSchema.gen
  implicit val jsonCodec: zio.json.JsonCodec[Person] =
    zio.schema.codec.JsonCodec.jsonCodec(schema)
}

object Main extends ZIOAppDefault {
  def run = for {
    _ <- ZIO.debug("JSON Codec Example:")
    person: Person  = Person("John", 42)
    encoded: String = person.toJson
    _       <- ZIO.debug(s"person object encoded to JSON string: $encoded")
    decoded <- ZIO.fromEither(Person.jsonCodec.decodeJson(encoded))
    _       <- ZIO.debug(s"JSON object decoded to Person class: $decoded")
  } yield ()
}
```

## BinaryCodec

We can also derive a binary codec from a ZIO Schema using the `schemaBasedBinaryCodec`:

```scala
object JsonCodec {
  implicit def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A] = ???
}
```

Let's try an example:

```scala mdoc:compile-only
import zio._
import zio.schema.codec.BinaryCodec
import zio.schema.{DeriveSchema, Schema}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] =
    DeriveSchema.gen
  implicit val jsonBinaryCodec: BinaryCodec[Person] =
    zio.schema.codec.JsonCodec.schemaBasedBinaryCodec(schema)
}

object Main extends ZIOAppDefault {
  def run = for {
    _ <- ZIO.debug("JSON Codec Example:")
    person: Person = Person("John", 42)
    encoded: Chunk[Byte] = Person.jsonBinaryCodec.encode(person)
    _ <- ZIO.debug(s"person object encoded to Binary JSON: ${toHex(encoded)}")
    decoded <- ZIO.fromEither(Person.jsonBinaryCodec.decode(encoded))
    _ <- ZIO.debug(s"JSON object decoded to Person class: $decoded")
  } yield ()

  def toHex(bytes: Chunk[Byte]): String =
    bytes.map("%02x".format(_)).mkString(" ")
}
```

The output of the above program is:

```scala
JSON Codec Example:
person object encoded to JSON string: 7b 22 6e 61 6d 65 22 3a 22 4a 6f 68 6e 22 2c 22 61 67 65 22 3a 34 32 7d
JSON object decoded to Person class: Person(John,42)
```

## Configuration

`JsonCodec.Configuration` controls how a codec derived with `jsonCodec(config)` or `schemaBasedBinaryCodec(config)` encodes and decodes. Its `default` value applies when you pass no configuration. It has these parameters:

- `explicitEmptyCollections`: whether empty collections are written as `[]`, or omitted and read back as empty when missing.
- `explicitNulls`: whether `None` is written as `null`, or omitted and read back as `None` when missing.
- `discriminatorSettings`: how the case of a sealed trait is recorded, such as the discriminator field name and its format.
- `fieldNameFormat`: a `NameFormat` that renames every field, such as `NameFormat.SnakeCase`.
- `treatStreamsAsArrays`: whether streams are encoded and decoded as JSON arrays.
- `rejectExtraFields`: whether decoding fails on fields that the type does not declare.

An annotation on a type or field overrides the matching setting for that type or field. For example, `@rejectExtraFields` on one case class wins over `rejectExtraFields = false` in the configuration. See [Schema Annotations](../../schema-annotations.md) for the annotation forms.

To write a `Profile` with snake-case field names, and to omit empty `Option` values and empty lists:

```scala mdoc:silent:reset
import zio.schema.codec.JsonCodec
import zio.schema.{DeriveSchema, NameFormat, Schema}

final case class Profile(userName: String, nickname: Option[String], tags: List[String])

object Profile {
  implicit val schema: Schema[Profile] = DeriveSchema.gen
}

val config = JsonCodec.Configuration(
  explicitEmptyCollections = JsonCodec.ExplicitConfig(encoding = false),
  explicitNulls = JsonCodec.ExplicitConfig(encoding = false),
  fieldNameFormat = NameFormat.SnakeCase
)
```

Encoding a profile with no nickname and no tags leaves out both optional parts:

```scala mdoc
println(JsonCodec.jsonCodec(config)(Profile.schema).encodeJson(Profile("alice", None, Nil), None).toString)
```

By utilizing JSON codecs derived from ZIO Schema, developers can easily serialize and deserialize data in JSON format without writing boilerplate code. This enhances productivity and simplifies data handling in Scala applications.
