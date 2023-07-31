---
id: bson
title: "Bson Codecs"
sidebar_label: "BSON"
---

## Introduction

BSON (Binary JSON) is a binary serialization format used to store and exchange data efficiently. In this article, we will explore how to derive BSON codecs from a ZIO Schema. The `zio-schema-bson` module, provides support for deriving codecs from ZIO Schema, and makes it easy to communicate data in BSON format.

## Installation

To use BSON codecs, you need to add the following dependency to your Scala project:

```scala
libraryDependencies += "dev.zio" %% "zio-schema-bson" % @VERSION@
```

## BsonSchemaCodec

The ZIO Schema library provides the `BsonSchemaCodec` object in the `zio.schema.codec` package. This object offers three methods that allow us to derive BSON encoders, decoders, and codecs from ZIO Schemas:

```scala
object BsonSchemaCodec {
  def bsonEncoder[A](schema: Schema[A]): BsonEncoder[A]
  def bsonDecoder[A](schema: Schema[A]): BsonDecoder[A]
  def bsonCodec[A](schema: Schema[A]): BsonCodec[A]
}
```

It has three methods, by calling each of them, we can get a `BsonEncoder[A]`, `BsonDecoder[A]`, or `BsonCodec[A]` from a `Schema[A]`. Let's see a simplified version of each of these traits:

### 1. BsonEncoder

The `BsonEncoder` trait defines a type class for encoding a value of type `A` into a BSON value. The `toBsonValue` method accomplishes this conversion:

```scala
trait BsonEncoder[A] {
  def toBsonValue(value: A): BsonValue
}
```

### 2. BsonDecoder

The BsonDecoder trait defines a type class for decoding a BSON value into a value of type A. The fromBsonValue method handles this conversion and returns an Either indicating success or an error:

```scala
trait BsonDecoder[A] {
  def fromBsonValue(value: BsonValue): Either[BsonDecoder.Error, A]
}
```

### 3. BsonCodec

The `BsonCodec` case class combines both the BSON encoder and decoder for a specific type `A`:

```scala
final case class BsonCodec[A](
  encoder: BsonEncoder[A],
  decoder: BsonDecoder[A]
)
```

## Example: Deriving a Bson Codec for a Case Class

Let's see an example of how to derive a BSON codec for a case class using ZIO Schema:

```scala mdoc:compile-only
import org.bson.BsonValue
import zio._
import zio.bson._
import zio.schema.codec._
import zio.schema.{DeriveSchema, Schema}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen
  implicit val bsonCodec: BsonCodec[Person] =
    BsonSchemaCodec.bsonCodec(Person.schema)
}

object Main extends ZIOAppDefault {
  def run = for {
    _ <- ZIO.debug("Bson Example:")
    person: Person     = Person("John", 42)
    encoded: BsonValue = person.toBsonValue
    _       <- ZIO.debug(s"person object encoded to BsonValue: $encoded")
    decoded <- ZIO.fromEither(encoded.as[Person])
    _ <- ZIO.debug(s"BsonValue of person object decoded to Person: $decoded")
  } yield ()
}
```

In the example above, we defined a case class `Person` with fields `name` and `age`. We then derived a ZIO Schema for the `Person` case class using `DeriveSchema.gen`.

The `BsonSchemaCodec.bsonCodec` method allowed us to create a BSON codec for the `Person` case class by passing its corresponding ZIO Schema. Now, we can effortlessly encode `Person` objects to BSON and decode BSON values back to Person instances.
