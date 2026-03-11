---
id: xml
title: "XML Codecs"
sidebar_label: "XML"
---

## Introduction

XML (Extensible Markup Language) is a widely used markup language for representing structured data. ZIO Schema provides the `zio-schema-xml` module that derives XML codecs from a ZIO Schema. The XML codec is implemented with zero dependencies, featuring its own XML AST with a hand-written parser (XmlReader) and writer (XmlWriter), modeled after the zio-blocks architecture. It supports cross-platform compilation for JVM, Scala.js, and Scala Native, allowing seamless serialization and deserialization of data in XML format across all platforms.

## Installation

To start using XML codecs in ZIO Schema, add the following dependency to your `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-schema-xml" % @VERSION@
```

## BinaryCodec

The `XmlCodec` object inside the `zio.schema.codec.xml` package provides the `schemaBasedBinaryCodec` operator which allows us to derive XML codecs from a ZIO Schema:

```scala
object XmlCodec {
  implicit def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A]
  def schemaBasedBinaryCodec[A](config: Configuration)(implicit schema: Schema[A]): BinaryCodec[A]
}
```

## Example: Basic Encoding/Decoding

Here's a basic example showing how to encode and decode data using the XML codec:

```scala mdoc:compile-only
import zio._
import zio.schema.codec.BinaryCodec
import zio.schema.codec.xml.XmlCodec
import zio.schema.{DeriveSchema, Schema}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] =
    DeriveSchema.gen
  implicit val xmlCodec: BinaryCodec[Person] =
    XmlCodec.schemaBasedBinaryCodec(schema)
}

object Main extends ZIOAppDefault {
  def run = for {
    _ <- ZIO.debug("XML Codec Example:")
    person: Person = Person("John", 42)
    encoded: Chunk[Byte] = Person.xmlCodec.encode(person)
    _ <- ZIO.debug(s"person object encoded to XML: ${new String(encoded.toArray)}")
    decoded <- ZIO.fromEither(Person.xmlCodec.decode(encoded))
    _ <- ZIO.debug(s"XML decoded to Person class: $decoded")
  } yield ()
}
```

The output produces the following XML structure:

```xml
<Person><name>John</name><age>42</age></Person>
```

## Example: Streaming Codecs

The XML codec supports streaming operations for processing large datasets efficiently:

```scala mdoc:compile-only
import zio._
import zio.schema.codec.BinaryCodec
import zio.schema.codec.xml.XmlCodec
import zio.schema.{DeriveSchema, Schema}
import zio.stream.ZStream

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] =
    DeriveSchema.gen
  implicit val xmlCodec: BinaryCodec[Person] =
    XmlCodec.schemaBasedBinaryCodec(schema)
}

object Main extends ZIOAppDefault {
  def run = for {
    _ <- ZIO.debug("XML Stream Codecs Example:")
    person = Person("John", 42)

    personToXml = Person.xmlCodec.streamEncoder
    xmlToPerson = Person.xmlCodec.streamDecoder

    newPerson <- ZStream(person)
      .via(personToXml)
      .via(xmlToPerson)
      .runHead
      .some
      .catchAll(error => ZIO.debug(error))
    _ <- ZIO.debug(
      "is old person the new person? " + (person == newPerson).toString
    )
    _ <- ZIO.debug("old person: " + person)
    _ <- ZIO.debug("new person: " + newPerson)
  } yield ()
}
```

## XML Annotations

ZIO Schema XML provides annotations to customize XML output:

### @xmlAttribute

The `@xmlAttribute` annotation marks a field to be encoded as an XML attribute instead of a child element:

```scala mdoc:compile-only
import zio.schema.codec.xml.xmlAttribute
import zio.schema.{DeriveSchema, Schema}

case class Person(@xmlAttribute id: Int, name: String)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen
}

// Produces: <Person id="42"><name>Alice</name></Person>
```

### @xmlNamespace

The `@xmlNamespace` annotation adds namespace information to a type:

```scala mdoc:compile-only
import zio.schema.codec.xml.xmlNamespace
import zio.schema.{DeriveSchema, Schema}

@xmlNamespace("http://example.com/ns", Some("ex"))
case class Item(name: String)

object Item {
  implicit val schema: Schema[Item] = DeriveSchema.gen
}

// Produces: <ex:Item xmlns:ex="http://example.com/ns"><name>Alice</name></ex:Item>
```

## Configuration

The XML codec supports configuration for both reading and writing:

### WriterConfig

The `WriterConfig` allows customization of XML output:

- `indentStep`: Number of spaces for indentation (0 for compact output)
- `includeDeclaration`: Whether to include XML declaration (`<?xml version="1.0"?>`)
- `encoding`: Character encoding (default: "UTF-8")

```scala
object WriterConfig {
  val default: WriterConfig         // No indentation, no declaration
  val pretty: WriterConfig          // 2-space indentation
  val withDeclaration: WriterConfig // Includes XML declaration
}
```

### ReaderConfig

The `ReaderConfig` provides safety limits for parsing:

- `maxDepth`: Maximum nesting depth (default: 1000)
- `maxAttributes`: Maximum attributes per element (default: 1000)
- `maxTextLength`: Maximum text content length (default: 10,000,000)
- `preserveWhitespace`: Whether to preserve whitespace in text content (default: false)

### Configuration Example

Here's how to configure the XML codec:

```scala mdoc:compile-only
import zio.schema.codec.xml.XmlCodec
import zio.schema.codec.xml.WriterConfig
import zio.schema.{DeriveSchema, Schema}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen
  
  val config = XmlCodec.Configuration(
    writerConfig = WriterConfig.pretty.copy(includeDeclaration = true)
  )
  
  val xmlCodec = XmlCodec.schemaBasedBinaryCodec(config)(schema)
}

// Produces formatted output with XML declaration:
// <?xml version="1.0" encoding="UTF-8"?>
// <Person>
//   <name>John</name>
//   <age>42</age>
// </Person>
```

The XML codec handles all ZIO Schema types including primitives, records, enums, collections, `Either`, `Tuple`, `Optional`, and recursive types. It properly escapes entity references (`&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;`) and supports CDATA sections for preserving special characters in text content.
