---
id: xml
title: "XML Codecs"
sidebar_label: "XML"
---

## Introduction

XML (Extensible Markup Language) is a widely used markup format for representing structured data. The `zio-schema-xml` module provides support for deriving XML codecs from a ZIO Schema, enabling easy serialization and deserialization of Scala data types to and from XML format.

The codec uses `scala-xml` under the hood for XML parsing and generation.

## Installation

To use XML codecs, add the following dependency to your `build.sbt`:

```scala
libraryDependencies += "dev.zio" %% "zio-schema-xml" % @VERSION@
```

## XmlCodec

The `XmlCodec` object inside the `zio.schema.codec` package provides the `xmlCodec` operator which allows deriving XML binary codecs from a ZIO Schema:

```scala
object XmlCodec {
  implicit def xmlCodec[A](implicit schema: Schema[A]): BinaryCodec[A]
  def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A]
}
```

## Example

```scala mdoc:compile-only
import zio._
import zio.schema.codec.XmlCodec
import zio.schema.{DeriveSchema, Schema}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen
}

object Main extends ZIOAppDefault {
  def run = for {
    _ <- ZIO.debug("XML Codec Example:")
    person = Person("John", 42)
    encoded: Chunk[Byte] = XmlCodec.xmlCodec[Person].encode(person)
    xml = new String(encoded.toArray, "UTF-8")
    _ <- ZIO.debug(s"Encoded XML:\n$xml")
    decoded <- ZIO.fromEither(XmlCodec.xmlCodec[Person].decode(encoded))
    _ <- ZIO.debug(s"Decoded back: $decoded")
  } yield ()
}
```

This produces:

```xml
<?xml version="1.0" encoding="UTF-8"?>
<Person><name>John</name><age>42</age></Person>
```

## Supported Types

The XML codec supports encoding and decoding of the following ZIO Schema types:

- **Primitives**: String, Boolean, Byte, Short, Int, Long, Float, Double, BigInteger, BigDecimal, Binary (Base64), Char, UUID, Currency, and all `java.time` types (Instant, LocalDate, LocalDateTime, ZonedDateTime, Duration, etc.)
- **Records**: Case classes with any number of fields
- **Enums / Sealed Traits**: Sum types with case class or case object variants
- **Collections**: List, Vector, Set, Map, NonEmptyList, NonEmptyMap
- **Optional**: `Option[A]`
- **Either**: `Either[A, B]`
- **Tuple**: `(A, B)`
- **Fallback**: `Fallback[A, B]` (Left, Right, Both)
- **Transform**: Schema transformations
- **Lazy**: Recursive schemas
- **Dynamic**: DynamicValue

## Annotation Support

The XML codec respects the following ZIO Schema annotations:

| Annotation | Description |
|---|---|
| `@fieldName("xml_name")` | Renames a record field in the XML output |
| `@fieldNameAliases("alias1", "alias2")` | Accepts alternative element names when decoding |
| `@transientField` | Excludes a field from encoding/decoding |
| `@fieldDefaultValue(value)` | Uses a default value when a field is missing during decoding |
| `@simpleEnum` | Encodes enum as plain text (case name only) instead of a wrapper element |
| `@caseName("name")` | Renames an enum case in the XML output |
| `@caseNameAliases("alias")` | Accepts alternative case names when decoding |
| `@discriminatorName("type")` | Uses a discriminator element (e.g. `<type>Dog</type>`) instead of wrapping in a case-name element |
| `@noDiscriminator` | Encodes enum cases without any discriminator; decoding tries each case |
| `@rejectExtraFields` | Rejects XML with unknown child elements during decoding |

## XML Structure

### Records

Record fields are encoded as child elements:

```xml
<Person><name>Alice</name><age>30</age></Person>
```

Optional fields with `None` are omitted entirely.

### Enums (Default)

Enum cases are wrapped in an element named after the case:

```xml
<Animal><Dog><name>Rex</name><breed>Lab</breed></Dog></Animal>
```

### Enums with `@discriminatorName`

```xml
<Animal><type>Dog</type><name>Rex</name><breed>Lab</breed></Animal>
```

### Simple Enums (`@simpleEnum`)

```xml
<Color>Red</Color>
```

### Collections

List/Set items are wrapped in `<item>` elements:

```xml
<items><item>1</item><item>2</item><item>3</item></items>
```

Map entries use `<entry>`, `<key>`, and `<value>`:

```xml
<map>
  <entry><key>a</key><value>1</value></entry>
  <entry><key>b</key><value>2</value></entry>
</map>
```

### Either

```xml
<value><left>42</left></value>
<!-- or -->
<value><right>hello</right></value>
```

### Streaming

The codec supports ZIO stream-based encoding and decoding via `streamEncoder` and `streamDecoder` pipelines.
