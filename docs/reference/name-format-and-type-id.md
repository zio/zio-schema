---
id: name-format-and-type-id
title: "Name Formats and Type Identifiers"
description: "Reference for NameFormat, which converts member names between naming styles, and TypeId, which identifies a schema's type."
keywords:
  - "ZIO Schema"
  - "NameFormat"
  - "TypeId"
  - "Reference"
---

Two small types appear in many schemas. `NameFormat` converts the names of fields and cases into another naming style, such as `snake_case`. `TypeId` identifies the type that a schema describes, such as `com.example.Person`.

## NameFormat

A `NameFormat` is a function from `String` to `String`. The `NameFormat` object provides the standard styles:

- `NameFormat.CamelCase` turns words into `camelCase`.
- `NameFormat.PascalCase` turns words into `PascalCase`.
- `NameFormat.SnakeCase` turns words into `snake_case`.
- `NameFormat.KebabCase` turns words into `kebab-case`.
- `NameFormat.Identity` returns the name unchanged.
- `NameFormat.Custom(f)` applies any function `f`.

The following examples show each style applied to the same name:

```scala mdoc
import zio.schema.NameFormat

NameFormat.SnakeCase("firstName")
NameFormat.CamelCase("first_name")
NameFormat.PascalCase("first_name")
NameFormat.KebabCase("firstName")
```

A `NameFormat` is applied to names in the codecs that support it. For example, the `fieldNameFormat` parameter of `JsonCodec.Configuration` takes a name format for JSON keys, as described in the [JSON Codecs](../derivations/codecs/json.md) page. A format changes only the names used for encoding, not the field names of the Scala type.

`NameFormat.Custom` accepts any function, so one can produce names that the standard styles do not cover:

```scala mdoc
val upperCase = NameFormat.Custom(_.toUpperCase)

upperCase("firstName")
```

## TypeId

A `TypeId` identifies the type described by a schema. It has two cases:

- `TypeId.Nominal` names a type that is declared in Scala. It stores the package names, the names of enclosing objects, and the type name.
- `TypeId.Structural` marks a type that has no name, such as an anonymous structure.

Use `TypeId.fromTypeName` to build an identifier for a type with no package, and `TypeId.parse` to read a fully qualified name:

```scala mdoc
import zio.schema.TypeId

TypeId.fromTypeName("Person").name

TypeId.parse("com.example.Outer.Person")
```

`TypeId.parse` cannot know which parts of a dotted name are packages and which are objects, so it guesses from the case of the first letter. Reading from the right, the last part is the type name, the parts before it that start with an upper-case letter are object names, and everything to their left is the package. A package name that starts with an upper-case letter is therefore read as an object name. An empty string parses to `TypeId.Structural`.

`TypeId.Nominal#fullyQualified` joins its parts back into a dotted name:

```scala mdoc
TypeId.parse("com.example.Outer.Person") match {
  case nominal: TypeId.Nominal => nominal.fullyQualified
  case other                   => other.name
}
```

Derived schemas get their `TypeId` from the type they describe, so you rarely build one by hand. To read the identifier of any schema, call `Schema.getTypeId`. It returns the `TypeId` of a record or an enumeration, the identifier of the Scala or Java type for standard types and built-in collections, the identifier of the underlying schema for a transformed schema, and `TypeId.Structural` for a schema created with `Schema.fail`:

```scala mdoc
import zio.schema.{DeriveSchema, Schema}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

Schema.getTypeId(Person.schema).name
```

`TypeId` has its own schema, `TypeId.schema`, which encodes an identifier as its fully qualified name and reads it back with `TypeId.parse`. This lets a `TypeId` be stored or sent like any other value.
