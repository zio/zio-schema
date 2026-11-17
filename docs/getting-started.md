---
id: getting-started
title: "Getting Started"
sidebar_label: "Getting Started"
description: "Install ZIO Schema, derive a schema for a case class, encode and decode JSON, validate values, and convert them to DynamicValue."
keywords:
  - "ZIO Schema"
  - "Getting Started"
  - "Schema derivation"
  - "JSON codec"
---

import Tabs from '@theme/Tabs';
import TabItem from '@theme/TabItem';

On this page we install ZIO Schema and write a first program. We describe a `Person` case class with a schema, then use that one schema to encode and decode JSON, to validate values, and to look at a value without knowing its type. Each step builds on the one before it, so read them in order.

## Add the Dependencies

The first program needs the core module and the JSON codec module. We add both to `build.sbt`:

```scala
libraryDependencies ++= Seq(
  "dev.zio" %% "zio-schema"      % "@VERSION@",
  "dev.zio" %% "zio-schema-json" % "@VERSION@"
)
```

The JSON module depends on `zio-schema-derivation`, which provides `DeriveSchema`, so we do not add it separately. On Scala 2, the derivation macros also need `scala-reflect` at compile time. On Scala 3, the project must use Scala 3.9 or later. The [Installation](index.md#installation) section shows how to add `scala-reflect`, and [Modules](index.md#modules) lists the other modules with the platforms they support, ready to copy, with `%%%` for Scala.js and Scala Native.

## 1. Define a Case Class and Derive Its Schema

A `Schema[A]` is a value that describes the structure of `A`: its name, its fields, and the types of those fields. We rarely write one by hand. Instead, we let the compiler derive it from the case class. The syntax depends on the Scala version:

<Tabs groupId="scala-version" defaultValue="scala2">
  <TabItem value="scala2" label="Scala 2">

```scala mdoc:silent
import zio.schema._
import zio.schema.annotation.validate
import zio.schema.validation.Validation

final case class Person(
  @validate(Validation.minLength(1)) name: String,
  @validate(Validation.between(0, 150)) age: Int
)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}
```

  </TabItem>
  <TabItem value="scala3" label="Scala 3">

```scala
import zio.schema._
import zio.schema.annotation.validate
import zio.schema.validation.Validation

final case class Person(
  @validate(Validation.minLength(1)) name: String,
  @validate(Validation.between(0, 150)) age: Int
) derives Schema
```

  </TabItem>
</Tabs>

On Scala 2, the `implicit val` in the companion object makes the schema available wherever a `Schema[Person]` is needed, without an import. On Scala 3, the `derives Schema` clause does the same thing. The `@validate` annotations attach rules to the fields; step 4 uses them.

## 2. Encode a Value as JSON

The `zio-schema-json` module turns any schema into a JSON codec. `JsonCodec.jsonCodec` returns a ZIO JSON `JsonCodec`, which can both encode and decode. We build the codec once and keep it:

```scala mdoc:silent
import zio.schema.codec.JsonCodec

val personCodec = JsonCodec.jsonCodec(Person.schema)
```

Encoding a `Person` produces its JSON text. The field names come from the case class:

```scala mdoc
val alice = Person("Alice", 30)

println(personCodec.encodeJson(alice, None).toString)
```

## 3. Decode JSON into a Value

The same codec reads JSON back. `JsonCodec#decodeJson` does not throw: it returns a `Right` with the value, or a `Left` with an error message:

```scala mdoc
personCodec.decodeJson("""{"name":"Bob","age":25}""")

personCodec.decodeJson("""{"name":"Bob"}""")
```

The second input has no `age` field, so decoding fails. Some fields may be left out: a missing `Option` field decodes as `None`, a missing collection as an empty one, and a missing field that has a default value in the case class takes that default. [Automatic Schema Derivation](automatic-schema-derivation.md) shows the default values.

## 4. Validate Values

Decoding checks the shape of the input, not the `@validate` rules. This input has the right shape, so it decodes, even though the name is empty and the age is out of range:

```scala mdoc
val decoded = personCodec.decodeJson("""{"name":"","age":200}""")
```

To check the rules, we call `Schema.validate` on the value. It returns one `ValidationError` per broken rule, and an empty `Chunk` when the value is valid:

```scala mdoc
decoded.map(person => Schema.validate(person).map(_.message))

Schema.validate(alice).isEmpty
```

In a real program, we decode first and validate second, and reject the input when either step fails.

## 5. Convert a Value to `DynamicValue`

A `DynamicValue` holds any value in a generic form: records, enums, sequences, and primitives, with no Scala type attached. Code that does not know about `Person`, such as a generic logger or a migration tool, can still read it. `Schema#toDynamic` converts a value to this form:

```scala mdoc:silent
val dynamic: DynamicValue = Person.schema.toDynamic(alice)
```

A record becomes a `DynamicValue.Record` that maps each field name to a `DynamicValue`. We can list the field names without referring to `Person`, and `Schema#fromDynamic` turns the generic value back into a typed one:

```scala mdoc
dynamic match {
  case DynamicValue.Record(_, values) => values.keys.toList
  case _                              => Nil
}

Person.schema.fromDynamic(dynamic)
```

## Next Steps

You now have one schema that drives JSON encoding, validation, and dynamic values. To go further:

- [Your First Schema](tutorial/first-schema.md) is a longer tutorial that also covers binary codecs and diffing and patching values.
- [Automatic Schema Derivation](automatic-schema-derivation.md) explains what `DeriveSchema` produces, and [Schema Derivation on Scala 2 and Scala 3](derivations/derivation-scala-versions.md) covers `derives Schema`, Scala 3 enums, and union types.
- [Schema Annotations](schema-annotations.md) lists the annotations that rename fields, set defaults, and change the encoding.
- [JSON Codecs](derivations/codecs/json.md) describes the JSON configuration options, and [Codecs](derivations/codecs/index.md) lists the other formats.
- [Validating Types](operations/validating-types.md) and [Dynamic Data Representation](operations/dynamic-data-representation.md) cover steps 4 and 5 in depth.
- [Schema Methods](reference/schema-methods.md) lists everything a schema can do.
