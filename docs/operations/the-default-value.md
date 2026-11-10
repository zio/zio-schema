---
id: the-default-value
title: "Getting The Default Value"
sidebar_label: "The Default Value"
---

Some tools need a sample value for a type, for example to fill a form, to build a test fixture, or to show a placeholder. The `defaultValue` method of a schema returns such a value. It has this signature:

```scala
sealed trait Schema[A] {
  def defaultValue: scala.util.Either[String, A]
}
```

The method returns `Right` with a value when the schema can produce one, and `Left` with an error message when it cannot.

## Standard Types

Every standard type has a default value. For example, `String` defaults to the empty string, and `Int` defaults to `0`:

```scala mdoc
import zio.schema._

Schema[String].defaultValue
Schema[Int].defaultValue
```

The standard types that generate random values, such as `UUID`, produce a new value on each call. The platform support for each type is listed in the [Standard Type Reference](../standard-type-reference.md).

## Overriding a Default with `default`

The `Schema#default` method returns a copy of the schema that uses the value you pass as its default:

```scala mdoc
val retries: Schema[Int] = Schema[Int].default(3)

retries.defaultValue
```

The original `Schema[Int]` keeps its own default, so the override affects only the new schema.

## Records and Enumerations

A record builds its default from the default value of each field. A field can take its default from the `default` method, and otherwise it uses the default of its own schema. If any field has no default, the record has none. The following case class uses only standard types, so every field has one:

```scala mdoc:silent
case class Settings(host: String, port: Int)

object Settings {
  implicit val schema: Schema[Settings] = DeriveSchema.gen[Settings]
}
```

```scala mdoc
Settings.schema.defaultValue
```

An enumeration uses the default value of its first case, in the order the cases appear in the schema.
