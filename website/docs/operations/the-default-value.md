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

```scala
import zio.schema._

Schema[String].defaultValue
// res0: Either[String, String] = Right(value = "")
Schema[Int].defaultValue
// res1: Either[String, Int] = Right(value = 0)
```

The standard types that generate random values, such as `UUID`, produce a new value on each call. The platform support for each type is listed in the [Standard Type Reference](../standard-type-reference.md).

## Overriding a Default with `default`

The `Schema#default` method returns a copy of the schema that uses the value you pass as its default:

```scala
val retries: Schema[Int] = Schema[Int].default(3)
// retries: Schema[Int] = Primitive(
//   standardType = int,
//   annotations = IndexedSeq(fieldDefaultValue(value = 3))
// )

retries.defaultValue
// res2: Either[String, Int] = Right(value = 3)
```

The original `Schema[Int]` keeps its own default, so the override affects only the new schema.

## Records and Enumerations

A record builds its default from the default value of each field. A field can take its default from the `default` method, and otherwise it uses the default of its own schema. If any field has no default, the record has none. The following case class uses only standard types, so every field has one:

```scala
case class Settings(host: String, port: Int)

object Settings {
  implicit val schema: Schema[Settings] = DeriveSchema.gen[Settings]
}
```

```scala
Settings.schema.defaultValue
// res3: Either[String, Settings] = Right(
//   value = Settings(host = "", port = 0)
// )
```

An enumeration uses the default value of its first case, in the order the cases appear in the schema.
