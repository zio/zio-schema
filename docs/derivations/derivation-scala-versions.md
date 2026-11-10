---
id: derivation-scala-versions
title: "Schema Derivation on Scala 2 and Scala 3"
description: "Explains how DeriveSchema differs between Scala 2 macros and Scala 3 inline methods, and how to call it on each version."
keywords:
  - "ZIO Schema"
  - "Schema derivation"
  - "Scala 2"
  - "Scala 3"
---

`DeriveSchema.gen` builds a `Schema` for a case class or sealed trait at compile time. The Scala version decides how that call is written, because the two compilers offer different ways to define a derivation. This page explains the difference so you can use the right form on each version.

## Scala 2: An Implicit Macro

On Scala 2, `DeriveSchema.gen` is an implicit whitebox macro. A whitebox macro can see the concrete types it is given and can return a tree that the compiler then type-checks. Because `gen` is implicit, the compiler finds it whenever a `Schema[T]` is needed and `T` is a supported type.

The following example derives the schema in the companion object, where the implicit value is needed by the rest of the program:

```scala mdoc:silent
import zio.schema._

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}
```

Placing the `implicit val` in the companion object means the schema is found without an import wherever `Person` is used.

## Scala 3: An Inline Method

On Scala 3, `DeriveSchema.gen` is a `transparent inline` method that expands during compilation. It is not implicit, so you call it explicitly and bind the result to an implicit value yourself, as the previous section does on Scala 2.

Scala 3 also provides an extension method on the `Schema` companion object, `Schema.derived[A]`. It calls `DeriveSchema.gen[A]` and is the Scala 3 spelling for the same operation:

```scala
implicit val schema: Schema[Person] = Schema.derived[Person]
```

Both spellings produce the same schema. Use whichever your team prefers.

## Choosing a Form

The table summarizes the differences:

| Aspect                  | Scala 2                     | Scala 3                          |
|-------------------------|-----------------------------|----------------------------------|
| Mechanism               | Whitebox macro              | Transparent inline method        |
| `DeriveSchema.gen`      | Implicit                    | Not implicit                     |
| Extra entry point       | None                        | `Schema.derived[A]`              |
| Typical use             | `implicit val schema = DeriveSchema.gen[T]` in the companion object | `implicit val schema = Schema.derived[T]` or `DeriveSchema.gen[T]` |

Both forms need the `zio-schema-derivation` module on the classpath. Derivation works for case classes and sealed traits.

For a walkthrough of deriving schemas in a project, see [Automatic Schema Derivation](../automatic-schema-derivation.md).
