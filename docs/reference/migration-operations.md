---
id: migration-operations
title: "Migration Operations"
description: "Reference for the Migration cases that change a DynamicValue, with an example of each operation that applies to values."
keywords:
  - "ZIO Schema"
  - "Migration"
  - "DynamicValue"
  - "Reference"
---

A `Migration` is one step that changes a `DynamicValue` so that it matches a new schema. Several steps can be combined in one `Chunk`. The [Schema Migration](../operations/schema-migration.md) page shows how to run them and how `Migration.derive` produces them. This page lists each operation and what it does to a value.

```scala mdoc:silent
import zio.Chunk
import zio.schema._
import zio.schema.meta.{Migration, MetaSchema, NodePath}

case class Person1(name: String, age: Int)

object Person1 {
  implicit val schema: Schema[Person1] = DeriveSchema.gen[Person1]
}

val person1: DynamicValue = Person1.schema.toDynamic(Person1("John Doe", 42))

def applied(migrations: Migration*): Either[String, DynamicValue] =
  person1.transform(Chunk.fromIterable(migrations))
```

## Operations on Fields

`DeleteNode` removes the node at a path. Here, the `age` field disappears from the value:

```scala mdoc
applied(Migration.DeleteNode(NodePath.root / "age"))
```

`AddNode` adds a node with the shape given by a `MetaSchema`. The new node needs a default value, and the step fails when none is available. The following step tries to add a `nickname` field, and its result shows that error:

```scala mdoc
applied(Migration.AddNode(NodePath.root / "nickname", MetaSchema.fromSchema(Schema[String])))
```

`ChangeType` changes the standard type of the primitive at a path, but only for the conversions that the library supports. The following step tries to change `age` from an integer to a double, and the library reports that this conversion is not available:

```scala mdoc
applied(Migration.ChangeType(NodePath.root / "age", StandardType.DoubleType))
```

`Optional` wraps the value at a path in an option, which turns a required field into an optional one. The following step makes `age` optional, and the result has the field inside an option:

```scala mdoc
applied(Migration.Optional(NodePath.root / "age"))
```

`Require` is the reverse. It makes an optional node required, and it fails on a node that is not optional. The following step applies it to `age`, which is not optional, so the step fails:

```scala mdoc
applied(Migration.Require(NodePath.root / "age"))
```

## Operations on Values

`UpdateFail` replaces the failure message of a node that is a failure. It fails on any other node. The following step applies it to `age`, which is an integer, so the step fails:

```scala mdoc
applied(Migration.UpdateFail(NodePath.root / "age", "age can no longer be converted"))
```

`AddCase` records that an enum has a new case. Applying it leaves the value unchanged, since an existing value never uses the new case. Its effect shows up when the schema itself is migrated.

`IncrementDimensions` and `DecrementDimensions` add or remove a level of sequence nesting at a path. They are produced when a field changes from a single value to a collection, or back.

`Recursive` applies a migration to a recursive type, where a node refers to a part of the value that repeats. The path given to `Recursive` is relative to the repeated part, so one migration covers every level.

`Relabel` renames a node. It takes a `LabelTransformation`, and this version of the library provides no public cases for that type, so applications should not construct `Relabel` directly.

## Running Several Steps

`DynamicValue#transform` takes a `Chunk[Migration]` and applies the steps to a value. The [Schema Migration](../operations/schema-migration.md) page shows a complete example of running steps that `Migration.derive` produced.
