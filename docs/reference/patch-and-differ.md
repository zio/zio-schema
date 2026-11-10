---
id: patch-and-differ
title: "Patch and Differ"
description: "Reference for the Differ and Patch types that compute and apply changes between two values."
keywords:
  - "ZIO Schema"
  - "Patch"
  - "Differ"
  - "Reference"
---

`Differ[A]` computes the difference between two values of type `A`, and `Patch[A]` describes that difference so it can be applied to an old value. Most code uses the `diff` and `patch` methods on a schema, which are described in [Diffing and Patching](../operations/diffing-and-patching.md). This page covers the types themselves, for code that builds or combines patches directly.

## Differ

A `Differ[A]` has one abstract method, `apply`, which takes the old and new values and returns a `Patch[A]`. `Schema#diff` builds its differ from the schema through `Differ.fromSchema`. The `Differ` trait also has combinators:

- `Differ#zip`, written `<*>`, combines two differs into one for a pair.
- `Differ#transform` adapts a differ to a related type, given conversions in both directions.
- `Differ#chunk` returns a differ for `Chunk[A]`, which uses a longest common subsequence algorithm to find inserted and removed elements.
- `Differ#optional` returns a differ for `Option[A]`.

The following example diffs two chunks. The patch records that the element `2` was removed, and applying it to the old chunk gives the new one:

```scala mdoc
import zio.Chunk
import zio.schema._

val listDiffer: Differ[Chunk[Int]] = Differ.fromSchema(Schema[Int]).chunk

val patch = listDiffer(Chunk(1, 2, 3), Chunk(1, 3))

patch.patch(Chunk(1, 2, 3))
```

## Patch

A `Patch[A]` is a sealed trait with these operations:

- `patch` applies the patch to a value and returns `Either[String, A]`.
- `invert` returns the patch that undoes this one, so applying the inverse to the new value gives the old value.
- `zip`, written `<*>`, combines a patch for `A` with a patch for `B` into one for `(A, B)`.
- `isIdentical` is `true` when the patch changes nothing.
- `isComparable` is `false` for the patch returned by `Patch.notComparable`, which reports that two values cannot be compared.

The `Patch` companion object provides `identical` and `notComparable`, which build the two special patches. Use them when you write your own differ and need to report no change or an incomparable pair.

The following example shows the inverse. Applying the inverted patch to the newer value restores the older one:

```scala mdoc:silent
case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val john  = Person("John", 42)
val older = john.copy(age = 43)
```

```scala mdoc
val forward = Person.schema.diff(john, older)

Person.schema.patch(older, forward.invert) == Right(john)
```

Comparing a value with itself gives an identical patch:

```scala mdoc
Person.schema.diff(john, john).isIdentical

Patch.identical[Person].isIdentical
```

## Writing Patches by Hand

Patches are data, so they can be built without a differ. The `Patch.identical` and `Patch.notComparable` helpers cover the two trivial cases. Every other patch comes from a differ. For a custom type, write a `Differ` with `Differ.instancePartial` or start from `Differ.identical` when no change is possible.
