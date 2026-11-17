---
id: custom-deriver
title: "Write a Custom Deriver"
description: "Build a Deriver that derives your own type class from any schema, by implementing the methods for each kind of schema."
keywords:
  - "ZIO Schema"
  - "Deriver"
  - "Type class derivation"
  - "How-to"
---

A `Deriver` builds an instance of a type class for any type that has a schema. `DeriveSchema.gen` goes from a type to its schema; a deriver goes one step further, from the schema to something else, such as a pretty-printer, a validator, or a count of fields. You write one method for each kind of schema, and the `Derive.derive` macro walks the structure of the type and calls those methods. `DeriveSchema.gen` itself is a separate macro and does not use `Deriver`.

This guide writes a deriver for a type class that counts the primitive values inside a type. A `Person` has two, its name and its age.

## Define the Type Class

The type class is an ordinary trait with one type parameter. It has a single method, `count`:

```scala mdoc:silent
import zio.Chunk
import zio.schema._
import zio.schema.Deriver.WrappedF

trait LeafCount[A] {
  def count: Int
}

def known[A](n: Int): LeafCount[A] = new LeafCount[A] {
  def count: Int = n
}
```

The `known` helper builds an instance from a number. The deriver uses it for every kind of schema.

## Implement the Deriver

A `Deriver[F]` is parameterized by the type class `F`. The seven methods below are abstract, so every custom deriver must implement them. Each receives the nested instances for the parts of the type it contains:

- `derivePrimitive` handles a standard type, such as `String` or `Int`. A primitive has one value, so it counts as 1.
- `deriveRecord` handles a case class. Its `fields` are the instances for each field, and the count is their sum.
- `deriveEnum` handles a sealed trait. The count is the largest count of its cases.
- `deriveOption` and `deriveSequence` handle `Option` and collections. They count the same values as the element type.
- `deriveMap` handles a map. It counts the keys and the values.
- `deriveTransformedRecord` handles a case class with more than 22 fields. Its schema is a `GenericRecord` (a map from field names to values) wrapped in a transformation to the case class, so the method receives both. It counts the fields of the record.

The `summoned` argument is an instance of the type class that the compiler finds at the `Derive.derive` call site (an implicit value in scope or in a companion object), if there is one. The deriver decides whether to use it. Each method below returns it when present, so a hand-written instance replaces the derived one. The [Use Hand-Written Instances](#use-hand-written-instances) section shows when the compiler looks for one:

```scala mdoc:silent
val leafCounter: Deriver[LeafCount] = new Deriver[LeafCount] {

  override def derivePrimitive[A](st: StandardType[A], summoned: => Option[LeafCount[A]]): LeafCount[A] =
    summoned.getOrElse(known(1))

  override def deriveRecord[A](
    record: Schema.Record[A],
    fields: => Chunk[WrappedF[LeafCount, _]],
    summoned: => Option[LeafCount[A]]
  ): LeafCount[A] =
    summoned.getOrElse(known(fields.map(_.unwrap.count).sum))

  override def deriveEnum[A](
    `enum`: Schema.Enum[A],
    cases: => Chunk[WrappedF[LeafCount, _]],
    summoned: => Option[LeafCount[A]]
  ): LeafCount[A] =
    summoned.getOrElse(known(cases.map(_.unwrap.count).foldLeft(0)(math.max)))

  override def deriveOption[A](
    option: Schema.Optional[A],
    inner: => LeafCount[A],
    summoned: => Option[LeafCount[Option[A]]]
  ): LeafCount[Option[A]] =
    summoned.getOrElse(known(inner.count))

  override def deriveSequence[C[_], A](
    sequence: Schema.Sequence[C[A], A, _],
    inner: => LeafCount[A],
    summoned: => Option[LeafCount[C[A]]]
  ): LeafCount[C[A]] =
    summoned.getOrElse(known(inner.count))

  override def deriveMap[K, V](
    map: Schema.Map[K, V],
    key: => LeafCount[K],
    value: => LeafCount[V],
    summoned: => Option[LeafCount[Map[K, V]]]
  ): LeafCount[Map[K, V]] =
    summoned.getOrElse(known(key.count + value.count))

  override def deriveTransformedRecord[A, B](
    record: Schema.Record[A],
    transform: Schema.Transform[A, B, _],
    fields: => Chunk[WrappedF[LeafCount, _]],
    summoned: => Option[LeafCount[B]]
  ): LeafCount[B] =
    summoned.getOrElse(known(fields.map(_.unwrap.count).sum))
}
```

The `fields` and `cases` arguments are lazy, so the deriver reads them only when it needs them. Each `unwrap` call gives the instance for one field or case.

## Derive an Instance

The `Derive.derive` method applies a deriver to a type. It needs the type's schema in scope, which the companion object provides. We define two models, one of which contains a list and an option:

```scala mdoc:silent
case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

case class Team(members: List[Person], lead: Option[Person])

object Team {
  implicit val schema: Schema[Team] = DeriveSchema.gen[Team]
}
```

Next, we derive the instances and read their counts:

```scala mdoc
Derive.derive[LeafCount, Person](leafCounter).count

Derive.derive[LeafCount, Team](leafCounter).count
```

A `Person` has two primitive values. A `Team` has the members, which are a list of persons, plus the lead, which is an optional person. The list counts its element type, so the members contribute 2, and the lead contributes 2. The total is 4.

`Derive.derive` follows the Scala type and expects the schema in scope to have the shape that `DeriveSchema.gen` produces for it. A type it does not recognize, such as a plain class with a hand-written schema, needs a summoned instance; without one, `Deriver#deriveUnknown` throws an `IllegalArgumentException` at runtime.

On Scala 3, a deriver also has a `Deriver#derive` method, so `leafCounter.derive[Person]` is the same as `Derive.derive[LeafCount, Person](leafCounter)`.

## Override the Optional Methods

Besides the seven abstract methods, a few methods have default implementations that you can override when a kind of schema needs special handling:

- `deriveEither` handles `Either`. By default, it treats it as an enum with two cases and calls `deriveEnum`.
- `deriveSet` handles `Set`. By default, it calls `deriveSequence`.
- `deriveTupleN` handles tuples. By default, it treats the tuple as a record and calls `deriveRecord`.
- `derivePrimitiveAlias` handles a Scala 3 opaque type whose underlying type is a primitive. By default, it returns the summoned instance and throws when there is none.

## Use Hand-Written Instances

The compiler looks for a summoned instance for every type nested inside the derived type, but never for the top-level type passed to `Derive.derive`. That top-level instance is usually the one being defined, so looking it up would find the definition itself.

To see the difference, we give `Secret` a hand-written instance that counts nothing, so secrets do not show up in the count:

```scala mdoc:silent
case class Secret(value: String, hint: String)

object Secret {
  implicit val schema: Schema[Secret]       = DeriveSchema.gen[Secret]
  implicit val leafCount: LeafCount[Secret] = known(0)
}

case class Account(owner: String, password: Secret)

object Account {
  implicit val schema: Schema[Account] = DeriveSchema.gen[Account]
}
```

Inside `Account`, the deriver receives the hand-written instance for `password` and returns it. When `Secret` is the top-level type, `summoned` is `None`, so the deriver counts its two fields:

```scala mdoc
Derive.derive[LeafCount, Account](leafCounter).count

Derive.derive[LeafCount, Secret](leafCounter).count
```

Our deriver checks `summoned` in every method. A deriver that should always prefer a hand-written instance can skip that check and call `Deriver#autoAcceptSummoned` instead. It returns a deriver that uses the summoned instance whenever there is one, and calls the original methods only when there is none:

```scala mdoc:silent
val acceptingCounter: Deriver[LeafCount] = leafCounter.autoAcceptSummoned
```

## Reuse Instances with `Deriver#cached`

Each `Derive.derive` call derives every nested instance again. `Deriver#cached` returns a deriver that stores each instance it builds and returns the stored one the next time the same type comes up, across all calls that use it. Because `Deriver#cached` is a `lazy val`, calling it twice on the same deriver returns the same cache:

```scala mdoc:silent
val cachedCounter: Deriver[LeafCount] = leafCounter.cached
```

Deriving `Team` after `Person` reuses the `Person` instance from the first call:

```scala mdoc
Derive.derive[LeafCount, Person](cachedCounter).count

Derive.derive[LeafCount, Team](cachedCounter).count
```

:::caution
The cache identifies a case class or sealed trait by its `TypeId`, which does not include type arguments. A cached deriver therefore returns the same instance for `Box[Int]` and `Box[String]`, whichever it derived first. Use an uncached deriver for generic types.
:::

## Write Generic Methods with `Factory`

`Derive.derive` is a macro, so it needs the concrete type at the place where it is called. A generic method such as `def leafCountOf[A]` cannot call it with its own type parameter `A`. The `Factory` type class moves that macro call to the caller: a `Factory[A]` is derived automatically wherever `A` is a concrete type, and `Factory#derive` applies a deriver to `A`.

We add a `Factory` context bound next to the `Schema` bound:

```scala mdoc:silent
def leafCountOf[A: Factory: Schema]: LeafCount[A] =
  implicitly[Factory[A]].derive[LeafCount](leafCounter)
```

The caller passes a concrete type, and the compiler derives its `Factory`:

```scala mdoc
leafCountOf[Team].count
```

On Scala 2, the code that the `Factory` macro generates refers to `Factory`, `Deriver`, `Schema`, and `Derive` by their short names, so the code that calls a method like `leafCountOf` needs `import zio.schema._`.

## Where to Go Next

- [Automatic Schema Derivation](../automatic-schema-derivation.md) describes the built-in derivation of schemas.
- [Write a Custom Codec](./custom-codec.md) shows the other common extension point.
- [Schema Methods](../reference/schema-methods.md) lists what a derived schema can do.
