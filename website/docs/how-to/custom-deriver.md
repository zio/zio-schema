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

A `Deriver` builds an instance of a type class for any type that has a schema. The built-in derivations, such as the one behind `DeriveSchema.gen`, produce schemas. A custom deriver produces something else, such as a pretty-printer, a validator, or a count of fields. You write one method for each kind of schema, and the library walks the structure of the type and calls those methods.

This guide writes a deriver for a type class that counts the primitive values inside a type. A `Person` has two, its name and its age.

## Define the Type Class

The type class is an ordinary trait with one type parameter. It has a single method, `count`:

```scala
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
- `deriveTransformedRecord` handles a record that is converted to another type. It counts the fields of the record.

The `summoned` argument is an instance that the compiler found in scope, if there is one. Each method returns it when present, so a hand-written instance always wins:

```scala
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

```scala
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

```scala
Derive.derive[LeafCount, Person](leafCounter).count
// res0: Int = 2

Derive.derive[LeafCount, Team](leafCounter).count
// res1: Int = 4
```

A `Person` has two primitive values. A `Team` has the members, which are a list of persons, plus the lead, which is an optional person. The list counts its element type, so the members contribute 2, and the lead contributes 2. The total is 4.

## Where to Go Next

- [Automatic Schema Derivation](../automatic-schema-derivation.md) describes the built-in derivation of schemas.
- [Write a Custom Codec](./custom-codec.md) shows the other common extension point.
- [Schema Methods](../reference/schema-methods.md) lists what a derived schema can do.
