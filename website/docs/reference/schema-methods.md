---
id: schema-methods
title: "Schema Methods"
description: "Reference for the methods of Schema[A]: combinators, defaults, validation, conversion, and diff and patch."
keywords:
  - "ZIO Schema"
  - "Schema"
  - "Reference"
  - "Combinators"
---

Every `Schema[A]` provides the methods below. They are grouped by purpose. Each example uses the same `Person` model:

```scala
import zio.schema._

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val john = Person("John", 42)
```

## Building Schemas

Combinators build a new schema from existing ones. `Schema#optional`, written `?`, accepts a missing value. `Schema#zip`, written `<*>`, pairs two schemas into a tuple schema. `Schema#orElseEither`, written `<+>`, accepts a value of either schema, as an `Either`. `Schema#repeated` returns a schema for a `Chunk` of values.

```scala
val maybeAge: Schema[Option[Int]] = Schema[Int].?

val pair: Schema[(String, Int)] = Schema[String] <*> Schema[Int]

val either: Schema[Either[String, Int]] = Schema[String] <+> Schema[Int]

val names: Schema[zio.Chunk[String]] = Schema[String].repeated
```

## Defaults and Annotations

`Schema#defaultValue` returns the default value of the schema. `Schema#default` returns a copy of the schema with a different default. `Schema#annotate` attaches an annotation of any type, and `Schema#annotations` lists the annotations of the schema. Schema annotations, such as `fieldName`, are covered in the [Basic Building Blocks](../basic-building-blocks.md) page.

```scala
Schema[Int].default(7).defaultValue
// res0: Either[String, Int] = Right(value = 7)

Schema[Int].annotations.isEmpty
// res1: Boolean = true
```

For more about defaults, see [Getting The Default Value](../operations/the-default-value.md).

## Validation

`Schema#validate` checks a value against the rules that its fields carry and returns the failed checks as a `Chunk`. An empty chunk means that the value passes. `Schema#validation` attaches a `Validation` rule to the schema itself. The rules and their combinators are described in the [Validation](../operations/validating-types.md) page.

```scala
Person.schema.validate(john).isEmpty
// res2: Boolean = true
```

## Transformations

`Schema#transform` maps a schema of `A` to a schema of `B`, given a function in each direction. `Schema#transformOrFail` does the same, but each function can fail with a message. These methods are covered in [Transforming Schemas](../operations/transforming-schemas.md).

```scala
val nameOnly: Schema[String] = Person.schema.transform(_.name, name => Person(name, 0))
```

```scala
nameOnly.defaultValue
// res3: Either[String, String] = Right(value = "")
```

## Conversion Through DynamicValue

`Schema#toDynamic` turns a value into a `DynamicValue`, and `Schema#fromDynamic` turns it back. `DynamicValue` is covered in [Dynamic Data Representation](../operations/dynamic-data-representation.md).

```scala
val dynamic = Person.schema.toDynamic(john)
// dynamic: DynamicValue = Record(
//   id = Nominal(
//     packageName = IndexedSeq("repl"),
//     objectNames = IndexedSeq("MdocSession", "MdocApp"),
//     typeName = "Person"
//   ),
//   values = ListMap(
//     "name" -> Primitive(value = "John", standardType = string),
//     "age" -> Primitive(value = 42, standardType = int)
//   )
// )

Person.schema.fromDynamic(dynamic) == Right(john)
// res4: Boolean = true
```

## Migration

`Schema#migrate` returns a function that converts values of this schema to values of another schema, when the two schemas have a clear mapping. `Schema#coerce` returns a schema for the new type when migrations exist in both directions, so values can convert back and forth. See [Schema Migration](../operations/schema-migration.md) for the full process.

## Diff and Patch

`Schema#diff` compares two values and returns a `Patch`. `Schema#patch` applies a patch to an old value. See [Diffing and Patching](../operations/diffing-and-patching.md) for details.

```scala
val older = john.copy(age = 43)
// older: Person = Person(name = "John", age = 43)

Person.schema.patch(john, Person.schema.diff(john, older)) == Right(older)
// res5: Boolean = true
```

## Ordering and Structure

`Schema#ordering` returns a Scala `Ordering` for the type, which is useful for sorting. `Schema#ast` returns the `MetaSchema` tree that describes the structure of the schema.

```scala
List(john, Person("Ann", 30)).sorted(Person.schema.ordering).map(_.name)
// res6: List[String] = List("Ann", "John")
```

`Schema#serializable` returns a schema that describes the schema itself. It currently fails at runtime for case classes; see [Serialization of the Schema Itself](../operations/serialization-of-the-schema-itself.md).
