---
id: dynamic-data-representation
title: "Dynamic Data Representation"
---

[`DynamicValue`](../reference/dynamic-value.md) is a way to describe the entire universe of possibilities for schema values. It does that in a way that we can interact with and introspect the data with its structure (type information). The structure of the data is baked into the data itself.

We can create a `DynamicValue` from a schema and a value using `DynamicValue.fromSchemaAndValue` (or `Schema#toDynamic`). We can turn it back into a typed value using `DynamicValue#toTypedValue`, which reports a failure as a message, or `DynamicValue#toValue`, which reports it as a `DecodeError`:

```scala
sealed trait DynamicValue {
  def toTypedValue[A](implicit schema: Schema[A]): Either[String, A]
  def toValue[A](implicit schema: Schema[A]): Either[DecodeError, A]
  def toTypedValueOption[A](implicit schema: Schema[A]): Option[A]
}

object DynamicValue {
  def apply[A](a: A)(implicit ev: Schema[A]): DynamicValue
  def fromSchemaAndValue[A](schema: Schema[A], value: A): DynamicValue
}
```

To convert `Person("John Doe", 42)` to a `DynamicValue`, we pass it to `DynamicValue.fromSchemaAndValue` together with its schema:

```scala mdoc:silent
import zio.schema._

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val person        = Person("John Doe", 42)
val dynamicPerson = DynamicValue.fromSchemaAndValue(Person.schema, person)
```

Calling `Person.schema.toDynamic(person)` gives the same result. Printing the dynamic value shows the mixture of the data and its structure:

```scala mdoc
println(dynamicPerson)
```

The record keeps the `TypeId` of `Person`, and each field keeps its value together with its standard type. This is in contrast to the relational database model, where the data structure is stored in the database schema and the data itself is stored in a separate location.

However, when we switch to document-based database models, such as JSON or XML, we can store both the data and its structure together. The JSON data model serves as a good example of self-describing data, as it allows us not only to include the data itself but also to add type information within the JSON. In this way, there is no need for a separate schema and data; everything is combined into a single entity.

## The Cases of `DynamicValue`

`DynamicValue` is a sealed trait, and each case mirrors one kind of schema: `Record` and `Enumeration` hold case classes and sealed traits, `Primitive` holds a value of a standard type, `Sequence`, `SetValue`, and `Dictionary` hold collections, and further cases hold optionals, `Either` and `Fallback` sides, tuples, and errors. The [Cases](../reference/dynamic-value.md#cases) section of the `DynamicValue` reference lists every case with its fields and the schemas that produce it.

## Schema: Converting to/from DynamicValue

With a `Schema[A]`, we can convert any value of type `A` to a `DynamicValue` and conversely we can convert it back to `A`:

```scala
sealed trait Schema[A] {
  def toDynamic(value: A): DynamicValue

  def fromDynamic(value: DynamicValue): scala.util.Either[String, A]
}
```

The `Schema#toDynamic` operation erases the type information of the value and places it into the value (the dynamic value) itself. The `Schema#fromDynamic` operation does the opposite: it takes the type information from the dynamic value and uses it to reconstruct the original value.

`Schema#fromDynamic` matches a record by its field names and the shape of their values, and ignores the `TypeId` stored in the dynamic value. So if two case classes have the same field names and field types, we can convert a dynamic value of one into a typed value of the other:

```scala mdoc:silent
case class User(name: String, age: Int)

object User {
  implicit val schema: Schema[User] = DeriveSchema.gen[User]
}
```

The dynamic value of a `Person` converts to a `User`:

```scala mdoc
dynamicPerson.toTypedValue[User]
```

The field names must match. A case class that stores the same data under another field name fails with an incompatible shape error:

```scala mdoc
case class Customer(fullName: String, age: Int)

object Customer {
  implicit val schema: Schema[Customer] = DeriveSchema.gen[Customer]
}

dynamicPerson.toTypedValue[Customer].isLeft
```

:::caution
`Schema#fromDynamic` cannot read back the dynamic value of a `NonEmptyChunk`, `NonEmptySet`, `NonEmptyMap`, or `Fallback`: the round trip returns `Left` with a cast error. The same failure affects every operation that goes through `DynamicValue`, such as `Schema#migrate`, `Schema#coerce`, and applying a record patch, when the value contains one of these types. To keep a non-empty collection that round-trips, describe it as a transformed `Chunk`, for example `Schema[Chunk[Int]].transformOrFail(c => NonEmptyChunk.fromChunk(c).toRight("empty chunk"), nec => Right(nec.toChunk))`.
:::

## Manipulating Dynamic Values

When we turn a typed value `A` into a `DynamicValue`, we can manipulate its structure and data dynamically. For example, we can remove a field from a record or make a field optional. This process is called dynamic value migration, and the [Schema Migration](schema-migration.md) page describes it.
