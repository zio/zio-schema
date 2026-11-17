---
id: dynamic-data-representation
title: "Dynamic Data Representation"
---

`DynamicValue` is a way to describe the entire universe of possibilities for schema values. It does that in a way that we can interact with and introspect the data with its structure (type information). The structure of the data is baked into the data itself.

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

Let's create a simple instance of `Person("John Doe", 42)` and convert it to `DynamicValue`:

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

`DynamicValue` is a sealed trait, and each case mirrors one kind of schema:

| Case                                    | Holds                                                    |
| --------------------------------------- | -------------------------------------------------------- |
| `Primitive(value, standardType)`        | A value of a standard type, such as a `String` or `Int`  |
| `Record(id, values)`                    | The fields of a record, by name                          |
| `Enumeration(id, (caseName, value))`    | The case of a sealed trait and its value                 |
| `Sequence(values)`                      | The elements of a list, vector, chunk, or other sequence |
| `SetValue(values)`                      | The elements of a set                                    |
| `Dictionary(entries)`                   | The key and value pairs of a map                         |
| `SomeValue(value)`, `NoneValue`         | An optional value                                        |
| `LeftValue(value)`, `RightValue(value)` | A side of an `Either`                                    |
| `BothValue(left, right)`                | Both sides of a `Fallback`                               |
| `Tuple(left, right)`                    | A pair                                                   |
| `Singleton(instance)`                   | A single fixed value                                     |
| `DynamicAst(ast)`                       | A `MetaSchema` that describes a schema                   |
| `Error(message)`                        | A value that could not be produced                       |

## Schema: Converting to/from DynamicValue

With a `Schema[A]`, we can convert any value of type `A` to a `DynamicValue` and conversely we can convert it back to `A`:

```scala
sealed trait Schema[A] {
  def toDynamic(value: A): DynamicValue

  def fromDynamic(value: DynamicValue): scala.util.Either[String, A]
}
```

The `Schema#toDynamic` operation erases the type information of the value and places it into the value (the dynamic value) itself. The `Schema#fromDynamic` operation does the opposite: it takes the type information from the dynamic value and uses it to reconstruct the original value.

`fromDynamic` matches a record by its field names and the shape of their values, and ignores the `TypeId` stored in the dynamic value. So if two case classes have the same field names and field types, we can convert a dynamic value of one into a typed value of the other:

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
`fromDynamic` cannot read back the dynamic value of a `NonEmptyChunk`, `NonEmptySet`, `NonEmptyMap`, or `Fallback`: the round trip returns `Left` with a cast error. The same failure affects every operation that goes through `DynamicValue`, such as `Schema#migrate`, `Schema#coerce`, and applying a record patch, when the value contains one of these types. To keep a non-empty collection that round-trips, describe it as a transformed `Chunk`, for example `Schema[Chunk[Int]].transformOrFail(c => NonEmptyChunk.fromChunk(c).toRight("empty chunk"), nec => Right(nec.toChunk))`.
:::

## Manipulating Dynamic Values

When we turn a typed value `A` into a `DynamicValue`, we can manipulate its structure and data dynamically. For example, we can remove a field from a record or make a field optional. This process is called dynamic value migration, which we will discuss in the [schema migration](schema-migration.md) section.
