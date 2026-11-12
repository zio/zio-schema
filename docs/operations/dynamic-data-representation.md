---
id: dynamic-data-representation
title: "Dynamic Data Representation"
---

DynamicValue is a way to describe the entire universe of possibilities for schema values. It does that in a way that we can interact with and introspect the data with its structure (type information). The structure of the data is baked into the data itself.

We can create a `DynamicValue` from a schema and a value using `DynamicValue.fromSchemaAndValue` (or `Schema#toDynamic`). We can turn it back into a typed value using `DynamicValue#toTypedValue`:

```scala
trait DynamicValue {
  def toTypedValue[A](implicit schema: Schema[A]): Either[String, A] =
}

object DynamicValue {
  def fromSchemaAndValue[A](schema: Schema[A], value: A): DynamicValue
}
```

Let's create a simple instance of `Person("John Doe", 42)` and convert it to `DynamicValue`:

```scala mdoc:compile-only
import zio.schema._

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val person        = Person("John Doe", 42)
val dynamicPerson = DynamicValue.fromSchemaAndValue(Person.schema, person)
// or we can call `toDynamic` on the schema directly:
// val dynamicPerson = Person.schema.toDynamic(person)
println(dynamicPerson)
```

As we can see, the dynamic value of `person` is the mixure of the data and its structure:

```scala
// The output pretty printed manually
Record(
  Nominal(Chunk(dev,zio,quickstart),Chunk(),Person),
  ListMap(
    name -> Primitive(John Doe,string),
    age -> Primitive(42,int)
  )
)
```

This is in contrast to the relational database model, where the data structure is stored in the database schema and the data itself is stored in a separate location.

However, when we switch to document-based database models, such as JSON or XML, we can store both the data and its structure together. The JSON data model serves as a good example of self-describing data, as it allows us not only to include the data itself but also to add type information within the JSON. In this way, there is no need for a separate schema and data; everything is combined into a single entity.

## Schema: Converting to/from DynamicValue

With a `Schema[A]`, we can convert any value of type `A` to a `DynamicValue` and conversely we can convert it back to `A`:

```scala
sealed trait Schema[A] {
  def toDynamic(value: A): DynamicValue

  def fromDynamic(value: DynamicValue): scala.util.Either[String, A]
}
```

The `toDynamic` operation erases the type information of the value and places it into the value (the dynamic value) itself. The `fromDynamic` operation does the opposite: it takes the type information from the dynamic value and uses it to reconstruct the original value.

Please note that, if we have two types `A` and `B` that are isomorphic, we can convert a dynamic value of type `A` to a typed value of type `B` and vice versa:

```scala mdoc:compile-only
import zio.schema._

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen
}

case class User(name: String, age: Int)

object User {
  implicit val schema: Schema[User] = DeriveSchema.gen
}

val johnPerson = Person("John Doe", 42)
val johnUser   = User("John Doe", 42)

val dynamicJohnPerson = Person.schema.toDynamic(johnPerson)
val dynamicJohnUser   = User.schema.toDynamic(johnUser)

println(dynamicJohnPerson)
// Output: Record(Nominal(Chunk(dev,zio,quickstart),Chunk(Main),Person),ListMap(name -> Primitive(John Doe,string), age -> Primitive(42,int)))
println(dynamicJohnUser)
// Output: Record(Nominal(Chunk(dev,zio,quickstart),Chunk(Main),User),ListMap(name -> Primitive(John Doe,string), age -> Primitive(42,int)))

assert(dynamicJohnPerson.toTypedValue[User] == Right(johnUser))
assert(dynamicJohnUser.toTypedValue[Person] == Right(johnPerson))
```

## Rebuilding a typed value from `schema.ast.toSchema`

`Schema#ast` (a `MetaSchema`) is a typeless description of a schema. Calling
`schema.ast.toSchema` materializes that description back into a `Schema`, but
record types become `Schema.GenericRecord`. A generic record can only construct
a `ListMap[String, _]`; it has lost the original case-class constructor, so:

```scala
val user        = Person("John Doe", 42)
val userDynamic = Person.schema.toDynamic(user)

userDynamic.toTypedValue(Person.schema)
// Right(Person("John Doe", 42))

userDynamic.toTypedValue(Person.schema.ast.toSchema)
// Right(ListMap(name -> "John Doe", age -> 42))
```

To recover the original typed value, supply a
[[zio.schema.DynamicValue.ConstructorRegistry ConstructorRegistry]] that maps
each record's `TypeId` back to a schema that still knows how to construct it.
`ConstructorRegistry.fromSchemas` walks a root schema and registers every
reachable non-generic record, so a single root is enough for nested, recursive
and enum cases:

```scala mdoc:compile-only
import zio.schema._

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val user        = Person("John Doe", 42)
val userDynamic = Person.schema.toDynamic(user)
val registry    = DynamicValue.ConstructorRegistry.fromSchemas(Person.schema)

assert(userDynamic.toTypedValue(registry)(Person.schema.ast.toSchema) == Right(user))
```

The existing `toTypedValue` / `toValue` / `toTypedValueOption` methods keep
their previous behaviour when no registry is supplied.

## Manipulating Dynamic Values

When we turn a typed value `A` into a `DynamicValue`, we can manipulate its structure and data dynamically. For example, we can add a new field to a record or change the type of a field. This process is called dynamic value migration, which we will discuss in the [schema migration](schema-migration.md) section.
