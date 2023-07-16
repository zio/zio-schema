---
id: dynamic-data-representation
title: "Dynamic Data Representation"
---

DynamicValue is a way to describe the entire universe of possibilities for schema values. It does that in a way that we can interact with and introspect the data with its structure (type information). The structure of the data is baked into the data itself.

Let's create a simple instance of `Person("John Doe", 42)` and convert it to `DynamicValue`:

```scala mdoc:compile-only
import zio.schema._

case class Person(name: String, age: Int)

object Person {
  implicit val schema = DeriveSchema.gen[Person]
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

## Converting to/from DynamicValue

With a `Schema[A]`, we can convert any value of type `A` to a `DynamicValue` and conversely we can convert it back to `A`:

```scala
sealed trait Schema[A] {
  def toDynamic(value: A): DynamicValue

  def fromDynamic(value: DynamicValue): scala.util.Either[String, A]
}
```

The `toDynamic` operation erases the type information of the value and places it into the value (the dynamic value) itself. The `fromDynamic` operation does the opposite: it takes the type information from the dynamic value and uses it to reconstruct the original value.
