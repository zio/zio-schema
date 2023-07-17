---
id: operations
title: "ZIO Schema Operations"
sidebar_label: "Operations"
---

Once we have defined our schemas, we can use them to perform a variety of operations. In this section, we will explore some of the most common operations that we can perform on schemas.



## Diffing and Patching

ZIO Schema provides two methods called `diff` and `patch`:

```scala
sealed trait Schema[A] {
  def diff(thisValue: A, thatValue: A): Patch[A]

  def patch(oldValue: A, diff: Patch[A]): scala.util.Either[String, A]
} 
```

The `diff` method takes two values of the same type `A` and returns a `Patch[A]` value that describes the differences between the two values. conversely, the `patch` method takes a value of type `A` and a `Patch[A]` value and returns a new value of type `A` that is the result of applying the patch to the original value.

Here is a simple example that demonstrate the how to use `diff` and `patch`:

```scala mdoc:compile-only
import zio.schema._

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val oldValue = Person("John", 42)
val newValue = Person("John", 43)

val patch: Patch[Person] =
  Person.schema.diff(oldValue, newValue)

assert(
  Person.schema.patch(oldValue, patch) == Right(newValue)
)
```


## Schema Serialization

In distributed systems, we often need to move computations to data instead of moving data to computations. The data is big and the network is slow, so moving it is expensive and sometimes impossible due to the volume of data. So in distributed systems, we would like to move our functions to the data and apply the data to the functions and gather the results back.

So we need a way to serialize our computations and send them through the network. In ZIO Schema, each schema itself has a schema, so we can treat the structure as pure data! we can serialize our schemas by calling the `serializable` method:

```scala
sealed trait Schema[A] {
  def serializable: Schema[Schema[A]]
}
```

By calling this method, we can get the schema of a schema. So we can serialize it and send it across the wire, and it can be deserialized on the other side. After deserializing it, we have a schema that is isomorphic to the original schema. So all the operations that we can perform on the original type `A`, we can perform on any value that is isomorphic to `A` on the other side.
