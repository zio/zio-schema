---
id: operations
title: "ZIO Schema Operations"
sidebar_label: "Operations"
---

Once we have defined our schemas, we can use them to perform a variety of operations. In this section, we will explore some of the most common operations that we can perform on schemas.

## Getting the Default Value of a Schema

ZIO Schema provides a method called `defaultValue` that returns the default value of the underlying type described by the schema. This method returns a `scala.util.Either[String, A]` value, where `A` is the type described by the schema. If the schema does not have a default value, the method returns a `Left` value containing an error message. Otherwise, it returns a `Right` value containing the default value:

```scala
sealed trait Schema[A] {
  def defaultValue: scala.util.Either[String, A]
}
```

ZIO Schema have out of the box default values for all standard types, such as `String`, `Int`, `Boolean`, ..., `LocalDateTime` and `UUID`. For example, the default value of a schema for `String` is the empty string, and the default value of a schema for `Int` is `0`.

## Generate Orderings for Schemas

Standard Scala library provides a type class called `Ordering[A]` that allows us to compare values of type `A`. ZIO Schema provides a method called `ordering` that generates an `Ordering[A]` instance for the underlying type described by the schema:

```scala
sealed trait Schema[A] {
  def ordering: Ordering[A]
}
```

Here is an example, where it helps us to sort the list of `Person`:

```scala mdoc:compile-only
import zio.schema._

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val sortedList: Seq[Person] =
  List(
    Person("John", 42),
    Person("Jane", 34)
  ).sorted(Person.schema.ordering)
```

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

## Automatic Migrations

With ZIO Schema, we can automatically migrate data from one version of a schema to another. As software evolves, we often need to add, change or remove old fields. ZIO Schema provides two methods called `migrate` and `coerce` which help migrate the old schema to the new one:

```scala
selaed trait Schema[A] {
  def migrate[B](newSchema: Schema[B]): Either[String, A => scala.util.Either[String, B]]

  def coerce[B](newSchema: Schema[B]): Either[String, Schema[B]] =
    for {
      f <- self.migrate(newSchema)
      g <- newSchema.migrate(self)
    } yield self.transformOrFail(f, g)
}
```

The `migrate` method takes a new schema and returns a function that can migrate values of the old schema to values of the new schema as a `Right` value of `Either`. If the schemas have unambiguous transformations or are incompatible, the method returns a `Left` value containing an error message.
