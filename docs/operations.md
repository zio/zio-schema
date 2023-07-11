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