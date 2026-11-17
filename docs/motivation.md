---
id: motivation
title: "The Motivation Behind ZIO Schema"
sidebar_label: "Motivation"
---

ZIO Schema is a library used in many ZIO projects such as _ZIO Flow_, _ZIO Redis_, _ZIO SQL_ and _ZIO DynamoDB_. It is all about reification of our types. Reification means transforming something abstract (e.g. side effects, accessing fields, structure)  into something "real" (values).

## Reification: Functional Effects

In functional effects, we reify by turning side-effects into values. For example, we might have a simple statement like this:

```scala mdoc:compile-only
println("Hello")
println("World")
```

In ZIO, we reify these statements into values:

```scala mdoc:compile-only
import zio._

val effect1 = ZIO.attempt(println("Hello"))
val effect2 = ZIO.attempt(println("World"))
```

Because the effects are values, we can combine them with operators, for example to run both in parallel and retry them on failure:

```scala mdoc:compile-only
import zio._

val both = (ZIO.attempt(println("Hello")) zipPar ZIO.attempt(println("World"))).retryN(100)
```

## Reification: Optics

In Scala, we have product types like this case class of a `Person`:

```scala mdoc:silent
final case class Person(name: String, age: Int)
```

This case class has two fields:

- A field `name` of type `String`
- A field `age` of type `Int`

The Scala language provides special support to access the fields inside case classes using the dot syntax:

```scala mdoc:silent
val person = Person("Michelle", 32)
val name = person.name
val age  = person.age
```

However, this is a "special language feature", it's not "real" like the side effects we've seen in the previous example (`println(...)` vs. `ZIO.attempt(println(...))`).

Because these basic operations are not "real," we are unable to create an operator that we can use, for example, we cannot combine two fields that are inside a nested structure.

The solution to this kind of problem is called an "Optic". Optics provide a way to access the fields of a case class and nested structures. There are three main types of optics:
- `Lens`: A lens focuses on one field of a product type, such as a case class.
- `Prism`: A prism focuses on one case of a sum type, such as one subtype of a sealed trait.
- `Traversal`: A traversal focuses on all the elements of a collection.

Optics allow us to take things which are not a first-class **concept**, and turn that into a first-class **value**, namely the concept of
- drilling down into a field inside a case class or
- drilling down into a nested structure.

Once we have a value, we can compose these things together to solve hard problems in functional programming, e.g.
- handling nested case class copies,
- iterations down deep inside on elements of a nested structure or collections

For more information on optics, refer to the [ZIO Optics](https://zio.dev/zio-optics/) documentation. ZIO Schema can derive these optics from a schema, as [Optics Derivation](derivations/optics-derivation.md) shows.


## Reification: Schema

So far we've looked at how to
- reify side-effects into values (ZIO)
- how to reify accessing and modifying fields inside case classes or arbitrary structures by turning these operations into values as well (Optics)

**ZIO Schema** is now about how to **describe entire data structures using values**.

The "built-in" way in scala on how to describe data structures are `case classes` and `classes`.

For example, assume we have the `Person` data type, like this:

```scala mdoc:compile-only
final case class Person(name: String, age: Int)
```

It has the following information:

- Name of the structure: `Person`
- Fields: `name` and `age`
- Type of the fields: `String` and `Int`
- Type of the structure: `Person`

ZIO Schema tries to reify the concept of structure for datatypes by turning the above information into values.

Not only for case classes, but also for other types like collections, tuples, enumerations etc.

