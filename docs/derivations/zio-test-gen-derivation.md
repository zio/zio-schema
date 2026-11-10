---
id: zio-test-gen-derivation
title: "Derivation of ZIO Test Generators"
sidebar_label: "ZIO Test Gen Derivation"
---

## Introduction

ZIO Test supports property-based testing via the `Gen` type. `Gen[R, A]` is a random generator of values of type `A`. Such a generator can be used to produce test cases for a property, which can then be checked for validity. The `zio-schema-zio-test` module provides a way to derive a `Gen[R, A]` from a `Schema[A]`, so we do not have to write a generator for each type by hand.

## Installation

To derive a generator from a schema, we add the following dependency to our `build.sbt` file:

```scala
libraryDependencies += "dev.zio" %% "zio-schema-zio-test" % @VERSION@
```

## DeriveGen

The `DeriveGen` object in the `zio.schema` package provides the `gen` operator. It takes a `Schema[A]` implicitly and returns a `Gen[Sized, A]`:

```scala
object DeriveGen {
  def gen[A](implicit schema: Schema[A]): Gen[Sized, A]
}
```

The `Sized` environment lets ZIO Test control the size of generated collections and strings.

## Example

To generate `Person` values, we first derive the schema, then derive the generator from it. We keep both in the companion object:

```scala mdoc:silent
import zio.schema.{DeriveGen, DeriveSchema, Schema}
import zio.test.{Gen, Sized}

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen
  val gen: Gen[Sized, Person]         = DeriveGen.gen
}
```

Next, we write a property test that checks a round trip through the schema's dynamic representation. Every generated `Person` must survive the conversion unchanged:

```scala mdoc:compile-only
import zio.test._

object PersonRoundTripSpec extends ZIOSpecDefault {
  def spec: Spec[Sized, Nothing] =
    test("a generated Person survives a DynamicValue round trip") {
      check(Person.gen) { p =>
        assertTrue(Person.schema.fromDynamic(Person.schema.toDynamic(p)) == Right(p))
      }
    }
}
```

The `check` operator runs the property with many generated values. If any value fails the assertion, ZIO Test reports the smallest failing input it finds.
