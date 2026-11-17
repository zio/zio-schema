---
id: basic-building-blocks
title: "Basic Building Blocks"
---

A ZIO Schema is built up from a few kinds of schemas: records (the sealed trait `Record[R]`), enumerations (the sealed trait `Enum[A]`), collections (case classes such as `Sequence[Col, Elem, I]`, `Map[K, V]`, and `Set[A]`), and primitives (the case class `Primitive[A]`). Most other schemas, such as `Optional` and `Tuple2`, combine these, and the rest are covered at the end of this page. This page introduces each kind; the [Schema Data Types](reference/schema-data-types.md) reference page lists every case class with its fields and constructors.

The core data type of ZIO Schema is a `Schema[A]` which is **invariant in `A`** by necessity, because a Schema allows us to derive operations that produce an `A` but also operations that consume an `A` and that imposes limitations on the types of **transformation operators** and **composition operators** that we can provide based on a `Schema`.

A simplified version of the trait looks like this:

```scala
sealed trait Schema[A] { self =>
  def zip[B](that: Schema[B]): Schema[(A, B)]

  def transform[B](f: A => B, g: B => A)(implicit loc: SourceLocation): Schema[B]
}
```

## Primitives

To describe scalar data type `A`, we use the `Primitive[A]` data type, which wraps a `StandardType`:

```scala
object Schema {
  final case class Primitive[A](
    standardType: StandardType[A],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Schema[A]
}
```

Primitive values are represented using the `Primitive[A]` case class and represent the elements that we cannot further define through other means. If we visualize our data structure as a tree, primitives are the leaves.

For a list of all standard types (and therefore primitive types) with built-in support, please see the [standard type reference](standard-type-reference.md).

Inside `Schema`'s companion object, an implicit method builds a `Schema[A]` for any type `A` that has an implicit `StandardType[A]`:

```scala
object Schema {
   implicit def primitive[A](implicit standardType: StandardType[A]): Schema[A] = ???
}
```

So we can easily create a `Schema` for a primitive type `A` either by calling `Schema.primitive[A]` or by calling `Schema.apply[A]`:

```scala mdoc:compile-only
import zio.schema._

val intSchema1: Schema[Int] = Schema[Int]
val intSchema2: Schema[Int] = Schema.primitive[Int]
```

## Fail

To represent the absence of schema information for a type `A`, we can use the `Schema.fail` constructor, which creates the following schema:

```scala
object Schema {
  final case class Fail[A](
    message: String,
    annotations: Chunk[Any] = Chunk.empty
  ) extends Schema[A]
}
```

The default value of a `Fail` schema is a `Left` with the message.

## Collections

Collection schemas describe types that hold many values of the same type. ZIO Schema has one case class for each family of collections.

### Sequence

Often we have a type that is a collection of elements. For example, we might have a `List[User]`. This is called a `Sequence` and is represented using the `Sequence[Col, Elem, I]` case class:

```scala
object Schema {
  sealed trait Collection[Col, Elem] extends Schema[Col]
  
  final case class Sequence[Col, Elem, I](
      elementSchema: Schema[Elem],
      fromChunk: Chunk[Elem] => Col,
      toChunk: Col => Chunk[Elem],
      override val annotations: Chunk[Any] = Chunk.empty,
      identity: I
    ) extends Collection[Col, Elem]
}
```

The `Sequence` can be anything that can be isomorphic to a list. The `identity` value tells sequences of different collection types apart, for example `"List"` and `"Vector"`.

The following schema describes a `List[Person]` by building a `Sequence` directly:

```scala mdoc:compile-only
import zio._
import zio.schema._
import zio.schema.Schema._
  
case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val personListSchema: Schema[List[Person]] =
  Sequence[List[Person], Person, String](
    elementSchema = Schema[Person],
    fromChunk = _.toList,
    toChunk = i => Chunk.fromIterable(i),
    annotations = Chunk.empty,
    identity = "List"
  )
```

ZIO Schema has `Schema.list[A]`, `Schema.chunk[A]` and `Schema.vector[A]` constructors that create `Schema[List[A]]`, `Schema[Chunk[A]]` and `Schema[Vector[A]]` for us:

```scala mdoc:compile-only
import zio._
import zio.schema._
import zio.schema.Schema._
  
case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  
  implicit val listSchema:   Schema[List[Person]]   = Schema.list[Person]
  implicit val chunkSchema:  Schema[Chunk[Person]]  = Schema.chunk[Person]
  implicit val vectorSchema: Schema[Vector[Person]] = Schema.vector[Person]
}
```

### Map

Likewise, we can have a type that is a map of keys to values. ZIO Schema represents this using the following case class:

```scala
object Schema {
  sealed trait Collection[Col, Elem] extends Schema[Col]

  final case class Map[K, V](
    keySchema: Schema[K],
    valueSchema: Schema[V],
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Collection[scala.collection.immutable.Map[K, V], (K, V)]
}
```

It stores the key and value schemas. Like `Sequence`, instead of using `Map` directly, we can use the `Schema.map[K, V]` constructor:

```scala mdoc:compile-only
import zio.schema._
import zio.schema.Schema._
  
case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  
  implicit val mapSchema: Schema[scala.collection.immutable.Map[String, Person]] = 
    Schema.map[String, Person]
}
```

### Set

The `Set` case class is similar to `Sequence` and `Map`. It is used to represent a schema for a set of elements:

```scala
object Schema {
  sealed trait Collection[Col, Elem] extends Schema[Col]

  final case class Set[A](
    elementSchema: Schema[A],
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Collection[scala.collection.immutable.Set[A], A]
}
```

To create a `Schema` for a `Set[A]`, we can use the above case class directly or use the `Schema.set[A]` constructor:

```scala mdoc:compile-only
import zio.schema._
import zio.schema.Schema._
  
case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  
  implicit val setSchema: Schema[scala.collection.immutable.Set[Person]] = 
    Schema.set[Person]
}
```

## Records

Our data structures usually are composed of a lot of types. For example, we might have a `User` type that has a `name` field, an `age` field, an `address` field, and a `friends` field:

```scala
case class User(name: String, age: Int, address: Address, friends: List[User])
```

This is called a **product type** in functional programming. The equivalent of a product type in ZIO Schema is called a record.

In ZIO Schema such a record would be represented using the `Record[R]` sealed trait:

```scala
object Schema {
  sealed trait Field[R, A] {
    type Field <: Singleton with String
    def name: Field
    def schema: Schema[A]
    def annotations: Chunk[Any]
    def validation: Validation[A]
    def get: R => A
    def set: (R, A) => R
  }

  sealed trait Record[R] extends Schema[R] {
    def id: TypeId
    val fields: Chunk[Field[R, _]]
    def construct(fieldValues: Chunk[Any])(implicit unsafe: Unsafe): scala.util.Either[String, R]
    def deconstruct(value: R)(implicit unsafe: Unsafe): Chunk[Option[Any]]
  }
}
```

A `Field[R, A]` describes a field of type `A` in a record of type `R`: its name, its schema, its annotations, its validation rule, and the functions that read and update it. `construct` builds a record from its field values, and `deconstruct` returns them. Both take an implicit `zio.Unsafe`, because they skip the type checks that the compiler would normally do.

ZIO Schema has specialized record types for case classes, called `CaseClass0[Z]`, `CaseClass1[A, Z]`, `CaseClass2[A1, A2, Z]`, ..., `CaseClass22`. `CaseClass0` describes a case object or a case class with no fields. The `CaseClass1.apply` and `CaseClass2.apply` constructors have these signatures:

```scala
sealed trait CaseClass1[A, Z] extends Record[Z]

object CaseClass1 {
  def apply[A, Z](
    id0: TypeId,
    field0: Field[Z, A],
    defaultConstruct0: A => Z,
    annotations0: Chunk[Any] = Chunk.empty
  ): CaseClass1[A, Z] = ???
}

object CaseClass2 {
  def apply[A1, A2, Z](
    id0: TypeId,
    field01: Field[Z, A1],
    field02: Field[Z, A2],
    construct0: (A1, A2) => Z,
    annotations0: Chunk[Any] = Chunk.empty
  ): CaseClass2[A1, A2, Z] = ???
}
```

They take a `TypeId`, one `Field` per field, and a construct function. The `TypeId` identifies the type, each `Field` stores the name and the schema of a field, and the construct function builds the type from the field values.

The following schema for `Person` builds a `CaseClass2` by hand:

```scala mdoc:compile-only
import zio.schema._

final case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] =
    Schema.CaseClass2[String, Int, Person](
      id0 = TypeId.fromTypeName("Person"),
      field01 = 
        Schema.Field(
          name0 = "name",
          schema0 = Schema[String],
          get0 = _.name,
          set0 = (p, x) => p.copy(name = x)
        ),
      field02 = 
        Schema.Field(
          name0 = "age",
          schema0 = Schema[Int],
          get0 = _.age,
          set0 = (person, age) => person.copy(age = age)
        ),
      construct0 = (name, age) => Person(name, age),
    )
}
```

There is also the `GenericRecord`, which is used for ad-hoc records, whose values are a `ListMap[String, _]` of field names to values, and for case classes with more than 22 fields:

```scala
object Schema {
  sealed case class GenericRecord(
    id: TypeId,
    fieldSet: FieldSet,
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Record[ListMap[String, _]]
}
```

## Enumerations

Other times, a type is one of several alternatives. For example, a payment method is either a credit card or a wire transfer:

```scala mdoc:compile-only
sealed trait PaymentMethod

object PaymentMethod {
  final case class CreditCard(number: String, expirationMonth: Int, expirationYear: Int) extends PaymentMethod
  final case class WireTransfer(accountNumber: String, bankCode: String) extends PaymentMethod
}
```

In functional programming, this kind of type is called a **sum type**. Scala 2 writes it as a **sealed trait**, and Scala 3 also offers an **enum**.

In ZIO Schema we call these types enumeration types, and they are represented using the `Enum[A]` sealed trait:

```scala
object Schema {
  sealed trait Enum[Z] extends Schema[Z]
}
```

It has specialized types `Enum1[A, Z]`, `Enum2[A1, A2, Z]`, ..., `Enum22[A1, A2, ..., A22, Z]` for enumerations with 1, 2, ..., 22 cases. Here is the definition of `Enum1` and `Enum2`:

```scala
object Schema {
  sealed case class Enum1[A, Z](
    id: TypeId,
    case1: Case[Z, A],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z]

  sealed case class Enum2[A1, A2, Z](
    id: TypeId,
    case1: Case[Z, A1],
    case2: Case[Z, A2],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z]

  // Enum3, Enum4, ..., Enum22
}
```

Each `Case[Z, A]` describes one case of the enumeration `Z` whose value has type `A`: its name, its schema, and the functions that build and take apart a value of the case. If the enumeration has more than 22 cases, we can use the `EnumN` case class:

```scala
object Schema {
  sealed case class EnumN[Z, C <: CaseSet.Aux[Z]](
    id: TypeId,
    caseSet: C,
    annotations: Chunk[Any] = Chunk.empty
  ) extends Enum[Z]
}
```

It has a simple constructor called `Schema.enumeration`:

```scala
object Schema {
  def enumeration[A, C <: CaseSet.Aux[A]](
    id: TypeId,
    caseSet: C,
    annotations: Chunk[Any] = Chunk.empty
  ): Schema[A] = ???
}
```

## Optionals

To create a `Schema` for optional values, we can use the `Optional` case class:

```scala
object Schema {
  final case class Optional[A](
    schema: Schema[A],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Schema[Option[A]]
}
```

Using the `Schema.option[A]` constructor makes it easier to do so:

```scala mdoc:compile-only
import zio.schema._

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val option: Schema[Option[Person]] = Schema.option[Person]
```

## Either

An `Either` has its own case class, which holds the schemas of both sides:

```scala
object Schema {
  final case class Either[A, B](
    left: Schema[A],
    right: Schema[B],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Schema[scala.util.Either[A, B]]
}
```

We can use `Schema.either[A, B]` to create a `Schema` for `scala.util.Either[A, B]`:

```scala mdoc:compile-only
import zio.schema._

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val eitherPersonSchema: Schema[scala.util.Either[String, Person]] =
  Schema.either[String, Person]
```

## Tuple

Each schema has a `Schema#zip` operator that allows us to combine two schemas and create a schema for a tuple of the two types:

```scala
sealed trait Schema[A] { self =>
  def zip[B](that: Schema[B]): Schema[(A, B)] =
    Schema.Tuple2(self, that)
}
```

It is implemented using the `Schema.Tuple2` case class:

```scala
object Schema {
  final case class Tuple2[A, B](
    left: Schema[A],
    right: Schema[B],
    annotations: Chunk[Any] = Chunk.empty
  ) extends Schema[(A, B)]
}
```

ZIO Schema also provides implicit schemas for tuples of arity 2, 3, ..., 22:

```scala
object Schema {
  implicit def tuple2[A, B](implicit c1: Schema[A], c2: Schema[B]): Schema[(A, B)] =
    c1.zip(c2)

  implicit def tuple3[A, B, C](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C]): Schema[(A, B, C)] =
    c1.zip(c2).zip(c3).transform({ case ((a, b), c) => (a, b, c) }, { case (a, b, c) => ((a, b), c) })

  // tuple4, tuple5, ..., tuple22
}
```

So we can easily create a `Schema` for a tuple of n elements, just by calling `Schema[(A1, A2, ..., An)]`:

```scala mdoc:compile-only
import zio.schema._

val tuple2: Schema[(String, Int)]          = Schema[(String, Int)]
val tuple3: Schema[(String, Int, Boolean)] = Schema[(String, Int, Boolean)]
// ...
```

## Other Schema Types

A few more schema types complete the picture. The following list describes what each one is for:

- `Transform[A, B, I]` describes a type `B` in terms of a schema for `A` and two conversions. `Schema#transform` and `Schema#transformOrFail` create it, as described in [Transforming Schemas](operations/transforming-schemas.md).
- `Lazy[A]` delays building a schema until it is first used. `Schema.defer` creates it, and it is needed to write a schema for a recursive type by hand.
- `NonEmptySequence[Col, Elem, I]` and `NonEmptyMap[K, V]` describe collections that cannot be empty, such as `NonEmptyChunk`, `NonEmptySet`, and `NonEmptyMap` from ZIO Prelude. `Schema.nonEmptyChunk`, `Schema.nonEmptySet`, and `Schema.nonEmptyMap` create them.
- `Fallback[A, B]` describes a `zio.schema.Fallback[A, B]`, which holds a left value, a right value, or both. `Schema.fallback` creates it.
- `Dynamic` describes a `DynamicValue`, a value whose structure is only known at runtime. `Schema.dynamicValue` is its implicit instance.

The following example writes a schema for a recursive tree by hand. The `children` field refers to the schema being defined, so it is wrapped in `Schema.defer`:

```scala mdoc:compile-only
import zio.schema._

final case class Tree(value: Int, children: List[Tree])

object Tree {
  implicit lazy val schema: Schema[Tree] =
    Schema.CaseClass2[Int, List[Tree], Tree](
      id0 = TypeId.fromTypeName("Tree"),
      field01 = Schema.Field(
        name0 = "value",
        schema0 = Schema[Int],
        get0 = _.value,
        set0 = (tree, value) => tree.copy(value = value)
      ),
      field02 = Schema.Field(
        name0 = "children",
        schema0 = Schema.defer(Schema.list(schema)),
        get0 = _.children,
        set0 = (tree, children) => tree.copy(children = children)
      ),
      construct0 = (value, children) => Tree(value, children)
    )
}
```

`DeriveSchema.gen` handles recursive types on its own, so `Schema.defer` is only needed when you write the schema yourself.
