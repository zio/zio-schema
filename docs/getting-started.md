---
id: getting-started 
title: "Getting Started"
---

To get started, first we need to understand that a ZIO Schema is basically built-up from these three
sealed traits: `Record[R]`, `Enum[A]` and `Sequence[Col, Elem]`, along with the case class `Primitive[A]`. Every other type is just a specialisation of one of these (or not relevant to get you started).

We will take a look at them now.

## Basic Building Blocks

### Schema

The core data type of ZIO Schema is a `Schema[A]` which is **invariant in `A`** by necessity, because a Schema allows us to derive operations that produce an `A` but also operations that consume an `A` and that imposes limitations on the types of **transformation operators** and **composition operators** that we can provide based on a `Schema`.

It looks kind of like this (simplified):

```scala
sealed trait Schema[A] { self =>
  def zip[B](that: Schema[B]): Schema[(A, B)]

  def transform[B](f: A => B, g: B => A): Schema[B]
}
```

### Records

Our data structures usually are composed of a lot of types. For example, we might have a `User`
type that has a `name` field, an `age` field, an `address` field, and a `friends` field.

```scala
case class User(name: String, age: Int, address: Address, friends: List[User])
```

This is called a **product type** in functional programming. The equivalent of a product type in ZIO Schema is called a record.

In ZIO Schema such a record would be represented using the `Record[R]` typeclass:

```scala
object Schema {
  sealed trait Record[R] extends Schema[R] {
    def fields: Chunk[Field[_]]
    def construct(value: R): Chunk[Any]
    def defaultValue: Either[String, R]
  }
}

```

### Enumerations

Other times, you might have a type that represents a list of different types. For example, we might have a type, like this:

```scala
sealed trait PaymentMethod 

object PaymentMethod {
  final case class CreditCard(number: String, expirationMonth: Int, expirationYear: Int) extends PaymentMethod
  final case class WireTransfer(accountNumber: String, bankCode: String) extends PaymentMethod
}
```

In functional programming, this kind of type is called a **sum type**:
- In Scala 2, this is called a **sealed trait**.
- In Scala3, this is called an **enum**.

In ZIO Schema we call these types `enumeration` types, and they are represented using the `Enum[A]` type class.

```scala
object Schema extends SchemaEquality {
  sealed trait Enum[A] extends Schema[A] {
    def annotations: Chunk[Any]
    def structure: ListMap[String, Schema[_]]
  }
}
```

### Sequence

Often you have a type that is a collection of elements. For example, you might have a `List[User]`. This is called a `Sequence` and is represented using the `Sequence[Col, Elem]` type class:

```scala
object Schema extends SchemaEquality {

  final case class Sequence[Col, Elem](
      elementSchema: Schema[Elem],
      fromChunk: Chunk[Elem] => Col,
      toChunk: Col => Chunk[Elem]
    ) extends Schema[Col] {
    self =>
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = Traversal[Col, Elem]
    override def makeAccessors(b: AccessorBuilder): b.Traversal[Col, Elem] = b.makeTraversal(self, schemaA)
    override def toString: String = s"Sequence($elementSchema)"
  }
}
```

### Optionals

A special variant of a collection type is the `Optional[A]` type:

```scala
object Schema extends SchemaEquality {

  final case class Optional[A](codec: Schema[A]) extends Schema[Option[A]] {
    self =>

    private[schema] val someCodec: Schema[Some[A]] = codec.transform(a => Some(a), _.get)

    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] =
      (Prism[Option[A], Some[A]], Prism[Option[A], None.type])

    val toEnum: Enum2[Some[A], None.type, Option[A]] = Enum2(
      Case[Some[A], Option[A]]("Some", someCodec, _.asInstanceOf[Some[A]], Chunk.empty),
      Case[None.type, Option[A]]("None", singleton(None), _.asInstanceOf[None.type], Chunk.empty),
      Chunk.empty
    )

    override def makeAccessors(b: AccessorBuilder): (b.Prism[Option[A], Some[A]], b.Prism[Option[A], None.type]) =
      b.makePrism(toEnum, toEnum.case1) -> b.makePrism(toEnum, toEnum.case2)
  }

}
```

### Primitives

Last but not least, we have primitive values.

```scala
object Schema extends SchemaEquality {
  final case class Primitive[A](standardType: StandardType[A]) extends Schema[A] {
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = Unit

    override def makeAccessors(b: AccessorBuilder): Unit = ()
  }
}
```

Primitive values are represented using the `Primitive[A]` type class and represent the elements,
that we cannot further define through other means. If we visualize our data structure as a tree, primitives are the leaves.

ZIO Schema provides a number of built-in primitive types, that we can use to represent our data.
These can be found in the `StandardType` companion-object.

