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

### Primitives

To describe scalar data type `A`, we use the `Primitive[A]` data type which basically is a wrapper around `StandardType`:

```scala
case class Primitive[A](standardType: StandardType[A]) extends Schema[A]
```

Primitive values are represented using the `Primitive[A]` type class and represent the elements, that we cannot further define through other means. If we visualize our data structure as a tree, primitives are the leaves.

ZIO Schema provides a number of built-in primitive types, that we can use to represent our data. These can be found in the [`StandardType`](https://github.com/zio/zio-schema/blob/main/zio-schema/shared/src/main/scala/zio/schema/StandardType.scala) companion-object:

```scala
sealed trait StandardType[A]
object StandardType {
  implicit object UnitType   extends StandardType[Unit]
  implicit object StringType extends StandardType[String]
  implicit object BoolType   extends StandardType[Boolean]
  // ...
}
```

Inside `Schema`'s companion object, we have an implicit conversion from `StandardType[A]` to `Schema[A]`:

```scala
object Schema {
   implicit def primitive[A](implicit standardType: StandardType[A]): Schema[A] = ???
}
```

So we can easily create a `Schema` for a primitive type `A` either by calling `Schema.primitive[A]` or by calling `Schema.apply[A]`:

```scala
val intSchema1: Schema[Int] = Schema[Int]
val intSchema2: Schema[Int] = Schema.primitive[Int] 
```

### Collections

#### Sequence

Often we have a type that is a collection of elements. For example, we might have a `List[User]`. This is called a `Sequence` and is represented using the `Sequence[Col, Elem, I]` type class:

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

The `Sequence` can be anything that can be isomorphic to a list. 

Here is an example schema for list of `Person`s:

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

ZIO Schema has a helper method `Schema.list[A]` that creates a `Schema[List[A]]` for us:

```scala mdoc:compile-only
import zio._
import zio.schema._
import zio.schema.Schema._
  
case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  
  implicit val listSchema: Schema[List[Person]] = Schema.list[Person]
}
```

#### Map

Likewise, we can have a type that is a map of keys to values. ZIO Schema represents this using the following type class:

```scala
object Schema {
  sealed trait Collection[Col, Elem] extends Schema[Col]

  case class Map[K, V](
    keySchema: Schema[K],
    valueSchema: Schema[V],
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Collection[scala.collection.immutable.Map[K, V], (K, V)]
}
```

It stores the key and value schemas. Like `Sequence`, instead of using `Map` directly, we can use the helper method `Schema.map[K, V]`:

```scala mdoc:compile-only
import zio._
import zio.schema._
import zio.schema.Schema._
  
case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  
  implicit val mapSchema: Schema[Map[String, Person]] = Schema.map[String, Person]
}
```

#### Set

The `Set` type class is similar to `Sequence` and `Map`. It is used to represent a schema for a set of elements:

```scala
object Schema {
  sealed trait Collection[Col, Elem] extends Schema[Col]

  case class Set[A](
    elementSchema: Schema[A],
    override val annotations: Chunk[Any] = Chunk.empty
  ) extends Collection[scala.collection.immutable.Set[A], A]
}
```

To create a `Schema` for a `Set[A]`, we can use the above type class directly or use the helper method `Schema.set[A]`:

```scala mdoc:compile-only
import zio._
import zio.schema._
import zio.schema.Schema._
  
case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  
  implicit val setSchema: Schema[Set[Person]] = Schema.set[Person]
}
```

### Records

Our data structures usually are composed of a lot of types. For example, we might have a `User` type that has a `name` field, an `age` field, an `address` field, and a `friends` field.

```scala
case class User(name: String, age: Int, address: Address, friends: List[User])
```

This is called a **product type** in functional programming. The equivalent of a product type in ZIO Schema is called a record.

In ZIO Schema such a record would be represented using the `Record[R]` typeclass:

```scala
object Schema {
  sealed trait Record[R] extends Schema[R] {
    def id: TypeId
    def fields: Chunk[Field[_]]
    def construct(fieldValues: Chunk[Any]): Either[String, R]
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
object Schema {
  sealed trait Enum[A] extends Schema[A] {
    def annotations: Chunk[Any]
    def structure: ListMap[String, Schema[_]]
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


