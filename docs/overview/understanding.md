---
id: understanding
title: "Understanding ZIO Schema"
---


## Understanding ZIO Schema

ZIO Schema is a library used in many ZIO projects such as _ZIO Flow_, _ZIO Redis_, _ZIO Web_, _ZIO SQL_ and _ZIO DynamoDB_.
ZIO is all about reification of your types. Reification means transforming something abstract (e.g. side effects, accessing fields, structure)  into something "real" (values).

### Reification: functional effects
In functional effects, we reify by turning side-effects into values.

E.g. you might have a simple statement like
```scala
println("Hello")
println("World")
```
and in ZIO we reify this statement to a value like

```scala
val effect1 = Task(println("Hello"))
val effect2 = Task(println("World"))
```

and then are able to do awesome things like:
```scala
(Task(println("Hello")) zipPar Task(println("World"))).retryN(100)
```

### Reification: Optics
In scala we have product types like this case class of a Person:
```scala
final case class Person(name: String, age: Int)
```
This case class has two fields:
- a field "name" of type `String`
- a field "age" of type `Int`

The Scala language provides special support to access the fields inside case classes using the dot syntax:

```scala
val person = Person("Michelle", 32)
val name = person.name
val age  = person.age
```

However, this is a "special language feature", it's not "real" like the side effects we've seen in the previous example ( `println(..) vs. Task`println(...)))` ).

Because these basic operations are not "real", we're unable to create an operator that we can use to
e.g. combine two fields that are inside a nested structure.

The solution to this kind of problem is called an "Optic". Optics provide a way to access the fields of a case class and nested structures.
There are three main types of optics:
- `Lens`: A lens is a way to access a field of a case class.
- `Prism`: A prism is a way to access a field of a nested structure or a collection.
- `Traversal`: A traversal is a way to access all fields of a case class, nested structures or collections.

Optics allow us to take things which are not a first-class **concept**, and turn that into a first-class **value**,
namely the concept of
- drilling down into a field inside a case class or
- drilling down into a nested structure.

Once we have a value, we can compose these things together to solve hard problems in functional programming, e.g.
- handling nested case class copies,
- iterations down deep inside on elements of a nested structure or collections

For more information on optics, refer to the [ZIO Optics](https://zio.github.io/zio-optics/docs/overview/overview_index) documentation.


### Reification: Schema

So far we've looked at how to
- reify side-effects into values (ZIO)
- how to reify accessing + modifying fields inside case classes or arbitrary structures by turning these operations into values as well (Optics)

ZIO Schema is now about how to describe entire data structures using values.

The "built-in" way in scala on how to describe data structures are `case classes` and `classes`.

E.g. the following data type:
```scala
final case class Person(name: String, age: Int)
```
has the following information:
- name of the structure: "Person"
- fields: "name" and "age"
- type of the fields: String and Int
- type of the structure: Person

ZIO Schema tries to reify the concept of structure for datatypes by turning the above information into values.

Not only for case classes, but also for other types like collections, tuples, enumerations etc.


## Getting started


To get started, first you need to understand that a ZIO Schema is basically built-up from these three
sealed traits:
- `Record[R]`
- `Enum[A]`
- `Sequence[Col, Elem]`
and the case class `Primitive[A]`. Every other type is just a specialisation of one of these (or not relevant to get you started).

We will take a look at them now.

### Basic Building Blocks

#### Schema

The core data type of ZIO Schema is a `Schema[A]` which is **invariant in `A`** by necessity, because a Schema allows you to
derive operations that produce an `A` but also operations that consume an `A` and that imposes limitations on the types of
**transformation operators** and **composition operators** that we can provide based on a `Schema`.

It looks kind of like this (simplified):
```scala
sealed trait Schema[A] {
  self =>
  type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]]

  /**
   * A symbolic operator for [[optional]].
   */
  def ? : Schema[Option[A]] = self.optional

  def makeAccessors(b: AccessorBuilder): Accessors[b.Lens, b.Prism, b.Traversal]
  
  /**
   * Transforms this `Schema[A]` into a `Schema[B]`, by supplying two functions that can transform
   * between `A` and `B`, without possibility of failure.
   */
  def transform[B](f: A => B, g: B => A): Schema[B] =
    Schema.Transform[A, B](self, a => Right(f(a)), b => Right(g(b)))

  def transformOrFail[B](f: A => Either[String, B], g: B => Either[String, A]): Schema[B] =
    Schema.Transform[A, B](self, f, g)
  
  def zip[B](that: Schema[B]): Schema[(A, B)] = Schema.Tuple(self, that)
}
```



#### Records

Your data structures usually are composed from a lot of types. For example, you might have a `User`
type that has a `name` field, an `age` field, an `address` field, and a `friends` field.

```scala
case class User(name: String, age: Int, address: Address, friends: List[User])
```

This is called a "product type" in functional programming.
The equivalent of a product type in ZIO Schema is called a record.

In ZIO Schema such a record would be represented using the `Record[R]` typeclass:

```scala
object Schema {
  sealed trait Record[R] extends Schema[R] {
    def structure: Chunk[Field[_]]
    def annotations: Chunk[Any] = Chunk.empty
    def rawConstruct(values: Chunk[Any]): Either[String, R]
  }
}

```


#### Enumerations
Other times, you might have a type that represents a list of different types. For example, you might
have a type, like this:
```scala
  sealed trait PaymentMethod 

  object PaymentMethod {
    final case class CreditCard(number: String, expirationMonth: Int, expirationYear: Int) extends PaymentMethod
    final case class WireTransfer(accountNumber: String, bankCode: String) extends PaymentMethod
  }
```

In functional programming, this kind of type is called a "sum type".
In Scala2, this is called a "sealed trait".
In Scala3, this is called an "enum".

In ZIO Schema we call these types `enumeration` types and they are
represented using the `Enum[A]` type class.

```scala
object Schema ... {
  sealed trait Enum[A] extends Schema[A] {
    def annotations: Chunk[Any]
    def structure: ListMap[String, Schema[_]]
  }
}

```

#### Sequence

Often you have a type that is a collection of elements. For example, you might have a `List[User]`.
This is called a `Sequence` and is represented using the `Sequence[Col, Elem]` type class:

```scala
object Schema ... {
  ...
 
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
  ...
}

```
#### Optionals
A special variant of a collection type is the `Optional[A]` type:
```scala
object Schema ... {

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

#### Primitives
Last but not least, we have primitive values.
```scala
object Schema ... {
  ...
  final case class Primitive[A](standardType: StandardType[A]) extends Schema[A] {
    type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = Unit

    override def makeAccessors(b: AccessorBuilder): Unit = ()
  }
  ...
}
```
Primitive values are represented using the `Primitive[A]` type class and represent the elements,
that we cannot further define through other means. If you visualize your data structure as a tree,
primitives are the leaves.

ZIO Schema provides a number of built-in primitive types, that you can use to represent your data.
These can be found in the `StandardType` companion-object.

### Transforming Schemas
Once we have a `Schema`, we can transform it into another `Schema` by applying a `Transformer`.
In normal Scala code this would be the equivalent of `map`.

In ZIO Schema this is modelled by the `Transform` type class:

```scala
  final case class Transform[A, B](codec: Schema[A], f: A => Either[String, B], g: B => Either[String, A])
      extends Schema[B] {
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = codec.Accessors[Lens, Prism, Traversal]

    override def makeAccessors(b: AccessorBuilder): codec.Accessors[b.Lens, b.Prism, b.Traversal] =
      codec.makeAccessors(b)

    override def serializable: Schema[Schema[_]] = Meta(SchemaAst.fromSchema(codec))
    override def toString: String                = s"Transform($codec)"
  }
```

In the example above, we can transform the `User` Schema into a `UserRecord` Schema, which is a record,
by using the `transform`-method, which has to be an "isomorphism" (= providing methods to transform A to B _and_ B to A):
```scala
  /**
   * Transforms this `Schema[A]` into a `Schema[B]`, by supplying two functions that can transform
   * between `A` and `B`, without possibility of failure.
   */
  def transform[B](f: A => B, g: B => A): Schema[B] =
    Schema.Transform[A, B](self, a => Right(f(a)), b => Right(g(b)))
```

#### Codecs
Once you have your schema, you can combine it with a codec.
A codec is a combination of a schema and a serializer.
Unlike codecs in other libraries, a codec in ZIO Schema has no type parameter.

```scala

trait Codec {
  def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte]
  def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A]

  def encode[A](schema: Schema[A]): A => Chunk[Byte]
  def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A]
}

```

It basically says:
`encoder[A]`: Given a `Schema[A]` it is capable of generating an `Encoder[A]` ( `A => Chunk[Byte]`) for any Schema.
`decoder[A]`: Given a `Schema[A]` it is capable of generating a `Decoder[A]` ( `Chunk[Byte] => Either[String, A]`) for any Schema.


Example of possible codecs are:
- CSV Codec
- JSON Codec (already available)
- Apache Avro Codec (in progress)
- Apache Thrift Codec (in progress)
- XML Codec
- YAML Codec
- Protobuf Codec (already available)
- QueryString Codec
- etc.
