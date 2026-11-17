---
id: schema-serialization
title: "Serialization of the Schema Itself"
sidebar_label: "Schema Serialization"
---

In distributed systems, we often move computation to the data instead of moving data to the computation. Large data sets are expensive to send over a slow network, so a receiver that already holds a function can be sent the data instead, or the other way around. To do that, both sides need to agree on the structure of the data. ZIO Schema makes the structure of a type a value, so the structure can be sent over the wire like any other data.

## Turning a Schema into Data

A `Schema[A]` contains functions, such as the constructor of a case class, and functions cannot be serialized. The `Schema#ast` method returns a `MetaSchema` instead: a plain tree that describes only the structure of the schema, such as its records, fields, cases, and primitive types. `MetaSchema` has its own schema, `MetaSchema.schema`, so any codec can encode it:

```scala
sealed trait Schema[A] {
  def ast: MetaSchema
}

sealed trait ExtensibleMetaSchema[BuiltIn <: TypeList] {
  def toSchema: Schema[_]
}

object MetaSchema {
  val schema: Schema[MetaSchema]
  def fromSchema[A](schema: Schema[A]): MetaSchema
}
```

`MetaSchema` is an alias for `ExtensibleMetaSchema[DynamicValue :: End]`, and `MetaSchema.fromSchema(schema)` returns the same tree as `schema.ast`.

## Sending a Schema over the Wire

The sender calls `Schema#ast` and encodes the result with a codec for `MetaSchema.schema`. The following example uses the JSON codec:

```scala mdoc:silent
import zio.Chunk
import zio.schema._
import zio.schema.codec.{BinaryCodec, DecodeError, JsonCodec}
import zio.schema.meta.MetaSchema

case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val astCodec: BinaryCodec[MetaSchema] = JsonCodec.schemaBasedBinaryCodec[MetaSchema](MetaSchema.schema)

val sentAst: Chunk[Byte] = astCodec.encode(Person.schema.ast)
```

The receiver decodes the bytes back into a `MetaSchema` with the same codec:

```scala mdoc:silent
val receivedAst: Either[DecodeError, MetaSchema] = astCodec.decode(sentAst)
```

The decoded tree is equal to the tree the sender encoded:

```scala mdoc
receivedAst == Right(Person.schema.ast)
```

## Rebuilding a Schema on the Receiver

`MetaSchema#toSchema` turns the tree back into a schema. The receiver does not have the `Person` class, so the rebuilt schema describes the same structure with generic types: a record becomes a `GenericRecord` whose values are `ListMap[String, _]`, and every sequence becomes a `Chunk`. The result has the type `Schema[_]`, because the Scala type is not known on the receiving side.

The rebuilt schema can read data that the sender encodes with the original schema. The following helper decodes JSON with any schema and returns the value as a `DynamicValue`:

```scala mdoc:silent
def decodeAsDynamic[A](schema: Schema[A], json: Chunk[Byte]): Either[DecodeError, DynamicValue] =
  JsonCodec.schemaBasedBinaryCodec[A](schema).decode(json).map(schema.toDynamic)

val sentPerson: Chunk[Byte] = JsonCodec.schemaBasedBinaryCodec[Person].encode(Person("John", 42))
```

Next, the receiver rebuilds the schema from the tree and decodes the person with it:

```scala mdoc
receivedAst.map(ast => decodeAsDynamic(ast.toSchema, sentPerson)).foreach(println)
```

The same idea works with any other codec, such as Protobuf, because `MetaSchema.schema` is an ordinary schema.

## What the Tree Keeps

A `MetaSchema` records structure only, so some information does not survive the trip:

- A `Transform` is replaced by the schema it wraps, so a newtype such as `Schema[BigDecimal]` arrives as its underlying primitive.
- Annotations, such as `fieldName` or `validate`, are not part of the tree.
- Every sequence, including a `Set` and a non-empty collection, arrives as a `Chunk`, and a `NonEmptyMap` arrives as a plain `Map`.
- Records and enumerations keep their `TypeId`, field names, and case names, and recursive types are kept as references.

## The `Schema#serializable` Method

`Schema#serializable` is meant to return a `Schema[Schema[A]]` that converts a schema to and from data in one step:

```scala
sealed trait Schema[A] {
  def serializable: Schema[Schema[A]]
}
```

:::caution
`Schema#serializable` does not work in the current version. It builds the schema of the *values* of `A` and treats it as the schema of a `Schema`. For a primitive schema such as `Schema[Int]`, calling `toDynamic` on the result returns a meaningless value, and for a case class it throws a `ClassCastException`. Use `Schema#ast` with `MetaSchema.schema`, and `MetaSchema#toSchema` on the receiving side, as shown above.
:::
