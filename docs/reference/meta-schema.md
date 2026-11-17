---
id: meta-schema
title: "MetaSchema"
description: "Reference for MetaSchema and ExtensibleMetaSchema, the serializable description of a schema's structure, and for NodePath."
keywords:
  - "ZIO Schema"
  - "Schema Serialization"
  - "NodePath"
  - "MetaSchema"
---

`MetaSchema` is a **schema described as plain data**: a tree of case classes that records the structure of a `Schema` (its records, fields, enum cases, collections, and primitive types) without any Scala functions. A `Schema[A]` holds constructors, getters, and transformation functions, so it cannot be sent over the network or stored in a database. Its `MetaSchema` can, and the receiving side rebuilds a working schema with the same structure from it. The schema migration machinery also compares two `MetaSchema` values to compute the steps between schema versions.

`MetaSchema` is a type alias for `ExtensibleMetaSchema[DynamicValue :: End]`. The `BuiltIn` type parameter of `ExtensibleMetaSchema` is a type-level list of types whose schemas both sides already know; `MetaSchema` uses a list with only `DynamicValue` in it. Every node records its position in the tree as a `NodePath` and whether it is optional:

```scala
package object meta {
  type MetaSchema = ExtensibleMetaSchema[DynamicValue :: End]
  type NodePath   = NodePath.Type
}

sealed trait ExtensibleMetaSchema[BuiltIn <: TypeList] {
  def builtInInstances: SchemaInstances[BuiltIn]
  def path: NodePath
  def optional: Boolean
  def toSchema: Schema[_]
}

object ExtensibleMetaSchema {
  // Every case also takes (implicit builtInInstances: SchemaInstances[BuiltIn])
  final case class Value[BuiltIn <: TypeList](valueType: StandardType[_], path: NodePath = NodePath.root, optional: Boolean = false)
  final case class Product[BuiltIn <: TypeList](id: TypeId, path: NodePath, fields: Chunk[Labelled[BuiltIn]] = Chunk.empty, optional: Boolean = false)
  final case class Sum[BuiltIn <: TypeList](id: TypeId, path: NodePath, cases: Chunk[Labelled[BuiltIn]] = Chunk.empty, optional: Boolean = false)
  final case class Tuple[BuiltIn <: TypeList](path: NodePath, left: ExtensibleMetaSchema[BuiltIn], right: ExtensibleMetaSchema[BuiltIn], optional: Boolean = false)
  final case class Either[BuiltIn <: TypeList](path: NodePath, left: ExtensibleMetaSchema[BuiltIn], right: ExtensibleMetaSchema[BuiltIn], optional: Boolean = false)
  final case class Fallback[BuiltIn <: TypeList](path: NodePath, left: ExtensibleMetaSchema[BuiltIn], right: ExtensibleMetaSchema[BuiltIn], optional: Boolean = false)
  final case class ListNode[BuiltIn <: TypeList](item: ExtensibleMetaSchema[BuiltIn], path: NodePath, optional: Boolean = false)
  final case class Dictionary[BuiltIn <: TypeList](keys: ExtensibleMetaSchema[BuiltIn], values: ExtensibleMetaSchema[BuiltIn], path: NodePath, optional: Boolean = false)
  final case class FailNode[BuiltIn <: TypeList](message: String, path: NodePath, optional: Boolean = false)
  final case class Ref[BuiltIn <: TypeList](refPath: NodePath, path: NodePath, optional: Boolean = false)
  final case class Known[BuiltIn <: TypeList](typeId: TypeId, path: NodePath, optional: Boolean = false)

  final case class Labelled[BuiltIn <: TypeList](label: String, schema: ExtensibleMetaSchema[BuiltIn])
}
```

## Usage

A sender turns a schema into a `MetaSchema` with `Schema#ast` and encodes it with `MetaSchema.schema`, the schema of `MetaSchema` itself. A receiver decodes the same bytes back into an equal `MetaSchema`:

```scala mdoc:reset
import zio.Chunk
import zio.schema._
import zio.schema.codec.JsonCodec
import zio.schema.meta.MetaSchema

final case class Person(name: String, age: Int, email: Option[String])

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}

val personAst: MetaSchema = Person.schema.ast

val wire: String = JsonCodec.jsonCodec(MetaSchema.schema).encodeJson(personAst, None).toString

JsonCodec.jsonCodec(MetaSchema.schema).decodeJson(wire) == Right(personAst)
```

The receiver then calls `ExtensibleMetaSchema#toSchema` to get a working schema back, as shown in [Rebuilding a Schema](#rebuilding-a-schema).

## Creating Values

A `MetaSchema` usually comes from an existing schema, but its cases are public case classes that can also be built directly.

### From a Schema

`Schema#ast` and `MetaSchema.fromSchema` both convert a schema into its `MetaSchema`. `ExtensibleMetaSchema.fromSchema` does the same for a custom list of built-in types, described in [Extending the Built-In Types](#extending-the-built-in-types):

```scala
sealed trait Schema[A] {
  def ast: MetaSchema
}

object MetaSchema {
  def fromSchema[A](schema: Schema[A]): MetaSchema
}

object ExtensibleMetaSchema {
  def fromSchema[A, BuiltIn <: TypeList](schema: Schema[A])(implicit builtInInstances: SchemaInstances[BuiltIn]): ExtensibleMetaSchema[BuiltIn]
}
```

`Schema#ast` is shorthand for `MetaSchema.fromSchema`, so the two give equal results:

```scala mdoc
MetaSchema.fromSchema(Person.schema) == Person.schema.ast
```

A recursive schema does not loop forever. When the conversion meets a record or enum it is already inside, it writes a `Ref` node that points to the path of the earlier node instead of expanding it again.

### By Hand

Each case takes an implicit `SchemaInstances[BuiltIn]` in a second parameter list. For `MetaSchema`, the compiler derives it from the implicit `Schema[DynamicValue]`, so building a node only needs the `TypeList` syntax in scope:

```scala mdoc
import zio.constraintless.TypeList._
import zio.schema.meta.ExtensibleMetaSchema

val intNode: MetaSchema = ExtensibleMetaSchema.Value[DynamicValue :: End](StandardType.IntType)

MetaSchema.fromSchema(Schema[Int]) == intNode
```

Equality of nodes compares the fields of the first parameter list only. `ExtensibleMetaSchema.equals` also provides a ZIO Prelude `Equal[MetaSchema]` instance.

## Core Operations

Once we have a `MetaSchema`, we can inspect its nodes, serialize it, and turn it back into a schema.

### Inspecting the Tree

`ExtensibleMetaSchema#toString` renders the tree in a compact text form. A top-level record shows as `product` and a nested one as `record`, enums show as `enum`, sequences as `list`, maps as `map`, and a `?` marks an optional node:

```scala mdoc
println(personAst)
```

Every node also exposes `ExtensibleMetaSchema#path`, its position from the root, and `ExtensibleMetaSchema#optional`, which is `true` when the schema wrapped the node in `Option`. A `Labelled` pairs a field or case name with the node for it. The fields of a record and the paths of their nodes are read like this:

```scala mdoc
personAst match {
  case ExtensibleMetaSchema.Product(_, _, fields, _) =>
    fields.map(field => (field.label, field.schema.path.render, field.schema.optional))
  case _                                             => Chunk.empty
}
```

### Serializing

`MetaSchema.schema` is the `Schema[MetaSchema]` that every codec uses to encode and decode a `MetaSchema`. `ExtensibleMetaSchema.schema` builds the same schema for any list of built-in types, and `MetaSchema.Labelled` is the `Labelled` type for `MetaSchema`:

```scala
object MetaSchema {
  type Labelled = ExtensibleMetaSchema.Labelled[DynamicValue :: End]

  val schema: Schema[MetaSchema]
}

object ExtensibleMetaSchema {
  implicit def schema[BuiltIn <: TypeList](implicit builtInInstances: SchemaInstances[BuiltIn]): Schema[ExtensibleMetaSchema[BuiltIn]]
}
```

The schema is also available implicitly, so `Schema[MetaSchema]` finds it. The implicit `Schema[Schema[A]]` that the `Schema` companion provides is built on it: it encodes a schema through its `MetaSchema` and decodes by calling `toSchema`. The decoded value is typed `Schema[A]`, but it has the rebuilt structure described below, not the original schema:

```scala
object Schema {
  implicit def schemaSchema[A]: Schema[Schema[A]]
}
```

Any codec works with `MetaSchema.schema`. The Usage example above encodes to JSON, and this is the JSON it writes for `Person`:

```scala mdoc
println(wire)
```

:::caution[Do not use Schema#serializable]
`Schema#serializable` does not produce a usable schema of schemas. For `Schema[Int]`, it encodes a meaningless value, and for a case class, encoding throws a `ClassCastException`. Use `MetaSchema.schema` (or the implicit `Schema[Schema[A]]`) together with `Schema#ast` and `ExtensibleMetaSchema#toSchema` instead. The [Schema Serialization](../operations/serialization-of-the-schema-itself.md) page explains why a schema is worth serializing.
:::

### Rebuilding a Schema

`ExtensibleMetaSchema#toSchema` turns a `MetaSchema` back into a `Schema`. The type of the original values is not part of the description, so the result is a `Schema[_]`:

```scala
sealed trait ExtensibleMetaSchema[BuiltIn <: TypeList] {
  def toSchema: Schema[_]
}
```

The rebuilt schema has the same structure as the original, but it works on generic values: a record becomes a `GenericRecord` over `ListMap[String, _]`. The following example decodes a `Person` value, in its dynamic form, with the rebuilt schema:

```scala mdoc:silent
val rebuilt: Schema[_] = personAst.toSchema

val asListMap: Either[String, Any] =
  rebuilt.fromDynamic(Person.schema.toDynamic(Person("Alice", 42, None)))
```

The result holds the same data as a `ListMap` keyed by field name:

```scala mdoc
asListMap
```

Each node becomes one schema:

| Node         | Rebuilt schema                                                                               |
| ------------ | -------------------------------------------------------------------------------------------- |
| `Value`      | `Schema.Primitive` of the same `StandardType`                                                |
| `Product`    | `Schema.GenericRecord` with the same `TypeId` and field names, over `ListMap[String, _]`     |
| `Sum`        | An enum schema with the same `TypeId` and case names; its cases use identity constructors    |
| `Tuple`      | `Schema.Tuple2`                                                                              |
| `Either`     | `Schema.Either`                                                                              |
| `Fallback`   | `Schema.Fallback`                                                                            |
| `ListNode`   | `Schema[Chunk[_]]`                                                                           |
| `Dictionary` | `Schema.Map`                                                                                 |
| `FailNode`   | `Schema.Fail` with the same message                                                          |
| `Ref`        | A lazy schema (`Schema.defer`) that points to the rebuilt node at `refPath`                  |
| `Known`      | The built-in schema with the same `TypeId`, or `Schema.Fail` when no built-in schema matches |

When `ExtensibleMetaSchema#optional` is `true` for a node, `ExtensibleMetaSchema#toSchema` wraps the rebuilt schema in `Schema#optional`.

## What the Round Trip Loses

A `MetaSchema` describes only structure. Converting a schema with `Schema#ast` and rebuilding it with `ExtensibleMetaSchema#toSchema` keeps the shape of the data, while everything that is code or metadata is gone:

| Part of the original schema                                             | What remains after the round trip                                                           |
| ----------------------------------------------------------------------- | ------------------------------------------------------------------------------------------- |
| `Schema#transform`, `Schema#transformOrFail`, and schemas built on them | Only the underlying schema; the functions and the target type are gone                      |
| Annotations on schemas, fields, and cases                               | None, so `@fieldName`, `@caseName`, `@transientField`, default values, and similar are lost |
| Field validations and `@validate`                                       | None                                                                                        |
| Case class types                                                        | A `GenericRecord` over `ListMap[String, _]`                                                 |
| Sealed trait cases                                                      | Enum cases whose values are generic records                                                 |
| `Set`, `NonEmptyChunk`, `NonEmptySet`                                   | A `Chunk`                                                                                   |
| `NonEmptyMap`                                                           | A `Map`                                                                                     |
| `Fallback` below the top level                                          | An `Either`                                                                                 |
| `Schema.Dynamic` with annotations                                       | The full structure of `DynamicValue.schema` instead of `Schema.Dynamic`                     |

A field that the original schema built with `Schema#transform` therefore comes back as its underlying type. For example, a schema for an `Email` wrapper rebuilds as a plain string schema:

```scala mdoc
final case class Email(value: String)

val emailSchema: Schema[Email] = Schema[String].transform(Email(_), _.value)

println(emailSchema.ast)
emailSchema.ast.toSchema
```

Annotations disappear in the same way, so codec settings that depend on them do not travel with the `MetaSchema`:

```scala mdoc
val annotated: Schema[Int] = Schema[Int].annotate("a note")

annotated.annotations
annotated.ast.toSchema.annotations
```

A `Fallback` at the top level of a schema keeps its own `Fallback` node, but `MetaSchema.schema` has no case for that node, so serialize such a schema inside a record or another container.

## NodePath

A `NodePath` is the path from the root of a schema to one of its nodes, as a sequence of labels. Record fields and enum cases use their names as labels, and the other nodes use fixed labels: `item` for the element of a sequence, `keys` and `values` for a map, and `left` and `right` for tuples and `Either`. `NodePath` is a ZIO Prelude subtype of `Chunk[String]`, so it supports every `Chunk` operation, and [Migration Operations](migration-operations.md) uses it to address the node each step changes.

The `NodePath` companion provides the root path and the operations for building and comparing paths:

```scala
object NodePath {
  val root: NodePath
  val empty: NodePath

  implicit class NodePathSyntax(self: NodePath) {
    def /(label: String): NodePath
    def /(subpath: NodePath): NodePath
    def relativeTo(path: NodePath): NodePath
    def isSubpathOf(path: NodePath): Boolean
    def partitionLeaf: (NodePath, Option[String])
    def render: String
  }
}
```

`NodePath.root` and `NodePath.empty` are both the empty path. `NodePathSyntax#/` appends a label or a whole path. `NodePathSyntax#relativeTo` removes a prefix, and returns the path unchanged when the prefix does not match. `NodePathSyntax#isSubpathOf` checks for a prefix, `NodePathSyntax#partitionLeaf` splits off the last label, and `NodePathSyntax#render` joins the labels with `/`, rendering the empty path as `ROOT`:

```scala mdoc
import zio.schema.meta.NodePath

val address = NodePath.root / "address"
val street  = address / "street"

street.render
street.relativeTo(address)
street.isSubpathOf(address)
street.partitionLeaf
address / (NodePath.root / "city" / "zip")
NodePath.root.render
```

`ExtensibleMetaSchema.nodePathSchema` is the implicit `Schema[NodePath]`, which encodes a path as a sequence of strings.

## Extending the Built-In Types

Sometimes both sides of a connection already share some schemas, for example a common library of domain types. Listing those types in the `BuiltIn` parameter of `ExtensibleMetaSchema` keeps them out of the serialized description: `ExtensibleMetaSchema.fromSchema` writes a `Known` node holding only the type's `TypeId`, and `ExtensibleMetaSchema#toSchema` returns the shared schema instance itself, with its transforms and annotations intact.

The type list is built from `zio.constraintless.TypeList`, and the compiler needs an implicit `Schema` for each type in it to derive the `SchemaInstances[BuiltIn]`:

```scala
trait SchemaInstances[As <: TypeList] {
  def withInstance[B, D](use: Schema[B] => D)(implicit ev: B IsElementOf As): D
  def all: List[Schema[_]]
}
```

The following example adds a `Pet` enum to the built-in list:

```scala mdoc:silent:reset
import zio.constraintless.TypeList._
import zio.schema._
import zio.schema.meta.ExtensibleMetaSchema

sealed trait Pet

object Pet {
  case object Rock                  extends Pet
  final case class Dog(name: String) extends Pet

  implicit lazy val schema: Schema[Pet] = DeriveSchema.gen[Pet]
}

type PetMetaSchema = ExtensibleMetaSchema[DynamicValue :: Pet :: End]

val petMetaSchemaSchema: Schema[PetMetaSchema] = ExtensibleMetaSchema.schema[DynamicValue :: Pet :: End]

val petMeta: PetMetaSchema = ExtensibleMetaSchema.fromSchema[Pet, DynamicValue :: Pet :: End](Pet.schema)
```

The schema of `Pet` becomes a single `Known` node, and rebuilding it returns the very same schema instance:

```scala mdoc
petMeta.isInstanceOf[ExtensibleMetaSchema.Known[_]]
petMeta.toSchema eq Pet.schema
```

Both sides must use the same type list, and the receiver decodes with `petMetaSchemaSchema` rather than `MetaSchema.schema`. Only record, enum, and `Schema.Dynamic` instances in the list become `Known` nodes, and the lookup compares schemas with `==`, so pass the same schema instance that the list holds.

## Integration

`MetaSchema` connects the structure of a schema to the rest of ZIO Schema:

- [DynamicValue](dynamic-value.md) is the data counterpart of `MetaSchema`, and its `DynamicAst` case carries a `MetaSchema` inside a value.
- [Schema Migration](../operations/schema-migration.md) shows how `Migration.derive` compares two `MetaSchema` values to compute migration steps.
- [Migration Operations](migration-operations.md) lists the steps, several of which take a `NodePath` or a `MetaSchema`.
- [Schema Serialization](../operations/serialization-of-the-schema-itself.md) describes the use case for sending schemas over the wire.
