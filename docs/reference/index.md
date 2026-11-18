---
id: index
title: "Reference"
sidebar_label: "Reference"
description: "Map of the ZIO Schema reference: writing schemas, the Schema API, operations on schemas, and codecs."
---

The reference describes what ZIO Schema provides: every way to write a schema, the types and methods of the `Schema` API, the operations a schema enables, and the codecs built on it. For the ideas behind the library, read [Concepts](../explanation/index.md) first; for step-by-step tasks, see the tutorials and how-to guides. The modules that each part needs are listed in [Installation](../index.md#installation) and [Modules](../index.md#modules).

## Writing Schemas

How to give a type a schema:

- [Manual Schema Construction](../manual-schema-construction.md) builds schemas by hand from the `Schema` constructors.
- [Automatic Schema Derivation](../automatic-schema-derivation.md) derives schemas for case classes and sealed traits with `DeriveSchema.gen`.
- [Schema Annotations](../schema-annotations.md) lists the annotations that rename fields and cases, mark fields as transient or optional, and attach validation rules.

## Schema API

The types and methods of the `Schema` API:

- [Schema Data Types](schema-data-types.md) lists every case of `Schema`, such as records, enumerations, collections, `Lazy`, and `Fallback`, with their signatures.
- [Standard Type Reference](../standard-type-reference.md) lists the primitive types that every schema is built from, and whether each type's default value can be generated on the JVM, Scala.js, and Scala Native.
- [Schema Methods](schema-methods.md) describes the methods on `Schema` and its companion object.
- [Name Formats and Type Identifiers](name-format-and-type-id.md) covers `NameFormat`, which rewrites field and case names, and `TypeId`, which names the type a schema describes.

## Operations

What a schema lets us do with values. [ZIO Schema Operations](../operations/index.md) gives an overview. Each topic page is paired with the reference page that lists its full API:

| Topic                                                                                 | API reference                                          |
| ------------------------------------------------------------------------------------- | ------------------------------------------------------ |
| [Getting The Default Value](../operations/the-default-value.md)                       |                                                        |
| [Transforming Schemas](../operations/transforming-schemas.md)                         |                                                        |
| [Ordering Derivation](../derivations/ordering-derivation.md)                          |                                                        |
| [Validation](../operations/validating-types.md)                                       | [Validation](validation.md)                            |
| [Diffing and Patching](../operations/diffing-and-patching.md)                         | [Patch and Differ](patch-and-differ.md)                |
| [Schema Migration](../operations/schema-migration.md)                                 | [Migration Operations](migration-operations.md)        |
| [Dynamic Data Representation](../operations/dynamic-data-representation.md)           | [DynamicValue](dynamic-value.md)                       |
| [Serialization of the Schema Itself](../operations/serialization-of-the-schema-itself.md) | [MetaSchema](meta-schema.md)                       |

## Codecs

How schemas become encoders and decoders. [Introduction to ZIO Schema Codecs](../derivations/codecs/index.md) compares the formats and shows which annotations each codec supports, and [Codec Core Types](codec-core.md) describes `Encoder`, `Decoder`, `Codec`, `BinaryCodec`, and `DecodeError`, which every codec shares. Each format has its own page:

- [JSON](../derivations/codecs/json.md)
- [Protocol Buffers](../derivations/codecs/protobuf.md)
- [Apache Avro](../derivations/codecs/avro.md)
- [BSON](../derivations/codecs/bson.md)
- [MessagePack](../derivations/codecs/message-pack.md)
- [Apache Thrift](../derivations/codecs/thrift.md)
- [XML](../derivations/codecs/xml.md)
