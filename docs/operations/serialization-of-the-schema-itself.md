---
id: schema-serialization
title: "Serialization of the Schema Itself"
sidebar_label: "Schema Serialization"
---

In distributed systems, we often move computation to the data instead of moving data to the computation. Large data sets are expensive to send over a slow network, so a receiver that already holds a function can be sent the data instead, or the other way around. To do that, both sides need to agree on the structure of the data. ZIO Schema makes the structure of a type a value, so the structure can in principle be sent over the wire like any other data.

The `serializable` method returns a schema that describes the schema itself:

```scala
sealed trait Schema[A] {
  def serializable: Schema[Schema[A]]
}
```

A sender would turn a schema into a value with `serializable.toDynamic`, and a receiver would turn it back into a `Schema[A]` with `fromDynamic`. The round trip would let the receiver convert data with the same structure as the sender.

:::caution
At the time of writing, `serializable` fails at runtime for schemas of case classes. Calling `toDynamic` on the result throws a `ClassCastException` from `GenericRecord` to `ListMap`. Do not rely on this round trip until the library fixes the problem.
:::
