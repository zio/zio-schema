---
id: use-cases
title: "ZIO Schema Use cases"
sidebar_label: "Use cases"
---

ZIO Schema allows us to create representations of our data types as values.

Once we have a representation of our data types, we can use it to:

- Serialize and deserialize our types
- Validate our types
- Transform our types
- Create instances of our types

We can then use one of the available codecs (or [write our own](how-to/custom-codec.md)) to serialize and deserialize our types. ZIO Schema ships these codecs, each in its own module:

- JSON (`zio-schema-json`)
- Protocol Buffers (`zio-schema-protobuf`)
- Apache Avro (`zio-schema-avro`)
- Apache Thrift (`zio-schema-thrift`)
- BSON (`zio-schema-bson`)
- MessagePack (`zio-schema-msg-pack`)
- XML (`zio-schema-xml`)

The [Installation](index.md#installation) section lists the platforms each module supports, and [Codecs](derivations/codecs/index.md) describes each codec. ZIO Schema has no CSV, YAML, or query-string codec; a custom codec can add one.

Example use cases that are possible:

- Serializing and deserializing JSON
- Serializing and deserializing XML
- Validating JSON
- Validating XML
- Transforming JSON
- Transforming XML
- Transforming JSON to XML
- Transforming XML to JSON
- Creating diffs from arbitrary data structures
- Creating migrations / evolutions e.g. of Events used in Event-Sourcing
- Transformation pipelines, e.g.
    1. Convert from protobuf to object, e.g. `PersonDTO`,
    2. Transform to another representation, e.g. `Person`,
    3. Validate
    4. Encode as JSON
    5. Send the JSON text over the network
