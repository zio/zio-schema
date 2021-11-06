---
id: overview_index
title: "Contents"
---

## Installation

Add in your `build.sbt`:

```scala
libraryDependencies += "dev.zio" %% "zio-schema" % "<version>"
```

## Purpose of ZIO-Schema
ZIO-Schema allows you to create representations of your data types as values. 

Once you have a representation of your data types, you can use it to 
 - serialize and deserialize your types
 - validate your types
 - transform your types
 - create instances of your types

You can then use one of the various codecs (or create your own) to serialize and deserialize your types.

Example of codecs are:
 - CSV Codec
 - Apache Avro Codec
 - JSON Codec
 - XML Codec
 - YAML Codec
 - Protobuf Codec
 - QueryString Codec
 - etc.

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
   1. convert from protobuf to object, e.g. `PersonDTO`,
   2. transform to another representation, e.g. `Person`,
   3. validate
   4. transform to JSON `JsonObject`
   5. serialize to `String`



