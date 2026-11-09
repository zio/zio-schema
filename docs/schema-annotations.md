---
id: schema-annotations
title: "Schema Annotations"
sidebar_label: "Schema Annotations"
---

## Overview

Annotations attach extra information to a case class, a field, or a case of a sealed trait, and codecs read that information when they encode or decode a value. A field annotation changes how one field is named or whether it is written at all. A type annotation changes how the whole type is laid out, for example how a sealed trait marks its case. Annotations live in the `zio.schema.annotation` package, and you can apply them in two ways:

- Directly on the definition, as in `case class User(@fieldName("user_name") username: String)`.
- With `Schema#annotate`, when you cannot edit the source, as in `User.schema.annotate(rejectExtraFields())`.

Each annotation below shows the JSON that the codec produces. The JSON codec is the main consumer of these annotations; other codecs may ignore some of them.

## Renaming Fields and Cases

These annotations change the names that appear in the encoded output. Renaming does not change the Scala names you use in code.

### Field Names

To encode a field under a different name, apply `fieldName` to the field:

```scala mdoc:silent:reset
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.fieldName
import zio.schema.codec.JsonCodec

final case class User(@fieldName("user_name") username: String, age: Int)

object User {
  implicit val schema: Schema[User] = DeriveSchema.gen
}
```

Encoding a `User` writes the field under `user_name`:

```scala mdoc
println(JsonCodec.jsonCodec(User.schema).encodeJson(User("alice", 30), None).toString)
```

To accept more than one incoming name when decoding, apply `fieldNameAliases` with the extra names. Encoding still uses the primary name from `fieldName`, and decoding accepts any listed name:

```scala mdoc:silent:reset
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.{fieldName, fieldNameAliases}
import zio.schema.codec.JsonCodec

final case class Account(@fieldName("email") @fieldNameAliases("mail", "e_mail") address: String)

object Account {
  implicit val schema: Schema[Account] = DeriveSchema.gen
}
```

Decoding a payload that uses the alias `mail` produces the same value as decoding `email`:

```scala mdoc
println(JsonCodec.jsonCodec(Account.schema).decodeJson("""{"mail":"a@example.com"}""").toString)
```

### Case Names

Sealed traits encode each case under its case name. Apply `caseName` to a case to change that name:

```scala mdoc:silent:reset
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.caseName
import zio.schema.codec.JsonCodec

sealed trait Status

object Status {
  @caseName("ACTIVE") case object Enabled extends Status
  case object Disabled extends Status

  implicit val schema: Schema[Status] = DeriveSchema.gen
}
```

The renamed case uses its new name in the JSON output, and the other case keeps its Scala name:

```scala mdoc
println(JsonCodec.jsonCodec(Status.schema).encodeJson(Status.Enabled, None).toString)
println(JsonCodec.jsonCodec(Status.schema).encodeJson(Status.Disabled, None).toString)
```

`caseNameAliases` works like `fieldNameAliases` for cases: it accepts extra names when decoding a case.

## Discriminators

A sealed trait is normally encoded with a discriminator, which is a field that records the case. These annotations change how that field is named or whether it exists.

### Discriminator Name

To store the case under a field with your own name, apply `discriminatorName` to the sealed trait:

```scala mdoc:silent:reset
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.discriminatorName
import zio.schema.codec.JsonCodec

@discriminatorName("kind")
sealed trait Shape

object Shape {
  final case class Circle(radius: Double) extends Shape
  final case class Square(side: Double)   extends Shape

  implicit val schema: Schema[Shape] = DeriveSchema.gen
}
```

The discriminator field is `kind`, and it holds the name of the case:

```scala mdoc
println(JsonCodec.jsonCodec(Shape.schema).encodeJson(Shape.Circle(1.5), None).toString)
```

### No Discriminator

To drop the discriminator field entirely, apply `noDiscriminator` to the sealed trait. The codec then encodes each case as its plain value and picks a case by trying each one when decoding:

```scala mdoc:silent:reset
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.noDiscriminator
import zio.schema.codec.JsonCodec

@noDiscriminator
sealed trait Reading

object Reading {
  final case class Celsius(value: Double)    extends Reading
  final case class Fahrenheit(value: Double) extends Reading

  implicit val schema: Schema[Reading] = DeriveSchema.gen
}
```

Encoding a case writes only its fields, so the output has no case name:

```scala mdoc
println(JsonCodec.jsonCodec(Reading.schema).encodeJson(Reading.Celsius(21.5), None).toString)
```

Because no field records the case, decoding can match a payload to more than one case. Use `noDiscriminator` only when the cases have distinct shapes.

## Controlling What Is Encoded

These annotations decide whether a field or case is written, and what happens when a field is missing.

### Transient Fields and Cases

To skip a field during encoding, apply `transientField`. When decoding, the field takes the default value of its schema, which is an empty string for `String`:

```scala mdoc:silent:reset
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.transientField
import zio.schema.codec.JsonCodec

final case class Session(token: String, @transientField cache: String = "none")

object Session {
  implicit val schema: Schema[Session] = DeriveSchema.gen
}
```

The `cache` field is absent from the output:

```scala mdoc
println(JsonCodec.jsonCodec(Session.schema).encodeJson(Session("abc", "warm"), None).toString)
```

`transientCase` applies the same idea to a whole case of a sealed trait. The codec writes an empty object, `{}`, in place of that case.

### Optional Fields

To let decoding succeed when a field is missing, apply `optionalField`. The codec then fills in the value that the field's schema provides for an absent value, such as `None` for an `Option` or an empty string for a `String`:

```scala mdoc:silent:reset
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.optionalField
import zio.schema.codec.JsonCodec

final case class Page(items: List[String], @optionalField nextPage: String)

object Page {
  implicit val schema: Schema[Page] = DeriveSchema.gen
}
```

Decoding a payload that omits `nextPage` succeeds:

```scala mdoc
println(JsonCodec.jsonCodec(Page.schema).decodeJson("""{"items":["a","b"]}""").toString)
```

### Default Values

`optionalField` uses the value that the field's schema computes. To choose the value yourself, apply `fieldDefaultValue`, which takes the value to use when the field is missing:

```scala mdoc:silent:reset
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.fieldDefaultValue
import zio.schema.codec.JsonCodec

final case class Search(query: String, @fieldDefaultValue("first") page: String)

object Search {
  implicit val schema: Schema[Search] = DeriveSchema.gen
}
```

When the payload omits `page`, the decoded value uses `"first"`:

```scala mdoc
println(JsonCodec.jsonCodec(Search.schema).decodeJson("""{"query":"zio"}""").toString)
```

### Rejecting Extra Fields

By default, decoding ignores fields that the type does not declare. To fail on them instead, apply `rejectExtraFields` to the type:

```scala mdoc:silent:reset
import zio.schema.{DeriveSchema, Schema}
import zio.schema.annotation.rejectExtraFields
import zio.schema.codec.JsonCodec

@rejectExtraFields
final case class Person(name: String, age: Int)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen
}
```

A payload with an undeclared `email` field now fails to decode:

```scala mdoc
println(JsonCodec.jsonCodec(Person.schema).decodeJson("""{"name":"bob","age":4,"email":"x"}""").toString)
```

You can also turn this on for every type at once with `JsonCodec.Configuration(rejectExtraFields = true)`. An annotation on a type overrides the configuration for that type.

## Validation and Descriptions

`Schema#validation` attaches a rule to a schema, and `Schema#validate` checks a value against that rule. The `description` annotation attaches documentation text to a type. Both are covered on [Validating Types](./operations/validating-types.md).
