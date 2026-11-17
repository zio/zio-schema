---
id: validation
title: "Validation"
---

Validation rules describe which values of a type are acceptable, such as a non-empty name or an age between 0 and 120. ZIO Schema stores these rules inside the schema, so application code can check any value of the type in the same way. The built-in codecs do not check the rules when they decode, so call `Schema#validate` yourself after decoding untrusted input. This page explains how rules are attached and checked; the [Validation](../reference/validation.md) reference page lists every rule constructor, combinator, predefined validator, and `ValidationError` case.

## Attaching Rules to Fields

The simplest way to add a rule is the `@validate` annotation on a case class field. The rule becomes part of the derived schema:

```scala mdoc:silent
import zio.schema._
import zio.schema.annotation.validate
import zio.schema.validation.Validation

final case class Person(
    @validate(Validation.minLength(1)) name: String,
    @validate(Validation.between(0, 120)) age: Int
)

object Person {
  implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
}
```

The `Validation.minLength(1)` rule rejects an empty name, and the `Validation.between(0, 120)` rule keeps the age in range. Both bounds are inclusive.

## Validating a Value

`Schema#validate` checks a value and returns the failed checks as a `Chunk[ValidationError]`. A valid value gives an empty chunk:

```scala mdoc
Person.schema.validate(Person("John", 42)).isEmpty
```

An invalid value gives one entry per failed check. The following person breaks both rules:

```scala mdoc
val problems = Person.schema.validate(Person("", 130))

problems.size
```

To print the individual failures, we iterate over the result:

```scala mdoc
problems.foreach(problem => println(problem))
```

Each printed entry names the failed check and the values it compared. For example, `MinLength(1,0,)` means the name needs at least one character and had none. The result has three entries for two broken rules, because `Validation.between(0, 120)` is built from smaller checks: "greater than 0 or equal to 0" and "less than 120 or equal to 120". An age of 130 fails both the less-than check and the equal-to check of the second part, so it reports two entries. Validation never changes the value; the calling code decides what to do with the failures.

## What `Schema#validate` Checks

`Schema#validate` walks the whole value. It checks the rule of each record field, and it also descends into nested records, the elements of collections, the keys and values of maps, the content of `Some`, and the active side of an `Either`. A rule on a field of a nested case class is therefore checked when we validate the outer value.

`Schema#validate` checks the value with the implicit `Schema[A]` that is in scope at the call, not with the schema on which it is called. In the common case, where the schema is the implicit value in the companion object, the two are the same.

:::caution
Only rules attached to record fields are checked, either with `@validate` or with the `validation0` parameter of `Schema.Field`. The following rules are not reported by `Schema#validate`:

- A rule attached with `Schema#validation`, which stores it as an annotation on the schema. `Schema#validate` never reads that annotation.
- `Validation.fail`, and rules that fail through it, such as the default `onLeft` of `Validation#right` and the default `onRight` of `Validation#left`. These rules fail with an empty list of errors, and `Schema#validate` treats an empty list as success.

For a rule on a whole value, call `Validation#validate` on the value directly, as in the next section.
:::

## Combining Rules

The `Validation` object provides constructors for common rules. Rules combine with operators on the `Validation` value itself:

- `&&` requires both rules to pass.
- `||` requires at least one rule to pass.
- `!` negates a rule.

The following rule accepts a username that is between 3 and 20 characters long and contains only lowercase letters and digits:

```scala mdoc:silent
import zio.schema.validation.Regex

val lowerOrDigit: Regex = Regex.Between('a', 'z') | Regex.Between('0', '9')

val username: Validation[String] =
  Validation.minLength(3) && Validation.maxLength(20) && Validation.regex(lowerOrDigit.atLeast(1))
```

Next, we check a few values against this rule:

```scala mdoc
username.validate("alice42").isRight
username.validate("A").isLeft
```

`Validation#validate` returns `Right(())` when the value passes and `Left` with the failed checks otherwise.

## Validating Options and Alternatives

`Validation#optional` applies a rule to an `Option` and accepts `None` by default:

```scala mdoc
val maybeAge: Validation[Option[Int]] = Validation.between(0, 120).optional()

maybeAge.validate(None).isRight
maybeAge.validate(Some(130)).isLeft
```

`Validation.anyOf` accepts a value when at least one of several rules passes. `Validation.allOf` is the same idea with every rule required:

```scala mdoc
val fourOrSmall: Validation[Int] =
  Validation.anyOf(Validation.equalTo(4), Validation.lessThan(3))

fourOrSmall.validate(4).isRight
fourOrSmall.validate(10).isLeft
```

## Where to Go Next

- The [Validation](../reference/validation.md) reference page covers the full `Validation` API, including the `Regex` DSL and the predefined validators for emails, times, and phone numbers.
- The [Standard Type Reference](../standard-type-reference.md) lists the types that the rules apply to.
- [Basic Building Blocks](../basic-building-blocks.md) shows how fields are declared in a schema.
