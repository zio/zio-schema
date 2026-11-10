---
id: validation
title: "Validation"
---

Validation rules describe which values of a type are acceptable, such as a non-empty name or an age between 0 and 120. ZIO Schema stores these rules inside the schema, so any codec or application code that uses the schema can check values in the same way.

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

The `minLength(1)` rule rejects an empty name, and the `between(0, 120)` rule keeps the age in range. Both bounds are inclusive.

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

Each printed entry names the failed rule and the values it compared. For example, `MinLength(1,0,)` means the name needs at least one character and had none. Validation never changes the value; the calling code decides what to do with the failures.

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
val evenOrSmall: Validation[Int] =
  Validation.anyOf(Validation.equalTo(4), Validation.lessThan(3))

evenOrSmall.validate(4).isRight
evenOrSmall.validate(10).isLeft
```

## Where to Go Next

- The [Standard Type Reference](../standard-type-reference.md) lists the types that the rules apply to.
- [Basic Building Blocks](../basic-building-blocks.md) shows how fields are declared in a schema.
