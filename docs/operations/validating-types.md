---
id: validation
title: "Validation"
description: "How to attach validation rules to a schema and check values, plus the full Validation API: rule constructors, combinators, the Regex DSL, predefined validators and ValidationError cases."
keywords:
  - "Data Validation"
  - "Regex DSL"
  - "ValidationError"
  - "validate Annotation"
  - "Validation"
---

Validation rules describe which values of a type are acceptable, such as a non-empty name or an age between 0 and 120. In ZIO Schema such a rule is a `Validation[A]`: a **composable rule** that decides whether a value of type `A` is acceptable, and explains why when it is not. Rules attach to the fields of a schema, so every program that holds the schema can check values the same way. The type parameter `A` is the type of value the rule checks; it is invariant. The fundamental operations are `Validation#validate`, which runs the rule, and the boolean operators `&&`, `||` and `!`, which combine rules.

`Validation` has these properties:

- Data, not code. A rule is a tree of predicates, so it can be inspected and combined before it runs.
- Error accumulation. A failed rule returns every failed check, not only the first.
- Pure. Validating never changes the value; the caller decides what to do with the errors.

A `Validation` wraps a boolean tree of predicates:

```scala
final case class Validation[A](bool: Bool[Predicate[A]]) {
  def &&(that: Validation[A]): Validation[A]
  def ||(that: Validation[A]): Validation[A]
  def unary_! : Validation[A]
  def validate(value: A): Either[Chunk[ValidationError], Unit]
}

object Validation extends Regexs with Time
```

All types on this page live in the `zio.schema.validation` package, except the `@validate` annotation, which lives in `zio.schema.annotation`. The [Usage](#usage) section shows the common path; the sections after it cover the full API.

## Usage

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

The `Validation.minLength(1)` rule rejects an empty name, and the `Validation.between(0, 120)` rule keeps the age in range; both bounds are inclusive. `Schema#validate` checks a value and returns the failed checks as a `Chunk[ValidationError]`. A valid value gives an empty chunk:

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

Each printed entry names the failed check and the values it compared. For example, `MinLength(1,0,)` means the name needs at least one character and had none. The result has three entries for two broken rules, because `Validation.between(lower, upper)` is built as `(greaterThan(lower) || equalTo(lower)) && (lessThan(upper) || equalTo(upper))`. An age of 130 fails both the less-than check and the equal-to check of the upper bound, so it reports two entries.

The codecs do not run these rules, so we call `Schema#validate` after decoding untrusted input, as [Validating After Decoding](#validating-after-decoding) shows. A rule can also run on its own with `Validation#validate`; see [Checking a Single Value](#checking-a-single-value).

## Attaching Rules to a Schema

A rule takes effect in `Schema.validate` only when it sits on a record field. ZIO Schema offers two ways to put it there: the `@validate` annotation for derived schemas, and the `validation0` parameter for schemas written by hand.

### The `@validate` Annotation

`@validate` is a static annotation that holds one rule:

```scala
final case class validate[A](validation: Validation[A]) extends scala.annotation.StaticAnnotation
```

When `DeriveSchema.gen` builds a record schema, it reads the `@validate` annotations on each constructor parameter and stores their rule in `Schema.Field#validation` for that field. A field with several `@validate` annotations gets all of them, combined with `&&`. A field without one gets `Validation.succeed`. In this example, `name` carries two annotations and `price` carries one:

```scala mdoc:reset
import zio.schema._
import zio.schema.annotation.validate
import zio.schema.validation.Validation

final case class Product(
  @validate(Validation.minLength(1)) @validate(Validation.maxLength(50)) name: String,
  @validate(Validation.greaterThan(BigDecimal(0))) price: BigDecimal
)

object Product {
  implicit val schema: Schema[Product] = DeriveSchema.gen[Product]
}

Schema.validate(Product("", BigDecimal(-1))).map(_.message)
```

:::caution[Scala 2 and Case Classes With More Than 22 Fields]
On Scala 2, `DeriveSchema.gen` represents a case class with more than 22 fields as a `GenericRecord`, and it drops every `@validate` annotation on such a class. `Schema.validate` then reports no errors for it. Scala 3 derivation keeps the annotations. On Scala 2, call `Validation#validate` on the field values directly.
:::

### Field Rules in Hand-Written Schemas

`Schema.Field.apply` takes the rule through its `validation0` parameter, which defaults to `Validation.succeed`. See [Schema Data Types](../basic-building-blocks.md) for the other parameters. The following schema rejects temperatures below absolute zero:

```scala mdoc:reset
import zio.schema._
import zio.schema.validation.Validation

final case class Temperature(celsius: Double)

object Temperature {
  implicit val schema: Schema[Temperature] =
    Schema.CaseClass1[Double, Temperature](
      id0 = TypeId.parse("Temperature"),
      field0 = Schema.Field[Temperature, Double](
        name0 = "celsius",
        schema0 = Schema[Double],
        validation0 = Validation.greaterThan(-273.15),
        get0 = _.celsius,
        set0 = (t, c) => t.copy(celsius = c)
      ),
      defaultConstruct0 = c => Temperature(c)
    )
}

Schema.validate(Temperature(-300.0)).map(_.message)
```

### Rules on a Whole Schema

`Schema#validation` returns a copy of the schema with the rule stored as a `@validate` annotation:

```scala
sealed trait Schema[A] {
  def validation(validation: Validation[A]): Schema[A]
}
```

`Schema.validate` does not read schema annotations. It only reads `Schema.Field#validation` of record fields, so the rule added below is never checked:

```scala mdoc:reset
import zio.schema._
import zio.schema.validation.Validation

val smallInt: Schema[Int] = Schema[Int].validation(Validation.lessThan(10))

Schema.validate(100)(smallInt)

Validation.lessThan(10).validate(100)
```

:::caution[`Schema#validation` Is Not Checked by `Schema#validate`]
`Schema#validation(v)` adds a rule that `Schema#validate` never checks. To enforce a rule, put it on a field with `@validate` (or `validation0`), or call `v.validate(value)` directly.
:::

## Running Rules

Rules run either one at a time, through `Validation#validate`, or for a whole value, through `Schema.validate`, which visits every field rule in the value.

### Checking a Single Value

`Validation#validate` returns `Right(())` when the value passes, and `Left` with the failed checks otherwise:

```scala
final case class Validation[A](bool: Bool[Predicate[A]]) {
  def validate(value: A): Either[Chunk[ValidationError], Unit]
}
```

A rule built with `&&` collects the errors of both sides. A rule built with `||` passes when either side passes, and reports the errors of both sides when both fail:

```scala mdoc:reset
import zio.schema.validation._

val shortCode: Validation[String] = Validation.minLength(2) && Validation.maxLength(4)

shortCode.validate("abc")

shortCode.validate("abcdef")

(Validation.equalTo(0) || Validation.greaterThan(100)).validate(50)
```

### Checking a Whole Value With a Schema

`Schema.validate` walks a value with its schema and runs the rule of every record field it meets. `Schema#validate` is the method form of the same operation:

```scala
object Schema {
  def validate[A](value: A)(implicit schema: Schema[A]): Chunk[ValidationError]
}

sealed trait Schema[A] {
  def validate(value: A)(implicit schema: Schema[A]): Chunk[ValidationError]
}
```

`Schema#validate` checks with the implicit `Schema[A]` in scope, not with the schema that receives the call. In the common case, where the schema is the implicit value in the companion object, the two are the same. When you hold a schema that is not the implicit one, call `Schema.validate(value)(schema)` instead.

The walk follows the structure of the value. It descends into nested records, the active case of an enum, the elements of sequences, sets and maps (keys and values), the content of `Some`, `Left`, `Right` and `Fallback`, both sides of a tuple, and lazy schemas. For a transformed schema, it converts the value back to the underlying type and validates that. Primitive, dynamic and failing schemas add no checks. Nested rules work without extra code:

```scala mdoc:reset
import zio.schema._
import zio.schema.annotation.validate
import zio.schema.validation.Validation

final case class Tag(@validate(Validation.minLength(1)) label: String)

object Tag {
  implicit val schema: Schema[Tag] = DeriveSchema.gen[Tag]
}

final case class Post(title: String, tags: List[Tag])

object Post {
  implicit val schema: Schema[Post] = DeriveSchema.gen[Post]
}

Schema.validate(Post("Hello", List(Tag("scala"), Tag(""))))
```

Besides the errors of the field rules, the walk can report two structural errors. A record that has no value for a field (for example a `GenericRecord` whose `ListMap` lacks a key) reports `ValidationError.MissingField`. A transformed schema whose reverse function fails reports `ValidationError.Generic` with the failure message.

### Validating After Decoding

To reject invalid input, we decode first and then call `Schema.validate` on the result:

```scala mdoc:reset
import zio.schema._
import zio.schema.annotation.validate
import zio.schema.codec.JsonCodec
import zio.schema.validation.Validation

final case class Account(@validate(Validation.minLength(3)) handle: String)

object Account {
  implicit val schema: Schema[Account] = DeriveSchema.gen[Account]
}

val decoded = JsonCodec.jsonDecoder(Account.schema).decodeJson("""{"handle":"x"}""")

decoded.map(Schema.validate(_))
```

:::caution[Codecs Do Not Validate]
The codecs do not validate when decoding: a value that breaks its `@validate` rules decodes successfully. Call `Schema.validate` after decoding.
:::

## Creating Rules

The `Validation` companion object has constructors for string and numeric checks, constant rules, and rules built from a collection of rules.

### String Rules

`Validation.minLength` and `Validation.maxLength` bound the length of a string; both bounds are inclusive. `Validation.regex` requires the whole string to match a pattern built with the [Regex DSL](#the-regex-dsl):

```scala
object Validation {
  def minLength(n: Int): Validation[String]
  def maxLength(n: Int): Validation[String]
  def regex(r: Regex): Validation[String]
}
```

The following rules check a length and a digits-only pattern:

```scala mdoc:reset
import zio.schema.validation._

Validation.minLength(2).validate("a")

Validation.maxLength(5).validate("hello")

Validation.regex(Regex.digit.atLeast(1)).validate("2024")
```

### Numeric Rules

`Validation.greaterThan`, `Validation.lessThan` and `Validation.equalTo` compare a number with a fixed value; `Validation.greaterThan` and `Validation.lessThan` are strict. `Validation.between` accepts values from `lower` to `upper`, both included. Each constructor needs an implicit `NumType[A]`:

```scala
object Validation {
  def greaterThan[A](value: A)(implicit numType: NumType[A]): Validation[A]
  def lessThan[A](value: A)(implicit numType: NumType[A]): Validation[A]
  def equalTo[A](value: A)(implicit numType: NumType[A]): Validation[A]
  def between[A](lower: A, upper: A)(implicit numType: NumType[A]): Validation[A]
}
```

`NumType` has instances for `Int`, `Long`, `Short`, `Float`, `Double`, `BigDecimal` and `BigInt`. The same constructors work across these types:

```scala mdoc
Validation.greaterThan(0L).validate(5L)

Validation.lessThan(1.5).validate(2.0)

Validation.equalTo(BigInt(42)).validate(BigInt(41))

Validation.between(1, 10).validate(11)
```

`NumType` has no instance for `Byte` or `Char`. For such a type, we adapt an `Int` rule with `Validation#contramap`:

```scala mdoc
val positiveByte: Validation[Byte] = Validation.greaterThan(0).contramap[Byte](_.toInt)

positiveByte.validate((-1).toByte)
```

### Constant Rules

`Validation.succeed` accepts every value, and `Validation.fail` rejects every value. They are the neutral starting points for building rules in a loop:

```scala
object Validation {
  def succeed[A]: Validation[A]
  def fail[A]: Validation[A]
}
```

`Validation.fail` rejects the value without describing why, so its error chunk is empty:

```scala mdoc
Validation.succeed[Int].validate(1)

Validation.fail[Int].validate(1)
```

The empty chunk matters for `Schema.validate`, which reports field errors by collecting the error chunks of the failed rules. A rule that fails with an empty chunk adds nothing:

```scala mdoc:reset
import zio.schema._
import zio.schema.annotation.validate
import zio.schema.validation.Validation

final case class Locked(@validate(Validation.fail[Int]) value: Int)

object Locked {
  implicit val schema: Schema[Locked] = DeriveSchema.gen[Locked]
}

Schema.validate(Locked(1))
```

:::caution[Rules That Fail With No Errors Are Invisible to `@validate`]
`Validation.fail`, and any negation that fails with an empty error chunk, report no error through `@validate`: `Schema.validate` returns an empty chunk even though the rule failed. This affects a negated `Validation.succeed` and the negation of a passing `Validation#optional`, `Validation#contramap` or `Validation.either` rule (see [Boolean Operators](#boolean-operators)), as well as `Validation#right` and `Validation#left` with their default argument. To detect these failures, call `Validation#validate` and check for `Left`.
:::

### Rules From a Collection of Rules

`Validation.allOf` requires every rule to pass, and `Validation.anyOf` requires at least one. Each has a varargs overload and an `Iterable` overload:

```scala
object Validation {
  def allOf[A](vs: Validation[A]*): Validation[A]
  def allOf[A](vl: Iterable[Validation[A]]): Validation[A]
  def anyOf[A](vs: Validation[A]*): Validation[A]
  def anyOf[A](vl: Iterable[Validation[A]]): Validation[A]
}
```

`Validation.allOf` folds the rules with `&&`, starting from `Validation.succeed`, and `Validation.anyOf` folds them with `||`, starting from `Validation.fail`. An empty `Validation.allOf` therefore accepts every value, and an empty `Validation.anyOf` rejects every value:

```scala mdoc:reset
import zio.schema.validation._

val forbidden = List("admin", "root").map(name => !Validation.regex(Regex.literal(name)))

Validation.allOf(forbidden).validate("root").isLeft

Validation.anyOf(Validation.equalTo(1), Validation.equalTo(2)).validate(2)

Validation.anyOf(List.empty[Validation[Int]]).validate(5).isLeft
```

## Combining and Adapting Rules

Combinators build larger rules from smaller ones. The boolean operators combine rules of the same type, and the adapters move a rule to a different input type.

### Boolean Operators

`&&` passes when both rules pass, `||` passes when either passes, and `!` (written as a prefix) passes when the rule fails:

```scala
final case class Validation[A](bool: Bool[Predicate[A]]) {
  def &&(that: Validation[A]): Validation[A]
  def ||(that: Validation[A]): Validation[A]
  def unary_! : Validation[A]
}
```

A negated rule still explains its failures. Every predicate records an error for the opposite outcome, and negation reports that error:

```scala mdoc:reset
import zio.schema.validation._

(!Validation.minLength(3)).validate("hello")

(!Validation.equalTo(0)).validate(0)
```

The following table lists the error each constructor reports when the rule fails, and when its negation fails:

| Constructor                                                                  | Error when the rule fails           | Error when the negation fails   |
| ---------------------------------------------------------------------------- | ----------------------------------- | ------------------------------- |
| `Validation.minLength(n)`                                                    | `MinLength(n, actual, string)`      | `MaxLength(n, actual, string)`  |
| `Validation.maxLength(n)`                                                    | `MaxLength(n, actual, string)`      | `MinLength(n, actual, string)`  |
| `Validation.regex(r)`                                                        | `RegexMatch(string, r)`             | `NotRegexMatch(string, r)`      |
| `Validation.greaterThan(x)`                                                  | `GreaterThan(value, x)`             | `LessThan(value, x)`            |
| `Validation.lessThan(x)`                                                     | `LessThan(value, x)`                | `GreaterThan(value, x)`         |
| `Validation.equalTo(x)`                                                      | `EqualTo(value, x)`                 | `NotEqualTo(value, x)`          |
| `Validation.succeed`                                                         | never fails                         | empty chunk                     |
| `Validation#optional` on `None`                                              | `EqualToNone()` (when not accepted) | `EqualToNone()` (when accepted) |
| `Validation#optional` on `Some`, `Validation#contramap`, `Validation.either` | errors of the inner rule            | empty chunk                     |

### Adapting the Input Type

`Validation#contramap` runs a rule on a value derived from the input. `Validation#optional` lifts a rule to `Option`; its `validNone` parameter decides whether `None` passes. `Validation.either` builds a rule for `Either` from one rule per side. `Validation#right` and `Validation#left` lift a rule to one side of an `Either` and take a rule for the other side, which defaults to `Validation.fail`:

```scala
final case class Validation[A](bool: Bool[Predicate[A]]) {
  def contramap[B](f: B => A): Validation[B]
  def optional(validNone: Boolean = true): Validation[Option[A]]
  def right[B](onLeft: Validation[B] = Validation.fail[B]): Validation[Either[B, A]]
  def left[B](onRight: Validation[B] = Validation.fail[B]): Validation[Either[A, B]]
}

object Validation {
  def either[L, R](left: Validation[L], right: Validation[R]): Validation[Either[L, R]]
}
```

We adapt a string rule to a wrapper type, to optional values, and to both sides of an `Either`:

```scala mdoc:reset
import zio.schema.validation._

final case class Username(value: String)

val usernameRule: Validation[Username] = Validation.minLength(3).contramap[Username](_.value)
usernameRule.validate(Username("jo"))

val nickname: Validation[Option[String]] = Validation.minLength(2).optional()
nickname.validate(None)
nickname.validate(Some("x"))

val required: Validation[Option[String]] = Validation.minLength(2).optional(validNone = false)
required.validate(None)

val idOrName: Validation[Either[Int, String]] =
  Validation.either(Validation.greaterThan(0), Validation.minLength(1))
idOrName.validate(Left(-5))

Validation.minLength(1).right[Int]().validate(Left(1)).isLeft
```

## Predefined Validators

The `Validation` object mixes in the `Regexs` and `Time` traits, so ready-made validators for common string formats are members of `Validation`. Phone number validators live in a separate object, `PhoneNumberValidation`.

### Identifiers, Emails, and Network Addresses

These validators are values of type `Validation[String]`:

| Validator               | Accepts                                                                                                              |
| ----------------------- | -------------------------------------------------------------------------------------------------------------------- |
| `Validation.identifier` | One or more ASCII letters, digits or underscores                                                                     |
| `Validation.email`      | An email address with a domain name, a bracketed IPv4 address or a bracketed `IPv6:` address; at most 254 characters |
| `Validation.ipV4`       | A dotted IPv4 address with each part from 0 to 255; leading zeros are allowed                                        |
| `Validation.ipV6`       | An IPv6 address, including `::` shortening and an embedded IPv4 tail                                                 |
| `Validation.uuidV4`     | A version 4 UUID in the 8-4-4-4-12 hexadecimal form                                                                  |

Each validator checks the whole string:

```scala mdoc:reset
import zio.schema.validation._

Validation.identifier.validate("user_1").isRight

Validation.email.validate("bob+and-alice@gmail.com").isRight
Validation.email.validate("a@@a.com").isRight

Validation.ipV4.validate("192.168.1.255").isRight
Validation.ipV4.validate("10.0.0.256").isRight

Validation.ipV6.validate("2001:cdba::3257:9652").isRight

Validation.uuidV4.validate(java.util.UUID.randomUUID().toString).isRight
Validation.uuidV4.validate("1e3118de-ddb6-11ec-8653-93e6961d46be").isRight
```

The last UUID is a version 1 UUID, so `Validation.uuidV4` rejects it.

### Durations and Times

`Validation.duration` accepts ISO-8601 durations such as `P1D` or `PT1H30M`, the form that `java.time.Duration#toString` produces. `Validation.time` builds a validator from a time format:

```scala
object Validation {
  lazy val duration: Validation[String]
  def time(format: String): Validation[String]
}
```

The format of `Validation.time` is close to the one of `java.time.format.DateTimeFormatter`. Each letter may appear once; a letter written twice requires two digits, and a letter written once accepts one or two digits:

| Letter | Meaning                                        | Allowed repetitions |
| ------ | ---------------------------------------------- | ------------------- |
| `H`    | Hour of day (`HH` accepts `00` to `24`)        | 1 or 2              |
| `h`    | Hour of the clock, `0` to `12`                 | 1 or 2              |
| `m`    | Minute, `0` to `59`                            | 1 or 2              |
| `s`    | Second, `0` to `59`                            | 1 or 2              |
| `S`    | Fraction of a second, exactly that many digits | 1 to 9              |
| `a`    | `AM` or `PM`                                   | 1                   |

Every other character is copied literally, except other letters, which are reserved. `Validation.time` throws `IllegalArgumentException` when the format repeats a letter, writes it too many times, uses a reserved letter, or contains no time field. These validators check durations and clock times:

```scala mdoc
Validation.duration.validate("PT1H30M").isRight
Validation.duration.validate("1 hour").isRight

Validation.time("HH:mm").validate("09:30").isRight
Validation.time("HH:mm").validate("9:30").isRight

Validation.time("h:mm a").validate("7:05 PM").isRight
```

### Phone Numbers

`PhoneNumberValidation` holds one validator per country, named `phoneNumber` followed by the ISO 3166-1 alpha-2 country code, such as `PhoneNumberValidation.phoneNumberUS`, `PhoneNumberValidation.phoneNumberDE` or `PhoneNumberValidation.phoneNumberJP`. Each validator accepts a number that starts either with an international prefix (`+`, `00`, or a country-specific one such as `011`) followed by the country calling code, or, for countries that have one, with the national trunk prefix. Digits may be separated by a space or a hyphen:

```scala
object PhoneNumberValidation extends PhoneNumberValidation

trait PhoneNumberValidation {
  lazy val phoneNumberUS: Validation[String]
  lazy val phoneNumberDE: Validation[String]
  // ... one `phoneNumberXX` for each of 253 country and territory codes
}
```

The trait also exposes the `Regex` pieces it builds the validators from: `optionalSpace`, `optionalSeparator`, `twoDigits`, `threeDigits`, `plus`, `doubleZero`, `internationalPrefix`, `nationalPrefixZero`, `nationalPrefixOne`, `nationalPrefixEight` and `digitsWithSeparator`. A US number dialled with the `011` prefix passes:

```scala mdoc
PhoneNumberValidation.phoneNumberUS.validate("011 1 447142964").isRight
```

## The Regex DSL

`Regex` is a sealed trait that describes a regular expression as data. `Validation.regex` turns a `Regex` into a rule, and the predefined validators are built from it. A `Regex` always matches the whole input.

### Building Blocks

The `Regex` companion object provides the atoms of a pattern:

```scala
object Regex {
  def literal(str: String): Regex
  def oneOf(chars: Char*): Regex
  def between(minChar: Char, maxChar: Char): Regex
  def filter(f: Char => Boolean): Regex

  val digit: Regex
  val digitNonZero: Regex
  val letter: Regex
  val digitOrLetter: Regex
  val hexDigit: Regex
  val hexDigitNonZero: Regex
}
```

`Regex.literal` matches an exact string. `Regex.oneOf` matches one of the given characters, and `Regex.between` matches one character in an inclusive range. `Regex.filter` matches one character for which the predicate holds; it tests every `Char` value up front, so build such a pattern once and reuse it. `Regex.letter` matches an ASCII letter, and `Regex.hexDigit` matches a digit or a letter from `a` to `f` in either case. The `NonZero` variants exclude `0`.

These constructors produce the case classes and objects of `Regex`, which a codec or a tool can inspect:

| Case                                                       | Built by                      |
| ---------------------------------------------------------- | ----------------------------- |
| `Literal(str: String)`                                     | `Regex.literal`               |
| `CharacterSet(set: Set[Char])`                             | `Regex.oneOf`, `Regex.filter` |
| `Between(start: Char, end: Char)`                          | `Regex.between`               |
| `Repeat(regex: Regex, min: Option[Int], max: Option[Int])` | the repetition operators      |
| `Sequence(first: Regex, second: Regex)`                    | `Regex#~`                     |
| `Alternate(left: Regex, right: Regex)`                     | `Regex#\|`                    |
| `Letter`, `Digit`                                          | `Regex.letter`, `Regex.digit` |
| `Empty`                                                    | matches the empty string      |

### Repetition and Composition

Methods on `Regex` repeat a pattern and join patterns together. `Regex#~` matches one pattern followed by another, and `Regex#|` matches either pattern. The repetition methods take inclusive bounds:

```scala
sealed trait Regex {
  def atLeast(n: Int): Regex
  def atMost(n: Int): Regex
  def between(min: Int, max: Int): Regex
  def exactly(n: Int): Regex
  def + : Regex
  def * : Regex
  def ? : Regex
  def ~(that: Regex): Regex
  def |(that: Regex): Regex
}
```

`+` is `atLeast(1)`, `*` is `atLeast(0)` and `?` is `atMost(1)`. Unlike `Regex.between` on the companion, which takes two characters, `Regex#between` takes two repetition counts. We combine these into a pattern for hex colours such as `#a0b1c2`:

```scala mdoc:silent:reset
import zio.schema.validation._

val hexColor: Regex = Regex.literal("#") ~ Regex.hexDigit.exactly(6)

val semver: Regex =
  Regex.digit.+ ~ Regex.oneOf('.') ~ Regex.digit.+ ~ Regex.oneOf('.') ~ Regex.digit.+ ~
    (Regex.oneOf('-') ~ Regex.digitOrLetter.+).?
```

### Testing and Rendering

`Regex#test` checks a string against the pattern, and `Regex.toRegexString` renders the pattern in `java.util.regex` syntax, escaping special characters:

```scala
sealed trait Regex {
  def test(string: String): Boolean
}

object Regex {
  def toRegexString(regex: Regex): String
}
```

The patterns from the previous section test and render as follows:

```scala mdoc
hexColor.test("#a0b1c2")
hexColor.test("a0b1c2")

semver.test("1.10.0-rc1")

Regex.toRegexString(hexColor)
```

`Regex#test` compiles the pattern on every call. `Regex.toRegexString` throws `IllegalArgumentException` for a `Repeat` with neither a minimum nor a maximum, which the DSL methods never build.

## Validation Errors

`ValidationError` is a sealed trait; `ValidationError#message` gives a human-readable description. `Validation#validate` and `Schema.validate` return these cases:

| Case                                                           | Reported when                                                       |
| -------------------------------------------------------------- | ------------------------------------------------------------------- |
| `MinLength(minLength: Int, actualLength: Int, string: String)` | A string is shorter than the minimum length                         |
| `MaxLength(maxLength: Int, actualLength: Int, string: String)` | A string is longer than the maximum length                          |
| `GreaterThan[A](value: A, expected: A)`                        | `value` is not greater than `expected`                              |
| `LessThan[A](value: A, expected: A)`                           | `value` is not less than `expected`                                 |
| `EqualTo[A](value: A, expected: A)`                            | `value` is not equal to `expected`                                  |
| `NotEqualTo[A](value: A, expected: A)`                         | `value` is equal to `expected` under a negated `Validation.equalTo` |
| `EqualToNone()`                                                | An `Option` rule meets `None`                                       |
| `RegexMatch(str: String, expected: Regex)`                     | `str` does not match `expected`                                     |
| `NotRegexMatch(str: String, expected: Regex)`                  | `str` matches `expected` under a negated `Validation.regex`         |
| `MissingField(field: Schema.Field[_, _])`                      | A record value has no value for `field`                             |
| `Generic(message: String)`                                     | A transformed schema cannot convert the value back                  |
| `EqualToLeft[A](value: A)`, `EqualToRight[A](value: A)`        | Defined for `Either` checks; no built-in rule reports them          |

We print the messages of a failed rule with `ValidationError#message`:

```scala mdoc:reset
import zio.schema.validation._

Validation.maxLength(3).validate("abcd").left.map(_.map(_.message))
```

`DecodeError` also has a separate `DecodeError.ValidationError` case, which carries the `Validation`, the `Schema.Field` and a message. The built-in codecs never report it, because they do not run rules while decoding. See [Codec Core Types](../reference/codec-core.md) for decode errors.

:::caution[`RegexMatch` Messages Print the Whole Pattern]
`ValidationError.RegexMatch#message` includes the complete `Regex` tree of the rule. For a predefined validator such as `Validation.email`, that message is very long and means nothing to an end user. Match on `ValidationError.RegexMatch` and write your own message; [How to Validate Untrusted Input](../how-to/validate-input.md) shows one way.
:::

## Low-Level Building Blocks

You rarely use these types directly; the constructors and combinators above build them for you. They matter when you inspect a rule, for example to translate it into a JSON Schema or a database constraint.

`Validation` wraps a `Bool[Predicate[A]]`. `Bool` is a boolean expression tree, `Predicate` is a single check, and `NumType` supplies the ordering for numeric checks:

```scala
sealed trait Bool[A] {
  def &&(that: Bool[A]): Bool[A]
  def ||(that: Bool[A]): Bool[A]
  def unary_! : Bool[A]
  def map[B](f: A => B): Bool[B]
}

object Bool {
  final case class And[A](left: Bool[A], right: Bool[A]) extends Bool[A]
  final case class Or[A](left: Bool[A], right: Bool[A])  extends Bool[A]
  final case class Leaf[A](value: A)                     extends Bool[A]
  final case class Not[A](value: Bool[A])                extends Bool[A]
}

sealed trait Predicate[A] {
  def validate(value: A): Either[Chunk[ValidationError], Chunk[ValidationError]]
  def contramap[B](f: B => A): Predicate[B]
}

sealed trait NumType[A] {
  def numeric: Numeric[A]
}
```

`Predicate#validate` returns `Left` with the errors of a failure, and `Right` with the errors to report if the predicate is negated. The `Predicate` cases are `Str.MinLength`, `Str.MaxLength`, `Str.Matches`, `Num.GreaterThan`, `Num.LessThan`, `Num.EqualTo`, `True`, `Optional`, `Contramap` and `Either`. The `NumType` instances are `IntType`, `LongType`, `ShortType`, `FloatType`, `DoubleType`, `BigDecimalType` and `BigIntType`. Reading the tree of a rule shows how it is built:

```scala mdoc
Validation.minLength(3).bool

(Validation.minLength(3) && !Validation.maxLength(1)).bool
```

## Integration

Rules reach the rest of ZIO Schema through record fields. `Schema.Field#validation` holds the rule of a field; see [Schema Data Types](../basic-building-blocks.md) for `Schema.Field` and the other schema cases, and [Basic Building Blocks](../basic-building-blocks.md) for how fields are declared in a schema. [Schema Methods](../reference/schema-methods.md) covers `Schema#validate` alongside the other methods of `Schema`, and [Schema Annotations](../schema-annotations.md) covers the other annotations that derivation reads. The [Standard Type Reference](../standard-type-reference.md) lists the types that the rules apply to. For a task-oriented walkthrough of checking request data, see [How to Validate Untrusted Input](../how-to/validate-input.md).
