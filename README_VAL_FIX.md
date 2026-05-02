# Issue #480 — Error Accumulation in `DynamicValue`

## Summary

`DynamicValue#toTypedValue` (and the underlying `toTypedValueLazyError`) is
fail-fast: as soon as it encounters a single mismatch it returns `Left(...)`
and stops walking the structure. For diagnostics, form-style validation and
"tell me everything that is wrong with this payload" workflows, callers want
*all* the errors at once.

This change introduces a new, additive method on `DynamicValue`:

```scala
def validate[A](schema: Schema[A]): Either[Chunk[String], Unit]
```

`validate` walks the entire `(DynamicValue, Schema)` pair and returns every
structural mismatch it finds, with each error tagged by the dotted path at
which it occurred (e.g. `user.address.zip: missing required field`).

## Design

- **Additive only.** The existing `toTypedValue`, `toValue`,
  `toTypedValueOption` and `toTypedValueLazyError` methods are unchanged.
  Their fail-fast semantics are preserved for callers that depend on them.
- **No new dependency.** The method returns `Either[Chunk[String], Unit]`
  using only `zio.Chunk`, which `zio-schema` already depends on. There is no
  need to pull in `zio-prelude`'s `Validation`.
- **Structural validation only.** `validate` checks shape, presence of
  required record fields, primitive type tags, and enum case names. It does
  not run user-defined `Schema.Transform` predicates, since those would
  require materialising the typed value and would shift this from validation
  into full decoding. Transforms recurse into their inner schema.

## Behaviour

| Construct                                | Behaviour                                                    |
| ---------------------------------------- | ------------------------------------------------------------ |
| `Primitive` vs `Primitive`               | Match required on `StandardType`                             |
| `Record` vs `Record` / `GenericRecord`   | Recurse into every field; missing required fields reported   |
| `Enumeration` vs `Enum`                  | Case name must exist; recurse into chosen case               |
| `Sequence`, `Set`, `Map`                 | Recurse into every element; errors tagged with index         |
| `Optional` + `None` / `Some`             | `None` always valid; `Some` recurses                         |
| `Tuple2`                                 | Recurse into both sides, accumulate                          |
| `Either`                                 | Recurse into matching side                                   |
| `Fallback` (`Left` / `Right` / `Both`)   | Recurse into matching side(s), accumulate for `Both`         |
| `Transform`                              | Recurse into inner schema (predicate not evaluated)          |
| `Lazy`                                   | Recurse into forced schema                                   |
| `Dynamic`                                | Always valid                                                 |
| `DynamicValue.Error(msg)`                | Reported as a single error                                   |
| Anything else                            | Reported as a shape mismatch                                 |

Errors carry a dotted path through the value, for example:

```
user.addresses.[1].zip: missing required field
user.age: primitive type mismatch: expected int, got string
```

## Files Touched

- `zio-schema/shared/src/main/scala/zio/schema/DynamicValue.scala`
  - Added `def validate[A](schema: Schema[A]): Either[Chunk[String], Unit]`
    on the `DynamicValue` trait.
  - Added `private[schema] def validateValue(...)` helper on the
    `DynamicValue` companion object that does the recursive walk and error
    accumulation.

No other files needed to change.
