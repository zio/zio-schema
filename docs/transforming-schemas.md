---
id: transforming-schemas
title: "Transforming Schemas"
---

Once we have a `Schema`, we can transform it into another `Schema` by applying a `Transformer`. In normal Scala code this would be the equivalent of `map`.

In ZIO Schema this is modelled by the `Transform` type class:

```scala
  final case class Transform[A, B](codec: Schema[A], f: A => Either[String, B], g: B => Either[String, A])
      extends Schema[B] {
    override type Accessors[Lens[_, _], Prism[_, _], Traversal[_, _]] = codec.Accessors[Lens, Prism, Traversal]

    override def makeAccessors(b: AccessorBuilder): codec.Accessors[b.Lens, b.Prism, b.Traversal] =
      codec.makeAccessors(b)

    override def serializable: Schema[Schema[_]] = Meta(SchemaAst.fromSchema(codec))
    override def toString: String                = s"Transform($codec)"
  }
```

In the previous example, we can transform the `User` Schema into a `UserRecord` Schema, which is a record, by using the `transform` method, which has to be an "isomorphism" (= providing methods to transform A to B _and_ B to A):

```scala
/**
 * Transforms this `Schema[A]` into a `Schema[B]`, by supplying two functions that can transform
 * between `A` and `B`, without possibility of failure.
 */
def transform[B](f: A => B, g: B => A): Schema[B] =
  Schema.Transform[A, B](self, a => Right(f(a)), b => Right(g(b)))
```

