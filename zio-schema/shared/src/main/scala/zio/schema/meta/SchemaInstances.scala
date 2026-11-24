package zio.schema.meta

import zio.constraintless.IsElementOf.{Head, Tail}
import zio.constraintless.{IsElementOf, TypeList}
import zio.schema.Schema

/**
 * Holds a `Schema` for each type in the type list `As`, in the style of
 * zio-constraintless's `Instances`. An [[ExtensibleMetaSchema]] uses it to find
 * its built-in types, and the implicit instances build it from the `Schema`
 * instances in scope.
 */
trait SchemaInstances[As <: TypeList] {

  /** Runs `use` with the schema of `B`, which the type list `As` contains. */
  def withInstance[B, D](use: Schema[B] => D)(implicit
    ev: B IsElementOf As
  ): D

  /** Returns the schemas in the order of the type list. */
  def all: List[Schema[_]]
}

/** Implicit [[SchemaInstances]] for any type list whose types have a `Schema`. */
object SchemaInstances {
  import TypeList._

  /** Builds the instances for `A :: As` from a `Schema[A]` and the instances for `As`. */
  implicit def instancesCons[A, As <: TypeList](implicit
    c: Schema[A],
    ev: SchemaInstances[As]
  ): SchemaInstances[A :: As] = new SchemaInstances[A :: As] {
    override def withInstance[B, D](
      use: Schema[B] => D
    )(implicit ev2: IsElementOf[B, A :: As]): D =
      ev2 match {
        case Head() =>
          use(
            c.asInstanceOf[Schema[B]]
          ) // Coz we have compile time evidence that B is in fact A
        case Tail(x) => ev.withInstance(use)(x)
      }

    override def all: List[Schema[_]] = c :: ev.all
  }

  // The definition is slightly from what mentioned in the paper where it traverses hlist
  /** The instances for the empty type list. */
  implicit lazy val instancesEnd: SchemaInstances[End] = new SchemaInstances[End] {
    override def withInstance[B, D](use: Schema[B] => D)(implicit
      ev: IsElementOf[B, End]
    ): D =
      sys.error("hmmm")

    override def all: List[Schema[_]] = List.empty
  }
}
