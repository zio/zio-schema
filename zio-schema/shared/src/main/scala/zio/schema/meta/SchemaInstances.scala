package zio.schema.meta

import zio.constraintless.IsElementOf.{Head, Tail}
import zio.constraintless.{IsElementOf, TypeList}
import zio.schema.Schema

/**
 * Special version of zio-constraintless's Instances for capturing Schema
 * instances
 */
trait SchemaInstances[As <: TypeList] {

  def withInstance[B, D](use: Schema[B] => D)(implicit
    ev: B IsElementOf As
  ): D

  def all: List[Schema[_]]
}

object SchemaInstances {
  import TypeList._

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
  implicit lazy val instancesEnd: SchemaInstances[End] = new SchemaInstances[End] {
    override def withInstance[B, D](use: Schema[B] => D)(implicit
      ev: IsElementOf[B, End]
    ): D =
      sys.error("hmmm")

    override def all: List[Schema[_]] = List.empty
  }
}
