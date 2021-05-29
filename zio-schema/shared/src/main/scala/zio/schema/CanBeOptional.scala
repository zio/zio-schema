package zio.schema

import scala.annotation.implicitNotFound

/**
 * A value of type `CanBeOptional[A]` provides implicit evidence that a schema for
 * type `A` can be made optional. That is, `A` is not already an `Option[_]`.
 */
@implicitNotFound(
  "An Optional schema cannot be embedded within an Optional schema. " + "This operation is not permitted since there is no way to distinguish " + "the encoding of `Some(None)` from the encoding of `None`."
)
sealed trait CanBeOptional[-A]

object CanBeOptional extends CanBeOptional[Any] {

  sealed trait Impl[-A]

  object Impl extends Impl[Any] {
    implicit def impl[A]: Impl[A]                = Impl
    implicit val implAmbiguous1: Impl[Option[_]] = Impl
    implicit val implAmbiguous2: Impl[Option[_]] = Impl
  }

  implicit def canBeOptional[A: Impl]: CanBeOptional[A] = CanBeOptional
}
