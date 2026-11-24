package zio.schema.annotation

import zio.schema.validation.Validation

/**
 * Attaches a [[zio.schema.validation.Validation]] to a case class field.
 * `DeriveSchema.gen` combines the `validate` annotations of a field with `&&`
 * into the field's `validation`, which `Schema.validate` checks.
 *
 * {{{
 * import zio.schema.{DeriveSchema, Schema}
 * import zio.schema.validation.Validation
 *
 * final case class Order(@validate(Validation.greaterThan(0)) quantity: Int)
 * object Order {
 *   implicit val schema: Schema[Order] = DeriveSchema.gen[Order]
 * }
 *
 * val errors = Schema.validate(Order(0))
 * // Chunk(GreaterThan(0,0))
 * }}}
 *
 * @param validation
 *   the validation to check on the field's value
 */
final case class validate[A](validation: Validation[A]) extends scala.annotation.StaticAnnotation
