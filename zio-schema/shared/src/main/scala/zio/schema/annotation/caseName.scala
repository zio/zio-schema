package zio.schema.annotation

import scala.annotation.StaticAnnotation

/**
 * Annotation for specifying an alternate name for a case of an enum.
 * `Schema.Case#caseName` returns this name instead of the subtype name, and the
 * JSON, XML and BSON codecs use it as the case name when encoding and decoding.
 * With `@caseName("kitty")` on `final case class Cat(name: String)`,
 * `JsonCodec` encodes `Cat("Tom")` as `{"kitty":{"name":"Tom"}}`.
 *
 * This is the dual of `@fieldName`, but for sums rather than products.
 *
 * @param name
 *   The alternate name to use for the case.
 */
final case class caseName(name: String) extends StaticAnnotation
