package zio.schema.annotation

import scala.annotation.StaticAnnotation

/*
 * Automatically added in sealed traits with only case objects or case class without parameters.
 * Gives error if it used in other types of enumerations.
 */

/**
 * Marks an enumeration whose cases are all case objects or case classes without fields. `DeriveSchema.gen` adds it, with `automaticallyAdded = true`, to such a sealed trait, and fails to compile with the message `... must be a simple Enum` when it is written by hand on a sealed trait that does not qualify. The JSON and XML codecs encode the cases of a simple enumeration as plain strings: `JsonCodec` encodes `case object Red` as `"Red"`.
 *
 * @param automaticallyAdded
 *   `true` when derivation added the annotation, `false` when it was written by hand
 */
final case class simpleEnum(automaticallyAdded: Boolean = false) extends StaticAnnotation
