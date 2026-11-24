package zio.schema.annotation

/**
 * Attaches a human-readable description to a type, field or case. On Scala 3, `DeriveSchema.gen` also adds a `description` to the schema of a type that has a Scaladoc comment, holding the raw comment text with its comment markers; on Scala 2 it adds none. No codec in zio-schema reads it, so it is metadata for your own interpreters.
 *
 * @param text
 *   the description
 */
final case class description(text: String) extends scala.annotation.StaticAnnotation
