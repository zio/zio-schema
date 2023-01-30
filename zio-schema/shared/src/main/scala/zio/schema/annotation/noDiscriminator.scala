package zio.schema.annotation

import scala.annotation.StaticAnnotation

/**
 * Indicates that an Enum should be encoded without any specific discriminator, relying on fallback on failure
 * when decoding.
 */
final case class noDiscriminator() extends StaticAnnotation
