package zio.schema.codec

import scala.xml.{ Node, XML }

package object xml {

  /**
   * The default configuration for XML encoding/decoding.
   */
  val defaultConfiguration: XmlCodec.Configuration = XmlCodec.Configuration()

  /**
   * Create an XML encoder for a given schema.
   */
  def xmlEncoder[A](schema: Schema[A]): Encoder[Node, Node, A] =
    XmlCodec.encoder(schema, defaultConfiguration)

  /**
   * Create an XML decoder for a given schema.
   */
  def xmlDecoder[A](schema: Schema[A]): Decoder[Node, Node, A] =
    XmlCodec.decoder(schema, defaultConfiguration)

  /**
   * Create an XML codec for a given schema.
   */
  def xmlCodec[A](schema: Schema[A]): Codec[Node, Node, A] =
    XmlCodec.codec(schema, defaultConfiguration)

  /**
   * Create an XML codec for a given schema with custom configuration.
   */
  def xmlCodec[A](schema: Schema[A], configuration: XmlCodec.Configuration): Codec[Node, Node, A] =
    XmlCodec.codec(schema, configuration)

}
