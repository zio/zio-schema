package zio.schema.codec

import scala.annotation.StaticAnnotation

/**
 * Sets the Protobuf field number of a record field, used by [[ProtobufCodec]]
 * instead of the field's 1-based position. Give each field of a record a
 * different number.
 *
 * {{{
 * import zio.schema.{DeriveSchema, Schema}
 * import zio.schema.codec.{ProtobufCodec, fieldNumber}
 *
 * final case class Item(@fieldNumber(5) sku: String, qty: Int)
 * implicit val itemSchema: Schema[Item] = DeriveSchema.gen[Item]
 *
 * ProtobufCodec.protobufCodec[Item].encode(Item("A1", 2))
 * // Chunk(42,2,65,49,16,2): field 5 holds "A1", field 2 holds 2
 * }}}
 */
final case class fieldNumber(n: Int) extends StaticAnnotation
