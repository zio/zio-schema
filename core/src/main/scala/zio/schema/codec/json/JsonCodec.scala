package zio.schema.codec.json

import zio.json.{ JsonDecoder, JsonEncoder }
import zio.schema._
import zio.schema.codec.Codec

// TODO: Should this be a class that takes a character encoding parameter?
object JsonCodec extends Codec {

  override def encoder[A](schema: Schema[A]): JsonEncoder[A] = schema match {
    case Schema.Record(structure) =>
      val keyValueEncoders = structure.view.mapValues(encoder(_)).toMap
      CodecEncoder.recordEncoder(keyValueEncoders)
    case Schema.Sequence(element)        => JsonEncoder.chunk(encoder(element))
    case Schema.Enumeration(enumeration) => CodecEncoder.recordEncoder(enumeration.view.mapValues(encoder(_)).toMap)
    case Schema.Transform(_, _, _)       => ???
    case Schema.Primitive(standardType)  => CodecEncoder.primitiveEncoder(standardType)
    case Schema.Tuple(left, right)       => JsonEncoder.tuple2(encoder(left), encoder(right))
    case Schema.Optional(schema)         => JsonEncoder.option(encoder(schema))
  }

  override def decoder[A](schema: Schema[A]): JsonDecoder[A] = schema match {
    case Schema.Record(_)               => ???
    case Schema.Sequence(element)       => JsonDecoder.chunk(decoder(element))
    case Schema.Enumeration(_)          => ???
    case Schema.Transform(_, _, _)      => ???
    case Schema.Primitive(standardType) => CodecDecoder.primitiveDecoder(standardType)
    case Schema.Tuple(left, right)      => JsonDecoder.tuple2(decoder(left), decoder(right))
    case Schema.Optional(schema)        => JsonDecoder.option(decoder(schema))
  }
}
