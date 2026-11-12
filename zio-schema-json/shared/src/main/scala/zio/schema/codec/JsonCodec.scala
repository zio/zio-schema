package zio.schema.codec

import zio.json.{JsonDecoder, JsonEncoder}
import zio.schema.Schema

final case class JsonCodec[A](decoder: JsonDecoder[A], encoder: JsonEncoder[A]) extends Codec[String, A] {
  override def decode(s: String): Either[String, A] = {
    val trimmed = s.trim
    decoder.decodeJson(trimmed, 0).flatMap {
      case (value, consumed) =>
        if (consumed == trimmed.length) {
          Right(value)
        } else {
          Left(s"Unexpected trailing characters at position $consumed: '${trimmed.substring(consumed)}'")
        }
    }
  }

  override def encode(a: A): String = encoder.encodeJson(a, None).toString
}

object JsonCodec {
  def apply[A](schema: Schema[A]): JsonCodec[A] =
    JsonCodec(JsonDecoder.fromSchema(schema), JsonEncoder.fromSchema(schema))
}
