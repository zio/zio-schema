package zio.schema.json

import zio.schema.DynamicValue

sealed trait JsonPatch {

  def apply(target: DynamicValue): Either[String, DynamicValue] = {
    val _ = target
    Left("Implementation missing")
  }
}

object JsonPatch {
  final case class Add(path: String, value: DynamicValue) extends JsonPatch
  final case class Remove(path: String)                   extends JsonPatch
}
