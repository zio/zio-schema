package zio.schema.json

import zio.schema.DynamicValue

sealed trait JsonPatch {

  def apply(target: DynamicValue): Either[String, DynamicValue] = this match {
    case JsonPatch.Add(path, value) =>
      val key = path.stripPrefix("/")
      if (key == "") {
        Left("path cannot be empty")
      } else {
        target match {
          case DynamicValue.Record(id, fields) =>
            Right(DynamicValue.Record(id, fields.updated(key, value)))
          case _ =>
            Left("expected record")
        }
      }

    case JsonPatch.Remove(path) =>
      val key = path.stripPrefix("/")
      if (key == "") {
        Left("path cannot be empty")
      } else {
        target match {
          case DynamicValue.Record(id, fields) =>
            if (fields.contains(key)) {
              Right(DynamicValue.Record(id, fields - key))
            } else {
              Left(s"path $key not found")
            }
          case _ =>
            Left("expected record")
        }
      }
  }
}

object JsonPatch {
  final case class Add(path: String, value: DynamicValue) extends JsonPatch
  final case class Remove(path: String)                   extends JsonPatch
}
