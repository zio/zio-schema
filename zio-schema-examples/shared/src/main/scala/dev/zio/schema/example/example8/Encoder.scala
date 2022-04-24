package dev.zio.schema.example.example8

import zio.schema.{ DynamicValue, Schema, StandardType }

trait Encoder[A] {
  def encode(in: A): Json
}

object Encoder {

  def deriveEncoder[A](implicit schema: Schema[A]): Encoder[A] = { (in: A) =>
    toJson(schema.toDynamic(in))
  }

  private def toJson(in: DynamicValue): Json =
    in match {
      case p: DynamicValue.Primitive[a] =>
        p.standardType match {
          case StandardType.StringType =>
            Json.JStr(p.value)

          case StandardType.DoubleType | StandardType.IntType | StandardType.LongType =>
            Json.JNum(p.value)

          case StandardType.BoolType =>
            Json.JBool(p.value)

          case _ =>
            Json.JStr(p.value.toString)
        }

      case DynamicValue.Sequence(chunk) =>
        Json.JArr(chunk.map(toJson).toList)

      case DynamicValue.Record(values) =>
        Json.JObj(values.map { case (k, v) => (k, toJson(v)) })
    }
}
