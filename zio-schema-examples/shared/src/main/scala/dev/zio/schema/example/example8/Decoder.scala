package dev.zio.schema.example.example8

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.prelude._
import zio.schema._

trait Decoder[+A] {
  def decode(in: Json): Either[String, A]
}

object Decoder {

  def deriveDecoder[A](implicit schema: Schema[A]): Decoder[A] =
    in =>
      for {
        dv <- jsonToDynamicValue(in)
        a  <- schema.fromDynamic(dv)
      } yield a

  private def jsonToDynamicValue(in: Json): Either[String, DynamicValue] =
    in match {
      case Json.JStr(s) =>
        Right(DynamicValue.Primitive(s, StandardType.StringType))

      case Json.JNum(d) =>
        Right(DynamicValue.Primitive(d, StandardType.DoubleType))

      case Json.JBool(b) =>
        Right(DynamicValue.Primitive(b, StandardType.BoolType))

      case Json.JArr(as) =>
        as.forEach(jsonToDynamicValue)
          .map(list => DynamicValue.Sequence(Chunk.fromIterable(list)))

      case Json.JObj(map) =>
        map.map {
          case (k, v) => k -> jsonToDynamicValue(v)
        }.forEach(a => a)
          .map(map => DynamicValue.Record(ListMap.from(map)))
    }
}
