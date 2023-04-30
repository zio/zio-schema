package dev.zio.schema.example.example8

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.prelude._
import zio.schema._

trait Decoder[+A] {
  def decode(in: Json): zio.prelude.Validation[String, A]
}

object Decoder {

  def deriveDecoder[A](implicit schema: Schema[A]): Decoder[A] =
    in =>
      for {
        dv <- jsonToDynamicValue(in)
        a  <- schema.fromDynamic(dv)
      } yield a

  private def jsonToDynamicValue(in: Json): zio.prelude.Validation[String, DynamicValue] =
    in match {
      case Json.JStr(s) =>
        zio.prelude.Validation.succeed(DynamicValue.Primitive(s, StandardType.StringType))

      case Json.JNum(d) =>
        zio.prelude.Validation.succeed(DynamicValue.Primitive(d, StandardType.DoubleType))

      case Json.JBool(b) =>
        zio.prelude.Validation.succeed(DynamicValue.Primitive(b, StandardType.BoolType))

      case Json.JArr(as) =>
        as.forEach(jsonToDynamicValue)
          .map(list => DynamicValue.Sequence(Chunk.fromIterable(list)))

      case Json.JObj(map) =>
        map.map {
          case (k, v) => k -> jsonToDynamicValue(v)
        }.foldRight[zio.prelude.Validation[String, DynamicValue]](zio.prelude.Validation.succeed(DynamicValue.Record(TypeId.Structural, ListMap.empty))) {
          case ((key, zio.prelude.Validation.succeed(value)), zio.prelude.Validation.succeed(DynamicValue.Record(_, values))) =>
            zio.prelude.Validation.succeed(DynamicValue.Record(TypeId.parse(key), values + (key -> value)))
          case ((_, Left(err)), _) => Left(err)
          case (_, Left(err))      => Left(err)
        }
    }
}
