package zio.schema

import zio.Chunk
import zio.test.Gen

import scala.collection.immutable.ListMap

object StructureGen {

  def listMap[R](structure: Chunk[Schema.Field[_]])(gen: Schema[_] => Gen[R, _]): Gen[R, ListMap[String, _]] =
    structure.foldLeft[Gen[R, ListMap[String, _]]](Gen.const(ListMap.empty)) {
      case (genMap, field) =>
        for {
          map   <- genMap
          value <- gen(field.schema)
        } yield map.updated(field.label, value)
    }

  /*
  def tuple[R](structure: ListMap[String, Schema[_]])(gen: Schema[_] => Gen[R, _]): Gen[R, (String, _)] =
    structure
      .foldLeft[Gen[R, ListMap[String, _]]](Gen.const(ListMap.empty)) {
        case (genMap, (key, schema)) =>
          for {
            map   <- genMap
            value <- gen(schema)
          } yield map.updated(key, value)
      }
      .flatMap(listMap => Gen.elements(listMap.toList: _*))
 */
}
