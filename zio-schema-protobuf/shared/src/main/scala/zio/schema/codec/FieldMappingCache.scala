package zio.schema.codec

import scala.collection.mutable

import zio.schema.Schema

/**
 * A per-encooding/decoding cache for field mappings. No need for thread safety as a single encoding/decoding
 * is sequential.
 */
class FieldMappingCache {
  private val mapping: mutable.Map[Schema[_], FieldMapping] = mutable.Map.empty

  def get(schema: Schema.Record[_]): FieldMapping =
    mapping.getOrElseUpdate(schema, FieldMapping.fromSchema(schema))
}

final case class FieldMapping(indexToFieldNumber: Map[Int, Int], fieldNumberToIndex: Map[Int, Int])

object FieldMapping {

  def fromSchema(schema: Schema.Record[_]): FieldMapping = {
    val indexToFieldNumber = schema.fields.zipWithIndex.map {
      case (field, index) => {
        val customFieldNumber = getFieldNumber(field)
        index -> customFieldNumber.getOrElse(index + 1)
      }
    }.toMap
    val fieldNumberToIndex = indexToFieldNumber.map(_.swap)
    FieldMapping(indexToFieldNumber, fieldNumberToIndex)
  }

  def getFieldNumber(field: Schema.Field[_, _]): Option[Int] =
    field.annotations.collectFirst {
      case fieldNumber(n) => n
    }
}
