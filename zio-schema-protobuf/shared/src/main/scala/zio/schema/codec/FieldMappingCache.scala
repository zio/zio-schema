package zio.schema.codec

import scala.collection.mutable

import zio.schema.Schema

/**
 * A per-encoding/decoding cache for field mappings. No need for thread safety
 * as a single encoding/decoding is sequential.
 */
private class FieldMappingCache {
  private val mapping: mutable.Map[Schema[_], FieldMapping] = mutable.Map.empty

  def get(schema: Schema.Record[_]): FieldMapping =
    mapping.getOrElseUpdate(schema, FieldMapping.fromSchema(schema))
}

/**
 * The Protobuf field numbers of one record's fields: `indexToFieldNumber` maps
 * a field's 0-based position in the record to its field number, and
 * `fieldNumberToIndex` maps back.
 */
final case class FieldMapping(indexToFieldNumber: Map[Int, Int], fieldNumberToIndex: Map[Int, Int])

/** Builds [[FieldMapping]] values from record schemas. */
object FieldMapping {

  /**
   * Returns the mapping for `schema`: the field at position `i` gets the number
   * from its [[fieldNumber]] annotation, or `i + 1` without one. A record whose
   * second field of three is annotated `@fieldNumber(5)` maps positions 0, 1, 2
   * to 1, 5, 3.
   */
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

  /**
   * Returns the number from the field's [[fieldNumber]] annotation, or `None`
   * when it has none.
   */
  def getFieldNumber(field: Schema.Field[_, _]): Option[Int] =
    field.annotations.collectFirst { case fieldNumber(n) =>
      n
    }
}
