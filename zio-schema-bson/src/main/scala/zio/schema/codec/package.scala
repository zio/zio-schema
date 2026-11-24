package zio.schema.codec

import org.bson.types.ObjectId

import zio.schema.{Schema, TypeId}

package object bson {

  /**
   * The field name `\$oid` that MongoDB Extended JSON uses for an `ObjectId`,
   * and the name of the single field of `ObjectIdSchema`.
   */
  val ObjectIdTag = "$oid"

  /**
   * A `Schema` for `org.bson.types.ObjectId`: a record with the single string
   * field `\$oid` that holds the ObjectId's hex string. The `BsonSchemaCodec`
   * encoder writes it as a BSON ObjectId value, and the decoder reads it from a
   * BSON ObjectId value or from a document with a `\$oid` field.
   */
  implicit val ObjectIdSchema: Schema[ObjectId] =
    Schema.CaseClass1[String, ObjectId](
      id0 = TypeId.fromTypeName("ObjectId"),
      field0 = Schema.Field(
        name0 = ObjectIdTag,
        schema0 = Schema[String],
        get0 = _.toHexString,
        set0 = (_, idStr) => new ObjectId(idStr)
      ),
      defaultConstruct0 = new ObjectId(_)
    )

}
