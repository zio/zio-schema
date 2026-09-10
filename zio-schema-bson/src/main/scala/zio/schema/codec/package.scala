package zio.schema.codec

import org.bson.types.ObjectId

import zio.schema.{Schema, TypeId}

package object bson {
  val ObjectIdTag = "$oid"

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
