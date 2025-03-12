package zio.schema.codec

import zio.bson._
import zio.schema.annotation.fieldDefaultValue
import zio.schema.{ DeriveSchema, Schema }

object BsonConfig {
  sealed trait WithoutDiscriminator

  object WithoutDiscriminator {
    case class A(s: String) extends WithoutDiscriminator
    case class B(s: String) extends WithoutDiscriminator

    implicit lazy val schema: Schema[WithoutDiscriminator]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[WithoutDiscriminator] = BsonSchemaCodec.bsonCodec(schema)
  }

  sealed trait WithClassNameTransformOptions

  object WithClassNameTransformOptions {
    case class A(s: String) extends WithClassNameTransformOptions
    case class B(s: String) extends WithClassNameTransformOptions

    implicit lazy val schema: Schema[WithClassNameTransformOptions] = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[WithClassNameTransformOptions] = BsonSchemaCodec.bsonCodec(
      schema,
      BsonSchemaCodec.Config.withClassNameMapping(
        classNameMapping = _.toLowerCase
      )
    )
  }

  sealed trait WithDiscriminatorOptions

  object WithDiscriminatorOptions {
    case class A(s: String) extends WithDiscriminatorOptions
    case class B(s: String) extends WithDiscriminatorOptions

    implicit lazy val schema: Schema[WithDiscriminatorOptions] = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[WithDiscriminatorOptions] = BsonSchemaCodec.bsonCodec(
      schema,
      BsonSchemaCodec.Config.withSumTypeHandling(
        sumTypeHandling = BsonSchemaCodec.SumTypeHandling.DiscriminatorField("type")
      )
    )
  }

  sealed trait WithoutDiscriminatorOptions

  object WithoutDiscriminatorOptions {
    case class A(s: String) extends WithoutDiscriminatorOptions
    case class B(s: String) extends WithoutDiscriminatorOptions

    implicit lazy val schema: Schema[WithoutDiscriminatorOptions] = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[WithoutDiscriminatorOptions] = BsonSchemaCodec.bsonCodec(
      schema,
      BsonSchemaCodec.Config.withSumTypeHandling(
        sumTypeHandling = BsonSchemaCodec.SumTypeHandling.WrapperWithClassNameField
      )
    )
  }

  @bsonDiscriminator("$type")
  sealed trait WithDiscriminator

  object WithDiscriminator {
    case class A(s: String) extends WithDiscriminator

    case class B(s: String) extends WithDiscriminator

    implicit lazy val schema: Schema[WithDiscriminator]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[WithDiscriminator] = BsonSchemaCodec.bsonCodec(schema)
  }

  sealed trait CaseNameEnumLike

  object CaseNameEnumLike {

    @bsonHint("aName")
    case object A extends CaseNameEnumLike

    @bsonHint("bName")
    case object B extends CaseNameEnumLike

    implicit lazy val schema: Schema[CaseNameEnumLike]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[CaseNameEnumLike] = BsonSchemaCodec.bsonCodec(schema)
  }

  sealed trait CaseNameWithoutDiscriminator

  object CaseNameWithoutDiscriminator {

    @bsonHint("aName")
    case class A(s: String) extends CaseNameWithoutDiscriminator

    @bsonHint("bName")
    case class B(s: String) extends CaseNameWithoutDiscriminator

    implicit lazy val schema: Schema[CaseNameWithoutDiscriminator]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[CaseNameWithoutDiscriminator] = BsonSchemaCodec.bsonCodec(schema)
  }

  @bsonDiscriminator("$type")
  sealed trait CaseNameWithDiscriminator

  object CaseNameWithDiscriminator {

    @bsonHint("aName")
    case class A(s: String) extends CaseNameWithDiscriminator

    @bsonHint("bName")
    case class B(s: String) extends CaseNameWithDiscriminator

    implicit lazy val schema: Schema[CaseNameWithDiscriminator]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[CaseNameWithDiscriminator] = BsonSchemaCodec.bsonCodec(schema)
  }

  case class FieldName(@bsonField("customName") a: String)

  object FieldName {
    implicit lazy val schema: Schema[FieldName]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[FieldName] = BsonSchemaCodec.bsonCodec(schema)
  }

  case class AllowExtraFields(a: String)

  object AllowExtraFields {
    implicit lazy val schema: Schema[AllowExtraFields]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[AllowExtraFields] = BsonSchemaCodec.bsonCodec(schema)
  }

  @bsonNoExtraFields
  case class RejectExtraFields(a: String)

  object RejectExtraFields {
    implicit lazy val schema: Schema[RejectExtraFields]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[RejectExtraFields] = BsonSchemaCodec.bsonCodec(schema)
  }

  case class TransientField(@bsonExclude @fieldDefaultValue("defaultValue") a: String, b: Int)

  object TransientField {
    implicit lazy val schema: Schema[TransientField]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[TransientField] = BsonSchemaCodec.bsonCodec(schema)
  }
}
