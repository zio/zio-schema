package zio.schema.codec

import zio.bson._
import zio.schema.annotation._
import zio.schema.{ DeriveSchema, Schema }

object MixedConfig {

  @noDiscriminator
  sealed trait NoDiscriminator

  object NoDiscriminator {
    case class A(a: String) extends NoDiscriminator
    case class B(b: String) extends NoDiscriminator
    case class C(c: String) extends NoDiscriminator

    implicit lazy val schema: Schema[NoDiscriminator]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[NoDiscriminator] = BsonSchemaCodec.bsonCodec(schema)
  }

  sealed trait WithoutDiscriminator

  object WithoutDiscriminator {
    case class A(s: String) extends WithoutDiscriminator
    case class B(s: String) extends WithoutDiscriminator

    implicit lazy val schema: Schema[WithoutDiscriminator]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[WithoutDiscriminator] = BsonSchemaCodec.bsonCodec(schema)
  }

  @discriminatorName("wrong")
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
    @caseName("wrong")
    case object A extends CaseNameEnumLike

    @caseName("wrong")
    @bsonHint("bName")
    case object B extends CaseNameEnumLike

    implicit lazy val schema: Schema[CaseNameEnumLike]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[CaseNameEnumLike] = BsonSchemaCodec.bsonCodec(schema)
  }

  sealed trait CaseNameWithoutDiscriminator

  object CaseNameWithoutDiscriminator {

    @caseName("wrong")
    @bsonHint("aName")
    case class A(s: String) extends CaseNameWithoutDiscriminator

    @bsonHint("bName")
    @caseName("wrong")
    case class B(s: String) extends CaseNameWithoutDiscriminator

    implicit lazy val schema: Schema[CaseNameWithoutDiscriminator]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[CaseNameWithoutDiscriminator] = BsonSchemaCodec.bsonCodec(schema)
  }

  @bsonDiscriminator("$type")
  @discriminatorName("wrong")
  sealed trait CaseNameWithDiscriminator

  object CaseNameWithDiscriminator {

    @bsonHint("aName")
    @caseName("wrong")
    case class A(s: String) extends CaseNameWithDiscriminator

    @bsonHint("bName")
    @caseName("wrong")
    case class B(s: String) extends CaseNameWithDiscriminator

    implicit lazy val schema: Schema[CaseNameWithDiscriminator]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[CaseNameWithDiscriminator] = BsonSchemaCodec.bsonCodec(schema)
  }

  sealed trait CaseNameAliasesWithoutDiscriminator

  object CaseNameAliasesWithoutDiscriminator {

    @bsonHint("aName")
    @caseNameAliases("aAlias1", "aAlias2")
    case class A(s: String) extends CaseNameAliasesWithoutDiscriminator

    @caseNameAliases("bAlias1", "bAlias2")
    case class B(s: String) extends CaseNameAliasesWithoutDiscriminator

    implicit lazy val schema: Schema[CaseNameAliasesWithoutDiscriminator]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[CaseNameAliasesWithoutDiscriminator] = BsonSchemaCodec.bsonCodec(schema)
  }

  @discriminatorName("wrong")
  @bsonDiscriminator("$type")
  sealed trait CaseNameAliasesWithDiscriminator

  object CaseNameAliasesWithDiscriminator {

    @caseNameAliases("aAlias1", "aAlias2")
    @bsonHint("aName")
    case class A(s: String) extends CaseNameAliasesWithDiscriminator

    @caseNameAliases("bAlias1", "bAlias2")
    case class B(s: String) extends CaseNameAliasesWithDiscriminator

    implicit lazy val schema: Schema[CaseNameAliasesWithDiscriminator]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[CaseNameAliasesWithDiscriminator] = BsonSchemaCodec.bsonCodec(schema)
  }

  case class FieldName(@fieldName("wrong") @bsonField("customName") a: String)

  object FieldName {
    implicit lazy val schema: Schema[FieldName]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[FieldName] = BsonSchemaCodec.bsonCodec(schema)
  }

  case class FieldDefaultValue(@fieldDefaultValue("defaultValue") a: String = "x")

  object FieldDefaultValue {
    implicit lazy val schema: Schema[FieldDefaultValue]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[FieldDefaultValue] = BsonSchemaCodec.bsonCodec(schema)
  }

  case class AllowExtraFields(a: String)

  object AllowExtraFields {
    implicit lazy val schema: Schema[AllowExtraFields]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[AllowExtraFields] = BsonSchemaCodec.bsonCodec(schema)
  }

  @rejectExtraFields
  @bsonNoExtraFields
  case class RejectExtraFields(a: String)

  object RejectExtraFields {
    implicit lazy val schema: Schema[RejectExtraFields]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[RejectExtraFields] = BsonSchemaCodec.bsonCodec(schema)
  }

  sealed trait TransientCaseWithoutDiscriminator

  object TransientCaseWithoutDiscriminator {
    case class A(s: String) extends TransientCaseWithoutDiscriminator
    case class B(s: String) extends TransientCaseWithoutDiscriminator

    @transientCase
    @bsonExclude
    case class C(s: String) extends TransientCaseWithoutDiscriminator

    implicit lazy val schema: Schema[TransientCaseWithoutDiscriminator]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[TransientCaseWithoutDiscriminator] = BsonSchemaCodec.bsonCodec(schema)
  }

  @discriminatorName("wrong")
  @bsonDiscriminator("$type")
  sealed trait TransientCaseWithDiscriminator

  object TransientCaseWithDiscriminator {
    case class A(s: String) extends TransientCaseWithDiscriminator
    case class B(s: String) extends TransientCaseWithDiscriminator

    @transientCase
    case class C(s: String) extends TransientCaseWithDiscriminator

    implicit lazy val schema: Schema[TransientCaseWithDiscriminator]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[TransientCaseWithDiscriminator] = BsonSchemaCodec.bsonCodec(schema)
  }

  case class TransientField(@bsonExclude @transientField @fieldDefaultValue("defaultValue") a: String, b: Int)

  object TransientField {
    implicit lazy val schema: Schema[TransientField]   = DeriveSchema.gen
    implicit lazy val codec: BsonCodec[TransientField] = BsonSchemaCodec.bsonCodec(schema)
  }
}
