package zio.schema

import zio.schema.GenUtil._
import zio.schema.SchemaGen._
import zio.test._

object DeriveGenSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[Environment, Failure] =
    suite("DeriveGenSpec")(
      testM("derive a generator for Schema.GenericRecord values") {
        checkGenSchema(anyGenericRecordAndGen.map(_._1))
      },
      testM("derive a generator for Schema.Sequence values") {
        checkSchema(schemaTransform)
      },
      testM("derive a generator for Schema.Enumeration values") {
        checkGenSchema(anyEnumerationAndGen.map(_._1))
      },
      testM("derive a generator for Schema.Transform values") {
        checkSchema(schemaTransform)
      },
      testM("derive a generator for Schema.Optional values") {
        checkSchema(schemaOptional)
      },
      testM("derive a generator for Schema.Tuple values") {
        checkSchema(schemaTuple)
      },
      testM("derive a generator for Schema.EitherSchema values") {
        checkSchema(schemaEither)
      },
      testM("derive a generator for Schema.CaseClass1 values") {
        checkGenSchema(Gen.const(Arity.arity1Schema))
      },
      testM("derive a generator for Schema.CaseClass2 values") {
        checkGenSchema(Gen.const(Arity.arity2Schema))
      },
      testM("derive a generator for Schema.CaseClass22 values") {
        checkGenSchema(Gen.const(Arity.highAritySchema))
      },
      testM("derive a generator for Schema.EnumN values") {
        checkGenSchema(Gen.const(Arity.arityEnumSchema))
      }
    )

  def schemaUnit: Schema.Primitive[Unit]         = Schema.Primitive(StandardType.UnitType)
  def schemaString: Schema.Primitive[String]     = Schema.Primitive(StandardType.StringType)
  def schemaTransform: Schema[String]            = schemaUnit.transform((_: Unit) => "unit", (_: String) => ())
  def schemaOptional: Schema[Option[Unit]]       = schemaUnit.optional
  def schemaTuple: Schema[(Unit, String)]        = schemaUnit <*> schemaString
  def schemaEither: Schema[Either[Unit, String]] = schemaUnit <+> schemaString
}
