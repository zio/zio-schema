package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.prelude.Validation
import zio.schema.codec.DecodeError
import zio.test.Assertion._
import zio.test._

object DynamicValueValidationSpec extends ZIOSpecDefault {

  final case class AccountRecord(id: Int, username: String, active: Boolean)

  object AccountRecord {
    implicit val schema: Schema[AccountRecord] = DeriveSchema.gen[AccountRecord]
  }

  def spec: Spec[TestEnvironment, Any] =
    suite("DynamicValueValidationSpec")(
      test("accumulates multiple field errors when decoding a Record") {
        val malformed = DynamicValue.Record(
          TypeId.parse("zio.schema.DynamicValueValidationSpec.AccountRecord"),
          ListMap(
            "id"       -> DynamicValue.Primitive("invalid-int", StandardType.StringType),
            "username" -> DynamicValue.Primitive(404, StandardType.IntType),
            "active"   -> DynamicValue.Primitive("not-a-bool", StandardType.StringType)
          )
        )
        val validated = malformed.toTypedValueValidation(AccountRecord.schema)
        val errors    = validated.fold( errs => errs.toChunk, _ => Chunk.empty[String])
        assertTrue(errors.size == 3)
      },
      test("accumulates all element errors across Sequences and Sets") {
        val seqDyn = DynamicValue.Sequence(
          Chunk(
            DynamicValue.Primitive("bad-1", StandardType.StringType),
            DynamicValue.Primitive(10, StandardType.IntType),
            DynamicValue.Primitive("bad-2", StandardType.StringType)
          )
        )
        val validated = seqDyn.toValueValidation(Schema.chunk(Schema[Int]))
        val errors    = validated.fold(errs => errs.toChunk, _ => Chunk.empty[DecodeError])
        assertTrue(errors.size == 2)
      },
      test("accumulates both key and value errors across Dictionary entries") {
        val dictDyn = DynamicValue.Dictionary(
          Chunk(
            (
              DynamicValue.Primitive(100, StandardType.IntType),
              DynamicValue.Primitive("wrong-val", StandardType.StringType)
            )
          )
        )
        val validated = dictDyn.toValueValidation(Schema.map(Schema[String], Schema[Int]))
        val errors    = validated.fold(errs => errs.toChunk, _ => Chunk.empty[DecodeError])
        assertTrue(errors.size == 2)
      },
      test("succeeds with Validation.Success for valid nested DynamicValue") {
        val expected = AccountRecord(7, "kalopsia", active = true)
        val dynamic  = DynamicValue.fromSchemaAndValue(AccountRecord.schema, expected)
        assert(dynamic.toTypedValueValidation(AccountRecord.schema))(equalTo(Validation.succeed(expected)))
      }
    )
}
