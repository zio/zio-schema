package zio.schema.codec

import java.util.UUID

import scala.collection.immutable.ListMap
import scala.util.Try

import zio.schema.Schema._
import zio.schema._
import zio.schema.codec.AvroAnnotations.{ BytesType, DecimalType, FieldOrderType }
import zio.test.Assertion._
import zio.test._
import zio.{ Chunk, Scope }

object AvroSchemaCodecSpec extends ZIOSpecDefault {
  import AssertionHelper._
  import SpecTestData._

  override def spec: Spec[Environment with TestEnvironment with Scope, Any] =
    suite("AvroSchemaCodecSpec")(
      suite("encode")(
        suite("enum")(
          test("encodes string only enum as avro enum") {
            val caseA = Schema.Case[String, String](
              "A",
              Schema.primitive(StandardType.StringType),
              identity,
              identity,
              _.isInstanceOf[String]
            )
            val caseB = Schema.Case[String, String](
              "B",
              Schema.primitive(StandardType.StringType),
              identity,
              identity,
              _.isInstanceOf[String]
            )
            val caseC = Schema.Case[String, String](
              "C",
              Schema.primitive(StandardType.StringType),
              identity,
              identity,
              _.isInstanceOf[String]
            )
            val schema = Schema.Enum3(TypeId.Structural, caseA, caseB, caseC, Chunk(AvroAnnotations.name("MyEnum")))

            val result = AvroSchemaCodec.encode(schema)

            val expected = """{"type":"enum","name":"MyEnum","symbols":["A","B","C"]}"""
            assert(result)(isRight(equalTo(expected)))
          },
          test("encodes sealed trait objects only as union of records when no avroEnum annotation is present") {

            val schema = DeriveSchema.gen[SpecTestData.CaseObjectsOnlyAdt]
            val result = AvroSchemaCodec.encode(schema)

            val expected =
              """[{"type":"record","name":"A","fields":[]},{"type":"record","name":"B","fields":[]},{"type":"record","name":"MyC","fields":[]}]"""
            assert(result)(isRight(equalTo(expected)))
          },
          test("encodes sealed trait objects only as enum when avroEnum annotation is present") {

            val schema = DeriveSchema.gen[SpecTestData.CaseObjectsOnlyAdt].annotate(AvroAnnotations.avroEnum())
            val result = AvroSchemaCodec.encode(schema)

            val expected = """{"type":"enum","name":"MyEnum","symbols":["A","B","MyC"]}"""
            assert(result)(isRight(equalTo(expected)))
          },
          test("ignores avroEnum annotation if ADT cannot be reduced to String symbols") {
            val schema = DeriveSchema.gen[SpecTestData.CaseObjectAndCaseClassAdt]
            val result = AvroSchemaCodec.encode(schema)

            val expected =
              """[{"type":"record","name":"A","fields":[]},{"type":"record","name":"B","fields":[]},{"type":"record","name":"MyC","fields":[]},{"type":"record","name":"D","fields":[{"name":"s","type":"string"}]}]"""
            assert(result)(isRight(equalTo(expected)))
          },
          test("flatten nested unions with initialSchemaDerived derivation") {
            val schema = DeriveSchema.gen[SpecTestData.UnionWithNesting]
            val result = AvroSchemaCodec.encode(schema)

            val expected =
              """[{"type":"record","name":"A","fields":[]},{"type":"record","name":"B","fields":[]},{"type":"record","name":"MyC","fields":[]},{"type":"record","name":"D","fields":[{"name":"s","type":"string"}]}]"""
            assert(result)(isRight(equalTo(expected)))
          },
          test("wraps nested unions") {
            val schemaA = DeriveSchema.gen[UnionWithNesting.Nested.A.type]
            val schemaB = DeriveSchema.gen[UnionWithNesting.Nested.B.type]
            val schemaC = DeriveSchema.gen[UnionWithNesting.C.type]
            val schemaD = DeriveSchema.gen[UnionWithNesting.D]

            val nested: Enum2[UnionWithNesting.Nested.A.type, UnionWithNesting.Nested.B.type, UnionWithNesting.Nested] =
              Schema.Enum2(
                TypeId.Structural,
                Schema.Case[UnionWithNesting.Nested, UnionWithNesting.Nested.A.type](
                  "A",
                  schemaA,
                  _.asInstanceOf[UnionWithNesting.Nested.A.type],
                  _.asInstanceOf[UnionWithNesting.Nested],
                  _.isInstanceOf[UnionWithNesting.Nested.A.type]
                ),
                Schema.Case[UnionWithNesting.Nested, UnionWithNesting.Nested.B.type](
                  "B",
                  schemaB,
                  _.asInstanceOf[UnionWithNesting.Nested.B.type],
                  _.asInstanceOf[UnionWithNesting.Nested],
                  _.isInstanceOf[UnionWithNesting.Nested.B.type]
                )
              )
            val unionWithNesting = Schema.Enum3(
              TypeId.Structural,
              Schema.Case[UnionWithNesting, UnionWithNesting.Nested](
                "Nested",
                nested,
                _.asInstanceOf[UnionWithNesting.Nested],
                _.asInstanceOf[UnionWithNesting],
                _.isInstanceOf[UnionWithNesting.Nested]
              ),
              Schema
                .Case[UnionWithNesting, UnionWithNesting.C.type](
                  "C",
                  schemaC,
                  _.asInstanceOf[UnionWithNesting.C.type],
                  _.asInstanceOf[UnionWithNesting],
                  _.isInstanceOf[UnionWithNesting.C.type]
                ),
              Schema.Case[UnionWithNesting, UnionWithNesting.D](
                "D",
                schemaD,
                _.asInstanceOf[UnionWithNesting.D],
                _.asInstanceOf[UnionWithNesting],
                _.isInstanceOf[UnionWithNesting.D]
              )
            )

            val schema = unionWithNesting
            val result = AvroSchemaCodec.encode(schema)

            val wrappedString =
              """[{"type":"record","name":"wrapper_Nested","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":[{"type":"record","name":"A","namespace":"","fields":[]},{"type":"record","name":"B","namespace":"","fields":[]}]}],"zio.schema.codec.avro.wrapper":true},{"type":"record","name":"C","fields":[]},{"type":"record","name":"D","fields":[{"name":"s","type":"string"}]}]"""
            assert(result)(isRight(equalTo(wrappedString)))
          }
        ),
        suite("record")(
          test("generate a static name if not specified via annotation") {
            val schema1 = DeriveSchema.gen[SpecTestData.Record]
            val schema2 = DeriveSchema.gen[SpecTestData.Record]
            val result1 = AvroSchemaCodec.encode(schema1)
            val result2 = AvroSchemaCodec.encode(schema2)

            val expected =
              """{"type":"record","name":"hashed_1642816955","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            assert(result1)(isRight(equalTo(expected))) && assert(result2)(isRight(equalTo(expected)))
          } @@ TestAspect.ignore, // TODO: FIX
          test("fail with left on invalid name") {
            val schema = DeriveSchema.gen[SpecTestData.Record].annotate(AvroAnnotations.name("0invalid"))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isLeft(containsString("""0invalid""")))
          },
          test("pick up name from annotation") {
            val schema = DeriveSchema.gen[SpecTestData.NamedRecord]
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"record","name":"MyNamedRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
                )
              )
            )
          },
          test("pick up name from annotation for fields") {
            val schema = DeriveSchema.gen[SpecTestData.NamedFieldRecord]
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"record","name":"MyNamedFieldRecord","fields":[{"name":"myNamedField","type":"string"},{"name":"b","type":"boolean"}]}"""
                )
              )
            )
          },
          test("pick up doc from annotation") {
            val schema = DeriveSchema.gen[SpecTestData.NamedRecord].annotate(AvroAnnotations.doc("My doc"))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"record","name":"MyNamedRecord","doc":"My doc","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
                )
              )
            )
          },
          test("pick up namespace from annotation") {
            val schema =
              DeriveSchema.gen[SpecTestData.NamedRecord].annotate(AvroAnnotations.namespace("test.namespace"))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"record","name":"MyNamedRecord","namespace":"test.namespace","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
                )
              )
            )
          },
          test("fail with left on invalid namespace") {
            val schema = DeriveSchema.gen[SpecTestData.NamedRecord].annotate(AvroAnnotations.namespace("0@-.invalid"))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isLeft(containsString("""0@-.invalid""")))
          },
          test("pick up error annotation") {
            val schema = DeriveSchema.gen[SpecTestData.NamedRecord].annotate(AvroAnnotations.error)
            val result = AvroSchemaCodec.encode(schema)

            val expected =
              """{"type":"error","name":"MyNamedRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            assert(result)(isRight(equalTo(expected)))
          },
          test("includes all fields") {
            val schema = DeriveSchema.gen[SpecTestData.NamedRecord]
            val result = AvroSchemaCodec.encode(schema)

            val expected =
              """{"type":"record","name":"MyNamedRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            assert(result)(isRight(equalTo(expected)))
          },
          test("includes nested record fields") {
            val schema = DeriveSchema.gen[SpecTestData.NestedRecord]
            val result = AvroSchemaCodec.encode(schema)

            val expected =
              """{"type":"record","name":"NestedRecord","fields":[{"name":"s","type":"string"},{"name":"nested","type":{"type":"record","name":"MyNamedRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}}]}"""
            assert(result)(isRight(equalTo(expected)))
          }
        ),
        suite("map")(
          test("string keys and string values") {
            val keySchema   = Schema.primitive(StandardType.StringType)
            val valueSchema = Schema.primitive(StandardType.StringType)
            val schema      = Schema.Map(keySchema, valueSchema)
            val result      = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"map","values":"string"}""")))
          },
          test("string keys and complex values") {
            val keySchema   = Schema.primitive(StandardType.StringType)
            val valueSchema = DeriveSchema.gen[SpecTestData.SimpleRecord]
            val schema      = Schema.Map(keySchema, valueSchema)
            val result      = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"map","values":{"type":"record","name":"Simple","fields":[{"name":"s","type":"string"}]}}"""
                )
              )
            )
          },
          test("complex keys and string values") {
            val keySchema   = DeriveSchema.gen[SpecTestData.SimpleRecord]
            val valueSchema = Schema.primitive(StandardType.StringType)
            val schema      = Schema.Map(keySchema, valueSchema)
            val result      = AvroSchemaCodec.encode(schema)

            val isArray    = startsWithString("""{"type":"array"""")
            val tupleItems = containsString(""""items":{"type":"record","name":"Tuple","namespace":"scala"""")
            val hasTupleField_1 = containsString(
              """{"name":"_1","type":{"type":"record","name":"Simple","namespace":"","fields":[{"name":"s","type":"string"}]}}"""
            )
            val hasTupleField_2 = containsString("""{"name":"_2","type":"string"}""")

            assert(result)(isRight(isArray && tupleItems && hasTupleField_1 && hasTupleField_2))
          },
          test("complex keys and complex values") {
            val keySchema   = DeriveSchema.gen[SpecTestData.SimpleRecord]
            val valueSchema = DeriveSchema.gen[SpecTestData.NamedRecord]
            val schema      = Schema.Map(keySchema, valueSchema)
            val result      = AvroSchemaCodec.encode(schema)

            val isArray    = startsWithString("""{"type":"array"""")
            val tupleItems = containsString(""""items":{"type":"record","name":"Tuple","namespace":"scala"""")
            val hasTupleField_1 = containsString(
              """{"name":"_1","type":{"type":"record","name":"Simple","namespace":"","fields":[{"name":"s","type":"string"}]}}"""
            )
            val hasTupleField_2 = containsString(
              """{"name":"_2","type":{"type":"record","name":"MyNamedRecord","namespace":"","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}}"""
            )

            assert(result)(isRight(isArray && tupleItems && hasTupleField_1 && hasTupleField_2))
          }
        ),
        suite("seq")(
          test("is mapped to an avro array") {
            val schema = Schema.Sequence[Chunk[String], String, String](
              Schema.primitive(StandardType.StringType),
              identity,
              identity,
              Chunk.empty,
              "Seq"
            )
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"array","items":"string"}""")))
          },
          test("encodes complex types") {
            val valueSchema = DeriveSchema.gen[SpecTestData.NamedRecord]
            val schema = Schema
              .Sequence[Chunk[NamedRecord], NamedRecord, String](valueSchema, identity, identity, Chunk.empty, "Seq")
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"array","items":{"type":"record","name":"MyNamedRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}}"""
                )
              )
            )
          }
        ),
        suite("set")(
          test("is mapped to an avro array") {
            val schema = Schema.Sequence[Chunk[String], String, String](
              Schema.primitive(StandardType.StringType),
              identity,
              identity,
              Chunk.empty,
              "Seq"
            )
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"array","items":"string"}""")))
          },
          test("encodes complex types") {
            val valueSchema = DeriveSchema.gen[SpecTestData.NamedRecord]
            val schema = Schema
              .Sequence[Chunk[NamedRecord], NamedRecord, String](valueSchema, identity, identity, Chunk.empty, "Seq")
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"array","items":{"type":"record","name":"MyNamedRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}}"""
                )
              )
            )
          }
        ),
        suite("optional")(
          test("creates a union with case NULL") {
            val schema = Schema.Optional(Schema.primitive(StandardType.StringType))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""["null","string"]""")))
          },
          test("encodes complex types") {
            val valueSchema = DeriveSchema.gen[SpecTestData.NamedRecord]
            val schema      = Schema.Optional(valueSchema)
            val result      = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """["null",{"type":"record","name":"MyNamedRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}]"""
                )
              )
            )
          },
          test("wraps optional of unit to prevent duplicate null in union") {
            val schema = Schema.Optional(Schema.primitive(StandardType.UnitType))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """["null",{"type":"record","name":"wrapper_hashed_3594628","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":"null"}],"zio.schema.codec.avro.wrapper":true}]"""
                )
              )
            )
          },
          test("encodes nested optionals") {
            val nested = Schema.Optional(Schema.primitive(StandardType.StringType))
            val schema = Schema.Optional(nested)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """["null",{"type":"record","name":"wrapper_hashed_n813828848","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":["null","string"]}],"zio.schema.codec.avro.wrapper":true}]"""
                )
              )
            )
          },
          test("encodes optionals of union") {
            val union  = DeriveSchema.gen[SpecTestData.CaseObjectsOnlyAdt]
            val schema = Schema.Optional(union)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """["null",{"type":"record","name":"wrapper_MyEnum","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":[{"type":"record","name":"A","namespace":"","fields":[]},{"type":"record","name":"B","namespace":"","fields":[]},{"type":"record","name":"MyC","namespace":"","fields":[]}]}],"zio.schema.codec.avro.wrapper":true}]"""
                )
              )
            )
          },
          test("encodes optionals of either") {
            val either =
              Schema.Either(Schema.primitive(StandardType.StringType), Schema.primitive(StandardType.IntType))
            val schema = Schema.Optional(either)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """["null",{"type":"record","name":"wrapper_hashed_n630422444","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":["string","int"]}],"zio.schema.codec.avro.either":true}]"""
                )
              )
            )
          }
        ),
        suite("either")(
          test("create an union") {
            val schema =
              Schema.Either(Schema.primitive(StandardType.StringType), Schema.primitive(StandardType.IntType))
            val result = AvroSchemaCodec.encode(schema)

            val expected =
              """{"type":"record","name":"wrapper_hashed_n630422444","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":["string","int"]}],"zio.schema.codec.avro.either":true}"""
            assert(result)(isRight(equalTo(expected)))
          },
          test("create a named union") {
            val schema = Schema
              .Either(Schema.primitive(StandardType.StringType), Schema.primitive(StandardType.IntType))
              .annotate(AvroAnnotations.name("MyEither"))
            val result = AvroSchemaCodec.encode(schema)

            val expected =
              """{"type":"record","name":"wrapper_MyEither","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":["string","int"]}],"zio.schema.codec.avro.either":true}"""
            assert(result)(isRight(equalTo(expected)))
          },
          test("encodes complex types") {
            val left   = DeriveSchema.gen[SpecTestData.SimpleRecord]
            val right  = Schema.primitive(StandardType.StringType)
            val schema = Schema.Either(left, right)
            val result = AvroSchemaCodec.encode(schema)

            val expected =
              """{"type":"record","name":"wrapper_hashed_754352222","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":[{"type":"record","name":"Simple","namespace":"","fields":[{"name":"s","type":"string"}]},"string"]}],"zio.schema.codec.avro.either":true}"""
            assert(result)(isRight(equalTo(expected)))
          },
          test("fails with duplicate names") {
            val left   = DeriveSchema.gen[SpecTestData.SimpleRecord]
            val right  = DeriveSchema.gen[SpecTestData.SimpleRecord]
            val schema = Schema.Either(left, right)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isLeft(equalTo("""Left and right schemas of either must have different fullnames: Simple"""))
            )
          },
          test("encodes either containing optional") {
            val left   = Schema.Optional(Schema.primitive(StandardType.StringType))
            val right  = Schema.primitive(StandardType.StringType)
            val schema = Schema.Either(left, right)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"record","name":"wrapper_hashed_n465006219","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":[{"type":"record","name":"wrapper_hashed_n813828848","fields":[{"name":"value","type":["null","string"]}],"zio.schema.codec.avro.wrapper":true},"string"]}],"zio.schema.codec.avro.either":true}"""
                )
              )
            )
          },
          test("encodes nested either") {
            val left   = Schema.Optional(Schema.primitive(StandardType.StringType))
            val right  = Schema.primitive(StandardType.StringType)
            val nested = Schema.Either(left, right)
            val schema = Schema.Either(nested, right)
            val result = AvroSchemaCodec.encode(schema)

            val expected =
              """{"type":"record","name":"wrapper_hashed_2071802344","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":[{"type":"record","name":"wrapper_hashed_n465006219","fields":[{"name":"value","type":[{"type":"record","name":"wrapper_hashed_n813828848","fields":[{"name":"value","type":["null","string"]}],"zio.schema.codec.avro.wrapper":true},"string"]}],"zio.schema.codec.avro.either":true},"string"]}],"zio.schema.codec.avro.either":true}"""
            assert(result)(isRight(equalTo(expected)))
          }
        ),
        suite("tuple")(
          test("creates a record type and applies the name") {
            val left   = Schema.primitive(StandardType.StringType)
            val right  = Schema.primitive(StandardType.StringType)
            val schema = Schema.Tuple2(left, right).annotate(AvroAnnotations.name("MyTuple"))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"record","name":"MyTuple","fields":[{"name":"_1","type":"string"},{"name":"_2","type":"string"}],"zio.schema.codec.recordType":"tuple"}"""
                )
              )
            )
          },
          test("encodes complex types") {
            val left   = DeriveSchema.gen[SpecTestData.SimpleRecord]
            val right  = DeriveSchema.gen[SpecTestData.NamedRecord]
            val schema = Schema.Tuple2(left, right).annotate(AvroAnnotations.name("MyTuple"))
            val result = AvroSchemaCodec.encode(schema)

            val field_1 =
              """{"name":"_1","type":{"type":"record","name":"Simple","fields":[{"name":"s","type":"string"}]}}"""
            val field_2 =
              """{"name":"_2","type":{"type":"record","name":"MyNamedRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}}"""
            assert(result)(isRight(containsString(field_1) && containsString(field_2)))
          },
          test("encodes duplicate complex types by reference") {
            val left   = DeriveSchema.gen[SpecTestData.SimpleRecord]
            val right  = DeriveSchema.gen[SpecTestData.SimpleRecord]
            val schema = Schema.Tuple2(left, right).annotate(AvroAnnotations.name("MyTuple"))
            val result = AvroSchemaCodec.encode(schema)

            val field_1 =
              """{"name":"_1","type":{"type":"record","name":"Simple","fields":[{"name":"s","type":"string"}]}}"""
            val field_2 = """{"name":"_2","type":"Simple"}"""
            assert(result)(isRight(containsString(field_1) && containsString(field_2)))
          }
        ),
        suite("primitives")(
          test("encodes UnitType") {
            val schema = Schema.primitive(StandardType.UnitType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("\"null\"")))
          },
          test("encodes StringType") {
            val schema = Schema.primitive(StandardType.StringType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("\"string\"")))
          },
          test("encodes BooleanType") {
            val schema = Schema.primitive(StandardType.BoolType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("\"boolean\"")))
          },
          test("encodes ShortType") {
            val schema = Schema.primitive(StandardType.ShortType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"int","zio.schema.codec.intType":"short"}""")))
          },
          test("encodes IntType") {
            val schema = Schema.primitive(StandardType.IntType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("\"int\"")))
          },
          test("encodes LongType") {
            val schema = Schema.primitive(StandardType.LongType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("\"long\"")))
          },
          test("encodes FloatType") {
            val schema = Schema.primitive(StandardType.FloatType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("\"float\"")))
          },
          test("encodes DoubleType") {
            val schema = Schema.primitive(StandardType.DoubleType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("\"double\"")))
          },
          test("encodes BinaryType as bytes") {
            val schema = Schema.primitive(StandardType.BinaryType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("\"bytes\"")))
          },
          test("encodes BinaryType as fixed") {
            val size = 12
            val schema =
              Schema
                .primitive(StandardType.BinaryType)
                .annotate(AvroAnnotations.bytes(BytesType.Fixed(size, "MyFixed")))
            val result = AvroSchemaCodec.encode(schema)

            val expected = """{"type":"fixed","name":"MyFixed","doc":"","size":12}"""
            assert(result)(isRight(equalTo(expected)))
          },
          test("encodes CharType") {
            val schema = Schema.primitive(StandardType.CharType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"int","zio.schema.codec.intType":"char"}""")))
          },
          test("encodes UUIDType") {
            val schema = Schema.primitive(StandardType.UUIDType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"string","logicalType":"uuid"}""")))
          },
          test("encodes BigDecimalType as Bytes") {
            val schema =
              Schema.primitive(StandardType.BigDecimalType).annotate(AvroAnnotations.decimal(DecimalType.Bytes))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"bytes","logicalType":"decimal","precision":48,"scale":24}""")))
          },
          test("encodes BigDecimalType as Bytes with scala and precision") {
            val schema = Schema
              .primitive(StandardType.BigDecimalType)
              .annotate(AvroAnnotations.decimal(DecimalType.Bytes))
              .annotate(AvroAnnotations.scale(10))
              .annotate(AvroAnnotations.precision(20))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"bytes","logicalType":"decimal","precision":20,"scale":10}""")))
          },
          test("encodes BigDecimalType as Fixed") {
            val schema =
              Schema.primitive(StandardType.BigDecimalType).annotate(AvroAnnotations.decimal(DecimalType.Fixed(21)))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"fixed","name":"Decimal_48_24","size":21,"logicalType":"decimal","precision":48,"scale":24}"""
                )
              )
            )
          },
          test("encodes BigDecimalType as Fixed with scala and precision") {
            val schema = Schema
              .primitive(StandardType.BigDecimalType)
              .annotate(AvroAnnotations.decimal(DecimalType.Fixed(9)))
              .annotate(AvroAnnotations.scale(10))
              .annotate(AvroAnnotations.precision(20))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"fixed","name":"Decimal_20_10","size":9,"logicalType":"decimal","precision":20,"scale":10}"""
                )
              )
            )
          },
          test("encodes BigIntegerType as Bytes") {
            val schema =
              Schema.primitive(StandardType.BigIntegerType).annotate(AvroAnnotations.decimal(DecimalType.Bytes))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"bytes","logicalType":"decimal","precision":24,"scale":24}""")))
          },
          test("encodes BigIntegerType as Bytes with scala and precision") {
            val schema = Schema
              .primitive(StandardType.BigIntegerType)
              .annotate(AvroAnnotations.decimal(DecimalType.Bytes))
              .annotate(AvroAnnotations.scale(10))
              .annotate(AvroAnnotations.precision(20))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"bytes","logicalType":"decimal","precision":10,"scale":10}""")))
          },
          test("encodes BigIntegerType as Fixed") {
            val schema =
              Schema.primitive(StandardType.BigIntegerType).annotate(AvroAnnotations.decimal(DecimalType.Fixed(11)))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"fixed","name":"Decimal_24_24","size":11,"logicalType":"decimal","precision":24,"scale":24}"""
                )
              )
            )
          },
          test("encodes BigIntegerType as Fixed with scala and precision") {
            val schema = Schema
              .primitive(StandardType.BigIntegerType)
              .annotate(AvroAnnotations.decimal(DecimalType.Fixed(5)))
              .annotate(AvroAnnotations.scale(10))
              .annotate(AvroAnnotations.precision(20))
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"fixed","name":"Decimal_10_10","size":5,"logicalType":"decimal","precision":10,"scale":10}"""
                )
              )
            )
          },
          test("encodes DayOfWeekType") {
            val schema = Schema.primitive(StandardType.DayOfWeekType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"int","zio.schema.codec.intType":"dayOfWeek"}""")))
          },
          test("encodes MonthType") {
            val schema = Schema.primitive(StandardType.MonthType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"int","zio.schema.codec.intType":"month"}""")))
          },
          test("encodes YearType") {
            val schema = Schema.primitive(StandardType.YearType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"int","zio.schema.codec.intType":"year"}""")))
          },
          test("encodes ZoneIdType") {
            val schema = Schema.primitive(StandardType.ZoneIdType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"string","zio.schema.codec.stringType":"zoneId"}""")))
          },
          test("encodes ZoneOffsetType") {
            val schema = Schema.primitive(StandardType.ZoneOffsetType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(isRight(equalTo("""{"type":"int","zio.schema.codec.intType":"zoneOffset"}""")))
          },
          //TODO 1
          //test("encodes MonthDayType") {
          //  val schema = Schema.primitive(StandardType.MonthDayType)
          //  val result = AvroSchemaCodec.encode(schema)

          //  assert(result)(
          //    isRight(
          //      equalTo(
          //        """{"type":"record","name":"MonthDay","namespace":"zio.schema.codec.avro","fields":[{"name":"month","type":"int"},{"name":"day","type":"int"}],"zio.schema.codec.recordType":"monthDay"}"""
          //      )
          //    )
          //  )
          //},
          //TODO 2
          //test("encodes PeriodType") {
          //  val schema = Schema.primitive(StandardType.PeriodType)
          //  val result = AvroSchemaCodec.encode(schema)
          //
          //  assert(result)(
          //    isRight(
          //      equalTo(
          //        """{"type":"record","name":"Period","namespace":"zio.schema.codec.avro","fields":[{"name":"years","type":"int"},{"name":"months","type":"int"},{"name":"days","type":"int"}],"zio.schema.codec.recordType":"period"}"""
          //      )
          //    )
          //  )
          //},
          //TODO 3
          //test("encodes YearMonthType") {
          //  val schema = Schema.primitive(StandardType.YearMonthType)
          //  val result = AvroSchemaCodec.encode(schema)
          //
          //  assert(result)(
          //    isRight(
          //      equalTo(
          //        """{"type":"record","name":"YearMonth","namespace":"zio.schema.codec.avro","fields":[{"name":"year","type":"int"},{"name":"month","type":"int"}],"zio.schema.codec.recordType":"yearMonth"}"""
          //      )
          //    )
          //  )
          //},
          //TODO 4
          //test("encodes Duration") {
          //  val schema = Schema.primitive(StandardType.DurationType) // .duration(ChronoUnit.DAYS))
          //  val result = AvroSchemaCodec.encode(schema)
          //
          //  assert(result)(
          //    isRight(
          //      equalTo(
          //        """{"type":"record","name":"Duration","namespace":"zio.schema.codec.avro","fields":[{"name":"seconds","type":"long"},{"name":"nanos","type":"int"}],"zio.schema.codec.recordType":"duration","zio.schema.codec.avro.durationChronoUnit":"DAYS"}"""
          //      )
          //    )
          //  )
          //},
          test("encodes InstantType as string") {
            val schema = Schema.primitive(StandardType.InstantType).annotate(AvroAnnotations.formatToString)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"string","zio.schema.codec.stringType":"instant"}"""
                )
              )
            )
          },
          test("encodes LocalDateType as string") {
            val schema =
              Schema.primitive(StandardType.LocalDateType).annotate(AvroAnnotations.formatToString)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"string","zio.schema.codec.stringType":"localDate"}"""
                )
              )
            )
          },
          test("encodes LocalTimeType as string") {
            val schema =
              Schema.primitive(StandardType.LocalTimeType).annotate(AvroAnnotations.formatToString)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"string","zio.schema.codec.stringType":"localTime"}"""
                )
              )
            )
          },
          test("encodes LocalDateTimeType as string") {
            val schema =
              Schema.primitive(StandardType.LocalDateTimeType).annotate(AvroAnnotations.formatToString)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"string","zio.schema.codec.stringType":"localDateTime"}"""
                )
              )
            )
          },
          test("encodes OffsetTimeType") {
            val schema = Schema.primitive(StandardType.OffsetTimeType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"string","zio.schema.codec.stringType":"offsetTime"}"""
                )
              )
            )
          },
          test("encodes OffsetDateTimeType") {
            val schema = Schema.primitive(StandardType.OffsetDateTimeType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"string","zio.schema.codec.stringType":"offsetDateTime"}"""
                )
              )
            )
          },
          test("encodes ZonedDateTimeType") {
            val schema = Schema.primitive(StandardType.ZonedDateTimeType)
            val result = AvroSchemaCodec.encode(schema)

            assert(result)(
              isRight(
                equalTo(
                  """{"type":"string","zio.schema.codec.stringType":"zoneDateTime"}"""
                )
              )
            )
          }
        ),
        test("fail should fail the encode") {
          val schema = Schema.fail("I'm failing")
          val result = AvroSchemaCodec.encode(schema)

          assert(result)(isLeft(equalTo("""I'm failing""")))
        },
        test("lazy is handled properly") {
          val schema = Schema.Lazy(() => Schema.primitive(StandardType.StringType))
          val result = AvroSchemaCodec.encode(schema)

          assert(result)(isRight(equalTo("\"string\"")))
        }
      ),
      /**
       * Test Decoder
       */
      suite("decode")(
        suite("record")(
          test("decode a simple record") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema.map(_.ast))(
              isRight(
                equalTo(
                  Schema
                    .record(
                      TypeId.fromTypeName("TestRecord"),
                      Schema.Field(
                        "s",
                        Schema.primitive(StandardType.StringType),
                        get0 = (p: ListMap[String, _]) => p("s").asInstanceOf[String],
                        set0 = (p: ListMap[String, _], v: String) => p.updated("s", v)
                      ),
                      Schema.Field(
                        "b",
                        Schema.primitive(StandardType.BoolType),
                        get0 = (p: ListMap[String, _]) => p("b").asInstanceOf[Boolean],
                        set0 = (p: ListMap[String, _], v: Boolean) => p.updated("b", v)
                      )
                    )
                    .ast
                )
              )
            )
          },
          test("decode a nested record") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"nested","type":{"type":"record","name":"Inner","fields":[{"name":"innerS","type":"string"}]}},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))
            val expectedSchema = Schema.record(
              TypeId.fromTypeName("TestRecord"),
              Schema.Field(
                "nested",
                Schema.record(
                  TypeId.fromTypeName("Inner"),
                  Schema.Field(
                    "innerS",
                    Schema.primitive(StandardType.StringType),
                    get0 = (p: ListMap[String, _]) => p("innerS").asInstanceOf[String],
                    set0 = (p: ListMap[String, _], v: String) => p.updated("innerS", v)
                  )
                ),
                get0 = (p: ListMap[String, _]) => p("nested").asInstanceOf[ListMap[String, _]],
                set0 = (p: ListMap[String, _], v: ListMap[String, _]) => p.updated("nested", v)
              ),
              Schema.Field(
                "b",
                Schema.primitive(StandardType.BoolType),
                get0 = (p: ListMap[String, _]) => p("b").asInstanceOf[Boolean],
                set0 = (p: ListMap[String, _], v: Boolean) => p.updated("b", v)
              )
            )

            assert(schema.map(_.ast))(isRight(equalTo(expectedSchema.ast)))
          },
          test("unwrap a wrapped initialSchemaDerived") {
            val s =
              """{"type":"record","zio.schema.codec.avro.wrapper":true,"name":"wrapper_xyz","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":{"type":"record","name":"TestRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema.map(_.ast))(
              isRight(
                equalTo(
                  Schema
                    .record(
                      TypeId.fromTypeName("TestRecord"),
                      Schema.Field(
                        "s",
                        Schema.primitive(StandardType.StringType),
                        get0 = (p: ListMap[String, _]) => p("s").asInstanceOf[String],
                        set0 = (p: ListMap[String, _], v: String) => p.updated("s", v)
                      ),
                      Schema.Field(
                        "b",
                        Schema.primitive(StandardType.BoolType),
                        get0 = (p: ListMap[String, _]) => p("b").asInstanceOf[Boolean],
                        set0 = (p: ListMap[String, _], v: Boolean) => p.updated("b", v)
                      )
                    )
                    .ast
                )
              )
            )
          },
          test("period record") {
            val s =
              """{"type":"record","name":"Period","namespace":"zio.schema.codec.avro","fields":[{"name":"years","type":"int"},{"name":"months","type":"int"},{"name":"days","type":"int"}],"zio.schema.codec.recordType":"period"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.PeriodType)))
          },
          test("yearMonth record") {
            val s =
              """{"type":"record","name":"YearMonth","namespace":"zio.schema.codec.avro","fields":[{"name":"year","type":"int"},{"name":"month","type":"int"}],"zio.schema.codec.recordType":"yearMonth"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.YearMonthType)))
          },
          test("tuple record successful") {
            val s =
              """{"type":"record","name":"Tuple","namespace":"zio.schema.codec.avro","fields":[{"name":"_1","type":"string"},{"name":"_2","type":"int"}],"zio.schema.codec.recordType":"tuple"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(
              isRight(isTuple(isStandardType(StandardType.StringType), isStandardType(StandardType.IntType)))
            )
          },
          test("tuple record failing") {
            val s =
              """{"type":"record","name":"Tuple","namespace":"zio.schema.codec.avro","fields":[{"name":"_1","type":"string"},{"name":"_2","type":"int"},{"name":"_3","type":"int"}],"zio.schema.codec.recordType":"tuple"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isLeft)
          },
          test("monthDay record") {
            val s =
              """{"type":"record","name":"MonthDay","namespace":"zio.schema.codec.avro","fields":[{"name":"month","type":"int"},{"name":"day","type":"int"}],"zio.schema.codec.recordType":"monthDay"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.MonthDayType)))
          },
          test("duration record without chrono unit annotation") {
            val s =
              """{"type":"record","name":"Duration","namespace":"zio.schema.codec.avro","fields":[{"name":"seconds","type":"long"},{"name":"nanos","type":"int"}],"zio.schema.codec.recordType":"duration"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.DurationType))) //(ChronoUnit.MILLIS))))
          },
          test("duration record chrono unit annotation") {
            val s =
              """{"type":"record","name":"Duration","namespace":"zio.schema.codec.avro","fields":[{"name":"seconds","type":"long"},{"name":"nanos","type":"int"}],"zio.schema.codec.recordType":"duration","zio.schema.codec.avro.durationChronoUnit":"DAYS"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.DurationType))) //(ChronoUnit.DAYS))))
          },
          test("assign the name annotation") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isRecord(hasNameAnnotation(equalTo("TestRecord")))))
          },
          test("assign the namespace annotation") {
            val s =
              """{"type":"record","name":"TestRecord","namespace":"MyTest","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isRecord(hasNamespaceAnnotation(equalTo("MyTest")))))
          },
          test("not assign the namespace annotation if missing") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isRecord(hasNamespaceAnnotation(anything).negate)))
          },
          zio.test.test("assign the doc annotation") {
            val s =
              """{"type":"record","name":"TestRecord","doc":"Very helpful documentation!","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isRecord(hasDocAnnotation(equalTo("Very helpful documentation!")))))
          },
          test("not assign the doc annotation if missing") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isRecord(hasDocAnnotation(anything).negate)))
          },
          test("assign the aliases annotation") {
            val s =
              """{"type":"record","name":"TestRecord","aliases":["wow", "cool"],"fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(
              isRight(isRecord(hasAliasesAnnotation(exists[String](equalTo("wow")) && exists(equalTo("cool")))))
            )
          },
          test("not assign the aliases annotation if missing") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isRecord(hasAliasesAnnotation(anything).negate)))
          },
          test("not assign the aliases annotation if empty") {
            val s =
              """{"type":"record","name":"TestRecord","aliases":[],"fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isRecord(hasAliasesAnnotation(anything).negate)))
          },
          zio.test.test("assign the error annotation") {
            val s =
              """{"type":"error","name":"MyNamedRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isRecord(hasErrorAnnotation)))
          },
          test("not assign the error annotation if not an error") {
            val s =
              """{"type":"record","name":"TestRecord","aliases":[],"fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isRecord(hasErrorAnnotation.negate)))
          }
        ),
        suite("fields")(
          test("decodes primitive fields of record") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val field1 = hasRecordField(hasLabel(equalTo("s")) && hasSchema(isStandardType(StandardType.StringType)))
            val field2 = hasRecordField(hasLabel(equalTo("b")) && hasSchema(isStandardType(StandardType.BoolType)))
            assert(schema)(isRight(isRecord(field1 && field2)))
          },
          test("decodes the fields complex initialSchemaDerived") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"complex","type":{"type":"record","name":"Complex","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val field1  = hasRecordField(hasLabel(equalTo("s")) && hasSchema(isStandardType(StandardType.StringType)))
            val field2  = hasRecordField(hasLabel(equalTo("b")) && hasSchema(isStandardType(StandardType.BoolType)))
            val complex = isRecord(field1 && field2)
            val field   = hasRecordField(hasLabel(equalTo("complex")) && hasSchema(complex))
            assert(schema)(isRight(isRecord(field)))
          },
          zio.test.test("assign the field name annotation") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val field1 = hasRecordField(hasLabel(equalTo("s")) && hasNameAnnotation(equalTo("s")))
            val field2 = hasRecordField(hasLabel(equalTo("b")) && hasNameAnnotation(equalTo("b")))
            assert(schema)(isRight(isRecord(field1 && field2)))
          },
          zio.test.test("assign the field doc annotation iff it exists") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"s","doc":"Very helpful doc!","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val field1 = hasRecordField(hasLabel(equalTo("s")) && hasDocAnnotation(equalTo("Very helpful doc!")))
            val field2 = hasRecordField(hasLabel(equalTo("b")) && hasDocAnnotation(anything).negate)
            assert(schema)(isRight(isRecord(field1 && field2)))
          },
          test("assign the field default annotation") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"s","default":"defaultValue","type":"string"},{"name":"complex","default":{"s":"defaultS","b":true},"type":{"type":"record","name":"Complex","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]}},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val field1 = hasRecordField(hasLabel(equalTo("s")) && hasFieldDefaultAnnotation(equalTo("defaultValue")))
            val field2 = hasRecordField(
              hasLabel(equalTo("complex")) && hasFieldDefaultAnnotation(asString(equalTo("""{s=defaultS, b=true}""")))
            )
            val field3 = hasRecordField(hasLabel(equalTo("b")) && hasFieldDefaultAnnotation(anything).negate)
            assert(schema)(isRight(isRecord(field1 && field2 && field3)))
          },
          zio.test.test("assign the fieldOrder annotation") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"s","order":"descending","type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val field1 = hasRecordField(
              hasLabel(equalTo("s")) && hasFieldOrderAnnotation(equalTo(AvroAnnotations.FieldOrderType.Descending))
            )
            val field2 = hasRecordField(
              hasLabel(equalTo("b")) && hasFieldOrderAnnotation(equalTo(AvroAnnotations.FieldOrderType.Ascending))
            )
            assert(schema)(isRight(isRecord(field1 && field2)))
          },
          zio.test.test("assign the field aliases annotation") {
            val s =
              """{"type":"record","name":"TestRecord","fields":[{"name":"s","aliases":["wow", "cool"],"type":"string"},{"name":"b","type":"boolean"}]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val field1 = hasRecordField(
              hasLabel(equalTo("s")) && hasAliasesAnnotation(Assertion.hasSameElements(Seq("wow", "cool")))
            )
            val field2 = hasRecordField(hasLabel(equalTo("b")) && hasAliasesAnnotation(anything).negate)
            assert(schema)(isRight(isRecord(field1 && field2)))
          }
        ),
        suite("enum")(
          test("decodes symbols as union of strings") {
            val s      = """{"type":"enum","name":"TestEnum","symbols":["a","b","c"]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val symbolKeysAssetion = Assertion.hasKeys(hasSameElements(Seq("a", "b", "c")))
            val enumStringTypeAssertion: Assertion[ListMap[String, (Schema[_], Chunk[Any])]] =
              Assertion.hasValues(forall(tuple2First(isStandardType(StandardType.StringType))))
            assert(schema)(isRight(isEnum(enumStructure(symbolKeysAssetion && enumStringTypeAssertion))))
          },
          test("assign the enum name annotation") {
            val s      = """{"type":"enum","name":"TestEnum","symbols":["a","b","c"]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isEnum(hasNameAnnotation(equalTo("TestEnum")))))
          },
          test("assign the enum namespace annotation") {
            val s      = """{"type":"enum","name":"TestEnum","namespace":"MyTest","symbols":["a","b","c"]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isEnum(hasNamespaceAnnotation(equalTo("MyTest")))))
          },
          test("not assign the enum namespace annotation if empty") {
            val s      = """{"type":"enum","name":"TestEnum","symbols":["a","b","c"]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isEnum(hasNamespaceAnnotation(anything).negate)))
          },
          test("assign the enum aliases annotation") {
            val s      = """{"type":"enum","name":"TestEnum","aliases":["MyAlias", "MyAlias2"],"symbols":["a","b","c"]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isEnum(hasAliasesAnnotation(hasSameElements(Seq("MyAlias", "MyAlias2"))))))
          },
          test("not assign the enum aliases annotation if empty") {
            val s      = """{"type":"enum","name":"TestEnum","symbols":["a","b","c"]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isEnum(hasAliasesAnnotation(anything).negate)))
          },
          test("assign the enum doc annotation") {
            val s =
              """{"type":"enum","name":"TestEnum","doc":"Some very helpful documentation!","symbols":["a","b","c"]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isEnum(hasDocAnnotation(equalTo("Some very helpful documentation!")))))
          },
          test("not assign the enum doc annotation if empty") {
            val s      = """{"type":"enum","name":"TestEnum","symbols":["a","b","c"]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isEnum(hasAliasesAnnotation(anything).negate)))
          },
          test("assign the enum default annotation") {
            val s      = """{"type":"enum","name":"TestEnum","default":"a","symbols":["a","b","c"]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isEnum(hasDefaultAnnotation(equalTo("a")))))
          },
          test("fail if enum default is not a symbol") {
            val s      = """{"type":"enum","name":"TestEnum","default":"d","symbols":["a","b","c"]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isLeft(equalTo("The Enum Default: d is not in the enum symbol set: [a, b, c]")))
          },
          test("not assign the enum default annotation if empty") {
            val s      = """{"type":"enum","name":"TestEnum","symbols":["a","b","c"]}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isEnum(hasDefaultAnnotation(anything).negate)))
          }
        ),
        test("decodes primitive array") {
          val s      = """{"type":"array","items":{"type":"int"}}"""
          val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

          assert(schema)(isRight(isSequence(hasSequenceElementSchema(isStandardType(StandardType.IntType)))))
        },
        test("decodes complex array") {
          val s =
            """{"type":"array","items":{"type":"record","name":"TestRecord","fields":[{"name":"f1","type":"int"},{"name":"f2","type":"string"}]}}"""
          val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

          assert(schema)(isRight(isSequence(hasSequenceElementSchema(isRecord(anything)))))
        },
        test("decodes map with string keys") {
          val s      = """{"type":"map","values":{"type":"int"}}"""
          val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

          assert(schema)(
            isRight(
              isMap(
                hasMapKeys(isStandardType(StandardType.StringType)) && hasMapValues(
                  isStandardType(StandardType.IntType)
                )
              )
            )
          )
        },
        suite("union")(
          test("option union with null on first position") {
            val s      = """[{"type":"null"}, {"type":"int"}]"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isOption(hasOptionElementSchema(isStandardType(StandardType.IntType)))))
          },
          test("option union with null on second position") {
            val s      = """[{"type":"int"}, {"type":"null"}]"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isOption(hasOptionElementSchema(isStandardType(StandardType.IntType)))))
          },
          test("not an option union with more than one element type") {
            val s      = """[{"type":"null"}, {"type":"int"}, {"type":"string"}]"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isOption(anything).negate))
          },
          test("nested either union") {
            val s =
              """{"type":"record","name":"wrapper_hashed_2071802344","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":[{"type":"record","name":"wrapper_hashed_n465006219","fields":[{"name":"value","type":[{"type":"record","name":"wrapper_hashed_n813828848","fields":[{"name":"value","type":["null","string"]}],"zio.schema.codec.avro.wrapper":true},"string"]}],"zio.schema.codec.avro.either":true},"string"]}],"zio.schema.codec.avro.either":true}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(
              isRight(
                isEither(
                  isEither(isOption(anything), isStandardType(StandardType.StringType)),
                  isStandardType(StandardType.StringType)
                )
              )
            )
          },
          test("union as zio initialSchemaDerived enumeration") {
            val s      = """[{"type":"null"}, {"type":"int"}, {"type":"string"}]"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val assertion1 = hasKey("null", tuple2First(isStandardType(StandardType.UnitType)))
            val sssertion2 = hasKey("int", tuple2First(isStandardType(StandardType.IntType)))
            val assertion3 = hasKey("string", tuple2First(isStandardType(StandardType.StringType)))
            assert(schema)(isRight(isEnum(enumStructure(assertion1 && sssertion2 && assertion3))))
          },
          test("correct case codec for case object of ADT") {
            val s =
              """[{"type":"record","name":"A","fields":[]},{"type":"record","name":"B","fields":[]},{"type":"record","name":"MyC","fields":[]}]"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val assertionA   = hasKey("A", tuple2First(isEmptyRecord))
            val assertionB   = hasKey("B", tuple2First(isEmptyRecord))
            val assertionMyC = hasKey("MyC", tuple2First(isEmptyRecord))
            assert(schema)(isRight(isEnum(enumStructure(assertionA && assertionB && assertionMyC))))
          },
          test("correct case codec for case class of ADT") {
            val s =
              """[{"type":"record","name":"A","fields":[{"name":"s","type":"string"},{"name":"b","type":"boolean"}]},{"type":"record","name":"B","fields":[]},{"type":"record","name":"MyC","fields":[]}]"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val assertionA = hasKey(
              "A",
              tuple2First(isRecord(hasRecordField(hasLabel(equalTo("s"))) && hasRecordField(hasLabel(equalTo("b")))))
            )
            val assertionB   = hasKey("B", tuple2First(isEmptyRecord))
            val assertionMyC = hasKey("MyC", tuple2First(isEmptyRecord))
            assert(schema)(isRight(isEnum(enumStructure(assertionA && assertionB && assertionMyC))))
          },
          test("unwrap nested union") {
            val s =
              """[{"type":"record","name":"wrapper_hashed_n465006219","namespace":"zio.schema.codec.avro","fields":[{"name":"value","type":[{"type":"record","name":"A","namespace":"","fields":[]},{"type":"record","name":"B","namespace":"","fields":[]}]}],"zio.schema.codec.avro.wrapper":true},{"type":"record","name":"C","fields":[]},{"type":"record","name":"D","fields":[{"name":"s","type":"string"}]}]"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val nestedEnumAssertion = isEnum(
              enumStructure(
                hasKey("A", tuple2First(isEmptyRecord)) && hasKey(
                  "B",
                  tuple2First(isEmptyRecord)
                )
              )
            )
            val nestedEnumKey =
              hasKey("zio.schema.codec.avro.wrapper_hashed_n465006219", tuple2First(nestedEnumAssertion))
            val cEnumKey = hasKey("C", tuple2First(isEmptyRecord))
            val dEnumKey = hasKey("D", tuple2First(isRecord(hasRecordField(hasLabel(equalTo("s"))))))
            assert(schema)(isRight(isEnum(enumStructure(nestedEnumKey && cEnumKey && dEnumKey))))
          }
        ),
        suite("fixed")(
          test("logical type decimal as BigDecimal") {
            val s =
              """{"type":"fixed","name":"Decimal_10_10","size":5,"logicalType":"decimal","precision":11,"scale":10}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val isDecimalAssertion = isStandardType(StandardType.BigDecimalType)
            val hasDecimalTypeAnnotation: Assertion[Iterable[Any]] =
              exists(equalTo(AvroAnnotations.decimal(DecimalType.Fixed(5))))
            val hasScalaAnnotation: Assertion[Iterable[Any]]     = exists(equalTo(AvroAnnotations.scale(10)))
            val hasPrecisionAnnotation: Assertion[Iterable[Any]] = exists(equalTo(AvroAnnotations.precision(11)))
            val hasAnnotationsAssertion =
              annotations(hasDecimalTypeAnnotation && hasScalaAnnotation && hasPrecisionAnnotation)
            assert(schema)(isRight(isDecimalAssertion && hasAnnotationsAssertion))
          },
          test("logical type decimal as BigInteger") {
            val s =
              """{"type":"fixed","name":"Decimal_10_10","size":5,"logicalType":"decimal","precision":10,"scale":10}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val isBigIntegerType = isStandardType(StandardType.BigIntegerType)
            val hasDecimalTypeAnnotation: Assertion[Iterable[Any]] =
              exists(equalTo(AvroAnnotations.decimal(DecimalType.Fixed(5))))
            val hasScalaAnnotation: Assertion[Iterable[Any]] = exists(equalTo(AvroAnnotations.scale(10)))
            val doesNotHavePrecisionAnnotation: Assertion[Iterable[Any]] =
              exists(Assertion.isSubtype[AvroAnnotations.precision.type](anything)).negate
            val hasAnnotationsAssertion =
              annotations(hasDecimalTypeAnnotation && hasScalaAnnotation && doesNotHavePrecisionAnnotation)
            assert(schema)(isRight(isBigIntegerType && hasAnnotationsAssertion))
          },
          test("fail on invalid logical type") {
            val s =
              """{"type":"fixed","name":"Decimal_10_10","size":5,"logicalType":"decimal","precision":9,"scale":10}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isLeft(equalTo("Invalid decimal scale: 10 (greater than precision: 9)")))
          },
          test("decode as binary") {
            val s      = """{"type":"fixed","name":"Something","size":5}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val hasNameAnnotation = annotations(exists(equalTo(AvroAnnotations.name("Something"))))
            assert(schema)(isRight(isStandardType(StandardType.BinaryType) && hasNameAnnotation))
          }
        ),
        suite("string")(
          test("decodes zoneId with formatter") {
            val s      = """{"type":"string","zio.schema.codec.stringType":"zoneId"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.ZoneIdType)))
          },
          test("decodes instant with formatter") {
            val s =
              """{"type":"string","zio.schema.codec.stringType":"instant","zio.schema.codec.avro.dateTimeFormatter":"ISO_INSTANT"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.InstantType)))
          },
          test("decodes instant using default") {
            val s      = """{"type":"string","zio.schema.codec.stringType":"instant"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.InstantType)))
          },
          test("decodes instant with formatter pattern") {
            val pattern = "yyyy MM dd"
            val s =
              s"""{"type":"string","zio.schema.codec.stringType":"instant","zio.schema.codec.avro.dateTimeFormatter":"$pattern"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.InstantType)))
          },
          test("decode DateTimeFormatter field fails on invalid formatter") {
            val pattern = "this is not a valid formatter pattern"
            val s =
              s"""{"type":"string","zio.schema.codec.stringType":"instant","zio.schema.codec.avro.dateTimeFormatter":"$pattern"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isLeft(equalTo("Unknown pattern letter: t")))
          },
          test("decodes localDate with formatter") {
            val s =
              """{"type":"string","zio.schema.codec.stringType":"localDate","zio.schema.codec.avro.dateTimeFormatter":"ISO_ORDINAL_DATE"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalDateType)))
          },
          test("decodes localDate with default formatter") {
            val s      = """{"type":"string","zio.schema.codec.stringType":"localDate"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalDateType)))
          },
          test("decodes localTime with formatter") {
            val s =
              """{"type":"string","zio.schema.codec.stringType":"localTime","zio.schema.codec.avro.dateTimeFormatter":"ISO_ORDINAL_DATE"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalTimeType)))
          },
          test("decodes localTime with default formatter") {
            val s      = """{"type":"string","zio.schema.codec.stringType":"localTime"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalTimeType)))
          },
          test("decodes localDateTime with formatter") {
            val s =
              """{"type":"string","zio.schema.codec.stringType":"localDateTime","zio.schema.codec.avro.dateTimeFormatter":"ISO_ORDINAL_DATE"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalDateTimeType)))
          },
          test("decodes localDateTime with default formatter") {
            val s      = """{"type":"string","zio.schema.codec.stringType":"localDateTime"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(
              isRight(isStandardType(StandardType.LocalDateTimeType))
            )
          },
          test("decodes zonedDateTime with formatter") {
            val s =
              """{"type":"string","zio.schema.codec.stringType":"zoneDateTime","zio.schema.codec.avro.dateTimeFormatter":"ISO_ORDINAL_DATE"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.ZonedDateTimeType)))
          },
          test("decodes zonedDateTime with default formatter") {
            val s      = """{"type":"string","zio.schema.codec.stringType":"zoneDateTime"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(
              isRight(isStandardType(StandardType.ZonedDateTimeType))
            )
          },
          test("decodes offsetTime with formatter") {
            val s =
              """{"type":"string","zio.schema.codec.stringType":"offsetTime","zio.schema.codec.avro.dateTimeFormatter":"ISO_ORDINAL_DATE"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.OffsetTimeType)))
          },
          test("decodes offsetTime with default formatter") {
            val s      = """{"type":"string","zio.schema.codec.stringType":"offsetTime"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.OffsetTimeType)))
          },
          test("decodes offsetDateTime with formatter") {
            val s =
              """{"type":"string","zio.schema.codec.stringType":"offsetDateTime","zio.schema.codec.avro.dateTimeFormatter":"ISO_ORDINAL_DATE"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.OffsetDateTimeType)))
          },
          test("decodes offsetDateTime with default formatter") {
            val s      = """{"type":"string","zio.schema.codec.stringType":"offsetDateTime"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(
              isRight(isStandardType(StandardType.OffsetDateTimeType))
            )
          },
          test("decodes logical type uuid") {
            val s      = """{"type":"string","logicalType":"uuid"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.UUIDType)))
          },
          test("decodes primitive type string") {
            val s      = """{"type":"string"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.StringType)))
          }
        ),
        suite("bytes")(
          test("logical type decimal as BigDecimal") {
            val s      = """{"type":"bytes","logicalType":"decimal","precision":20,"scale":10}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val isDecimalAssertion = isStandardType(StandardType.BigDecimalType)
            val hasDecimalTypeAnnotation: Assertion[Iterable[Any]] =
              exists(equalTo(AvroAnnotations.decimal(DecimalType.Bytes)))
            val hasScalaAnnotation: Assertion[Iterable[Any]]     = exists(equalTo(AvroAnnotations.scale(10)))
            val hasPrecisionAnnotation: Assertion[Iterable[Any]] = exists(equalTo(AvroAnnotations.precision(20)))
            val hasAnnotationsAssertion =
              annotations(hasDecimalTypeAnnotation && hasScalaAnnotation && hasPrecisionAnnotation)
            assert(schema)(isRight(isDecimalAssertion && hasAnnotationsAssertion))
          },
          test("logical type decimal as BigInteger") {
            val s      = """{"type":"bytes","logicalType":"decimal","precision":20,"scale":20}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            val isBigIntegerAssertion = isStandardType(StandardType.BigIntegerType)
            val hasDecimalTypeAnnotation: Assertion[Iterable[Any]] =
              exists(equalTo(AvroAnnotations.decimal(DecimalType.Bytes)))
            val hasScalaAnnotation: Assertion[Iterable[Any]] = exists(equalTo(AvroAnnotations.scale(20)))
            val hasAnnotationsAssertion                      = annotations(hasDecimalTypeAnnotation && hasScalaAnnotation)
            assert(schema)(isRight(isBigIntegerAssertion && hasAnnotationsAssertion))
          },
          test("decode as binary") {
            val s      = """{"type":"bytes"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.BinaryType)))
          }
        ),
        suite("int")(
          test("decodes char") {
            val s      = """{"type":"int","zio.schema.codec.intType":"char"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.CharType)))
          },
          test("decodes dayOfWeek") {
            val s      = """{"type":"int","zio.schema.codec.intType":"dayOfWeek"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.DayOfWeekType)))
          },
          test("decodes Year") {
            val s      = """{"type":"int","zio.schema.codec.intType":"year"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.YearType)))
          },
          test("decodes short") {
            val s      = """{"type":"int","zio.schema.codec.intType":"short"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.ShortType)))
          },
          test("decodes month") {
            val s      = """{"type":"int","zio.schema.codec.intType":"month"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.MonthType)))
          },
          test("decodes zoneOffset") {
            val s      = """{"type":"int","zio.schema.codec.intType":"zoneOffset"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.ZoneOffsetType)))
          },
          test("decodes int") {
            val s      = """{"type":"int"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.IntType)))
          },
          test("decodes logical type timemillis") {
            val s =
              """{"type":"int","logicalType":"time-millis","zio.schema.codec.avro.dateTimeFormatter":"ISO_ORDINAL_DATE"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalTimeType)))
          },
          test("decodes logical type timemillis with default formatter") {
            val s      = """{"type":"int","logicalType":"time-millis"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalTimeType)))
          },
          test("decodes logical type date") {
            val s =
              """{"type":"int","logicalType":"date"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalDateType)))
          },
          test("decodes logical type date with default formatter") {
            val s      = """{"type":"int","logicalType":"date"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalDateType)))
          }
        ),
        suite("long")(
          test("decodes long") {
            val s      = """{"type":"long"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LongType)))
          },
          test("decodes logical type timeMicros") {
            val s =
              """{"type":"long","logicalType":"time-micros","zio.schema.codec.avro.dateTimeFormatter":"ISO_ORDINAL_DATE"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalTimeType)))
          },
          test("decodes logical type timeMicros with default formatter") {
            val s      = """{"type":"long","logicalType":"time-micros"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalTimeType)))
          },
          test("decodes logical type timestampMillis") {
            val s =
              """{"type":"long","logicalType":"timestamp-millis","zio.schema.codec.avro.dateTimeFormatter":"ISO_ORDINAL_DATE"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.InstantType)))
          },
          test("decodes logical type timestampMillis with default formatter") {
            val s      = """{"type":"long","logicalType":"timestamp-millis"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.InstantType)))
          },
          test("decodes logical type timestampMicros") {
            val s =
              """{"type":"long","logicalType":"timestamp-micros","zio.schema.codec.avro.dateTimeFormatter":"ISO_ORDINAL_DATE"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.InstantType)))
          },
          test("decodes logical type timestampMicros with default formatter") {
            val s      = """{"type":"long","logicalType":"timestamp-micros"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.InstantType)))
          },
          test("decodes logical type LocalTimestamp millis") {
            val s =
              """{"type":"long","logicalType":"local-timestamp-millis","zio.schema.codec.avro.dateTimeFormatter":"ISO_ORDINAL_DATE"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalDateTimeType)))
          },
          test("decodes logical type LocalTimestamp millis with default formatter") {
            val s      = """{"type":"long","logicalType":"local-timestamp-millis"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(
              isRight(isStandardType(StandardType.LocalDateTimeType))
            )
          },
          test("decodes logical type LocalTimestamp micros") {
            val s =
              """{"type":"long","logicalType":"local-timestamp-micros","zio.schema.codec.avro.dateTimeFormatter":"ISO_ORDINAL_DATE"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(isRight(isStandardType(StandardType.LocalDateTimeType)))
          },
          test("decodes logical type LocalTimestamp micros with default formatter") {
            val s      = """{"type":"long","logicalType":"local-timestamp-micros"}"""
            val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

            assert(schema)(
              isRight(isStandardType(StandardType.LocalDateTimeType))
            )
          }
        ),
        test("float") {
          val s      = """{"type":"float"}"""
          val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

          assert(schema)(isRight(isStandardType(StandardType.FloatType)))
        },
        test("double") {
          val s      = """{"type":"double"}"""
          val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

          assert(schema)(isRight(isStandardType(StandardType.DoubleType)))
        },
        test("boolean") {
          val s      = """{"type":"boolean"}"""
          val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

          assert(schema)(isRight(isStandardType(StandardType.BoolType)))
        },
        test("null") {
          val s      = """{"type":"null"}"""
          val schema = AvroSchemaCodec.decode(Chunk.fromArray(s.getBytes()))

          assert(schema)(isRight(isStandardType(StandardType.UnitType)))
        }
      ),
      test("encode/decode full adt test") {
        val initialSchemaDerived = DeriveSchema.gen[FullAdtTest.TopLevelUnion]

        val decoded = for {
          avroSchemaString <- AvroSchemaCodec.encode(initialSchemaDerived)
          decoded          <- AvroSchemaCodec.decode(Chunk.fromArray(avroSchemaString.getBytes()))
          //_ <- AvroSchemaCodec.encode(decoded) TODO: this fails
        } yield decoded

        assert(decoded)(isRight(hasField("ast", _.ast, equalTo(initialSchemaDerived.ast))))
      } @@ TestAspect.ignore // TODO: FIX
    )
}

object AssertionHelper {

  def isRecord[A](assertion: Assertion[Schema.Record[A]]): Assertion[Schema[_]] =
    Assertion.isCase[Schema[_], Schema.Record[A]](
      "Record", {
        case r: Schema.Record[_] => Try { r.asInstanceOf[Schema.Record[A]] }.toOption
        case _                   => None
      },
      assertion
    )

  def isEmptyRecord[A]: Assertion[Schema[_]] =
    Assertion.isCase[Schema[_], Schema[_]](
      "EmptyRecord", {
        case r: CaseClass0[_]                                                => Some(r)
        case r @ GenericRecord(_, structure, _) if structure.toChunk.isEmpty => Some(r)
        case _                                                               => None
      },
      anything
    )

  def isEnum[A](assertion: Assertion[Schema.Enum[A]]): Assertion[Schema[_]] =
    Assertion.isCase[Schema[_], Schema.Enum[A]](
      "Enum", {
        case r: Schema.Enum[_] => Try { r.asInstanceOf[Schema.Enum[A]] }.toOption
        case _                 => None
      },
      assertion
    )

  def isSequence[A](assertion: Assertion[Schema.Sequence[_, A, _]]): Assertion[Schema[_]] =
    Assertion.isCase[Schema[_], Schema.Sequence[_, A, _]](
      "List", {
        case r: Schema.Sequence[_, _, _] => Try { r.asInstanceOf[Schema.Sequence[_, A, _]] }.toOption
        case _                           => None
      },
      assertion
    )

  def isMap[K, V](assertion: Assertion[Schema.Map[K, V]]): Assertion[Schema[_]] =
    Assertion.isCase[Schema[_], Schema.Map[K, V]](
      "Map", {
        case r: Schema.Map[_, _] => Try { r.asInstanceOf[Schema.Map[K, V]] }.toOption
        case _                   => None
      },
      assertion
    )

  def isTuple[A, B](assertion: Assertion[Schema.Tuple2[A, B]]): Assertion[Schema[_]] =
    Assertion.isCase[Schema[_], Schema.Tuple2[A, B]](
      "Tuple", {
        case r: Schema.Tuple2[_, _] => Try { r.asInstanceOf[Schema.Tuple2[A, B]] }.toOption
        case _                      => None
      },
      assertion
    )

  def isTuple[A, B](assertionA: Assertion[Schema[A]], assertionB: Assertion[Schema[B]]): Assertion[Schema[_]] =
    isTuple[A, B](
      hasField[Schema.Tuple2[A, B], Schema[A]]("left", _.left, assertionA) && hasField[Schema.Tuple2[A, B], Schema[B]](
        "right",
        _.right,
        assertionB
      )
    )

  def isEither[A, B](assertion: Assertion[Schema.Either[A, B]]): Assertion[Schema[_]] =
    Assertion.isCase[Schema[_], Schema.Either[A, B]](
      "Either", {
        case r: Schema.Either[_, _] => Try { r.asInstanceOf[Schema.Either[A, B]] }.toOption
        case _                      => None
      },
      assertion
    )

  def isEither[A, B](leftAssertion: Assertion[Schema[A]], rightAssertion: Assertion[Schema[B]]): Assertion[Schema[_]] =
    isEither[A, B](
      hasField[Schema.Either[A, B], Schema[A]]("left", _.left, leftAssertion) && hasField[
        Schema.Either[A, B],
        Schema[B]
      ]("right", _.right, rightAssertion)
    )

  def isOption[A](assertion: Assertion[Schema.Optional[A]]): Assertion[Schema[_]] =
    Assertion.isCase[Schema[_], Schema.Optional[A]](
      "Optional", {
        case r: Schema.Optional[_] => Try { r.asInstanceOf[Schema.Optional[A]] }.toOption
        case _                     => None
      },
      assertion
    )

  def tuple2First[A](assertion: Assertion[A]): Assertion[(A, _)] =
    Assertion.isCase[(A, _), A]("Tuple", {
      case (a, _) => Some(a)
    }, assertion)

  def hasMapKeys[K](assertion: Assertion[Schema[K]]): Assertion[Schema.Map[K, _]] =
    hasField("keySchema", _.keySchema, assertion)

  def hasMapValues[V](assertion: Assertion[Schema[V]]): Assertion[Schema.Map[_, V]] =
    hasField("valueSchema", _.valueSchema, assertion)

  def enumStructure(assertion: Assertion[ListMap[String, (Schema[_], Chunk[Any])]]): Assertion[Schema.Enum[_]] =
    Assertion.assertionRec("enumStructure")(assertion)(
      enum =>
        Some(`enum`.cases.foldRight(ListMap.empty[String, (Schema[_], Chunk[Any])]) { (caseValue, acc) =>
          (acc + (caseValue.id -> scala.Tuple2(caseValue.schema, caseValue.annotations)))
        })
    )

  def annotations(assertion: Assertion[Chunk[Any]]): Assertion[Any] =
    Assertion.assertionRec("hasAnnotations")(assertion) {
      case s: Schema[_]          => Some(s.annotations)
      case f: Schema.Field[_, _] => Some(f.annotations)
      case _                     => None
    }

  def hasNameAnnotation(assertion: Assertion[String]): Assertion[Any] =
    annotations(Assertion.exists(Assertion.isSubtype[AvroAnnotations.name](hasField("name", _.name, assertion))))

  def hasNamespaceAnnotation(assertion: Assertion[String]): Assertion[Any] =
    annotations(
      Assertion.exists(Assertion.isSubtype[AvroAnnotations.namespace](hasField("namespace", _.namespace, assertion)))
    )

  def hasDocAnnotation(assertion: Assertion[String]): Assertion[Any] =
    annotations(Assertion.exists(Assertion.isSubtype[AvroAnnotations.doc](hasField("doc", _.doc, assertion))))

  def hasFieldOrderAnnotation(assertion: Assertion[FieldOrderType]): Assertion[Field[_, _]] =
    annotations(
      Assertion.exists(
        Assertion.isSubtype[AvroAnnotations.fieldOrder](hasField("fieldOrderType", _.fieldOrderType, assertion))
      )
    )

  def hasAliasesAnnotation(assertion: Assertion[Iterable[String]]): Assertion[Any] =
    annotations(
      Assertion.exists(Assertion.isSubtype[AvroAnnotations.aliases](hasField("aliases", _.aliases, assertion)))
    )

  def hasFieldDefaultAnnotation(assertion: Assertion[Object]): Assertion[Field[_, _]] =
    annotations(
      Assertion.exists(
        Assertion.isSubtype[AvroAnnotations.default](hasField("javaDefaultObject", _.javaDefaultObject, assertion))
      )
    )

  def hasDefaultAnnotation(assertion: Assertion[Object]): Assertion[Schema[_]] =
    annotations(
      Assertion.exists(
        Assertion.isSubtype[AvroAnnotations.default](hasField("javaDefaultObject", _.javaDefaultObject, assertion))
      )
    )

  val hasErrorAnnotation: Assertion[Any] =
    annotations(Assertion.exists(Assertion.isSubtype[AvroAnnotations.error.type](Assertion.anything)))

  def asString(assertion: Assertion[String]): Assertion[Any] =
    Assertion.assertionRec("asString")(assertion)(v => Some(v.toString))

  def recordFields(assertion: Assertion[Iterable[Schema.Field[_, _]]]): Assertion[Schema.Record[_]] =
    Assertion.assertionRec[Schema.Record[_], Chunk[Field[_, _]]]("hasRecordField")(
      assertion
    ) {
      case r: Schema.Record[_] => Some(r.fields)
      case _                   => None
    }

  def hasSequenceElementSchema[A](assertion: Assertion[Schema[A]]): Assertion[Schema.Sequence[_, A, _]] =
    Assertion.hasField("schemaA", _.elementSchema, assertion)

  def hasOptionElementSchema[A](assertion: Assertion[Schema[A]]): Assertion[Schema.Optional[A]] =
    Assertion.hasField("schema", _.schema, assertion)

  def hasRecordField(assertion: Assertion[Schema.Field[_, _]]): Assertion[Schema.Record[_]] =
    recordFields(Assertion.exists(assertion))

  def hasLabel(assertion: Assertion[String]): Assertion[Schema.Field[_, _]] =
    hasField("label", _.name, assertion)

  def hasSchema(assertion: Assertion[Schema[_]]): Assertion[Schema.Field[_, _]] =
    hasField("initialSchemaDerived", _.schema, assertion)

  def isPrimitive[A](assertion: Assertion[Primitive[A]]): Assertion[Schema[_]] =
    Assertion.isCase[Schema[_], Primitive[A]]("Primitive", {
      case p: Primitive[_] => Try { p.asInstanceOf[Primitive[A]] }.toOption
      case _               => None
    }, assertion)

  def isStandardType[A](standardType: StandardType[A]): Assertion[Schema[_]] =
    isPrimitive[A](hasField("standardType", _.standardType, equalTo(standardType)))

  def isPrimitiveType[A](assertion: Assertion[StandardType[A]]): Assertion[Schema[_]] =
    isPrimitive[A](hasField("standardType", _.standardType, assertion))
}

object SpecTestData {

  @AvroAnnotations.name("MyEnum")
  sealed trait CaseObjectsOnlyAdt

  object CaseObjectsOnlyAdt {
    case object A extends CaseObjectsOnlyAdt
    case object B extends CaseObjectsOnlyAdt

    @AvroAnnotations.name("MyC")
    case object C extends CaseObjectsOnlyAdt
  }

  @AvroAnnotations.avroEnum
  sealed trait CaseObjectAndCaseClassAdt

  object CaseObjectAndCaseClassAdt {
    case object A extends CaseObjectAndCaseClassAdt
    case object B extends CaseObjectAndCaseClassAdt

    @AvroAnnotations.name("MyC")
    case object C           extends CaseObjectAndCaseClassAdt
    case class D(s: String) extends CaseObjectAndCaseClassAdt
  }

  sealed trait UnionWithNesting

  object UnionWithNesting {
    sealed trait Nested extends UnionWithNesting

    object Nested {
      case object A extends Nested
      case object B extends Nested
    }

    @AvroAnnotations.name("MyC")
    case object C           extends UnionWithNesting
    case class D(s: String) extends UnionWithNesting
  }

  case class Record(s: String, b: Boolean)

  @AvroAnnotations.name("MyNamedRecord")
  case class NamedRecord(s: String, b: Boolean)

  @AvroAnnotations.name("MyNamedFieldRecord")
  case class NamedFieldRecord(@AvroAnnotations.name("myNamedField") s: String, b: Boolean)

  @AvroAnnotations.name("NestedRecord")
  case class NestedRecord(s: String, nested: NamedRecord)

  @AvroAnnotations.name("Simple")
  case class SimpleRecord(s: String)

  object FullAdtTest {
    sealed trait TopLevelUnion

    object TopLevelUnion {
      case class RecordWithPrimitives(
        string: String,
        bool: Boolean,
        int: Int,
        double: Double,
        float: Float,
        short: Short,
        bigInt: BigInt,
        bigDecimal: BigDecimal,
        unit: Unit,
        char: Char,
        uuid: UUID
      ) extends TopLevelUnion
      case class NestedRecord(innerRecord: InnerRecord)                                          extends TopLevelUnion
      case class Unions(union: Union)                                                            extends TopLevelUnion
      case class Enumeration(`enum`: Enum)                                                       extends TopLevelUnion
      case class Iterables(list: List[String], map: scala.collection.immutable.Map[String, Int]) extends TopLevelUnion

      // TODO: Schema derivation fails for the following case classes
      // case class RecordWithTimeRelatedPrimitives(localDateTime: LocalDateTime, localTime: LocalTime, localDate: LocalDate, offsetTime: OffsetTime, offsetDateTime: OffsetDateTime, zonedDateTime: ZonedDateTime, zoneOffset: ZoneOffset, zoneId: ZoneId, instant: Instant) extends TopLevelUnion
      // case class IterablesComplex(list: List[InnerRecord], map: Map[InnerRecord, Enum]) extends TopLevelUnion
    }

    case class InnerRecord(s: String, union: Option[scala.util.Either[String, Int]])

    sealed trait Union
    case class NestedUnion(inner: InnerUnion) extends Union
    case object OtherCase                     extends Union

    sealed trait InnerUnion
    case class InnerUnionCase1(s: String) extends InnerUnion
    case class InnerUnionCase2(i: Int)    extends InnerUnion
    sealed trait InnerUnionNested         extends InnerUnion
    case object InnerUnionNestedCase1     extends InnerUnionNested
    case object InnerUnionNestedCase2     extends InnerUnionNested

    sealed trait Enum
    case object EnumCase1 extends Enum
    case object EnumCase2 extends Enum
  }
}
