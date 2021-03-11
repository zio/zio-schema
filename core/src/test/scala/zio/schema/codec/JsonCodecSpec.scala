package zio.schema.codec

// import java.time.Year
import zio.Chunk
import zio.duration._
import zio.json.{ DeriveJsonEncoder, JsonEncoder }
import zio.schema.{ Schema, SchemaGen, StandardType }
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test.environment.TestEnvironment
import zio.test.{ testM, _ }

//TODO encode and decode specs
object JsonCodecSpec extends DefaultRunnableSpec {

  def spec: ZSpec[TestEnvironment, Any] =
    suite("JsonCodec Spec")(
      encoderSuite,
      decoderSuite,
      encoderDecoderSuite
    ) @@ timeout(90.seconds)

  // TODO: Add tests for the transducer contract.

  private val encoderSuite = suite("Should correctly encode")(
    suite("primitive")(
      testM("unit") {
        assertEncodesUnit
      },
      testM("string")(
        checkM(Gen.anyString)(assertEncodesString)
      )
    ),
    suite("optional")(
      testM("of primitives") {
        assertEncodesJson(
          Schema.Optional(Schema.Primitive(StandardType.StringType)),
          Some("value")
        ) &>
          assertEncodesJson(
            Schema.Optional(Schema.Primitive(StandardType.StringType)),
            None
          )
      }
    ),
    suite("tuple")(
      testM("of primitives") {
        assertEncodesJson(
          Schema.Tuple(
            Schema.Primitive(StandardType.StringType),
            Schema.Primitive(StandardType.IntType)
          ),
          ("L", 1)
        )
      }
    ),
    suite("sequence")(
      testM("of primitives") {
        assertEncodesJson(
          Schema.Sequence(Schema.Primitive(StandardType.StringType)),
          Chunk("a", "b", "c")
        )
      }
    ),
    suite("record")(
      testM("of primitives") {
        assertEncodes(
          recordSchema,
          Map[String, Any]("foo" -> "s", "bar" -> 1),
          JsonCodec.Encoder.charSequenceToByteChunk("""{"foo":"s","bar":1}""")
        )
      },
      testM("of records") {
        assertEncodes(
          nestedRecordSchema,
          Map[String, Any]("l1" -> "s", "l2" -> Map("foo" -> "s", "bar" -> 1)),
          JsonCodec.Encoder.charSequenceToByteChunk("""{"l1":"s","l2":{"foo":"s","bar":1}}""")
        )
      },
      testM("case class") {
        assertEncodesJson(
          searchRequestSchema,
          SearchRequest("query", 1, 2)
        )
      }
    ),
    suite("enumeration")(
      testM("of primitives") {
        assertEncodes(
          enumSchema,
          Map[String, Any]("string" -> "foo"),
          JsonCodec.Encoder.charSequenceToByteChunk("""{"string":"foo"}""")
        )
      },
      testM("ADT") {
        assertEncodes(
          adtSchema,
          Enumeration(StringValue("foo")),
          JsonCodec.Encoder.charSequenceToByteChunk("""{"value":{"string":"foo"}}""")
        )
      }
    )
  )

  private val decoderSuite = suite("Should correctly decode")(
    suite("primitive")(
      testM("unit") {
        assertDecodesUnit
      },
      suite("string")(
        testM("example") {
          assertDecodesString("hello")
        },
        testM("any") {
          checkM(Gen.anyString)(assertDecodesString)
        }
      )
    )
  )

  private val encoderDecoderSuite = suite("Encoding then decoding")(
    testM("unit") {
      assertEncodesThenDecodes(Schema.Primitive(StandardType.UnitType), ())
    },
    testM("primitive") {
      checkM(SchemaGen.anyPrimitiveAndValue) {
        case (schema, value) => assertEncodesThenDecodes(schema, value)
      }
    },
    suite("optional")(
      testM("of primitive") {
        checkM(SchemaGen.anyOptionalAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      testM("of tuple") {
        checkM(SchemaGen.anyTupleAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      testM("of record") {
        checkM(SchemaGen.anyRecordAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      testM("of enumeration") {
        checkM(SchemaGen.anyEnumerationAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      testM("of sequence") {
        checkM(SchemaGen.anySequenceAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      }
    ),
    testM("tuple") {
      checkM(SchemaGen.anyTupleAndValue) {
        case (schema, value) => assertEncodesThenDecodes(schema, value)
      }
    },
    testM("sequence") {
      checkM(SchemaGen.anySequenceAndValue) {
        case (schema, value) => assertEncodesThenDecodes(schema, value)
      }
    },
    suite("record")(
      testM("any") {
        checkM(SchemaGen.anyRecordAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      testM("of records") {
        checkM(SchemaGen.anyRecordOfRecordsAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      testM("of primitives") {
        assertEncodesThenDecodes(
          recordSchema,
          Map[String, Any]("foo" -> "s", "bar" -> 1)
        )
      },
      testM("of record") {
        assertEncodesThenDecodes(
          nestedRecordSchema,
          Map[String, Any]("l1" -> "s", "l2" -> Map[String, Any]("foo" -> "s", "bar" -> 1))
        )
      },
      testM("case class") {
        assertEncodesThenDecodes(
          searchRequestSchema,
          SearchRequest("query", 1, 2)
        )
      }
    ),
    suite("enumeration")(
      testM("of primitives") {
        assertEncodesThenDecodes(
          enumSchema,
          Map("string" -> "foo")
        )
      },
      testM("ADT") {
        assertEncodesThenDecodes(
          adtSchema,
          Enumeration(StringValue("foo"))
        ) &> assertEncodesThenDecodes(adtSchema, Enumeration(IntValue(-1))) &> assertEncodesThenDecodes(
          adtSchema,
          Enumeration(BooleanValue(false))
        )
      }
    )
  )

  private def assertEncodesUnit = {
    val schema = Schema.Primitive(StandardType.UnitType)
    assertEncodes(schema, (), Chunk.empty)
  }

  private def assertEncodesString(value: String) = {
    val schema = Schema.Primitive(StandardType.StringType)
    assertEncodes(schema, value, stringify(value))
  }

  private def assertEncodes[A](schema: Schema[A], value: A, chunk: Chunk[Byte]) = {
    val stream = ZStream
      .succeed(value)
      .transduce(JsonCodec.encoder(schema))
      .runCollect
    assertM(stream)(equalTo(chunk))
  }

  private def assertEncodesJson[A](schema: Schema[A], value: A)(implicit enc: JsonEncoder[A]) = {
    val stream = ZStream
      .succeed(value)
      .transduce(JsonCodec.encoder(schema))
      .runCollect
    assertM(stream)(equalTo(jsonEncoded(value)))
  }

  private def assertDecodesUnit = {
    val schema = Schema.Primitive(StandardType.UnitType)
    assertDecodes(schema, (), Chunk.empty)
  }

  private def assertDecodesString(value: String) = {
    val schema = Schema.Primitive(StandardType.StringType)
    assertDecodes(schema, value, stringify(value))
  }

  private def assertDecodes[A](schema: Schema[A], value: A, chunk: Chunk[Byte]) = {
    val result = ZStream.fromChunk(chunk).transduce(JsonCodec.decoder(schema)).runCollect
    assertM(result)(equalTo(Chunk(value)))
  }

  private def assertEncodesThenDecodes[A](schema: Schema[A], value: A) = {
    val result = ZStream
      .succeed(value)
      .transduce(JsonCodec.encoder(schema))
      .runCollect
      .flatMap { encoded =>
        ZStream
          .fromChunk(encoded)
          .transduce(JsonCodec.decoder(schema))
          .runCollect
      }
    assertM(result)(equalTo(Chunk(value)))
  }

  private def jsonEncoded[A](value: A)(implicit enc: JsonEncoder[A]): Chunk[Byte] =
    JsonCodec.Encoder.charSequenceToByteChunk(enc.encodeJson(value, None))

  private def stringify(s: String): Chunk[Byte] = {
    val encoded = JsonEncoder.string.encodeJson(s, None)
    JsonCodec.Encoder.charSequenceToByteChunk(encoded)
  }

  case class SearchRequest(query: String, pageNumber: Int, resultPerPage: Int)

  object SearchRequest {
    implicit val encoder: JsonEncoder[SearchRequest] = DeriveJsonEncoder.gen[SearchRequest]
  }

  val searchRequestSchema: Schema[SearchRequest] = Schema.caseClassN(
    "query"         -> Schema[String],
    "pageNumber"    -> Schema[Int],
    "resultPerPage" -> Schema[Int]
  )(SearchRequest.apply, SearchRequest.unapply)

  val recordSchema: Schema.Record = Schema.Record(
    Map(
      "foo" -> Schema.Primitive(StandardType.StringType),
      "bar" -> Schema.Primitive(StandardType.IntType)
    )
  )

  val nestedRecordSchema: Schema.Record = Schema.Record(
    Map(
      "l1" -> Schema.Primitive(StandardType.StringType),
      "l2" -> recordSchema
    )
  )

  val enumSchema: Schema.Enumeration = Schema.Enumeration(
    Map(
      "string"  -> Schema.Primitive(StandardType.StringType),
      "int"     -> Schema.Primitive(StandardType.IntType),
      "boolean" -> Schema.Primitive(StandardType.BoolType)
    )
  )

  sealed trait OneOf
  case class StringValue(value: String)   extends OneOf
  case class IntValue(value: Int)         extends OneOf
  case class BooleanValue(value: Boolean) extends OneOf

  val schemaOneOf: Schema[OneOf] = Schema.Transform(
    Schema.enumeration(
      Map(
        "string"  -> Schema[String],
        "int"     -> Schema[Int],
        "boolean" -> Schema[Boolean]
      )
    ),
    (value: Map[String, _]) => {
      value
        .get("string")
        .map(v => Right(StringValue(v.asInstanceOf[String])))
        .orElse(value.get("int").map(v => Right(IntValue(v.asInstanceOf[Int]))))
        .orElse(value.get("boolean").map(v => Right(BooleanValue(v.asInstanceOf[Boolean]))))
        .getOrElse(Left("No value found"))
    }, {
      case StringValue(v)  => Right(Map("string"  -> v))
      case IntValue(v)     => Right(Map("int"     -> v))
      case BooleanValue(v) => Right(Map("boolean" -> v))
    }
  )

  case class Enumeration(oneOf: OneOf)

  val adtSchema: Schema[Enumeration] =
    Schema.caseClassN("value" -> schemaOneOf)(Enumeration, Enumeration.unapply)

}
