package zio.schema.codec

// import java.time.Year
import java.time.format.DateTimeFormatter
import java.time.{ ZoneId, ZoneOffset }

import scala.collection.immutable.ListMap

import zio.Console._
import zio._
import zio.json.JsonDecoder.JsonError
import zio.json.{ DeriveJsonEncoder, JsonEncoder }
import zio.schema.CaseSet._
import zio.schema._
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

object JsonCodecSpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment, Any] =
    suite("JsonCodec Spec")(
      encoderSuite,
      decoderSuite,
      encoderDecoderSuite
    ) @@ timeout(90.seconds)

  // TODO: Add tests for the pipeline contract.

  private val encoderSuite = suite("encoding")(
    suite("primitive")(
      test("unit") {
        assertEncodesJson(Schema[Unit], (), "{}")
      },
      test("string")(
        check(Gen.string)(s => assertEncodes(Schema[String], s, stringify(s)))
      ),
      test("ZoneOffset") {
        assertEncodesJson(Schema.Primitive(StandardType.ZoneOffsetType), ZoneOffset.UTC)
      },
      test("ZoneId") {
        assertEncodesJson(Schema.Primitive(StandardType.ZoneIdType), ZoneId.systemDefault())
      }
    ),
    suite("optional")(
      test("of primitives") {
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
      test("of primitives") {
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
      test("of primitives") {
        assertEncodesJson(
          Schema.chunk(Schema.Primitive(StandardType.StringType)),
          Chunk("a", "b", "c")
        )
      }
    ),
    suite("Map")(
      test("of complex keys and values") {
        assertEncodes(
          Schema.map[Key, Value],
          Map(Key("a", 0) -> Value(0, true), Key("b", 1) -> Value(1, false)),
          JsonCodec.Encoder.charSequenceToByteChunk(
            """[[{"name":"a","index":0},{"first":0,"second":true}],[{"name":"b","index":1},{"first":1,"second":false}]]"""
          )
        )
      }
    ),
    suite("Set")(
      test("of complex values") {
        assertEncodes(
          Schema.set[Value],
          Set(Value(0, true), Value(1, false)),
          JsonCodec.Encoder.charSequenceToByteChunk(
            """[{"first":0,"second":true},{"first":1,"second":false}]"""
          )
        )
      }
    ),
    suite("record")(
      test("of primitives") {
        assertEncodes(
          recordSchema,
          ListMap[String, Any]("foo" -> "s", "bar" -> 1),
          JsonCodec.Encoder.charSequenceToByteChunk("""{"foo":"s","bar":1}""")
        )
      },
      test("of records") {
        assertEncodes(
          nestedRecordSchema,
          ListMap[String, Any]("l1" -> "s", "l2" -> ListMap("foo" -> "s", "bar" -> 1)),
          JsonCodec.Encoder.charSequenceToByteChunk("""{"l1":"s","l2":{"foo":"s","bar":1}}""")
        )
      },
      test("case class") {
        check(searchRequestGen) { searchRequest =>
          assertEncodesJson(
            searchRequestSchema,
            searchRequest
          )
        }
      },
      test("case object") {
        assertEncodesJson(
          schemaObject,
          Singleton,
          "{}"
        )
      }
    ),
    suite("enumeration")(
      test("of primitives") {
        assertEncodes(
          enumSchema,
          ("foo"),
          JsonCodec.Encoder.charSequenceToByteChunk("""{"string":"foo"}""")
        )
      },
      test("ADT") {
        assertEncodes(
          Schema[Enumeration],
          Enumeration(StringValue("foo")),
          JsonCodec.Encoder.charSequenceToByteChunk("""{"oneOf":{"StringValue":{"value":"foo"}}}""")
        )
      }
    )
  )

  private val decoderSuite = suite("decoding")(
    suite("primitive")(
      test("unit") {
        assertEncodesJson(Schema[Unit], (), "{}")
      },
      suite("string")(
        test("any") {
          check(Gen.string)(s => assertDecodes(Schema[String], s, stringify(s)))
        }
      )
    ),
    suite("transform")(
      test("string") {
        val stringSchema    = Schema.Primitive(StandardType.StringType)
        val transformSchema = stringSchema.transform[Int](_ => 1, _.toString)
        assertDecodes(transformSchema, 1, stringify("string"))
      },
      test("failed") {
        val errorMessage = "I'm sorry Dave, I can't do that"
        val schema: Schema[Int] = Schema
          .Primitive(StandardType.StringType)
          .transformOrFail[Int](_ => Left(errorMessage), i => Right(i.toString))
        check(Gen.int(Int.MinValue, Int.MaxValue)) { int =>
          assertDecodesToError(
            schema,
            JsonEncoder.string.encodeJson(int.toString, None),
            JsonError.Message(errorMessage) :: Nil
          )
        }
      }
    ),
    suite("case class")(
      test("case object") {
        assertDecodes(schemaObject, Singleton, JsonCodec.Encoder.charSequenceToByteChunk("{}"))
      }
    )
  )

  private val encoderDecoderSuite = suite("encoding then decoding")(
    test("unit") {
      assertEncodesThenDecodes(Schema[Unit], ())
    },
    test("primitive") {
      check(SchemaGen.anyPrimitiveAndValue) {
        case (schema, value) => assertEncodesThenDecodes(schema, value)
      }
    },
    suite("either")(
      test("of primitives") {
        check(SchemaGen.anyEitherAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("of tuples") {
        check(
          for {
            left  <- SchemaGen.anyTupleAndValue
            right <- SchemaGen.anyTupleAndValue
          } yield (
            Schema.EitherSchema(left._1.asInstanceOf[Schema[(Any, Any)]], right._1.asInstanceOf[Schema[(Any, Any)]]),
            Right(right._2)
          )
        ) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("of enums") {
        check(for {
          (left, value) <- SchemaGen.anyEnumerationAndValue
          (right, _)    <- SchemaGen.anyEnumerationAndValue
        } yield (Schema.EitherSchema(left, right), Left(value))) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("of sequence") {
        check(
          for {
            left  <- SchemaGen.anySequenceAndValue
            right <- SchemaGen.anySequenceAndValue
          } yield (
            Schema.EitherSchema(left._1.asInstanceOf[Schema[Chunk[Any]]], right._1.asInstanceOf[Schema[Chunk[Any]]]),
            Left(left._2)
          )
        ) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("of map") {
        check(
          for {
            left  <- SchemaGen.anyMapAndValue
            right <- SchemaGen.anyMapAndValue
          } yield (
            Schema
              .EitherSchema(left._1.asInstanceOf[Schema[Map[Any, Any]]], right._1.asInstanceOf[Schema[Map[Any, Any]]]),
            Left(left._2)
          )
        ) {
          case (schema, value) =>
            assertEncodesThenDecodes[Either[Map[Any, Any], Map[Any, Any]]](
              schema.asInstanceOf[Schema[Either[Map[Any, Any], Map[Any, Any]]]],
              value.asInstanceOf[Either[Map[Any, Any], Map[Any, Any]]]
            )
        }
      },
      test("of set") {
        check(
          for {
            left  <- SchemaGen.anySetAndValue
            right <- SchemaGen.anySetAndValue
          } yield (
            Schema
              .EitherSchema(left._1.asInstanceOf[Schema[Set[Any]]], right._1.asInstanceOf[Schema[Set[Any]]]),
            Left(left._2)
          )
        ) {
          case (schema, value) =>
            assertEncodesThenDecodes[Either[Set[Any], Set[Any]]](
              schema.asInstanceOf[Schema[Either[Set[Any], Set[Any]]]],
              value.asInstanceOf[Either[Set[Any], Set[Any]]]
            )
        }
      },
      test("Map of complex keys and values") {
        assertEncodes(
          Schema.map[Key, Value],
          Map(Key("a", 0) -> Value(0, true), Key("b", 1) -> Value(1, false)),
          JsonCodec.Encoder.charSequenceToByteChunk(
            """[[{"name":"a","index":0},{"first":0,"second":true}],[{"name":"b","index":1},{"first":1,"second":false}]]"""
          )
        )
      },
      test("Set of complex values") {
        assertEncodes(
          Schema.set[Value],
          Set(Value(0, true), Value(1, false)),
          JsonCodec.Encoder.charSequenceToByteChunk(
            """[{"first":0,"second":true},{"first":1,"second":false}]"""
          )
        )
      },
      test("of records") {
        check(for {
          (left, a)       <- SchemaGen.anyRecordAndValue
          primitiveSchema <- SchemaGen.anyPrimitive
        } yield (Schema.EitherSchema(left, primitiveSchema), Left(a))) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("of records of records") {
        check(for {
          (left, _)  <- SchemaGen.anyRecordOfRecordsAndValue
          (right, b) <- SchemaGen.anyRecordOfRecordsAndValue
        } yield (Schema.EitherSchema(left, right), Right(b))) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      test("mixed") {
        check(for {
          (left, _)      <- SchemaGen.anyEnumerationAndValue
          (right, value) <- SchemaGen.anySequenceAndValue
        } yield (Schema.EitherSchema(left, right), Right(value))) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      }
    ),
    suite("optional")(
      test("of primitive") {
        check(SchemaGen.anyOptionalAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("of tuple") {
        check(SchemaGen.anyTupleAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      test("of record") {
        check(SchemaGen.anyRecordAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      test("of enumeration") {
        check(SchemaGen.anyEnumerationAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      test("of sequence") {
        check(SchemaGen.anySequenceAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      test("of Map") {
        check(SchemaGen.anyMapAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      test("of Set") {
        check(SchemaGen.anySetAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      test("of Map") {
        check(SchemaGen.anyMapAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      test("of Set") {
        check(SchemaGen.anySetAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      }
    ),
    test("tuple") {
      check(SchemaGen.anyTupleAndValue) {
        case (schema, value) => assertEncodesThenDecodes(schema, value)
      }
    },
    suite("sequence")(
      test("of primitives") {
        check(SchemaGen.anySequenceAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("of records") {
        check(SchemaGen.anyCaseClassAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.chunk(schema), Chunk.fill(3)(value))
        }
      },
      test("of java.time.ZoneOffset") {
        //FIXME test independently because including ZoneOffset in StandardTypeGen.anyStandardType wreaks havoc.
        check(Gen.chunkOf(JavaTimeGen.anyZoneOffset)) { chunk =>
          assertEncodesThenDecodes(
            Schema.chunk(Schema.Primitive(StandardType.ZoneOffsetType)),
            chunk
          )
        }
      },
      test("of map") {
        check(SchemaGen.anyMapAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.chunk(schema), Chunk.fill(3)(value))
        }
      },
      test("of set") {
        check(SchemaGen.anySetAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.chunk(schema), Chunk.fill(3)(value))
        }
      }
    ),
    suite("map")(
      test("encodes and decodes a Map") {
        check(SchemaGen.anyMapAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      }
    ),
    suite("set")(
      test("encodes and decodes a Set") {
        check(SchemaGen.anySetAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      }
    ),
    suite("case class")(
      test("basic") {
        check(searchRequestGen) { value =>
          assertEncodesThenDecodes(searchRequestSchema, value)
        }
      },
      test("object") {
        assertEncodesThenDecodes(schemaObject, Singleton)
      }
    ),
    suite("record")(
      test("any") {
        check(SchemaGen.anyRecordAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("minimal test case") {
        SchemaGen.anyRecordAndValue.runHead.flatMap {
          case Some((schema, value)) =>
            val key      = new String(Array('\u0007', '\n'))
            val embedded = Schema.record(Schema.Field(key, schema))
            assertEncodesThenDecodes(embedded, ListMap(key -> value))
          case None => ZIO.fail("Should never happen!")
        }
      },
      test("record of records") {
        check(SchemaGen.anyRecordOfRecordsAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      test("of primitives") {
        check(SchemaGen.anyRecordAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("of ZoneOffsets") {
        check(JavaTimeGen.anyZoneOffset) { zoneOffset =>
          assertEncodesThenDecodes(
            Schema.record(Schema.Field("zoneOffset", Schema.Primitive(StandardType.ZoneOffsetType))),
            ListMap[String, Any]("zoneOffset" -> zoneOffset)
          )
        }
      },
      test("of record") {
        assertEncodesThenDecodes(
          nestedRecordSchema,
          ListMap[String, Any]("l1" -> "s", "l2" -> ListMap[String, Any]("foo" -> "s", "bar" -> 1))
        )
      }
    ),
    suite("enumeration")(
      test("of primitives") {
        assertEncodesThenDecodes(
          enumSchema,
          "foo"
        )
      },
      test("ADT") {
        assertEncodesThenDecodes(
          Schema[Enumeration],
          Enumeration(StringValue("foo"))
        ) &> assertEncodesThenDecodes(Schema[Enumeration], Enumeration(IntValue(-1))) &> assertEncodesThenDecodes(
          Schema[Enumeration],
          Enumeration(BooleanValue(false))
        )
      }
    ),
    suite("transform")(
      test("any") {
        check(SchemaGen.anyTransformAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      }
    ),
    suite("any schema")(
      test("leaf") {
        check(SchemaGen.anyLeafAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      test("recursive schema") {
        check(SchemaGen.anyTreeAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      test("recursive data type") {
        check(SchemaGen.anyRecursiveTypeAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      suite("dynamic")(
        test("dynamic int") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(StandardType.IntType)
          ) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic instant") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(StandardType.InstantType(DateTimeFormatter.ISO_INSTANT))
          ) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic zoned date time") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(
              StandardType.ZonedDateTimeType(DateTimeFormatter.ISO_ZONED_DATE_TIME)
            )
          ) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic duration") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(StandardType.DurationType)
          ) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic string") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(StandardType.StringType)
          ) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic unit") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(StandardType.UnitType)
          ) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic json") {
          check(
            DynamicValueGen.anyDynamicValueOfSchema(SchemaGen.Json.schema)
          ) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic tuple") {
          check(
            DynamicValueGen.anyDynamicTupleValue(Schema[String], Schema[Int])
          ) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic record") {
          check(
            SchemaGen.anyRecord.flatMap(DynamicValueGen.anyDynamicValueOfSchema)
          ) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic (string, record)") {
          check(
            SchemaGen.anyRecord.flatMap(record => DynamicValueGen.anyDynamicTupleValue(Schema[String], record))
          ) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        }
      ),
      test("semi dynamic record") {
        check(
          SchemaGen.anyRecord.flatMap(
            record =>
              DynamicValueGen
                .anyDynamicValueOfSchema(record)
                .map(dyn => (dyn.toTypedValue(record).toOption.get, record))
          )
        ) { value =>
          val schema = Schema.semiDynamic[ListMap[String, _]]()
          assertEncodesThenDecodesWithDifferentSchemas(
            encodingSchema = schema,
            decodingSchema = schema,
            value = value,
            compare = compareRandomRecords
          )
        }
      }
    )
  )

  private def compareRandomRecords(
    a: (ListMap[String, _], Schema[ListMap[String, _]]),
    b: (ListMap[String, _], Schema[ListMap[String, _]])
  ): Boolean = {
    val (aValue, aSchema) = a
    val (bValue, bSchema) = b
    if (Schema.structureEquality.equal(aSchema, bSchema)) {
      // We cannot check real equality because the transformations are lost
      aValue.keySet == bValue.keySet
    } else {
      false
    }
  }

  private def assertEncodes[A](schema: Schema[A], value: A, chunk: Chunk[Byte]) = {
    val stream = ZStream
      .succeed(value)
      .via(JsonCodec.encoder(schema))
      .runCollect
    assertZIO(stream)(equalTo(chunk))
  }

  private def assertEncodesJson[A](schema: Schema[A], value: A, json: String) = {
    val stream = ZStream
      .succeed(value)
      .via(JsonCodec.encoder(schema))
      .runCollect
      .map(chunk => new String(chunk.toArray))
    assertZIO(stream)(equalTo(json))
  }

  private def assertEncodesJson[A](schema: Schema[A], value: A)(implicit enc: JsonEncoder[A]) = {
    val stream = ZStream
      .succeed(value)
      .via(JsonCodec.encoder(schema))
      .runCollect
    assertZIO(stream)(equalTo(jsonEncoded(value)))
  }

  private def assertDecodesToError[A](schema: Schema[A], json: CharSequence, errors: List[JsonError]) = {
    val stream = ZStream
      .fromChunk(JsonCodec.Encoder.charSequenceToByteChunk(json))
      .via(JsonCodec.decoder(schema))
      .catchAll(ZStream.succeed[String](_))
      .runHead
    assertZIO(stream)(isSome(equalTo(JsonError.render(errors))))
  }

  private def assertDecodes[A](schema: Schema[A], value: A, chunk: Chunk[Byte]) = {
    val result = ZStream.fromChunk(chunk).via(JsonCodec.decoder(schema)).runCollect
    assertZIO(result)(equalTo(Chunk(value)))
  }

  private def assertEncodesThenDecodes[A](schema: Schema[A], value: A, print: Boolean = false) =
    assertEncodesThenDecodesWithDifferentSchemas(schema, schema, value, (x: A, y: A) => x == y, print)

  private def assertEncodesThenDecodesWithDifferentSchemas[A1, A2](
    encodingSchema: Schema[A1],
    decodingSchema: Schema[A2],
    value: A1,
    compare: (A1, A2) => Boolean,
    print: Boolean = false
  ) =
    ZStream
      .succeed(value)
      .tap(value => printLine(s"Input Value: $value").when(print).ignore)
      .via(JsonCodec.encoder(encodingSchema))
      .runCollect
      .tap(encoded => printLine(s"Encoded: ${new String(encoded.toArray)}").when(print).ignore)
      .flatMap { encoded =>
        ZStream
          .fromChunk(encoded)
          .via(JsonCodec.decoder(decodingSchema))
          .runCollect
          .tapError { err =>
            printLineError(s"Decoding failed for input ${new String(encoded.toArray)}\nError Message: $err")
          }
      }
      .tap(decoded => printLine(s"Decoded: $decoded").when(print).ignore)
      .either
      .map { result =>
        assertTrue(
          compare(value, result.toOption.get.head)
        )
      }

  private def flatten[A](value: A): A = value match {
    case Some(None)    => None.asInstanceOf[A]
    case Some(Some(a)) => flatten(Some(flatten(a))).asInstanceOf[A]
    case _             => value
  }

  implicit def mapEncoder[K, V](
    implicit keyEncoder: JsonEncoder[K],
    valueEncoder: JsonEncoder[V]
  ): JsonEncoder[Map[K, V]] =
    JsonEncoder.chunk(keyEncoder.zip(valueEncoder)).contramap[Map[K, V]](m => Chunk.fromIterable(m))

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

  private val searchRequestGen: Gen[Sized, SearchRequest] =
    for {
      query      <- Gen.string
      pageNumber <- Gen.int(Int.MinValue, Int.MaxValue)
      results    <- Gen.int(Int.MinValue, Int.MaxValue)
    } yield SearchRequest(query, pageNumber, results)

  val searchRequestSchema: Schema[SearchRequest] = DeriveSchema.gen[SearchRequest]

  val recordSchema: Schema[ListMap[String, _]] = Schema.record(
    Schema.Field("foo", Schema.Primitive(StandardType.StringType)),
    Schema.Field("bar", Schema.Primitive(StandardType.IntType))
  )

  val nestedRecordSchema: Schema[ListMap[String, _]] = Schema.record(
    Schema.Field("l1", Schema.Primitive(StandardType.StringType)),
    Schema.Field("l2", recordSchema)
  )

  val enumSchema: Schema[Any] = Schema.enumeration[Any, CaseSet.Aux[Any]](
    caseOf[String, Any]("string")(_.asInstanceOf[String]) ++ caseOf[Int, Any]("int")(_.asInstanceOf[Int]) ++ caseOf[
      Boolean,
      Any
    ]("boolean")(_.asInstanceOf[Boolean])
  )

  sealed trait OneOf
  case class StringValue(value: String)   extends OneOf
  case class IntValue(value: Int)         extends OneOf
  case class BooleanValue(value: Boolean) extends OneOf

  object OneOf {
    implicit val schema: Schema[OneOf] = DeriveSchema.gen[OneOf]
  }

  case class Enumeration(oneOf: OneOf)

  object Enumeration {
    implicit val schema: Schema[Enumeration] = DeriveSchema.gen[Enumeration]
  }

  case object Singleton
  implicit val schemaObject: Schema[Singleton.type] = DeriveSchema.gen[Singleton.type]

  case class Key(name: String, index: Int)

  object Key {
    implicit lazy val schema: Schema[Key] = DeriveSchema.gen[Key]
  }
  case class Value(first: Int, second: Boolean)

  object Value {
    implicit lazy val schema: Schema[Value] = DeriveSchema.gen[Value]
  }
}
