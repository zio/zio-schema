package zio.schema.codec

// import java.time.Year
import java.time.{ ZoneId, ZoneOffset }

import scala.collection.immutable.ListMap

import zio.console._
import zio.duration._
import zio.json.JsonDecoder.JsonError
import zio.json.{ DeriveJsonEncoder, JsonEncoder }
import zio.random.Random
import zio.schema.CaseSet._
import zio.schema.{ CaseSet, DeriveSchema, JavaTimeGen, Schema, SchemaGen, StandardType }
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._
import zio.test.environment.TestEnvironment
import zio.{ Chunk, ZIO }

object JsonCodecSpec extends DefaultRunnableSpec {

  def spec: ZSpec[TestEnvironment, Any] =
    suite("JsonCodec Spec")(
      encoderSuite,
      decoderSuite,
      encoderDecoderSuite
    ) @@ timeout(90.seconds)

  // TODO: Add tests for the transducer contract.

  private val encoderSuite = suite("encoding")(
    suite("primitive")(
      testM("unit") {
        assertEncodesJson(Schema[Unit], (), "{}")
      },
      testM("string")(
        checkM(Gen.anyString)(s => assertEncodes(Schema[String], s, stringify(s)))
      ),
      testM("ZoneOffset") {
        assertEncodesJson(Schema.Primitive(StandardType.ZoneOffsetType), ZoneOffset.UTC)
      },
      testM("ZoneId") {
        assertEncodesJson(Schema.Primitive(StandardType.ZoneIdType), ZoneId.systemDefault())
      }
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
          Schema.chunk(Schema.Primitive(StandardType.StringType)),
          Chunk("a", "b", "c")
        )
      }
    ),
    suite("Map")(
      testM("of complex keys and values") {
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
      testM("of complex values") {
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
      testM("of primitives") {
        assertEncodes(
          recordSchema,
          ListMap[String, Any]("foo" -> "s", "bar" -> 1),
          JsonCodec.Encoder.charSequenceToByteChunk("""{"foo":"s","bar":1}""")
        )
      },
      testM("of records") {
        assertEncodes(
          nestedRecordSchema,
          ListMap[String, Any]("l1" -> "s", "l2" -> ListMap("foo" -> "s", "bar" -> 1)),
          JsonCodec.Encoder.charSequenceToByteChunk("""{"l1":"s","l2":{"foo":"s","bar":1}}""")
        )
      },
      testM("case class") {
        checkM(searchRequestGen) { searchRequest =>
          assertEncodesJson(
            searchRequestSchema,
            searchRequest
          )
        }
      },
      testM("case object") {
        assertEncodesJson(
          schemaObject,
          Singleton,
          "{}"
        )
      }
    ),
    suite("enumeration")(
      testM("of primitives") {
        assertEncodes(
          enumSchema,
          ("foo"),
          JsonCodec.Encoder.charSequenceToByteChunk("""{"string":"foo"}""")
        )
      },
      testM("ADT") {
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
      testM("unit") {
        assertEncodesJson(Schema[Unit], (), "{}")
      },
      suite("string")(
        testM("any") {
          checkM(Gen.anyString)(s => assertDecodes(Schema[String], s, stringify(s)))
        }
      )
    ),
    suite("transform")(
      testM("string") {
        val stringSchema    = Schema.Primitive(StandardType.StringType)
        val transformSchema = stringSchema.transform[Int](_ => 1, _.toString)
        assertDecodes(transformSchema, 1, stringify("string"))
      },
      testM("failed") {
        val errorMessage = "I'm sorry Dave, I can't do that"
        val schema: Schema[Int] = Schema
          .Primitive(StandardType.StringType)
          .transformOrFail[Int](_ => Left(errorMessage), i => Right(i.toString))
        checkM(Gen.int(Int.MinValue, Int.MaxValue)) { int =>
          assertDecodesToError(
            schema,
            JsonEncoder.string.encodeJson(int.toString, None),
            JsonError.Message(errorMessage) :: Nil
          )
        }
      }
    ),
    suite("case class")(
      testM("case object") {
        assertDecodes(schemaObject, Singleton, JsonCodec.Encoder.charSequenceToByteChunk("{}"))
      }
    )
  )

  private val encoderDecoderSuite = suite("encoding then decoding")(
    testM("unit") {
      assertEncodesThenDecodes(Schema[Unit], ())
    },
    testM("primitive") {
      checkM(SchemaGen.anyPrimitiveAndValue) {
        case (schema, value) => assertEncodesThenDecodes(schema, value)
      }
    },
    suite("either")(
      testM("of primitives") {
        checkM(SchemaGen.anyEitherAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      testM("of tuples") {
        checkM(
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
      testM("of enums") {
        checkM(for {
          (left, value) <- SchemaGen.anyEnumerationAndValue
          (right, _)    <- SchemaGen.anyEnumerationAndValue
        } yield (Schema.EitherSchema(left, right), Left(value))) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      testM("of sequence") {
        checkM(
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
      testM("of map") {
        checkM(
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
      testM("of set") {
        checkM(
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
      testM("Map of complex keys and values") {
        assertEncodes(
          Schema.map[Key, Value],
          Map(Key("a", 0) -> Value(0, true), Key("b", 1) -> Value(1, false)),
          JsonCodec.Encoder.charSequenceToByteChunk(
            """[[{"name":"a","index":0},{"first":0,"second":true}],[{"name":"b","index":1},{"first":1,"second":false}]]"""
          )
        )
      },
      testM("Set of complex values") {
        assertEncodes(
          Schema.set[Value],
          Set(Value(0, true), Value(1, false)),
          JsonCodec.Encoder.charSequenceToByteChunk(
            """[{"first":0,"second":true},{"first":1,"second":false}]"""
          )
        )
      },
      testM("of records") {
        checkM(for {
          (left, a)       <- SchemaGen.anyRecordAndValue
          primitiveSchema <- SchemaGen.anyPrimitive
        } yield (Schema.EitherSchema(left, primitiveSchema), Left(a))) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      testM("of records of records") {
        checkM(for {
          (left, _)  <- SchemaGen.anyRecordOfRecordsAndValue
          (right, b) <- SchemaGen.anyRecordOfRecordsAndValue
        } yield (Schema.EitherSchema(left, right), Right(b))) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      testM("mixed") {
        checkM(for {
          (left, _)      <- SchemaGen.anyEnumerationAndValue
          (right, value) <- SchemaGen.anySequenceAndValue
        } yield (Schema.EitherSchema(left, right), Right(value))) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      }
    ),
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
      },
      testM("of Map") {
        checkM(SchemaGen.anyMapAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      testM("of Set") {
        checkM(SchemaGen.anySetAndValue) {
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
    suite("sequence")(
      testM("of primitives") {
        checkM(SchemaGen.anySequenceAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      testM("of records") {
        checkM(SchemaGen.anyCaseClassAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.chunk(schema), Chunk.fill(3)(value))
        }
      },
      testM("of java.time.ZoneOffset") {
        //FIXME test independently because including ZoneOffset in StandardTypeGen.anyStandardType wreaks havoc.
        checkM(Gen.chunkOf(JavaTimeGen.anyZoneOffset)) { chunk =>
          assertEncodesThenDecodes(
            Schema.chunk(Schema.Primitive(StandardType.ZoneOffsetType)),
            chunk
          )
        }
      },
      testM("of map") {
        checkM(SchemaGen.anyMapAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.chunk(schema), Chunk.fill(3)(value))
        }
      },
      testM("of set") {
        checkM(SchemaGen.anySetAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.chunk(schema), Chunk.fill(3)(value))
        }
      }
    ),
    suite("map")(
      testM("encodes and decodes a Map") {
        checkM(SchemaGen.anyMapAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      }
    ),
    suite("set")(
      testM("encodes and decodes a Set") {
        checkM(SchemaGen.anySetAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      }
    ),
    suite("case class")(
      testM("basic") {
        checkM(searchRequestGen) { value =>
          assertEncodesThenDecodes(searchRequestSchema, value)
        }
      },
      testM("object") {
        assertEncodesThenDecodes(schemaObject, Singleton)
      }
    ),
    suite("record")(
      testM("any") {
        checkM(SchemaGen.anyRecordAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      testM("minimal test case") {
        SchemaGen.anyRecordAndValue.runHead.flatMap {
          case Some((schema, value)) =>
            val key      = new String(Array('\u0007', '\n'))
            val embedded = Schema.record(Schema.Field(key, schema))
            assertEncodesThenDecodes(embedded, ListMap(key -> value))
          case None => ZIO.fail("Should never happen!")
        }
      },
      testM("record of records") {
        checkM(SchemaGen.anyRecordOfRecordsAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      testM("of primitives") {
        checkM(SchemaGen.anyRecordAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      testM("of ZoneOffsets") {
        checkM(JavaTimeGen.anyZoneOffset) { zoneOffset =>
          assertEncodesThenDecodes(
            Schema.record(Schema.Field("zoneOffset", Schema.Primitive(StandardType.ZoneOffsetType))),
            ListMap[String, Any]("zoneOffset" -> zoneOffset)
          )
        }
      },
      testM("of record") {
        assertEncodesThenDecodes(
          nestedRecordSchema,
          ListMap[String, Any]("l1" -> "s", "l2" -> ListMap[String, Any]("foo" -> "s", "bar" -> 1))
        )
      }
    ),
    suite("enumeration")(
      testM("of primitives") {
        assertEncodesThenDecodes(
          enumSchema,
          "foo"
        )
      },
      testM("ADT") {
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
      testM("any") {
        checkM(SchemaGen.anyTransformAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      }
    ),
    suite("any schema")(
      testM("leaf") {
        checkM(SchemaGen.anyLeafAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      testM("recursive schema") {
        checkM(SchemaGen.anyTreeAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      testM("recursive data type") {
        checkM(SchemaGen.anyRecursiveTypeAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      }
    )
  )

  private def assertEncodes[A](schema: Schema[A], value: A, chunk: Chunk[Byte]) = {
    val stream = ZStream
      .succeed(value)
      .transduce(JsonCodec.encoder(schema))
      .runCollect
    assertM(stream)(equalTo(chunk))
  }

  private def assertEncodesJson[A](schema: Schema[A], value: A, json: String) = {
    val stream = ZStream
      .succeed(value)
      .transduce(JsonCodec.encoder(schema))
      .runCollect
      .map(chunk => new String(chunk.toArray))
    assertM(stream)(equalTo(json))
  }

  private def assertEncodesJson[A](schema: Schema[A], value: A)(implicit enc: JsonEncoder[A]) = {
    val stream = ZStream
      .succeed(value)
      .transduce(JsonCodec.encoder(schema))
      .runCollect
    assertM(stream)(equalTo(jsonEncoded(value)))
  }

  private def assertDecodesToError[A](schema: Schema[A], json: CharSequence, errors: List[JsonError]) = {
    val stream = ZStream
      .fromChunk(JsonCodec.Encoder.charSequenceToByteChunk(json))
      .transduce(JsonCodec.decoder(schema))
      .catchAll(ZStream.succeed[String](_))
      .runHead
    assertM(stream)(isSome(equalTo(JsonError.render(errors))))
  }

  private def assertDecodes[A](schema: Schema[A], value: A, chunk: Chunk[Byte]) = {
    val result = ZStream.fromChunk(chunk).transduce(JsonCodec.decoder(schema)).runCollect
    assertM(result)(equalTo(Chunk(value)))
  }

  private def assertEncodesThenDecodes[A](schema: Schema[A], value: A, print: Boolean = false) = {
    val result = ZStream
      .succeed(value)
      .tap(value => putStrLn(s"Input Value: $value").when(print).ignore)
      .transduce(JsonCodec.encoder(schema))
      .runCollect
      .tap(encoded => putStrLn(s"Encoded: ${new String(encoded.toArray)}").when(print).ignore)
      .flatMap { encoded =>
        ZStream
          .fromChunk(encoded)
          .transduce(JsonCodec.decoder(schema))
          .runCollect
          .tapError { err =>
            putStrLnErr(s"Decoding failed for input ${new String(encoded.toArray)}\nError Message: $err")
          }
      }
      .tap(decoded => putStrLn(s"Decoded: $decoded").when(print).ignore)
      .either
    assertM(result)(isRight(equalTo(Chunk(value))))
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
    JsonEncoder.chunk(keyEncoder.both(valueEncoder)).contramap[Map[K, V]](m => Chunk.fromIterable(m))

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

  private val searchRequestGen: Gen[Random with Sized, SearchRequest] =
    for {
      query      <- Gen.anyString
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
