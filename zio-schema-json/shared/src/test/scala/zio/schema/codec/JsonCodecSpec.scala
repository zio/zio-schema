package zio.schema.codec

import java.time.{ ZoneId, ZoneOffset }

import scala.collection.immutable.ListMap

import zio.Console._
import zio._
import zio.json.JsonDecoder.JsonError
import zio.json.ast.Json
import zio.json.{ DeriveJsonEncoder, JsonEncoder }
import zio.schema.CaseSet._
import zio.schema._
import zio.schema.annotation._
import zio.schema.codec.DecodeError.ReadError
import zio.schema.codec.JsonCodec.JsonEncoder.charSequenceToByteChunk
import zio.schema.codec.JsonCodecSpec.PaymentMethod.{ CreditCard, PayPal, WireTransfer }
import zio.schema.codec.JsonCodecSpec.Subscription.{ OneTime, Recurring }
import zio.schema.meta.MetaSchema
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._

object JsonCodecSpec extends ZIOSpecDefault {

  case class Person(name: String, age: Int)

  val personSchema: Schema[Person] = DeriveSchema.gen[Person]

  def spec: Spec[TestEnvironment, Any] =
    suite("JsonCodec Spec")(
      encoderSuite,
      decoderSuite,
      encoderDecoderSuite
    ) @@ timeout(180.seconds)

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
      },
      test("Currency") {
        assertEncodesJson(
          Schema.Primitive(StandardType.CurrencyType),
          java.util.Currency.getInstance("USD")
        )
      } @@ TestAspect.jvmOnly
    ),
    suite("fallback")(
      test("left") {
        assertEncodesJson(
          Schema.Fallback(Schema[Int], Schema[String]),
          Fallback.Left(3),
          "3"
        )
      },
      test("right") {
        assertEncodesJson(
          Schema.Fallback(Schema[Int], Schema[String]),
          Fallback.Right("hello"),
          """"hello""""
        )
      },
      test("both") {
        assertEncodesJson(
          Schema.Fallback(Schema[Int], Schema[String]),
          Fallback.Both(3, "hello"),
          """[3,"hello"]"""
        )
      },
      test("both pretty printed") {
        val json = JsonCodec
          .jsonCodec(Schema.Fallback(Schema.set[Int], Schema.chunk[String]))
          .encoder
          .encodeJson(
            Fallback.Both(Set(3), Chunk("hello")),
            Some(0)
          )
        assert(json)(
          equalTo(
            """[
              |  [
              |    3
              |  ],
              |  [
              |    "hello"
              |  ]
              |]""".stripMargin
          )
        )
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
    suite("case class")(
      test("backticked field name") {
        assertEncodesJson(
          Schema[BacktickedFieldName],
          BacktickedFieldName("test"),
          """{"x-api-key":"test"}"""
        )
      }
    ),
    suite("optional field annotation")(
      test("list empty") {
        assertEncodesJson(
          Schema[WithOptField],
          WithOptField(Nil, Map("foo" -> 1)),
          """{"map":{"foo":1}}"""
        )
      },
      test("map empty") {
        assertEncodesJson(
          Schema[WithOptField],
          WithOptField(List("foo"), Map.empty),
          """{"list":["foo"]}"""
        )
      },
      test("all empty") {
        assertEncodesJson(
          Schema[WithOptField],
          WithOptField(Nil, Map.empty),
          """{}"""
        )
      }
    ),
    suite("empty collections config")(
      test("list empty") {
        assertEncodesJson(
          Schema[ListAndMapAndOption],
          ListAndMapAndOption(Nil, Map("foo" -> 1), Some("foo")),
          """{"map":{"foo":1},"option":"foo"}""",
          JsonCodec.Config(ignoreEmptyCollections = true, explicitNulls = false)
        )
      },
      test("map empty") {
        assertEncodesJson(
          Schema[ListAndMapAndOption],
          ListAndMapAndOption(List("foo"), Map.empty, Some("foo")),
          """{"list":["foo"],"option":"foo"}""",
          JsonCodec.Config(ignoreEmptyCollections = true, explicitNulls = false)
        )
      },
      test("option empty") {
        assertEncodesJson(
          Schema[ListAndMapAndOption],
          ListAndMapAndOption(List("foo"), Map("foo" -> 1), None),
          """{"list":["foo"],"map":{"foo":1}}""",
          JsonCodec.Config(ignoreEmptyCollections = true, explicitNulls = false)
        )
      },
      test("all empty") {
        assertEncodesJson(
          Schema[ListAndMapAndOption],
          ListAndMapAndOption(Nil, Map.empty, None),
          """{}""",
          JsonCodec.Config(ignoreEmptyCollections = true, explicitNulls = false)
        )
      },
      test("all empty, but don't ignore empty collections") {
        assertEncodesJson(
          Schema[ListAndMapAndOption],
          ListAndMapAndOption(Nil, Map.empty, None),
          """{"list":[],"map":{},"option":null}""",
          JsonCodec.Config(ignoreEmptyCollections = false, explicitNulls = true)
        )
      }
    ),
    suite("tuple")(
      test("of primitives") {
        assertEncodesJson(
          Schema.Tuple2(
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
          charSequenceToByteChunk(
            """[[{"name":"a","index":0},{"first":0,"second":true}],[{"name":"b","index":1},{"first":1,"second":false}]]"""
          )
        )
      },
      test("of simple keys and values") {
        assertEncodes(
          Schema.map[Int, Value],
          Map(0 -> Value(0, true), 1 -> Value(1, false)),
          charSequenceToByteChunk(
            """{"0":{"first":0,"second":true},"1":{"first":1,"second":false}}"""
          )
        )
      },
      test("of simple keys and values where the key's schema is lazy") {
        assertEncodes(
          Schema.map[Int, Value](Schema.defer(Schema[Int]), Schema[Value]),
          Map(0 -> Value(0, true), 1 -> Value(1, false)),
          charSequenceToByteChunk(
            """{"0":{"first":0,"second":true},"1":{"first":1,"second":false}}"""
          )
        )
      },
      test("of complex keys with transformation to primitive keys") {
        assertEncodes(
          Schema
            .map[KeyWrapper, ValueWrapper],
          Map(
            KeyWrapper("wrapped_key_1") -> ValueWrapper(value = "some_value"),
            KeyWrapper("wrapped_key_2") -> ValueWrapper(value = "some_other_value")
          ),
          charSequenceToByteChunk(
            """{"wrapped_key_1":{"value":"some_value"},"wrapped_key_2":{"value":"some_other_value"}}"""
          )
        )
      }
    ),
    suite("Set")(
      test("of complex values") {
        assertEncodes(
          Schema.set[Value],
          Set(Value(0, true), Value(1, false)),
          charSequenceToByteChunk(
            """[{"first":0,"second":true},{"first":1,"second":false}]"""
          )
        )
      }
    ),
    suite("record")(
      test("missing fields should be replaced by default values") {
        assertDecodes(
          recordSchema,
          ListMap[String, Any]("foo" -> "s", "bar" -> 1),
          charSequenceToByteChunk("""{"foo":"s"}""")
        )
      },
      test("of primitives") {
        assertEncodes(
          recordSchema,
          ListMap[String, Any]("foo" -> "s", "bar" -> 1),
          charSequenceToByteChunk("""{"foo":"s","bar":1}""")
        )
      },
      test("of records") {
        assertEncodes(
          nestedRecordSchema,
          ListMap[String, Any]("l1" -> "s", "l2" -> ListMap("foo" -> "s", "bar" -> 1)),
          charSequenceToByteChunk("""{"l1":"s","l2":{"foo":"s","bar":1}}""")
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
      },
      test("record with option fields") {
        assertEncodes(
          recordWithOptionSchema,
          ListMap[String, Any]("foo" -> Some("s"), "bar" -> None),
          charSequenceToByteChunk("""{"foo":"s"}""")
        )
      },
      test("record with option fields and flag to encode nulls") {
        assertEncodes(
          recordWithOptionSchema,
          ListMap[String, Any]("foo" -> Some("s"), "bar" -> None),
          charSequenceToByteChunk("""{"foo":"s","bar":null}"""),
          JsonCodec.Config.default.copy(explicitNulls = true)
        )
      },
      test("case class with option fields omitted when empty") {
        assertEncodes(
          WithOptionFields.schema,
          WithOptionFields(Some("s"), None),
          charSequenceToByteChunk("""{"a":"s"}""")
        )
      }
    ),
    suite("enumeration")(
      test("of primitives") {
        assertEncodes(
          enumSchema,
          ("foo"),
          charSequenceToByteChunk("""{"string":"foo"}""")
        )
      },
      test("ADT") {
        assertEncodes(
          Schema[Enumeration],
          Enumeration(StringValue("foo")),
          charSequenceToByteChunk("""{"oneOf":{"StringValue":{"value":"foo"}}}""")
        )
      },
      test("ADT with annotation") {
        assertEncodes(
          Schema[Enumeration2],
          Enumeration2(StringValue2("foo2")),
          charSequenceToByteChunk("""{"oneOf":{"_type":"StringValue2","value":"foo2"}}""")
        )
      },
      test("transient field annotation") {
        assertEncodes(
          searchRequestWithTransientFieldSchema,
          SearchRequestWithTransientField("foo", 10, 20, "bar"),
          charSequenceToByteChunk("""{"query":"foo","pageNumber":10,"resultPerPage":20}""")
        )
      },
      test("case name annotation") {
        assertEncodes(
          PaymentMethod.schema,
          WireTransfer("foo", "bar"),
          charSequenceToByteChunk("""{"wire_transfer":{"accountNumber":"foo","bankCode":"bar"}}""")
        )
      },
      test("transient case annotation") {
        assertEncodes(
          PaymentMethod.schema,
          PayPal("foo@bar.com"),
          charSequenceToByteChunk("""{}""")
        )
      },
      test("case name annotation with discriminator") {
        assertEncodes(
          Subscription.schema,
          Recurring("monthly", 10),
          charSequenceToByteChunk("""{"type":"recurring","period":"monthly","amount":10}""")
        )
      },
      test("case name annotation with empty fields") {
        assertEncodes(
          Subscription.schema,
          Subscription.Unlimited(None),
          charSequenceToByteChunk("""{"type":"unlimited"}""")
        )
      },
      test("pretty printing with discriminator field") {
        val json = JsonCodec
          .jsonCodec(Schema[OneOf4])
          .encoder
          .encodeJson(
            RecordExampleWithDiscriminator(f1 = Some("test"), f2 = None),
            Some(0)
          )
        assert(json)(
          equalTo(
            """{
              |  "type" : "RecordExampleWithDiscriminator",
              |  "$f1" : "test"
              |}""".stripMargin
          )
        )
      },
      test("pretty printing with discriminator key") {
        val json = JsonCodec
          .jsonCodec(Schema[OneOf])
          .encoder
          .encodeJson(
            StringValue("test"),
            Some(0)
          )
        assert(json)(
          equalTo(
            """{
              |  "StringValue" : {
              |    "value" : "test"
              |  }
              |}""".stripMargin
          )
        )
      },
      suite("with no discriminator")(
        test("example 1") {
          assertEncodes(
            Prompt.schema,
            Prompt.Single("hello"),
            charSequenceToByteChunk("""{"value":"hello"}""")
          )
        },
        test("example 2") {
          assertEncodes(
            Prompt.schema,
            Prompt.Multiple(List("hello", "world")),
            charSequenceToByteChunk("""{"value":["hello","world"]}""")
          )
        }
      )
    ),
    suite("dynamic direct mapping")(
      test("record") {
        assertEncodes(
          Schema.dynamicValue.annotate(directDynamicMapping()),
          DynamicValue.Record(
            TypeId.Structural,
            ListMap(
              "foo" -> DynamicValue.Primitive("s", StandardType.StringType),
              "bar" -> DynamicValue.Primitive(1, StandardType.IntType)
            )
          ),
          charSequenceToByteChunk("""{"foo":"s","bar":1}""")
        )
      }
    ),
    suite("zio.json.ast.Json encoding")(
      test("Json.Obj") {
        assertEncodes(
          zio.schema.codec.json.schemaJson,
          Json.Obj("foo" -> Json.Str("bar"), "null" -> Json.Null),
          charSequenceToByteChunk("""{"foo":"bar","null":null}""")
        )
      },
      test("Json.Arr") {
        assertEncodes(
          zio.schema.codec.json.schemaJson,
          Json.Arr(Json.Str("foo"), Json.Num(1)),
          charSequenceToByteChunk("""["foo",1]""")
        )
      },
      test("Json.Num Int") {
        assertEncodes(
          zio.schema.codec.json.schemaJson,
          Json.Num(1),
          charSequenceToByteChunk("""1""")
        )
      },
      test("Json.Num Long") {
        assertEncodes(
          zio.schema.codec.json.schemaJson,
          Json.Num(1L),
          charSequenceToByteChunk("""1""")
        )
      },
      test("Json.Num Double") {
        assertEncodes(
          zio.schema.codec.json.schemaJson,
          Json.Num(1.1),
          charSequenceToByteChunk("""1.1""")
        )
      },
      test("Json.Str") {
        assertEncodes(
          zio.schema.codec.json.schemaJson,
          Json.Str("foo"),
          charSequenceToByteChunk(""""foo"""")
        )
      },
      test("Json.Bool") {
        assertEncodes(
          zio.schema.codec.json.schemaJson,
          Json.Bool(true),
          charSequenceToByteChunk("""true""")
        )
      },
      test("Json.Null") {
        assertEncodes(
          zio.schema.codec.json.schemaJson,
          Json.Null,
          charSequenceToByteChunk("""null""")
        )
      }
    ),
    suite("Generic Record")(
      test("Do not encode transient field") {
        assertEncodes(
          RecordExample.schema,
          RecordExample(f1 = "test", f3 = Some("transient"), f20 = None, f21 = Vector.empty, f22 = Nil),
          charSequenceToByteChunk(
            """{"$f1":"test","f21":[],"f22":[]}""".stripMargin
          )
        )
      }
    ),
    suite("EnumN")(
      test("Respects the case name annotation") {
        assertEncodesJson(
          Enum23Cases.schema,
          Enum23Cases.Case1("foo"),
          """{"NumberOne":{"value":"foo"}}"""
        )
      }
    ),
    suite("Streams")(
      suite("Streams of integers")(
        test("Encodes a stream with multiple integers") {
          assertEncodesMany(Schema[Int], 1 to 5, charSequenceToByteChunk("1\n2\n3\n4\n5"))
        },
        test("Encodes a stream with multiple integers to an array") {
          assertEncodesMany(
            Schema[Int],
            1 to 5,
            charSequenceToByteChunk("[1,2,3,4,5]"),
            JsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)
          )
        },
        test("Decodes a stream with multiple integers separated by newlines") {
          assertDecodesMany(Schema[Int], Chunk.fromIterable(1 to 5), charSequenceToByteChunk("1\n2\n3\n4\n5"))
        },
        test("Decodes a stream with multiple integers separated by spaces") {
          assertDecodesMany(Schema[Int], Chunk.fromIterable(1 to 5), charSequenceToByteChunk("1 2 3 4 5"))
        },
        test("Decodes a stream with multiple integers separated by commas and other non JSON number characters") {
          assertDecodesMany(Schema[Int], Chunk.fromIterable(1 to 5), charSequenceToByteChunk("1 2, 3;;; 4x5"))
        },
        test("Decodes a stream with multiple integers encoded as an array") {
          assertDecodesMany(
            Schema[Int],
            Chunk.fromIterable(1 to 5),
            charSequenceToByteChunk("[1,2,3,4,5]"),
            JsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)
          )
        },
        test("Decodes a stream with multiple integers encoded as an array with additional whitespace") {
          assertDecodesMany(
            Schema[Int],
            Chunk.fromIterable(1 to 5),
            charSequenceToByteChunk("""
                                      |   [1,
                                      |2,3,
                                      |4,   5]   """.stripMargin),
            JsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)
          )
        }
      ),
      suite("Streams of booleans")(
        test("Encodes a stream with multiple booleans") {
          assertEncodesMany(Schema[Boolean], List(true, true, false), charSequenceToByteChunk("true\ntrue\nfalse"))
        },
        test("Encodes a stream with multiple booleans to an array") {
          assertEncodesMany(
            Schema[Boolean],
            List(true, true, false),
            charSequenceToByteChunk("[true,true,false]"),
            JsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)
          )
        },
        test("Decodes a stream with multiple booleans separated by newlines") {
          assertDecodesMany(Schema[Boolean], Chunk(true, true, false), charSequenceToByteChunk("true\ntrue\nfalse"))
        },
        test("Decodes a stream with multiple booleans separated by spaces") {
          assertDecodesMany(Schema[Boolean], Chunk(true, true, false), charSequenceToByteChunk("true true false"))
        },
        test("Decodes a stream with multiple booleans as an array") {
          assertDecodesMany(
            Schema[Boolean],
            Chunk(true, true, false),
            charSequenceToByteChunk("[true, true, false]"),
            JsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)
          )
        },
        test(
          "Decodes a stream with multiple booleans separated by commas and other non JSON boolean characters and not separated at all"
        ) {
          assertDecodesMany(
            Schema[Boolean],
            Chunk(true, true, false, false),
            charSequenceToByteChunk("true true, falsefalse")
          )
        }
      ),
      suite("Streams of strings")(
        test("Encodes a stream with multiple strings") {
          assertEncodesMany(Schema[String], List("a", "b", "c"), charSequenceToByteChunk("\"a\"\n\"b\"\n\"c\""))
        },
        test("Encodes a stream with multiple strings as an array") {
          assertEncodesMany(
            Schema[String],
            List("a", "b", "c"),
            charSequenceToByteChunk("[\"a\",\"b\",\"c\"]"),
            JsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)
          )
        },
        test("Decodes a stream with multiple strings separated by newlines") {
          assertDecodesMany(Schema[String], Chunk("a", "b", "c"), charSequenceToByteChunk("\"a\"\n\"b\"\n\"c\""))
        },
        test("Decodes a stream with multiple strings as an array") {
          assertDecodesMany(
            Schema[String],
            Chunk("a", "b", "c"),
            charSequenceToByteChunk("[\"a\", \"b\",\n\"c\"]"),
            JsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)
          )
        },
        test("Decodes a stream with multiple strings separated by spaces, commas and not separated at all") {
          assertDecodesMany(Schema[String], Chunk("a", "b", "c", "d"), charSequenceToByteChunk(""""a" "b","c""d""""))
        }
      ),
      suite("Stream of records")(
        test("Encodes a stream with multiple records") {
          assertEncodesMany(
            personSchema,
            List(
              Person("Alice", 1),
              Person("Bob", 2),
              Person("Charlie", 3)
            ),
            charSequenceToByteChunk(
              """{"name":"Alice","age":1}
                |{"name":"Bob","age":2}
                |{"name":"Charlie","age":3}""".stripMargin
            )
          )
        },
        test("Encodes a stream with multiple records as an array") {
          assertEncodesMany(
            personSchema,
            List(
              Person("Alice", 1),
              Person("Bob", 2),
              Person("Charlie", 3)
            ),
            charSequenceToByteChunk(
              """[{"name":"Alice","age":1},{"name":"Bob","age":2},{"name":"Charlie","age":3}]"""
            ),
            JsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)
          )
        },
        test("Decodes a stream with multiple records separated by newlines") {
          assertDecodesMany(
            personSchema,
            Chunk(
              Person("Alice", 1),
              Person("Bob", 2),
              Person("Charlie", 3)
            ),
            charSequenceToByteChunk(
              """{"name":"Alice","age":1}
                |{"name":"Bob","age":2}
                |{"name":"Charlie","age":3}""".stripMargin
            )
          )
        },
        test("Decodes a stream with multiple records, not separated with internal newlines") {
          assertDecodesMany(
            personSchema,
            Chunk(
              Person("Alice", 1),
              Person("Bob", 2),
              Person("Charlie", 3)
            ),
            charSequenceToByteChunk(
              """{"name":"Alice","age":1}{"name":"Bob",
                |"age"
                |:2}{"name":"Charlie","age":3}""".stripMargin
            )
          )
        },
        test("Decodes a stream with multiple records formatted as an array") {
          assertDecodesMany(
            personSchema,
            Chunk(
              Person("Alice", 1),
              Person("Bob", 2),
              Person("Charlie", 3)
            ),
            charSequenceToByteChunk(
              """[{"name":"Alice","age":1},   {"name":"Bob","age":2},
                |{"name":"Charlie","age"
                |: 3}]""".stripMargin
            ),
            JsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)
          )
        },
        test("Encodes a stream with no records") {
          assertEncodesMany(
            personSchema,
            List.empty[Person],
            charSequenceToByteChunk("")
          )
        },
        test("Encodes a stream with no records") {
          assertEncodesMany(
            personSchema,
            List.empty[Person],
            charSequenceToByteChunk("[]"),
            JsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)
          )
        },
        test("Decodes a stream with no records") {
          assertDecodesMany(
            personSchema,
            Chunk.empty,
            charSequenceToByteChunk("")
          )
        },
        test("Decodes a stream with no records from an array") {
          assertDecodesMany(
            personSchema,
            Chunk.empty,
            charSequenceToByteChunk("   [ ]  "),
            JsonCodec.Config(ignoreEmptyCollections = false, treatStreamsAsArrays = true)
          )
        }
      )
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
      ),
      test("Currency") {
        check(Gen.currency)(
          currency => assertDecodes(Schema[java.util.Currency], currency, stringify(currency.getCurrencyCode))
        )
      } @@ TestAspect.jvmOnly
    ),
    suite("Generic record")(
      test("with extra fields") {
        assertDecodes(
          recordSchema,
          ListMap[String, Any]("foo" -> "s", "bar" -> 1),
          charSequenceToByteChunk("""{"foo":"s","bar":1,"baz":2}""")
        )
      },
      test("with empty optional and collection fields without default values") {
        assertDecodes(
          RecordExample.schema,
          RecordExample(f1 = "test", f2 = None, f20 = None, f21 = Vector.empty, f22 = Nil),
          charSequenceToByteChunk("""{"$f1":"test"}""")
        )
      },
      test("missing required fields") {
        assertDecodesToError(
          RecordExample.schema,
          """{}""",
          JsonError.Message("missing") :: JsonError.ObjectAccess("$f1") :: Nil
        )
      },
      test("aliased field") {
        assertDecodes(
          RecordExample.schema,
          RecordExample(f1 = "test", f2 = Some("alias"), f20 = None, f21 = Vector.empty, f22 = Nil),
          charSequenceToByteChunk("""{"$f1":"test", "field2":"alias"}""")
        )
      },
      test("reject extra fields") {
        assertDecodes(
          RecordExample.schema.annotate(rejectExtraFields()),
          RecordExample(f1 = "test", f20 = None, f21 = Vector.empty, f22 = Nil),
          charSequenceToByteChunk("""{"$f1":"test", "extraField":"extra"}""")
        ).flip.map(err => assertTrue(err.getMessage() == "(extra field)"))
      },
      test("reject duplicated fields") {
        assertDecodesToError(
          recordSchema,
          """{"foo":"test","bar":10,"foo":10}""",
          JsonError.Message("duplicate") :: JsonError.ObjectAccess("foo") :: Nil
        ) &>
          assertDecodesToError(
            recordSchema,
            """{"bar":10,"foo":"test","bar":"100"}""",
            JsonError.Message("duplicate") :: JsonError.ObjectAccess("bar") :: Nil
          )
      },
      test("optional field with schema or annotated default value") {
        assertDecodes(
          RecordExampleWithOptField.schema,
          RecordExampleWithOptField(f1 = Some("test"), f2 = None, f4 = "", f5 = "hello"),
          charSequenceToByteChunk("""{"$f1":"test"}""")
        )
      }
    ),
    suite("EnumN")(
      test("Respects the case name annotation") {
        assertDecodes(
          Enum23Cases.schema,
          Enum23Cases.Case1("foo"),
          charSequenceToByteChunk("""{"NumberOne":{"value":"foo"}}""")
        )
      },
      test("Respects case aliases") {
        assertDecodes(
          Enum23Cases.schema,
          Enum23Cases.Case1("foo"),
          charSequenceToByteChunk("""{"One":{"value":"foo"}}""")
        )
      }
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
    suite("fallback")(
      test("correctly fallbacks to right") {
        assertDecodes(
          Schema.Fallback(Schema[Int], Schema[String]),
          Fallback.Right("hello"),
          charSequenceToByteChunk("""["wrong","hello"]""")
        )
      },
      test("correctly fallbacks to left") {
        assertDecodes(
          Schema.Fallback(Schema[Int], Schema[String]),
          Fallback.Left(30),
          charSequenceToByteChunk("""[30,"hello"]""")
        )
      },
      test("correctly fallbacks to right with full decode") {
        assertDecodes(
          Schema.Fallback(Schema[Int], Schema[String], true),
          Fallback.Right("hello"),
          charSequenceToByteChunk("""[ "wrong", "hello"]""")
        )
      },
      test("correctly fallbacks to both with full decode") {
        assertDecodes(
          Schema.Fallback(Schema[Int], Schema[String], true),
          Fallback.Both(30, "hello"),
          charSequenceToByteChunk("""[ 30, "hello"]""")
        )
      },
      test("correctly fallbacks to left with full decode") {
        assertDecodes(
          Schema.Fallback(Schema[Int], Schema[String], true),
          Fallback.Left(30),
          charSequenceToByteChunk("""[30,30]""")
        )
      }
    ),
    suite("case class")(
      test("case object") {
        assertDecodes(schemaObject, Singleton, charSequenceToByteChunk("{}"))
      },
      test("optional") {
        assertDecodes(
          optionalSearchRequestSchema,
          OptionalSearchRequest("test", 0, 10, Schema[String].defaultValue.getOrElse("")),
          charSequenceToByteChunk("""{"query":"test","pageNumber":0,"resultPerPage":10}""")
        )
      },
      test("reject extra fields") {
        assertDecodesToError(
          PersonWithRejectExtraFields.schema,
          """{"name":"test","age":10,"extraField":10}""",
          JsonError.Message("extra field") :: Nil
        ) &>
          assertDecodesToError(
            schemaObject.annotate(rejectExtraFields()),
            """{"extraField":10}""",
            JsonError.Message("extra field") :: Nil
          )
      },
      test("reject duplicated fields") {
        assertDecodesToError(
          personSchema,
          """{"name":"test","age":10,"name":10}""",
          JsonError.Message("duplicate") :: JsonError.ObjectAccess("name") :: Nil
        ) &>
          assertDecodesToError(
            personSchema,
            """{"age":10,"name":"test","age":"100"}""",
            JsonError.Message("duplicate") :: JsonError.ObjectAccess("age") :: Nil
          )
      },
      test("transient field annotation with default value in class definition") {
        assertDecodes(
          searchRequestWithTransientFieldSchema,
          SearchRequestWithTransientField("test", 0, 10),
          charSequenceToByteChunk("""{"query":"test","pageNumber":0,"resultPerPage":10}""")
        )
      },
      test("transient field annotation with default value implicitly available for the field type") {
        case class CaseClassWithTransientField(transient: String)
        assertDecodes(
          Schema.CaseClass1[String, CaseClassWithTransientField](
            id0 = TypeId.fromTypeName("SearchRequestWithTransientField"),
            field0 = Schema.Field(
              name0 = "transient",
              schema0 = Schema[String],
              get0 = _.transient,
              set0 = (x, transient) => x.copy(transient = transient),
              annotations0 = Chunk(new transientField())
            ),
            defaultConstruct0 = new CaseClassWithTransientField(_)
          ),
          CaseClassWithTransientField(Schema[String].defaultValue.toOption.get),
          charSequenceToByteChunk("""{}""")
        )
      },
      test("fieldDefaultValue") {
        assertDecodes(
          fieldDefaultValueSearchRequestSchema,
          FieldDefaultValueSearchRequest("test", 0, 10, "test"),
          charSequenceToByteChunk("""{"query":"test","pageNumber":0,"resultPerPage":10}""")
        )
      },
      test("backticked field name") {
        assertDecodes(
          BacktickedFieldName.schema,
          BacktickedFieldName("test"),
          charSequenceToByteChunk("""{"x-api-key":"test"}""")
        )
      },
      test("field name with alias - id") {
        assertDecodes(
          Order.schema,
          Order(1, BigDecimal.valueOf(10), "test"),
          charSequenceToByteChunk("""{"id":1,"value":10,"description":"test"}""")
        )
      },
      test("field name with alias - order_id") {
        assertDecodes(
          Order.schema,
          Order(1, BigDecimal.valueOf(10), "test"),
          charSequenceToByteChunk("""{"id":1,"value":10,"description":"test"}""")
        )
      },
      test("old field name rejected") {
        assertDecodesToError(
          Order.schema,
          """{"order-id":1,"value":10,"description":"test"}""",
          JsonError.Message("missing") :: JsonError.ObjectAccess("orderId") :: Nil
        )
      },
      test("field name with alias - no alias") {
        assertDecodes(
          Order.schema,
          Order(1, BigDecimal.valueOf(10), "test"),
          charSequenceToByteChunk("""{"orderId":1,"value":10,"description":"test"}""")
        )
      },
      test("with option fields encoded as null") {
        assertDecodes(
          recordWithOptionSchema,
          ListMap[String, Any]("foo" -> Some("s"), "bar" -> None),
          charSequenceToByteChunk("""{"foo":"s","bar":null}""")
        )
      },
      test("with transient fields encoded as implicitly available schema default values") {
        assertDecodes(
          recordWithTransientSchema,
          ListMap[String, Any]("foo" -> "", "bar" -> 0),
          charSequenceToByteChunk("""{}""")
        )
      },
      test("case class with option fields encoded as null") {
        assertDecodes(
          WithOptionFields.schema,
          WithOptionFields(Some("s"), None),
          charSequenceToByteChunk("""{"a":"s","b":null}""")
        )
      },
      test("case class with int option field present (at end) from pretty printed json") {
        assertDecodes(
          WithOptionFields.schema,
          WithOptionFields(Some("s"), Some(1)),
          charSequenceToByteChunk(
            """
              |{
              |  "a": "s",
              |  "b": 1
              |}
              |""".stripMargin
          )
        )
      },
      test("case class with option fields omitted when empty") {
        assertDecodes(
          WithOptionFields.schema,
          WithOptionFields(Some("s"), None),
          charSequenceToByteChunk("""{"a":"s"}""")
        )
      },
      test("case class with complex option field correctly decodes") {
        assertDecodes(
          WithComplexOptionField.schema,
          WithComplexOptionField(Some(Order(1, BigDecimal.valueOf(10), "test"))),
          charSequenceToByteChunk("""{"order":{"id":1,"value":10,"description":"test"}}""")
        )
      }
    ),
    suite("case class with more than 64 fields")(
      test("required and optional fields") {
        assertDecodes(
          BigProduct.schema,
          BigProduct(f00 = true, f67 = None, f68 = Nil, f69 = Vector.empty),
          charSequenceToByteChunk("""{"f00":true}""")
        )
      },
      test("missing requireda fields") {
        assertDecodesToError(
          BigProduct.schema,
          """{}""",
          JsonError.Message("missing") :: JsonError.ObjectAccess("f00") :: Nil
        )
      },
      test("reject extra fields") {
        assertDecodesToError(
          BigProduct.schema.annotate(rejectExtraFields()),
          """{"f00":true,"extraField":10}""",
          JsonError.Message("extra field") :: Nil
        )
      },
      test("reject duplicated fields") {
        assertDecodesToError(
          BigProduct.schema,
          """{"f00":true,"age":10,"f00":false}""",
          JsonError.Message("duplicate") :: JsonError.ObjectAccess("f00") :: Nil
        )
      },
      test("field name with alias - id") {
        assertDecodes(
          BigProduct.schema,
          BigProduct(f00 = true, f01 = Some(123.toByte), f67 = None, f68 = Nil, f69 = Vector.empty),
          charSequenceToByteChunk("""{"f00":true,"f-01":123}""")
        )
      }
    ),
    suite("enums")(
      test("case name aliases - default") {
        assertDecodes(
          PaymentMethod.schema,
          CreditCard("foo", 12, 2022),
          charSequenceToByteChunk("""{"CreditCard":{"number":"foo","expirationMonth":12,"expirationYear":2022}}""")
        )
      },
      test("case name aliases - first alias") {
        assertDecodes(
          PaymentMethod.schema,
          CreditCard("foo", 12, 2022),
          charSequenceToByteChunk("""{"credit_card":{"number":"foo","expirationMonth":12,"expirationYear":2022}}""")
        )
      },
      test("case name aliases - second alias") {
        assertDecodes(
          PaymentMethod.schema,
          CreditCard("foo", 12, 2022),
          charSequenceToByteChunk("""{"cc":{"number":"foo","expirationMonth":12,"expirationYear":2022}}""")
        )
      },
      test("case name") {
        assertDecodes(
          PaymentMethod.schema,
          WireTransfer("foo", "bar"),
          charSequenceToByteChunk("""{"wire_transfer":{"accountNumber":"foo","bankCode":"bar"}}""")
        )
      },
      test("no discriminator")(
        assertDecodesToError(PaymentMethod.schema, "{}", JsonError.Message("missing subtype") :: Nil)
      ),
      test("illegal case")(
        assertDecodesToError(PaymentMethod.schema, """{"cash":{}}""", JsonError.Message("unrecognized subtype") :: Nil)
      )
    ),
    suite("enums - with discriminator")(
      test("case name") {
        assertDecodes(
          Subscription.schema,
          Recurring("monthly", 100),
          charSequenceToByteChunk("""{"type":"recurring","period":"monthly","amount":100}""")
        )
      },
      test("case name aliases - first alias") {
        assertDecodes(
          Subscription.schema,
          OneTime(1000),
          charSequenceToByteChunk("""{"type":"one_time","amount":1000}""")
        )
      },
      test("case name aliases - second alias") {
        assertDecodes(
          Subscription.schema,
          OneTime(1000),
          charSequenceToByteChunk("""{"type":"onetime","amount":1000}""")
        )
      },
      test("case name aliases - type in the middle") {
        assertDecodes(
          Subscription.schema,
          Recurring("monthly", 100),
          charSequenceToByteChunk("""{"period":"monthly","type":"recurring","amount":100}""")
        )
      },
      test("case name aliases - type in the last place") {
        assertDecodes(
          Subscription.schema,
          OneTime(1000),
          charSequenceToByteChunk("""{"amount":1000, "type":"onetime"}""")
        )
      },
      test("case name aliases - type in the last place and the content with an escaped string") {
        @discriminatorName("type")
        sealed trait Example {
          type Content
          def content: Content
        }

        object Example {
          @caseName("JSON")
          final case class JsonInput(content: String) extends Example {
            override type Content = String
          }

          implicit val schema: Schema[Example] = DeriveSchema.gen
        }

        assertDecodes(
          Example.schema,
          Example.JsonInput(
            """"{\n  \"name\": \"John\",\"location\":\"Sydney\",\n  \"email\": \"jdoe@test.com\"\n}""""
          ),
          charSequenceToByteChunk(
            """
              |{
              |  "content": "\"{\\n  \\\"name\\\": \\\"John\\\",\\\"location\\\":\\\"Sydney\\\",\\n  \\\"email\\\": \\\"jdoe@test.com\\\"\\n}\"",
              |  "type": "JSON"
              |}
              |""".stripMargin.trim
          )
        )
      },
      test("case name - illegal discriminator value") {
        assertDecodesToError(
          Subscription.schema,
          """{"amount":1000, "type":123}""",
          JsonError.Message("expected '\"' got '1'") :: JsonError.ObjectAccess("type") :: Nil
        )
      },
      test("case name - empty fields") {
        assertDecodes(
          Subscription.schema,
          Subscription.Unlimited(None),
          charSequenceToByteChunk("""{"type":"unlimited"}""")
        )
      }
    ),
    suite("enums - with no discriminator")(
      test("example 1") {
        assertDecodes(
          Prompt.schema,
          Prompt.Single("hello"),
          charSequenceToByteChunk("""{"value":"hello"}""")
        )
      },
      test("example 2") {
        assertDecodes(
          Prompt.schema,
          Prompt.Multiple(List("hello", "world")),
          charSequenceToByteChunk("""{"value":["hello","world"]}""")
        )
      },
      test("wrong example") {
        assertDecodesToError(
          Prompt.schema,
          "123",
          JsonError.Message("none of the subtypes could decode the data") :: Nil
        )
      }
    ),
    suite("dynamic direct mapping")(
      test("record") {
        assertDecodes(
          Schema.dynamicValue.annotate(directDynamicMapping()),
          DynamicValue.Record(
            TypeId.Structural,
            ListMap(
              "foo" -> DynamicValue.Primitive("s", StandardType.StringType),
              "bar" -> DynamicValue.Primitive(java.math.BigDecimal.valueOf(1), StandardType.BigDecimalType)
            )
          ),
          charSequenceToByteChunk("""{"foo":"s","bar":1}""")
        )
      }
    ),
    suite("Map")(
      test("of complex keys and values") {
        assertDecodes(
          Schema.map[Key, Value],
          Map(Key("a", 0) -> Value(0, true), Key("b", 1) -> Value(1, false)),
          charSequenceToByteChunk(
            """[[{"name":"a","index":0},{"first":0,"second":true}],[{"name":"b","index":1},{"first":1,"second":false}]]"""
          )
        )
      },
      test("of simple keys and values") {
        assertDecodes(
          Schema.map[Int, Value],
          Map(0 -> Value(0, true), 1 -> Value(1, false)),
          charSequenceToByteChunk(
            """{"0":{"first":0,"second":true},"1":{"first":1,"second":false}}"""
          )
        )
      },
      test("of simple keys and values where the key schema is lazy") {
        assertDecodes(
          Schema.map[Int, Value](Schema.defer(Schema[Int]), Schema[Value]),
          Map(0 -> Value(0, true), 1 -> Value(1, false)),
          charSequenceToByteChunk(
            """{"0":{"first":0,"second":true},"1":{"first":1,"second":false}}"""
          )
        )
      },
      test("of primitive keys with transformation to complex keys") {
        assertDecodes(
          Schema
            .map[KeyWrapper, ValueWrapper],
          Map(
            KeyWrapper("wrapped_key_1") -> ValueWrapper(value = "some_value"),
            KeyWrapper("wrapped_key_2") -> ValueWrapper(value = "some_other_value")
          ),
          charSequenceToByteChunk(
            """{"wrapped_key_1":{"value":"some_value"},"wrapped_key_2":{"value":"some_other_value"}}"""
          )
        )
      }
    ),
    suite("Missing collection fields")(
      test("map") {
        assertDecodes(
          Schema[ListAndMapAndOption],
          ListAndMapAndOption(Nil, Map.empty, None),
          charSequenceToByteChunk("""{"list":[]}""")
        )
      },
      test("list") {
        assertDecodes(
          Schema[ListAndMapAndOption],
          ListAndMapAndOption(Nil, Map.empty, None),
          charSequenceToByteChunk("""{"map":{}}""")
        )
      },
      test("set") {
        assertDecodes(
          Schema[SetWrapper],
          SetWrapper(Set.empty),
          charSequenceToByteChunk("""{}""")
        )
      },
      test("vector") {
        assertDecodes(
          Schema[VectorWrapper],
          VectorWrapper(Vector.empty),
          charSequenceToByteChunk("""{}""")
        )
      },
      test("chunck") {
        assertDecodes(
          Schema[ChunckWrapper],
          ChunckWrapper(Chunk.empty),
          charSequenceToByteChunk("""{}""")
        )
      }
    ),
    suite("zio.json.ast.Json decoding")(
      test("Json.Obj") {
        assertDecodes(
          zio.schema.codec.json.schemaJson,
          Json.Obj("foo" -> Json.Str("bar"), "null" -> Json.Null),
          charSequenceToByteChunk("""{"foo":"bar","null":null}""")
        )
      },
      test("Json.Arr") {
        assertDecodes(
          zio.schema.codec.json.schemaJson,
          Json.Arr(Json.Str("foo"), Json.Num(1)),
          charSequenceToByteChunk("""["foo",1]""")
        )
      },
      test("Json.Num Int") {
        assertDecodes(
          zio.schema.codec.json.schemaJson,
          Json.Num(1),
          charSequenceToByteChunk("""1""")
        )
      },
      test("Json.Num Long") {
        assertDecodes(
          zio.schema.codec.json.schemaJson,
          Json.Num(1L),
          charSequenceToByteChunk("""1""")
        )
      },
      test("Json.Num Double") {
        assertDecodes(
          zio.schema.codec.json.schemaJson,
          Json.Num(1.1),
          charSequenceToByteChunk("""1.1""")
        )
      },
      test("Json.Str") {
        assertDecodes(
          zio.schema.codec.json.schemaJson,
          Json.Str("foo"),
          charSequenceToByteChunk(""""foo"""")
        )
      },
      test("Json.Bool") {
        assertDecodes(
          zio.schema.codec.json.schemaJson,
          Json.Bool(true),
          charSequenceToByteChunk("""true""")
        )
      },
      test("Json.Null") {
        assertDecodes(
          zio.schema.codec.json.schemaJson,
          Json.Null,
          charSequenceToByteChunk("""null""")
        )
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
            Schema.Either(left._1.asInstanceOf[Schema[(Any, Any)]], right._1.asInstanceOf[Schema[(Any, Any)]]),
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
        } yield (Schema.Either(left, right), Left(value))) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("of sequence") {
        check(
          for {
            left  <- SchemaGen.anySequenceAndValue
            right <- SchemaGen.anySequenceAndValue
          } yield (
            Schema.Either(left._1.asInstanceOf[Schema[Chunk[Any]]], right._1.asInstanceOf[Schema[Chunk[Any]]]),
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
              .Either(left._1.asInstanceOf[Schema[Map[Any, Any]]], right._1.asInstanceOf[Schema[Map[Any, Any]]]),
            Left(left._2)
          )
        ) {
          case (schema, value) =>
            assertEncodesThenDecodes[scala.util.Either[Map[Any, Any], Map[Any, Any]]](
              schema.asInstanceOf[Schema[scala.util.Either[Map[Any, Any], Map[Any, Any]]]],
              value.asInstanceOf[scala.util.Either[Map[Any, Any], Map[Any, Any]]]
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
              .Either(left._1.asInstanceOf[Schema[Set[Any]]], right._1.asInstanceOf[Schema[Set[Any]]]),
            Left(left._2)
          )
        ) {
          case (schema, value) =>
            assertEncodesThenDecodes[scala.util.Either[Set[Any], Set[Any]]](
              schema.asInstanceOf[Schema[scala.util.Either[Set[Any], Set[Any]]]],
              value.asInstanceOf[scala.util.Either[Set[Any], Set[Any]]]
            )
        }
      },
      test("Map of complex keys and values") {
        assertEncodes(
          Schema.map[Key, Value],
          Map(Key("a", 0) -> Value(0, true), Key("b", 1) -> Value(1, false)),
          charSequenceToByteChunk(
            """[[{"name":"a","index":0},{"first":0,"second":true}],[{"name":"b","index":1},{"first":1,"second":false}]]"""
          )
        )
      },
      test("Set of complex values") {
        assertEncodes(
          Schema.set[Value],
          Set(Value(0, true), Value(1, false)),
          charSequenceToByteChunk(
            """[{"first":0,"second":true},{"first":1,"second":false}]"""
          )
        )
      },
      test("of records") {
        check(for {
          (left, a)       <- SchemaGen.anyRecordAndValue()
          primitiveSchema <- SchemaGen.anyPrimitive
        } yield (Schema.Either(left, primitiveSchema), Left(a))) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("of records of records") {
        check(for {
          (left, _)  <- SchemaGen.anyRecordOfRecordsAndValue
          (right, b) <- SchemaGen.anyRecordOfRecordsAndValue
        } yield (Schema.Either(left, right), Right(b))) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      test("mixed") {
        check(for {
          (left, _)      <- SchemaGen.anyEnumerationAndValue
          (right, value) <- SchemaGen.anySequenceAndValue
        } yield (Schema.Either(left, right), Right(value))) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      }
    ),
    suite("fallback")(
      test("of enums") {
        check(for {
          (left, value) <- SchemaGen.anyEnumerationAndValue
          (right, _)    <- SchemaGen.anyEnumerationAndValue
        } yield (Schema.Fallback(left, right), Fallback.Left(value))) {
          case (schema, value) => assertEncodesThenDecodesFallback(schema, value)
        }
      },
      test("of map") {
        check(
          for {
            left  <- SchemaGen.anyMapAndValue
            right <- SchemaGen.anyMapAndValue
          } yield (
            Schema
              .Fallback(left._1.asInstanceOf[Schema[Map[Any, Any]]], right._1.asInstanceOf[Schema[Map[Any, Any]]]),
            Fallback.Left(left._2)
          )
        ) {
          case (schema, value) =>
            assertEncodesThenDecodesFallback[Map[Any, Any], Map[Any, Any]](
              schema,
              value.asInstanceOf[Fallback[Map[Any, Any], Map[Any, Any]]]
            )
        }
      },
      test("of records") {
        check(for {
          (left, a)       <- SchemaGen.anyRecordAndValue()
          primitiveSchema <- SchemaGen.anyPrimitive
        } yield (Schema.Fallback(left, primitiveSchema), Fallback.Left(a))) {
          case (schema, value) => assertEncodesThenDecodesFallback(schema, value)
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
        check(SchemaGen.anyRecordAndValue()) {
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
      test("optional") {
        check(optionalSearchRequestGen) { value =>
          assertEncodesThenDecodes(optionalSearchRequestSchema, value)
        }
      },
      test("object") {
        assertEncodesThenDecodes(schemaObject, Singleton)
      },
      test("all optional fields empty") {
        assertEncodesThenDecodes(AllOptionalFields.schema, AllOptionalFields(None, None, None))
      },
      test("recursive data structure")(
        assertDecodes(
          Schema[Recursive],
          Recursive(Some(Recursive(None))),
          charSequenceToByteChunk("""{"n":{"n":null}}""")
        )
      )
    ),
    suite("record")(
      test("any") {
        check(SchemaGen.anyRecordAndValue()) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("minimal test case") {
        SchemaGen.anyRecordAndValue().runHead.flatMap {
          case Some((schema, value)) =>
            val key = new String(Array('\u0007', '\n'))
            val embedded = Schema.record(
              TypeId.Structural,
              Schema
                .Field[ListMap[String, _], ListMap[String, _]](
                  key,
                  schema,
                  get0 = (p: ListMap[String, _]) => p(key).asInstanceOf[ListMap[String, _]],
                  set0 = (p: ListMap[String, _], v: ListMap[String, _]) => p.updated(key, v)
                )
            )
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
        check(SchemaGen.anyRecordAndValue()) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      },
      test("of ZoneOffsets") {
        check(JavaTimeGen.anyZoneOffset) { zoneOffset =>
          assertEncodesThenDecodes(
            Schema.record(
              TypeId.parse("java.time.ZoneOffset"),
              Schema.Field(
                "zoneOffset",
                Schema.Primitive(StandardType.ZoneOffsetType),
                get0 = (p: ListMap[String, _]) => p("zoneOffset").asInstanceOf[ZoneOffset],
                set0 = (p: ListMap[String, _], v: ZoneOffset) => p.updated("zoneOffset", v)
              )
            ),
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
      },
      test("ADT with annotation") {
        assertEncodesThenDecodes(
          Schema[Enumeration2],
          Enumeration2(StringValue2("foo"))
        ) &> assertEncodesThenDecodes(
          Schema[Enumeration2],
          Enumeration2(`StringValue2-Backticked`("foo", "bar"))
        ) &> assertEncodesThenDecodes(Schema[Enumeration2], Enumeration2(IntValue2(-1))) &> assertEncodesThenDecodes(
          Schema[Enumeration2],
          Enumeration2(BooleanValue2(false))
        )
      },
      test("ADT with noDiscriminator") {
        assertEncodesThenDecodes(
          Schema[Enumeration3],
          Enumeration3(StringValue3("foo"))
        ) &> assertEncodesThenDecodes(
          Schema[Enumeration3],
          Enumeration3(`StringValue3-Backticked`("foo", "bar"))
        )
      },
      test("ADT with generic records and discriminator field") {
        assertEncodesThenDecodes(
          Schema[OneOf4],
          RecordExampleWithDiscriminator(f1 = Some("test"), f2 = None)
        )
      },
      test("of case classes with discriminator") {
        assertEncodesThenDecodes(Schema[Command], Command.Cash) &>
          assertEncodesThenDecodes(Schema[Command], Command.Buy(100))
      },
      test("decode discriminated case objects in array")(
        assertDecodes(Schema[List[Command]], Command.Cash :: Nil, charSequenceToByteChunk("""[{"type":"Cash"}]"""))
      ),
      test("no discriminator field")(
        assertDecodesToError(Schema[Command], "{\"b\":123}", JsonError.Message("missing subtype") :: Nil) &>
          assertDecodesToError(Schema[Command], "{}", JsonError.Message("missing subtype") :: Nil)
      ),
      test("illegal case")(
        assertDecodesToError(
          Schema[Command],
          "{\"type\":\"Run\"}",
          JsonError.Message("unrecognized subtype") :: JsonError.ObjectAccess("type") :: Nil
        )
      ),
      test("decode discriminated case objects with extra fields")(
        assertDecodes(Schema[Command], Command.Cash, charSequenceToByteChunk("""{"type":"Cash","extraField":1}""")) &>
          assertDecodes(Schema[Command], Command.Cash, charSequenceToByteChunk("""{"extraField":1,"type":"Cash"}"""))
      ),
      suite("of case objects with up to 64 cases")(
        test("without annotation")(
          assertEncodesThenDecodes(Schema[Color], Color.Red)
        ),
        test("with caseName")(
          assertEncodesThenDecodes(Schema[Color], Color.Grass) &>
            assertEncodesJson(Schema[Color], Color.Grass, "\"Green\"") &>
            assertDecodes(Schema[Color], Color.Grass, charSequenceToByteChunk("\"Green\"")) &>
            assertDecodesToError(Schema[Color], "\"Grass\"", JsonError.Message("unrecognized string") :: Nil)
        ),
        test("with caseAliases")(
          assertEncodesThenDecodes(Schema[Color], Color.Blue) &>
            assertEncodesJson(Schema[Color], Color.Blue, "\"Blue\"") &>
            assertDecodes(Schema[Color], Color.Blue, charSequenceToByteChunk("\"Blue\"")) &>
            assertDecodes(Schema[Color], Color.Blue, charSequenceToByteChunk("\"LightBlue\"")) &>
            assertDecodes(Schema[Color], Color.Blue, charSequenceToByteChunk("\"DarkBlue\""))
        ),
        test("invalid case")(
          assertDecodesToError(Schema[Color], "\"not a color\"", JsonError.Message("unrecognized string") :: Nil)
        )
      ),
      suite("of case objects with more than 64 cases")(
        test("without annotation")(
          assertEncodesThenDecodes(Schema[BigEnum], BigEnum.Case69)
        ),
        test("with caseName")(
          assertEncodesThenDecodes(Schema[BigEnum], BigEnum.Case00) &>
            assertEncodesJson(Schema[BigEnum], BigEnum.Case00, "\"Case_00\"") &>
            assertDecodes(Schema[BigEnum], BigEnum.Case00, charSequenceToByteChunk("\"Case_00\"")) &>
            assertDecodesToError(Schema[BigEnum], "\"Case00\"", JsonError.Message("unrecognized string") :: Nil)
        ),
        test("with caseAliases")(
          assertEncodesThenDecodes(Schema[BigEnum], BigEnum.Case00) &>
            assertDecodes(Schema[BigEnum], BigEnum.Case00, charSequenceToByteChunk("\"Case-00\""))
        ),
        test("invalid case")(
          assertDecodesToError(Schema[BigEnum], "\"CaseXX\"", JsonError.Message("unrecognized string") :: Nil)
        )
      ),
      suite("of case classes and case objects with more than 64 cases")(
        test("without annotation")(
          assertEncodesThenDecodes(Schema[BigEnum2], BigEnum2.Case69)
        ),
        test("with caseName")(
          assertEncodesThenDecodes(Schema[BigEnum2], BigEnum2.Case00(123.toByte)) &>
            assertEncodesJson(Schema[BigEnum2], BigEnum2.Case00(123.toByte), "{\"Case_00\":{\"b\":123}}") &>
            assertDecodes(
              Schema[BigEnum2],
              BigEnum2.Case00(123.toByte),
              charSequenceToByteChunk("{\"Case_00\":{\"b\":123}}")
            ) &>
            assertDecodesToError(Schema[BigEnum2], "{\"Case00\":{}}", JsonError.Message("unrecognized subtype") :: Nil)
        ),
        test("with caseAliases")(
          assertEncodesThenDecodes(Schema[BigEnum2], BigEnum2.Case00(123.toByte)) &>
            assertDecodes(
              Schema[BigEnum2],
              BigEnum2.Case00(123.toByte),
              charSequenceToByteChunk("{\"Case-00\":{\"b\":123}}")
            )
        ),
        test("no discriminator key")(
          assertDecodesToError(Schema[BigEnum2], "{}", JsonError.Message("missing subtype") :: Nil)
        ),
        test("invalid case")(
          assertDecodesToError(Schema[BigEnum2], "{\"CaseXX\":{}}", JsonError.Message("unrecognized subtype") :: Nil)
        )
      ),
      suite("of case classes and case objects with more than 64 cases and discriminator field")(
        test("without annotation")(
          assertEncodesThenDecodes(Schema[BigEnum3], BigEnum3.Case69)
        ),
        test("with caseName")(
          assertEncodesThenDecodes(Schema[BigEnum3], BigEnum3.Case00(123.toByte)) &>
            assertEncodesJson(Schema[BigEnum3], BigEnum3.Case00(123.toByte), "{\"type\":\"Case_00\",\"b\":123}") &>
            assertDecodes(
              Schema[BigEnum3],
              BigEnum3.Case00(123.toByte),
              charSequenceToByteChunk("{\"b\":123,\"type\":\"Case_00\"}")
            ) &>
            assertDecodesToError(
              Schema[BigEnum3],
              "{\"type\":\"Case00\"}",
              JsonError.Message("unrecognized subtype") :: JsonError.ObjectAccess("type") :: Nil
            )
        ),
        test("with caseAliases")(
          assertEncodesThenDecodes(Schema[BigEnum3], BigEnum3.Case00(123.toByte)) &>
            assertDecodes(
              Schema[BigEnum3],
              BigEnum3.Case00(123.toByte),
              charSequenceToByteChunk("{\"type\":\"Case-00\",\"b\":123}")
            )
        ),
        test("no discriminator field")(
          assertDecodesToError(Schema[BigEnum3], "{\"b\":123}", JsonError.Message("missing subtype") :: Nil) &>
            assertDecodesToError(Schema[BigEnum3], "{}", JsonError.Message("missing subtype") :: Nil)
        ),
        test("invalid case")(
          assertDecodesToError(
            Schema[BigEnum3],
            "{\"type\":\"CaseXX\"}",
            JsonError.Message("unrecognized subtype") :: JsonError.ObjectAccess("type") :: Nil
          )
        )
      )
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
      test("deep recursive data type") {
        check(SchemaGen.anyDeepRecursiveTypeAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      } @@ TestAspect.size(200),
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
            DynamicValueGen.anyPrimitiveDynamicValue(StandardType.InstantType)
          ) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic zoned date time") {
          check(
            DynamicValueGen.anyPrimitiveDynamicValue(
              StandardType.ZonedDateTimeType
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
        },
        test("dynamic sequence") {
          check(SchemaGen.anyRecord.flatMap(DynamicValueGen.anyDynamicSequence)) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic set") {
          check(SchemaGen.anyRecord.flatMap(DynamicValueGen.anyDynamicSet)) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        }
      ),
      test("deserialized dynamic record converted to typed value") {
        check(SchemaGen.anyRecordAndValue(maxFieldCount = 15)) {
          case (schema, value) =>
            val dyn = DynamicValue.fromSchemaAndValue(schema, value)
            ZStream
              .succeed(dyn)
              .via(JsonCodec.schemaBasedBinaryCodec(Schema.dynamicValue).streamEncoder)
              .via(JsonCodec.schemaBasedBinaryCodec(Schema.dynamicValue).streamDecoder)
              .map(_.toTypedValue(schema))
              .runHead
              .map { result =>
                val resultList = result.get.toOption.get.toList
                assertTrue(
                  resultList == value.toList
                )
              }
        }
      } @@ TestAspect.size(1000) @@ TestAspect.samples(1000) @@ TestAspect.exceptNative,
      suite("meta schema")(
        test("primitive string meta schema") {
          assertEncodesThenDecodes(MetaSchema.schema, Schema[String].ast)
        },
        test("case class meta schema") {
          assertEncodesThenDecodes(MetaSchema.schema, Schema[SchemaGen.Arity1].ast)
        },
        test("recursive class meta schema") {
          assertEncodesThenDecodes(MetaSchema.schema, Schema[SchemaGen.Json].ast)
        },
        test("dynamic value meta schema") {
          assertEncodesThenDecodes(MetaSchema.schema, Schema[DynamicValue].ast)
        },
        test("any meta schema") {
          check(SchemaGen.anySchema) { schema =>
            val metaSchema = schema.ast
            assertEncodesThenDecodes(MetaSchema.schema, metaSchema)
          }
        }
      )
    )
  )

  private def assertEncodes[A](
    schema: Schema[A],
    value: A,
    chunk: Chunk[Byte],
    cfg: JsonCodec.Config = JsonCodec.Config.default,
    print: Boolean = false
  ) = {
    val stream = ZStream
      .succeed(value)
      .via(JsonCodec.schemaBasedBinaryCodec(cfg)(schema).streamEncoder)
      .runCollect
      .tap { chunk =>
        printLine(s"${new String(chunk.toArray)}").when(print).ignore
      }
    assertZIO(stream)(equalTo(chunk))
  }

  private def assertEncodesMany[A](
    schema: Schema[A],
    values: Seq[A],
    chunk: Chunk[Byte],
    cfg: JsonCodec.Config = JsonCodec.Config.default,
    print: Boolean = false
  ) = {
    val stream = ZStream
      .fromIterable(values)
      .via(JsonCodec.schemaBasedBinaryCodec(cfg)(schema).streamEncoder)
      .runCollect
      .tap { chunk =>
        printLine(s"${new String(chunk.toArray)}").when(print).ignore
      }
    assertZIO(stream)(equalTo(chunk))
  }

  private def assertEncodesJson[A](
    schema: Schema[A],
    value: A,
    json: String,
    cfg: JsonCodec.Config = JsonCodec.Config.default
  ) = {
    val stream = ZStream
      .succeed(value)
      .via(JsonCodec.schemaBasedBinaryCodec[A](cfg)(schema).streamEncoder)
      .runCollect
      .map(chunk => new String(chunk.toArray))
    assertZIO(stream)(equalTo(json))
  }

  def assertEncodesJson[A](schema: Schema[A], value: A)(implicit enc: JsonEncoder[A]): ZIO[Any, Nothing, TestResult] = {
    val stream = ZStream
      .succeed(value)
      .via(JsonCodec.schemaBasedBinaryCodec[A](schema).streamEncoder)
      .runCollect
    assertZIO(stream)(equalTo(jsonEncoded(value)))
  }

  private def assertDecodesToError[A](
    schema: Schema[A],
    json: CharSequence,
    errors: List[JsonError],
    cfg: JsonCodec.Config = JsonCodec.Config.default
  ) = {
    val stream = ZStream
      .fromChunk(charSequenceToByteChunk(json))
      .via(JsonCodec.schemaBasedBinaryCodec[A](cfg)(schema).streamDecoder)
      .catchAll(ZStream.succeed[DecodeError](_))
      .runHead
    assertZIO(stream)(isSome(equalTo(ReadError(Cause.empty, JsonError.render(errors)))))
  }

  private def assertDecodes[A](
    schema: Schema[A],
    value: A,
    chunk: Chunk[Byte],
    cfg: JsonCodec.Config = JsonCodec.Config.default
  ) = {
    val result = ZStream.fromChunk(chunk).via(JsonCodec.schemaBasedBinaryCodec[A](cfg)(schema).streamDecoder).runCollect
    assertZIO(result)(equalTo(Chunk(value)))
  }

  private def assertDecodesMany[A](
    schema: Schema[A],
    values: Chunk[A],
    chunk: Chunk[Byte],
    cfg: JsonCodec.Config = JsonCodec.Config.default
  ) = {
    val result = ZStream.fromChunk(chunk).via(JsonCodec.schemaBasedBinaryCodec[A](cfg)(schema).streamDecoder).runCollect
    assertZIO(result)(equalTo(values))
  }

  private def assertEncodesThenDecodesFallback[A, B](
    schema: Schema.Fallback[A, B],
    value: Fallback[A, B]
  ): ZIO[Any, Nothing, TestResult] =
    ZStream
      .succeed(value)
      .via(JsonCodec.schemaBasedBinaryCodec[zio.schema.Fallback[A, B]](JsonCodec.Config.default)(schema).streamEncoder)
      .runCollect
      .flatMap { encoded =>
        ZStream
          .fromChunk(encoded)
          .via(
            JsonCodec.schemaBasedBinaryCodec[zio.schema.Fallback[A, B]](JsonCodec.Config.default)(schema).streamDecoder
          )
          .runCollect
      }
      .either
      .flatMap { result =>
        val expected = if (schema.fullDecode) value else value.simplify
        result.map(_.headOption.getOrElse(expected)) match {
          case Right(obtained) =>
            if (expected == obtained)
              ZIO.succeed(assertTrue(expected == obtained))
            else
              assertEncodesThenDecodesFallback(schema, obtained)
          case Left(_) => ZIO.succeed(assertTrue(false))
        }

      }

  private def assertEncodesThenDecodes[A](schema: Schema[A], value: A, print: Boolean = false) =
    assertEncodesThenDecodesWithDifferentSchemas(schema, schema, value, (x: A, y: A) => x == y, print)

  private def assertEncodesThenDecodesWithDifferentSchemas[A1, A2](
    encodingSchema: Schema[A1],
    decodingSchema: Schema[A2],
    value: A1,
    compare: (A1, A2) => Boolean,
    print: Boolean,
    cfg: JsonCodec.Config = JsonCodec.Config.default
  ) =
    ZStream
      .succeed(value)
      .tap(value => printLine(s"Input Value: $value").when(print).ignore)
      .via(JsonCodec.schemaBasedBinaryCodec[A1](cfg)(encodingSchema).streamEncoder)
      .runCollect
      .tap(encoded => printLine(s"Encoded: ${new String(encoded.toArray)}").when(print).ignore)
      .flatMap { encoded =>
        ZStream
          .fromChunk(encoded)
          .via(JsonCodec.schemaBasedBinaryCodec[A2](cfg)(decodingSchema).streamDecoder)
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

  implicit def mapEncoder[K, V](
    implicit keyEncoder: JsonEncoder[K],
    valueEncoder: JsonEncoder[V]
  ): JsonEncoder[Map[K, V]] =
    JsonEncoder.chunk(keyEncoder.zip(valueEncoder)).contramap[Map[K, V]](m => Chunk.fromIterable(m))

  private def jsonEncoded[A](value: A)(implicit enc: JsonEncoder[A]): Chunk[Byte] =
    charSequenceToByteChunk(enc.encodeJson(value, None))

  private def stringify(s: String): Chunk[Byte] = {
    val encoded = JsonEncoder.string.encodeJson(s, None)
    charSequenceToByteChunk(encoded)
  }

  case class SearchRequest(query: String, pageNumber: Int, resultPerPage: Int, nextPage: Option[String])

  object SearchRequest {
    implicit val encoder: JsonEncoder[SearchRequest] = DeriveJsonEncoder.gen[SearchRequest]
  }

  case class OptionalSearchRequest(
    query: String,
    pageNumber: Int,
    resultPerPage: Int,
    @optionalField nextPage: String
  )

  object OptionalSearchRequest {
    implicit val encoder: JsonEncoder[OptionalSearchRequest] = DeriveJsonEncoder.gen[OptionalSearchRequest]
  }

  @rejectExtraFields final case class PersonWithRejectExtraFields(name: String, age: Int)

  object PersonWithRejectExtraFields {
    implicit val encoder: JsonEncoder[PersonWithRejectExtraFields] =
      DeriveJsonEncoder.gen[PersonWithRejectExtraFields]

    val schema: Schema[PersonWithRejectExtraFields] = DeriveSchema.gen[PersonWithRejectExtraFields]
  }

  case class FieldDefaultValueSearchRequest(
    query: String,
    pageNumber: Int,
    resultPerPage: Int,
    @fieldDefaultValue("test") nextPage: String
  )

  object FieldDefaultValueSearchRequest {
    implicit val encoder: JsonEncoder[FieldDefaultValueSearchRequest] =
      DeriveJsonEncoder.gen[FieldDefaultValueSearchRequest]
  }

  private val searchRequestGen: Gen[Sized, SearchRequest] =
    for {
      query      <- Gen.string
      pageNumber <- Gen.int(Int.MinValue, Int.MaxValue)
      results    <- Gen.int(Int.MinValue, Int.MaxValue)
      nextPage   <- Gen.option(Gen.asciiString)
    } yield SearchRequest(query, pageNumber, results, nextPage)

  private val optionalSearchRequestGen: Gen[Sized, OptionalSearchRequest] =
    for {
      query      <- Gen.string
      pageNumber <- Gen.int(Int.MinValue, Int.MaxValue)
      results    <- Gen.int(Int.MinValue, Int.MaxValue)
      nextPage   <- Gen.asciiString
    } yield OptionalSearchRequest(query, pageNumber, results, nextPage)

  val searchRequestSchema: Schema[SearchRequest] = DeriveSchema.gen[SearchRequest]

  val optionalSearchRequestSchema: Schema[OptionalSearchRequest] = DeriveSchema.gen[OptionalSearchRequest]

  case class SearchRequestWithTransientField(
    query: String,
    pageNumber: Int,
    resultPerPage: Int,
    @transientField nextPage: String = "transient"
  )

  val searchRequestWithTransientFieldSchema: Schema[SearchRequestWithTransientField] =
    DeriveSchema.gen[SearchRequestWithTransientField]

  val fieldDefaultValueSearchRequestSchema: Schema[FieldDefaultValueSearchRequest] =
    DeriveSchema.gen[FieldDefaultValueSearchRequest]

  val recordSchema: Schema[ListMap[String, _]] = Schema.record(
    TypeId.Structural,
    Schema.Field(
      "foo",
      Schema.Primitive(StandardType.StringType),
      get0 = (p: ListMap[String, _]) => p("foo").asInstanceOf[String],
      set0 = (p: ListMap[String, _], v: String) => p.updated("foo", v)
    ),
    Schema
      .Field(
        "bar",
        Schema.Primitive(StandardType.IntType),
        annotations0 = Chunk(fieldDefaultValue(1)),
        get0 = (p: ListMap[String, _]) => p("bar").asInstanceOf[Int],
        set0 = (p: ListMap[String, _], v: Int) => p.updated("bar", v)
      )
  )

  val recordWithOptionSchema: Schema[ListMap[String, _]] = Schema.record(
    TypeId.Structural,
    Schema.Field(
      "foo",
      Schema.Primitive(StandardType.StringType).optional,
      get0 = (p: ListMap[String, _]) => p("foo").asInstanceOf[Option[String]],
      set0 = (p: ListMap[String, _], v: Option[String]) => p.updated("foo", v)
    ),
    Schema
      .Field(
        "bar",
        Schema.Primitive(StandardType.IntType).optional,
        get0 = (p: ListMap[String, _]) => p("bar").asInstanceOf[Option[Int]],
        set0 = (p: ListMap[String, _], v: Option[Int]) => p.updated("bar", v)
      )
  )

  val recordWithTransientSchema: Schema[ListMap[String, _]] = Schema.record(
    TypeId.Structural,
    Schema.Field(
      "foo",
      Schema.Primitive(StandardType.StringType),
      annotations0 = Chunk(transientField()),
      get0 = (p: ListMap[String, _]) => p("foo").asInstanceOf[String],
      set0 = (p: ListMap[String, _], v: String) => p.updated("foo", v)
    ),
    Schema
      .Field(
        "bar",
        Schema.Primitive(StandardType.IntType),
        annotations0 = Chunk(transientField()),
        get0 = (p: ListMap[String, _]) => p("bar").asInstanceOf[Int],
        set0 = (p: ListMap[String, _], v: Int) => p.updated("bar", v)
      )
  )

  val nestedRecordSchema: Schema[ListMap[String, _]] = Schema.record(
    TypeId.Structural,
    Schema.Field(
      "l1",
      Schema.Primitive(StandardType.StringType),
      get0 = (p: ListMap[String, _]) => p("l1").asInstanceOf[String],
      set0 = (p: ListMap[String, _], v: String) => p.updated("l1", v)
    ),
    Schema.Field(
      "l2",
      recordSchema,
      get0 = (p: ListMap[String, _]) => p("l2").asInstanceOf[ListMap[String, _]],
      set0 = (p: ListMap[String, _], v: ListMap[String, _]) => p.updated("l2", v)
    )
  )

  val enumSchema: Schema[Any] = Schema.enumeration[Any, CaseSet.Aux[Any]](
    TypeId.Structural,
    caseOf[String, Any]("string")(_.asInstanceOf[String])(_.asInstanceOf[Any])(_.isInstanceOf[String]) ++ caseOf[
      Int,
      Any
    ]("int")(_.asInstanceOf[Int])(_.asInstanceOf[Any])(_.isInstanceOf[Int]) ++ caseOf[
      Boolean,
      Any
    ]("boolean")(_.asInstanceOf[Boolean])(_.asInstanceOf[Any])(_.isInstanceOf[Boolean])
  )

  sealed trait OneOf

  case class StringValue(value: String) extends OneOf

  case class IntValue(value: Int) extends OneOf

  case class BooleanValue(value: Boolean) extends OneOf

  object OneOf {
    implicit val schema: Schema[OneOf] = DeriveSchema.gen[OneOf]
  }

  case class Enumeration(oneOf: OneOf)

  object Enumeration {
    implicit val schema: Schema[Enumeration] = DeriveSchema.gen[Enumeration]
  }

  @discriminatorName("_type")
  sealed trait OneOf2

  case class StringValue2(value: String) extends OneOf2

  case class IntValue2(value: Int) extends OneOf2

  case class BooleanValue2(value: Boolean) extends OneOf2

  case class `StringValue2-Backticked`(value1: String, value2: String) extends OneOf2

  case class Enumeration2(oneOf: OneOf2)

  object Enumeration2 {
    implicit val schema: Schema[Enumeration2] = DeriveSchema.gen[Enumeration2]
  }

  @noDiscriminator
  sealed trait OneOf3

  case class StringValue3(value: String) extends OneOf3

  case class IntValue3(value: Int) extends OneOf3

  case class BooleanValue3(value: Boolean) extends OneOf3

  case class `StringValue3-Backticked`(value1: String, value2: String) extends OneOf3

  case class Nested(oneOf: OneOf3) extends OneOf3

  case class Enumeration3(oneOf: OneOf3)

  object Enumeration3 {
    implicit val schema: Schema[Enumeration3] = DeriveSchema.gen[Enumeration3]
  }

  sealed trait Color

  object Color {
    case object Red extends Color

    @caseName("Green")
    case object Grass extends Color

    @caseNameAliases("LightBlue", "DarkBlue")
    case object Blue extends Color

    implicit val schema: Schema[Color] = DeriveSchema.gen[Color]
  }

  @annotation.discriminatorName("type")
  sealed trait Command

  object Command {
    case class Buy(credits: Int) extends Command

    case object Cash extends Command

    implicit val schema: Schema[Command] = DeriveSchema.gen[Command]
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

  sealed trait PaymentMethod

  object PaymentMethod {

    @caseNameAliases("credit_card", "cc") final case class CreditCard(
      number: String,
      expirationMonth: Int,
      expirationYear: Int
    ) extends PaymentMethod

    @caseName("wire_transfer") final case class WireTransfer(accountNumber: String, bankCode: String)
        extends PaymentMethod

    @transientCase final case class PayPal(email: String) extends PaymentMethod

    implicit lazy val schema: Schema[PaymentMethod] = DeriveSchema.gen[PaymentMethod]
  }

  @discriminatorName("type") sealed trait Subscription

  object Subscription {

    @caseName("recurring") final case class Recurring(
      period: String,
      amount: Int
    ) extends Subscription

    @caseNameAliases("one_time", "onetime") final case class OneTime(
      amount: Int
    ) extends Subscription

    @caseName("unlimited") final case class Unlimited(until: Option[Long]) extends Subscription

    implicit lazy val schema: Schema[Subscription] = DeriveSchema.gen[Subscription]
  }

  case class Order(
    @fieldNameAliases("order_id", "id") @fieldName("orderId") `order-id`: Int,
    value: BigDecimal,
    description: String
  )

  object Order {
    implicit lazy val schema: Schema[Order] = DeriveSchema.gen[Order]
  }

  @noDiscriminator sealed trait Prompt

  object Prompt {
    final case class Single(value: String) extends Prompt

    final case class Multiple(value: List[String]) extends Prompt

    implicit lazy val schema: Schema[Prompt] = DeriveSchema.gen[Prompt]
  }

  final case class WithOptionFields(a: Option[String], b: Option[Int])

  object WithOptionFields {
    implicit lazy val schema: Schema[WithOptionFields] = DeriveSchema.gen[WithOptionFields]
  }

  final case class WithComplexOptionField(order: Option[Order])

  object WithComplexOptionField {
    implicit lazy val schema: Schema[WithComplexOptionField] = DeriveSchema.gen[WithComplexOptionField]
  }

  final case class WithOptField(@optionalField list: List[String], @optionalField map: Map[String, Int])

  object WithOptField {
    implicit lazy val schema: Schema[WithOptField] = DeriveSchema.gen[WithOptField]
  }

  final case class ListAndMapAndOption(list: List[String], map: Map[String, Int], option: Option[String])

  object ListAndMapAndOption {
    implicit lazy val schema: Schema[ListAndMapAndOption] = DeriveSchema.gen[ListAndMapAndOption]
  }

  final case class SetWrapper(set: Set[String])

  object SetWrapper {
    implicit lazy val schema: Schema[SetWrapper] = DeriveSchema.gen[SetWrapper]
  }

  final case class VectorWrapper(sequence: Vector[String])

  object VectorWrapper {
    implicit lazy val schema: Schema[VectorWrapper] = DeriveSchema.gen[VectorWrapper]
  }

  final case class ChunckWrapper(chunk: Chunk[String])

  object ChunckWrapper {
    implicit lazy val schema: Schema[ChunckWrapper] = DeriveSchema.gen[ChunckWrapper]
  }

  final case class KeyWrapper(key: String)

  object KeyWrapper {
    implicit lazy val schema: Schema[KeyWrapper] = Schema[String].transform(KeyWrapper.apply, _.key)
  }

  final case class ValueWrapper(value: String)

  object ValueWrapper {
    implicit lazy val schema: Schema[ValueWrapper] = DeriveSchema.gen[ValueWrapper]
  }

  final case class MapOfComplexKeysAndValues(map: Map[KeyWrapper, ValueWrapper])

  object MapOfComplexKeysAndValues {
    implicit lazy val mapSchema: Schema[Map[KeyWrapper, ValueWrapper]] = Schema.map[KeyWrapper, ValueWrapper]
    implicit lazy val schema: Schema[MapOfComplexKeysAndValues]        = DeriveSchema.gen[MapOfComplexKeysAndValues]
  }

  final case class AllOptionalFields(
    name: Option[String],
    mode: Option[Int],
    active: Option[Boolean]
  )

  object AllOptionalFields {
    implicit lazy val schema: Schema[AllOptionalFields] = DeriveSchema.gen[AllOptionalFields]
  }

  @discriminatorName("type")
  sealed trait OneOf4

  object OneOf4 {
    implicit lazy val schema: Schema[OneOf4] = DeriveSchema.gen
  }

  @rejectExtraFields case class RecordExampleWithDiscriminator(
    @fieldName("$f1") f1: Option[String], // the only field that does not have a default value
    @fieldNameAliases("field2") f2: Option[String] = None,
    @transientField f3: Option[String] = None,
    f4: Option[String] = None,
    f5: Option[String] = None,
    f6: Option[String] = None,
    f7: Option[String] = None,
    f8: Option[String] = None,
    f9: Option[String] = None,
    f10: Option[String] = None,
    f11: Option[String] = None,
    f12: Option[String] = None,
    f13: Option[String] = None,
    f14: Option[String] = None,
    f15: Option[String] = None,
    f16: Option[String] = None,
    f17: Option[String] = None,
    f18: Option[String] = None,
    f19: Option[String] = None,
    f20: Option[String] = None,
    f21: Option[String] = None,
    f22: Option[String] = None,
    @fieldName("$f23") f23: Option[String] = None
  ) extends OneOf4

  case class RecordExample(
    @fieldName("$f1") f1: String,
    @fieldNameAliases("field2") f2: Option[String] = None,
    @transientField f3: Option[String] = None,
    f4: Option[String] = None,
    f5: Option[String] = None,
    f6: Option[String] = None,
    f7: Option[String] = None,
    f8: Option[String] = None,
    f9: Option[String] = None,
    f10: Option[String] = None,
    f11: Option[String] = None,
    f12: Option[String] = None,
    f13: Option[String] = None,
    f14: Option[String] = None,
    f15: Option[String] = None,
    f16: Option[String] = None,
    f17: List[String] = Nil,
    f18: Vector[String] = Vector.empty,
    f19: Option[RecordExample] = None,
    f20: Option[String],
    f21: Vector[String],
    f22: List[String],
    @fieldName("$f23") f23: Option[String] = None
  )

  case class RecordExampleWithOptField(
    @fieldName("$f1") f1: Option[String], // the only field that does not have a default value
    @optionalField @fieldNameAliases("field2") f2: Option[String] = None,
    @transientField f3: Option[String] = None,
    @optionalField f4: String,
    @optionalField @fieldDefaultValue("hello") f5: String,
    f6: Option[String] = None,
    f7: Option[String] = None,
    f8: Option[String] = None,
    f9: Option[String] = None,
    f10: Option[String] = None,
    f11: Option[String] = None,
    f12: Option[String] = None,
    f13: Option[String] = None,
    f14: Option[String] = None,
    f15: Option[String] = None,
    f16: Option[String] = None,
    f17: Option[String] = None,
    f18: Option[String] = None,
    f19: Option[String] = None,
    f20: Option[String] = None,
    f21: Option[String] = None,
    f22: Option[String] = None,
    @fieldName("$f23") f23: Option[String] = None
  )

  object RecordExample {
    implicit lazy val schema: Schema[RecordExample] = DeriveSchema.gen[RecordExample]
  }

  object RecordExampleWithOptField {
    implicit lazy val schema: Schema[RecordExampleWithOptField] =
      DeriveSchema.gen[RecordExampleWithOptField]
  }

  sealed trait Enum23Cases

  object Enum23Cases {
    implicit lazy val schema: Schema[Enum23Cases] = DeriveSchema.gen[Enum23Cases]

    @caseName("NumberOne")
    @caseNameAliases("One") case class Case1(value: String) extends Enum23Cases

    case class Case2(value: Int) extends Enum23Cases

    case class Case3(value: String) extends Enum23Cases

    case class Case4(value: String) extends Enum23Cases

    case class Case5(value: String) extends Enum23Cases

    case class Case6(value: String) extends Enum23Cases

    case class Case7(value: String) extends Enum23Cases

    case class Case8(value: String) extends Enum23Cases

    case class Case9(value: String) extends Enum23Cases

    case class Case10(value: String) extends Enum23Cases

    case class Case11(value: String) extends Enum23Cases

    case class Case12(value: String) extends Enum23Cases

    case class Case13(value: String) extends Enum23Cases

    case class Case14(value: String) extends Enum23Cases

    case class Case15(value: String) extends Enum23Cases

    case class Case16(value: String) extends Enum23Cases

    case class Case17(value: String) extends Enum23Cases

    case class Case18(value: String) extends Enum23Cases

    case class Case19(value: String) extends Enum23Cases

    case class Case20(value: String) extends Enum23Cases

    case class Case21(value: String) extends Enum23Cases

    case class Case22(value: String) extends Enum23Cases

    case class Case23(value: String) extends Enum23Cases
  }

  case class Recursive(n: Option[Recursive] = None)

  object Recursive {
    implicit val schema: Schema[Recursive] = DeriveSchema.gen
  }

  case class BacktickedFieldName(`x-api-key`: String)

  object BacktickedFieldName {
    implicit val schema: Schema[BacktickedFieldName] = DeriveSchema.gen
  }

  sealed trait BigEnum

  object BigEnum {

    @caseName("Case_00")
    @caseNameAliases("Case-00")
    case object Case00 extends BigEnum
    case object Case01 extends BigEnum
    case object Case02 extends BigEnum
    case object Case03 extends BigEnum
    case object Case04 extends BigEnum
    case object Case05 extends BigEnum
    case object Case06 extends BigEnum
    case object Case07 extends BigEnum
    case object Case08 extends BigEnum
    case object Case09 extends BigEnum
    case object Case10 extends BigEnum
    case object Case11 extends BigEnum
    case object Case12 extends BigEnum
    case object Case13 extends BigEnum
    case object Case14 extends BigEnum
    case object Case15 extends BigEnum
    case object Case16 extends BigEnum
    case object Case17 extends BigEnum
    case object Case18 extends BigEnum
    case object Case19 extends BigEnum
    case object Case20 extends BigEnum
    case object Case21 extends BigEnum
    case object Case22 extends BigEnum
    case object Case23 extends BigEnum
    case object Case24 extends BigEnum
    case object Case25 extends BigEnum
    case object Case26 extends BigEnum
    case object Case27 extends BigEnum
    case object Case28 extends BigEnum
    case object Case29 extends BigEnum
    case object Case30 extends BigEnum
    case object Case31 extends BigEnum
    case object Case32 extends BigEnum
    case object Case33 extends BigEnum
    case object Case34 extends BigEnum
    case object Case35 extends BigEnum
    case object Case36 extends BigEnum
    case object Case37 extends BigEnum
    case object Case38 extends BigEnum
    case object Case39 extends BigEnum
    case object Case40 extends BigEnum
    case object Case41 extends BigEnum
    case object Case42 extends BigEnum
    case object Case43 extends BigEnum
    case object Case44 extends BigEnum
    case object Case45 extends BigEnum
    case object Case46 extends BigEnum
    case object Case47 extends BigEnum
    case object Case48 extends BigEnum
    case object Case49 extends BigEnum
    case object Case50 extends BigEnum
    case object Case51 extends BigEnum
    case object Case52 extends BigEnum
    case object Case53 extends BigEnum
    case object Case54 extends BigEnum
    case object Case55 extends BigEnum
    case object Case56 extends BigEnum
    case object Case57 extends BigEnum
    case object Case58 extends BigEnum
    case object Case59 extends BigEnum
    case object Case60 extends BigEnum
    case object Case61 extends BigEnum
    case object Case62 extends BigEnum
    case object Case63 extends BigEnum
    case object Case64 extends BigEnum
    case object Case65 extends BigEnum
    case object Case66 extends BigEnum
    case object Case67 extends BigEnum
    case object Case68 extends BigEnum
    case object Case69 extends BigEnum

    implicit val schema: Schema[BigEnum] = DeriveSchema.gen
  }

  sealed trait BigEnum2

  object BigEnum2 {

    @caseName("Case_00")
    @caseNameAliases("Case-00")
    case class Case00(b: Byte) extends BigEnum2
    case object Case01         extends BigEnum2
    case object Case02         extends BigEnum2
    case object Case03         extends BigEnum2
    case object Case04         extends BigEnum2
    case object Case05         extends BigEnum2
    case object Case06         extends BigEnum2
    case object Case07         extends BigEnum2
    case object Case08         extends BigEnum2
    case object Case09         extends BigEnum2
    case object Case10         extends BigEnum2
    case object Case11         extends BigEnum2
    case object Case12         extends BigEnum2
    case object Case13         extends BigEnum2
    case object Case14         extends BigEnum2
    case object Case15         extends BigEnum2
    case object Case16         extends BigEnum2
    case object Case17         extends BigEnum2
    case object Case18         extends BigEnum2
    case object Case19         extends BigEnum2
    case object Case20         extends BigEnum2
    case object Case21         extends BigEnum2
    case object Case22         extends BigEnum2
    case object Case23         extends BigEnum2
    case object Case24         extends BigEnum2
    case object Case25         extends BigEnum2
    case object Case26         extends BigEnum2
    case object Case27         extends BigEnum2
    case object Case28         extends BigEnum2
    case object Case29         extends BigEnum2
    case object Case30         extends BigEnum2
    case object Case31         extends BigEnum2
    case object Case32         extends BigEnum2
    case object Case33         extends BigEnum2
    case object Case34         extends BigEnum2
    case object Case35         extends BigEnum2
    case object Case36         extends BigEnum2
    case object Case37         extends BigEnum2
    case object Case38         extends BigEnum2
    case object Case39         extends BigEnum2
    case object Case40         extends BigEnum2
    case object Case41         extends BigEnum2
    case object Case42         extends BigEnum2
    case object Case43         extends BigEnum2
    case object Case44         extends BigEnum2
    case object Case45         extends BigEnum2
    case object Case46         extends BigEnum2
    case object Case47         extends BigEnum2
    case object Case48         extends BigEnum2
    case object Case49         extends BigEnum2
    case object Case50         extends BigEnum2
    case object Case51         extends BigEnum2
    case object Case52         extends BigEnum2
    case object Case53         extends BigEnum2
    case object Case54         extends BigEnum2
    case object Case55         extends BigEnum2
    case object Case56         extends BigEnum2
    case object Case57         extends BigEnum2
    case object Case58         extends BigEnum2
    case object Case59         extends BigEnum2
    case object Case60         extends BigEnum2
    case object Case61         extends BigEnum2
    case object Case62         extends BigEnum2
    case object Case63         extends BigEnum2
    case object Case64         extends BigEnum2
    case object Case65         extends BigEnum2
    case object Case66         extends BigEnum2
    case object Case67         extends BigEnum2
    case object Case68         extends BigEnum2
    case object Case69         extends BigEnum2

    implicit val schema: Schema[BigEnum2] = DeriveSchema.gen
  }

  @discriminatorName("type")
  sealed trait BigEnum3

  object BigEnum3 {

    @caseName("Case_00")
    @caseNameAliases("Case-00")
    case class Case00(b: Byte) extends BigEnum3
    case object Case01         extends BigEnum3
    case object Case02         extends BigEnum3
    case object Case03         extends BigEnum3
    case object Case04         extends BigEnum3
    case object Case05         extends BigEnum3
    case object Case06         extends BigEnum3
    case object Case07         extends BigEnum3
    case object Case08         extends BigEnum3
    case object Case09         extends BigEnum3
    case object Case10         extends BigEnum3
    case object Case11         extends BigEnum3
    case object Case12         extends BigEnum3
    case object Case13         extends BigEnum3
    case object Case14         extends BigEnum3
    case object Case15         extends BigEnum3
    case object Case16         extends BigEnum3
    case object Case17         extends BigEnum3
    case object Case18         extends BigEnum3
    case object Case19         extends BigEnum3
    case object Case20         extends BigEnum3
    case object Case21         extends BigEnum3
    case object Case22         extends BigEnum3
    case object Case23         extends BigEnum3
    case object Case24         extends BigEnum3
    case object Case25         extends BigEnum3
    case object Case26         extends BigEnum3
    case object Case27         extends BigEnum3
    case object Case28         extends BigEnum3
    case object Case29         extends BigEnum3
    case object Case30         extends BigEnum3
    case object Case31         extends BigEnum3
    case object Case32         extends BigEnum3
    case object Case33         extends BigEnum3
    case object Case34         extends BigEnum3
    case object Case35         extends BigEnum3
    case object Case36         extends BigEnum3
    case object Case37         extends BigEnum3
    case object Case38         extends BigEnum3
    case object Case39         extends BigEnum3
    case object Case40         extends BigEnum3
    case object Case41         extends BigEnum3
    case object Case42         extends BigEnum3
    case object Case43         extends BigEnum3
    case object Case44         extends BigEnum3
    case object Case45         extends BigEnum3
    case object Case46         extends BigEnum3
    case object Case47         extends BigEnum3
    case object Case48         extends BigEnum3
    case object Case49         extends BigEnum3
    case object Case50         extends BigEnum3
    case object Case51         extends BigEnum3
    case object Case52         extends BigEnum3
    case object Case53         extends BigEnum3
    case object Case54         extends BigEnum3
    case object Case55         extends BigEnum3
    case object Case56         extends BigEnum3
    case object Case57         extends BigEnum3
    case object Case58         extends BigEnum3
    case object Case59         extends BigEnum3
    case object Case60         extends BigEnum3
    case object Case61         extends BigEnum3
    case object Case62         extends BigEnum3
    case object Case63         extends BigEnum3
    case object Case64         extends BigEnum3
    case object Case65         extends BigEnum3
    case object Case66         extends BigEnum3
    case object Case67         extends BigEnum3
    case object Case68         extends BigEnum3
    case object Case69         extends BigEnum3

    implicit val schema: Schema[BigEnum3] = DeriveSchema.gen
  }

  case class BigProduct(
    f00: Boolean,
    @fieldNameAliases("f-01") f01: Option[Byte] = None,
    f02: Option[Short] = None,
    f03: Option[Int] = None,
    f04: Option[Long] = None,
    f05: Option[Float] = None,
    f06: Option[Double] = None,
    f07: Option[Byte] = None,
    f08: Option[Byte] = None,
    f09: Option[Byte] = None,
    f10: Option[Byte] = None,
    f11: Option[Byte] = None,
    f12: Option[Byte] = None,
    f13: Option[Byte] = None,
    f14: Option[Byte] = None,
    f15: Option[Byte] = None,
    f16: Option[Byte] = None,
    f17: Option[Byte] = None,
    f18: Option[Byte] = None,
    f19: Option[Byte] = None,
    f20: Option[Byte] = None,
    f21: Option[Byte] = None,
    f22: Option[Byte] = None,
    f23: Option[Byte] = None,
    f24: Option[Byte] = None,
    f25: Option[Byte] = None,
    f26: Option[Byte] = None,
    f27: Option[Byte] = None,
    f28: Option[Byte] = None,
    f29: Option[Byte] = None,
    f30: Option[Byte] = None,
    f31: Option[Byte] = None,
    f32: Option[Byte] = None,
    f33: Option[Byte] = None,
    f34: Option[Byte] = None,
    f35: Option[Byte] = None,
    f36: Option[Byte] = None,
    f37: Option[Byte] = None,
    f38: Option[Byte] = None,
    f39: Option[Byte] = None,
    f40: Option[Byte] = None,
    f41: Option[Byte] = None,
    f42: Option[Byte] = None,
    f43: Option[Byte] = None,
    f44: Option[Byte] = None,
    f45: Option[Byte] = None,
    f46: Option[Byte] = None,
    f47: Option[Byte] = None,
    f48: Option[Byte] = None,
    f49: Option[Byte] = None,
    f50: Option[Byte] = None,
    f51: Option[Byte] = None,
    f52: Option[Byte] = None,
    f53: Option[Byte] = None,
    f54: Option[Byte] = None,
    f55: Option[Byte] = None,
    f56: Option[Byte] = None,
    f57: Option[Byte] = None,
    f58: Option[Byte] = None,
    f59: Option[Byte] = None,
    f60: Option[Byte] = None,
    f61: Option[Byte] = None,
    f62: Option[Byte] = None,
    f63: Option[Byte] = None,
    f64: Option[Byte] = None,
    f65: Option[Byte] = None,
    f66: Option[Byte] = None,
    f67: Option[BigProduct],
    f68: List[Byte],
    f69: Vector[Byte]
  )

  object BigProduct {
    implicit val schema: Schema[BigProduct] = DeriveSchema.gen
  }
}
