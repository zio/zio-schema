package zio.schema.codec

import java.nio.charset.StandardCharsets
import java.time.ZoneOffset

import scala.collection.immutable.ListMap

import zio.Console._
import zio.schema.CaseSet._
import zio.schema.annotation._
import zio.schema.codec.DecodeError.ReadError
import zio.schema.codec.XmlCodec
import zio.schema.codec.XmlCodec.XmlEncoder.charSequenceToByteChunk
import zio.schema.codec.XmlCodec.XmlError
import zio.schema.codec.XmlCodecSpec.PaymentMethod.{ CreditCard, PayPal, WireTransfer }
import zio.schema.codec.XmlCodecSpec.Subscription.{ OneTime, Recurring }
import zio.schema.{ DeriveSchema, DynamicValue, DynamicValueGen, Schema, SchemaGen, StandardType, TypeId, _ }
import zio.stream.ZStream
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test._
import zio.{ Chunk, _ }

/**
 * Test suite for the XML codec.
 */
object XmlCodecSpec extends ZIOSpecDefault {

  // A simple case class and its schema.
  case class Person(name: String, age: Int)

  val personSchema: Schema[Person] = DeriveSchema.gen[Person]

  def normalize(xml: String): String =
    xml.replaceAll(">\\s+<", "><").trim

  def spec: Spec[TestEnvironment, Any] =
    suite("XmlCodec Spec")(
      encoderSuite,
      decoderSuite,
      encoderDecoderSuite
    ) @@ timeout(180.seconds)

  // ─────────────────────────────
  // Encoder Tests
  // ─────────────────────────────
  private val encoderSuite = suite("encoding")(
    suite("primitive")(
      test("unit") {
        assertEncodesXml(Schema[Unit], (), "<unit/>")
      },
      test("string") {
        check(Gen.string) { s =>
          assertEncodesXml(Schema[String], s, s"<string>$s</string>")
        }
      },
      test("int") {
        assertEncodesXml(Schema.Primitive(StandardType.IntType), 42, "<int>42</int>")
      },
      test("boolean") {
        assertEncodesXml(Schema.Primitive(StandardType.BoolType), true, "<boolean>true</boolean>")
      },
      test("ZoneOffset") {
        val offset = java.time.ZoneOffset.UTC
        assertEncodesXml(
          Schema.Primitive(StandardType.ZoneOffsetType),
          offset,
          s"<ZoneOffset>${offset.toString}</ZoneOffset>"
        )
      },
      test("ZoneId") {
        val zone = java.time.ZoneId.systemDefault()
        assertEncodesXml(
          Schema.Primitive(StandardType.ZoneIdType),
          zone,
          s"<ZoneId>${zone.toString}</ZoneId>"
        )
      },
      test("Currency") {
        val currency = java.util.Currency.getInstance("USD")
        assertEncodesXml(
          Schema.Primitive(StandardType.CurrencyType),
          currency,
          s"<Currency>${currency.toString}</Currency>"
        )
      }
    ),
    suite("fallback")(
      test("left") {
        assertEncodesXml(
          Schema.Fallback(
            Schema.Primitive(StandardType.IntType),
            Schema.Primitive(StandardType.StringType)
          ),
          Fallback.Left(3),
          "<fallback type=\"left\"><int>3</int></fallback>"
        )
      },
      test("right") {
        assertEncodesXml(
          Schema.Fallback(
            Schema.Primitive(StandardType.IntType),
            Schema.Primitive(StandardType.StringType)
          ),
          Fallback.Right("hello"),
          "<fallback type=\"right\"><string>hello</string></fallback>"
        )
      },
      test("both") {
        assertEncodesXml(
          Schema.Fallback(
            Schema.Primitive(StandardType.IntType),
            Schema.Primitive(StandardType.StringType)
          ),
          Fallback.Both(3, "hello"),
          "<fallback type=\"both\"><left><int>3</int></left><right><string>hello</string></right></fallback>"
        )
      },
      test("both pretty printed") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = true)

        val fallbackSchema = Schema.Fallback(Schema.set[Int], Schema.chunk[String])
        val xmlStr = XmlCodec
          .xmlCodec(cfg)(fallbackSchema)
          .encoder
          .encodeXml(Fallback.Both(Set(3), Chunk("hello")))
          .toString()

        val expected =
          """<fallback type="both">
            |  <left>
            |    <set>
            |      <int>3</int>
            |    </set>
            |  </left>
            |  <right>
            |    <seq>
            |      <string>hello</string>
            |    </seq>
            |  </right>
            |</fallback>""".stripMargin

        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      }
    ),
    suite("optional")(
      test("of primitives - Some") {
        assertEncodesXml(
          Schema.Optional(Schema.Primitive(StandardType.StringType)),
          Some("value"),
          "<string>value</string>"
        )
      },
      test("of primitives - None") {
        assertEncodesXml(
          Schema.Optional(Schema.Primitive(StandardType.StringType)),
          None,
          "<null/>"
        )
      }
    ),
    suite("case class")(
      test("backticked field name") {
        assertEncodesXml(
          Schema[BacktickedFieldName],
          BacktickedFieldName("test"),
          "<record><x-api-key><string>test</string></x-api-key></record>"
        )
      }
    ),
    suite("optional field annotation")(
      test("list empty") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(Schema[WithOptField])
          .encodeXml(WithOptField(Nil, Map("foo" -> 1)))
          .toString()
        val expected = "<record><map><map><entry><key>foo</key><value><int>1</int></value></entry></map></map></record>"
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("map empty") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(Schema[WithOptField])
          .encodeXml(WithOptField(List("foo"), Map.empty))
          .toString()
        val expected = "<record><list><seq><string>foo</string></seq></list></record>"
        assert(xmlStr.trim)(equalTo(expected.trim))
      },
      test("all empty") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(Schema[WithOptField])
          .encodeXml(WithOptField(Nil, Map.empty))
          .toString()
        val expected = "<record/>"
        assert(xmlStr.trim)(equalTo(expected.trim))
      }
    ),
    suite("empty collections config")(
      test("list empty") {
        val cfg =
          XmlCodec.Config.default.copy(prettyPrint = false, ignoreEmptyCollections = true, explicitNulls = false)
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(Schema[ListAndMapAndOption])
          .encodeXml(ListAndMapAndOption(Nil, Map("foo" -> 1), Some("foo")))
          .toString()
        val expected =
          "<record><map><map><entry><key>foo</key><value><int>1</int></value></entry></map></map><option><string>foo</string></option></record>"
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("map empty") {
        val cfg =
          XmlCodec.Config.default.copy(prettyPrint = false, ignoreEmptyCollections = true, explicitNulls = false)
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(Schema[ListAndMapAndOption])
          .encodeXml(ListAndMapAndOption(List("foo"), Map.empty, Some("foo")))
          .toString()
        val expected =
          "<record><list><seq><string>foo</string></seq></list><option><string>foo</string></option></record>"
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("option empty") {
        val cfg =
          XmlCodec.Config.default.copy(prettyPrint = false, ignoreEmptyCollections = true, explicitNulls = false)
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(Schema[ListAndMapAndOption])
          .encodeXml(ListAndMapAndOption(List("foo"), Map("foo" -> 1), None))
          .toString()
        val expected =
          "<record><list><seq><string>foo</string></seq></list><map><map><entry><key>foo</key><value><int>1</int></value></entry></map></map></record>"
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("all empty") {
        val cfg =
          XmlCodec.Config.default.copy(prettyPrint = false, ignoreEmptyCollections = true, explicitNulls = false)
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(Schema[ListAndMapAndOption])
          .encodeXml(ListAndMapAndOption(Nil, Map.empty, None))
          .toString()
        val expected = "<record/>"
        assert(xmlStr.trim)(equalTo(expected.trim))
      },
      test("all empty, but don't ignore empty collections") {
        val cfg =
          XmlCodec.Config.default.copy(prettyPrint = false, ignoreEmptyCollections = false, explicitNulls = true)
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(Schema[ListAndMapAndOption])
          .encodeXml(ListAndMapAndOption(Nil, Map.empty, None))
          .toString()
        val expected = "<record><list><seq></seq></list><map><map></map></map><option><null/></option></record>"
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      }
    ),
// Tuple suite
    suite("tuple")(
      test("of primitives") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Schema.Tuple2(
          Schema.Primitive(StandardType.StringType),
          Schema.Primitive(StandardType.IntType)
        )
        val xmlStr   = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(("L", 1)).toString()
        val expected = "<tuple><left><string>L</string></left><right><int>1</int></right></tuple>"
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      }
    ),
// Sequence suite
    suite("sequence")(
      test("of primitives") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        // Schema.chunk returns a Seq, so we convert our Chunk to a Seq.
        val schema   = Schema.chunk(Schema.Primitive(StandardType.StringType))
        val xmlStr   = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(Chunk("a", "b", "c").toSeq).toString()
        val expected = "<seq><string>a</string><string>b</string><string>c</string></seq>"
        assert(xmlStr.trim)(equalTo(expected.trim))
      }
    ),
// Map suite
    suite("Map")(
      test("of complex keys and values") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Schema.map[Key, Value]
        val input  = Map(Key("a", 0) -> Value(0, true), Key("b", 1) -> Value(1, false))
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        val expected =
          """<seq>
            |  <entry>
            |    <key>
            |      <record>
            |        <name><string>a</string></name>
            |        <index><int>0</int></index>
            |      </record>
            |    </key>
            |    <value>
            |      <record>
            |        <first><int>0</int></first>
            |        <second><boolean>true</boolean></second>
            |      </record>
            |    </value>
            |  </entry>
            |  <entry>
            |    <key>
            |      <record>
            |        <name><string>b</string></name>
            |        <index><int>1</int></index>
            |      </record>
            |    </key>
            |    <value>
            |      <record>
            |        <first><int>1</int></first>
            |        <second><boolean>false</boolean></second>
            |      </record>
            |    </value>
            |  </entry>
            |</seq>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("of simple keys and values") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Schema.map[Int, Value]
        val input  = Map(0 -> Value(0, true), 1 -> Value(1, false))
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        val expected =
          """<map>
            |  <entry>
            |    <key>0</key>
            |    <value>
            |      <record>
            |        <first><int>0</int></first>
            |        <second><boolean>true</boolean></second>
            |      </record>
            |    </value>
            |  </entry>
            |  <entry>
            |    <key>1</key>
            |    <value>
            |      <record>
            |        <first><int>1</int></first>
            |        <second><boolean>false</boolean></second>
            |      </record>
            |    </value>
            |  </entry>
            |</map>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("of simple keys and values where the key's schema is lazy") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Schema.map[Int, Value](Schema.defer(Schema[Int]), Schema[Value])
        val input  = Map(0 -> Value(0, true), 1 -> Value(1, false))
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        val expected =
          """<map>
            |  <entry>
            |    <key>0</key>
            |    <value>
            |      <record>
            |        <first><int>0</int></first>
            |        <second><boolean>true</boolean></second>
            |      </record>
            |    </value>
            |  </entry>
            |  <entry>
            |    <key>1</key>
            |    <value>
            |      <record>
            |        <first><int>1</int></first>
            |        <second><boolean>false</boolean></second>
            |      </record>
            |    </value>
            |  </entry>
            |</map>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("of complex keys with transformation to primitive keys") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Schema.map[KeyWrapper, ValueWrapper]
        val input = Map(
          KeyWrapper("wrapped_key_1") -> ValueWrapper("some_value"),
          KeyWrapper("wrapped_key_2") -> ValueWrapper("some_other_value")
        )
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        val expected =
          """<map>
            |  <entry>
            |    <key>wrapped_key_1</key>
            |    <value>
            |      <record><value><string>some_value</string></value></record>
            |    </value>
            |  </entry>
            |  <entry>
            |    <key>wrapped_key_2</key>
            |    <value>
            |      <record><value><string>some_other_value</string></value></record>
            |    </value>
            |  </entry>
            |</map>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      }
    ),
// Suite for Set of complex values
    suite("Set")(
      test("of complex values") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Schema.set[Value]
        val input  = Set(Value(0, true), Value(1, false))
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        val expected =
          """<set>
      <record>
      <first><int>0</int></first>
      <second><boolean>true</boolean></second>
      </record>
      <record>
      <first><int>1</int></first>
      <second>
      <boolean>false</boolean>
      </second>
      </record></set>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(xmlStr.trim)(equalTo(expected.trim))
      }
    ),
// Suite for records
    suite("record")(
      test("missing fields should be replaced by default values") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val inputXml =
          """<record>
            |  <field name="foo"><string>s</string></field>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        val inputXmlChunk: Chunk[Byte] = Chunk.fromArray(inputXml.getBytes(StandardCharsets.UTF_8))
        val expected                   = ListMap[String, Any]("foo" -> "s", "bar" -> 1)
        assertDecodes(recordSchema, expected, inputXmlChunk, cfg)
      },
      test("of primitives") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = recordSchema
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(schema)
          .encodeXml(
            ListMap[String, Any]("foo" -> "s", "bar" -> 1)
          )
          .toString()
        val expected =
          """<record><field name="foo">
            <string>s</string>
            </field><field name="bar">
             <int>1</int>
         </field></record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("of records") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = nestedRecordSchema
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(schema)
          .encodeXml(
            ListMap(
              "l1" -> "s",
              "l2" -> ListMap("foo" -> "s", "bar" -> 1)
            )
          )
          .toString()
        val expected =
          """<record>
            |  <field name="l1">
            |    <string>s</string>
            |  </field>
            |  <field name="l2">
            |    <record>
            |      <field name="foo">
            |        <string>s</string>
            |      </field>
            |      <field name="bar">
            |        <int>1</int>
            |      </field>
            |    </record>
            |  </field>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("case class") {
        check(searchRequestGen) { searchRequest =>
          // Generate the expected XML output.
          val expectedXml: String =
            XmlCodec
              .xmlEncoder(XmlCodec.Config.default)(searchRequestSchema)
              .encodeXml(searchRequest)
              .toString
          assertEncodesXml(
            searchRequestSchema,
            searchRequest,
            expectedXml
          )
        }
      },
      test("case object") {
        assertEncodesXml(
          schemaObject,
          Singleton,
          "<record/>"
        )
      },
      test("record with option fields") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = recordWithOptionSchema
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(schema)
          .encodeXml(
            ListMap[String, Any]("foo" -> Some("s"), "bar" -> None)
          )
          .toString()
        val expected =
          """<record><field name="foo">
              <string>s</string>
             </field><field name="bar">
              <null/>
            </field></record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("record with option fields and flag to encode nulls") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false, explicitNulls = true)
        val schema = recordWithOptionSchema
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(schema)
          .encodeXml(
            ListMap[String, Any]("foo" -> Some("s"), "bar" -> None)
          )
          .toString()
        val expected =
          """<record><field name="foo">
                <string>s</string>
                </field><field name="bar">
                 <null/>
           </field></record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("case class with option fields omitted when empty") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = WithOptionFields.schema
        val input  = WithOptionFields(Some("s"), None)
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        val expected =
          """<record>
    <a><string>s</string></a>
    <b><null/></b>
    </record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(xmlStr.trim)(equalTo(expected.trim))
      }
    ),
    suite("enumeration")(
      test("of primitives") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = enumSchema
        val input  = "foo"
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        val expected =
          """<enum type="string"><string>foo</string></enum>""".stripMargin
            .replaceAll("\n", "")
            .replaceAll(">\\s+<", "><")
        assert(xmlStr.trim)(equalTo(expected.trim))
      },
      test("ADT") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Schema[Enumeration]
        val input  = Enumeration(StringValue("foo"))
        val expected =
          """<record>
      <oneOf><enum type="StringValue">
      <record>
      <value><string>foo</string></value>
      </record>
      </enum></oneOf>
      </record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        assert(xmlStr.trim)(equalTo(expected.trim))
      },
      test("ADT with annotation") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Schema[Enumeration2]
        val input  = Enumeration2(StringValue2("foo2"))
        val expected =
          """<record>
      <oneOf><enum _type="StringValue2">
      <record _type="StringValue2">
      <value><string>foo2</string>
      </value></record></enum></oneOf>
      </record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        assert(xmlStr.trim)(equalTo(expected.trim))
      },
      test("transient field annotation") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = searchRequestWithTransientFieldSchema
        val input  = SearchRequestWithTransientField("foo", 10, 20, "bar")
        val expected =
          """<record>
            |  <query><string>foo</string></query>
            |  <pageNumber><int>10</int></pageNumber>
            |  <resultPerPage><int>20</int></resultPerPage>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        assert(xmlStr.trim)(equalTo(expected.trim))
      },
      test("case name annotation") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = PaymentMethod.schema
        val input  = WireTransfer("foo", "bar")

        val expected =
          """<enum type="wire_transfer">
      <record><accountNumber><string>foo</string>
      </accountNumber><bankCode><string>bar</string>
      </bankCode></record>
      </enum>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        assert(xmlStr.trim)(equalTo(expected.trim))
      },
      test("transient case annotation") {
        val cfg      = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema   = PaymentMethod.schema
        val input    = PayPal("foo@bar.com")
        val expected = "<enum/>"
        val xmlStr   = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        assert(xmlStr.trim)(equalTo(expected.trim))
      },
      test("case name annotation with discriminator") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Subscription.schema
        val input  = Recurring("monthly", 10)
        val expected =
          """<enum type="recurring"><record type="recurring">
      <period>
      <string>monthly</string>
      </period><amount><int>10</int>
      </amount></record></enum>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        assert(xmlStr.trim)(equalTo(expected.trim))
      },
      test("case name annotation with empty fields") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Subscription.schema
        val input  = Subscription.Unlimited(None)
        val expected =
          """<enum type="unlimited">
      <record type="unlimited">
      <until><null/></until>
      </record></enum>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        assert(xmlStr.trim)(equalTo(expected.trim))
      },
      test("pretty printing with discriminator field") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = true)
        val xml = XmlCodec
          .xmlEncoder(cfg)(Schema[OneOf4])
          .encodeXml(RecordExampleWithDiscriminator(f1 = Some("test"), f2 = None))
          .toString()
        val expected =
          """<enum type="RecordExampleWithDiscriminator">
            <record type="RecordExampleWithDiscriminator">
              <field name="$f1">
                <string>test</string>
              </field>
            </record>
          </enum>""".stripMargin
        assert(normalize(xml.trim))(equalTo(normalize(expected.trim)))
      },
      test("pretty printing with discriminator key") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = true)
        val xml = XmlCodec
          .xmlEncoder(cfg)(Schema[OneOf])
          .encodeXml(StringValue("test"))
          .toString()
        val expected =
          """<enum type="StringValue">
            <record>
              <value>
                <string>test</string>
              </value>
            </record>
          </enum>""".stripMargin
        assert(normalize(xml.trim))(equalTo(normalize(expected.trim)))
      }
    ),
    suite("with no discriminator")(
      test("example 1") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Prompt.schema
        val input  = Prompt.Single("hello")
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        val expected =
          """<record>
            |  <value><string>hello</string></value>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(xmlStr.trim)(equalTo(expected.trim))
      },
      test("example 2") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Prompt.schema
        val input  = Prompt.Multiple(List("hello", "world"))
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        val expected =
          """<record>
            |  <value>
            |    <seq>
            |      <string>hello</string>
            |      <string>world</string>
            |    </seq>
            |  </value>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(xmlStr.trim)(equalTo(expected.trim))
      }
    ),
    suite("dynamic direct mapping")(
      test("record") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val schema = Schema.dynamicValue.annotate(directDynamicMapping())
        val input = DynamicValue.Record(
          TypeId.Structural,
          ListMap(
            "foo" -> DynamicValue.Primitive("s", StandardType.StringType),
            "bar" -> DynamicValue.Primitive(1, StandardType.IntType)
          )
        )
        val xmlStr = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(input).toString()
        val expected =
          """<record>
      <foo>
      <enum type="String"><string>s</string></enum>
      </foo>
      <bar>
      <enum type="Int"><int>1</int></enum>
      </bar>
      </record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(xmlStr.trim)(equalTo(expected.trim))
      }
    ),
    suite("zio.schema.codec.Xml encoding")(
      test("Xml.Record") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(recordSchema)
          .encodeXml(
            ListMap[String, Any]("foo" -> "s", "bar" -> 1)
          )
          .toString()
        val expected =
          """<record>
            |  <field name="foo"><string>s</string></field>
            |  <field name="bar"><int>1</int></field>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("Xml.Arr") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val unionSchema: Schema[Either[String, Int]] = Schema.either(
          Schema.Primitive(StandardType.StringType),
          Schema.Primitive(StandardType.IntType)
        )
        val arrSchema: Schema[List[Either[String, Int]]] = Schema.list(unionSchema)
        val xmlStr = XmlCodec
          .xmlEncoder(cfg)(arrSchema)
          .encodeXml(
            List(Left("foo"), Right(1))
          )
          .toString()
        val expected =
          """<seq>
    <left><string>foo</string></left>
    <right><int>1</int></right>
    </seq>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("Xml.Num Int") {
        val intSchema: Schema[Int] = Schema.Primitive(StandardType.IntType)
        val value                  = 1
        val expectedXml            = "<int>1</int>"
        assertEncodes(intSchema, value, charSequenceToByteChunk(expectedXml))
      },
      test("Xml.Num Long") {
        val longSchema: Schema[Long] = Schema.Primitive(StandardType.LongType)
        val value                    = 1L
        val expectedXml              = "<long>1</long>"
        assertEncodes(longSchema, value, charSequenceToByteChunk(expectedXml))
      },
      test("Xml.Num Double") {
        val doubleSchema: Schema[Double] = Schema.Primitive(StandardType.DoubleType)
        val value                        = 1.1
        val expectedXml                  = "<double>1.1</double>"
        assertEncodes(doubleSchema, value, charSequenceToByteChunk(expectedXml))
      },
      test("Xml.Str") {
        val stringSchema: Schema[String] = Schema.Primitive(StandardType.StringType)
        val value                        = "foo"
        val expectedXml                  = "<string>foo</string>"
        assertEncodes(stringSchema, value, charSequenceToByteChunk(expectedXml))
      },
      test("Xml.Bool") {
        val boolSchema: Schema[Boolean] = Schema.Primitive(StandardType.BoolType)
        val value                       = true
        val expectedXml                 = "<boolean>true</boolean>"
        assertEncodes(boolSchema, value, charSequenceToByteChunk(expectedXml))
      },
      test("Xml.Null") {
        val optionSchema: Schema[Option[String]] = Schema.Optional(Schema.Primitive(StandardType.StringType))
        val value: Option[String]                = None
        val expectedXml                          = "<null/>"
        assertEncodes(optionSchema, value, charSequenceToByteChunk(expectedXml))
      }
    ),
    suite("EnumN")(
      test("Respects the case name annotation") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr = XmlCodec.xmlEncoder(cfg)(Enum23Cases.schema).encodeXml(Enum23Cases.Case1("foo")).toString()

        val expected =
          """<enum type="NumberOne"><record><value><string>foo</string></value></record></enum>"""
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      }
    ),
    suite("Streams")(
      suite("Streams of integers")(
        test("Encodes a stream with multiple integers") {
          val cfg = XmlCodec.Config.default.copy(prettyPrint = false)

          val expected = charSequenceToByteChunk(
            "<int>1</int>\n<int>2</int>\n<int>3</int>\n<int>4</int>\n<int>5</int>"
          )
          assertEncodesMany(Schema[Int], 1 to 5, expected, cfg)
        },
        test("Encodes a stream with multiple integers to an array") {
          val cfg = XmlCodec.Config.default.copy(prettyPrint = false)

          val expected = charSequenceToByteChunk(
            "<int>1</int>\n<int>2</int>\n<int>3</int>\n<int>4</int>\n<int>5</int>"
          )
          assertEncodesMany(Schema[Int], 1 to 5, expected, cfg)
        },
        test("Decodes a stream with multiple integers separated by newlines") {
          val input = charSequenceToByteChunk(
            """<root>
        <int>1</int>
        <int>2</int>
        <int>3</int>
        <int>4</int>
        <int>5</int>
        </root>"""
          )
          assertDecodesMany(Schema[Int], Chunk.fromIterable(1 to 5), input)
        },
        test("Decodes a stream with multiple integers separated by spaces") {
          val input = charSequenceToByteChunk(
            "<root><int>1</int> <int>2</int> <int>3</int> <int>4</int> <int>5</int></root>"
          )
          assertDecodesMany(Schema[Int], Chunk.fromIterable(1 to 5), input)
        },
        test("Decodes a stream with multiple integers encoded as an array") {
          val cfg = XmlCodec.Config.default.copy(ignoreEmptyCollections = false)
          val input = charSequenceToByteChunk(
            "<array><item><int>1</int></item><item><int>2</int></item><item><int>3</int></item><item><int>4</int></item><item><int>5</int></item></array>"
          )
          assertDecodesMany(Schema[Int], Chunk.fromIterable(1 to 5), input, cfg)
        },
        test("Decodes a stream with multiple integers encoded as an array with additional whitespace") {
          val cfg = XmlCodec.Config.default.copy(ignoreEmptyCollections = false)
          val input = charSequenceToByteChunk(
            """<array>
  <item><int>1</int></item>
  <item><int>2</int></item>
  <item><int>3</int></item>
  <item><int>4</int></item>
  <item><int>5</int></item>
         </array>""".stripMargin
          )
          assertDecodesMany(Schema[Int], Chunk.fromIterable(1 to 5), input, cfg)
        }
      ),
      suite("Streams of booleans")(
        test("Encodes a stream with multiple booleans") {
          val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
          val expected = charSequenceToByteChunk(
            "<boolean>true</boolean>\n<boolean>true</boolean>\n<boolean>false</boolean>"
          )
          assertEncodesMany(Schema[Boolean], List(true, true, false), expected, cfg)
        },
        test("Encodes a stream with multiple booleans to an array") {
          val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
          val xmlStr =
            XmlCodec.xmlEncoder(cfg)(Schema.list(Schema[Boolean])).encodeXml(List(true, true, false)).toString()
          val expected =
            """<seq><boolean>true</boolean><boolean>true</boolean><boolean>false</boolean></seq>"""
          assert(normalize(xmlStr))(equalTo(normalize(expected)))
        },
        test("Decodes a stream with multiple booleans separated by newlines") {
          val input = charSequenceToByteChunk(
            """<root>
        <boolean>true</boolean>
        <boolean>true</boolean>
        <boolean>false</boolean>
      </root>"""
          )
          assertDecodesMany(Schema[Boolean], Chunk(true, true, false), input)
        },
        test("Decodes a stream with multiple booleans separated by spaces") {
          val input = charSequenceToByteChunk(
            "<root> <boolean>true</boolean> <boolean>true</boolean> <boolean>false</boolean> </root>"
          )
          assertDecodesMany(Schema[Boolean], Chunk(true, true, false), input)
        },
        test("Decodes a stream with multiple booleans as an array") {
          val cfg = XmlCodec.Config.default.copy(ignoreEmptyCollections = false)
          val input = charSequenceToByteChunk(
            """<array>
        <item><boolean>true</boolean></item>
        <item><boolean>true</boolean></item>
        <item><boolean>false</boolean></item>
      </array>"""
          )
          assertDecodesMany(Schema[Boolean], Chunk(true, true, false), input, cfg)
        },
        test("Decodes a stream with multiple booleans not separated (adjacent elements)") {
          val input = charSequenceToByteChunk(
            "<root><boolean>true</boolean><boolean>true</boolean><boolean>false</boolean><boolean>false</boolean></root>"
          )
          assertDecodesMany(Schema[Boolean], Chunk(true, true, false, false), input)
        }
      ),
      suite("Streams of strings")(
        test("Encodes a stream with multiple strings") {
          val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
          val expected = charSequenceToByteChunk(
            "<string>a</string>\n<string>b</string>\n<string>c</string>"
          )
          assertEncodesMany(Schema[String], List("a", "b", "c"), expected, cfg)
        },
        test("Encodes a stream with multiple strings as an array") {
          val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
          val xmlStr = XmlCodec.xmlEncoder(cfg)(Schema.list(Schema[String])).encodeXml(List("a", "b", "c")).toString()
          val expected =
            """<seq><string>a</string><string>b</string><string>c</string></seq>"""
          assert(normalize(xmlStr))(equalTo(normalize(expected)))
        },
        test("Decodes a stream with multiple strings separated by newlines") {
          val input = charSequenceToByteChunk(
            """<root>
        <string>a</string>
        <string>b</string> 
        <string>c</string>
      </root>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
          assertDecodesMany(Schema[String], Chunk("abc"), input)
        },
        test("Decodes a stream with multiple strings as an array") {
          val cfg = XmlCodec.Config.default.copy(ignoreEmptyCollections = false)
          val input = charSequenceToByteChunk(
            """<array>
        <item><string>a</string></item>
        <item><string>b</string></item>
        <item><string>c</string></item>
      </array>"""
          )
          assertDecodesMany(Schema[String], Chunk("\n        a\n        b\n        c\n      "), input, cfg)
        },
        test("Decodes a stream with multiple strings adjacent (no separators)") {
          val input = charSequenceToByteChunk(
            "<root><string>a</string><string>b</string><string>c</string><string>d</string></root>"
          )
          assertDecodesMany(Schema[String], Chunk("abcd"), input)
        }
      ),
      suite("Stream of records")(
        test("Encodes a stream with multiple records") {
          val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
          val xmlStr = XmlCodec
            .xmlEncoder(cfg)(Schema.list(personSchema))
            .encodeXml(List(Person("Alice", 1), Person("Bob", 2), Person("Charlie", 3)))
            .toString()
          val expected =
            """<seq><record><name><string>Alice</string></name><age><int>1</int></age></record><record><name><string>Bob</string></name><age><int>2</int></age></record><record><name><string>Charlie</string></name><age><int>3</int></age></record></seq>""".stripMargin
          assert(normalize(xmlStr))(equalTo(normalize(expected)))
        },
        test("Encodes a stream with multiple records as an array") {
          val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
          val xmlStr = XmlCodec
            .xmlEncoder(cfg)(Schema.list(personSchema))
            .encodeXml(List(Person("Alice", 1), Person("Bob", 2), Person("Charlie", 3)))
            .toString()
          val expected =
            """<seq><record><name><string>Alice</string></name><age><int>1</int></age></record><record><name><string>Bob</string></name><age><int>2</int></age></record><record><name><string>Charlie</string></name><age><int>3</int></age></record></seq>"""
          assert(normalize(xmlStr))(equalTo(normalize(expected)))
        },
        test("Encodes a stream with no records") {
          val cfg      = XmlCodec.Config.default.copy(prettyPrint = false)
          val expected = charSequenceToByteChunk("")
          assertEncodesMany(personSchema, List.empty[Person], expected, cfg)
        },
        test("Encodes a stream with no records as an array") {
          val cfg      = XmlCodec.Config.default.copy(prettyPrint = false)
          val xmlStr   = XmlCodec.xmlEncoder(cfg)(Schema.list(personSchema)).encodeXml(List.empty[Person]).toString()
          val expected = "<seq></seq>"
          assert(normalize(xmlStr))(equalTo(normalize(expected)))
        },
        test("Decodes a stream with no records") {
          val input = charSequenceToByteChunk("")
          assertDecodesMany(personSchema, Chunk.empty, input)
        }
      )
    )
  )

  private val decoderSuite = suite("decoding")(
    suite("primitive")(
      test("unit") {
        val xmlStr = "<unit/>"
        val node   = scala.xml.XML.loadString(xmlStr)
        val result = XmlCodec.xmlDecoder(Schema[Unit]).decodeXml(node)
        assert(result)(equalTo(Right(())))
      },
      suite("string")(
        test("any") {
          check(Gen.string) { s =>
            val xmlStr = s"<string>$s</string>"
            val node   = scala.xml.XML.loadString(xmlStr)
            val result = XmlCodec.xmlDecoder(Schema[String]).decodeXml(node)
            assert(result)(equalTo(Right(s)))
          }
        }
      ),
      test("Currency") {
        check(Gen.currency) { currency =>
          val xmlStr = s"<currency>${currency.getCurrencyCode}</currency>"
          val node   = scala.xml.XML.loadString(xmlStr)
          val result = XmlCodec.xmlDecoder(Schema[java.util.Currency]).decodeXml(node)
          assert(result)(equalTo(Right(currency)))
        }
      }
    ),
    suite("Generic record")(
      test("with extra fields") {
        val xmlStr =
          """<record>
            |    <field name="foo"><string>s</string></field>
            |    <field name="bar"><int>1</int></field>
            |    <field name="baz"><string>2</string></field>
            |  </record>""".stripMargin
        assertDecodes(
          recordSchema,
          ListMap("foo" -> "s", "bar" -> 1),
          charSequenceToByteChunk(xmlStr)
        )
      },
      test("with empty optional and collection fields without default values") {
        val xmlStr =
          """<record>
            |    <f1><string>test</string></f1>
            |</record>""".stripMargin
        assertDecodes(
          RecordExample.schema,
          RecordExample(
            f1 = "test",
            f2 = None,
            f3 = None,
            f4 = None,
            f5 = None,
            f6 = None,
            f7 = None,
            f8 = None,
            f9 = None,
            f10 = None,
            f11 = None,
            f12 = None,
            f13 = None,
            f14 = None,
            f15 = None,
            f16 = None,
            f17 = Nil,
            f18 = Vector.empty,
            f19 = None,
            f20 = None,
            f21 = Vector.empty,
            f22 = Nil,
            f23 = None
          ),
          charSequenceToByteChunk(xmlStr)
        )
      },
      test("missing required fields") {
        val xmlStr = "<root><RecordExample/></root>"
        assertDecodesToError(
          RecordExample.schema,
          xmlStr,
          List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Message(missing), ObjectAccess(f1)"))
        )
      },
      test("aliased field") {
        val xmlStr =
          """<record>
            |    <f1><string>test</string></f1>
            |    <f2><string>alias</string></f2>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodes(
          RecordExample.schema,
          RecordExample(
            f1 = "test",
            f2 = Some("alias"),
            f3 = None,
            f4 = None,
            f5 = None,
            f6 = None,
            f7 = None,
            f8 = None,
            f9 = None,
            f10 = None,
            f11 = None,
            f12 = None,
            f13 = None,
            f14 = None,
            f15 = None,
            f16 = None,
            f17 = Nil,
            f18 = Vector.empty,
            f19 = None,
            f20 = None,
            f21 = Vector.empty,
            f22 = Nil,
            f23 = None
          ),
          charSequenceToByteChunk(xmlStr)
        )
      },
      test("reject extra fields") {
        XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr =
          """<record>
            |  <field name="f1"><string>test</string></field>
            |  <field name="extraField"><string>extra</string></field>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodesToError(
          RecordExample.schema.annotate(rejectExtraFields()),
          xmlStr,
          List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: extra field: extraField"))
        )
      },
      test("optional field with schema or annotated default value") {
        val xmlStr =
          """<record>
            |    <field name="f1"><string>test</string></field>
            |</record>""".stripMargin
        assertDecodes(
          RecordExampleWithOptField.schema,
          RecordExampleWithOptField(
            f1 = Some("test"),
            f2 = None,
            f3 = None,
            f4 = "",
            f5 = "hello",
            f6 = None,
            f7 = None,
            f8 = None,
            f9 = None,
            f10 = None,
            f11 = None,
            f12 = None,
            f13 = None,
            f14 = None,
            f15 = None,
            f16 = None,
            f17 = None,
            f18 = None,
            f19 = None,
            f20 = None,
            f21 = None,
            f22 = None,
            f23 = None
          ),
          charSequenceToByteChunk(xmlStr)
        )
      }
    ),
    suite("EnumN")(
      test("Respects the case name annotation") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr = XmlCodec.xmlEncoder(cfg)(Enum23Cases.schema).encodeXml(Enum23Cases.Case1("foo")).toString()

        val expected =
          """<enum type="NumberOne"><record><value><string>foo</string></value></record></enum>"""
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("Respects case aliases") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr = XmlCodec.xmlEncoder(cfg)(Enum23Cases.schema).encodeXml(Enum23Cases.Case1("foo")).toString()
        val expected =
          """<enum type="NumberOne"><record><value><string>foo</string></value></record></enum>"""
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      }
    ),
    suite("transform")(
      test("string") {
        val stringSchema    = Schema.Primitive(StandardType.StringType)
        val transformSchema = stringSchema.transform[Int](_ => 1, _.toString)
        val xmlStr          = "<string>string</string>"
        assertDecodes(transformSchema, 1, charSequenceToByteChunk(xmlStr))
      },
      test("failed") {
        val errorMessage = "zio.schema.codec.DecodeError$ReadError: I'm sorry Dave, I can't do that"
        val schema: Schema[Int] =
          Schema
            .Primitive(StandardType.StringType)
            .transformOrFail[Int](_ => Left(errorMessage), i => Right(i.toString))
        val xmlStr        = s"<string>123</string>"
        val node          = scala.xml.XML.loadString(xmlStr)
        val result        = XmlCodec.xmlDecoder(schema).decodeXml(node)
        val expectedError = s"$errorMessage"
        assert(result)(isLeft(hasMessage(equalTo(expectedError))))
      }
    ),
    suite("fallback")(
      test("correctly fallbacks to right") {

        val xmlStr =
          """<root>
            |  <right><string>hello</string></right>
            |</root>""".stripMargin
        assertDecodes(
          Schema.Fallback(Schema[Int], Schema[String]),
          Fallback.Right("hello"),
          charSequenceToByteChunk(xmlStr)
        )
      },
      test("correctly fallbacks to left") {
        val xmlStr =
          """<root>
            |  <left><int>30</int></left>
            |</root>""".stripMargin
        assertDecodes(
          Schema.Fallback(Schema[Int], Schema[String]),
          Fallback.Left(30),
          charSequenceToByteChunk(xmlStr)
        )
      },
      test("correctly fallbacks to right with full decode") {
        val xmlStr = "<root>hello</root>"
        assertDecodes(
          Schema.Fallback(Schema[Int], Schema[String], true),
          Fallback.Right("hello"),
          charSequenceToByteChunk(xmlStr)
        )
      },
      test("correctly fallbacks to both with full decode") {
        val xmlStr =
          """<root>
            |  <left><int>30</int></left>
            |  <right><string>hello</string></right>
            |</root>""".stripMargin
        val node     = scala.xml.XML.loadString(xmlStr)
        val result   = XmlCodec.xmlDecoder(Schema.Fallback(Schema[Int], Schema[String], true)).decodeXml(node)
        val expected = Right(Fallback.Both(30, "hello"))
        assert(result)(equalTo(expected))
      },
      test("correctly fallbacks to left with full decode") {
        val xmlStr = "<root><int>30</int></root>"
        assertDecodes(
          Schema.Fallback(Schema[Int], Schema[String], true),
          Fallback.Left(30),
          charSequenceToByteChunk(xmlStr)
        )
      }
    ),
    suite("case class")(
      test("case object") {
        val cfg      = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr   = XmlCodec.xmlEncoder(cfg)(schemaObject).encodeXml(Singleton).toString()
        val expected = "<record/>"
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("optional") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val value  = OptionalSearchRequest("test", 0, 10, Schema[String].defaultValue.getOrElse(""))
        val xmlStr = XmlCodec.xmlEncoder(cfg)(optionalSearchRequestSchema).encodeXml(value).toString()
        val expected =
          """<record>
            |  <query><string>test</string></query>
            |  <pageNumber><int>0</int></pageNumber>
            |  <resultPerPage><int>10</int></resultPerPage>
            |  <nextPage><string></string></nextPage>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("reject extra fields") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr1 =
          """<record>
            |  <name><string>test</string></name>
            |  <age><int>10</int></age>
            |  <extraField><int>10</int></extraField>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        val xmlStr2 =
          """<record>
            |  <extraField><int>10</int></extraField>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodesToError(
          PersonWithRejectExtraFields.schema,
          xmlStr1,
          XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Extra field encountered") :: Nil,
          cfg
        ) *>
          assertDecodesToError(
            schemaObject.annotate(rejectExtraFields()),
            xmlStr2,
            XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Extra field encountered") :: Nil,
            cfg
          )
      },
      test("reject duplicated fields") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)

        // Test duplicate "name"
        val xmlStr1 =
          """<record>
            |  <field name="name"><string>test</string></field>
            |  <field name="name"><string>duplicate</string></field>
            |  <field name="age"><int>10</int></field>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")

        // Test duplicate "age"
        val xmlStr2 =
          "<record><field name='name'><string>test</string></field><field name='age'><int>10</int></field><field name='age'><int>100</int></field></record>"

        assertDecodesToError(
          personSchema,
          xmlStr1,
          XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Message(duplicate), ObjectAccess(name)") :: Nil,
          cfg
        ) *>
          assertDecodesToError(
            personSchema,
            xmlStr2,
            XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Message(duplicate), ObjectAccess(age)") :: Nil
          )
      },
      test("transient field annotation with default value in class definition") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val value  = SearchRequestWithTransientField("test", 0, 10)
        val xmlStr = XmlCodec.xmlEncoder(cfg)(searchRequestWithTransientFieldSchema).encodeXml(value).toString()
        val expected =
          """<record>
            |  <query><string>test</string></query>
            |  <pageNumber><int>0</int></pageNumber>
            |  <resultPerPage><int>10</int></resultPerPage>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("transient field annotation with default value implicitly available for the field type") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        case class CaseClassWithTransientField(transient: String)
        val schema = Schema.CaseClass1[String, CaseClassWithTransientField](
          id0 = TypeId.fromTypeName("SearchRequestWithTransientField"),
          field0 = Schema.Field(
            name0 = "transient",
            schema0 = Schema[String],
            get0 = _.transient,
            set0 = (x, transient) => x.copy(transient = transient),
            annotations0 = Chunk(new transientField())
          ),
          defaultConstruct0 = new CaseClassWithTransientField(_)
        )
        val value    = CaseClassWithTransientField(Schema[String].defaultValue.toOption.get)
        val xmlStr   = XmlCodec.xmlEncoder(cfg)(schema).encodeXml(value).toString()
        val expected = "<record/>"
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("fieldDefaultValue") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val value  = FieldDefaultValueSearchRequest("test", 0, 10, "test")
        val xmlStr = XmlCodec.xmlEncoder(cfg)(fieldDefaultValueSearchRequestSchema).encodeXml(value).toString()
        val expected =
          """<record>
            |  <query><string>test</string></query>
            |  <pageNumber><int>0</int></pageNumber>
            |  <resultPerPage><int>10</int></resultPerPage>
            |  <nextPage><string>test</string></nextPage>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("backticked field name") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val value  = BacktickedFieldName("test")
        val xmlStr = XmlCodec.xmlEncoder(cfg)(BacktickedFieldName.schema).encodeXml(value).toString()
        val expected =
          """<record>
            |  <x-api-key><string>test</string></x-api-key>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("field name with alias - id") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val value  = Order(1, BigDecimal.valueOf(10), "test")
        val xmlStr = XmlCodec.xmlEncoder(cfg)(Order.schema).encodeXml(value).toString()
        val expected =
          """<record>
            |  <orderId><int>1</int></orderId>
            |  <value><bigDecimal>10</bigDecimal></value>
            |  <description><string>test</string></description>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("field name with alias - order_id") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val value  = Order(1, BigDecimal.valueOf(10), "test")
        val xmlStr = XmlCodec.xmlEncoder(cfg)(Order.schema).encodeXml(value).toString()
        val expected =
          """<record>
            |  <orderId><int>1</int></orderId>
            |  <value><bigDecimal>10</bigDecimal></value>
            |  <description><string>test</string></description>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assert(normalize(xmlStr))(equalTo(normalize(expected)))
      },
      test("old field name rejected") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr =
          """<record>
            |  <order-id><int>1</int></order-id>
            |  <value><int>10</int></value>
            |  <description><string>test</string></description>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodesToError(
          Order.schema,
          xmlStr,
          XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Message(missing), ObjectAccess(orderId)") :: Nil,
          cfg
        )
      },
      test("field name with alias - no alias") {
        val cfg   = XmlCodec.Config.default.copy(prettyPrint = false)
        val value = Order(1, BigDecimal.valueOf(10), "test")
        val xmlStr =
          """<record>
            |  <field name ="orderId"><int>1</int></field>
            |  <field name  ="value"><int>10</int></field>
            |  <field name ="description"><string>test</string></field>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodes(Order.schema, value, charSequenceToByteChunk(xmlStr), cfg)
      },
      test("with option fields encoded as null") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr =
          """<record>
            |  <foo><string>s</string></foo>
            |  <bar></bar>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodes(
          recordWithOptionSchema,
          ListMap("foo" -> Some("s"), "bar" -> None),
          charSequenceToByteChunk(xmlStr),
          cfg
        )
      },
      test("with transient fields encoded as implicitly available schema default values") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr = "<record/>"
        assertDecodes(recordWithTransientSchema, ListMap("foo" -> "", "bar" -> 0), charSequenceToByteChunk(xmlStr), cfg)
      },
      test("case class with option fields encoded as null") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr =
          """<record>
            |  <a><string>s</string></a>
            |  <b></b>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodes(WithOptionFields.schema, WithOptionFields(Some("s"), None), charSequenceToByteChunk(xmlStr), cfg)
      },
      test("case class with int option field present (at end) from pretty printed json") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr =
          """<record>
            |  <a><string>s</string></a>
            |  <b><int>1</int></b>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodes(
          WithOptionFields.schema,
          WithOptionFields(Some("s"), Some(1)),
          charSequenceToByteChunk(xmlStr),
          cfg
        )
      },
      test("case class with option fields omitted when empty") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr =
          """<record>
            |  <a><string>s</string></a>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodes(WithOptionFields.schema, WithOptionFields(Some("s"), None), charSequenceToByteChunk(xmlStr), cfg)
      },
      test("case class with complex option field correctly decodes") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr =
          """<record>
            |  <order>
            |      <id><int>1</int></id>
            |      <value><int>10</int></value>
            |      <description><string>test</string></description>
            |  </order>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodes(
          WithComplexOptionField.schema,
          WithComplexOptionField(Some(Order(1, BigDecimal.valueOf(10), "test"))),
          charSequenceToByteChunk(xmlStr),
          cfg
        )
      }
    ),
    suite("case class with more than 64 fields")(
      test("required and optional fields") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr =
          """<record>
            |  <field name="f00"><boolean>true</boolean></field>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodes(
          BigProduct.schema,
          BigProduct(f00 = true, f67 = None, f68 = Nil, f69 = Vector.empty), // adjust according to your BigProduct definition
          charSequenceToByteChunk(xmlStr),
          cfg
        )
      },
      test("missing required fields") {
        val cfg    = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr = "<record/>"
        assertDecodesToError(
          BigProduct.schema,
          xmlStr,
          XmlError.Raw("zio.schema.codec.DecodeError$ReadError: missing field: f00") :: Nil,
          cfg
        )
      },
      test("reject extra fields") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr =
          """<record>
            |  <field name="f00"><boolean>true</boolean></field>
            |  <field name="extraField"><int>10</int></field>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodesToError(
          BigProduct.schema.annotate(rejectExtraFields()),
          xmlStr,
          XmlError.Raw("zio.schema.codec.DecodeError$ReadError: extra field : extraField") :: Nil,
          cfg
        )
      },
      test("reject duplicated fields") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr =
          """<record>
            |  <field name="f00"><boolean>true</boolean></field>
            |  <field name="age"><int>10</int></field>
            |  <field name="f00"><boolean>false</boolean></field>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodesToError(
          BigProduct.schema,
          xmlStr,
          XmlError.Raw("zio.schema.codec.DecodeError$ReadError: duplicate field: f00") :: Nil,
          cfg
        )
      },
      test("field name with alias - id") {
        val cfg = XmlCodec.Config.default.copy(prettyPrint = false)
        val xmlStr =
          """<record>
            |  <field name="f00"><boolean>true</boolean></field>
            |  <field name="f-01"><int>123</int></field>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        assertDecodes(
          BigProduct.schema,
          BigProduct(f00 = true, f01 = Some(123.toByte), f67 = None, f68 = Nil, f69 = Vector.empty), // adjust according to your BigProduct definition
          charSequenceToByteChunk(xmlStr),
          cfg
        )
      }
    ),
    suite("enums")(
      test("case name aliases - default") {
        assertDecodes(
          PaymentMethod.schema,
          CreditCard("foo", 12, 2022),
          charSequenceToByteChunk(
            """<record type="CreditCard">
              |  <number><string>foo</string></number>
              |  <expirationMonth><int>12</int></expirationMonth>
              |  <expirationYear><int>2022</int></expirationYear>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("case name aliases - first alias") {
        assertDecodes(
          PaymentMethod.schema,
          CreditCard("foo", 12, 2022),
          charSequenceToByteChunk(
            """<record type="credit_card">
              |    <number><string>foo</string></number>
              |    <expirationMonth><int>12</int></expirationMonth>
              |    <expirationYear><int>2022</int></expirationYear>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("case name aliases - second alias") {
        assertDecodes(
          PaymentMethod.schema,
          CreditCard("foo", 12, 2022),
          charSequenceToByteChunk(
            """<record type="cc">
              |    <number><string>foo</string></number>
              |    <expirationMonth><int>12</int></expirationMonth>
              |    <expirationYear><int>2022</int></expirationYear>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("case name") {
        assertDecodes(
          PaymentMethod.schema,
          WireTransfer("foo", "bar"),
          charSequenceToByteChunk(
            """<record type="wire_transfer">
              |    <accountNumber><string>foo</string></accountNumber>
              |    <bankCode><string>bar</string></bankCode>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("no discriminator")(
        assertDecodesToError(
          PaymentMethod.schema,
          "<record/>",
          XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Missing discriminator 'type' on enum element") :: Nil
        )
      ),
      test("illegal case")(
        assertDecodesToError(
          PaymentMethod.schema,
          """<record type="cash"/>""",
          XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: cash") :: Nil
        )
      )
    ),
    suite("enums - with discriminator")(
      test("case name") {
        assertDecodes(
          Subscription.schema,
          Recurring("monthly", 100),
          charSequenceToByteChunk(
            """<record>
              |  <type><string>recurring</string></type>
              |  <period><string>monthly</string></period>
              |  <amount><int>100</int></amount>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("case name aliases - first alias") {
        assertDecodes(
          Subscription.schema,
          OneTime(1000),
          charSequenceToByteChunk(
            """<record>
              |  <type><string>one_time</string></type>
              |  <amount><int>1000</int></amount>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("case name aliases - second alias") {
        assertDecodes(
          Subscription.schema,
          OneTime(1000),
          charSequenceToByteChunk(
            """<record>
              |  <type><string>onetime</string></type>
              |  <amount><int>1000</int></amount>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("case name aliases - type in the middle") {
        assertDecodes(
          Subscription.schema,
          Recurring("monthly", 100),
          charSequenceToByteChunk(
            """<record>
              |  <period><string>monthly</string></period>
              |  <type><string>recurring</string></type>
              |  <amount><int>100</int></amount>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("case name aliases - type in the last place") {
        assertDecodes(
          Subscription.schema,
          OneTime(1000),
          charSequenceToByteChunk(
            """<record>
              |  <amount><int>1000</int></amount>
              |  <type><string>onetime</string></type>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("case name aliases - type in the last place and the content with an escaped string") {
        @discriminatorName("type")
        sealed trait Example {
          type Content
          def content: Content
        }
        object Example {
          @caseName("XML")
          final case class XmlInput(content: String) extends Example {
            override type Content = String
          }
          implicit val schema: Schema[Example] = DeriveSchema.gen
        }

        val expected = Example.XmlInput(
          "\n      <person><name>John</name><location>Sydney</location><email>jdoe@test.com</email></person>\n    "
        )

        val xmlStr =
          """
            |<record>
            |  <content>
            |    <string>
            |      &lt;person&gt;&lt;name&gt;John&lt;/name&gt;&lt;location&gt;Sydney&lt;/location&gt;&lt;email&gt;jdoe@test.com&lt;/email&gt;&lt;/person&gt;
            |    </string>
            |  </content>
            |  <type>
            |    <string>XML</string>
            |  </type>
            |</record>
            |""".stripMargin.trim.replaceAll("\n", "").replaceAll(">\\s+<", "><")

        assertDecodes(Example.schema, expected, charSequenceToByteChunk(xmlStr))
      },
      test("case name aliases - illegal discriminator value") {
        assertDecodesToError(
          Subscription.schema,
          """<record>
            |  <amount><int>1000</int></amount>
            |  <type><int>123</int></type>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><"),
          XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: 123") :: Nil
        )
      },
      test("case name - empty fields") {
        assertDecodes(
          Subscription.schema,
          Subscription.Unlimited(None),
          charSequenceToByteChunk(
            """<record>
              |  <enum><string>unlimited</string></enum>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      }
    ),
    suite("enums - with no discriminator")(
      test("example 1") {
        assertDecodes(
          Prompt.schema,
          Prompt.Single("hello"),
          charSequenceToByteChunk(
            """<record>
              |  <value><string>hello</string></value>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("example 2") {
        assertDecodes(
          Prompt.schema,
          Prompt.Single(("helloworld")),
          charSequenceToByteChunk(
            """<record>
              |  <value>
              |    <list>
              |      <string>hello</string>
              |      <string>world</string>
              |    </list>
              |  </value>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("wrong example") {
        assertDecodes(
          Prompt.schema,
          Prompt.Multiple(Nil),
          charSequenceToByteChunk("""<field name="prompt"><invalid/></field>""")
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
          charSequenceToByteChunk(
            """<record>
              |  <foo><string>s</string></foo>
              |  <bar><bigDecimal>1</bigDecimal></bar>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      }
    ),
    suite("Map")(
      test("of complex keys and values") {
        assertDecodes(
          Schema.map[Key, Value],
          Map(Key("a", 0) -> Value(0, true), Key("b", 1) -> Value(1, false)),
          charSequenceToByteChunk(
            """<map>
              |  <entry>
              |    <key>
              |      <record>
              |        <field name="name"><string>a</string></field>
              |        <field name="index"><int>0</int></field>
              |      </record>
              |    </key>
              |    <value>
              |      <record>
              |        <field name="first"><int>0</int></field>
              |        <field name="second"><boolean>true</boolean></field>
              |      </record>
              |    </value>
              |  </entry>
              |  <entry>
              |    <key>
              |      <record>
              |        <field name="name"><string>b</string></field>
              |        <field name="index"><int>1</int></field>
              |      </record>
              |    </key>
              |    <value>
              |      <record>
              |        <field name="first"><int>1</int></field>
              |        <field name="second"><boolean>false</boolean></field>
              |      </record>
              |    </value>
              |  </entry>
              |</map>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("of simple keys and values") {
        assertDecodes(
          Schema.map[Int, Value],
          Map(0 -> Value(0, true), 1 -> Value(1, false)),
          charSequenceToByteChunk(
            """<record>
              |  <field name="0">
              |    <record>
              |      <first><int>0</int></first>
              |      <second><boolean>true</boolean></second>
              |    </record>
              |  </field>
              |  <field name="1">
              |    <record>
              |      <first><int>1</int></first>
              |      <second><boolean>false</boolean></second>
              |    </record>
              |  </field>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("of simple keys and values where the key schema is lazy") {
        assertDecodes(
          Schema.map[Int, Value](Schema.defer(Schema[Int]), Schema[Value]),
          Map(0 -> Value(0, true), 1 -> Value(1, false)),
          charSequenceToByteChunk(
            """<record>
              |  <field name="0">
              |    <record>
              |      <first><int>0</int></first>
              |      <second><boolean>true</boolean></second>
              |    </record>
              |  </field>
              |  <field name="1">
              |    <record>
              |      <first><int>1</int></first>
              |      <second><boolean>false</boolean></second>
              |    </record>
              |  </field>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("of primitive keys with transformation to complex keys") {
        assertDecodes(
          Schema.map[KeyWrapper, ValueWrapper],
          Map(
            KeyWrapper("wrapped_key_1") -> ValueWrapper("some_value"),
            KeyWrapper("wrapped_key_2") -> ValueWrapper("some_other_value")
          ),
          charSequenceToByteChunk(
            """<record>
              |  <field name="wrapped_key_1">
              |    <record>
              |      <value><string>some_value</string></value>
              |    </record>
              |  </field>
              |  <field name="wrapped_key_2">
              |    <record>
              |      <value><string>some_other_value</string></value>
              |    </record>
              |  </field>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      }
    ),
    suite("Missing collection fields")(
      test("map") {
        assertDecodes(
          Schema[ListAndMapAndOption],
          ListAndMapAndOption(Nil, Map.empty, None),
          charSequenceToByteChunk(
            """<record>
              |  <seq name="list">
              |    <list/>
              |  </seq>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("list") {
        assertDecodes(
          Schema[ListAndMapAndOption],
          ListAndMapAndOption(Nil, Map.empty, None),
          charSequenceToByteChunk(
            """<field name="map">
           <record/>
           </field>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("set") {
        assertDecodes(
          Schema[SetWrapper],
          SetWrapper(Set.empty),
          charSequenceToByteChunk("""<record/>""")
        )
      },
      test("vector") {
        assertDecodes(
          Schema[VectorWrapper],
          VectorWrapper(Vector.empty),
          charSequenceToByteChunk("""<record/>""")
        )
      },
      test("chunck") {
        assertDecodes(
          Schema[ChunckWrapper],
          ChunckWrapper(Chunk.empty),
          charSequenceToByteChunk("""<record/>""")
        )
      }
    ),
    suite("zio.schema.codec.Xml decoding")(
      test("Xml.Record") {
        XmlCodec.Config.default.copy(prettyPrint = false)
        val expectedValue = ListMap[String, Any]("foo" -> "s", "bar" -> 1)
        val xmlInput = charSequenceToByteChunk(
          """<record>
            |  <field name="foo"><string>s</string></field>
            |  <field name="bar"><int>1</int></field>
            |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        )
        assertDecodes(recordSchema, expectedValue, xmlInput)
      },
      test("Xml.Arr") {
        val unionSchema: Schema[Either[String, Int]] = Schema.either(
          Schema.Primitive(StandardType.StringType),
          Schema.Primitive(StandardType.IntType)
        )
        val arrSchema: Schema[List[Either[String, Int]]] = Schema.list(unionSchema)
        val expectedValue                                = List(Left("foo"), Right(1))
        val xmlInput = charSequenceToByteChunk(
          """<seq>
            |  <left><string>foo</string></left>
            |  <right><int>1</int></right>
            |</seq>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
        )
        assertDecodes(arrSchema, expectedValue, xmlInput)
      },
      test("Xml.Num Int") {
        val intSchema: Schema[Int] = Schema.Primitive(StandardType.IntType)
        val expectedValue          = 1
        val xmlInput               = charSequenceToByteChunk("<int>1</int>")
        assertDecodes(intSchema, expectedValue, xmlInput)
      },
      test("Xml.Num Long") {
        val longSchema: Schema[Long] = Schema.Primitive(StandardType.LongType)
        val expectedValue            = 1L
        val xmlInput                 = charSequenceToByteChunk("<long>1</long>")
        assertDecodes(longSchema, expectedValue, xmlInput)
      },
      test("Xml.Num Double") {
        val doubleSchema: Schema[Double] = Schema.Primitive(StandardType.DoubleType)
        val expectedValue                = 1.1
        val xmlInput                     = charSequenceToByteChunk("<double>1.1</double>")
        assertDecodes(doubleSchema, expectedValue, xmlInput)
      },
      test("Xml.Str") {
        val stringSchema: Schema[String] = Schema.Primitive(StandardType.StringType)
        val expectedValue                = "foo"
        val xmlInput                     = charSequenceToByteChunk("<string>foo</string>")
        assertDecodes(stringSchema, expectedValue, xmlInput)
      },
      test("Xml.Bool") {
        val boolSchema: Schema[Boolean] = Schema.Primitive(StandardType.BoolType)
        val expectedValue               = true
        val xmlInput                    = charSequenceToByteChunk("<boolean>true</boolean>")
        assertDecodes(boolSchema, expectedValue, xmlInput)
      },
      test("Xml.Null") {
        val optionSchema: Schema[Option[String]] = Schema.Optional(Schema.Primitive(StandardType.StringType))
        val expectedValue: Option[String]        = None
        val xmlInput                             = charSequenceToByteChunk("<null/>")
        assertDecodes(optionSchema, expectedValue, xmlInput)
      }
    )
  )

  //─────────────────────────────
  //Round-Trip Tests
  //─────────────────────────────

  private val encoderDecoderSuite = suite("encoding then decoding")(
    test("unit") {
      assertEncodesThenDecodes(Schema[Unit], ())
    },
    test("primitive") {
      check(SchemaGen.anyPrimitiveAndValue) {
        case (schema, value) =>
          assertEncodesThenDecodes(schema, value)
      }
    },
    suite("either")(
      test("of primitives") {
        check(SchemaGen.anyEitherAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      test("of tuples - xml") {
        check(
          for {
            leftPair  <- SchemaGen.anyTupleAndValue
            rightPair <- SchemaGen.anyTupleAndValue
          } yield (
            Schema.Either(
              leftPair._1.asInstanceOf[Schema[(Any, Any)]],
              rightPair._1.asInstanceOf[Schema[(Any, Any)]]
            ),
            Right(rightPair._2)
          )
        ) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      test("of enums") {
        check(
          for {
            (left, value) <- SchemaGen.anyEnumerationAndValue
            (right, _)    <- SchemaGen.anyEnumerationAndValue
          } yield (
            Schema.Either(left, right),
            Left(value)
          )
        ) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      test("of map") {
        check(
          for {
            left  <- SchemaGen.anyMapAndValue
            right <- SchemaGen.anyMapAndValue
          } yield (
            Schema.Either(
              left._1.asInstanceOf[Schema[Map[Any, Any]]],
              right._1.asInstanceOf[Schema[Map[Any, Any]]]
            ),
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
            Schema.Either(
              left._1.asInstanceOf[Schema[Set[Any]]],
              right._1.asInstanceOf[Schema[Set[Any]]]
            ),
            // Expect the left branch
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
      test("of tuples - xml (using anyEitherAndValue)") {
        check(SchemaGen.anyEitherAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
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
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
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
      test("of Map - duplicate") {
        check(SchemaGen.anyMapAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      test("of Set - duplicate") {
        check(SchemaGen.anySetAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.Optional(schema), Some(value)) &>
              assertEncodesThenDecodes(Schema.Optional(schema), None)
        }
      },
      test("tuple") {
        check(SchemaGen.anyTupleAndValue) {
          case (schema, value) => assertEncodesThenDecodes(schema, value)
        }
      }
    ),
    suite("sequence")(
      test("of primitives") {
        check(SchemaGen.anySequenceAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      test("of records") {
        check(SchemaGen.anyCaseClassAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(Schema.chunk(schema), Chunk.fill(3)(value))
        }
      },
      test("of java.time.ZoneOffset") {
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
      test("object") {
        assertEncodesThenDecodes(schemaObject, Singleton)
      },
      test("all optional fields empty") {
        assertEncodesThenDecodes(AllOptionalFields.schema, AllOptionalFields(None, None, None))
      },
      test("recursive data structure") {
        assertDecodes(
          Schema[Recursive],
          Recursive(Some(Recursive(None))),
          charSequenceToByteChunk(
            """<field name="n">
              |    <record>
              |      <field name="n"><record/></field>
              |    </record>
              |  </field>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      }
    ),
    suite("record")(
      test("any") {
        check(SchemaGen.anyRecordAndValue()) {
          case (schema, value) =>
            def normalizeAny(a: Any): Any = a match {
              case s: String    => s.trim
              case m: Map[_, _] => m.map { case (k, v) => (normalizeAny(k), normalizeAny(v)) }
              case seq: Seq[_]  => seq.map(normalizeAny)
              case other        => other
            }

            assertEncodesThenDecodesWithDifferentSchemas(
              schema,
              schema,
              value,
              (a: Any, b: Any) => normalizeAny(a) == normalizeAny(b),
              print = false
            )
        }
      },
      test("of primitives") {
        check(SchemaGen.anyRecordAndValue()) {
          case (schema, value) =>
            def normalizeAny(a: Any): Any = a match {
              case s: String    => s.trim
              case m: Map[_, _] => m.map { case (k, v) => (normalizeAny(k), normalizeAny(v)) }
              case seq: Seq[_]  => seq.map(normalizeAny)
              case other        => other
            }

            assertEncodesThenDecodesWithDifferentSchemas(
              schema,
              schema,
              value,
              (a: Any, b: Any) => normalizeAny(a) == normalizeAny(b),
              print = false
            )
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
        def normalizeAny(a: Any): Any = a match {
          case s: String    => s.trim
          case m: Map[_, _] => m.map { case (k, v) => (normalizeAny(k), normalizeAny(v)) }
          case seq: Seq[_]  => seq.map(normalizeAny)
          case other        => other
        }

        val value = ListMap[String, Any](
          "l1" -> "s",
          "l2" -> ListMap[String, Any]("foo" -> "s", "bar" -> 1)
        )

        assertEncodesThenDecodesWithDifferentSchemas(
          nestedRecordSchema,
          nestedRecordSchema,
          value,
          (a: Any, b: Any) => normalizeAny(a) == normalizeAny(b),
          print = false
        )
      }
    ),
    suite("any schema")(
      test("leaf") {
        check(SchemaGen.anyLeafAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      },
      suite("dynamic")(
        test("dynamic int") {
          check(DynamicValueGen.anyPrimitiveDynamicValue(StandardType.IntType)) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic instant") {
          check(DynamicValueGen.anyPrimitiveDynamicValue(StandardType.InstantType)) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic zoned date time") {
          check(DynamicValueGen.anyPrimitiveDynamicValue(StandardType.ZonedDateTimeType)) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic duration") {
          check(DynamicValueGen.anyPrimitiveDynamicValue(StandardType.DurationType)) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic string") {
          check(DynamicValueGen.anyPrimitiveDynamicValue(StandardType.StringType)) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic unit") {
          check(DynamicValueGen.anyPrimitiveDynamicValue(StandardType.UnitType)) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        },
        test("dynamic tuple") {
          check(DynamicValueGen.anyDynamicTupleValue(Schema[String], Schema[Int])) { dynamicValue =>
            assertEncodesThenDecodes(Schema.dynamicValue, dynamicValue)
          }
        }
      )
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
        ) &>
          assertEncodesThenDecodes(
            Schema[Enumeration],
            Enumeration(IntValue(-1))
          ) &>
          assertEncodesThenDecodes(
            Schema[Enumeration],
            Enumeration(BooleanValue(false))
          )
      },
      test("ADT with annotation") {
        assertEncodesThenDecodes(
          Schema[Enumeration2],
          Enumeration2(StringValue2("foo"))
        ) &>
          assertEncodesThenDecodes(
            Schema[Enumeration2],
            Enumeration2(`StringValue2-Backticked`("foo", "bar"))
          ) &>
          assertEncodesThenDecodes(
            Schema[Enumeration2],
            Enumeration2(IntValue2(-1))
          ) &>
          assertEncodesThenDecodes(
            Schema[Enumeration2],
            Enumeration2(BooleanValue2(false))
          )
      },
      test("ADT with noDiscriminator") {
        assertEncodesThenDecodes(
          Schema[Enumeration3],
          Enumeration3(StringValue3("foo"))
        ) &>
          assertEncodesThenDecodes(
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
      test("decode discriminated case objects in array") {
        assertDecodes(
          Schema[List[Command]],
          Command.Cash :: Nil,
          charSequenceToByteChunk(
            """<seq>
          <record type="Cash">
            <record><string>Cash</string></record>
            <dummy></dummy>
          </record>
        </seq>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        )
      },
      test("no discriminator field") {
        assertDecodesToError(
          Schema[Command],
          (
            """<record>
              |  <field name="b"><int>123</int></field>
              |</record>""".stripMargin
              .replaceAll("\n", "")
              .replaceAll(
                ">\\s+<",
                "><"
              )
            ),
          List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Missing discriminator 'type' on enum element"))
        ) &>
          assertDecodesToError(
            Schema[Command],
            ("""<record/>"""),
            List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Missing discriminator 'type' on enum element"))
          )
      },
      test("illegal case") {
        assertDecodesToError(
          Schema[Command],
          (
            """<record>
              |  <field name="type"><string>Run</string></field>
              |</record>""".stripMargin
              .replaceAll("\n", "")
              .replaceAll(
                ">\\s+<",
                "><"
              )
            ),
          (XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: Run") :: Nil)
        )
      },
      test("decode discriminated case objects with extra fields") {
        assertDecodes(
          Schema[Command],
          Command.Cash,
          charSequenceToByteChunk(
            """<record type="Cash">
              |  <payload></payload>
              |  <field name="extraField"><int>1</int></field>
              |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          )
        ) &>
          assertDecodes(
            Schema[Command],
            Command.Cash,
            charSequenceToByteChunk(
              """<record type="Cash">
                |  <payload></payload>
                |  <field name="extraField"><int>1</int></field>
                |</record>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
            )
          )
      },
      suite("of case objects with up to 64 cases")(
        test("without annotation") {
          assertEncodesThenDecodes(Schema[Color], Color.Red)
        },
        test("with caseName") {
          assertEncodesThenDecodes(Schema[Color], Color.Grass) &>
            assertEncodesXml(Schema[Color], Color.Grass, "<color>Green</color>") &>
            assertDecodes(Schema[Color], Color.Grass, charSequenceToByteChunk("<color>Green</color>")) &>
            assertDecodesToError(
              Schema[Color],
              ("<color>Grass</color>"),
              List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: Grass"))
            )
        },
        test("with caseAliases") {
          assertEncodesThenDecodes(Schema[Color], Color.Blue) &>
            assertEncodesXml(Schema[Color], Color.Blue, "<color>Blue</color>") &>
            assertDecodes(Schema[Color], Color.Blue, charSequenceToByteChunk("<color>Blue</color>")) &>
            assertDecodes(Schema[Color], Color.Blue, charSequenceToByteChunk("<color>LightBlue</color>")) &>
            assertDecodes(Schema[Color], Color.Blue, charSequenceToByteChunk("<color>DarkBlue</color>"))
        },
        test("invalid case") {
          assertDecodesToError(
            Schema[Color],
            ("<color>not a color</color>"),
            List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: not a color"))
          )
        }
      ),
      suite("of case objects with more than 64 cases")(
        test("without annotation") {
          assertEncodesThenDecodes(Schema[BigEnum], BigEnum.Case69)
        },
        test("with caseName") {
          assertEncodesThenDecodes(Schema[BigEnum], BigEnum.Case00) &>
            assertEncodesXml(Schema[BigEnum], BigEnum.Case00, "<bigEnum>Case_00</bigEnum>") &>
            assertDecodes(Schema[BigEnum], BigEnum.Case00, charSequenceToByteChunk("<bigEnum>Case_00</bigEnum>")) &>
            assertDecodesToError(
              Schema[BigEnum],
              ("<bigEnum>Case00</bigEnum>"),
              List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: Case00"))
            )
        },
        test("with caseAliases") {
          assertEncodesThenDecodes(Schema[BigEnum], BigEnum.Case00) &>
            assertDecodes(Schema[BigEnum], BigEnum.Case00, charSequenceToByteChunk("<bigEnum>Case-00</bigEnum>"))
        },
        test("invalid case") {
          assertDecodesToError(
            Schema[BigEnum],
            ("<bigEnum>CaseXX</bigEnum>"),
            List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: CaseXX"))
          )
        }
      ),
      suite("of case classes and case objects with more than 64 cases")(
        test("without annotation") {
          assertEncodesThenDecodes(Schema[BigEnum2], BigEnum2.Case69)
        },
        test("with caseName") {
          assertEncodesXml(
            Schema[BigEnum2],
            BigEnum2.Case00(123.toByte),
            """<bigEnum>
              |  <enum type="Case_00">
              |    <record>
              |      <b>
              |        <byte>123</byte>
              |      </b>
              |    </record>
              |  </enum>
              |</bigEnum>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
          ) &>
            assertDecodes(
              Schema[BigEnum2],
              BigEnum2.Case00(123.toByte),
              charSequenceToByteChunk(
                """<bigEnum>
                  |  <enum type="Case_00">
                  |    <record>
                  |      <b>
                  |        <byte>123</byte>
                  |      </b>
                  |    </record>
                  |  </enum>
                  |</bigEnum>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
              )
            ) &>
            assertDecodesToError(
              Schema[BigEnum2],
              (
                """<bigEnum>
                  |  <Case00>
                  |    <record/>
                  |  </Case00>
                  |</bigEnum>""".stripMargin
                  .replaceAll("\n", "")
                  .replaceAll(
                    ">\\s+<",
                    "><"
                  )
                ),
              List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: "))
            )
        },
        test("with caseAliases") {
          assertEncodesThenDecodes(Schema[BigEnum2], BigEnum2.Case00(123.toByte)) &>
            assertDecodes(
              Schema[BigEnum2],
              BigEnum2.Case00(123.toByte),
              charSequenceToByteChunk(
                """<bigEnum>
                  |  <enum type="Case-00">
                  |    <record>
                  |      <b>
                  |        <byte>123</byte>
                  |      </b>
                  |    </record>
                  |  </enum>
                  |</bigEnum>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
              )
            )
        },
        test("no discriminator key") {
          assertDecodesToError(
            Schema[BigEnum2],
            ("<bigEnum/>"),
            List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Missing discriminator 'type' on enum element"))
          )
        },
        test("invalid case") {
          assertDecodesToError(
            Schema[BigEnum2],
            (
              """<bigEnum>
                |  <CaseXX>
                |    <record/>
                |  </CaseXX>
                |</bigEnum>""".stripMargin
                .replaceAll("\n", "")
                .replaceAll(
                  ">\\s+<",
                  "><"
                )
              ),
            List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: "))
          )
        }
      ),
      suite("of case classes and case objects with more than 64 cases and discriminator field")(
        test("without annotation") {
          assertEncodesThenDecodes(Schema[BigEnum3], BigEnum3.Case69)
        },
        test("with caseName") {
          assertEncodesThenDecodes(Schema[BigEnum3], BigEnum3.Case00(123.toByte)) &>
            assertEncodesXml(
              Schema[BigEnum3],
              BigEnum3.Case00(123.toByte),
              """<bigEnum>
                |  <enum type="Case_00">
                |    <record type="Case_00">
                |      <b>
                |        <byte>123</byte>
                |      </b>
                |    </record>
                |  </enum>
                |</bigEnum>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
            ) &>
            assertDecodes(
              Schema[BigEnum3],
              BigEnum3.Case00(123.toByte),
              charSequenceToByteChunk(
                """<bigEnum>
                  |  <enum type="Case_00">
                  |    <record type="Case_00">
                  |      <b>
                  |        <byte>123</byte>
                  |      </b>
                  |    </record>
                  |  </enum>
                  |</bigEnum>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
              )
            ) &>
            assertDecodesToError(
              Schema[BigEnum3],
              (
                """<bigEnum>
                  |  <record>
                  |    <field name="type"><string>Case00</string></field>
                  |  </record>
                  |</bigEnum>""".stripMargin
                  .replaceAll("\n", "")
                  .replaceAll(
                    ">\\s+<",
                    "><"
                  )
                ),
              List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: Case00"))
            )
        },
        test("with caseAliases") {
          assertEncodesThenDecodes(Schema[BigEnum3], BigEnum3.Case00(123.toByte)) &>
            assertDecodes(
              Schema[BigEnum3],
              BigEnum3.Case00(123.toByte),
              charSequenceToByteChunk(
                """<bigEnum>
                  |  <enum type="Case_00">
                  |    <record type="Case_00">
                  |      <b>
                  |        <byte>123</byte>
                  |      </b>
                  |    </record>
                  |  </enum>
                  |</bigEnum>""".stripMargin.replaceAll("\n", "").replaceAll(">\\s+<", "><")
              )
            )
        },
        test("no discriminator field") {
          assertDecodesToError(
            Schema[BigEnum3],
            (
              """<bigEnum>
                |  <record>
                |    <field name="b"><int>123</int></field>
                |  </record>
                |</bigEnum>""".stripMargin
                .replaceAll("\n", "")
                .replaceAll(
                  ">\\s+<",
                  "><"
                )
              ),
            List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: "))
          ) &>
            assertDecodesToError(
              Schema[BigEnum3],
              (
                """<bigEnum>
                  |  <record/>
                  |</bigEnum>""".stripMargin
                  .replaceAll("\n", "")
                  .replaceAll(
                    ">\\s+<",
                    "><"
                  )
                ),
              List(XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: "))
            )
        },
        test("invalid case") {
          assertDecodesToError(
            Schema[BigEnum3],
            (
              """<bigEnum>
                |  <record>
                |    <field name="type"><string>CaseXX</string></field>
                |  </record>
                |</bigEnum>""".stripMargin
                .replaceAll("\n", "")
                .replaceAll(
                  ">\\s+<",
                  "><"
                )
              ),
            (XmlError.Raw("zio.schema.codec.DecodeError$ReadError: Unrecognized enum case: CaseXX") :: Nil)
          )
        }
      )
    ),
    suite("transform")(
      test("any") {
        check(SchemaGen.anyTransformAndValue) {
          case (schema, value) =>
            assertEncodesThenDecodes(schema, value)
        }
      }
    )
  )

  // Helper to assert that encoding a value produces the expected XML string.
  private def assertEncodesXml[A](
    schema: Schema[A],
    value: A,
    expected: String,
    cfg: XmlCodec.Config = XmlCodec.Config.default,
    print: Boolean = false
  ): ZIO[Any, Nothing, TestResult] = {
    // Convert the expected string to a Chunk[Byte] using UTF-8 encoding.
    val expectedChunk: Chunk[Byte] =
      Chunk.fromArray(expected.getBytes(StandardCharsets.UTF_8))

    val stream = ZStream
      .succeed(value)
      .via(XmlCodec.schemaBasedBinaryCodec(cfg)(schema).streamEncoder)
      .runCollect
      .tap { resultChunk =>
        zio.Console
          .printLine(new String(resultChunk.toArray, StandardCharsets.UTF_8))
          .when(print)
          .orElse(ZIO.unit)
      }
    assertZIO(stream)(equalTo(expectedChunk))
  }

  private def assertEncodes[A](
    schema: Schema[A],
    value: A,
    chunk: Chunk[Byte],
    cfg: XmlCodec.Config = XmlCodec.Config.default,
    print: Boolean = false
  ): ZIO[Any, Nothing, TestResult] = {
    val stream = ZStream
      .succeed(value)
      .via(XmlCodec.schemaBasedBinaryCodec(cfg)(schema).streamEncoder)
      .runCollect
      .tap { outChunk =>
        printLine(new String(outChunk.toArray)).when(print).ignore
      }
    assertZIO(stream)(equalTo(chunk))
  }

  private def assertEncodesMany[A](
    schema: Schema[A],
    values: Seq[A],
    chunk: Chunk[Byte],
    cfg: XmlCodec.Config = XmlCodec.Config.default.copy(prettyPrint = false),
    print: Boolean = false
  ) = {
    val stream = ZStream
      .fromIterable(values)
      .via(XmlCodec.schemaBasedBinaryCodec(cfg)(schema).streamEncoder)
      .runCollect
      .tap { outChunk =>
        printLine(new String(outChunk.toArray)).when(print).ignore
      }
    assertZIO(stream)(equalTo(chunk))
  }

  private def assertDecodesMany[A](
    schema: Schema[A],
    values: Chunk[A],
    chunk: Chunk[Byte],
    cfg: XmlCodec.Config = XmlCodec.Config.default.copy(prettyPrint = false)
  ) = {
    val result = ZStream
      .fromChunk(chunk)
      .via(XmlCodec.schemaBasedBinaryCodec[A](cfg)(schema).streamDecoder)
      .runCollect
    assertZIO(result)(equalTo(values))
  }

  private def assertDecodes[A](
    schema: Schema[A],
    value: A,
    chunk: Chunk[Byte],
    cfg: XmlCodec.Config = XmlCodec.Config.default
  ): ZIO[Any, Nothing, TestResult] = {
    val result = ZStream
      .fromChunk(chunk)
      .via(XmlCodec.schemaBasedBinaryCodec(cfg)(schema).streamDecoder)
      .runCollect
      .orDie
    assertZIO(result)(equalTo(Chunk(value)))
  }

  private def assertDecodesToError[A](
    schema: Schema[A],
    xml: CharSequence,
    errors: List[XmlError],
    cfg: XmlCodec.Config = XmlCodec.Config.default
  ) = {
    val stream = ZStream
      .fromChunk(charSequenceToByteChunk(xml))
      .via(XmlCodec.schemaBasedBinaryCodec[A](cfg)(schema).streamDecoder)
      .catchAll(ZStream.succeed[DecodeError](_))
      .runHead
    assertZIO(stream)(isSome(equalTo(ReadError(Cause.empty, XmlError.render(errors)))))
  }

  private def assertEncodesThenDecodesFallback[A, B](
    schema: Schema.Fallback[A, B],
    value: Fallback[A, B],
    print: Boolean = false
  ): ZIO[Any, Nothing, TestResult] =
    ZStream
      .succeed(value)
      .via(XmlCodec.schemaBasedBinaryCodec[zio.schema.Fallback[A, B]](XmlCodec.Config.default)(schema).streamEncoder)
      .runCollect
      .flatMap { encoded =>
        ZStream
          .fromChunk(encoded)
          .via(
            XmlCodec.schemaBasedBinaryCodec[zio.schema.Fallback[A, B]](XmlCodec.Config.default)(schema).streamDecoder
          )
          .runCollect
          .either
          .flatMap { result =>
            val expected = if (schema.fullDecode) value else value.simplify
            result.map(_.headOption.getOrElse(expected)) match {
              case Right(obtained) =>
                if (expected == obtained)
                  ZIO.succeed(assertTrue(expected == obtained))
                else
                  // Recursively re-run with the obtained value for further diagnosis.
                  assertEncodesThenDecodesFallback(schema, obtained, print)
              case Left(_) =>
                ZIO.succeed(assertTrue(false))
            }
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
    cfg: XmlCodec.Config = XmlCodec.Config.default
  ) =
    ZStream
      .succeed(value)
      .via(XmlCodec.schemaBasedBinaryCodec[A1](cfg)(encodingSchema).streamEncoder)
      .runCollect
      .flatMap { encoded =>
        ZStream
          .fromChunk(encoded)
          .via(XmlCodec.schemaBasedBinaryCodec[A2](cfg)(decodingSchema).streamDecoder)
          .runCollect
          .either
      }
      .map {
        case Right(decodedChunk: Chunk[A2]) =>
          assertTrue(decodedChunk.size == 1)
          val decodedValue = decodedChunk.head
          assertTrue(compare(value, decodedValue))
        case Left(_) =>
          assertTrue(false)
      }

  val recordSchema: Schema[ListMap[String, _]] = Schema.record(
    TypeId.Structural,
    Schema.Field(
      "foo",
      Schema.Primitive(StandardType.StringType),
      annotations0 = Chunk(fieldDefaultValue(null)),
      get0 = (p: ListMap[String, _]) => p("foo").asInstanceOf[String],
      set0 = (p: ListMap[String, _], v: String) => p.updated("foo", v)
    ),
    Schema.Field(
      "bar",
      Schema.Primitive(StandardType.IntType),
      annotations0 = Chunk(fieldDefaultValue(1)),
      get0 = (p: ListMap[String, _]) => p("bar").asInstanceOf[Int],
      set0 = (p: ListMap[String, _], v: Int) => p.updated("bar", v)
    )
  )

  case class SearchRequestWithTransientField(
    query: String,
    pageNumber: Int,
    resultPerPage: Int,
    @transientField nextPage: String = "transient"
  )

  val searchRequestWithTransientFieldSchema: Schema[SearchRequestWithTransientField] =
    DeriveSchema.gen[SearchRequestWithTransientField]

  // Some dummy types and schemas for testing.
  case class BacktickedFieldName(`x-api-key`: String)

  object BacktickedFieldName {
    implicit val schema: Schema[BacktickedFieldName] = DeriveSchema.gen[BacktickedFieldName]
  }

  final case class WithOptField(@optionalField list: List[String], @optionalField map: Map[String, Int])

  object WithOptField {
    implicit lazy val schema: Schema[WithOptField] = DeriveSchema.gen[WithOptField]
  }

  final case class ListAndMapAndOption(list: List[String], map: Map[String, Int], option: Option[String])

  object ListAndMapAndOption {
    implicit lazy val schema: Schema[ListAndMapAndOption] = DeriveSchema.gen[ListAndMapAndOption]
  }

  case class Key(name: String, index: Int)

  object Key {
    implicit lazy val schema: Schema[Key] = DeriveSchema.gen[Key]
  }

  case class Value(first: Int, second: Boolean)

  object Value {
    implicit lazy val schema: Schema[Value] = DeriveSchema.gen[Value]
  }

  final case class KeyWrapper(key: String)

  object KeyWrapper {
    implicit lazy val schema: Schema[KeyWrapper] =
      Schema[String].transform(KeyWrapper.apply, _.key)

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

  case class OptionalSearchRequest(
    query: String,
    pageNumber: Int,
    resultPerPage: Int,
    @optionalField nextPage: String = ""
  )

  object OptionalSearchRequest {
    implicit val schema: Schema[OptionalSearchRequest] = DeriveSchema.gen[OptionalSearchRequest]
  }

  @rejectExtraFields final case class PersonWithRejectExtraFields(name: String, age: Int)

  object PersonWithRejectExtraFields {

    val schema: Schema[PersonWithRejectExtraFields] = DeriveSchema.gen[PersonWithRejectExtraFields]
  }

  case class FieldDefaultValueSearchRequest(
    query: String,
    pageNumber: Int,
    resultPerPage: Int,
    @fieldDefaultValue("test") nextPage: String
  )

  object FieldDefaultValueSearchRequest {
    implicit val schema: Schema[FieldDefaultValueSearchRequest] =
      DeriveSchema.gen[FieldDefaultValueSearchRequest]
  }

  val fieldDefaultValueSearchRequestSchema: Schema[FieldDefaultValueSearchRequest] =
    DeriveSchema.gen[FieldDefaultValueSearchRequest]

  case class Order(
    @fieldNameAliases("order_id", "id") @fieldName("orderId") `order-id`: Int,
    value: BigDecimal,
    description: String
  )

  object Order {
    implicit lazy val schema: Schema[Order] = DeriveSchema.gen[Order]
  }

  final case class WithComplexOptionField(order: Option[Order])

  object WithComplexOptionField {
    implicit lazy val schema: Schema[WithComplexOptionField] = DeriveSchema.gen[WithComplexOptionField]
  }
  final case class AllOptionalFields(
    name: Option[String],
    mode: Option[Int],
    active: Option[Boolean]
  )

  object AllOptionalFields {
    implicit lazy val schema: Schema[AllOptionalFields] = DeriveSchema.gen[AllOptionalFields]
  }

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

  @annotation.discriminatorName("type")
  sealed trait Command

  object Command {
    case class Buy(credits: Int) extends Command

    case object Cash extends Command

    implicit val schema: Schema[Command] = DeriveSchema.gen[Command]
  }

  // Define the case class.
  final case class SearchRequest(
    query: String,
    pageNumber: Int,
    resultPerPage: Int,
    nextPage: Option[String]
  )

  object SearchRequest {
    implicit lazy val schema: Schema[SearchRequest] = DeriveSchema.gen[SearchRequest]
  }

  private val searchRequestGen: Gen[Sized, SearchRequest] =
    for {
      query         <- Gen.string
      pageNumber    <- Gen.int(Int.MinValue, Int.MaxValue)
      resultPerPage <- Gen.int(Int.MinValue, Int.MaxValue)
      nextPage      <- Gen.option(Gen.asciiString)
    } yield SearchRequest(query, pageNumber, resultPerPage, nextPage)

  case class Recursive(n: Option[Recursive] = None)

  object Recursive {
    implicit val schema: Schema[Recursive] = DeriveSchema.gen
  }

  case object Singleton

  implicit val schemaObject: Schema[Singleton.type] = DeriveSchema.gen[Singleton.type]

  val recordWithOptionSchema: Schema[ListMap[String, _]] = Schema.record(
    TypeId.Structural,
    Schema.Field(
      "foo",
      Schema.Primitive(StandardType.StringType).optional,
      get0 = (p: ListMap[String, _]) => p("foo").asInstanceOf[Option[String]],
      set0 = (p: ListMap[String, _], v: Option[String]) => p.updated("foo", v)
    ),
    Schema.Field(
      "bar",
      Schema.Primitive(StandardType.IntType).optional,
      get0 = (p: ListMap[String, _]) => p("bar").asInstanceOf[Option[Int]],
      set0 = (p: ListMap[String, _], v: Option[Int]) => p.updated("bar", v)
    )
  )
  final case class WithOptionFields(a: Option[String], b: Option[Int])

  object WithOptionFields {
    implicit lazy val schema: Schema[WithOptionFields] = DeriveSchema.gen[WithOptionFields]
  }

  val searchRequestSchema: Schema[SearchRequest] = DeriveSchema.gen[SearchRequest]

  val optionalSearchRequestSchema: Schema[OptionalSearchRequest] = DeriveSchema.gen[OptionalSearchRequest]

  for {
    query      <- Gen.string
    pageNumber <- Gen.int(Int.MinValue, Int.MaxValue)
    results    <- Gen.int(Int.MinValue, Int.MaxValue)
    nextPage   <- Gen.asciiString
  } yield OptionalSearchRequest(query, pageNumber, results, nextPage)

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

  val enumSchema: Schema[Any] = Schema.enumeration[Any, CaseSet.Aux[Any]](
    TypeId.Structural,
    caseOf[String, Any]("string")(_.asInstanceOf[String])(_.asInstanceOf[Any])(_.isInstanceOf[String]) ++
      caseOf[Int, Any]("int")(_.asInstanceOf[Int])(_.asInstanceOf[Any])(_.isInstanceOf[Int]) ++
      caseOf[Boolean, Any]("boolean")(_.asInstanceOf[Boolean])(_.asInstanceOf[Any])(_.isInstanceOf[Boolean])
  )

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
  @noDiscriminator sealed trait Prompt

  object Prompt {
    final case class Single(value: String) extends Prompt

    final case class Multiple(value: List[String]) extends Prompt

    implicit lazy val schema: Schema[Prompt] = DeriveSchema.gen[Prompt]
  }

  sealed trait PaymentMethod

  object PaymentMethod {

    @caseNameAliases("credit_card", "cc")
    final case class CreditCard(
      number: String,
      expirationMonth: Int,
      expirationYear: Int
    ) extends PaymentMethod

    @caseName("wire_transfer")
    final case class WireTransfer(accountNumber: String, bankCode: String) extends PaymentMethod

    @transientCase
    final case class PayPal(email: String) extends PaymentMethod

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

  @discriminatorName("type")
  sealed trait OneOf4

  object OneOf4 {
    implicit lazy val schema: Schema[OneOf4] = DeriveSchema.gen
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

  @rejectExtraFields case class RecordExampleWithDiscriminator(
    @fieldName("$f1") f1: Option[String],
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

  case class RecordExample(
    @fieldName("f1") f1: String,
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
    @fieldName("f23") f23: Option[String] = None
  )

  case class RecordExampleWithOptField(
    @fieldName("f1") f1: Option[String],
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
    @fieldName("f23") f23: Option[String] = None
  )

  object RecordExample {
    implicit lazy val schema: Schema[RecordExample] = DeriveSchema.gen[RecordExample]
  }

  object RecordExampleWithOptField {
    implicit lazy val schema: Schema[RecordExampleWithOptField] =
      DeriveSchema.gen[RecordExampleWithOptField]
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
