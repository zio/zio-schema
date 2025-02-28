package zio.schema.codec

import zio.Console._
import zio._
import zio.json.{ DeriveJsonEncoder, JsonEncoder }
import zio.schema._
import zio.schema.annotation._
import zio.test.Assertion._
import zio.test.TestAspect._
import zio.test.*

object DefaultValueSpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment, Any] =
    suite("Custom Spec")(
      customSuite
    ) @@ timeout(90.seconds)

  private val customSuite = suite("custom")(
    suite("default value schema")(
      test("default value at last field") {
        val result = JsonCodec.jsonDecoder(Schema[WithDefaultValue]).decodeJson("""{"orderId": 1}""")
        assertTrue(result.isRight)
      }
    ),
    suite("enum with discrimintator")(
      test("default value at last field") {
        val encoder = JsonCodec.jsonEncoder(Schema[Base])
        val decoder = JsonCodec.jsonDecoder(Schema[Base])
        val value = BaseB("a", Inner(1))
        val json = """{"type":"BaseB","a":"a","b":{"i":1}}"""
        assert(decoder.decodeJson(json))(equalTo(Right(value))) &&
        assert(encoder.encodeJson(value))(equalTo(json))
      }
    ),
    suite("union types")(
      test("union type of standard types") {
        val schema = Schema.chunk(DeriveSchema.gen[Int | String | Boolean])
        val decoder = JsonCodec.jsonDecoder(schema)
        val encoder = JsonCodec.jsonEncoder(schema)
        val json = """["abc",1,true]"""
        val value = Chunk[Int | String | Boolean]("abc", 1, true)
        assert(decoder.decodeJson(json))(equalTo(Right(value))) &&
        assert(encoder.encodeJson(value))(equalTo(json))
      },
      test("union type of enums") {
        val schema = Schema.chunk(Schema[Result])
        val decoder = JsonCodec.jsonDecoder(schema)
        val encoder = JsonCodec.jsonEncoder(schema)
        val json = """[{"res":{"Left":"Err1"}},{"res":{"Left":"Err21"}},{"res":{"Right":{"i":1}}}]"""
        val value = Chunk[Result](Result(Left(ErrorGroup1.Err1)), Result(Left(ErrorGroup2.Err21)), Result(Right(Value(1))))
        assert(decoder.decodeJson(json))(equalTo(Right(value))) &&
        assert(encoder.encodeJson(value))(equalTo(json))
      },
      test("union type of custom types") {
        import UnionValue.given

        val decoder = zio.json.JsonCodec[Map[String, UnionValue]].decoder
        val encoder = zio.json.JsonCodec[Map[String, UnionValue]].encoder
        val json = """{"a":1,"b":"toto","c":true,"d":null}"""
        val value = Map("a" -> 1, "b" -> "toto", "c" -> true, "d" -> null)
        assert(decoder.decodeJson(json))(equalTo(Right(value))) &&
        assert(encoder.encodeJson(value))(equalTo(json))
      }
    )
  )

  case class WithDefaultValue(orderId: Int, description: String = "desc")

  object WithDefaultValue {
    implicit lazy val schema: Schema[WithDefaultValue] = DeriveSchema.gen[WithDefaultValue]
  }

  enum ErrorGroup1:
    case Err1
    case Err2
    case Err3

  enum ErrorGroup2:
    case Err21
    case Err22
    case Err23

  case class Value(i: Int)
  object Value:
    given Schema[Value] = DeriveSchema.gen[Value]

  case class Result(res: Either[ErrorGroup1 | ErrorGroup2, Value])
  object Result:
    given Schema[Result] = DeriveSchema.gen[Result]

  case class Inner(i: Int) derives Schema

  @discriminatorName("type")
  sealed trait Base derives Schema:
    def a: String

  case class BaseA(a: String) extends Base derives Schema

  case class BaseB(a: String, b: Inner) extends Base derives Schema

  given Schema[Null] = Schema.option[Unit].transform[Null]({ _ => null }, { _ => None })

  type UnionValue = Int | Boolean | String | Null

  object UnionValue {
    given Schema[UnionValue] = Schema.enumeration[UnionValue, CaseSet.Aux[UnionValue]](
      TypeId.Structural,
      CaseSet.caseOf[Int, UnionValue]("int")(_.asInstanceOf[Int])(_.asInstanceOf[UnionValue])(_.isInstanceOf[Int]) ++
        CaseSet.caseOf[Boolean, UnionValue]("boolean")(_.asInstanceOf[Boolean])(_.asInstanceOf[UnionValue])(_.isInstanceOf[Boolean]) ++
        CaseSet.caseOf[String, UnionValue]("string")(_.asInstanceOf[String])(_.asInstanceOf[UnionValue])(_.isInstanceOf[String]) ++
        CaseSet.caseOf[Null, UnionValue]("null")(_.asInstanceOf[Null])(_.asInstanceOf[UnionValue])(_ == null),
      annotations = Chunk(zio.schema.annotation.noDiscriminator())
    )

    given zio.json.JsonCodec[UnionValue] = JsonCodec.jsonCodec(Schema[UnionValue])
  }
}
