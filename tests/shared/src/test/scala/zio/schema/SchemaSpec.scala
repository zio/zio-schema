package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.CaseSet._
import zio.test.Assertion._
import zio.test._

object SchemaSpec extends ZIOSpecDefault {

  def spec: Spec[Environment, Any] = suite("Schema Spec")(
    suite("Should have valid equals")(
      test("primitive") {
        assert(schemaUnit)(equalTo(schemaUnit))
      },
      test("sequence") {
        assert(Schema.chunk(schemaUnit))(equalTo(Schema.chunk(schemaUnit)))
      } @@ TestAspect.scala2Only,
      test("tuple") {
        assert(Schema.Tuple2(schemaUnit, schemaUnit))(equalTo(Schema.Tuple2(schemaUnit, schemaUnit))) &&
        assert(Schema.Tuple2(schemaTransform, schemaTransform))(
          equalTo(Schema.Tuple2(schemaTransform, schemaTransform))
        )
      },
      // TODO: disabled due to the fact that get operation is a different lambda instance
//      test("record") {
//        assert(schemaRecord("key"))(equalTo(schemaRecord("key"))) &&
//        assert(schemaRecord("key1"))(not(equalTo(schemaRecord("key2"))))
//      },
      test("transform") {
        assert(schemaTransform)(equalTo(schemaTransform)) &&
        assert(schemaTransformMethod)(equalTo(schemaTransformMethod))
      } @@ TestAspect.scala2Only,
      test("optional") {
        assert(Schema.Optional(schemaUnit))(equalTo(Schema.Optional(schemaUnit)))
      },
      test("enumeration") {
        assert(schemaEnum("key"))(equalTo(schemaEnum("key"))) &&
        assert(schemaEnum("key1"))(not(equalTo(schemaEnum("key2"))))

      } @@ TestAspect.scala2Only
    ),
    test("Tuple.toRecord should preserve annotations") {
      val left        = Schema.primitive(StandardType.StringType)
      val right       = Schema.primitive(StandardType.StringType)
      val tupleSchema = Schema.Tuple2(left, right, Chunk("some Annotation"))
      val record      = tupleSchema.toRecord
      assert(record.annotations)(hasFirst(equalTo("some Annotation")))
    }
  )

  def schemaUnit: Schema[Unit] = Schema[Unit]
  def schemaInt: Schema[Int]   = Schema[Int]

  def schemaRecord(key: String): Schema[ListMap[String, _]] =
    Schema.record(
      TypeId.Structural,
      Schema.Field(
        key,
        schemaUnit.asInstanceOf[Schema[Any]],
        get0 = (p: ListMap[String, _]) => p(key),
        set0 = (p: ListMap[String, _], v: Any) => p.updated(key, v)
      )
    )

  def schemaEnum(key: String): Schema[Any] =
    Schema.enumeration[Any, CaseSet.Aux[Any]](
      TypeId.Structural,
      caseOf[Unit, Any](key)(_ => ())(_.asInstanceOf[CaseSet.Aux[Any]])(
        _.isInstanceOf[Unit]
      )
    )

  val f: Unit => Either[String, Int] = _ => Right(0)
  val g: Int => Either[String, Unit] = _ => Right(())
  def schemaTransform: Schema[Int]   = schemaUnit.transformOrFail[Int](f, g)

  def tranformF(u: Unit): Either[String, Int] = Some(u).map(_ => 0).toRight("")
  def tranformG(i: Int): Either[String, Unit] = Some(i).map(_ => ()).toRight("")
  def schemaTransformMethod: Schema[Int]      = schemaUnit.transformOrFail(tranformF, tranformG)

}
