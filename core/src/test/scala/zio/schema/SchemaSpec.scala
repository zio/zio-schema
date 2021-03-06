package zio.schema

import zio.test.Assertion._
import zio.test.{ ZSpec, _ }

object SchemaSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("Schema Spec")(
    suite("Should have valid equals")(
      test("primitive") {
        assert(schemaUnit)(equalTo(schemaUnit))
      },
      test("sequence") {
        assert(Schema.Sequence(schemaUnit))(equalTo(Schema.Sequence(schemaUnit)))
      },
      test("tuple") {
        assert(Schema.Tuple(schemaUnit, schemaUnit))(equalTo(Schema.Tuple(schemaUnit, schemaUnit)))
        assert(Schema.Tuple(schemaTransform, schemaTransform))(equalTo(Schema.Tuple(schemaTransform, schemaTransform)))
      },
      test("record") {
        assert(schemaRecord("key"))(equalTo(schemaRecord("key")))
        assert(schemaRecord("key1"))(not(equalTo(schemaRecord("key2"))))
      },
      test("transform") {
        assert(schemaTransform)(equalTo(schemaTransform))
        assert(schemaTransformMethod)(equalTo(schemaTransformMethod))
      },
      test("optional") {
        assert(Schema.Optional(schemaUnit))(equalTo(Schema.Optional(schemaUnit)))
      },
      test("enumeration") {
        assert(schemaEnum("key"))(equalTo(schemaEnum("key")))
        assert(schemaEnum("key1"))(not(equalTo(schemaEnum("key2"))))

      }
    )
  )

  def schemaUnit: Schema.Primitive[Unit] = Schema.Primitive(StandardType.UnitType)
  def schemaInt: Schema.Primitive[Int]   = Schema.Primitive(StandardType.IntType)

  def schemaRecord(key: String): Schema.Record    = Schema.Record(Map(key      -> schemaUnit))
  def schemaEnum(key: String): Schema.Enumeration = Schema.Enumeration(Map(key -> schemaUnit))

  val f: Unit => Either[String, Int] = _ => Right(0)
  val g: Int => Either[String, Unit] = _ => Right(())
  def schemaTransform: Schema[Int]   = schemaUnit.transformOrFail[Int](f, g)

  def tranformF(u: Unit): Either[String, Int] = Some(u).map(_ => 0).toRight("")
  def tranformG(i: Int): Either[String, Unit] = Some(i).map(_ => ()).toRight("")
  def schemaTransformMethod: Schema[Int]      = schemaUnit.transformOrFail(tranformF, tranformG)

}
