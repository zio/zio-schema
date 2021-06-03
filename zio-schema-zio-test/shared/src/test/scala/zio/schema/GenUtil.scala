package zio.schema

import zio.URIO
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object GenUtil {

  def checkSchema[A](schema: Schema[A]): URIO[TestConfig with Sized with Random, TestResult] = {
    val gen = DeriveGen.gen(schema)

    check(gen) { a =>
      val result = DynamicValue.fromSchemaAndValue(schema, a)
      assert(result.toTypedValue(schema))(isRight(equalTo(a)))
    }
  }

  def checkGenSchema[A](
    schemaGen: Gen[Random with Sized, Schema[A]]
  ): URIO[TestConfig with Random with Sized, TestResult] = {
    val schemaAndGen =
      for {
        schema <- schemaGen
        result <- DeriveGen.gen(schema)
      } yield schema -> result

    check(schemaAndGen) {
      case (schema, a) =>
        val result = DynamicValue.fromSchemaAndValue(schema, a)
        assert(result.toTypedValue(schema))(isRight(equalTo(a)))
    }
  }
}
