package zio.schema

import zio.URIO
import zio.random.Random
import zio.test.Assertion._
import zio.test._

object GenUtil {
  final case class TestData[A](name: String, schema: Schema[A]) {
    def gen: Gen[Random with Sized, A] = DeriveGen.gen(schema)
  }

  def checkGenValue[A](schema: Schema[A]): URIO[TestConfig with Sized with Random, TestResult] = {
    val gen = DeriveGen.gen(schema)

    check(gen) { a =>
      val result = DynamicValue.fromSchemaAndValue(schema, a)
      assert(result.toTypedValue(schema))(isRight(equalTo(a)))
    }
  }
  // def testGen[A](data: TestData[A]): ZSpec[TestConfig with Random with Sized, Nothing] =
  //   testGen(data.name)(data.schema)
}
