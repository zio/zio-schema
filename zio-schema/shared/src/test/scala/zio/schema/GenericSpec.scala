package zio.schema

import zio._
import zio.test._
import zio.test.Assertion._

object GenericSpec extends DefaultRunnableSpec {

  def spec =
    suite("GenericSpec")(
      testM("round-trips Ints") {
        val schema: Schema[Int] = Schema.primitive[Int]
        genericLaw(Gen.anyInt, schema)
      },
      testM("round-trips Doubles") {
        val schema: Schema[Double] = Schema.primitive[Double]
        genericLaw(Gen.anyDouble, schema)
      },
      testM("round-trips String") {
        val schema: Schema[String] = Schema.primitive[String]
        genericLaw(Gen.anyString, schema)
      },
      testM("round-trips Boolean") {
        val schema: Schema[Boolean] = Schema.primitive[Boolean]
        genericLaw(Gen.boolean, schema)
      },
      testM("round-trips Floats") {
        val schema: Schema[Float] = Schema.primitive[Float]
        genericLaw(Gen.anyFloat, schema)
      },
      testM("round-trips Records") {
        val schema: Schema[Map[String, _]] =
          Schema.record(
            Map("name" -> Schema.primitive(StandardType.StringType), "age" -> Schema.primitive(StandardType.IntType))
          )

        val recordGen = for {
          name <- Gen.anyString
          age  <- Gen.anyInt
        } yield Map[String, Any]("name" -> name, "age" -> age)

        genericLaw(recordGen, schema)
      }
    )

  def genericLaw[R <: TestConfig, A](gen: Gen[R, A], schema: Schema[A]): URIO[R, TestResult] =
    check(gen) { a =>
      assert(schema.fromGeneric(schema.toGeneric(a)))(isRight(equalTo(a)))
    }

}
