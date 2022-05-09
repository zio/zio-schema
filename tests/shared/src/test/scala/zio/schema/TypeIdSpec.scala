package zio.schema

import zio.Chunk
import zio.test.Assertion._
import zio.test.{ DefaultRunnableSpec, ZSpec, _ }

object TypeIdSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] =
    suite("TypeId")(
      test("parse class name in package") {
        val expected = TypeId.Nominal(Chunk.fromIterable("foo" :: "bar" :: Nil), Chunk.empty, "Baz")

        assert(TypeId.parse("foo.bar.Baz"))(equalTo(expected))
      },
      test("parse inner class name in some object") {
        val expected = TypeId.Nominal(Chunk.fromIterable("foo" :: "bar" :: Nil), Chunk("Baz"), "Random")

        assert(TypeId.parse("foo.bar.Baz.Random"))(equalTo(expected))
      },
      test("parse one string") {
        val expected = TypeId.Nominal(Chunk.empty, Chunk.empty, "Foo")

        assert(TypeId.parse("Foo"))(equalTo(expected))
      },
      test("parse complex structure") {
        val expected = TypeId.Nominal(
          Chunk.fromIterable("dev" :: "zio" :: "schema" :: "example" :: "example2" :: Nil),
          Chunk.fromIterable("Domain" :: "PaymentMethod" :: Nil),
          "CreditCard"
        )

        assert(TypeId.parse("dev.zio.schema.example.example2.Domain.PaymentMethod.CreditCard"))(equalTo(expected))
      },
      test("fully qualified name of nominal structure") {
        val expected = TypeId.Nominal(
          Chunk.fromIterable("dev" :: "zio" :: "schema" :: "example" :: "example2" :: Nil),
          Chunk.fromIterable("Domain" :: "PaymentMethod" :: Nil),
          "CreditCard"
        )

        assert(expected.fullyQualified)(equalTo("dev.zio.schema.example.example2.Domain.PaymentMethod.CreditCard"))
      },
      test("name of nominal structure") {
        val expected = TypeId.Nominal(
          Chunk.fromIterable("dev" :: "zio" :: "schema" :: "example" :: "example2" :: Nil),
          Chunk.fromIterable("Domain" :: "PaymentMethod" :: Nil),
          "CreditCard"
        )

        assert(expected.name)(equalTo("CreditCard"))
      }
    )
}
