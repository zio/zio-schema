package zio.schema

import zio.test._

object StandardTypeCoerceSpec extends DefaultRunnableSpec {

  def spec = suite("StandardTypeCoerceSpec")(
    suite("coerce")(
      suite("String")(
        test("-> Int") {
          val coercion = StandardType.StringType.coerce(StandardType.IntType)

          assertTrue(
            coercion.get("-1") == Right(-1),
            coercion.get("1") == Right(1),
            coercion.get("hello") == Left("String hello could not be coerced into an Int"),
            coercion.get("1.5") == Left("String 1.5 could not be coerced into an Int")
          )
        },
        test("-> Long") {
          val coercion = StandardType.StringType.coerce(StandardType.LongType)

          assertTrue(
            coercion.get("-5") == Right(-5L),
            coercion.get("5") == Right(5L),
            coercion.get("999999999999") == Right(999_999_999_999L),
            coercion.get("hello") == Left("String hello could not be coerced into a Long"),
            coercion.get("1.5") == Left("String 1.5 could not be coerced into a Long")
          )
        },
        test("-> Float") {
          val coercion = StandardType.StringType.coerce(StandardType.FloatType)

          assertTrue(
            coercion.get("1.5") == Right(1.5f),
            coercion.get("hello") == Left("String hello could not be coerced into a Float")
          )
        },
        test("-> Double") {
          val coercion = StandardType.StringType.coerce(StandardType.DoubleType)

          assertTrue(
            coercion.get("1.5") == Right(1.5),
            coercion.get("hello") == Left("String hello could not be coerced into a Double")
          )
        }
      )
    )
  )
}
