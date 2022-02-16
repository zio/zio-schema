package zio.schema

import zio.test._

object FieldMatchingSpec extends DefaultRunnableSpec {
  final case class Person(firstName: String, age: Int)
  private val personSchema: Schema.Record[Person] =
    DeriveSchema.gen[Person].asInstanceOf[Schema.Record[Person]]

  def spec =
    suite("FieldMatchingSpec")(
      suite(".bestFieldMatch")(
        suite("finds perfect matches")(
          test("returns first index of perfect match") {
            val result = Schema.bestFieldMatch(personSchema, "firstName", StandardType[String])
            assertTrue(result == Some(0))
          },
          test("returns first index of perfect match") {
            val result = Schema.bestFieldMatch(personSchema, "age", StandardType[Int])
            assertTrue(result == Some(1))
          },
          test("returns None if there is no match") {
            val result = Schema.bestFieldMatch(personSchema, "missing", StandardType[Long])
            assertTrue(result == None)
          }
        ),
        suite("finds case insensitive matches")(
          test("returns first index of case-insensitive match") {
            val result = Schema.bestFieldMatch(personSchema, "FIRSTNAME", StandardType[String])
            assertTrue(result == Some(0))
          },
          test("returns first index of case-insensitive match") {
            val result = Schema.bestFieldMatch(personSchema, "aGe", StandardType[Int])
            assertTrue(result == Some(1))
          }
        ),
        suite("finds format insensitive matches")(
          test("returns first index of format-insensitive match") {
            val result = Schema.bestFieldMatch(personSchema, "first_name", StandardType[String])
            assertTrue(result == Some(0))
          }
        )
      ),
      suite(".bestFieldMatches")(
        test("succeeds for all perfect matches") {
          val result = Schema
            .bestFieldMatches(
              personSchema,
              Set(
                "firstName" -> StandardType[String],
                "age"       -> StandardType[Int]
              )
            )
            .toOption
            .get

          assertTrue(
            result("firstName") == 0,
            result("age") == 1
          )

        },
        test("fails for any ambiguous matches") {
          val result = Schema
            .bestFieldMatches(
              personSchema,
              Set(
                "FIRSTNAME" -> StandardType[String],
                "firstName" -> StandardType[String],
                "aGe"       -> StandardType[Int]
              )
            )

          assertTrue(
            result == Left(
              "Field firstName is ambiguous"
            )
          )
        },
        test("fails for any missing matches") {
          val result = Schema
            .bestFieldMatches(
              personSchema,
              Set(
                "firstName" -> StandardType[String],
                "age"       -> StandardType[Int],
                "missing"   -> StandardType[Long]
              )
            )

          assertTrue(
            result == Left(
              "No field found for missing"
            )
          )
        }
      )
    )
}
