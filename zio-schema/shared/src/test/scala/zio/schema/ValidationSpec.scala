package zio.schema.validation
import zio.test._

object ValidationSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("ValidationSpec")(
    test("Greater than") {
      val validation = Validation.greaterThan(4)

      assertTrue(validation.validate(4).isLeft) &&
      assertTrue(validation.validate(5).isRight) &&
      assertTrue(validation.validate(6).isRight) &&
      assertTrue(validation.validate(3).isLeft)
    },
    test("Less than") {
      val validation = Validation.lessThan(4)

      assertTrue(validation.validate(3).isRight) &&
      assertTrue(validation.validate(2).isRight) &&
      assertTrue(validation.validate(5).isLeft) &&
      assertTrue(validation.validate(4).isLeft)
    },
    test("Equal to") {
      val validation = Validation.equalTo(3)

      assertTrue(validation.validate(3).isRight) &&
      assertTrue(validation.validate(5).isLeft) &&
      assertTrue(validation.validate(2).isLeft)
    },
    test("MinLength") {
      val validation = Validation.minLength(4)

      assertTrue(validation.validate("hello").isRight) &&
      assertTrue(validation.validate("Todd").isRight) &&
      assertTrue(validation.validate("how").isLeft)
      assertTrue(validation.validate("hi").isLeft)
    },
    test("MaxLength") {
      val validation = Validation.maxLength(4)

      assertTrue(validation.validate("Todd").isRight) &&
      assertTrue(validation.validate("how").isRight) &&
      assertTrue(validation.validate("hello").isLeft) &&
      assertTrue(validation.validate("Automobile").isLeft)
    },
    test("Regex digit or letter Validation") {
      val validation = Validation.regex(Regex.digitOrLetter)

      assertTrue(validation.validate("a").isRight) &&
      assertTrue(validation.validate("1").isRight) &&
      assertTrue(validation.validate("12").isLeft) &&
      assertTrue(validation.validate("*").isLeft) &&
      assertTrue(validation.validate("ab").isLeft) &&
      assertTrue(validation.validate("").isLeft) &&
      assertTrue(validation.validate("&").isLeft)
    },
    test("Regex identifier Validation") {
      val validation = Validation.identifier

      assertTrue(validation.validate("_").isRight) &&
      assertTrue(validation.validate("a").isRight) &&
      assertTrue(validation.validate("ab").isRight) &&
      assertTrue(validation.validate("").isLeft) &&
      assertTrue(validation.validate("*").isLeft)
    },
    test("Regex email Validation") {
      val validation = Validation.email

      assertTrue(validation.validate("bob101@gmail.com").isRight) &&
      assertTrue(validation.validate("bob_and_alice@gmail.com").isRight) &&
      assertTrue(validation.validate("bob101*@gmail.com").isLeft) &&
      assertTrue(validation.validate("_").isLeft) &&
      assertTrue(validation.validate("@").isLeft) &&
      assertTrue(validation.validate("@.").isLeft) &&
      assertTrue(validation.validate("b@.com").isLeft) &&
      assertTrue(validation.validate("").isLeft) &&
      assertTrue(validation.validate("1@.com").isLeft) &&
      assertTrue(validation.validate("1@a.com").isLeft) &&
      assertTrue(validation.validate("1@a.b.com").isLeft) &&
      assertTrue(validation.validate("a@a..b.com").isLeft)
    },
    test("Regex uuid Validation") {
      val validation = Validation.uuidV4

      assertTrue(validation.validate("487f5075-fa89-4723-a26d-2e7a13245135").isRight) &&
      assertTrue(validation.validate("AB27B385-D019-41F6-BBB4-CD6EC668AD60").isRight) &&
      // version 1 UUID
      assertTrue(validation.validate("1e3118de-ddb6-11ec-8653-93e6961d46be").isLeft) &&
      assertTrue(validation.validate("487f5075-fa89-4723-a26d-2e7a13245").isLeft) &&
      assertTrue(validation.validate("487f5075fa894723a26d2e7a13245135").isLeft) &&
      assertTrue(validation.validate("").isLeft)
    }
  )
}
