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

      assertTrue(validation.validate("hello").isLeft) &&
      assertTrue(validation.validate("Todd").isRight) &&
      assertTrue(validation.validate("how").isRight)
      assertTrue(validation.validate("Automobile").isLeft)
    },
    test("Regex digit or letter Validation") {
      val validation = Validation.regex(Regex.digitOrLetter)
      //val validation = Validation.identifier

      assertTrue(validation.validate("a").isRight) &&
      assertTrue(validation.validate("ab").isLeft) &&
      assertTrue(validation.validate("").isLeft) &&
      assertTrue(validation.validate("&").isLeft)
    },
    test("Regex identifier Validation") {
      val validation = Validation.identifier

      assertTrue(validation.validate("_").isRight)

    },
    test("Regex email Validation") {
      val validation = Validation.email

      assertTrue(validation.validate("_").isLeft) &&
      assertTrue(validation.validate("bob101@gmail.com").isRight) &&
      assertTrue(validation.validate("bob101*@gmail.com").isLeft) &&
      assertTrue(validation.validate("@").isLeft) &&
      assertTrue(validation.validate("@.").isLeft) &&
      assertTrue(validation.validate("b@.com").isLeft) &&
      assertTrue(validation.validate("").isLeft) &&
      assertTrue(validation.validate("1@.com").isLeft) &&
      assertTrue(validation.validate("1@a.com").isLeft) &&
      assertTrue(validation.validate("1@a.b.com").isLeft) &&
      assertTrue(validation.validate("a@a..b.com").isLeft)
      // assertTrue(validation.validate("a..b@a.com").isLeft) TODO - should this be Left or Right?
    },
  )
}
