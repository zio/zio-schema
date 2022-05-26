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
    test("DateFormat") {
      val mmddyyyy = Validation.dateTime("mm-dd-yyyy")
      val mmdd     = Validation.dateTime("mm/dd")

      assertTrue(mmddyyyy.validate("09-13-2020").isRight) &&
      assertTrue(mmddyyyy.validate("09-13").isLeft) &&
      assertTrue(mmddyyyy.validate("09/13/2020").isLeft) &&
      assertTrue(mmdd.validate("09/13").isRight) &&
      assertTrue(mmdd.validate("09-13").isLeft) &&
      assertTrue(mmdd.validate("09/13/2020").isLeft)
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
    test("Regex phone number Validation for CH") {
      val validation = Validation.phoneNumberCh

      assertTrue(validation.validate("0041791234567").isRight) &&
      assertTrue(validation.validate("0041 79 123 45 67").isRight) &&
      assertTrue(validation.validate("+41791234567").isRight) &&
      assertTrue(validation.validate("+41 79 123 45 67").isRight) &&
      assertTrue(validation.validate("0791234567").isRight) &&
      assertTrue(validation.validate("-41 79 123 45 67").isLeft) &&
      assertTrue(validation.validate("+41 79 123 45 678").isLeft) &&
      assertTrue(validation.validate("79 123 45 678").isLeft)
    },
    test("Regex phone number Validation for DE") {
      val validation = Validation.phoneNumberDe

      assertTrue(validation.validate("+49 30 901820").isRight) &&
      assertTrue(validation.validate("004930901820").isRight) &&
      assertTrue(validation.validate("030901820").isRight) &&
      assertTrue(validation.validate("030 901820").isRight) &&
      assertTrue(validation.validate("+49 1522 3433333").isRight) &&
      assertTrue(validation.validate("+49 152 901820").isRight) &&
      assertTrue(validation.validate("0049 152 901820").isRight) &&
      assertTrue(validation.validate("+49 152 901820").isRight) &&
      assertTrue(validation.validate("0041 30 901820").isLeft) &&
      assertTrue(validation.validate("+49 0152 901820").isLeft) &&
      assertTrue(validation.validate("49 152 901820").isLeft) &&
      assertTrue(validation.validate("049 152 901820").isLeft)
    },
    test("Regex phone number Validation for HU") {
      val validation = Validation.phoneNumberHu

      assertTrue(validation.validate("003612318855").isRight) &&
      assertTrue(validation.validate("0036 1 231 88 55").isRight) &&
      assertTrue(validation.validate("0036 1 231 8855").isRight) &&
      assertTrue(validation.validate("+3611234567").isRight) &&
      assertTrue(validation.validate("+36 1 123 45 67").isRight) &&
      assertTrue(validation.validate("+36 1 123 4567").isRight) &&
      assertTrue(validation.validate("0611234567").isRight) &&
      assertTrue(validation.validate("0036-30-231-88-55").isRight) &&
      assertTrue(validation.validate("0036-30-231-8855").isRight) &&
      assertTrue(validation.validate("+36301234567").isRight) &&
      assertTrue(validation.validate("+36-30-123-45-67").isRight) &&
      assertTrue(validation.validate("+36-30-123-4567").isRight) &&
      assertTrue(validation.validate("06301234567").isRight) &&
      assertTrue(validation.validate("+36 11 123 45 67").isLeft) &&
      assertTrue(validation.validate("+36 5 123 45 67").isLeft) &&
      assertTrue(validation.validate("-36 1 123 45 67").isLeft) &&
      assertTrue(validation.validate("+36 1 123 45 678").isLeft) &&
      assertTrue(validation.validate("1 123 45 678").isLeft) &&
      assertTrue(validation.validate("-36-30-123-45-67").isLeft) &&
      assertTrue(validation.validate("+36-30-123-45-678").isLeft) &&
      assertTrue(validation.validate("30-123-45-678").isLeft)
    }
  )
}
