package zio.schema.validation
import zio.test._

object PhoneNumberValidationSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("PhoneNumberValidationSpec")(
    test("Regex phone number Validation for CH") {
      val validation = PhoneNumberValidations.phoneNumberCh

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
      val validation = PhoneNumberValidations.phoneNumberDe

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
      val validation = PhoneNumberValidations.phoneNumberHu

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
    },
    test("Regex phone number Validation for PT") {
      val validation = PhoneNumberValidations.phoneNumberPt

      assertTrue(validation.validate("00351211140200").isRight) &&
        assertTrue(validation.validate("00351 211 140 200").isRight) &&
        assertTrue(validation.validate("00351 21 114 02 00").isRight) &&
        assertTrue(validation.validate("+351211140200").isRight) &&
        assertTrue(validation.validate("+351 21 114 02 00").isRight) &&
        assertTrue(validation.validate("+351 21 114 0200").isRight) &&
        assertTrue(validation.validate("-351 21 114 02 00").isLeft) &&
        assertTrue(validation.validate("+351 21 114 02 006").isLeft) &&
        assertTrue(validation.validate("21 114 02 006").isLeft)
    }
  )
}
