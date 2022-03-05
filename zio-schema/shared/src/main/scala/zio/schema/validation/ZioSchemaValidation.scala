package zio.schema.validation

object ZioSchemaValidation {
  // Goal: a way to validate zio-schema data that can be serialized
  // - validation cannot embed any scala functions
  // - validations should be able to be stored as generic data
  // - meta-level
  // Want: A DSL That can encode rules about what valid data is
  //  1) generated in a way that does not care about types
  //  2) we can also try to leverage a type system
  //   validation: A => Boolean

  // validation.fromJsonSchema(schema)

  object examples {
    import Validation._

    val nameValidation: Validation[String] = minLength(4) && maxLength(50)

    val anyInt: Validation[Int]       = anyOf(greaterThan(2), lessThan(4), equalTo(3))
    val anyString: Validation[String] = anyOf(minLength(3), maxLength(10))

    val all: Validation[String] = allOf(minLength(3), maxLength(4))
    val allInt                  = allOf(greaterThan(2), lessThan(4), equalTo(3))
  }

  // object Validation {
  //   // all the leaves will need to have specialized type parameters
  //   // 1) verify that a field inside a record has a certain type
  //   // 2) verify that an integer falls in a certain range *
  //   // 3) verify that a string length falls inside a certain range *
  //   // 4) verify that a string matches a simplified regular expression *
  //   // 5) verify that a decimal number falls within a certain range. *
  //   // 6) verify that strings represent/contain date-time information.
  //   // 7) cross-field validation

  //   final case class Str(stringValidation: Bool[Str]) extends Validation[String] { self =>
  //     def &&(that: Str): Str = {
  //       Str(self.stringValidation && that.stringValidation)
  //     }
  //     def ||(that: Str): Str = {
  //       Str(self.stringValidation || that.stringValidation)
  //     }
  //     def unary_! = Str(!self.stringValidation)
  //   }

  //   final case class Num[A](Num: Bool[Num[A]], numType: NumType[A]) extends Validation[A] { self =>
  //     def &&(that: Num[A]): Num[A] = Num(self.Num && that.Num, that.numType)
  //     def ||(that: Num[A]): Num[A] = Num(self.Num || that.Num, that.numType)
  //     def unary_! : Validation[A] = Num(!self.Num, self.numType)
  //   }
  // }

  // Problems:
  //  - no cross-field validation

  // Need a validation that accepts anything
}
