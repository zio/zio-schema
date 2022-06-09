package zio.schema.validation

object ZioSchemaValidation {
  // validation.fromJsonSchema(schema)

  object examples {
    import Validation._

    val nameValidation: Validation[String] = minLength(4) && maxLength(50)

    val anyInt: Validation[Int]       = anyOf(greaterThan(2), lessThan(4), equalTo(3))
    val anyString: Validation[String] = anyOf(minLength(3), maxLength(10))

    val all: Validation[String] = allOf(minLength(3), maxLength(4))
    val allInt: Validation[Int] = allOf(greaterThan(2), lessThan(4), equalTo(3))
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

  // Problems:
  //  - no cross-field validation

  // Need a validation that accepts anything
}
