package dev.zio.schema.example.example5

import zio._
import zio.schema._

/**
 * Example 5: In this example, we use ZIO-Schema to detect changes in our objects.
 *
 */
private[example5] object Domain {
  final case class Person(name: String, age: Int)

  object Person {
    val name: Schema.Field[String] = Schema.Field[String]("name", Schema.primitive[String])
    val age: Schema.Field[Int]     = Schema.Field[Int]("age", Schema.primitive[Int])

    val schema: Schema[Person] = Schema.CaseClass2[String, Int, Person](
      field1 = name,
      field2 = age,
      construct = (name, age) => Person(name, age),
      extractField1 = p => p.name,
      extractField2 = p => p.age
    )
  }

  final case class PersonDTO(firstname: String, lastname: String, years: Int, isMajor: Boolean)

  object PersonDTO {
    val firstname: Schema.Field[String] = Schema.Field("firstname", Schema.primitive[String])
    val lastname: Schema.Field[String]  = Schema.Field("lastname", Schema.primitive[String])
    val years: Schema.Field[Int]        = Schema.Field("years", Schema.primitive[Int])
    val isMajor: Schema.Field[Boolean]  = Schema.Field("isMajor", Schema.primitive[Boolean])

    val schema: Schema[PersonDTO] = Schema.CaseClass4[String, String, Int, Boolean, PersonDTO](
      field1 = firstname,
      field2 = lastname,
      field3 = years,
      field4 = isMajor,
      construct = (fn, ln, y, m) => PersonDTO(fn, ln, y, m),
      extractField1 = _.firstname,
      extractField2 = _.lastname,
      extractField3 = _.years,
      extractField4 = _.isMajor
    )
  }

}

object Example5_Diffing extends ZIOAppDefault {

  import Domain._

  val personDTO: PersonDTO = PersonDTO("Mike", "Moe", 32, true)

  val patch: Patch[PersonDTO] =
    PersonDTO.schema.diff(personDTO, personDTO.copy(lastname = "Max", years = 40, isMajor = false))

  val result: Either[String, PersonDTO] = PersonDTO.schema.patch(personDTO, patch)

  val inverted: Either[String, PersonDTO] = result.flatMap(t => PersonDTO.schema.unpatch(t, patch))

  override val run: UIO[Unit] = ZIO.debug(result) *> ZIO.debug(inverted)
}
