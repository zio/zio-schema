package dev.zio.schema.example.example5

import zio._
import zio.schema.Schema._
import zio.schema._

/**
 * Example 5: In this example, we use ZIO-Schema to detect changes in our objects.
 *
 */
private[example5] object Domain {
  final case class Person(name: String, age: Int)

  val nameType      = "name"
  val ageType       = "age"
  val firstnameType = "firstname"
  val lastnameType  = "lastname"
  val yearsType     = "years"

  object Person {
    val name: Field[nameType.type, String] = Field[nameType.type, String](nameType, primitive[String])
    val age: Field[ageType.type, Int]      = Field[ageType.type, Int](ageType, primitive[Int])

    val schema: Schema[Person] = CaseClass2[nameType.type, ageType.type, String, Int, Person](
      TypeId.parse("dev.zio.schema.example.example5.Domain.Person"),
      field1 = name,
      field2 = age,
      construct = (name, age) => Person(name, age),
      extractField1 = p => p.name,
      extractField2 = p => p.age
    )
  }

  final case class PersonDTO(firstname: String, lastname: String, years: Int)

  object PersonDTO {
    val firstname: Schema.Field[firstnameType.type, String] = Schema.Field(firstnameType, Schema.primitive[String])
    val lastname: Schema.Field[lastnameType.type, String]   = Schema.Field(lastnameType, Schema.primitive[String])
    val years: Schema.Field[yearsType.type, Int]            = Schema.Field(yearsType, Schema.primitive[Int])

    val schema: Schema[PersonDTO] =
      Schema.CaseClass3[firstnameType.type, lastnameType.type, yearsType.type, String, String, Int, PersonDTO](
        TypeId.parse("dev.zio.schema.example.example5.Domain.PersonDTO"),
        field1 = firstname,
        field2 = lastname,
        field3 = years,
        construct = (fn, ln, y) => PersonDTO(fn, ln, y),
        extractField1 = _.firstname,
        extractField2 = _.lastname,
        extractField3 = _.years
      )
  }

}

object Example5_Diffing extends ZIOAppDefault {

  import Domain._

  val personDTO: PersonDTO = PersonDTO("Mike", "Moe", 32)

  val diff: Patch[PersonDTO] = PersonDTO.schema.diff(personDTO, personDTO.copy(lastname = "Max"))

  override val run: UIO[Unit] = ZIO.debug(diff)
}
