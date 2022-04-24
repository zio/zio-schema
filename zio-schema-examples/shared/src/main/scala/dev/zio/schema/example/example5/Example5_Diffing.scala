package dev.zio.schema.example.example5

import zio.schema.{ Diff, Schema }
import zio.schema.Schema._
import zio.{ ExitCode, URIO, ZIO }

/**
 * Example 5: In this example, we use ZIO-Schema to detect changes in our objects.
 *
 */
private[example5] object Domain {
  final case class Person(name: String, age: Int)

  object Person {
    val name = Field[String]("name", primitive[String])
    val age  = Field[Int]("age", primitive[Int])

    val schema: Schema[Person] = CaseClass2[String, Int, Person](
      field1 = name,
      field2 = age,
      construct = (name, age) => Person(name, age),
      extractField1 = p => p.name,
      extractField2 = p => p.age
    )
  }

  final case class PersonDTO(firstname: String, lastname: String, years: Int)

  object PersonDTO {
    val firstname = Field("firstname", primitive[String])
    val lastname  = Field("lastname", primitive[String])
    val years     = Field("years", primitive[Int])

    val schema: Schema[PersonDTO] =
      CaseClass3[String, String, Int, PersonDTO](
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

object Example5_Diffing extends zio.App {

  import Domain._

  val personDTO: PersonDTO = PersonDTO("Mike", "Moe", 32)

  val diff: Diff[PersonDTO] = PersonDTO.schema.diff(personDTO, personDTO.copy(lastname = "Max"))

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = ZIO.debug(diff).exitCode
}
