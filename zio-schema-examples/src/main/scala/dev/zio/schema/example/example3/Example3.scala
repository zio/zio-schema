package dev.zio.schema.example.example3

import zio.schema._
import zio.{Chunk, ExitCode, URIO, ZIO}


/**
 * Example3:
 * In this example we'll take a look on how to use ZIO-Schema to
 * transform a PersonDTO (e.g. from a REST API) into a Person (e.g. for a database).
 **/
final case class PersonDTO(firstname: String, lastname: String, years: Int)
object PersonDTO {
  val firstname = Schema.Field("firstname", Schema.primitive[String])
  val lastname  = Schema.Field("lastname", Schema.primitive[String])
  val years     = Schema.Field("years", Schema.primitive[Int])
  val schema: Schema[PersonDTO] = Schema.CaseClass3[String, String, Int, PersonDTO](
    field1 = firstname,
    field2 = lastname,
    field3 = years,
    construct = (fn, ln, y) => PersonDTO(fn, ln, y),
    extractField1 = _.firstname,
    extractField2 = _.lastname,
    extractField3 = _.years
  )
}

object Example3 extends zio.App {
  import dev.zio.schema.example.example2.Domain._
  import zio.schema.codec.JsonCodec

  val personTransformation: Schema[Person] = PersonDTO.schema.transform[Person](
    (dto: PersonDTO) => Person(dto.firstname + " " + dto.lastname, dto.years),
    (person: Person) => {
      val name = person.name.split(" ").toSeq
      PersonDTO(name.head, name.tail.mkString(" "), person.age)
    }
  )

  val example = for {
    _ <- ZIO.unit
    json = """{"firstname":"John","lastname":"Doe","years":42}"""
    dto <- ZIO.fromEither(JsonCodec.decode[PersonDTO](PersonDTO.schema)(Chunk.fromArray(json.getBytes)))
    _ <- ZIO.debug("PersonDTO: " + dto)
    trans <- ZIO.fromEither(PersonDTO.schema.migrate(personTransformation))
    person <- ZIO.fromEither(trans(dto))
    _ <- ZIO.debug("Person: " + person)
  } yield person

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = for {
    _ <- ZIO.unit
    pers <- example.exitCode

  } yield pers
}
