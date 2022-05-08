package dev.zio.schema.example.example3

import zio.schema.Schema._
import zio.schema._
import zio.{ Chunk, ExitCode, URIO, ZIO }

/**
 * Example3:
 * In this example we'll take a look on how to use ZIO-Schema to
 * transform a PersonDTO (e.g. from a REST API) into a Person (e.g. for a database) in a single step.
 *
 * To do this, we'll transform the Schema of the PersonDTO into a Schema of the Person.
 **/
private[example3] object Domain {
  final case class Person(name: String, age: Int)

  object Person {
    val name: Field[String] = Field[String]("name", primitive[String])
    val age: Field[Int]     = Field[Int]("age", primitive[Int])

    val schema: Schema[Person] = CaseClass2[String, Int, Person](
      TypeId.parse("dev.zio.example.example3.Domain.Person"),
      field1 = name,
      field2 = age,
      construct = (name, age) => Person(name, age),
      extractField1 = p => p.name,
      extractField2 = p => p.age
    )
  }

  final case class PersonDTO(firstname: String, lastname: String, years: Int)

  object PersonDTO {
    val firstname: Field[String] = Field("firstname", primitive[String])
    val lastname: Field[String]  = Field("lastname", primitive[String])
    val years: Field[Int]        = Field("years", primitive[Int])

    val schema: Schema[PersonDTO] = CaseClass3[String, String, Int, PersonDTO](
      TypeId.parse("dev.zio.example.example3.Domain.PersonDTO"),
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

object Example3 extends zio.App {
  import dev.zio.schema.example.example3.Domain._
  import zio.schema.codec.JsonCodec

  val personTransformation: Schema[Person] = PersonDTO.schema.transform[Person](
    (dto: PersonDTO) => Person(dto.firstname + " " + dto.lastname, dto.years),
    (person: Person) => {
      val name = person.name.split(" ").toSeq
      PersonDTO(name.head, name.tail.mkString(" "), person.age)
    }
  )

  val example: ZIO[Any, String, Person] = for {
    _      <- ZIO.unit
    json   = """{"firstname":"John","lastname":"Doe","years":42}"""
    chunks = Chunk.fromArray(json.getBytes)
    _      <- ZIO.debug("input JSON    : " + json)

    // get objects from JSON
    personDTO <- ZIO.fromEither(JsonCodec.decode[PersonDTO](PersonDTO.schema)(chunks))
    person    <- ZIO.fromEither(JsonCodec.decode[Person](personTransformation)(chunks))
    _         <- ZIO.debug("PersonDTO     : " + personDTO)
    _         <- ZIO.debug("Person        : " + person)

    // get JSON from Objects
    personJson    = new String(JsonCodec.encode[Person](Person.schema)(person).toArray)
    personDTOJson = new String(JsonCodec.encode[Person](personTransformation)(person).toArray)
    _             <- ZIO.debug("Person    JSON: " + personJson)
    _             <- ZIO.debug("PersonDTO JSON: " + personDTOJson)

  } yield person

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = example.exitCode
}
