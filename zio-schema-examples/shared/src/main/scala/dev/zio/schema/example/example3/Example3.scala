package dev.zio.schema.example.example3

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

  final case class PersonDTO(firstname: String, lastname: String, years: Int)

  object PersonDTO {
    val firstname: Schema.Field[String] = Schema.Field("firstname", Schema.primitive[String])
    val lastname: Schema.Field[String]  = Schema.Field("lastname", Schema.primitive[String])
    val years: Schema.Field[Int]        = Schema.Field("years", Schema.primitive[Int])

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
