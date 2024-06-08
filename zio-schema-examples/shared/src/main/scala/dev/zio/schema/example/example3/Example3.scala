package dev.zio.schema.example.example3

import zio._
import zio.schema.Schema._
import zio.schema._

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

    val name: Field[Person, String] =
      Field[Person, String]("name", primitive[String], get0 = _.name, set0 = (p, v) => p.copy(name = v))

    val age: Field[Person, Int] =
      Field[Person, Int]("age", primitive[Int], get0 = _.age, set0 = (p, v) => p.copy(age = v))

    val schema: Schema[Person] = CaseClass2[String, Int, Person](
      TypeId.parse("dev.zio.example.example3.Domain.Person"),
      field01 = name,
      field02 = age,
      construct0 = (name, age) => Person(name, age)
    )
  }

  final case class PersonDTO(firstname: String, lastname: String, years: Int)

  object PersonDTO {

    val firstname: Field[PersonDTO, String] =
      Field("firstname", primitive[String], get0 = _.firstname, set0 = (p, v) => p.copy(firstname = v))

    val lastname: Field[PersonDTO, String] =
      Field("lastname", primitive[String], get0 = _.lastname, set0 = (p, v) => p.copy(lastname = v))

    val years: Field[PersonDTO, Int] =
      Field("years", primitive[Int], get0 = _.years, set0 = (p, v) => p.copy(years = v))

    val schema: Schema[PersonDTO] = CaseClass3[String, String, Int, PersonDTO](
      TypeId.parse("dev.zio.example.example3.Domain.PersonDTO"),
      field01 = firstname,
      field02 = lastname,
      field03 = years,
      construct0 = (fn, ln, y) => PersonDTO(fn, ln, y)
    )
  }

}

object Example3 extends ZIOAppDefault {
  import dev.zio.schema.example.example3.Domain._
  import zio.schema.codec.JsonCodec
    implicit val defaultConfig: JsonCodec.Config = JsonCodec.Config.default


  val personTransformation: Schema[Person] = PersonDTO.schema.transform[Person](
    (dto: PersonDTO) => Person(dto.firstname + " " + dto.lastname, dto.years),
    (person: Person) => {
      val name = person.name.split(" ").toSeq
      PersonDTO(name.head, name.tail.mkString(" "), person.age)
    }
  )

  override val run: ZIO[Environment with ZIOAppArgs, Any, Any] = for {
    _      <- ZIO.unit
    json   = """{"firstname":"John","lastname":"Doe","years":42}"""
    chunks = Chunk.fromArray(json.getBytes)
    _      <- ZIO.debug("input JSON    : " + json)

    // get objects from JSON
    personDTO <- ZIO.fromEither(
                  JsonCodec.schemaBasedBinaryCodec[PersonDTO](PersonDTO.schema, defaultConfig).decode(chunks)
                )
    person <- ZIO.fromEither(
               JsonCodec.schemaBasedBinaryCodec[Person](personTransformation, defaultConfig).decode(chunks)
             )
    _ <- ZIO.debug("PersonDTO     : " + personDTO)
    _ <- ZIO.debug("Person        : " + person)

    // get JSON from Objects
    personJson = new String(
      JsonCodec.schemaBasedBinaryCodec[Person](Person.schema, defaultConfig).encode(person).toArray
    )
    personDTOJson = new String(
      JsonCodec.schemaBasedBinaryCodec[Person](personTransformation, defaultConfig).encode(person).toArray
    )
    _ <- ZIO.debug("Person    JSON: " + personJson)
    _ <- ZIO.debug("PersonDTO JSON: " + personDTOJson)

  } yield ()
}
