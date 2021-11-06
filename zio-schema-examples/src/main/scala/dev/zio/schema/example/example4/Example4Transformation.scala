package dev.zio.schema.example.example4

import zio.schema.Schema
import zio.schema.ast.{Migration, NodePath, SchemaAst}
import zio.{Chunk, ExitCode, URIO, ZIO}

/**
 * Example 4: In this Example, we use ZIO-Schema to migrate objects from one representation to another.
 * This is a very common use case in proper designed applications where we have to migrate between
 * different representations depending on the layer, e.g.
 * transforming a Request-DTO to a Domain-DTO to a Database-DTO.
 **/
private[example4] object Domain {
  final case class WebPerson(name: String, age: Int)
  object WebPerson {
    val name = Schema.Field[String]("name", Schema.primitive[String])
    val age  = Schema.Field[Int]("age", Schema.primitive[Int])
    val schema: Schema[WebPerson] = Schema.CaseClass2[String, Int, WebPerson](
      field1 = name,
      field2 = age,
      construct = (name, age) => WebPerson(name, age),
      extractField1 = p => p.name,
      extractField2 = p => p.age
    )
  }

  final case class DomainPerson(firstname: String, lastname: String, years: Int)
  object DomainPerson {
    val firstname = Schema.Field("firstname", Schema.primitive[String])
    val lastname  = Schema.Field("lastname", Schema.primitive[String])
    val years     = Schema.Field("years", Schema.primitive[Int])
    val schema: Schema[DomainPerson] = Schema.CaseClass3[String, String, Int, DomainPerson](
      field1 = firstname,
      field2 = lastname,
      field3 = years,
      construct = (fn, ln, y) => DomainPerson(fn, ln, y),
      extractField1 = _.firstname,
      extractField2 = _.lastname,
      extractField3 = _.years
    )
  }

}

// TODO - not working:
//  Here I try to convert between WebPerson and DomainPerson using the `transform` method on `Schema`
object Example4Transformation extends zio.App {

  import Domain._

  val webPerson = WebPerson("Mike Moe", 32)

  val personTransformation: Schema[DomainPerson] = WebPerson.schema.transform[DomainPerson](
    (person: WebPerson) => {
      val name = person.name.split(" ").toSeq
      DomainPerson(name.head, name.tail.mkString(" "), person.age)
    },
    (dto: DomainPerson) => WebPerson(dto.firstname + " " + dto.lastname, dto.years),
  )
  val domainPerson = WebPerson.schema.migrate(personTransformation).map(f => f(webPerson))

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = ZIO.debug(domainPerson).exitCode
}

// TODO - not working!:
//  Here I try to convert between WebPerson and DomainPerson
//  using the roundtrip with dynamic and SchemaAst migrations.
object Example4Ast extends zio.App {

  import Domain._

  val webPerson = WebPerson("Mike Moe", 32)

  val dyn = WebPerson.schema.toDynamic(webPerson).transform(
    Chunk(
      Migration.AddNode(NodePath.root / "lastname", SchemaAst.fromSchema(DomainPerson.lastname.schema)),
//      Migration.Relabel(NodePath.root / "years", Migration.LabelTransformation("age")) // does not compile, LabelTransformation
  ))
    //.flatMap(dv => DomainPerson.schema.fromDynamic(dv))


  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = ZIO.debug(dyn).exitCode
}
