package dev.zio.schema.example.example4

import zio._
import zio.schema._
import zio.schema.meta._

/**
 * Example 4: In this Example, we use ZIO-Schema to migrate objects from one representation to another.
 * This is a very common use case in proper designed applications where we have to migrate between
 * different representations depending on the layer, e.g.
 * transforming a Request-DTO to a Domain-DTO to a Database-DTO.
 **/
private[example4] object Domain {
  final case class WebPerson(name: String, age: Int)

  object WebPerson {

    val name: Schema.Field[WebPerson, String] =
      Schema.Field[WebPerson, String]("name", Schema.primitive[String], get = _.name)
    val age: Schema.Field[WebPerson, Int] = Schema.Field[WebPerson, Int]("age", Schema.primitive[Int], get = _.age)

    val schema: Schema[WebPerson] = Schema.CaseClass2[String, Int, WebPerson](
      TypeId.parse("dev.zio.schema.example.example4.Domain.WebPerson"),
      field1 = name,
      field2 = age,
      construct = (name, age) => WebPerson(name, age)
    )
  }

  final case class DomainPerson(firstname: String, lastname: String, years: Int)

  object DomainPerson {

    val firstname: Schema.Field[DomainPerson, String] =
      Schema.Field("firstname", Schema.primitive[String], get = _.firstname)

    val lastname: Schema.Field[DomainPerson, String] =
      Schema.Field("lastname", Schema.primitive[String], get = _.lastname)
    val years: Schema.Field[DomainPerson, Int] = Schema.Field("years", Schema.primitive[Int], get = _.years)

    val schema: Schema[DomainPerson] = Schema.CaseClass3[String, String, Int, DomainPerson](
      TypeId.parse("dev.zio.schema.example.example4.Domain.DomainPerson"),
      field1 = firstname,
      field2 = lastname,
      field3 = years,
      construct = (fn, ln, y) => DomainPerson(fn, ln, y)
    )
  }

}

// TODO - not working:
//  Here I try to convert between WebPerson and DomainPerson using the `transform` method on `Schema`
object Example4Transformation extends ZIOAppDefault {

  import Domain._

  val webPerson: WebPerson = WebPerson("Mike Moe", 32)

  val personTransformation: Schema[DomainPerson] = WebPerson.schema.transform[DomainPerson](
    (person: WebPerson) => {
      val name = person.name.split(" ").toSeq
      DomainPerson(name.head, name.tail.mkString(" "), person.age)
    },
    (dto: DomainPerson) => WebPerson(dto.firstname + " " + dto.lastname, dto.years)
  )

  val domainPerson: Either[String, DomainPerson] =
    WebPerson.schema.migrate(personTransformation).flatMap(f => f(webPerson))

  override def run: UIO[Unit] = ZIO.debug(domainPerson)
}

// TODO - not working!:
//  Here I try to convert between WebPerson and DomainPerson
//  using the roundtrip with dynamic and SchemaAst migrations.
object Example4Ast extends zio.ZIOAppDefault {

  import Domain._

  val webPerson: WebPerson = WebPerson("Mike Moe", 32)

  val dyn: Either[String, DynamicValue] = WebPerson.schema
    .toDynamic(webPerson)
    .transform(
      Chunk(
        Migration.AddNode(NodePath.root / "lastname", MetaSchema.fromSchema(DomainPerson.lastname.schema))
//      Migration.Relabel(NodePath.root / "years", Migration.LabelTransformation("age")) // does not compile, LabelTransformation
      )
    )
  //.flatMap(dv => DomainPerson.schema.fromDynamic(dv))

  override def run: UIO[Unit] = ZIO.debug(dyn)
}

object Example4Ast2 extends zio.ZIOAppDefault {
  import Domain._

  val webPerson: WebPerson = WebPerson("Mike Moe", 32)

  val personTransformation: Schema[DomainPerson] = WebPerson.schema.transform[DomainPerson](
    (person: WebPerson) => {
      val name = person.name.split(" ").toSeq
      DomainPerson(name.head, name.tail.mkString(" "), person.age)
    },
    (dto: DomainPerson) => WebPerson(dto.firstname + " " + dto.lastname, dto.years)
  )
  val webPersonAst: MetaSchema    = MetaSchema.fromSchema(WebPerson.schema)
  val domainPersonAst: MetaSchema = MetaSchema.fromSchema(DomainPerson.schema)
  val migrationAst: MetaSchema    = MetaSchema.fromSchema(personTransformation)

  val migrationWebPersonAstToMigrationAst: Either[String, Chunk[Migration]] =
    Migration.derive(webPersonAst, migrationAst)

  val migrationWebPersonAstToDomainPersonAst: Either[String, Chunk[Migration]] =
    Migration.derive(webPersonAst, domainPersonAst)

  override def run: ZIO[Environment with ZIOAppArgs, Any, Any] =
    for {
      _ <- ZIO.debug(webPersonAst)
      _ <- ZIO.debug(domainPersonAst)
      _ <- ZIO.debug(migrationAst)
      _ <- ZIO.debug("migrationWebPersonAstToMigrationAst" + migrationWebPersonAstToMigrationAst)
      _ <- ZIO.debug("migrationWebPersonAstToDomainPersonAst" + migrationWebPersonAstToDomainPersonAst)
      x = WebPerson.schema.migrate(personTransformation).flatMap(f => f(webPerson))
      _ <- ZIO.debug(x) // Left(Failed to cast Record(ListMap(name -> Primitive(Mike Moe,string), age -> Primitive(32,int))) to schema Transform(CaseClass2(Field(name,Primitive(string)),Field(age,Primitive(int)))))

      domainPerson = WebPerson.schema.migrate(DomainPerson.schema).flatMap(f => f(webPerson))
      _            <- ZIO.debug(domainPerson) // Left(Cannot add node at path firstname: No default value is available)
    } yield ()
}
