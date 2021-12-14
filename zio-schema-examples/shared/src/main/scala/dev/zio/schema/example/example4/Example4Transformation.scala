package dev.zio.schema.example.example4

import zio.schema.ast.{ Migration, NodePath, SchemaAst }
import zio.schema.{ DynamicValue, Schema }
import zio.{ Chunk, ExitCode, URIO, ZIO }

/**
 * Example 4: In this Example, we use ZIO-Schema to migrate objects from one representation to another.
 * This is a very common use case in proper designed applications where we have to migrate between
 * different representations depending on the layer, e.g.
 * transforming a Request-DTO to a Domain-DTO to a Database-DTO.
 **/
private[example4] object Domain {
  final case class WebPerson(name: String, age: Int)

  object WebPerson {
    val name: Schema.Field[String] = Schema.Field[String]("name", Schema.primitive[String])
    val age: Schema.Field[Int]     = Schema.Field[Int]("age", Schema.primitive[Int])

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
    val firstname: Schema.Field[String] = Schema.Field("firstname", Schema.primitive[String])
    val lastname: Schema.Field[String]  = Schema.Field("lastname", Schema.primitive[String])
    val years: Schema.Field[Int]        = Schema.Field("years", Schema.primitive[Int])

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

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = ZIO.debug(domainPerson).exitCode
}

// TODO - not working!:
//  Here I try to convert between WebPerson and DomainPerson
//  using the roundtrip with dynamic and SchemaAst migrations.
object Example4Ast extends zio.App {

  import Domain._

  val webPerson: WebPerson = WebPerson("Mike Moe", 32)

  val dyn: Either[String, DynamicValue] = WebPerson.schema
    .toDynamic(webPerson)
    .transform(
      Chunk(
        Migration.AddNode(NodePath.root / "lastname", SchemaAst.fromSchema(DomainPerson.lastname.schema))
//      Migration.Relabel(NodePath.root / "years", Migration.LabelTransformation("age")) // does not compile, LabelTransformation
      )
    )
  //.flatMap(dv => DomainPerson.schema.fromDynamic(dv))

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = ZIO.debug(dyn).exitCode
}

object Example4Ast2 extends zio.App {
  import Domain._

  val webPerson: WebPerson = WebPerson("Mike Moe", 32)

  val personTransformation: Schema[DomainPerson] = WebPerson.schema.transform[DomainPerson](
    (person: WebPerson) => {
      val name = person.name.split(" ").toSeq
      DomainPerson(name.head, name.tail.mkString(" "), person.age)
    },
    (dto: DomainPerson) => WebPerson(dto.firstname + " " + dto.lastname, dto.years)
  )
  val webPersonAst: SchemaAst    = SchemaAst.fromSchema(WebPerson.schema)
  val domainPersonAst: SchemaAst = SchemaAst.fromSchema(DomainPerson.schema)
  val migrationAst: SchemaAst    = SchemaAst.fromSchema(personTransformation)

  val migrationWebPersonAstToMigrationAst: Either[String, Chunk[Migration]] =
    Migration.derive(webPersonAst, migrationAst)

  val migrationWebPersonAstToDomainPersonAst: Either[String, Chunk[Migration]] =
    Migration.derive(webPersonAst, domainPersonAst)

  val effect: ZIO[Any, Nothing, Unit] = for {
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

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = effect.exitCode
}
