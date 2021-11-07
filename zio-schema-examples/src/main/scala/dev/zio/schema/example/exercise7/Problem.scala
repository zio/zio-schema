package dev.zio.schema.example.exercise7

import zio.schema.Schema

/**
 * This exercise is based on John DeGoes Spartan training on ZIO-Schema from 2021-11-04
 */
private[exercise7] object Problem {


  final case class Person(name: String, age: Int)
  final case class Profile(location: String, address: String)

  def decodePersonFromQueryParams(params: Map[String, List[String]]): Either[String, Person] =
    for {
      name <- params.get("name").toRight("name parameter is missing")
      age  <- params.get("age").toRight("age parameter is missing")
    } yield Person(name.head, age.head.toInt)

  def decodeProfileFromQueryParams(params: Map[String, List[String]]): Either[String, Profile] =
    for {
      location <- params.get("location").toRight("location parameter is missing")
      address  <- params.get("address").toRight("address parameter is missing")
    } yield Profile(location.head, address.head)

  object Approach1 {
    def decodeQueryParams[A](implicit schema: Schema[A]): Either[String, A] = ???
  }

  object Approach2 {
    import Schema._

    def decodeFromQueryParams[A](implicit schema: Schema[A]): Either[String, A] = {
      schema match {
        case enum: Enum[_] => ???
        case record: Record[_] => ???
        case Sequence(schemaA, fromChunk, toChunk) => ???
        case Transform(codec, f, g) => ???
        case Primitive(standardType) => ???
        case Optional(codec) => ???
        case Fail(message) => ???
        case Tuple(left, right) => ???
        case EitherSchema(left, right) => ???
        case Lazy(schema0) => ???
        case Meta(ast) => ???
      }
    }
  }
}
