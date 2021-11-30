package dev.zio.schema.example.exercise7

import zio.Chunk
import zio.schema.{DeriveSchema, DynamicValue, Schema, StandardType}

import scala.collection.immutable.ListMap

/** This exercise is based on John DeGoes Spartan training on ZIO-Schema from 2021-11-04
  */
private[exercise7] object Problem {

  final case class Person(name: String, age: Int)
  object Person  {
    implicit val schema: Schema[Person] = DeriveSchema.gen[Person]
  }
  final case class Profile(location: String, address: String)
  object Profile {
    implicit val schema: Schema[Profile] = DeriveSchema.gen[Profile]
  }
  // sample url1: /foo/?name=john&age=42#foo
  // sample url2: /foo/?name=john&age=42&location=london&address=baker%20street

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

  object Approach1 extends scala.App {
    // this will be a "quick and dirty" solution, that can be accomplished in a few minutes.
    // not suitable for _extremely high performance_ applications
    // probably suitable for the normal business application with medium performance requirements
    def decode[A](
        params: Map[String, List[String]]
    )(implicit schema: Schema[A]): Either[String, A] =
      toDV(params)
        .map(_.toTypedValue(schema))
        .collectFirst { case Right(v) =>
          v
        }
        .toRight("some error")

    // parse each element into String and if possible Int representations. We basically create all
    // possible solutions here. The Set[DynamicValue] removes duplicates.
    def toDV(params: Map[String, List[String]]): Set[DynamicValue] = {
      import DynamicValue._
      params
        .foldLeft[Set[ListMap[String, DynamicValue]]](Set(ListMap())) { case (set, (key, values)) =>
          set.flatMap { acc =>
            values match {
              case Nil      => Set(acc.updated(key, Singleton(())))
              case x :: Nil =>
                val strInterpretation =
                  Set(acc.updated(key, Primitive[String](x, StandardType.StringType)))
                val intInterpretation = x.toIntOption match {
                  case Some(value) =>
                    Set(acc.updated(key, Primitive[Int](value, StandardType.IntType)))
                  case None        => Set()
                }
                strInterpretation ++ intInterpretation
              case xs       =>
                Set(
                  acc.updated(
                    key,
                    DynamicValue.Sequence(
                      Chunk.fromIterable(xs).map(Primitive[String](_, StandardType.StringType))
                    )
                  )
                )
            }
          }
        }
        .map(DynamicValue.Record)
    }

    val p = decode[Person](Map("name" -> List("John"), "age" -> List("42")))

    println(p)
  }

  object Approach2 extends scala.App {
    import Schema._
    type QueryParams = Map[String, List[String]]

    // this will be a sophisticated solution for a high performance library like ZIO
    def decodeFromQueryParams[A](
        params: QueryParams
    )(implicit schema: Schema[A], decoder: QueryParams => Either[String, A]): Either[String, A] =
      decoder(params)

    def buildDecoder[A](implicit schema: Schema[A]): QueryParams => Either[String, A] = {

      def compile[B](key: Option[String], schema: Schema[B]): QueryParams => Either[String, B] =
        schema match {
          case transform: Transform[a, B] =>
            import transform.{f, codec}
            val func = compile(key, codec)
            (params: QueryParams) => func(params).flatMap(f)
          case Primitive(standardType, _) =>
            key match {
              case None      =>
                val error = Left(s"Cannot extract a primitive out of a query string")
                Function.const(error)
              case Some(key) =>
                standardType match {
                  case StandardType.StringType =>
                    val f: QueryParams => Either[String, B] = (qp: QueryParams) =>
                      qp.get(key) match {
                        case Some(value :: _) => Right[String, B](value.asInstanceOf[B])
                        case _                => Left(s"Cannot extract a primitive string out of nothing")
                      }
                    f
                  case StandardType.IntType    =>
                    val f: QueryParams => Either[String, B] = (qp: QueryParams) =>
                      qp.get(key) match {
                        case Some(value :: _) =>
                          value.toIntOption
                            .toRight(s"cannot create an integer out of ${value}")
                            .asInstanceOf[Either[String, B]]
                        case _                => Left(s"Cannot extract a primitive string out of nothing")
                      }
                    f
                  case _                       =>
                    val error = Left(s"Expected String or Int but found ${standardType}")
                    Function.const(error)
                }
            }

          case caseClass1: CaseClass1[a, B] =>
            import caseClass1.{field, construct}
            val f = compile[a](Some(field.label), field.schema)
            (qp: QueryParams) => f(qp).map(v => construct(v))

          case caseClass2: CaseClass2[a, b, B] =>
            import caseClass2.{field1, field2, construct}
            val f1 = compile[a](Some(field1.label), field1.schema)
            val f2 = compile[b](Some(field2.label), field2.schema)

            (qp: QueryParams) =>
              for {
                v1 <- f1(qp)
                v2 <- f2(qp)
              } yield construct(v1, v2)

          case caseClass3: CaseClass3[a, b, c, B] =>
            import caseClass3.{field1, field2, field3, construct}
            val f1 = compile[a](Some(field1.label), field1.schema)
            val f2 = compile[b](Some(field2.label), field2.schema)
            val f3 = compile[c](Some(field3.label), field3.schema)

            (qp: QueryParams) =>
              for {
                v1 <- f1(qp)
                v2 <- f2(qp)
                v3 <- f3(qp)
              } yield construct(v1, v2, v3)

          case record: Record[_] => ???

          case enum: Enum[_]    => ???
          //        case Optional(codec) => ???
          case Fail(message, _) => Function.const(Left(message))
          //        case Tuple(left, right) => ???
          //        case EitherSchema(left, right) => ???
          case Lazy(schema0)    =>
            // lazy val to make sure its only compiled on first usage and not instantly recursing
            lazy val compiled = compile(key, schema0())
            (qp: QueryParams) => compiled(qp)
          case Meta(ast, _)     => compile(key, ast.toSchema.asInstanceOf[Schema[B]])
          case _                =>
            val err = Left(s"Decoding from query parameters is not supported for ${schema}")
            Function.const(err)
        }

      compile(None, schema)
    }

    implicit val personDecoder = buildDecoder[Person]

    println("approach 2")

    private val data = Map("name" -> List("John"), "age" -> List("42"))

    println(decodeFromQueryParams[Person](data))
  }
}

object Runner extends scala.App {

  Problem.Approach1.main(Array.empty)
  Problem.Approach2.main(Array.empty)
}
