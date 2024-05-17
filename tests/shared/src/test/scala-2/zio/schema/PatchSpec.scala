package zio.schema

import zio.schema.DeriveSchema.gen
import zio.schema.SchemaGen.{ SchemaTest, schemasAndGens }
import zio.schema.types.Arities._
import zio.schema.types.{ Arities, Recursive }
import zio.test.Assertion._
import zio.test._
import zio.{ Scope, URIO }

object PatchSpec extends ZIOSpecDefault {

  def spec: Spec[TestEnvironment with Scope, Any] = suite("PatchSpec")(
    suite("identity law")(
      suite("standard types")(
        schemaPatchIdentityLawTests()
      ),
      suite("option")(
        schemaPatchIdentityLawTests(Some(schema => Schema.option(schema)))
      ),
      suite("either")(
        schemaPatchIdentityLawTests(Some(schema => Schema.either(schema, schema)), Some(name => s"Either[$name,$name]"))
      ),
      suite("records")(
        test("singleton")(patchIdentityLaw[Singleton.type]),
        test("case class")(patchIdentityLaw[Pet.Dog]),
        test("generic record")(patchIdentityLaw[SchemaGen.Arity24]),
        test("recursive")(patchIdentityLaw[Recursive.RecursiveList])
      ),
      suite("enums")(
        test("sealed trait")(patchIdentityLaw[Pet]),
        test("high arity")(patchIdentityLaw[Arities]) @@ TestAspect.ignore,
        test("recursive")(patchIdentityLaw[Recursive])
      )
    ),
    suite("patch law")(
      suite("standard types")(
        schemaPatchLawTests()
      ),
      suite("option")(
        schemaPatchLawTests(Some(schema => Schema.option(schema)))
      ),
      suite("either")(
        schemaPatchLawTests(Some(schema => Schema.either(schema, schema)), Some(name => s"Either[$name,$name]"))
      ),
      suite("lists")(
        suite("of standard types")(
          schemaPatchLawTests(Some((primitiveSchema => Schema.list(primitiveSchema))))
        ),
        suite("of records")(
          test("Dog")(patchLaw[List[Pet.Dog]])
        ),
        suite("of enumerations")(
          test("Pet")(patchLaw[List[Pet]]),
          test("recursive")(patchLaw[List[Recursive]])
        )
      ),
      suite("vectors")(
        suite("of standard types")(
          schemaPatchLawTests(Some((primitiveSchema => Schema.vector(primitiveSchema))))
        ),
        suite("of records")(
          test("Dog")(patchLaw[Vector[Pet.Dog]])
        ),
        suite("of enumerations")(
          test("Pet")(patchLaw[Vector[Pet]]),
          test("recursive")(patchLaw[Vector[Recursive]])
        )
      ),
      suite("sets")(
        suite("of standard types")(
          schemaPatchLawTests(Some((primitiveSchema => Schema.set(primitiveSchema))))
        ),
        suite("of records")(
          test("Dog")(patchLaw[Set[Pet.Dog]])
        ),
        suite("of enumerations")(
          test("Pet")(patchLaw[Set[Pet]]),
          test("recursive")(patchLaw[Set[Recursive]])
        )
      ),
      suite("maps")(
        suite("of standard types")(
          schemaPatchLawTests(
            Some((primitiveSchema => Schema.map(primitiveSchema, primitiveSchema))),
            Some(name => (s"$name -> $name"))
          )
        ),
        suite("of records")(
          test("Int -> Dog")(patchLaw[Map[Int, Pet.Dog]]),
          test("Dog -> Cat")(patchLaw[Map[Pet.Dog, Pet.Cat]])
        ),
        suite("of enumerations")(
          test("Int -> Pet")(patchLaw[Map[Int, Pet]]),
          test("Dog -> Pet")(patchLaw[Map[Pet.Dog, Pet]]),
          test("Pet -> Pet")(patchLaw[Map[Pet, Pet]])
        )
      ),
      suite("records")(
        test("singleton")(patchLaw[Singleton.type]),
        test("case class")(patchLaw[Pet.Dog]),
        test("generic record")(patchLaw[SchemaGen.Arity24]),
        test("recursive")(patchLaw[Recursive.RecursiveEither])
      ),
      suite("enums")(
        test("sealed trait")(patchLaw[Pet]),
        test("high arity")(patchLaw[Arities]) @@ TestAspect.ignore,
        test("recursive")(patchLaw[Recursive])
      )
    ),
    suite("not comparable")(
      test("Left <-> Right") {
        notComparable[Either[String, String]](_.isLeft, _.isRight)(_.isLeft)
      },
      test("Separate enum cases") {
        notComparable[Pet](_.isInstanceOf[Pet.Dog], _.isInstanceOf[Pet.Cat])(_.isLeft)
      }
    )
  )

  private def patchIdentityLaw[A](implicit schema: Schema[A]): URIO[Sized with TestConfig, TestResult] =
    check(DeriveGen.gen[A]) { a =>
      assertTrue(schema.diff(a, a).isIdentical)
    }

  private def schemaPatchIdentityLawTests(
    schemaConversionFuncOption: Option[Schema[_] => Schema[_]] = None,
    renamingFuncOption: Option[String => String] = None
  ): List[Spec[Sized with TestConfig, Nothing]] =
    schemaPatchTests(schema => patchIdentityLaw(schema), schemaConversionFuncOption, renamingFuncOption)

  private def schemaPatchLawTests(
    schemaConversionFuncOption: Option[Schema[_] => Schema[_]] = None,
    renamingFuncOption: Option[String => String] = None
  ): List[Spec[Sized with TestConfig, Nothing]] =
    schemaPatchTests(schema => patchLaw(schema), schemaConversionFuncOption, renamingFuncOption)

  private def schemaPatchTests(
    patchingFunc: Schema[_] => URIO[Sized with TestConfig, TestResult],
    schemaConversionFuncOption: Option[Schema[_] => Schema[_]],
    renamingFuncOption: Option[String => String]
  ): List[Spec[Sized with TestConfig, Nothing]] =
    schemasAndGens.map {
      case SchemaTest(name, standardType, _) =>
        val primitiveSchema = Schema.primitive(standardType)
        val finalSchema = schemaConversionFuncOption.fold[Schema[_]](primitiveSchema) { schemaConversionFunc =>
          schemaConversionFunc(primitiveSchema)
        }
        val finalTestName = renamingFuncOption.fold(name)(renamingFunc => renamingFunc(name))
        standardType match {
          case _ @StandardType.MonthDayType =>
            test(finalTestName) {
              patchingFunc(finalSchema)
            } @@ TestAspect.ignore // TODO Leap years!
          case _ =>
            test(finalTestName) {
              patchingFunc(finalSchema)
            }
        }
    }

  private def patchLaw[A](implicit schema: Schema[A]): URIO[Sized with TestConfig, TestResult] = {
    val gen = DeriveGen.gen[A]
    check(gen <*> gen) {
      case (l, r) =>
        val diff = schema.diff(l, r)
        if (diff.isComparable) {
          val afterInvert        = diff.invert.invert
          val patched            = schema.diff(l, r).patch(l)
          val patchedAfterInvert = afterInvert.patch(l)
          assert(patched)(isRight(equalTo(r))) && assert(patchedAfterInvert)(isRight(equalTo(r)))
        } else {
          assertTrue(true)
        }
    }
  }

  private def notComparable[A](leftFilter: A => Boolean, rightFilter: A => Boolean)(
    assertion: Either[String, A] => Boolean
  )(implicit schema: Schema[A]): URIO[Sized with TestConfig, TestResult] = {
    val gen = DeriveGen.gen[A]

    check(gen.withFilter(leftFilter) <*> gen.withFilter(rightFilter)) {
      case (l, r) =>
        assert(assertion(schema.diff(l, r).patch(l)))(isTrue)
    }
  }

  sealed trait Pet

  object Pet {
    case class Dog(name: String) extends Pet

    object Dog {
      implicit lazy val schema: Schema[Dog] = DeriveSchema.gen[Dog]
    }
    case class Cat(name: String) extends Pet

    object Cat {
      implicit lazy val schema: Schema[Cat] = DeriveSchema.gen
    }
    case class Parrot(name: String, color: Int = 55) extends Pet

    object Parrot {
      implicit val schema: Schema[Parrot] = DeriveSchema.gen
    }

    implicit lazy val schema: Schema[Pet] = DeriveSchema.gen
  }

  case class Person(name: String, age: Int)

  object Person {
    implicit lazy val schema: Schema[Person] = DeriveSchema.gen
  }

  case object Singleton

  implicit val singletonSchema: Schema[Singleton.type] = Schema.singleton(Singleton)
}
