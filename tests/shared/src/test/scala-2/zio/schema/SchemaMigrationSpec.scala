package zio.schema

import zio._
import zio.schema.syntax._
import zio.test.Assertion._
import zio.test._

object SchemaMigrationSpec extends ZIOSpecDefault {
  import SchemaAssertions._

  override def spec: Spec[TestEnvironment, Any] = suite("Schema Migration Spec")(
    suite("case class")(
      suite("isomorphisms")(isomorphismTests: _*),
      test("delete field recursively") {
        val original = Recursive1(
          0,
          "0",
          Some(
            Recursive1(
              1,
              "1",
              Some(
                Recursive1(
                  2,
                  "2",
                  None
                )
              )
            )
          )
        )

        val expectedMigration = Recursive3(
          0,
          Some(
            Recursive3(
              1,
              Some(
                Recursive3(
                  2,
                  None
                )
              )
            )
          )
        )

        val actualMigration = original.migrate[Recursive3]

        assert(actualMigration)(isRight(equalTo(expectedMigration)))
      },
      test("require optional field") {
        assert(PetFood.DogFood(List("i"), Some("brand")))(migratesTo(BrandedPetFood.DogFood(List("i"), "brand")))
      },
      test("fail when required field is missing") {
        assert(PetFood.DogFood(Nil, None))(cannotMigrateValue[PetFood.DogFood, BrandedPetFood.DogFood])
      }
    ),
    suite("enumN")(
      test("enumN with recursive types") {
        check(Version1.gen) { v1 =>
          assert(v1)(migratesTo(Version1.migrated(v1)))
        }
      },
      test("migrates to equivalent type") {
        check(PetFood.gen) { from =>
          PetFood.brandedEquivalent(from) match {
            case Left(_)   => assert(from)(cannotMigrateValue[PetFood, BrandedPetFood])
            case Right(to) => assert(from)(migratesTo(to))
          }
        }
      }
    ),
    suite("map")(
      test("map keys") {
        check(Gen.mapOf(Gen.int(0, 100), Gen.int(1, 100))) { map =>
          val expected: Map[Option[Int], Int] = map.map { case (k, v) => Some(k) -> v }.toMap
          assert(map)(migratesTo(expected))
        }
      },
      test("map values") {
        check(Gen.mapOf(Gen.int(0, 100), Gen.int(1, 100))) { map =>
          val expected: Map[Int, Option[Int]] = map.map { case (k, v) => k -> Some(v) }
          assert(map)(migratesTo(expected))
        }
      } @@ TestAspect.shrinks(0)
    )
  )

  val isomorphismTests: List[Spec[TestEnvironment with Sized with TestConfig, Nothing]] =
    List(
      test("DogFood <-> CatFood")(
        isomorphismLaw[TestEnvironment, PetFood.DogFood, PetFood.CatFood](
          SchemaGen.anyValueForSchema(Schema[PetFood.DogFood]).map(_._2)
        )
      ),
      test("BrandedDogFood <-> BrandedCatFood")(
        isomorphismLaw[TestEnvironment, BrandedPetFood.DogFood, BrandedPetFood.CatFood](
          SchemaGen.anyValueForSchema(Schema[BrandedPetFood.DogFood]).map(_._2)
        )
      ),
      test("NestedEither1 <-> NestedEither2")(
        isomorphismLaw[TestEnvironment, NestedEither1, NestedEither2](NestedEither1.gen)
      ),
      test("NestedEnum1 <-> NestedEnum2")(
        isomorphismLaw[TestEnvironment, NestedEnum1, NestedEnum2](NestedEnum1.genMigratable)
      ),
      test("Recursive1 <-> Recursive2")(isomorphismLaw[TestEnvironment, Recursive1, Recursive2](Recursive1.gen))
    )

  private def isomorphismLaw[R, A: Schema, B: Schema](
    genA: Gen[Sized, A]
  ): URIO[R with Sized with TestConfig, TestResult] =
    check(genA) { a =>
      val roundTrip = a.migrate[B].flatMap(_.migrate[A])

      assertTrue(roundTrip == Right(a))
    }

  case class Recursive1(level: Int, value: String, r: Option[Recursive1])

  object Recursive1 {
    implicit lazy val schema: Schema[Recursive1] = DeriveSchema.gen

    def genTree(depth: Int): Recursive1 =
      if (depth == 0)
        Recursive1(0, "foo", None)
      else Recursive1(depth, "foo", Some(genTree(depth - 1)))

    lazy val gen: Gen[Sized, Recursive1] =
      Gen.int(2, 10).map(genTree)
  }
  case class Recursive2(level: Int, value: String, r: Option[Recursive2])

  object Recursive2 {
    implicit lazy val schema: Schema[Recursive2] = DeriveSchema.gen

    def genTree(depth: Int): Recursive2 =
      if (depth == 0)
        Recursive2(0, "foo", None)
      else Recursive2(depth, "foo", Some(genTree(depth - 1)))

    lazy val gen: Gen[Sized, Recursive2] =
      Gen.int(2, 10).map(genTree)
  }

  case class Recursive3(level: Int, r: Option[Recursive3])

  object Recursive3 {
    implicit lazy val schema: Schema[Recursive3] = DeriveSchema.gen
  }

  sealed trait PetFood

  object PetFood {
    case class DogFood(ingredients: List[String], brand: Option[String]) extends PetFood

    object DogFood {
      implicit lazy val schema: Schema[DogFood] = DeriveSchema.gen

      lazy val gen: Gen[Sized, DogFood] =
        (Gen.listOf(Gen.string) <*> Gen.option(Gen.string)).map((DogFood.apply _).tupled)
    }
    case class CatFood(ingredients: List[String], brand: Option[String]) extends PetFood

    object CatFood {
      implicit lazy val schema: Schema[CatFood] = DeriveSchema.gen

      lazy val gen: Gen[Sized, CatFood] =
        (Gen.listOf(Gen.string) <*> Gen.option(Gen.string)).map((CatFood.apply _).tupled)
    }

    def brandedEquivalent(p: PetFood): Either[String, BrandedPetFood] = p match {
      case CatFood(ingredients, Some(brand)) => Right(BrandedPetFood.CatFood(ingredients, brand))
      case DogFood(ingredients, Some(brand)) => Right(BrandedPetFood.DogFood(ingredients, brand))
      case _                                 => Left("error")
    }

    implicit lazy val schema: Schema[PetFood] = DeriveSchema.gen

    val gen: Gen[Sized, PetFood] =
      Gen.oneOf(
        DogFood.gen,
        CatFood.gen
      )

    val genMigratable: Gen[Sized, PetFood] =
      Gen.oneOf(
        DogFood.gen.withFilter(_.brand.isDefined),
        CatFood.gen.withFilter(_.brand.isDefined)
      )
  }

  sealed trait BrandedPetFood

  object BrandedPetFood {
    case class DogFood(ingredients: List[String], brand: String) extends BrandedPetFood

    object DogFood {
      implicit lazy val schema: Schema[DogFood] = DeriveSchema.gen

      val gen: Gen[Sized, DogFood] =
        (Gen.listOf(Gen.string) <*> Gen.string).map((DogFood.apply _).tupled)
    }
    case class CatFood(ingredients: List[String], brand: String) extends BrandedPetFood

    object CatFood {
      implicit lazy val schema: Schema[CatFood] = DeriveSchema.gen

      val gen: Gen[Sized, CatFood] =
        (Gen.listOf(Gen.string) <*> Gen.string).map((CatFood.apply _).tupled)
    }
    case class HamsterFood(ingredients: List[String], brand: String) extends BrandedPetFood

    object HamsterFood {
      implicit lazy val schema: Schema[HamsterFood] = DeriveSchema.gen

      val gen: Gen[Sized, HamsterFood] =
        (Gen.listOf(Gen.string) <*> Gen.string).map((HamsterFood.apply _).tupled)
    }

    implicit lazy val schema: Schema[BrandedPetFood] = DeriveSchema.gen

    val gen: Gen[Sized, BrandedPetFood] = Gen.oneOf(DogFood.gen, CatFood.gen, HamsterFood.gen)

  }

  sealed trait Pet

  object Pet {
    case class Cat(color: String, breed: String, favoriteFood: Option[PetFood]) extends Pet

    object Cat {
      implicit def schema: Schema[Cat] = DeriveSchema.gen
    }
    case class Dog(breed: String, legs: Int, favoriteFood: Option[PetFood]) extends Pet

    object Dog {
      implicit def schema: Schema[Dog] = DeriveSchema.gen
    }

    implicit def schema: Schema[Pet] = DeriveSchema.gen
  }

  sealed trait NamedPet

  object NamedPet {
    case class Cat(name: String, color: String, breed: String, weight: Int, favoriteFood: BrandedPetFood)
        extends NamedPet

    object Cat {
      implicit def schema: Schema[Cat] = DeriveSchema.gen
    }
    case class Dog(name: String, color: String, breed: String, weight: Int, favoriteFood: BrandedPetFood)
        extends NamedPet

    object Dog {
      implicit def schema: Schema[Dog] = DeriveSchema.gen
    }
    case class Hamster(name: String, color: String, weight: Int, favoriteFood: BrandedPetFood) extends NamedPet

    object Hamster {
      implicit def schema: Schema[Hamster] = DeriveSchema.gen
    }

    implicit def schema: Schema[NamedPet] = DeriveSchema.gen
  }

  sealed trait Version1

  object Version1 {

    case class A1(v1: Int, v2: String, v3: Option[String], rs: List[Version1]) extends Version1

    implicit lazy val schema: Schema[Version1] = DeriveSchema.gen

    def genTree(depth: Int): Gen[Sized, Version1] =
      for {
        v1 <- Gen.int
        v2 <- Gen.string
        v3 <- Gen.string
        rs <- if (depth > 0) Gen.listOfBounded(1, 3)(genTree(depth - 1)) else Gen.const(Nil)
      } yield A1(v1, v2, Some(v3), rs)

    val gen: Gen[Sized, Version1] = Gen.int(0, 3).flatMap(genTree)

    def migrated(v1: Version1): Version2 = v1 match {
      case A1(v1, _, v3, rs) =>
        Version2.A1(v1, v3.get, rs.map(migrated))

    }
  }

  sealed trait Version2

  object Version2 {
    case class A1(v1: Int, v3: String, rs: List[Version2]) extends Version2

    implicit lazy val schema: Schema[Version2] = DeriveSchema.gen
  }

  case class NestedEither1(v1: Int, v2: Either[String, PetFood.DogFood])

  object NestedEither1 {
    implicit lazy val schema: Schema[NestedEither1] = DeriveSchema.gen

    val gen: Gen[Sized, NestedEither1] =
      for {
        v1 <- Gen.int
        v2 <- Gen.either(Gen.string, PetFood.DogFood.gen)
      } yield NestedEither1(v1, v2)
  }

  case class NestedEither2(v1: Int, v2: Either[String, PetFood.CatFood])

  object NestedEither2 {
    implicit lazy val schema: Schema[NestedEither2] = DeriveSchema.gen

    val gen: Gen[Sized, NestedEither2] =
      for {
        v1 <- Gen.int
        v2 <- Gen.either(Gen.string, PetFood.CatFood.gen)
      } yield NestedEither2(v1, v2)
  }

  case class NestedEnum1(v1: Int, v2: PetFood, v3: List[BrandedPetFood])

  object NestedEnum1 {
    implicit lazy val schema: Schema[NestedEnum1] = DeriveSchema.gen

    val gen: Gen[Sized, NestedEnum1] =
      for {
        v1 <- Gen.int
        v2 <- PetFood.gen
        v3 <- Gen.listOf(BrandedPetFood.gen)
      } yield NestedEnum1(v1, v2, v3)

    val genMigratable: Gen[Sized, NestedEnum1] =
      for {
        v1 <- Gen.int
        v2 <- PetFood.genMigratable
        v3 <- Gen.listOf(
               BrandedPetFood.gen.withFilter {
                 case BrandedPetFood.HamsterFood(_, _) => false
                 case _                                => true
               }
             )
      } yield NestedEnum1(v1, v2, v3)
  }

  case class NestedEnum2(v1: Int, v2: BrandedPetFood, v3: List[PetFood])

  object NestedEnum2 {
    implicit lazy val schema: Schema[NestedEnum2] = DeriveSchema.gen

    val gen: Gen[Sized, NestedEnum2] =
      for {
        v1 <- Gen.int
        v2 <- BrandedPetFood.gen
        v3 <- Gen.listOf(PetFood.gen)
      } yield NestedEnum2(v1, v2, v3)
  }

}
