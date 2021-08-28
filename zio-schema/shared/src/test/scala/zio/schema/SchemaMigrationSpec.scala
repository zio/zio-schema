package zio.schema

import zio._
import zio.random.Random
import zio.test.Assertion._
import zio.test._
import zio.test.environment.TestEnvironment

object SchemaMigrationSpec extends DefaultRunnableSpec {
  override def spec: ZSpec[TestEnvironment, Failure] = suite("Schema Migration Spec")(
    suite("case class")(
      suite("isomorphisms")(isomorphismTests: _*)
    )
  )

  val isomorphismTests: List[ZSpec[TestEnvironment with Sized with Random with TestConfig, Nothing]] =
    List(
      testM("DogFood <-> CatFood")(
        isomorphismLaw[TestEnvironment, PetFood.DogFood, PetFood.CatFood](
          SchemaGen.anyValueForSchema(Schema[PetFood.DogFood]).map(_._2)
        )
      ),
      testM("BrandedDogFood <-> BrandedCatFood")(
        isomorphismLaw[TestEnvironment, BrandedPetFood.BrandedDogFood, BrandedPetFood.BrandedCatFood](
          SchemaGen.anyValueForSchema(Schema[BrandedPetFood.BrandedDogFood]).map(_._2)
        )
      ),
      testM("Recursive1 <-> Recursive2")(isomorphismLaw[TestEnvironment, Recursive1, Recursive2](Recursive1.gen))
    )

  private def isomorphismLaw[R, A: Schema, B: Schema](
    genA: Gen[Random with Sized, A]
  ): URIO[R with Random with Sized with TestConfig, TestResult] =
    check(genA) { a =>
      val AtoB      = Schema[A].migrate(Schema[B])
      val BtoA      = Schema[B].migrate(Schema[A])
      val roundTrip = AtoB.flatMap(f => f(a)).flatMap(b => BtoA.flatMap(g => g(b)))

      assert(roundTrip)(isRight(equalTo(a)))
    }

  case class Recursive1(level: Int, r: Option[Recursive1])

  object Recursive1 {
    implicit lazy val schema: Schema[Recursive1] = DeriveSchema.gen

    def genTree(depth: Int): Recursive1 =
      if (depth == 0)
        Recursive1(0, None)
      else Recursive1(depth, Some(genTree(depth - 1)))

    def gen: Gen[Random with Sized, Recursive1] =
      Gen.int(2, 10).map(genTree)
  }
  case class Recursive2(level: Int, r: Option[Recursive2])

  object Recursive2 {
    implicit lazy val schema: Schema[Recursive2] = DeriveSchema.gen

    def genTree(depth: Int): Recursive2 =
      if (depth == 0)
        Recursive2(0, None)
      else Recursive2(depth, Some(genTree(depth - 1)))

    def gen: Gen[Random with Sized, Recursive2] =
      Gen.int(2, 10).map(genTree)
  }

  sealed trait PetFood

  object PetFood {
    case class DogFood(ingredients: List[String], brand: Option[String]) extends PetFood

    object DogFood {
      implicit def schema: Schema[DogFood] = DeriveSchema.gen
    }
    case class CatFood(ingredients: List[String], brand: Option[String]) extends PetFood

    object CatFood {
      implicit def schema: Schema[CatFood] = DeriveSchema.gen
    }

    implicit def schema: Schema[PetFood] = DeriveSchema.gen
  }

  sealed trait BrandedPetFood

  object BrandedPetFood {
    case class BrandedDogFood(ingredients: List[String], brand: String) extends BrandedPetFood

    object BrandedDogFood {
      implicit def schema: Schema[BrandedDogFood] = DeriveSchema.gen
    }
    case class BrandedCatFood(ingredients: List[String], brand: String) extends BrandedPetFood

    object BrandedCatFood {
      implicit def schema: Schema[BrandedCatFood] = DeriveSchema.gen
    }
    case class BandedHamsterFood(ingredients: List[String], brand: String) extends BrandedPetFood

    object BandedHamsterFood {
      implicit def schema: Schema[BandedHamsterFood] = DeriveSchema.gen
    }

    implicit def schema: Schema[BrandedPetFood] = DeriveSchema.gen
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
    case class NamedCat(name: String, color: String, breed: String, weight: Int, favoriteFood: BrandedPetFood)
        extends NamedPet

    object NamedCat {
      implicit def schema: Schema[NamedCat] = DeriveSchema.gen
    }
    case class NamedDog(name: String, color: String, breed: String, weight: Int, favoriteFood: BrandedPetFood)
        extends NamedPet

    object NamedDog {
      implicit def schema: Schema[NamedDog] = DeriveSchema.gen
    }
    case class NamedHamster(name: String, color: String, weight: Int, favoriteFood: BrandedPetFood) extends NamedPet

    object NamedHamster {
      implicit def schema: Schema[NamedHamster] = DeriveSchema.gen
    }

    implicit def schema: Schema[NamedPet] = DeriveSchema.gen
  }
}
