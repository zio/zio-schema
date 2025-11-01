package zio.schema.optics

import zio._
import zio.optics._
import zio.schema._
import zio.test.Assertion._
import zio.test._

object LensSpec extends ZIOSpecDefault {

  def spec: Spec[Environment, Any] =
    suite("LensSpec")(
      suite("constructors")(
        test("first")(lensLaws(Gen.int.zip(Gen.int), Gen.int)(Lens.first)),
        test("second")(lensLaws(Gen.int.zip(Gen.int), Gen.int)(Lens.second)),
        test("field1")(lensLaws(TestClass.gen, Gen.int)(TestClass.field1)),
        test("field2")(lensLaws(TestClass.gen, Gen.string)(TestClass.field2)),
        test("field2")(lensLaws(TestClass.gen, Gen.long)(TestClass.field3))
      )
    )

  def lensLaws[F, Env <: TestConfig, Whole, Piece](genWhole: Gen[Env, Whole], genPiece: Gen[Env, Piece])(
    lens: ZioOpticsBuilder.Lens[F, Whole, Piece]
  ): URIO[Env, TestResult] =
    for {
      getSetIdentity <- getSetIdentity(genWhole)(lens)
      setGetIdentity <- setGetIdentity(genWhole, genPiece)(lens)
      setSetIdentity <- setSetIdentity(genWhole, genPiece)(lens)
    } yield setGetIdentity && getSetIdentity && setSetIdentity

  def getSetIdentity[F, Env <: TestConfig, Whole, Piece](genWhole: Gen[Env, Whole])(
    lens: ZioOpticsBuilder.Lens[F, Whole, Piece]
  ): URIO[Env, TestResult] =
    check(genWhole) { wholeBefore =>
      val wholeAfter = lens.get(wholeBefore).flatMap(lens.set(_)(wholeBefore))
      assert(wholeAfter)(isRight(equalTo(wholeBefore)))
    }

  def setGetIdentity[F, Env <: TestConfig, Whole, Piece](genWhole: Gen[Env, Whole], genPiece: Gen[Env, Piece])(
    lens: ZioOpticsBuilder.Lens[F, Whole, Piece]
  ): URIO[Env, TestResult] =
    check(genWhole, genPiece) { (whole, pieceBefore) =>
      val pieceAfter = lens.set(pieceBefore)(whole).flatMap(lens.get)
      assert(pieceAfter)(isRight(equalTo(pieceBefore)))
    }

  def setSetIdentity[F, Env <: TestConfig, Whole, Piece](genWhole: Gen[Env, Whole], genPiece: Gen[Env, Piece])(
    lens: ZioOpticsBuilder.Lens[F, Whole, Piece]
  ): URIO[Env, TestResult] =
    check(genWhole, genPiece) { (whole, piece) =>
      val wholeBefore = lens.set(piece)(whole)
      val wholeAfter  = lens.set(piece)(whole).flatMap(lens.set(piece))
      assert(wholeAfter)(equalTo(wholeBefore))
    }

  case class TestClass(field1: Int, field2: String, field3: Long)

  object TestClass {
    implicit val schema: Schema.CaseClass3[Int, String, Long, TestClass] = DeriveSchema.gen[TestClass]

    val gen: Gen[Sized, TestClass] = for {
      f1 <- Gen.int
      f2 <- Gen.string
      f3 <- Gen.long
    } yield TestClass(f1, f2, f3)

    val (field1, field2, field3) = schema.makeAccessors(ZioOpticsBuilder)
  }

}
