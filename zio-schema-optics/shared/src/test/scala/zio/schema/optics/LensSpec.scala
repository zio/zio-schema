package zio.schema.optics
import zio.optics._
import zio.schema._
import zio.test.Assertion._
import zio.test.{ Gen, Sized, TestConfig, _ }
import zio.{ Random, _ }

object LensSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("LensSpec")(
    suite("constructors")(
      test("first")(lensLaws(Gen.int.zip(Gen.int), Gen.int)(Lens.first)),
      test("second")(lensLaws(Gen.int.zip(Gen.int), Gen.int)(Lens.second)),
      test("field1")(lensLaws(TestClass.gen, Gen.int)(TestClass.field1)),
      test("field2")(lensLaws(TestClass.gen, Gen.string)(TestClass.field2)),
      test("field2")(lensLaws(TestClass.gen, Gen.long)(TestClass.field3))
    )
  )

  def lensLaws[Env <: TestConfig, Whole, Piece](genWhole: Gen[Env, Whole], genPiece: Gen[Env, Piece])(
    lens: ZioOpticsBuilder.Lens[Whole, Piece]
  ): URIO[Env, TestResult] =
    for {
      getSetIdentity <- getSetIdentity(genWhole)(lens)
      setGetIdentity <- setGetIdentity(genWhole, genPiece)(lens)
      setSetIdentity <- setSetIdentity(genWhole, genPiece)(lens)
    } yield setGetIdentity && getSetIdentity && setSetIdentity

  def getSetIdentity[Env <: TestConfig, Whole, Piece](genWhole: Gen[Env, Whole])(
    lens: ZioOpticsBuilder.Lens[Whole, Piece]
  ): URIO[Env, TestResult] =
    check(genWhole) { wholeBefore =>
      val wholeAfter = lens.get(wholeBefore).flatMap(lens.set(_)(wholeBefore))
      assert(wholeAfter)(isRight(equalTo(wholeBefore)))
    }

  def setGetIdentity[Env <: TestConfig, Whole, Piece](genWhole: Gen[Env, Whole], genPiece: Gen[Env, Piece])(
    lens: ZioOpticsBuilder.Lens[Whole, Piece]
  ): URIO[Env, TestResult] =
    check(genWhole, genPiece) { (whole, pieceBefore) =>
      val pieceAfter = lens.set(pieceBefore)(whole).flatMap(lens.get)
      assert(pieceAfter)(isRight(equalTo(pieceBefore)))
    }

  def setSetIdentity[Env <: TestConfig, Whole, Piece](genWhole: Gen[Env, Whole], genPiece: Gen[Env, Piece])(
    lens: ZioOpticsBuilder.Lens[Whole, Piece]
  ): URIO[Env, TestResult] =
    check(genWhole, genPiece) { (whole, piece) =>
      val wholeBefore = lens.set(piece)(whole)
      val wholeAfter  = lens.set(piece)(whole).flatMap(lens.set(piece))
      assert(wholeAfter)(equalTo(wholeBefore))
    }

  case class TestClass(field1: Int, field2: String, field3: Long)

  object TestClass {
    implicit val schema: Schema.CaseClass3[Int, String, Long, TestClass] = DeriveSchema.gen[TestClass]

    val gen: Gen[Random with Sized, TestClass] = for {
      f1 <- Gen.int
      f2 <- Gen.string
      f3 <- Gen.long
    } yield TestClass(f1, f2, f3)

    val (field1, field2, field3) = schema.makeAccessors(ZioOpticsBuilder)
  }

}
