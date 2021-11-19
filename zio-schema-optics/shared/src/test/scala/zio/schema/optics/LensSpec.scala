package zio.schema.optics
import zio._
import zio.optics._
import zio.random.Random
import zio.schema._
import zio.test.Assertion._
import zio.test._

object LensSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("LensSpec")(
    suite("constructors")(
      testM("first")(lensLaws(Gen.anyInt.zip(Gen.anyInt), Gen.anyInt)(Lens.first)),
      testM("second")(lensLaws(Gen.anyInt.zip(Gen.anyInt), Gen.anyInt)(Lens.second)),
      testM("field1")(lensLaws(TestClass.gen, Gen.anyInt)(TestClass.field1)),
      testM("field2")(lensLaws(TestClass.gen, Gen.anyString)(TestClass.field2)),
      testM("field2")(lensLaws(TestClass.gen, Gen.anyLong)(TestClass.field3))
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
      f1 <- Gen.anyInt
      f2 <- Gen.anyString
      f3 <- Gen.anyLong
    } yield TestClass(f1, f2, f3)

    val (field1, field2, field3) = schema.makeAccessors(ZioOpticsBuilder)
  }

}
