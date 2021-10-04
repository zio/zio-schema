package zio.schema.optics
import zio._
import zio.optics._
import zio.random.Random
import zio.schema._
import zio.test.Assertion._
import zio.test._

object LensSpec extends DefaultRunnableSpec {
  import SchemaDerivation._

  def spec: ZSpec[Environment, Failure] = suite("LensSpec")(
    suite("constructors")(
      testM("first")(lensLaws(Gen.anyInt.zip(Gen.anyInt), Gen.anyInt)(Lens.first)),
      testM("second")(lensLaws(Gen.anyInt.zip(Gen.anyInt), Gen.anyInt)(Lens.second)),
      testM("field1")(lensLaws(testClassGen, Gen.anyInt)(field1)),
      testM("field2")(lensLaws(testClassGen, Gen.anyString)(field2)),
      testM("field2")(lensLaws(testClassGen, Gen.anyLong)(field3))
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


      val testClassGen: Gen[Random with Sized, TestClass] = for {
      f1 <- Gen.anyInt
      f2 <- Gen.anyString
      f3 <- Gen.anyLong
    } yield TestClass(f1, f2, f3)

  // case class TestClass(field1: Int, field2: String, field3: Long)

  val s = gen[TestClass]

  case class Foo(a: Int, b: String)
  // object Foo {
  //   implicit val schema =  gen[Foo]
  // }
  case class Bar(a: Foo, b: String)

  val s1 = {
    val s = gen[Bar]
    println(s"Schema:\n$s")
    s
  }

  val (field1, field2, field3) = s.makeAccessors(ZioOpticsBuilder)

  // object TestClass {
  //   implicit val schema: Schema.CaseClass3[Int, String, Long, TestClass] =
  //     DeriveSchema.gen[TestClass].asInstanceOf[Schema.CaseClass3[Int, String, Long, TestClass]]

  //   val s: Schema.CaseClass3[Int, String, Long, TestClass] = 
  //     Schema.CaseClass3[Int,String,Long,TestClass](
  //       annotations = Chunk.empty,
  //       field1 = Schema.Field("field1", Schema.primitive(StandardType.IntType)),
  //       field2 = Schema.Field("field2", Schema.primitive(StandardType.StringType)),
  //       field3 = Schema.Field("field3", Schema.primitive(StandardType.LongType)),
  //       (_1: Int, _2: String, _3: Long) => TestClass(_1,_2,_3),
  //       (t: TestClass) => t.field1,
  //       (t: TestClass) => t.field2,
  //       (t: TestClass) => t.field3,
  //     )

  //   val gen: Gen[Random with Sized, TestClass] = for {
  //     f1 <- Gen.anyInt
  //     f2 <- Gen.anyString
  //     f3 <- Gen.anyLong
  //   } yield TestClass(f1, f2, f3)

  //   val (field1, field2, field3) = s.makeAccessors(ZioOpticsBuilder)
  // }

}
