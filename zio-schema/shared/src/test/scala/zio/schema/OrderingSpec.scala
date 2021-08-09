package zio.schema

//import zio._
import zio.URIO
import zio.random._
import zio.schema.Schema.Primitive
import zio.schema.SchemaGen.{SchemaTest, schemasAndGens}
import zio.test.Assertion._
import zio.test._
//import zio.console._
//import zio.schema.SchemaSpec.{schemaUnit, suite, test}
import zio.schema.DeriveSchema._

object OrderingSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[Environment, Any] =
    suite("should correctly sort")(

      suite("primitives")(primitiveOrderingTests:_*),

      testM("tuples"){
        checkM(sortedTuples)(input => {
          val schema = Schema[(Int,Double)]
//          println(s"tuple schema: ${schema}")
          val result =
            shuffle(input)
              .map(_.sorted(schema.ordering))
          assertM(result)(equalTo(input))
        })
      },
      testM("nested tuples"){
        checkM(sortedTuplesNested)(input => {
          val schema = Schema[((Int,Short),Double)]
//          println(s"nested tuple schema: ${schema}")
          val result =
            shuffle(input)
              .map(_.sorted(schema.ordering))
          assertM(result)(equalTo(input))
        })
      },

      testM("records"){
        checkM(sortedRecords)(input => {
          val schema = Schema[Rec]
          val result = shuffle(input).map(_.sorted(schema.ordering))
          assertM(result)(equalTo(input))
        })
      },

      testM("either"){
        checkM(sortedEithers)(input => {
          val schema = Schema[Either[Int,String]]
//          println(s"either schema: ${schema}")
          val result = shuffle(input).map(_.sorted(schema.ordering))
          assertM(result)(equalTo(input))
        })
      },

      testM("option"){
        checkM(sortedOptions)(input => {
          val schema = DeriveSchema.gen[Option[String]]
//          println(s"option schema: ${schema}")
          val result = shuffle(input).map(_.sorted(schema.ordering))
          assertM(result)(equalTo(input))
        })
      }
    )

  val primitiveOrderingTests: List[ZSpec[Sized with Random with TestConfig, Nothing]] = schemasAndGens.map {
    case SchemaTest(name, schema, gen) =>
      testM(s"$name") {
        primitiveOrderingLaw(gen, schema)
      }
  }

  private def primitiveOrderingLaw[R,A](gen:Gen[R,A], standardType:StandardType[A]): URIO[R with Random with TestConfig, TestResult] = {
    check(Gen.listOfN(100)(gen)) { lst =>
      val schema = Primitive(standardType)
      val schemaOrdering = schema.ordering
      assert(lst.sorted(schemaOrdering))(equalTo(lst.sorted(standardType)))
    }
  }

  def sortedEithers:Gen[Random with Sized,List[Either[Int,String]]] = {
    for {
      x <- Gen.listOf(Gen.anyInt)
      y <- Gen.listOf(Gen.anyString)
    } yield ( x.sorted.map(Left(_)) ++ y.sorted.map(Right(_)) )
  }

  case class Rec(s:Short, i:Int, d:Double)

  def sortedRecords:Gen[Random with Sized, List[Rec]] = {
    for{
      shorts <- Gen.listOf(Gen.anyShort)
      ints <- Gen.listOf(Gen.anyInt)
      doubles <- Gen.listOf(Gen.anyDouble)
    } yield {
      for{
        s <- shorts.sorted
        i <- ints.sorted
        d <- doubles.sorted
      } yield Rec(s,i,d)
    }
  }

  def sortedTuplesNested:Gen[Random with Sized, List[((Int,Short),Double)]] = {
    for{
      doubles <- Gen.listOf(Gen.anyDouble)
      ints <- Gen.listOf(Gen.anyInt)
      ss <- Gen.listOf(Gen.anyShort)
    } yield {
      for{
        i <- ints.sorted
        s <- ss.sorted
        d <- doubles.sorted
      } yield ((i,s),d)
    }
  }

  def sortedTuples:Gen[Random with Sized, List[(Int,Double)]] = {
    for{
      doubles <- Gen.listOf(Gen.anyDouble)
      ints <- Gen.listOf(Gen.anyInt)
    } yield {
      for{
        i <- ints.sorted
        d <- doubles.sorted
      } yield (i,d)
    }
  }

  def sortedOptions:Gen[Random with Sized,List[Option[String]]] = {
    for {
      x <- Gen.listOf(Gen.elements(None))
      y <- Gen.listOf(Gen.anyString)
    } yield x ++ y.sorted.map(Some(_))
  }



}
