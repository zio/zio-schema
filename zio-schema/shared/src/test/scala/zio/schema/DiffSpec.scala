package zio.schema

import java.math.BigInteger

import zio.Chunk
import zio.random.Random
import zio.schema.syntax._
import zio.test.{ DefaultRunnableSpec, Diff => _, _ }

object DiffSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("Differ")(
    suite("standard types")(
      testM("int") {
        check(Gen.anyInt <*> Gen.anyInt) {
          case (left, right) =>
            assertTrue(left.diff(right) == Diff.Number(left - right))
            assertTrue(left.diff(left) == Diff.Identical)
        }
      },
      testM("double") {
        check(Gen.anyDouble <*> Gen.anyDouble) {
          case (left, right) =>
            assertTrue(left.diff(right) == Diff.Number(left - right))
            assertTrue(left.diff(left) == Diff.Identical)
        }
      },
      testM("float") {
        check(Gen.anyFloat <*> Gen.anyFloat) {
          case (left, right) =>
            assertTrue(left.diff(right) == Diff.Number(left - right))
            assertTrue(left.diff(left) == Diff.Identical)
        }
      },
      testM("long") {
        check(Gen.anyLong <*> Gen.anyLong) {
          case (left, right) =>
            assertTrue(left.diff(right) == Diff.Number(left - right))
            assertTrue(left.diff(left) == Diff.Identical)
        }
      },
      testM("short") {
        check(Gen.anyShort <*> Gen.anyShort) {
          case (left, right) =>
            assertTrue(left.diff(right) == Diff.Number(left - right))
            assertTrue(left.diff(left) == Diff.Identical)
        }
      },
      testM("BigInteger") {
        check(bigIntegerGen <*> bigIntegerGen) {
          case (left, right) =>
            assertTrue(left.diff(right) == Diff.BigInt(left.subtract(right)))
            assertTrue(left.diff(left) == Diff.Identical)
        }
      },
      testM("BigDecimal") {
        check(bigDecimalGen <*> bigDecimalGen) {
          case (left, right) =>
            assertTrue(left.diff(right) == Diff.BigDecimal(left.subtract(right)))
            assertTrue(left.diff(left) == Diff.Identical)
        }
      }
    ),
    suite("collections") {
      testM("list of primitives of equal length") {
        check(Gen.listOfN(10)(Gen.anyInt) <*> Gen.listOfN(10)(Gen.anyInt)) {
          case (ls, rs) =>
            val expected = Diff.Sequence(
              Chunk
                .fromIterable(ls.zip(rs).map(p => p._1 - p._2).map(d => if (d != 0) Diff.Number(d) else Diff.Identical))
            )
            assertTrue(Schema[List[Int]].diff(ls, rs) == expected)
            assertTrue(Schema[List[Int]].diff(ls, ls) == Diff.Identical)
        }
      }
    }
  )

  val bigIntegerGen: Gen[Random, BigInteger]           = Gen.anyLong.map(d => java.math.BigInteger.valueOf(d))
  val bigDecimalGen: Gen[Random, java.math.BigDecimal] = Gen.anyDouble.map(d => java.math.BigDecimal.valueOf(d))

}
