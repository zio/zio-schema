package zio.schema

import java.math.BigInteger
import java.time.temporal.ChronoUnit
import java.time.{ DayOfWeek, MonthDay }

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.random.Random
import zio.schema.SchemaGen.{ Arity1, Arity24 }
import zio.schema.syntax._
import zio.test.{ DefaultRunnableSpec, Diff => _, _ }

object DiffSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("Differ")(
    suite("standard types")(
      suite("binary")(
        testM("same length") {
          check(Gen.chunkOfN(10)(Gen.anyByte) <*> Gen.chunkOfN(10)(Gen.anyByte)) {
            case (theseBytes, thoseBytes) =>
              val expected =
                if (theseBytes == thoseBytes)
                  Diff.Identical
                else
                  Diff.Sequence(
                    theseBytes
                      .zip(thoseBytes)
                      .map(b => b._1 ^ b._2)
                      .map {
                        case 0 => Diff.Identical
                        case i => Diff.Binary(i)
                      }
                  )

              assertTrue(theseBytes.diffEach(thoseBytes) == expected)
          }
        },
        testM("that is longer") {
          check(Gen.chunkOfN(10)(Gen.anyByte) <*> Gen.chunkOfN(12)(Gen.anyByte)) {
            case (theseBytes, thoseBytes) =>
              val expected =
                Diff.Sequence(
                  theseBytes
                    .zip(thoseBytes)
                    .map(b => b._1 ^ b._2)
                    .map {
                      case 0 => Diff.Identical
                      case i => Diff.Binary(i)
                    } ++ Chunk(Diff.Total(thoseBytes(10), Diff.Tag.Right), Diff.Total(thoseBytes(11), Diff.Tag.Right))
                )

              assertTrue(theseBytes.diffEach(thoseBytes) == expected)
          }
        },
        testM("this is longer") {
          check(Gen.chunkOfN(12)(Gen.anyByte) <*> Gen.chunkOfN(10)(Gen.anyByte)) {
            case (theseBytes, thoseBytes) =>
              val expected =
                Diff.Sequence(
                  theseBytes
                    .zip(thoseBytes)
                    .map(b => b._1 ^ b._2)
                    .map {
                      case 0 => Diff.Identical
                      case i => Diff.Binary(i)
                    } ++ Chunk(Diff.Total(theseBytes(10), Diff.Tag.Left), Diff.Total(theseBytes(11), Diff.Tag.Left))
                )

              assertTrue(theseBytes.diffEach(thoseBytes) == expected)
          }
        }
      ),
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
    suite("string")(
      testM("identical") {
        check(Gen.anyString) { s =>
          assertTrue(s.diffEach(s) == Diff.Identical)
        }
      },
      testM("append character") {
        check(Gen.anyString <*> Gen.anyChar) {
          case (str, ch) =>
            val expected =
              Diff.Myers(Chunk.fromIterable(str.map(c => Diff.Edit.Keep(c.toString))) :+ Diff.Edit.Insert(ch.toString))
            assertTrue(str.diffEach(str + ch.toString) == expected)
        }
      }
    ),
    suite("temporal")(
      testM("day of week") {
        check(Gen.elements(1, 2, 3, 4, 5, 6, 7) <*> Gen.elements(1, 2, 3, 4, 5, 6, 7)) {
          case (i1, i2) =>
            val expected = if (i1 == i2) Diff.Identical else Diff.Temporal((i2 - i1).toLong, ChronoUnit.DAYS)
            assertTrue(DayOfWeek.of(i1).diff(DayOfWeek.of(i2)) == expected)
        }
      },
      testM("month") {
        check(Gen.anyMonth <*> Gen.anyMonth) {
          case (thisMonth, thatMonth) =>
            val expected =
              if (thisMonth == thatMonth) Diff.Identical
              else Diff.Temporal((thatMonth.getValue - thisMonth.getValue).toLong, ChronoUnit.MONTHS)
            assertTrue(thisMonth.diff(thatMonth) == expected)
        }
      },
      suite("month day")(
        test("leap year adjustment") {
          val expected     = Diff.MonthDays(1, 2)
          val thisMonthDay = MonthDay.of(2, 28)
          val thatMonthDay = MonthDay.of(3, 1)
          assertTrue(thisMonthDay.diff(thatMonthDay) == expected)
        },
        test("no leap year adjustment") {
          val expected     = Diff.MonthDays(-1, -1)
          val thisMonthDay = MonthDay.of(2, 1)
          val thatMonthDay = MonthDay.of(1, 31)
          assertTrue(thisMonthDay.diff(thatMonthDay) == expected)
        },
        testM("any") {
          check(Gen.anyMonthDay <*> Gen.anyMonthDay) {
            case (thisMonthDay, thatMonthDay) if thisMonthDay == thatMonthDay =>
              assertTrue(thisMonthDay.diff(thatMonthDay) == Diff.Identical)
            case (thisMonthDay, thatMonthDay) =>
              val expected = Diff.MonthDays(
                ChronoUnit.DAYS.between(thisMonthDay.atYear(2001), thatMonthDay.atYear(2001)).toInt,
                ChronoUnit.DAYS.between(thisMonthDay.atYear(2000), thatMonthDay.atYear(2000)).toInt
              )
              assertTrue(thisMonthDay.diff(thatMonthDay) == expected)
          }
        }
      )
    ),
    suite("collections")(
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
      },
      testM("list of primitive where that list is longer") {
        check(Gen.listOfN(10)(Gen.anyInt) <*> Gen.listOfN(12)(Gen.anyInt)) {
          case (ls, rs) =>
            val expected = Diff.Sequence(
              Chunk
                .fromIterable(
                  ls.zip(rs).map(p => p._1 - p._2).map(d => if (d != 0) Diff.Number(d) else Diff.Identical)
                ) ++ Chunk(Diff.Total(rs(10), Diff.Tag.Right), Diff.Total(rs(11), Diff.Tag.Right))
            )
            assertTrue(Schema[List[Int]].diff(ls, rs) == expected)
        }
      },
      testM("list of primitive where this list is longer") {
        check(Gen.listOfN(12)(Gen.anyInt) <*> Gen.listOfN(10)(Gen.anyInt)) {
          case (ls, rs) =>
            val expected = Diff.Sequence(
              Chunk
                .fromIterable(
                  ls.zip(rs).map(p => p._1 - p._2).map(d => if (d != 0) Diff.Number(d) else Diff.Identical)
                ) ++ Chunk(Diff.Total(ls(10), Diff.Tag.Left), Diff.Total(ls(11), Diff.Tag.Left))
            )
            assertTrue(Schema[List[Int]].diff(ls, rs) == expected)
        }
      },
      testM("any list of primitives") {
        check(Gen.chunkOf(Gen.anyInt) <*> Gen.chunkOf(Gen.anyInt)) {
          case (ls, rs) =>
            val expected =
              if (ls == rs)
                Diff.Identical
              else
                Diff.Sequence(ls.zipAll(rs).map(p => p._1.diff(p._2)))
            assertTrue(ls.diffEach(rs) == expected)
        }
      }
    ),
    suite("records")(
      testM("records with invalid structure not be comparable") {
        check(Gen.mapOf(Gen.anyString, Gen.anyInt) <*> Gen.mapOf(Gen.anyString, Gen.anyInt)) {
          case (thisMap, thatMap) =>
            val diff = Schema
              .GenericRecord(Chunk(Schema.Field("key", Schema[String])))
              .diff(ListMap.empty ++ thisMap, ListMap.empty ++ thatMap)
            assertTrue(diff == Diff.NotComparable)
        }
      }
    ),
    suite("product type")(
      testM("arity 1") {
        check(Gen.anyInt) { i =>
          assertTrue(Arity1(i).diff(Arity1(i - 1)) == Diff.Record(ListMap("value" -> Diff.Number[Int](1))))
        }
      },
      testM("arity 2") {
        check(SchemaGen.anyArity2 <*> SchemaGen.anyArity2) {
          case (thisA, thatA) =>
            val expected =
              if (thisA == thatA)
                Diff.Identical
              else
                Diff.Record(
                  ListMap("value1" -> thisA.value1.diffEach(thatA.value1), "value2" -> thisA.value2.diff(thatA.value2))
                )
            assertTrue(thisA.diff(thatA) == expected)
        }
      },
      testM("arity greater than 22") {
        check(SchemaGen.anyArity24 <*> SchemaGen.anyArity24) {
          case (thisA, thatA) =>
            val expected =
              if (thisA == thatA)
                Diff.Identical
              else {
                Diff.Record(
                  ListMap.empty ++ Schema[Arity24]
                    .asInstanceOf[Schema.Transform[ListMap[String, _], Arity24]]
                    .codec
                    .asInstanceOf[Schema.GenericRecord]
                    .structure
                    .zipWithIndex
                    .map {
                      case (field, index) =>
                        field.label -> Differ
                          .fromSchema(field.schema)
                          .asInstanceOf[Differ[Any]](
                            thisA.asInstanceOf[Product].productElement(index),
                            thatA.asInstanceOf[Product].productElement(index)
                          )
                    }
                    .toList
                )
              }
            assertTrue(thisA.diff(thatA) == expected)
        }
      },
      testM("identical") {
        check(SchemaGen.anyArity) { value =>
          assertTrue(value.diff(value) == Diff.Identical)
        }
      }
    )
  )

  val bigIntegerGen: Gen[Random, BigInteger]           = Gen.anyLong.map(d => java.math.BigInteger.valueOf(d))
  val bigDecimalGen: Gen[Random, java.math.BigDecimal] = Gen.anyDouble.map(d => java.math.BigDecimal.valueOf(d))

}
