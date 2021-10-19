package zio.schema

import java.math.BigInteger
import java.time.temporal.ChronoUnit
import java.time.{ DayOfWeek, MonthDay }

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.random.Random
import zio.schema.SchemaGen.{ Arity1, Arity24 }
import zio.schema.StandardType._
import zio.schema.syntax._
import zio.schema.types.Arities._
import zio.test._

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

              assertTrue(theseBytes.diffEach(thoseBytes) == expected) &&
              assertTrue(theseBytes.runPatch(expected) == Right(thoseBytes))
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

              assertTrue(theseBytes.diffEach(thoseBytes) == expected) &&
              assertTrue(theseBytes.runPatch(expected) == Right(thoseBytes))
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

              assertTrue(theseBytes.diffEach(thoseBytes) == expected) &&
              assertTrue(theseBytes.runPatch(expected) == Right(thoseBytes))
          }
        }
      ),
      suite("int")(
        testM("identical") {
          check(Gen.anyInt) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        testM("different") {
          check(Gen.anyInt <*> Gen.anyInt) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Number(left - right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("double")(
        testM("identical") {
          check(Gen.anyDouble) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        testM("different") {
          check(Gen.anyDouble <*> Gen.anyDouble) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Number(left - right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("float")(
        testM("identical") {
          check(Gen.anyFloat) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        testM("different") {
          check(Gen.anyFloat <*> Gen.anyFloat) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Number(left - right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("long")(
        testM("identical") {
          check(Gen.anyLong) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        testM("different") {
          check(Gen.anyLong <*> Gen.anyLong) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Number(left - right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("short")(
        testM("identical") {
          check(Gen.anyShort) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        testM("different") {
          check(Gen.anyShort <*> Gen.anyShort) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Number((left - right).asInstanceOf[Short])) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("char")(
        testM("identical") {
          check(Gen.anyChar) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        testM("different") {
          check(Gen.anyChar <*> Gen.anyChar) {
            case (left, right) if left != right =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Number((left - right).asInstanceOf[Char])) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("BigInteger")(
        testM("identical") {
          check(bigIntegerGen) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        testM("different") {
          check(bigIntegerGen <*> bigIntegerGen) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.BigInt(left.subtract(right))) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("BigDecimal")(
        testM("identical") {
          check(bigDecimalGen) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        testM("different") {
          check(bigDecimalGen <*> bigDecimalGen) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.BigDecimal(left.subtract(right))) &&
              assertTrue(left.runPatch(diff).map(_.compareTo(right)) == Right(0))
          }
        }
      )
    ),
    suite("string")(
      testM("identical") {
        check(Gen.anyString) { s =>
          val diff = s.diffEach(s)
          assertTrue(diff == Diff.Identical) &&
          assertTrue(s.runPatch(diff) == Right(s))
        }
      },
      testM("append character") {
        check(Gen.anyString <*> Gen.anyChar) {
          case (str, ch) =>
            val str2 = str + ch.toString
            val diff =
              Diff.Myers(Chunk.fromIterable(str.map(c => Diff.Edit.Keep(c.toString))) :+ Diff.Edit.Insert(ch.toString))

            assertTrue(str.diffEach(str2) == diff) &&
            assertTrue(str.runPatch(diff) == Right(str2))
        }
      },
      testM("different") {
        check(Gen.anyString <*> Gen.anyString) {
          case (left, right) =>
            val diff = left.diffEach(right)
            assertTrue(left.runPatch(diff) == Right(right))
        }
      }
    ),
    suite("UUID")(
      testM("identical") {
        check(Gen.anyUUID) { uuid =>
          val diff = uuid.diffEach(uuid)
          assertTrue(diff == Diff.Identical) &&
          assertTrue(uuid.runPatch(diff) == Right(uuid))
        }
      },
      testM("different") {
        check(Gen.anyUUID <*> Gen.anyUUID) {
          case (left, right) =>
            val diff = left.diffEach(right)
            assertTrue(left.runPatch(diff) == Right(right))
        }
      }
    ),
    test("boolean") {
      def boolChecker(from: Boolean, to: Boolean) = {
        val diff = from.diff(to)
        assertTrue(diff == Diff.Bool(from ^ to)) &&
        assertTrue(from.runPatch(diff) == Right(to))
      }
      boolChecker(true, true) &&
      boolChecker(false, false) &&
      boolChecker(true, false) &&
      boolChecker(false, true)
    },
    suite("ZoneId")(
      testM("identical") {
        check(Gen.anyZoneId) { x =>
          val diff = x.diff(x)
          assertTrue(diff == Diff.Identical) &&
          assertTrue(x.runPatch(diff) == Right(x))
        }
      },
      testM("different") {
        check(Gen.anyZoneId <*> Gen.anyZoneId) {
          case (left, right) =>
            val diff = left.diff(right)
            assertTrue(left.runPatch(diff) == Right(right))
        }
      }
    ),
    suite("temporal")(
      testM("Year") {
        check(Gen.anyYear <*> Gen.anyYear) {
          case (left, right) =>
            val unit = ChronoUnit.YEARS
            val diff = left.diff(right)
            assertTrue(diff == Diff.Temporal(unit.between(left, right), unit)) &&
            assertTrue(left.runPatch(diff) == Right(right))
        }
      },
      testM("YearMonth") {
        check(Gen.anyYearMonth <*> Gen.anyYearMonth) {
          case (left, right) =>
            val unit = ChronoUnit.MONTHS
            val diff = left.diff(right)
            assertTrue(diff == Diff.Temporal(unit.between(left, right), unit)) &&
            assertTrue(left.runPatch(diff) == Right(right))
        }
      },
      testM("LocalDate") {
        check(Gen.anyLocalDate <*> Gen.anyLocalDate) {
          case (left, right) =>
            val unit = ChronoUnit.DAYS
            val diff = left.diff(right)
            assertTrue(diff == Diff.Temporal(unit.between(left, right), unit)) &&
            assertTrue(left.runPatch(diff) == Right(right))
        }
      },

      //    TODO Bug -  This produces
      //    [info]         java.lang.ArithmeticException: long overflow
      //    [info]          at java.lang.Math.multiplyExact(Unknown Source)
      //    [info]          at java.time.LocalDateTime.until(Unknown Source)
      //    [info]          at java.time.temporal.ChronoUnit.between(Unknown Source)
      //    [info]          at zio.schema.Differ$.zio$schema$Differ$$$anonfun$temporal$1(Diff.scala:150)
      //    [info]          at zio.schema.Differ$$anonfun$temporal$2.apply(Diff.scala:150)
      //    [info]          at zio.schema.Differ$$anonfun$temporal$2.apply(Diff.scala:150)
      //    [info]          at zio.schema.Schema.diff(Schema.scala:69)
      //    Notes: All the temporal times with MILLIS can produce this error when doing unit.between(a, b)
      //           we should look for a different way to store time differences...

      // testM("LocalDateTime") {
      //   check(Gen.anyLocalDateTime <*> Gen.anyLocalDateTime) {
      //     case (left, right) =>
      //       val unit = ChronoUnit.MILLIS
      //       val diff = left.diff(right)
      //       assertTrue(diff == Diff.Temporal(unit.between(left, right), unit)) &&
      //       assertTrue(left.runPatch(diff) == Right(right))
      //   }
      // },

      // TODO Bug - This produces:
      //    [info]         java.time.temporal.UnsupportedTemporalTypeException: Unsupported unit: Millis
      //    [info]          at java.time.Duration.get(Unknown Source)
      //    [info]          at zio.schema.Differ$.zio$schema$Differ$$$anonfun$temporalAmount$1(Diff.scala:147)
      //    [info]          at zio.schema.Differ$$anonfun$temporalAmount$2.apply(Diff.scala:147)
      //    [info]          at zio.schema.Differ$$anonfun$temporalAmount$2.apply(Diff.scala:147)
      //    [info]          at zio.schema.Schema.diff(Schema.scala:69)
      // Notes: java.time.Duration does not support Milliseconds
      //  as per https://docs.oracle.com/javase/8/docs/api/java/time/Duration.html
      //  it supports seconds and nanoseconds

      // testM("Duration") {
      //   check(Gen.anyFiniteDuration <*> Gen.anyFiniteDuration) {
      //     case (left, right) =>
      //       val unit = ChronoUnit.MILLIS
      //       val diff = left.diff(right)
      //       assertTrue(diff == Diff.Temporal(left.get(unit) - right.get(unit), unit)) &&
      //       assertTrue(left.runPatch(diff) == Right(right))
      //   }
      // },

      testM("day of week") {
        check(Gen.elements(1, 2, 3, 4, 5, 6, 7) <*> Gen.elements(1, 2, 3, 4, 5, 6, 7)) {
          case (i1, i2) =>
            val expected = if (i1 == i2) Diff.Identical else Diff.Temporal((i2 - i1).toLong, ChronoUnit.DAYS)
            assertTrue(DayOfWeek.of(i1).diff(DayOfWeek.of(i2)) == expected) &&
            assertTrue(DayOfWeek.of(i1).runPatch(expected) == Right(DayOfWeek.of(i2)))
        }
      },
      testM("month") {
        check(Gen.anyMonth <*> Gen.anyMonth) {
          case (thisMonth, thatMonth) =>
            val expected =
              if (thisMonth == thatMonth) Diff.Identical
              else Diff.Temporal((thatMonth.getValue - thisMonth.getValue).toLong, ChronoUnit.MONTHS)
            assertTrue(thisMonth.diff(thatMonth) == expected) &&
            assertTrue(thisMonth.runPatch(expected) == Right(thatMonth))
        }
      },
      suite("month day")(
        test("leap year adjustment") {
          val expected     = Diff.MonthDays(1, 2)
          val thisMonthDay = MonthDay.of(2, 28)
          val thatMonthDay = MonthDay.of(3, 1)
          assertTrue(thisMonthDay.diff(thatMonthDay) == expected) &&
          assertTrue(thisMonthDay.runPatch(expected) == Right(thatMonthDay))
        },
        test("no leap year adjustment") {
          val expected     = Diff.MonthDays(-1, -1)
          val thisMonthDay = MonthDay.of(2, 1)
          val thatMonthDay = MonthDay.of(1, 31)
          assertTrue(thisMonthDay.diff(thatMonthDay) == expected) &&
          assertTrue(thisMonthDay.runPatch(expected) == Right(thatMonthDay))
        },
        testM("any") {
          check(Gen.anyMonthDay <*> Gen.anyMonthDay) {
            case (thisMonthDay, thatMonthDay) if thisMonthDay == thatMonthDay =>
              assertTrue(thisMonthDay.diff(thatMonthDay) == Diff.Identical) &&
              assertTrue(thisMonthDay.runPatch(Diff.Identical) == Right(thisMonthDay))
            case (thisMonthDay, thatMonthDay) =>
              val expected = Diff.MonthDays(
                ChronoUnit.DAYS.between(thisMonthDay.atYear(2001), thatMonthDay.atYear(2001)).toInt,
                ChronoUnit.DAYS.between(thisMonthDay.atYear(2000), thatMonthDay.atYear(2000)).toInt
              )
              assertTrue(thisMonthDay.diff(thatMonthDay) == expected)
            //assertTrue(thisMonthDay.runPatch(expected) == Right(thatMonthDay))
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

            val diff = ls.diffEach(rs)
            val diffSame = ls.diffEach(ls)

            assertTrue(diff == expected) &&
            assertTrue(ls.runPatch(diff) == Right(rs)) &&
            assertTrue(diffSame == Diff.Identical) &&
            assertTrue(ls.runPatch(diffSame) == Right(ls))
        }
      },
      testM("list of primitive where that list is longer") {
        check(Gen.listOfN(10)(Gen.anyLong) <*> Gen.listOfN(12)(Gen.anyLong)) {
          case (ls, rs) =>
            val expected = Diff.Sequence(
              Chunk
                .fromIterable(
                  ls.zip(rs).map(p => p._1 - p._2).map(d => if (d != 0) Diff.Number(d) else Diff.Identical)
                ) ++ Chunk(Diff.Total(rs(10), Diff.Tag.Right), Diff.Total(rs(11), Diff.Tag.Right))
            )
            val diff = ls.diffEach(rs)
            assertTrue(diff == expected) &&
            assertTrue(ls.runPatch(diff) == Right(rs))
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
            val diff = ls.diffEach(rs)
            assertTrue(diff == expected) &&
            assertTrue(ls.runPatch(diff) == Right(rs))
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

            val diff = ls.diffEach(rs)
            assertTrue(diff == expected) &&
            assertTrue(ls.runPatch(diff) == Right(rs))
        }
      }
    ),
    suite("optional")(
      testM("identical") {
        check(Gen.option(Gen.anyLong)) { x =>
          val diff = x.diff(x)
          assertTrue(diff == Diff.Identical) &&
          assertTrue(x.runPatch(diff) == Right(x))
        }
      },
      test("None to Some") {
        val left: Option[Int]  = None
        val right: Option[Int] = Some(2)
        val schemaInt          = Schema[Int]
        val schema             = Schema.Optional(schemaInt)
        val diff               = schema.diff(left, right)
        assertTrue(diff == Diff.Total(2, Diff.Tag.Right)) &&
        assertTrue(left.runPatch(diff) == Right(right))
      },
      test("Some to None") {
        val left: Option[Int]  = Some(6)
        val right: Option[Int] = None
        val schemaInt          = Schema[Int]
        val schema             = Schema.Optional(schemaInt)
        val diff               = schema.diff(left, right)
        assertTrue(diff == Diff.Total(6, Diff.Tag.Left)) &&
        assertTrue(left.runPatch(diff) == Right(right))
      },
      test("Some to Some") {
        val left: Option[Int]  = Some(6)
        val right: Option[Int] = Some(24)
        val schemaInt          = Schema[Int]
        val schema             = Schema.Optional(schemaInt)
        val diff               = schema.diff(left, right)
        assertTrue(diff == Diff.Number(-18)) &&
        assertTrue(left.runPatch(diff) == Right(right))
      },
      testM("different") {
        check(Gen.option(Gen.anyInt) <*> Gen.option(Gen.anyInt)) {
          case (left, right) =>
            val schemaInt = Schema[Int]
            val schema    = Schema.Optional(schemaInt)
            val diff      = schema.diff(left, right)
            assertTrue(left.runPatch(diff) == Right(right))
        }
      }
    ),
    suite("records")(
      testM("records with invalid structure not be comparable") {
        check(Gen.mapOf(Gen.anyString, Gen.anyInt) <*> Gen.mapOf(Gen.anyString, Gen.anyInt)) {
          case (thisMap, thatMap) =>
            val diff = Schema
              .record(Schema.Field("key", Schema[String]))
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

            val diff = thisA.diffEach(thatA)
            assertTrue(diff == expected) &&
            assertTrue(thisA.runPatch(diff) == Right(thatA))
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
            val diff = thisA.diffEach(thatA)
            assertTrue(diff == expected) &&
            assertTrue(thisA.runPatch(diff) == Right(thatA))
        }
      },
      testM("identical") {
        check(SchemaGen.anyArity) { value =>
          val diff = value.diffEach(value)
          assertTrue(diff == Diff.Identical) &&
          assertTrue(value.runPatch(diff) == Right(value))
        }
      }
    ),
    testM("tuple") {
      check(Gen.anyDouble <*> Gen.anyLong <*> Gen.anyDouble <*> Gen.anyLong) {
        case (((left1, right1), left2), right2) =>
          val tuple1 = (left1, right1)
          val tuple2 = (left2, right2)
          val diff   = tuple1.diff(tuple2)
          val expexted1 = if (left1 - left2 == 0) Diff.Identical else Diff.Number(left1 - left2)
          val expexted2 = if (right1 - right2 == 0) Diff.Identical else Diff.Number(right1 - right2)
          assertTrue(diff == Diff.Tuple(expexted1, expexted2)) &&
          assertTrue(tuple1.runPatch(diff) == Right(tuple2))
      }
    },
    suite("transform")(
      test("different") {
        val f      = (i: Int) => Right(i.toString())
        val g      = (s: String) => scala.util.Try(s.toInt).toEither.left.map(_.toString())
        val schema = Schema.Transform[Int, String](Schema[Int], f, g)
        val diff   = schema.diff("4", "6")
        val patch  = schema.patch(diff)
        assertTrue(diff == Diff.Number(-2)) &&
        assertTrue(patch.flatMap(_.apply("4")) == Right("6"))
      }
    ),
    suite("enum")(
      test("identical") {
        import Pet._
        val pet                 = Dog("Spike")
        val schema: Schema[Pet] = DeriveSchema.gen
        val diff                = schema.diff(pet, pet)
        val patch               = schema.patch(diff)
        assertTrue(diff == Diff.Identical) &&
        assertTrue(patch.flatMap(_.apply(pet)) == Right(pet))
      },
      test("different") {
        import Diff.Edit._
        import Pet._
        val pet1                = Dog("Spike")
        val pet2                = Dog("Spot")
        val schema: Schema[Pet] = DeriveSchema.gen
        val diff                = schema.diff(pet1, pet2)
        val patch               = schema.patch(diff)
        val expected = Diff.Record(ListMap("name"->
          Diff.Myers(Chunk(Keep("S"), Keep("p"), Insert("o"), Insert("t"), Delete("i"), Delete("k"), Delete("e")))
        ))
        assertTrue(diff == expected) &&
        assertTrue(patch.flatMap(_.apply(pet1)) == Right(pet2))
      }
    ),
    suite("patchLaws")(
      testM("law") {
        check(Gen.anyInt <*> Gen.anyInt) {
          case (thisA, thatA) =>
            val diff1 = thisA.diff(thatA)
            val diff2 = thatA.diff(thisA)
            assertTrue(thisA.runPatch(diff1).flatMap(_.runPatch(diff2)) == Right(thisA)) &&
            assertTrue(thatA.runPatch(diff2).flatMap(_.runPatch(diff1)) == Right(thatA))
        }
      }
    )
  )

  val bigIntegerGen: Gen[Random, BigInteger]           = Gen.anyLong.map(d => java.math.BigInteger.valueOf(d))
  val bigDecimalGen: Gen[Random, java.math.BigDecimal] = Gen.anyDouble.map(d => java.math.BigDecimal.valueOf(d))

  sealed trait Pet

  object Pet {
    case class Dog(name: String)                     extends Pet
    case class Cat(name: String)                     extends Pet
    case class Parrot(name: String, color: Int = 55) extends Pet
  }
}
