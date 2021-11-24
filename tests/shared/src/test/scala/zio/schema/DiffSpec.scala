package zio.schema

import java.math.MathContext
import java.time.temporal.{ ChronoField, ChronoUnit }
import java.time.{ DayOfWeek, MonthDay }

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.SchemaGen.{ Arity1, Arity24 }
import zio.schema.StandardType._
import zio.schema.syntax._
import zio.schema.types.Arities._
import zio.test.{ Gen, _ }

object DiffSpec extends DefaultRunnableSpec {

  def spec: ZSpec[Environment, Failure] = suite("Differ")(
    suite("standard types")(
      suite("unit")(
        test("always identical") {
          check(Gen.unit <*> Gen.unit) {
            case (thisU: Unit) =>
              assertTrue(thisU.diff(thisU) == Diff.Identical)
          }
        }
      ),
      suite("binary")(
        test("same length") {
          check(Gen.chunkOfN(10)(Gen.byte) <*> Gen.chunkOfN(10)(Gen.byte)) {
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
              assertTrue(expected.patch(theseBytes) == Right(thoseBytes)) &&
              assertTrue(theseBytes.runPatch(expected) == Right(thoseBytes))
          }
        },
        test("that is longer") {
          check(Gen.chunkOfN(10)(Gen.byte) <*> Gen.chunkOfN(12)(Gen.byte)) {
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
              assertTrue(expected.patch(theseBytes) == Right(thoseBytes)) &&
              assertTrue(theseBytes.runPatch(expected) == Right(thoseBytes))
          }
        },
        test("this is longer") {
          check(Gen.chunkOfN(12)(Gen.byte) <*> Gen.chunkOfN(10)(Gen.byte)) {
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
              assertTrue(expected.patch(theseBytes) == Right(thoseBytes)) &&
              assertTrue(theseBytes.runPatch(expected) == Right(thoseBytes))
          }
        }
      ),
      suite("int")(
        test("identical") {
          check(Gen.int) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.int <*> Gen.int) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Number(left - right)) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("double")(
        test("identical") {
          check(Gen.double) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.double <*> Gen.double) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Number(left - right)) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("float")(
        test("identical") {
          check(Gen.float) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.float <*> Gen.float) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Number(left - right)) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("long")(
        test("identical") {
          check(Gen.long) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.long <*> Gen.long) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Number(left - right)) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("short")(
        test("identical") {
          check(Gen.short) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.short <*> Gen.short) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Number((left - right).asInstanceOf[Short])) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("char")(
        test("identical") {
          check(Gen.char) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.char <*> Gen.char) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Number((left - right).asInstanceOf[Char])) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("BigInteger")(
        test("identical") {
          check(StandardTypeGen.javaBigInt) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(StandardTypeGen.javaBigInt <*> StandardTypeGen.javaBigInt) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.BigInt(left.subtract(right))) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("BigDecimal")(
        test("identical") {
          check(StandardTypeGen.javaBigDecimal) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(StandardTypeGen.javaBigDecimal <*> StandardTypeGen.javaBigDecimal) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(
                diff == Diff.BigDecimal(
                  left.round(new MathContext(right.precision())).subtract(right, MathContext.UNLIMITED),
                  right.precision()
                )
              ) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      )
    ),
    suite("string")(
      test("identical") {
        check(Gen.string) { s =>
          val diff = s.diffEach(s)
          assertTrue(diff == Diff.Identical) &&
          assertTrue(diff.patch(s) == Right(s)) &&
          assertTrue(s.runPatch(diff) == Right(s))
        }
      },
      test("append character") {
        check(Gen.string <*> Gen.char) {
          case (str, ch) =>
            val str2 = str + ch.toString
            val diff =
              Diff.Myers(Chunk.fromIterable(str.map(c => Diff.Edit.Keep(c.toString))) :+ Diff.Edit.Insert(ch.toString))

            assertTrue(str.diffEach(str2) == diff) &&
            assertTrue(diff.patch(str) == Right(str2)) &&
            assertTrue(str.runPatch(diff) == Right(str2))
        }
      },
      test("different") {
        check(Gen.string <*> Gen.string) {
          case (left, right) =>
            val diff = left.diffEach(right)
            assertTrue(diff.patch(left) == Right(right)) &&
            assertTrue(left.runPatch(diff) == Right(right))
        }
      },
      test("apply diff to different string should result in error") {
        import Diff.Edit._
        val joe      = "Joe"
        val moe      = "Moe"
        val bob      = "Bob"
        val diff     = joe.diffEach(moe)
        val expected = Diff.Myers(Chunk(Insert("M"), Delete("J"), Keep("o"), Keep("e")))
        assertTrue(diff == expected) &&
        assertTrue(diff.patch(joe) == Right(moe)) &&
        assertTrue(diff.patch(moe).isLeft) &&
        assertTrue(diff.patch(bob).isLeft) &&
        assertTrue(joe.runPatch(diff) == Right(moe)) &&
        assertTrue(moe.runPatch(diff).isLeft) &&
        assertTrue(bob.runPatch(diff).isLeft)
      }
    ),
    suite("UUID")(
      test("identical") {
        check(Gen.uuid) { uuid =>
          val diff = uuid.diffEach(uuid)
          assertTrue(diff == Diff.Identical) &&
          assertTrue(diff.patch(uuid) == Right(uuid)) &&
          assertTrue(uuid.runPatch(diff) == Right(uuid))
        }
      },
      test("different") {
        check(Gen.uuid <*> Gen.uuid) {
          case (left, right) =>
            val diff = left.diffEach(right)
            assertTrue(diff.patch(left) == Right(right)) &&
            assertTrue(left.runPatch(diff) == Right(right))
        }
      }
    ),
    test("boolean") {
      def boolChecker(from: Boolean, to: Boolean) = {
        val diff = from.diff(to)
        assertTrue(diff == Diff.Bool(from ^ to)) &&
        assertTrue(diff.patch(from) == Right(to)) &&
        assertTrue(from.runPatch(diff) == Right(to))
      }
      boolChecker(true, true) &&
      boolChecker(false, false) &&
      boolChecker(true, false) &&
      boolChecker(false, true)
    },
    suite("ZoneId")(
      test("identical") {
        check(Gen.zoneId) { x =>
          val diff = x.diff(x)
          assertTrue(diff == Diff.Identical) &&
          assertTrue(diff.patch(x) == Right(x)) &&
          assertTrue(x.runPatch(diff) == Right(x))
        }
      },
      test("different") {
        check(Gen.zoneId <*> Gen.zoneId) {
          case (left, right) =>
            val diff = left.diff(right)
            assertTrue(diff.patch(left) == Right(right)) &&
            assertTrue(left.runPatch(diff) == Right(right))
        }
      }
    ),
    suite("temporal")(
      suite("Year")(
        test("identical") {
          check(Gen.year) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.year <*> Gen.year) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Temporal(List((left.getValue - right.getValue).toLong))) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("YearMonth")(
        test("identical") {
          check(Gen.yearMonth) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.yearMonth <*> Gen.yearMonth) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(
                diff == Diff.Temporal(
                  List(left.getLong(ChronoField.PROLEPTIC_MONTH) - right.getLong(ChronoField.PROLEPTIC_MONTH))
                )
              ) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("LocalDate")(
        test("identical") {
          check(Gen.localDate) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.localDate <*> Gen.localDate) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Temporal(List(left.toEpochDay - right.toEpochDay))) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("LocalTime")(
        test("identical") {
          check(Gen.localTime) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.localTime <*> Gen.localTime) {
            case (left, right) =>
              val diff = left.diff(right)
              assertTrue(diff == Diff.Temporal(List(left.toNanoOfDay() - right.toNanoOfDay()))) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("Instant")(
        test("identical") {
          check(Gen.instant) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.instant <*> Gen.instant) {
            case (left, right) =>
              val diff = left.diff(right)
              val expected =
                Diff.Temporal(List(left.getEpochSecond - right.getEpochSecond, (left.getNano - right.getNano).toLong))
              assertTrue(diff == expected) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("Duration")(
        test("identical") {
          check(Gen.finiteDuration) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.finiteDuration <*> Gen.finiteDuration) {
            case (left, right) =>
              val diff = left.diff(right)
              val expected =
                Diff.Temporal(List(left.getSeconds - right.getSeconds, (left.getNano - right.getNano).toLong))
              assertTrue(diff == expected) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("OffsetTime")(
        test("identical") {
          check(Gen.offsetTime) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.offsetTime <*> Gen.offsetTime) {
            case (left, right) =>
              val diff = left.diff(right)
              val expected = Diff.Temporal(
                List(
                  left.toLocalTime.toNanoOfDay - right.toLocalTime.toNanoOfDay,
                  (left.getOffset.getTotalSeconds - right.getOffset.getTotalSeconds).toLong
                )
              )
              assertTrue(diff == expected) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("ZoneOffsetTime")(
        test("identical") {
          check(Gen.zoneOffset) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.zoneOffset <*> Gen.zoneOffset) {
            case (left, right) =>
              val diff = left.diff(right)
              val expected = Diff.Temporal(
                List(
                  (right.getTotalSeconds - left.getTotalSeconds).toLong
                )
              )
              assertTrue(diff == expected) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("LocalDateTime")(
        test("identical") {
          check(Gen.localDateTime) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.localDateTime <*> Gen.localDateTime) {
            case (left, right) =>
              val diff = left.diff(right)
              val expected = Diff.Temporal(
                List(
                  left.toLocalDate.toEpochDay - right.toLocalDate.toEpochDay,
                  left.toLocalTime.toNanoOfDay - right.toLocalTime.toNanoOfDay
                )
              )
              assertTrue(diff == expected) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("OffsetDateTime")(
        test("identical") {
          check(Gen.offsetDateTime) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.offsetDateTime <*> Gen.offsetDateTime) {
            case (left, right) =>
              val diff = left.diff(right)
              val expected = Diff.Temporal(
                List(
                  left.toLocalDate.toEpochDay - right.toLocalDate.toEpochDay,
                  left.toLocalTime.toNanoOfDay - right.toLocalTime.toNanoOfDay,
                  (left.getOffset.getTotalSeconds - right.getOffset.getTotalSeconds).toLong
                )
              )
              assertTrue(diff == expected) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      suite("ZonedDateTime")(
        test("identical") {
          check(Gen.zonedDateTime) { x =>
            val diff = x.diff(x)
            assertTrue(diff == Diff.Identical) &&
            assertTrue(diff.patch(x) == Right(x)) &&
            assertTrue(x.runPatch(diff) == Right(x))
          }
        },
        test("different") {
          check(Gen.zonedDateTime <*> Gen.zonedDateTime) {
            case (left, right) =>
              val diff                    = left.diff(right)
              val diffZoneId: Diff        = left.getZone.getId.diffEach(right.getZone.getId)
              val diffLocalDateTime: Diff = left.toLocalDateTime.diff(right.toLocalDateTime)
              val expected                = Diff.ZonedDateTime(diffLocalDateTime, diffZoneId)
              assertTrue(diff == expected) &&
              assertTrue(diff.patch(left) == Right(right)) &&
              assertTrue(left.runPatch(diff) == Right(right))
          }
        }
      ),
      test("day of week") {
        check(Gen.elements(1, 2, 3, 4, 5, 6, 7) <*> Gen.elements(1, 2, 3, 4, 5, 6, 7)) {
          case (i1, i2) =>
            val expected = if (i1 == i2) Diff.Identical else Diff.Temporal(List[Long]((i2 - i1).toLong))
            assertTrue(DayOfWeek.of(i1).diff(DayOfWeek.of(i2)) == expected) &&
            assertTrue(expected.patch(DayOfWeek.of(i1)) == Right(DayOfWeek.of(i2))) &&
            assertTrue(DayOfWeek.of(i1).runPatch(expected) == Right(DayOfWeek.of(i2)))
        }
      },
      test("month") {
        check(Gen.month <*> Gen.month) {
          case (thisMonth, thatMonth) =>
            val expected =
              if (thisMonth == thatMonth) Diff.Identical
              else Diff.Temporal(List[Long]((thatMonth.getValue - thisMonth.getValue).toLong))
            assertTrue(thisMonth.diff(thatMonth) == expected) &&
            assertTrue(expected.patch(thisMonth) == Right(thatMonth)) &&
            assertTrue(thisMonth.runPatch(expected) == Right(thatMonth))
        }
      },
      suite("month day")(
        test("leap year adjustment") {
          val expected     = Diff.Temporal(List[Long](1L, 2L))
          val thisMonthDay = MonthDay.of(2, 28)
          val thatMonthDay = MonthDay.of(3, 1)
          assertTrue(thisMonthDay.diff(thatMonthDay) == expected) &&
          assertTrue(expected.patch(thisMonthDay) == Right(thatMonthDay)) &&
          assertTrue(thisMonthDay.runPatch(expected) == Right(thatMonthDay))
        },
        test("no leap year adjustment") {
          val expected     = Diff.Temporal(List[Long](-1L, -1L))
          val thisMonthDay = MonthDay.of(2, 1)
          val thatMonthDay = MonthDay.of(1, 31)
          assertTrue(thisMonthDay.diff(thatMonthDay) == expected) &&
          assertTrue(expected.patch(thisMonthDay) == Right(thatMonthDay)) &&
          assertTrue(thisMonthDay.runPatch(expected) == Right(thatMonthDay))
        },
        test("any") {
          check(Gen.monthDay <*> Gen.monthDay) {
            case (thisMonthDay, thatMonthDay) if thisMonthDay == thatMonthDay =>
              assertTrue(thisMonthDay.diff(thatMonthDay) == Diff.Identical) &&
                assertTrue(thisMonthDay.runPatch(Diff.Identical) == Right(thisMonthDay))
            case (thisMonthDay, thatMonthDay) =>
              val expected = Diff.Temporal(
                List[Long](
                  ChronoUnit.DAYS.between(thisMonthDay.atYear(2001), thatMonthDay.atYear(2001)),
                  ChronoUnit.DAYS.between(thisMonthDay.atYear(2000), thatMonthDay.atYear(2000))
                )
              )
              assertTrue(thisMonthDay.diff(thatMonthDay) == expected)
            // assertTrue(expected.patch(thisMonthDay) == Right(thatMonthDay)) &&
            //assertTrue(thisMonthDay.runPatch(expected) == Right(thatMonthDay))
          }
        }
      ),
      suite("period")(
        test("identical") {
          check(Gen.period) { period =>
            val diff = period.diff(period)
            assertTrue(diff == Diff.Identical)
            assertTrue(period.runPatch(diff) == Right(period))
          }
        },
        test("different") {
          check(Gen.period <*> Gen.period) {
            case (thisPeriod, thatPeriod) =>
              val diff = thisPeriod.diff(thatPeriod)
              assertTrue(
                diff == Diff.Temporal(
                  List(
                    (thisPeriod.getDays - thatPeriod.getDays).toLong,
                    (thisPeriod.getMonths - thatPeriod.getMonths).toLong,
                    (thisPeriod.getYears - thatPeriod.getYears).toLong
                  )
                )
              )
              assertTrue(thisPeriod.runPatch(diff) == Right(thatPeriod))
          }
        }
      )
    ),
    suite("collections")(
      test("list of primitives of equal length") {
        check(Gen.listOfN(10)(Gen.int) <*> Gen.listOfN(10)(Gen.int)) {
          case (ls, rs) =>
            val expected = Diff.Sequence(
              Chunk
                .fromIterable(ls.zip(rs).map(p => p._1 - p._2).map(d => if (d != 0) Diff.Number(d) else Diff.Identical))
            )

            val diff     = ls.diffEach(rs)
            val diffSame = ls.diffEach(ls)

            assertTrue(diff == expected) &&
            assertTrue(diff.patch(ls) == Right(rs)) &&
            assertTrue(ls.runPatch(diff) == Right(rs)) &&
            assertTrue(diffSame == Diff.Identical) &&
            assertTrue(diffSame.patch(ls) == Right(ls)) &&
            assertTrue(ls.runPatch(diffSame) == Right(ls))
        }
      },
      test("list of primitive where that list is longer") {
        check(Gen.listOfN(10)(Gen.long) <*> Gen.listOfN(12)(Gen.long)) {
          case (ls, rs) =>
            val expected = Diff.Sequence(
              Chunk
                .fromIterable(
                  ls.zip(rs).map(p => p._1 - p._2).map(d => if (d != 0) Diff.Number(d) else Diff.Identical)
                ) ++ Chunk(Diff.Total(rs(10), Diff.Tag.Right), Diff.Total(rs(11), Diff.Tag.Right))
            )
            val diff = ls.diffEach(rs)
            assertTrue(diff == expected) &&
            assertTrue(diff.patch(ls) == Right(rs)) &&
            assertTrue(ls.runPatch(diff) == Right(rs))
        }
      },
      test("list of primitive where this list is longer") {
        check(Gen.listOfN(12)(Gen.int) <*> Gen.listOfN(10)(Gen.int)) {
          case (ls, rs) =>
            val expected = Diff.Sequence(
              Chunk
                .fromIterable(
                  ls.zip(rs).map(p => p._1 - p._2).map(d => if (d != 0) Diff.Number(d) else Diff.Identical)
                ) ++ Chunk(Diff.Total(ls(10), Diff.Tag.Left), Diff.Total(ls(11), Diff.Tag.Left))
            )
            val diff = ls.diffEach(rs)
            assertTrue(diff == expected) &&
            assertTrue(diff.patch(ls) == Right(rs)) &&
            assertTrue(ls.runPatch(diff) == Right(rs))
        }
      },
      test("any list of primitives") {
        check(Gen.chunkOf(Gen.int) <*> Gen.chunkOf(Gen.int)) {
          case (ls, rs) =>
            val expected =
              if (ls == rs)
                Diff.Identical
              else
                Diff.Sequence(ls.zipAll(rs).map(p => p._1.diff(p._2)))

            val diff = ls.diffEach(rs)
            assertTrue(diff == expected) &&
            assertTrue(diff.patch(ls) == Right(rs)) &&
            assertTrue(ls.runPatch(diff) == Right(rs))
        }
      }
    ),
    suite("map")(
      test("identical") {
        check(Gen.mapOfN(5)(Gen.int, Gen.long)) { map =>
          assertTrue(Schema.map[Int, Long].diff(map, map) == Diff.Identical)
        }
      },
      test("total diffs for non-shared keys") {
        check(Gen.mapOfN(5)(Gen.int, Gen.long)) {
          case map =>
            val newKey   = map.keySet.sum
            val newValue = 0L
            val left     = map + (newKey -> newValue)
            val right    = map
            assertTrue(left.diff(right) == Diff.Map(Chunk(newKey -> Diff.Total(newValue, Diff.Tag.Left))))
            assertTrue(right.diff(left) == Diff.Map(Chunk(newKey -> Diff.Total(newValue, Diff.Tag.Right))))

            val rightPatch = right.diff(left)
            assertTrue(right.runPatch(rightPatch) == Right(left))

            val leftPatch = left.diff(right)
            assertTrue(left.runPatch(leftPatch) == Right(left))
        }
      },
      test("diffs for shard keys") {
        check(Gen.mapOfN(5)(Gen.int, Gen.long)) {
          case map =>
            val left  = map.map { case (k, v) => (k, v + 1) }
            val right = map
            val expected =
              Diff.Map(
                Chunk.fromIterable(map.map {
                  case (k, _) =>
                    k -> Diff.Number(1L)
                })
              )

            val patch = left.diff(right)
            assertTrue(
              patch match {
                case Diff.Map(diffs) =>
                  Map(diffs: _*) == Map(expected.differences: _*)
                case _ => false
              }
            )

            assertTrue(
              left.runPatch(patch) == Right(right)
            )
        }
      }
    ),
    suite("optional")(
      test("identical") {
        check(Gen.option(Gen.long)) { x =>
          val diff = x.diff(x)
          assertTrue(diff == Diff.Identical) &&
          assertTrue(diff.patch(x) == Right(x)) &&
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
        assertTrue(diff.patch(left) == Right(right)) &&
        assertTrue(left.runPatch(diff) == Right(right))
      },
      test("Some to None") {
        val left: Option[Int]  = Some(6)
        val right: Option[Int] = None
        val schemaInt          = Schema[Int]
        val schema             = Schema.Optional(schemaInt)
        val diff               = schema.diff(left, right)
        assertTrue(diff == Diff.Total(6, Diff.Tag.Left)) &&
        assertTrue(diff.patch(left) == Right(right)) &&
        assertTrue(left.runPatch(diff) == Right(right))
      },
      test("Some to Some") {
        val left: Option[Int]  = Some(6)
        val right: Option[Int] = Some(24)
        val schemaInt          = Schema[Int]
        val schema             = Schema.Optional(schemaInt)
        val diff               = schema.diff(left, right)
        assertTrue(diff == Diff.Number(-18)) &&
        assertTrue(diff.patch(left) == Right(right)) &&
        assertTrue(left.runPatch(diff) == Right(right))
      },
      test("different") {
        check(Gen.option(Gen.int) <*> Gen.option(Gen.int)) {
          case (left, right) =>
            val schemaInt = Schema[Int]
            val schema    = Schema.Optional(schemaInt)
            val diff      = schema.diff(left, right)
            assertTrue(diff.patch(left) == Right(right)) &&
            assertTrue(left.runPatch(diff) == Right(right))
        }
      }
    ),
    suite("records")(
      test("records with invalid structure not be comparable") {
        check(Gen.mapOf(Gen.string, Gen.int) <*> Gen.mapOf(Gen.string, Gen.int)) {
          case (thisMap, thatMap) =>
            val diff = Schema
              .record(Schema.Field("key", Schema[String]))
              .diff(ListMap.empty ++ thisMap, ListMap.empty ++ thatMap)
            assertTrue(diff == Diff.NotComparable)
        }
      }
    ),
    suite("product type")(
      test("arity 1") {
        check(Gen.int) { i =>
          assertTrue(Arity1(i).diff(Arity1(i - 1)) == Diff.Record(ListMap("value" -> Diff.Number[Int](1))))
        }
      },
      test("arity 2") {
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
            assertTrue(diff.patch(thisA) == Right(thatA)) &&
            assertTrue(thisA.runPatch(diff) == Right(thatA))
        }
      },
      test("arity greater than 22") {
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
            assertTrue(diff.patch(thisA) == Right(thatA)) &&
            assertTrue(thisA.runPatch(diff) == Right(thatA))
        }
      },
      test("identical") {
        check(SchemaGen.anyArity) { value =>
          val diff = value.diffEach(value)
          assertTrue(diff == Diff.Identical) &&
          assertTrue(diff.patch(value) == Right(value)) &&
          assertTrue(value.runPatch(diff) == Right(value))
        }
      },
      test("error case") {
        import Diff.Edit._
        val joe  = Person("Joe", 11)
        val moe  = Person("Moe", 15)
        val bob  = Person("Bob", 11)
        val diff = Schema[Person].diff(joe, moe)
        val expected = Diff.Record(
          ListMap(
            "name" -> Diff.Myers(Chunk(Insert("M"), Delete("J"), Keep("o"), Keep("e"))),
            "age"  -> Diff.Number(-4)
          )
        )
        assertTrue(diff == expected) &&
        assertTrue(diff.patch(joe) == Right(moe)) &&
        assertTrue(diff.patch(moe).isLeft) &&
        assertTrue(diff.patch(bob).isLeft) &&
        assertTrue(Schema[Person].patch(joe, diff) == Right(moe)) &&
        assertTrue(Schema[Person].patch(moe, diff).isLeft) &&
        assertTrue(Schema[Person].patch(bob, diff).isLeft)
      }
    ),
    suite("tuple")(
      test("success") {
        check(Gen.double <*> Gen.long <*> Gen.double <*> Gen.long) {
          case (left1, right1, left2, right2) =>
            val tuple1    = (left1, right1)
            val tuple2    = (left2, right2)
            val diff      = tuple1.diff(tuple2)
            val expexted1 = if (left1 - left2 == 0) Diff.Identical else Diff.Number(left1 - left2)
            val expexted2 = if (right1 - right2 == 0) Diff.Identical else Diff.Number(right1 - right2)
            assertTrue(diff == Diff.Tuple(expexted1, expexted2)) &&
            assertTrue(diff.patch(tuple1) == Right(tuple2)) &&
            assertTrue(tuple1.runPatch(diff) == Right(tuple2))
        }
      },
      test("error case") {
        import Diff.Edit._
        val joe      = ("Joe", 11)
        val moe      = ("Moe", 15)
        val bob      = ("Bob", 11)
        val diff     = joe.diffEach(moe)
        val expected = Diff.Tuple(Diff.Myers(Chunk(Insert("M"), Delete("J"), Keep("o"), Keep("e"))), Diff.Number(-4))
        assertTrue(diff == expected) &&
        assertTrue(diff.patch(joe) == Right(moe)) &&
        assertTrue(diff.patch(moe).isLeft) &&
        assertTrue(diff.patch(bob).isLeft) &&
        assertTrue(joe.runPatch(diff) == Right(moe)) &&
        assertTrue(moe.runPatch(diff).isLeft) &&
        assertTrue(bob.runPatch(diff).isLeft)
      }
    ),
    suite("transform")(
      test("different") {
        val f               = (i: Int) => Right(i.toString())
        val g               = (s: String) => scala.util.Try(s.toInt).toEither.left.map(_.toString())
        implicit val schema = Schema.Transform[Int, String](Schema[Int], f, g, Chunk.empty)
        val diff            = schema.diff("4", "6")
        assertTrue(diff == Diff.Number(-2)) &&
        assertTrue(diff.patch("4") == Right("6")) &&
        assertTrue(schema.patch("4", diff) == Right("6"))
      }
    ),
    suite("either")(
      test("right identical") {
        val x: Either[Int, Int] = Right(10)
        val diff                = x.diff(x)
        assertTrue(diff == Diff.Either(Diff.Identical, Diff.Tag.Right)) &&
        assertTrue(diff.patch(x) == Right(x)) &&
        assertTrue(x.runPatch(diff) == Right(x))
      },
      test("right different") {
        val from: Either[Int, Int] = Right(10)
        val to: Either[Int, Int]   = Right(20)
        val diff                   = from.diff(to)
        assertTrue(diff == Diff.Either(Diff.Number(-10), Diff.Tag.Right)) &&
        assertTrue(diff.patch(from) == Right(to)) &&
        assertTrue(from.runPatch(diff) == Right(to))
      },
      test("left identical") {
        val x: Either[Long, Int] = Left(10L)
        val diff                 = x.diff(x)
        assertTrue(diff == Diff.Either(Diff.Identical, Diff.Tag.Left)) &&
        assertTrue(diff.patch(x) == Right(x)) &&
        assertTrue(x.runPatch(diff) == Right(x))
      },
      test("left different") {
        val from: Either[Long, Int] = Left(10L)
        val to: Either[Long, Int]   = Left(20L)
        val diff                    = from.diff(to)
        assertTrue(diff == Diff.Either(Diff.Number(-10L), Diff.Tag.Left)) &&
        assertTrue(diff.patch(from) == Right(to)) &&
        assertTrue(from.runPatch(diff) == Right(to))
      },
      test("left to right error") {
        val from: Either[Int, Int] = Left(10)
        val to: Either[Int, Int]   = Right(20)
        val diff                   = from.diff(to)
        assertTrue(diff == Diff.NotComparable) &&
        assertTrue(diff.patch(from).isLeft) &&
        assertTrue(from.runPatch(diff).isLeft)
      },
      test("right to left error") {
        val from: Either[Int, Int] = Right(10)
        val to: Either[Int, Int]   = Left(20)
        val diff                   = from.diff(to)
        assertTrue(diff == Diff.NotComparable) &&
        assertTrue(diff.patch(from).isLeft) &&
        assertTrue(from.runPatch(diff).isLeft)
      },
      test("right - apply diff to different string should result in error") {
        import Diff.Edit._
        val joe: Either[Int, String] = Right("Joe")
        val moe: Either[Int, String] = Right("Moe")
        val bob: Either[Int, String] = Right("Bob")
        val diff                     = joe.diffEach(moe)
        val expected                 = Diff.Either(Diff.Myers(Chunk(Insert("M"), Delete("J"), Keep("o"), Keep("e"))), Diff.Tag.Right)
        assertTrue(diff == expected) &&
        assertTrue(diff.patch(joe) == Right(moe)) &&
        assertTrue(diff.patch(moe).isLeft) &&
        assertTrue(diff.patch(bob).isLeft) &&
        assertTrue(joe.runPatch(diff) == Right(moe)) &&
        assertTrue(moe.runPatch(diff).isLeft) &&
        assertTrue(bob.runPatch(diff).isLeft)
      },
      test("left - apply diff to different string should result in error") {
        import Diff.Edit._
        val joe: Either[String, Long] = Left("Joe")
        val moe: Either[String, Long] = Left("Moe")
        val bob: Either[String, Long] = Left("Bob")
        val diff                      = joe.diffEach(moe)
        val expected                  = Diff.Either(Diff.Myers(Chunk(Insert("M"), Delete("J"), Keep("o"), Keep("e"))), Diff.Tag.Left)
        assertTrue(diff == expected) &&
        assertTrue(diff.patch(joe) == Right(moe)) &&
        assertTrue(diff.patch(moe).isLeft) &&
        assertTrue(diff.patch(bob).isLeft) &&
        assertTrue(joe.runPatch(diff) == Right(moe)) &&
        assertTrue(moe.runPatch(diff).isLeft) &&
        assertTrue(bob.runPatch(diff).isLeft)
      }
    ),
    suite("enumN")(
      test("identical") {
        import Pet._
        val pet  = Dog("Spike")
        val diff = Schema[Pet].diff(pet, pet)
        assertTrue(diff == Diff.Identical) &&
        assertTrue(diff.patch(pet) == Right(pet)) &&
        assertTrue(schema.patch(pet, diff) == Right(pet))
      },
      test("different") {
        import Diff.Edit._
        import Pet._
        val pet1 = Dog("Spike")
        val pet2 = Dog("Spot")
        val diff = Schema[Pet].diff(pet1, pet2)
        val expected = Diff.Record(
          ListMap(
            "name" ->
              Diff.Myers(Chunk(Keep("S"), Keep("p"), Insert("o"), Insert("t"), Delete("i"), Delete("k"), Delete("e")))
          )
        )
        assertTrue(diff == expected) &&
        assertTrue(diff.patch(pet1) == Right(pet2)) &&
        assertTrue(schema.patch(pet1, diff) == Right(pet2))
      },
      test("different enumN types - NotComparable") {
        import Pet._
        val pet1 = Dog("Spike")
        val pet2 = Cat("Spot")
        val diff = Schema[Pet].diff(pet1, pet2)
        assertTrue(diff == Diff.NotComparable) &&
        assertTrue(diff.patch(pet1).isLeft) &&
        assertTrue(schema.patch(pet1, diff).isLeft)
      },
      test("error case") {
        import Diff.Edit._
        import Pet._
        val pet1 = Dog("Spike")
        val pet2 = Dog("Spot")
        val pet3 = Dog("Zeeke")
        val diff = schema.diff(pet1, pet2)
        val expected = Diff.Record(
          ListMap(
            "name" ->
              Diff.Myers(Chunk(Keep("S"), Keep("p"), Insert("o"), Insert("t"), Delete("i"), Delete("k"), Delete("e")))
          )
        )
        assertTrue(diff == expected) &&
        assertTrue(diff.patch(pet1) == Right(pet2)) &&
        assertTrue(diff.patch(pet2).isLeft) &&
        assertTrue(diff.patch(pet3).isLeft) &&
        assertTrue(schema.patch(pet1, diff) == Right(pet2)) &&
        assertTrue(schema.patch(pet2, diff).isLeft) &&
        assertTrue(schema.patch(pet3, diff).isLeft)
      }
    ),
    suite("patchLaws")(
      test("law") {
        check(Gen.int <*> Gen.int) {
          case (thisA, thatA) =>
            val diff1 = thisA.diff(thatA)
            val diff2 = thatA.diff(thisA)
            assertTrue(diff1.patch(thisA).flatMap(diff2.patch(_)) == Right(thisA)) &&
            assertTrue(diff2.patch(thatA).flatMap(diff1.patch(_)) == Right(thatA)) &&
            assertTrue(thisA.runPatch(diff1).flatMap(_.runPatch(diff2)) == Right(thisA)) &&
            assertTrue(thatA.runPatch(diff2).flatMap(_.runPatch(diff1)) == Right(thatA))
        }
      },
      test("identity law") {
        check(SchemaGen.anySchemaAndValue) {
          case (schema, value) =>
            val diff = schema.diff(value, value)
            assertTrue(schema.diff(value, value) == Diff.Identical)
            assertTrue(diff.patch(value)(schema) == Right(value))
        }
      }
    )
  )

  sealed trait Pet

  object Pet {
    case class Dog(name: String) extends Pet

    object Dog {
      implicit lazy val schema: Schema[Dog] = DeriveSchema.gen[Dog]
    }
    case class Cat(name: String) extends Pet

    object Cat {
      implicit lazy val schema: Schema[Cat] = DeriveSchema.gen
    }
    case class Parrot(name: String, color: Int = 55) extends Pet

    object Parrot {
      implicit val schema: Schema[Parrot] = DeriveSchema.gen
    }

    implicit lazy val schema: Schema[Pet] = DeriveSchema.gen
  }

  case class Person(name: String, age: Int)

  object Person {
    implicit lazy val schema: Schema[Person] = DeriveSchema.gen
  }
}
