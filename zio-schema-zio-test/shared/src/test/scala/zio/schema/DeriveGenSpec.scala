package zio.schema

import zio.ZIO
import zio.schema.TestData._
import zio.test.Assertion._
import zio.test._

object DeriveGenSpec extends ZIOSpecDefault {
  override def spec: Spec[Environment, Any] = suite("DeriveGenSpec")(
    test("correctly derives Primitives") {
      for {
        unit           <- generateValue(DeriveGen.gen(unitSchema))
        string         <- generateValue(DeriveGen.gen(stringSchema))
        boolean        <- generateValue((DeriveGen.gen(booleanSchema)))
        short          <- generateValue(DeriveGen.gen(shortSchema))
        int            <- generateValue(DeriveGen.gen(intSchema))
        long           <- generateValue(DeriveGen.gen(longSchema))
        float          <- generateValue(DeriveGen.gen(floatSchema))
        double         <- generateValue(DeriveGen.gen(doubleSchema))
        binary         <- generateValue(DeriveGen.gen(binarySchema))
        char           <- generateValue(DeriveGen.gen(charSchema))
        bigDecemal     <- generateValue(DeriveGen.gen(bigDecimalSchema))
        bigInteger     <- generateValue(DeriveGen.gen(bigIntegerSchema))
        month          <- generateValue(DeriveGen.gen(monthSchema))
        monthDay       <- generateValue(DeriveGen.gen(monthDaySchema))
        period         <- generateValue(DeriveGen.gen(periodSchema))
        dayOfWeek      <- generateValue(DeriveGen.gen(dayOfWeekSchema))
        year           <- generateValue(DeriveGen.gen(yearSchema))
        yearMonth      <- generateValue(DeriveGen.gen(yearMonthSchema))
        zoneId         <- generateValue(DeriveGen.gen(zoneIdSchema))
        zoneOffset     <- generateValue(DeriveGen.gen(zoneOffsetSchema))
        instant        <- generateValue(DeriveGen.gen(instantSchema))
        localDate      <- generateValue(DeriveGen.gen(localDateSchema))
        localTime      <- generateValue(DeriveGen.gen(localTimeSchema))
        localDateTime  <- generateValue(DeriveGen.gen(localDateTimeSchema))
        offsetTime     <- generateValue(DeriveGen.gen(offsetTimeSchema))
        offsetDateTime <- generateValue(DeriveGen.gen(offsetDateTimeSchema))
        zonedDateTime  <- generateValue(DeriveGen.gen(zonedDateTimeSchema))
        uuid           <- generateValue(DeriveGen.gen(uuidSchema))
      } yield unit && string && boolean && short && int && long && float && double &&
        binary && char && bigDecemal && bigInteger && month && monthDay && period && dayOfWeek &&
        year && yearMonth && zoneId && zoneOffset && instant && localDate && localTime && localDateTime &&
        offsetTime && offsetDateTime && zonedDateTime && uuid
    },
    test("correctly derives Tuple") {
      generateValue(DeriveGen.gen(tupleSchema))
    },
    test("correctly derives Either") {
      generateValue(DeriveGen.gen(eitherSchema))
    },
    test("correctly derives Collections") {
      for {
        list <- generateValue(DeriveGen.gen(listSchema))
        map  <- generateValue(DeriveGen.gen(mapSchema))
      } yield list && map
    },
    test("correctly derives Optional") {
      generateValue(DeriveGen.gen(optionalSchema))
    },
    test("correctly derives Transform") {
      generateValue(DeriveGen.gen(transformSchema))
    },
    test("correctly derives Lazy") {
      generateValue(DeriveGen.gen(lazySchema))
    },
    test("correctly derives Enums") {
      for {
        enum2  <- generateValue(DeriveGen.gen(Enum2.schema))
        enum23 <- generateValue(DeriveGen.gen(Enum23.schema))
      } yield enum2 && enum23
    },
    test("correctly derives CaseClasses") {
      generateValue(DeriveGen.gen(CaseClass22.schema))
    }
  )

  private def generateValue[R, A](gen: Gen[R, A]): ZIO[R, Nothing, TestResult] =
    assertZIO(gen.runCollect)(isNonEmpty)
}
