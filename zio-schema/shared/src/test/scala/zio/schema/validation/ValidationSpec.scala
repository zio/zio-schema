package zio.schema.validation

import java.time.format.DateTimeFormatter

import scala.util.Try

import zio.Scope
import zio.test._

object ValidationSpec extends ZIOSpecDefault {
  import zio.schema.validation.ValidationSpec.Hour._
  import zio.schema.validation.ValidationSpec.Minute._
  import zio.schema.validation.ValidationSpec.Second._
  import zio.schema.validation.ValidationSpec.Fraction._
  import zio.schema.validation.ValidationSpec.AmPm._

  def spec: Spec[Environment with TestEnvironment with Scope, Any] = suite("ValidationSpec")(
    test("Greater than") {
      val validation = Validation.greaterThan(4)

      assertTrue(validation.validate(4).isLeft) &&
      assertTrue(validation.validate(5).isRight) &&
      assertTrue(validation.validate(6).isRight) &&
      assertTrue(validation.validate(3).isLeft)
    },
    test("Less than") {
      val validation = Validation.lessThan(4)

      assertTrue(validation.validate(3).isRight) &&
      assertTrue(validation.validate(2).isRight) &&
      assertTrue(validation.validate(5).isLeft) &&
      assertTrue(validation.validate(4).isLeft)
    },
    test("Equal to") {
      val validation = Validation.equalTo(3)

      assertTrue(validation.validate(3).isRight) &&
      assertTrue(validation.validate(5).isLeft) &&
      assertTrue(validation.validate(2).isLeft)
    },
    test("MinLength") {
      val validation = Validation.minLength(4)

      assertTrue(validation.validate("hello").isRight) &&
      assertTrue(validation.validate("Todd").isRight) &&
      assertTrue(validation.validate("how").isLeft) &&
      assertTrue(validation.validate("hi").isLeft)
    },
    test("MaxLength") {
      val validation = Validation.maxLength(4)

      assertTrue(validation.validate("Todd").isRight) &&
      assertTrue(validation.validate("how").isRight) &&
      assertTrue(validation.validate("hello").isLeft) &&
      assertTrue(validation.validate("Automobile").isLeft)
    },
    test("Regex digit or letter Validation") {
      val validation = Validation.regex(Regex.digitOrLetter)

      assertTrue(validation.validate("a").isRight) &&
      assertTrue(validation.validate("1").isRight) &&
      assertTrue(validation.validate("12").isLeft) &&
      assertTrue(validation.validate("*").isLeft) &&
      assertTrue(validation.validate("ab").isLeft) &&
      assertTrue(validation.validate("").isLeft) &&
      assertTrue(validation.validate("&").isLeft)
    },
    test("Regex identifier Validation") {
      val validation = Validation.identifier

      assertTrue(validation.validate("_").isRight) &&
      assertTrue(validation.validate("a").isRight) &&
      assertTrue(validation.validate("ab").isRight) &&
      assertTrue(validation.validate("").isLeft) &&
      assertTrue(validation.validate("*").isLeft)
    },
    suite("Regex email Validation")(
      test("should reject an invalid email") {
        val examples = Gen.fromIterable {
          Seq(
            "bob101*@gmail.com",
            "_",
            "@",
            "@.",
            "b@.com",
            "",
            "1@.com",
            "1@a.com",
            "1@a.b.com",
            "a@@a.com",
            "a@b@c@example.com",
            "a@a..b.com",
            "john@doe..",
            "john.doe@foo.bar,com",
            "john.doe@underscore_not_allowed_in_domain_part.example.com",
            "xy@email.is-too-long.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.a123",
            "the-local-part-too-long-over-64-so.invalid.123456789.123456789.a1@even.broad.definition.of.RFC5322.accepts.it",
            "postmaster@[123.123.123]",
            "postmaster@[IPv6:1200::AB00:1234::2552:7777:1313]",
            "postmaster@[2001:cdba:0:0:0:0:3257:9652]"
          )
        }
        val validationResult = (value: String) => Validation.email.validate(value)

        checkAll(examples) { email =>
          assertTrue(validationResult(email).isLeft)
        }
      },
      test("should accept a correct email") {
        val examples = Gen.fromIterable {
          Seq(
            "bob101@gmail.com",
            "bob+and-alice@gmail.com",
            "bob_and_alice@gmail.com",
            "johndoe@total.length.up.to.254-chars.so.still-valid.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.123456789.a1",
            "the-local-part-is-below-64-so.valid.123456789.123456789.a1234567@foo.com",
            "name@a--b.pl",
            "bob-@foo.co.uk",
            "bob+@foo.co.uk",
            "a.b@a.com",
            "a..b@a.com",
            "postmaster@[123.123.123.123]",
            "postmaster@[IPv6:2001:cdba:0:0:0:0:3257:9652]"
          )
        }
        val validationResult = (value: String) => Validation.email.validate(value)

        checkAll(examples) { email =>
          assertTrue(validationResult(email).isRight)
        }
      }
    ),
    suite("Regex IPv4 Validation")(
      test("should accept a valid IPv4 address") {
        val examples = Gen.fromIterable {
          Seq(
            "255.255.255.255",
            "192.168.1.255",
            "192.168.001.255",
            "0.0.0.0",
            "1.1.1.1",
            "1.0.128.0",
            "127.0.0.1",
            "0.0.0.1",
            "10.0.0.255",
            "69.89.31.226",
            "01.001.100.199"
          )
        }

        val validationResult = (value: String) => Validation.ipV4.validate(value)

        checkAll(examples) { ip =>
          assertTrue(validationResult(ip).isRight)
        }
      },
      test("should reject an invalid IPv4 address") {
        val examples = Gen.fromIterable {
          Seq(
            "10.0.0.256",
            "256.256.256.256",
            "127.0.0 1",
            "192.168.1",
            "-1.0.0.1"
          )
        }

        val validationResult = (value: String) => Validation.ipV4.validate(value)

        checkAll(examples) { ip =>
          assertTrue(validationResult(ip).isLeft)
        }
      }
    ),
    suite("Regex IPv6 Validation")(
      test("should accept a valid IPv6 address") {
        val examples = Gen.fromIterable {
          Seq(
            "2001:470:9b36:1::2",
            "2001:cdba:0:0:0:0:3257:9652",
            "2001:cdba::3257:9652",
            "0000:0000:0000:0000:0000:0000:0000:0001",
            "0:0:0:0:0:0:0:1",
            "0000:0000:0000:0000:0000:0000:0000:0000",
            "2001:0000:0000:0000:0000:0001:0000:0000",
            "0:0:0:0:0:0:0:0",
            "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff",
            "2001:0db8:3c4d:0015:0000:0000:1a2f:1a2b",
            "12AB:0000:0000:CD30:0000:0000:0000:0000",
            "12AB::CD30:0:0:0:0",
            "12AB:0:0:CD30::",
            "2001:cdba:0000:0000:0000:0000:3257:9652"
          )
        }

        val validationResult = (value: String) => Validation.ipV6.validate(value)

        checkAll(examples) { ip =>
          assertTrue(validationResult(ip).isRight)
        }
      },
      test("should reject an invalid IPv6 address") {
        val examples = Gen.fromIterable {
          Seq(
            " 001:db8:122:344::192.0.2.33",
            "1200::AB00:1234::2552:7777:1313",
            "1200:0000:AB00:1234:O000:2552:7777:1313"
          )
        }

        val validationResult = (value: String) => Validation.ipV6.validate(value)

        checkAll(examples) { ip =>
          assertTrue(validationResult(ip).isLeft)
        }
      }
    ),
    suite("Regex uuid Validations")(
      test("valid UUID") {
        val validation = Validation.uuidV4
        check(Gen.uuid) { uuid =>
          assertTrue(validation.validate(uuid.toString).isRight)
        }
      },
      test("invalid UUID") {
        val validation = Validation.uuidV4
        assertTrue(validation.validate("1e3118de-ddb6-11ec-8653-93e6961d46be").isLeft) &&
        assertTrue(validation.validate("487f5075-fa89-4723-a26d-2e7a13245").isLeft) &&
        assertTrue(validation.validate("487f5075fa894723a26d2e7a13245135").isLeft) &&
        assertTrue(validation.validate("").isLeft)
      }
    ),
    test("Time Validation HH") {
      val parsedTimes =
        parseTimes(CreateTimesConfig(HasHour, "", NoMinute, "", NoSecond, "", NoFraction, "", NoAmPm), "HH")
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation H") {
      val parsedTimes =
        parseTimes(CreateTimesConfig(HasHour, "", NoMinute, "", NoSecond, "", NoFraction, "", NoAmPm), "H")
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation mm") {
      val parsedTimes =
        parseTimes(CreateTimesConfig(NoHour, "", HasMinute, "", NoSecond, "", NoFraction, "", NoAmPm), "mm")
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation m") {
      val parsedTimes =
        parseTimes(CreateTimesConfig(NoHour, "", HasMinute, "", NoSecond, "", NoFraction, "", NoAmPm), "m")
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation HHmm") {
      val parsedTimes =
        parseTimes(CreateTimesConfig(HasHour, "", HasMinute, "", NoSecond, "", NoFraction, "", NoAmPm), "HHmm")
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation HH:mm") {
      val parsedTimes =
        parseTimes(CreateTimesConfig(HasHour, ":", HasMinute, "", NoSecond, "", NoFraction, "", NoAmPm), "HH:mm")
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation HH:mm:ss") {
      val parsedTimes =
        parseTimes(CreateTimesConfig(HasHour, ":", HasMinute, ":", HasSecond, "", NoFraction, "", NoAmPm), "HH:mm:ss")
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation HH:mm:ss a") {
      val parsedTimes = parseTimes(
        CreateTimesConfig(HasHour, ":", HasMinute, ":", HasSecond, "", NoFraction, " ", HasAmPm),
        "HH:mm:ss a"
      )
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation H:m:s") {
      val parsedTimes =
        parseTimes(CreateTimesConfig(HasHour, ":", HasMinute, ":", HasSecond, "", NoFraction, "", NoAmPm), "H:m:s")
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation H:m:s a") {
      val parsedTimes =
        parseTimes(CreateTimesConfig(HasHour, ":", HasMinute, ":", HasSecond, "", NoFraction, " ", HasAmPm), "H:m:s a")
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation hh:mm:ss") {
      val parsedTimes =
        parseTimes(CreateTimesConfig(HasHour, ":", HasMinute, ":", HasSecond, "", NoFraction, "", NoAmPm), "hh:mm:ss")
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation hh:mm:ss a") {
      val parsedTimes = parseTimes(
        CreateTimesConfig(HasHour, ":", HasMinute, ":", HasSecond, "", NoFraction, " ", HasAmPm),
        "hh:mm:ss a"
      )
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation h:m:s") {
      val parsedTimes =
        parseTimes(CreateTimesConfig(HasHour, ":", HasMinute, ":", HasSecond, "", NoFraction, "", NoAmPm), "h:m:s")
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation h:m:s a") {
      val parsedTimes =
        parseTimes(CreateTimesConfig(HasHour, ":", HasMinute, ":", HasSecond, "", NoFraction, " ", HasAmPm), "h:m:s a")
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation HH:mm:ss S") {
      val parsedTimes = parseTimes(
        CreateTimesConfig(HasHour, ":", HasMinute, ":", HasSecond, " ", HasFraction, "", NoAmPm),
        "HH:mm:ss S"
      )
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation HH:mm:ss.SSS") {
      val parsedTimes = parseTimes(
        CreateTimesConfig(HasHour, ":", HasMinute, ":", HasSecond, ".", HasFraction, "", NoAmPm),
        "HH:mm:ss.SSS"
      )
      assertParsedTimes(parsedTimes)
    },
    test("Time Validation HH:mm:ss SSSSSSSSS a") {
      val parsedTimes = parseTimes(
        CreateTimesConfig(HasHour, ":", HasMinute, ":", HasSecond, " ", HasFraction, " ", HasAmPm),
        "HH:mm:ss SSSSSSSSS a"
      )
      assertParsedTimes(parsedTimes)
    },
    test("Regex duration Validation") {
      check(Gen.finiteDuration) { duration =>
        assertTrue(Validation.duration.validate(duration.toString).isRight)
      }
    }
  )

  private def assertParsedTimes(parsedTimes: ParsedTimes) =
    parsedTimes.enoughParsed && parsedTimes.allParseResultsCorrect && parsedTimes.allWrongNotParsed && parsedTimes.allWrongParseResultsCorrect

  private case class ParsedTimes(properTimeResults: Seq[ParsedTime], wrongTimeResults: Seq[ParsedTime]) {

    def enoughParsed: TestResult =
      assertTrue(properTimeResults.count(result => result.parsed) >= properTimeResults.size / 4)

    def allParseResultsCorrect: TestResult =
      assertTrue(properTimeResults.forall(result => result.valid == result.parsed))
    def allWrongNotParsed: TestResult = assertTrue(wrongTimeResults.forall(result => !result.valid))

    def allWrongParseResultsCorrect: TestResult =
      assertTrue(wrongTimeResults.forall(result => result.valid == result.parsed))
  }

  private case class ParsedTime(time: String, valid: Boolean, parsed: Boolean)

  private def parseTimes(createTimesConfig: CreateTimesConfig, format: String): ParsedTimes = {
    val times      = createTimes(exampleTimes, createTimesConfig)
    val wrongTimes = createTimes(wrongExampleTimes, createTimesConfig)
    val validation = Validation.time(format)
    val formatter  = DateTimeFormatter.ofPattern(format)

    def innerParse(times: Seq[String]) =
      times.map { time =>
        val valid  = validation.validate(time).isRight
        val parsed = Try(formatter.parse(time)).isSuccess
        ParsedTime(time, valid, parsed)
      }

    val results          = innerParse(times)
    val wrongTimeResults = innerParse(wrongTimes)

    ParsedTimes(results, wrongTimeResults)
  }

  private case class ExampleTime(hour: String, minute: String, second: String, fraction: String, amPm: String)

  private val exampleTimes =
    Seq(
      ExampleTime("0", "0", "0", "0", "AM"),
      ExampleTime("1", "1", "1", "1", "AM"),
      ExampleTime("00", "00", "00", "0", "AM"),
      ExampleTime("00", "00", "00", "000000000", "AM"),
      ExampleTime("01", "01", "00", "1", "AM"),
      ExampleTime("01", "01", "00", "000000001", "AM"),
      // ExampleTime("01", "01", "00", "", "PM"), // TODO - good syntax but invalid time for format "HH:mm:ss a" and "H:m:s a"
      ExampleTime("10", "00", "00", "123", "AM"),
      ExampleTime("12", "00", "00", "0", "PM"),
      ExampleTime("12", "09", "09", "000", "PM"),
      ExampleTime("12", "59", "59", "555", "PM"),
      ExampleTime("13", "09", "09", "100000000", "PM"),
      ExampleTime("20", "20", "20", "123456789", "PM"),
      ExampleTime("22", "22", "22", "2", "PM"),
      ExampleTime("23", "59", "00", "222222222", "PM"),
      ExampleTime("23", "59", "59", "999", "PM"),
      ExampleTime("0", "00", "00", "00000", "AM"),
      ExampleTime("1", "11", "00", "000", "AM"),
      // ExampleTime("24", "00", "00", "", "PM"), // TODO - good syntax but invalid time for format "HH:mm:ss a" and "H:m:s a"
      ExampleTime("24", "00", "00", "000000000", "AM")
    )

  private val wrongExampleTimes =
    Seq(
      ExampleTime("", "", "", "", ""),
      ExampleTime("-1", "-1", "-1", "-1", "-"),
      ExampleTime("25", "60", "60", "1000000000", "PM"),
      ExampleTime("90", "90", "90", "9999999999", "PM"),
      ExampleTime("05-05", "05-05", "05-05", "05-05", "PM"),
      ExampleTime("0505", "0505", "0505", "050505050505", "PM"),
      ExampleTime("123", "123", "123", "1231231231", "AM"),
      ExampleTime("111", "111", "111", "0000000001", "XX")
    )

  sealed private trait Hour
  private object Hour {
    case object HasHour extends Hour
    case object NoHour  extends Hour
  }

  sealed private trait Minute
  private object Minute {
    case object HasMinute extends Minute
    case object NoMinute  extends Minute
  }

  sealed private trait Second
  private object Second {
    case object HasSecond extends Second
    case object NoSecond  extends Second
  }

  sealed private trait Fraction
  private object Fraction {
    case object HasFraction extends Fraction
    case object NoFraction  extends Fraction
  }

  sealed private trait AmPm
  private object AmPm {
    case object HasAmPm extends AmPm
    case object NoAmPm  extends AmPm
  }

  private case class CreateTimesConfig(
    hasHour: Hour,
    minuteSeparator: String = "",
    hasMinute: Minute,
    secondSeparator: String = "",
    hasSecond: Second,
    fractionSeparator: String = "",
    hasFraction: Fraction,
    amPmSeparator: String = "",
    hasAmPm: AmPm
  )

  private def createTimes(
    exampleTimes: Seq[ExampleTime],
    config: CreateTimesConfig
  ): Seq[String] =
    exampleTimes.map {
      case ExampleTime(hour, minute, second, fraction, amPm) =>
        val hourStr = config.hasHour match {
          case HasHour => hour
          case NoHour  => ""
        }
        val minuteStr = config.hasMinute match {
          case HasMinute => minute
          case NoMinute  => ""
        }
        val secondStr = config.hasSecond match {
          case HasSecond => second
          case NoSecond  => ""
        }
        val fractionStr = config.hasFraction match {
          case HasFraction => fraction
          case NoFraction  => ""
        }
        val amPmStr = config.hasAmPm match {
          case HasAmPm => amPm
          case NoAmPm  => ""
        }
        s"$hourStr${config.minuteSeparator}$minuteStr${config.secondSeparator}$secondStr${config.fractionSeparator}$fractionStr${config.amPmSeparator}$amPmStr"
    }

}
