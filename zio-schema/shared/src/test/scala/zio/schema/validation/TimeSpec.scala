package zio.schema.validation

import zio.Scope
import zio.test.Assertion._
import zio.test._

object TimeSpec extends ZIOSpecDefault {

  def spec: Spec[Environment with TestEnvironment with Scope, Any] =
    suite("TimeSpec")(
      test("Valid formats") {
        assert(Validation.time("H"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("HH"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("h"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("hh"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("m"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("mm"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("s"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("ss"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("S"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("SS"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("SSS"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("SSSSSSSSS"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("HHmm"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("H:m"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("H:m a"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("HH:mm"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("HH:mm a"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("HHmmss"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("HH:mm:ss"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("HH:mm:ssa"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("HH:mm:ss a"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("a HH:mm:ss"))(isSubtype[Validation[String]](anything)) &&
        assert(Validation.time("H:m:s a"))(isSubtype[Validation[String]](anything))
      },
      test("Invalid formats") {
        assert(Validation.time("HHH"))(throws(hasMessage(containsString("max length for")))) &&
        assert(Validation.time("hhh"))(throws(hasMessage(containsString("max length for")))) &&
        assert(Validation.time("mmm"))(throws(hasMessage(containsString("max length for")))) &&
        assert(Validation.time("sss"))(throws(hasMessage(containsString("max length for")))) &&
        assert(Validation.time("SSSSSSSSSS"))(throws(hasMessage(containsString("max length for")))) &&
        assert(Validation.time("aa"))(throws(hasMessage(containsString("max length for")))) &&
        assert(Validation.time("HHmmH"))(throws(hasMessage(containsString("already used in format")))) &&
        assert(Validation.time("HHmmsm"))(throws(hasMessage(containsString("already used in format")))) &&
        assert(Validation.time("hhmmhh"))(throws(hasMessage(containsString("already used in format")))) &&
        assert(Validation.time("a HH:mm:s a"))(throws(hasMessage(containsString("already used in format")))) &&
        assert(Validation.time("HH:mm:ss.SS S"))(throws(hasMessage(containsString("already used in format")))) &&
        assert(Validation.time(":"))(throws(hasMessage(containsString("There is no time field")))) &&
        assert(Validation.time("b"))(throws(hasMessage(containsString("All letters are reserved")))) &&
        assert(Validation.time("j"))(throws(hasMessage(containsString("All letters are reserved")))) &&
        assert(Validation.time("B"))(throws(hasMessage(containsString("All letters are reserved")))) &&
        assert(Validation.time("J"))(throws(hasMessage(containsString("All letters are reserved")))) &&
        assert(Validation.time(""))(throws(hasMessage(containsString("There is no time field"))))
      }
    )
}
