package zio.schema.validation

import zio.Scope
import zio.test.Assertion._
import zio.test._

object TimeSpec extends ZIOSpecDefault {

  def spec: Spec[Environment with TestEnvironment with Scope, Any] = suite("TimeSpec")(
    test("Valid formats") {
      assert(SchemaValidation.time("H"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("HH"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("h"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("hh"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("m"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("mm"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("s"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("ss"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("S"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("SS"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("SSS"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("SSSSSSSSS"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("HHmm"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("H:m"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("H:m a"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("HH:mm"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("HH:mm a"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("HHmmss"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("HH:mm:ss"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("HH:mm:ssa"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("HH:mm:ss a"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("a HH:mm:ss"))(isSubtype[SchemaValidation[String]](anything)) &&
      assert(SchemaValidation.time("H:m:s a"))(isSubtype[SchemaValidation[String]](anything))
    },
    test("Invalid formats") {
      assert(SchemaValidation.time("HHH"))(throws(hasMessage(containsString("max length for")))) &&
      assert(SchemaValidation.time("hhh"))(throws(hasMessage(containsString("max length for")))) &&
      assert(SchemaValidation.time("mmm"))(throws(hasMessage(containsString("max length for")))) &&
      assert(SchemaValidation.time("sss"))(throws(hasMessage(containsString("max length for")))) &&
      assert(SchemaValidation.time("SSSSSSSSSS"))(throws(hasMessage(containsString("max length for")))) &&
      assert(SchemaValidation.time("aa"))(throws(hasMessage(containsString("max length for")))) &&
      assert(SchemaValidation.time("HHmmH"))(throws(hasMessage(containsString("already used in format")))) &&
      assert(SchemaValidation.time("HHmmsm"))(throws(hasMessage(containsString("already used in format")))) &&
      assert(SchemaValidation.time("hhmmhh"))(throws(hasMessage(containsString("already used in format")))) &&
      assert(SchemaValidation.time("a HH:mm:s a"))(throws(hasMessage(containsString("already used in format")))) &&
      assert(SchemaValidation.time("HH:mm:ss.SS S"))(throws(hasMessage(containsString("already used in format")))) &&
      assert(SchemaValidation.time(":"))(throws(hasMessage(containsString("There is no time field")))) &&
      assert(SchemaValidation.time("b"))(throws(hasMessage(containsString("All letters are reserved")))) &&
      assert(SchemaValidation.time("j"))(throws(hasMessage(containsString("All letters are reserved")))) &&
      assert(SchemaValidation.time("B"))(throws(hasMessage(containsString("All letters are reserved")))) &&
      assert(SchemaValidation.time("J"))(throws(hasMessage(containsString("All letters are reserved")))) &&
      assert(SchemaValidation.time(""))(throws(hasMessage(containsString("There is no time field"))))
    }
  )
}
