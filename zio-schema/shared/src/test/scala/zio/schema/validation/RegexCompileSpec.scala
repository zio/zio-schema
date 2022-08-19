package zio.schema.validation

import zio.Scope
import zio.test._

object RegexCompileSpec extends ZIOSpecDefault {

  def spec: Spec[Environment with TestEnvironment with Scope, Any] = suite("Compiling Regex")(
    test("compile oneOf singleton") {
      val regex    = Regex.oneOf('a')
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "a")
    },
    test("compile literal") {
      val regex    = Regex.literal("abc")
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "(abc)")
    },
    test("compile oneOf several elements") {
      val regex    = Regex.oneOf('a', 'b', 'c')
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "[abc]")
    },
    test("compile between") {
      val regex    = Regex.between('a', 'z')
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "[a-z]")
    },
    test("compile *") {
      val regex    = Regex.oneOf('a', 'b', 'c').*
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "([abc])*")
    },
    test("compile +") {
      val regex    = Regex.oneOf('a', 'b', 'c').+
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "([abc])+")
    },
    test("compile ?") {
      val regex    = Regex.oneOf('a', 'b', 'c').?
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "([abc])?")
    },
    test("compile repeat n") {
      val regex    = Regex.oneOf('a', 'b', 'c').between(2, 2)
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "([abc]){2}")
    },
    test("compile repeat n+") {
      val regex    = Regex.oneOf('a', 'b', 'c').atLeast(2)
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "([abc]){2,}")
    },
    test("compile repeat n to m") {
      val regex    = Regex.oneOf('a', 'b', 'c').between(2, 4)
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "([abc]){2,4}")
    },
    test("compile alternative") {
      val regex    = Regex.oneOf('a', 'b', 'c') | Regex.oneOf('d', 'e', 'f')
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "(([abc])|([def]))")
    },
    test("compile letter") {
      val regex    = Regex.letter
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "[a-zA-Z]")
    },
    test("compile digit") {
      val regex    = Regex.digit
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "\\d")
    },
    test("compile sequence") {
      val regex    = Regex.oneOf('a', 'b', 'c') ~ Regex.oneOf('d', 'e', 'f')
      val regexStr = Regex.toRegexString(regex)
      assertTrue(regexStr == "[abc][def]")
    }
  )
}
