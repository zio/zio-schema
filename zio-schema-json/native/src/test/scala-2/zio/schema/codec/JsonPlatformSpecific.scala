package zio.schema.codec

import zio.test.{ Spec, TestEnvironment }

object JsonPlatformSpecific {
  val platformSpecificEncoderTests: List[Spec[TestEnvironment, Nothing]]     = List.empty
  val platformSpecificDecoderTests: List[Spec[TestEnvironment, DecodeError]] = List.empty
}
