package zio.schema.codec

import zio.test.{ Sized, Spec }

object ProtobufPlatformSpecific {
  val platformSpecificEncodeAndDecode: List[Spec[Sized, DecodeError]] = List.empty
}
