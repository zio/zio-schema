package zio.schema.codec

import zio.json.JsonDecoder.JsonError
import zio.json.ast.Json
import zio.json.internal.{FastStringReader, Lexer, RetractReader, UnexpectedEnd}
import zio.json.{JsonDecoder => ZJsonDecoder}

/**
 * Enforces end-of-input validation on root single-value JSON decoding so trailing non-whitespace tokens are rejected.
 *
 * Root `decodeJson` invocations pass an empty `JsonError` trace alongside a `FastStringReader`, whereas streaming
 * pipelines and nested field decoders either populate the trace or supply buffered reader implementations.
 */
final private[codec] class StrictJsonDecoder[A](private[codec] val underlying: ZJsonDecoder[A])
    extends ZJsonDecoder[A] {

  override def unsafeDecode(trace: List[JsonError], in: RetractReader): A = {
    val decoded = underlying.unsafeDecode(trace, in)
    if (StrictJsonDecoder.isRootSingleValueReader(trace, in)) {
      StrictJsonDecoder.ensureEndOfInput(trace, in)
    }
    decoded
  }

  override def unsafeDecodeMissing(trace: List[JsonError]): A =
    underlying.unsafeDecodeMissing(trace)

  final override def unsafeFromJsonAST(trace: List[JsonError], json: Json): A =
    underlying.unsafeFromJsonAST(trace, json)
}

private[codec] object StrictJsonDecoder {

  def wrap[A](decoder: ZJsonDecoder[A]): ZJsonDecoder[A] =
    decoder match {
      case strict: StrictJsonDecoder[A] => strict
      case other                        => new StrictJsonDecoder(other)
    }

  private def isRootSingleValueReader(trace: List[JsonError], in: RetractReader): Boolean =
    trace.isEmpty && in.isInstanceOf[FastStringReader]

  private def ensureEndOfInput(trace: List[JsonError], in: RetractReader): Unit = {
    var trailing: Char = 0
    val hasTrailing =
      try {
        trailing = in.nextNonWhitespace()
        true
      } catch {
        case _: UnexpectedEnd => false
      }
    if (hasTrailing) {
      Lexer.error(s"expected end of input, got '$trailing'", trace)
    }
  }
}
