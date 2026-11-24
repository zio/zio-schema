package zio.schema.codec.xml

/**
 * An XML error, such as an error from parsing XML text, with the 1-based `line` and `column` where parsing stopped when they are known. `toString` gives the message and position, such as `XmlError: Expected '=' at line 1, column 5`. `XmlCodec` puts that text in the `DecodeError.ReadError` it returns. It records no stack trace.
 */
class XmlError(message: String, val line: Option[Int] = None, val column: Option[Int] = None)
    extends Exception(message, null, false, false) {

  override def toString: String = {
    val position = (line, column) match {
      case (Some(l), Some(c)) => s" at line $l, column $c"
      case (Some(l), None)    => s" at line $l"
      case _                  => ""
    }
    s"XmlError: $message$position"
  }
}

/** Constructors for `XmlError`. */
object XmlError {

  /** Creates an error for malformed XML at `line` and `column`. */
  def parseError(message: String, line: Int, column: Int): XmlError =
    new XmlError(message, Some(line), Some(column))

  /** Creates an error with `message` and no position. */
  def validationError(message: String): XmlError =
    new XmlError(message)

  /** Creates an error with `message` and no position. */
  def encodingError(message: String): XmlError =
    new XmlError(message)
}
