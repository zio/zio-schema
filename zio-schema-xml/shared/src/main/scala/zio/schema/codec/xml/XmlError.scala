package zio.schema.codec.xml

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

object XmlError {

  def parseError(message: String, line: Int, column: Int): XmlError =
    new XmlError(message, Some(line), Some(column))

  def validationError(message: String): XmlError =
    new XmlError(message)

  def encodingError(message: String): XmlError =
    new XmlError(message)
}
