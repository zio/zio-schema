package zio.schema.codec.xml

import zio.Chunk

object XmlReader {

  def read(input: String, config: ReaderConfig): Either[XmlError, Xml] = {
    val parser = new Parser(input, config)
    parser.skipXmlDeclaration()
    parser.skipWhitespace()
    if (parser.isEof)
      Left(XmlError.parseError("Empty input", parser.line, parser.col))
    else
      parser.parseNode(0)
  }

  private class Parser(input: String, config: ReaderConfig) {
    private var pos: Int = 0
    var line: Int        = 1
    var col: Int         = 1

    def isEof: Boolean = pos >= input.length

    private def peek: Char =
      if (isEof) throw new IllegalStateException("Unexpected end of input")
      else input.charAt(pos)

    private def advance(): Char = {
      val c = input.charAt(pos)
      pos += 1
      if (c == '\n') {
        line += 1; col = 1
      } else col += 1
      c
    }

    private def startsWith(s: String): Boolean =
      input.startsWith(s, pos)

    private def consume(s: String): Either[XmlError, Unit] =
      if (startsWith(s)) {
        var i = 0
        while (i < s.length) { advance(); i += 1 }
        Right(())
      } else
        Left(error(s"Expected '$s'"))

    private def error(msg: String): XmlError =
      XmlError.parseError(msg, line, col)

    def skipWhitespace(): Unit =
      while (!isEof && Character.isWhitespace(peek)) { val _ = advance() }

    def skipXmlDeclaration(): Unit =
      if (startsWith("<?xml")) {
        while (!isEof && !startsWith("?>")) { val _ = advance() }
        if (!isEof) {
          val _ = (advance(), advance())
        }
      }

    def parseNode(depth: Int): Either[XmlError, Xml] =
      if (depth > config.maxDepth)
        Left(error(s"Maximum depth ${config.maxDepth} exceeded"))
      else if (isEof)
        Left(error("Unexpected end of input"))
      else if (startsWith("<![CDATA["))
        parseCData()
      else if (startsWith("<!--"))
        parseComment()
      else if (startsWith("<?"))
        parseProcessingInstruction()
      else if (startsWith("</"))
        Left(error("Unexpected closing tag"))
      else if (peek == '<')
        parseElement(depth)
      else
        parseText()

    private def parseElement(depth: Int): Either[XmlError, Xml] = {
      advance() // consume '<'
      for {
        name  <- parseName()
        attrs <- parseAttributes()
        elem <- {
          skipWhitespace()
          if (startsWith("/>")) {
            advance(); advance()
            Right(Xml.Element(name, attrs, Chunk.empty))
          } else if (peek == '>') {
            advance()
            parseChildren(name, depth).map(children => Xml.Element(name, attrs, children))
          } else
            Left(error("Expected '>' or '/>'"))
        }
      } yield elem
    }

    private def parseName(): Either[XmlError, XmlName] = {
      skipWhitespace()
      val sb = new StringBuilder
      if (isEof || !isNameStartChar(peek))
        return Left(error("Expected element name"))
      while (!isEof && isNameChar(peek)) sb.append(advance())
      val raw      = sb.toString
      val colonIdx = raw.indexOf(':')
      if (colonIdx > 0 && colonIdx < raw.length - 1)
        Right(XmlName(raw.substring(colonIdx + 1), Some(raw.substring(0, colonIdx)), None))
      else
        Right(XmlName(raw))
    }

    private def parseAttributes(): Either[XmlError, Chunk[(XmlName, String)]] = {
      val builder = new scala.collection.mutable.ArrayBuffer[(XmlName, String)]()
      var count   = 0
      while ({
        skipWhitespace()
        !isEof && peek != '>' && !startsWith("/>") && isNameStartChar(peek)
      }) {
        if (count >= config.maxAttributes)
          return Left(error(s"Maximum attributes ${config.maxAttributes} exceeded"))
        parseName() match {
          case Left(e) => return Left(e)
          case Right(attrName) =>
            skipWhitespace()
            consume("=") match {
              case Left(e) => return Left(e)
              case Right(_) =>
                skipWhitespace()
                parseAttributeValue() match {
                  case Left(e) => return Left(e)
                  case Right(v) =>
                    builder += ((attrName, v))
                    count += 1
                }
            }
        }
      }
      Right(Chunk.fromIterable(builder))
    }

    private def parseAttributeValue(): Either[XmlError, String] = {
      if (isEof) return Left(error("Expected attribute value"))
      val quote = peek
      if (quote != '"' && quote != '\'')
        return Left(error("Expected '\"' or '''"))
      advance()
      val sb = new StringBuilder
      while (!isEof && peek != quote) {
        if (peek == '&')
          parseEntityReference() match {
            case Left(e)  => return Left(e)
            case Right(c) => sb.append(c)
          } else
          sb.append(advance())
      }
      if (isEof) return Left(error("Unterminated attribute value"))
      advance() // consume closing quote
      Right(sb.toString)
    }

    private def parseChildren(parentName: XmlName, depth: Int): Either[XmlError, Chunk[Xml]] = {
      val builder = new scala.collection.mutable.ArrayBuffer[Xml]()
      while (!isEof && !startsWith("</")) {
        if (peek == '<') {
          if (startsWith("<![CDATA["))
            parseCData() match {
              case Left(e)  => return Left(e)
              case Right(n) => builder += n
            } else if (startsWith("<!--"))
            parseComment() match {
              case Left(e)  => return Left(e)
              case Right(n) => builder += n
            } else if (startsWith("<?"))
            parseProcessingInstruction() match {
              case Left(e)  => return Left(e)
              case Right(n) => builder += n
            } else
            parseElement(depth + 1) match {
              case Left(e)  => return Left(e)
              case Right(n) => builder += n
            }
        } else {
          parseText() match {
            case Left(e) => return Left(e)
            case Right(t) =>
              t match {
                case Xml.Text(v) if !config.preserveWhitespace && v.trim.isEmpty => ()
                case _                                                           => builder += t
              }
          }
        }
      }
      if (isEof) return Left(error(s"Unterminated element '${parentName.qualifiedName}'"))
      consume("</") match {
        case Left(e) => return Left(e)
        case Right(_) =>
          parseName() match {
            case Left(e) => return Left(e)
            case Right(closeName) =>
              if (closeName.qualifiedName != parentName.qualifiedName)
                return Left(
                  error(
                    s"Mismatched closing tag: expected '${parentName.qualifiedName}', got '${closeName.qualifiedName}'"
                  )
                )
              skipWhitespace()
              consume(">") match {
                case Left(e)  => return Left(e)
                case Right(_) => ()
              }
          }
      }
      Right(Chunk.fromIterable(builder))
    }

    private def parseText(): Either[XmlError, Xml] = {
      val sb = new StringBuilder
      while (!isEof && peek != '<') {
        if (sb.length >= config.maxTextLength)
          return Left(error(s"Maximum text length ${config.maxTextLength} exceeded"))
        if (peek == '&')
          parseEntityReference() match {
            case Left(e)  => return Left(e)
            case Right(c) => sb.append(c)
          } else
          sb.append(advance())
      }
      Right(Xml.Text(sb.toString))
    }

    private def parseCData(): Either[XmlError, Xml] = {
      consume("<![CDATA[") match {
        case Left(e)  => return Left(e)
        case Right(_) => ()
      }
      val sb = new StringBuilder
      while (!isEof && !startsWith("]]>")) {
        if (sb.length >= config.maxTextLength)
          return Left(error(s"Maximum text length ${config.maxTextLength} exceeded"))
        sb.append(advance())
      }
      if (isEof) return Left(error("Unterminated CDATA section"))
      consume("]]>") match {
        case Left(e)  => Left(e)
        case Right(_) => Right(Xml.CData(sb.toString))
      }
    }

    private def parseComment(): Either[XmlError, Xml] = {
      consume("<!--") match {
        case Left(e)  => return Left(e)
        case Right(_) => ()
      }
      val sb = new StringBuilder
      while (!isEof && !startsWith("-->")) sb.append(advance())
      if (isEof) return Left(error("Unterminated comment"))
      consume("-->") match {
        case Left(e)  => Left(e)
        case Right(_) => Right(Xml.Comment(sb.toString))
      }
    }

    private def parseProcessingInstruction(): Either[XmlError, Xml] = {
      consume("<?") match {
        case Left(e)  => return Left(e)
        case Right(_) => ()
      }
      val targetSb = new StringBuilder
      while (!isEof && !Character.isWhitespace(peek) && !startsWith("?>")) targetSb.append(advance())
      val target = targetSb.toString
      skipWhitespace()
      val dataSb = new StringBuilder
      while (!isEof && !startsWith("?>")) dataSb.append(advance())
      if (isEof) return Left(error("Unterminated processing instruction"))
      consume("?>") match {
        case Left(e)  => Left(e)
        case Right(_) => Right(Xml.ProcessingInstruction(target, dataSb.toString.trim))
      }
    }

    private def parseEntityReference(): Either[XmlError, String] = {
      advance() // consume '&'
      val sb = new StringBuilder
      while (!isEof && peek != ';') {
        if (sb.length > 10) return Left(error("Invalid entity reference"))
        sb.append(advance())
      }
      if (isEof) return Left(error("Unterminated entity reference"))
      advance() // consume ';'
      sb.toString match {
        case "amp"  => Right("&")
        case "lt"   => Right("<")
        case "gt"   => Right(">")
        case "quot" => Right("\"")
        case "apos" => Right("'")
        case other  => Left(error(s"Unknown entity reference: '$other'"))
      }
    }

    private def isNameStartChar(c: Char): Boolean =
      c == '_' || c == ':' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

    private def isNameChar(c: Char): Boolean =
      isNameStartChar(c) || c == '-' || c == '.' || (c >= '0' && c <= '9')
  }

}
