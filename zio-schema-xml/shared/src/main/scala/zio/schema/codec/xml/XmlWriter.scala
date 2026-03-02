package zio.schema.codec.xml

import zio.Chunk

object XmlWriter {

  def write(xml: Xml, config: WriterConfig): String = {
    val sb = new StringBuilder
    if (config.includeDeclaration)
      sb.append(s"""<?xml version="1.0" encoding="${config.encoding}"?>""")
    writeNode(sb, xml, config, 0, config.includeDeclaration)
    sb.toString
  }

  private def writeNode(
    sb: StringBuilder,
    xml: Xml,
    config: WriterConfig,
    depth: Int,
    needsNewline: Boolean
  ): Unit =
    xml match {
      case Xml.Element(name, attributes, children) =>
        if (needsNewline && config.indentStep > 0) {
          sb.append('\n')
          indent(sb, depth, config.indentStep)
        }
        sb.append('<')
        sb.append(name.qualifiedName)
        writeAttributes(sb, attributes)
        if (children.isEmpty) {
          sb.append("/>")
        } else {
          sb.append('>')
          val hasElementChildren = children.exists(_.isInstanceOf[Xml.Element])
          children.foreach { child =>
            writeNode(sb, child, config, depth + 1, hasElementChildren)
          }
          if (hasElementChildren && config.indentStep > 0) {
            sb.append('\n')
            indent(sb, depth, config.indentStep)
          }
          sb.append("</")
          sb.append(name.qualifiedName)
          sb.append('>')
        }

      case Xml.Text(value) =>
        sb.append(escapeText(value))

      case Xml.CData(value) =>
        sb.append("<![CDATA[")
        sb.append(value)
        sb.append("]]>")

      case Xml.Comment(value) =>
        if (needsNewline && config.indentStep > 0) {
          sb.append('\n')
          indent(sb, depth, config.indentStep)
        }
        sb.append("<!--")
        sb.append(value)
        sb.append("-->")

      case Xml.ProcessingInstruction(target, data) =>
        if (needsNewline && config.indentStep > 0) {
          sb.append('\n')
          indent(sb, depth, config.indentStep)
        }
        sb.append("<?")
        sb.append(target)
        if (data.nonEmpty) {
          sb.append(' ')
          sb.append(data)
        }
        sb.append("?>")
    }

  private def writeAttributes(sb: StringBuilder, attributes: Chunk[(XmlName, String)]): Unit =
    attributes.foreach {
      case (name, value) =>
        sb.append(' ')
        sb.append(name.qualifiedName)
        sb.append("=\"")
        sb.append(escapeAttribute(value))
        sb.append('"')
    }

  private def indent(sb: StringBuilder, depth: Int, step: Int): Unit = {
    var i     = 0
    val total = depth * step
    while (i < total) {
      sb.append(' ')
      i += 1
    }
  }

  private def escapeText(s: String): String = {
    val sb = new StringBuilder(s.length)
    var i  = 0
    while (i < s.length) {
      s.charAt(i) match {
        case '&'   => sb.append("&amp;")
        case '<'   => sb.append("&lt;")
        case '>'   => sb.append("&gt;")
        case other => sb.append(other)
      }
      i += 1
    }
    sb.toString
  }

  private def escapeAttribute(s: String): String = {
    val sb = new StringBuilder(s.length)
    var i  = 0
    while (i < s.length) {
      s.charAt(i) match {
        case '&'   => sb.append("&amp;")
        case '<'   => sb.append("&lt;")
        case '>'   => sb.append("&gt;")
        case '"'   => sb.append("&quot;")
        case '\''  => sb.append("&apos;")
        case other => sb.append(other)
      }
      i += 1
    }
    sb.toString
  }
}
