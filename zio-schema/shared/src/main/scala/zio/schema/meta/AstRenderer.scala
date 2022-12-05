package zio.schema.meta

import zio.Chunk

private[schema] object AstRenderer {
  private val INDENT_STEP = 2

  def render(ast: ExtensibleMetaSchema[_]): String = ast match {
    case v @ ExtensibleMetaSchema.Value(_, _, _)    => renderValue(v, 0, None)
    case f @ ExtensibleMetaSchema.FailNode(_, _, _) => renderFail(f, 0, None)
    case ExtensibleMetaSchema.Product(_, _, fields, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"product")
      if (optional) buffer.append("?")
      buffer.append("\n").append(fields.map(renderField(_, INDENT_STEP)).mkString("\n")).toString
    case ExtensibleMetaSchema.Tuple(_, left, right, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"tuple")
      if (optional) buffer.append("?")
      buffer
        .append("\n")
        .append(Chunk("left" -> left, "right" -> right).map(renderField(_, INDENT_STEP)).mkString("\n"))
        .toString
    case ExtensibleMetaSchema.Sum(_, _, cases, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"enum")
      if (optional) buffer.append("?")
      buffer.append("\n").append(cases.map(renderField(_, INDENT_STEP)).mkString("\n")).toString
    case ExtensibleMetaSchema.Either(_, left, right, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"either")
      if (optional) buffer.append("?")
      buffer
        .append("\n")
        .append(Chunk("left" -> left, "right" -> right).map(renderField(_, INDENT_STEP)).mkString("\n"))
        .toString
    case ExtensibleMetaSchema.ListNode(items, _, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"list")
      if (optional) buffer.append("?")
      buffer
        .append("\n")
        .append(Chunk("item" -> items).map(renderField(_, INDENT_STEP)).mkString("\n"))
        .toString
    case ExtensibleMetaSchema.Dictionary(keys, values, _, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"map")
      if (optional) buffer.append("?")
      buffer
        .append("\n")
        .append(Chunk("keys" -> keys, "values" -> values).map(renderField(_, INDENT_STEP)).mkString("\n"))
        .toString
    case ExtensibleMetaSchema.Ref(refPath, _, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"ref#$refPath")
      if (optional) buffer.append("?")
      buffer.toString
    case ExtensibleMetaSchema.Known(typeId, _, optional) =>
      val buffer = new StringBuffer()
      buffer.append(typeId.toString)
      if (optional) buffer.append("?")
      buffer.toString
  }

  def renderField(value: ExtensibleMetaSchema.Labelled[_], indent: Int): String = {
    val buffer = new StringBuffer()
    value match {
      case (label, value @ ExtensibleMetaSchema.Value(_, _, _)) =>
        renderValue(value, indent, Some(label))
      case (label, fail @ ExtensibleMetaSchema.FailNode(_, _, _)) =>
        renderFail(fail, indent, Some(label))
      case (label, ExtensibleMetaSchema.Product(_, _, fields, optional)) =>
        pad(buffer, indent)
        buffer.append(s"$label: record")
        if (optional) buffer.append("?")
        buffer.append("\n").append(fields.map(renderField(_, indent + INDENT_STEP)).mkString("\n")).toString
      case (label, ExtensibleMetaSchema.Tuple(_, left, right, optional)) =>
        pad(buffer, indent)
        buffer.append(s"$label: tuple")
        if (optional) buffer.append("?")
        buffer
          .append("\n")
          .append(Chunk("left" -> left, "right" -> right).map(renderField(_, indent + INDENT_STEP)).mkString("\n"))
          .toString
      case (label, ExtensibleMetaSchema.Sum(_, _, cases, optional)) =>
        pad(buffer, indent)
        buffer.append(s"$label: enum")
        if (optional) buffer.append("?")
        buffer.append("\n").append(cases.map(renderField(_, indent + INDENT_STEP)).mkString("\n")).toString
      case (label, ExtensibleMetaSchema.Either(_, left, right, optional)) =>
        pad(buffer, indent)
        buffer.append(s"$label: either")
        if (optional) buffer.append("?")
        buffer
          .append("\n")
          .append(Chunk("left" -> left, "right" -> right).map(renderField(_, indent + INDENT_STEP)).mkString("\n"))
          .toString
      case (label, ExtensibleMetaSchema.ListNode(items, _, optional)) =>
        val buffer = new StringBuffer()
        buffer.append(s"$label: list")
        if (optional) buffer.append("?")
        buffer
          .append("\n")
          .append(Chunk("item" -> items).map(renderField(_, INDENT_STEP)).mkString("\n"))
          .toString
      case (label, ExtensibleMetaSchema.Dictionary(keys, values, _, optional)) =>
        val buffer = new StringBuffer()
        buffer.append(s"$label: map")
        if (optional) buffer.append("?")
        buffer
          .append("\n")
          .append(Chunk("keys" -> keys, "values" -> values).map(renderField(_, INDENT_STEP)).mkString("\n"))
          .toString
      case (label, ExtensibleMetaSchema.Ref(refPath, _, optional)) =>
        pad(buffer, indent)
        buffer.append(s"$label: ")
        if (optional) buffer.append("?")
        buffer.append(s"{ref#${refPath.render}}").toString
      case (label, ExtensibleMetaSchema.Known(typeId, _, optional)) =>
        pad(buffer, indent)
        buffer.append(s"$label: ")
        buffer.append(typeId.toString)
        if (optional) buffer.append("?")
        buffer.toString
    }
  }

  def renderValue(value: ExtensibleMetaSchema.Value[_], indent: Int, label: Option[String]): String = {
    val buffer = new StringBuffer()
    pad(buffer, indent)
    label.foreach(l => buffer.append(s"$l: "))
    if (value.optional) buffer.append("?")
    buffer.append(value.valueType.tag).toString
  }

  def renderFail(fail: ExtensibleMetaSchema.FailNode[_], indent: Int, label: Option[String]): String = {
    val buffer = new StringBuffer()
    pad(buffer, indent)
    label.foreach(l => buffer.append(s"$l: "))
    if (fail.optional) buffer.append("?")
    buffer.append(s"FAIL: ${fail.message}").toString
  }

  private def pad(buffer: StringBuffer, indent: Int): StringBuffer = {
    if (indent > 0) {
      buffer.append("|")
      for (_ <- 0 until indent) {
        buffer.append("-")
      }
    }
    buffer
  }
}
