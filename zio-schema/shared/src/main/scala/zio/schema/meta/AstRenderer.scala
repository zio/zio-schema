package zio.schema.meta

import zio.Chunk
import zio.constraintless.TypeList
import zio.schema.meta.ExtensibleMetaSchema.{Labelled, Product}

private[schema] object AstRenderer {

  def render(ast: ExtensibleMetaSchema[_]): String = ast match {
    case v @ ExtensibleMetaSchema.Value(_, _, _)    => renderValue(v, List(), None, isLast = true)
    case f @ ExtensibleMetaSchema.FailNode(_, _, _) => renderFail(f, List(), None, isLast = true)
    case ExtensibleMetaSchema.Product(_, _, fields, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"record")
      if (optional) buffer.append("?")
      if (fields.nonEmpty) {
        buffer.append("\n")
        buffer.append(fields.zipWithIndex.map { case (fld, idx) =>
          renderField(fld, List(), isLast = idx == fields.size - 1)
        }.mkString("\n"))
      }
      buffer.toString
    case ExtensibleMetaSchema.Tuple(_, left, right, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"tuple")
      if (optional) buffer.append("?")
      val tFields = Chunk(Labelled("left", left), Labelled("right", right))
      buffer
        .append("\n")
        .append(tFields.zipWithIndex.map { case (fld, idx) =>
          renderField(fld, List(), isLast = idx == tFields.size - 1)
        }.mkString("\n"))
        .toString
    case ExtensibleMetaSchema.Sum(_, _, cases, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"enum:")
      if (optional) buffer.append("?")
      if (cases.nonEmpty) {
        buffer.append("\n")
        buffer.append(
          cases.zipWithIndex.map { case (fld, idx) =>
            if (
              fld.schema match {
                case Product(_, _, fields, _) => fields.isEmpty
                case _ => false
              }
            ) {
              val buf = new StringBuffer()
              pad(buf, List(), isLast = idx == cases.size - 1)
              buf.append(fld.label).toString
            } else {
              renderField(fld, List(), isLast = idx == cases.size - 1)
            }
          }.mkString("\n")
        )
      }
      buffer.toString
    case ExtensibleMetaSchema.Either(_, left, right, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"either")
      if (optional) buffer.append("?")
      val efields = Chunk(Labelled("left", left), Labelled("right", right))
      buffer.append("\n")
      buffer.append(efields.zipWithIndex.map { case (fld, idx) =>
        renderField(fld, List(), isLast = idx == efields.size - 1)
      }.mkString("\n"))
      buffer.toString
    case ExtensibleMetaSchema.Fallback(_, left, right, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"fallback")
      if (optional) buffer.append("?")
      val ffields = Chunk(Labelled("left", left), Labelled("right", right))
      buffer.append("\n")
      buffer.append(ffields.zipWithIndex.map { case (fld, idx) =>
        renderField(fld, List(), isLast = idx == ffields.size - 1)
      }.mkString("\n"))
      buffer.toString
    case ExtensibleMetaSchema.ListNode(items, _, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"list")
      if (optional) buffer.append("?")
      val itemsChunk = Chunk(Labelled("item", items))
      buffer.append("\n")
      buffer.append(itemsChunk.zipWithIndex.map { case (fld, idx) =>
        renderField(fld, List(), isLast = idx == itemsChunk.size - 1)
      }.mkString("\n"))
      buffer.toString
    case ExtensibleMetaSchema.Dictionary(keys, values, _, optional) =>
      val buffer = new StringBuffer()
      buffer.append(s"map")
      if (optional) buffer.append("?")
      val mfields = Chunk(Labelled("keys", keys), Labelled("values", values))
      buffer.append("\n")
      buffer.append(mfields.zipWithIndex.map { case (fld, idx) =>
        renderField(fld, List(), isLast = idx == mfields.size - 1)
      }.mkString("\n"))
      buffer.toString
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

  def renderField[BuiltIn <: TypeList](labelled: ExtensibleMetaSchema.Labelled[BuiltIn], indentGuides: List[Boolean], isLast: Boolean): String = {
    val buffer = new StringBuffer()
    labelled.schema match {
      case value @ ExtensibleMetaSchema.Value(_, _, _) =>
        renderValue(value, indentGuides, Some(labelled.label), isLast)
      case fail @ ExtensibleMetaSchema.FailNode(_, _, _) =>
        renderFail(fail, indentGuides, Some(labelled.label), isLast)
      case ExtensibleMetaSchema.Product(_, _, fields, optional) =>
        pad(buffer, indentGuides, isLast)
        buffer.append(s"${labelled.label}: record")
        if (optional) buffer.append("?")
        if (fields.nonEmpty) {
          buffer.append("\n")
          buffer.append(fields.zipWithIndex.map { case (fld, idx) =>
            renderField(fld, indentGuides :+ !isLast, isLast = idx == fields.size - 1)
          }.mkString("\n"))
        }
        buffer.toString
      case ExtensibleMetaSchema.Tuple(_, left, right, optional) =>
        pad(buffer, indentGuides, isLast)
        buffer.append(s"${labelled.label}: tuple")
        if (optional) buffer.append("?")
        val tfields = Chunk(Labelled("left", left), Labelled("right", right))
        buffer.append("\n")
        buffer.append(tfields.zipWithIndex.map { case (fld, idx) =>
          renderField(fld, indentGuides :+ !isLast, isLast = idx == tfields.size - 1)
        }.mkString("\n"))
        buffer.toString
      case ExtensibleMetaSchema.Sum(_, _, cases, optional) =>
        pad(buffer, indentGuides, isLast)
        buffer.append(s"${labelled.label}: enum")
        if (optional) buffer.append("?")
        if (cases.nonEmpty) {
          buffer.append("\n")
          buffer.append(cases.zipWithIndex.map { case (fld, idx) =>
            renderField(fld, indentGuides :+ !isLast, isLast = idx == cases.size - 1)
          }.mkString("\n"))
        }
        buffer.toString
      case ExtensibleMetaSchema.Either(_, left, right, optional) =>
        pad(buffer, indentGuides, isLast)
        buffer.append(s"${labelled.label}: either")
        if (optional) buffer.append("?")
        val efields = Chunk(Labelled("left", left), Labelled("right", right))
        buffer.append("\n")
        buffer.append(efields.zipWithIndex.map { case (fld, idx) =>
          renderField(fld, indentGuides :+ !isLast, isLast = idx == efields.size - 1)
        }.mkString("\n"))
        buffer.toString
      case ExtensibleMetaSchema.Fallback(_, left, right, optional) =>
        pad(buffer, indentGuides, isLast)
        buffer.append(s"${labelled.label}: fallback")
        if (optional) buffer.append("?")
        val ffields = Chunk(Labelled("left", left), Labelled("right", right))
        buffer.append("\n")
        buffer.append(ffields.zipWithIndex.map { case (fld, idx) =>
          renderField(fld, indentGuides :+ !isLast, isLast = idx == ffields.size - 1)
        }.mkString("\n"))
        buffer.toString
      case ExtensibleMetaSchema.ListNode(items, _, optional) =>
        pad(buffer, indentGuides, isLast)
        buffer.append(s"${labelled.label}: list")
        if (optional) buffer.append("?")
        val itemsChunk = Chunk(Labelled("item", items))
        buffer.append("\n")
        buffer.append(itemsChunk.zipWithIndex.map { case (fld, idx) =>
          renderField(fld, indentGuides :+ !isLast, isLast = idx == itemsChunk.size - 1)
        }.mkString("\n"))
        buffer.toString
      case ExtensibleMetaSchema.Dictionary(keys, values, _, optional) =>
        pad(buffer, indentGuides, isLast)
        buffer.append(s"${labelled.label}: map")
        if (optional) buffer.append("?")
        val mfields = Chunk(Labelled("keys", keys), Labelled("values", values))
        buffer.append("\n")
        buffer.append(mfields.zipWithIndex.map { case (fld, idx) =>
          renderField(fld, indentGuides :+ !isLast, isLast = idx == mfields.size - 1)
        }.mkString("\n"))
        buffer.toString
      case ExtensibleMetaSchema.Ref(refPath, _, optional) =>
        pad(buffer, indentGuides, isLast)
        buffer.append(s"${labelled.label}: ")
        if (optional) buffer.append("?")
        buffer.append(s"{ref#${refPath.render}}").toString
      case ExtensibleMetaSchema.Known(typeId, _, optional) =>
        pad(buffer, indentGuides, isLast)
        buffer.append(s"${labelled.label}: ")
        buffer.append(typeId.toString)
        if (optional) buffer.append("?")
        buffer.toString
    }
  }

  def renderValue(value: ExtensibleMetaSchema.Value[_], indentGuides: List[Boolean], label: Option[String], isLast: Boolean): String = {
    val buffer = new StringBuffer()
    pad(buffer, indentGuides, isLast)
    label.foreach(l => buffer.append(s"$l: "))
    if (value.optional) buffer.append("?")
    buffer.append(value.valueType.tag).toString
  }

  def renderFail(fail: ExtensibleMetaSchema.FailNode[_], indentGuides: List[Boolean], label: Option[String], isLast: Boolean): String = {
    val buffer = new StringBuffer()
    pad(buffer, indentGuides, isLast)
    label.foreach(l => buffer.append(s"$l: "))
    if (fail.optional) buffer.append("?")
    buffer.append(s"FAIL: ${fail.message}").toString
  }

  private def pad(buffer: StringBuffer, indentGuides: List[Boolean], isLast: Boolean): StringBuffer = {
    indentGuides.foreach { guide =>
      buffer.append(if (guide) "│   " else "    ")
    }
    buffer.append(if (isLast) "└── " else "├── ")
  }
}
