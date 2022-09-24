package zio.schema

import zio.prelude._

object FieldId extends Subtype[String] {

  implicit class TypeIdSyntax(private val self: FieldId) extends AnyVal {
    
  }
}