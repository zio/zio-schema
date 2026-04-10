package zio.schema

sealed trait SchemaExpr[+A] {
  def toDynamic: DynamicValue
}

object SchemaExpr {
  case object DefaultValue extends SchemaExpr[Nothing] {
    override def toDynamic: DynamicValue = DynamicValue.NoneValue // Placeholder or actual default
  }

  case class Constant[A](value: A, schema: Schema[A]) extends SchemaExpr[A] {
    override def toDynamic: DynamicValue = schema.toDynamic(value)
  }

  // Primitive transformations could be added here
}
