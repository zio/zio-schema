package zio.schema.internal

trait DeriveTypeId { self : TypeId.type =>
  inline def gen[T](): TypeId =  ${DeriveTypeId.genTypeIdImpl[T]}
}

object DeriveTypeId {
 
  def genTypeIdImpl(using ctx: Quotes): Expr[TypeId] = {
    import ctx.reflect._
    //todo
    '{ ??? }
  }
}