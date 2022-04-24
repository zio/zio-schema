package zio.schema

import scala.quoted._
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValueTuple}
import Schema.{Tuple => SchemaTuple, _}

object DeriveSchema {

  transparent inline def gen[T]= ${ deriveSchema[T] }

  def deriveSchema[T: Type](using Quotes) = 
    DeriveSchema().deriveSchema[T]
}

private case class DeriveSchema()(using val ctx: Quotes) extends ReflectionUtils(ctx) {
  import ctx.reflect._

  def deriveSchema[T: Type] = {
    val typeRepr = TypeRepr.of[T]
    val mirror = Mirror(typeRepr).get
    mirror.mirrorType match {
      case MirrorType.Sum => 
        ???
      case MirrorType.Product =>
        deriveCaseClass[T](mirror)
    }
    // throw new Error(s"Not implemented deriveSchema")
  }

  // sealed case class CaseClass2[A1, A2, Z](
  //   field1: Field[A1], 
  //   field2: Field[A2],
  //   construct: (A1, A2) => Z, 
  //   extractField1: Z => A1,
  //   extractField2: Z => A2, 
  //   override val annotations: Chunk[Any] = Chunk.empty) extends Record[Z] { self =>

  def deriveCaseClass[T: Type](mirror: Mirror) = {
    val labels = mirror.labels
    // val huh:  Int =  mirror.types.map(_.asType)
    mirror.types.map(_.asType).toList match {
      case List('[t1], '[t2]) => 
        val schema1 = Expr.summon[Schema[t1]].get
        val field1 = '{ Field(${Expr(labels(0))}, $schema1)}
        val schema2 = Expr.summon[Schema[t2]].get
        val field2 = '{ Field(${Expr(labels(1))}, $schema2)}
        TypeRepr.of[T].typeSymbol
        val product = Expr.summon[scala.deriving.Mirror.ProductOf[T]].get
        val constructor = '{ (p1: t1, p2: t2) => $product.fromProduct((p1, p2)) } 
        val select1 = '{ (t: T) => ${Select.unique('t.asTerm, labels(0)).asExprOf[t1]} }
        val select2 = '{ (t: T) => ${Select.unique('t.asTerm, labels(1)).asExprOf[t2]} }
        val cc = '{ CaseClass2($field1, $field2, $constructor, $select1, $select2) }
        println(cc.show)
        cc
    }
  }

}

