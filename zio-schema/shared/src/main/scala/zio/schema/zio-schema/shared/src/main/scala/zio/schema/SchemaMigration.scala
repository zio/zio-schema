package zio.schema.migration

import scala.quoted._
import zio.schema._
import zio.Chunk

object AtomicMigrationDeriver {
  inline def derive[A, B]: Migration = ${ deriveImpl[A, B] }

  def deriveImpl[A: Type, B: Type](using Quotes): Expr[Migration] = {
    import quotes.reflect._
    
    def internalDerive[T1: Type, T2: Type](stack: Set[TypeRepr]): Expr[Migration] = {
      val t1 = TypeRepr.of[T1]; val t2 = TypeRepr.of[T2]

      if (stack.exists(_ =:= t1)) '{ Migration.Combined(Chunk.empty) }
      else {
        val nextStack = stack + t1
        val aFields = t1.typeSymbol.caseFields
        val bFields = t2.typeSymbol.caseFields

        val fromA = aFields.flatMap { aSym =>
          val aFTpe = t1.memberType(aSym)
          bFields.find(_.name == aSym.name) match {
            case Some(bSym) =>
              val bFTpe = t2.memberType(bSym)
              if (aFTpe =:= bFTpe) Nil
              else (aFTpe.asType, bFTpe.asType) match {
                case ('[at], '[bt]) =>
                  val sA = Expr.summon[Schema[at]].getOrElse(report.errorAndAbort(s"No Schema A"))
                  val sB = Expr.summon[Schema[bt]].getOrElse(report.errorAndAbort(s"No Schema B"))
                  
                  val transform = if (aFTpe <:< TypeRepr.of[Int] && bFTpe <:< TypeRepr.of[Long])
                    '{ (v: at) => v.asInstanceOf[Int].toLong.asInstanceOf[bt] }
                  else if (aFTpe <:< TypeRepr.of[Float] && bFTpe <:< TypeRepr.of[Double])
                    '{ (v: at) => v.asInstanceOf[Float].toDouble.asInstanceOf[bt] }
                  else '{ (v: at) => v.asInstanceOf[bt] }

                  List('{ Migration.Transform(${Expr(aSym.name)}, $sA, $sB, $transform) })
              }
            case None => 
              aFTpe.asType match {
                case '[at] =>
                  val sA = Expr.summon[Schema[at]].getOrElse(report.errorAndAbort("No Schema found"))
                  List('{ Migration.RemoveField(${Expr(aSym.name)}, $sA, DynamicValue.None) })
              }
          }
        }

        val added = bFields.filterNot(b => aFields.exists(_.name == b.name)).map { bSym =>
          val bFTpe = t2.memberType(bSym)
          bFTpe.asType match {
            case '[bt] =>
              val sB = Expr.summon[Schema[bt]].getOrElse(report.errorAndAbort(s"No Schema for ${bSym.name}"))
              val defaultVal = if (bFTpe <:< TypeRepr.of[Int]) '{ DynamicValue.Primitive(0, StandardType.IntType) }
                else if (bFTpe <:< TypeRepr.of[Long]) '{ DynamicValue.Primitive(0L, StandardType.LongType) }
                else if (bFTpe <:< TypeRepr.of[String]) '{ DynamicValue.Primitive("", StandardType.StringType) }
                else if (bFTpe <:< TypeRepr.of[Boolean]) '{ DynamicValue.Primitive(false, StandardType.BoolType) }
                else '{ DynamicValue.None }

              '{ Migration.AddField(${Expr(bSym.name)}, $sB, $defaultVal) }
          }
        }
        '{ Migration.Combined(Chunk.fromIterable(${Expr.ofList(fromA ++ added.toList)})) }
      }
    }
    internalDerive[A, B](Set.empty)
  }
}
