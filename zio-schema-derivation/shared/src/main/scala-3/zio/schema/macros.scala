package zio.schema

import scala.quoted._
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValueTuple}
import Schema.{Tuple => SchemaTuple, _}

object DeriveSchema {

  transparent inline implicit def gen[T]: Schema[T] = ${ deriveSchema[T] }

  def deriveSchema[T: Type](using Quotes): Expr[Schema[T]] = 
    DeriveSchema().deriveSchema[T]
}

private case class DeriveSchema()(using val ctx: Quotes) extends ReflectionUtils(ctx) {
  import ctx.reflect._

  def deriveSchema[T: Type]: Expr[Schema[T]] = {
    val typeRepr = TypeRepr.of[T]
    val mirror = Mirror(typeRepr).get
    mirror.mirrorType match {
      case MirrorType.Sum => 
        ???
      case MirrorType.Product =>
        deriveCaseClass[T](mirror)
    }
  }

  def deriveCaseClass[T: Type](mirror: Mirror) = {
    val labels = mirror.labels.toList
    val types = mirror.types.toList
    val typesAndLabels = types.zip(labels)

    val fields = typesAndLabels.map { case (tpe, label) => deriveField(tpe, label) }
    val selects = typesAndLabels.map { case (tpe, label) => deriveSelect[T](tpe, label) }
    val constructor = deriveConstructor[T](mirror).asExpr
    val args = fields ++ Seq(constructor) ++ selects ++ Seq('{ zio.Chunk.empty })
    val terms = Expr.ofTupleFromSeq(args)
    val ctor = TypeRepr.of[CaseClass2[_, _, _]].typeSymbol.primaryConstructor

    val typeArgs = 
      (types.appended(TypeRepr.of[T])).map { tpe =>
        tpe.asType match
          case '[tt] => TypeTree.of[tt]
      }

    val typeTree = caseClassTypeTree[T](labels.length)

    val applied = Apply(
      TypeApply(
        Select(New(typeTree), ctor),
        typeArgs
      ),
      args.map(_.asTerm)
    )
    applied.asExprOf[Schema[T]]
  }

  def deriveField(repr: TypeRepr, label: String) = {
    repr.asType match { case '[t] => 
      val schema = Expr.summon[Schema[t]].getOrElse(deriveSchema[t])
      '{ Field(${Expr(label)}, $schema) }
    }
  }

  def deriveConstructor[T: Type](mirror: Mirror) = {
    val product = Expr.summon[scala.deriving.Mirror.ProductOf[T]].get
    val methodType = MethodType(mirror.labels.toList)(_ => mirror.types.toList, _ => TypeRepr.of[T])
    Lambda(Symbol.spliceOwner, methodType, { (sym, reprs) =>
      val tupled = Expr.ofTupleFromSeq(reprs.map(_.asExpr))
      Select.overloaded(product.asTerm, "fromProduct", List.empty, List(tupled.asTerm))
    })
  }

  def deriveSelect[T: Type](typeRepr: TypeRepr, label: String) = {
    typeRepr.asType match {
      case '[t] =>
        '{ (t: T) => ${Select.unique('t.asTerm, label).asExprOf[t]} }
    }
  }

  def caseClassTypeTree[T: Type](arity: Int): TypeTree = 
    arity match {
      // case 0 => TypeTree.of[CaseClass0[T]]
      case 1 => TypeTree.of[CaseClass1[_, T]]
      case 2 => TypeTree.of[CaseClass2[_, _, T]]
      case 3 => TypeTree.of[CaseClass3[_, _, _, T]]
      case 4 => TypeTree.of[CaseClass4[_, _, _, _, T]]
      case 5 => TypeTree.of[CaseClass5[_, _, _, _, _, T]]
      case 6 => TypeTree.of[CaseClass6[_, _, _, _, _, _, T]]
      case 7 => TypeTree.of[CaseClass7[_, _, _, _, _, _, _, T]]
      case 8 => TypeTree.of[CaseClass8[_, _, _, _, _, _, _, _, T]]
      case 9 => TypeTree.of[CaseClass9[_, _, _, _, _, _, _, _, _, T]]
      case 10 => TypeTree.of[CaseClass10[_, _, _, _, _, _, _, _, _, _, T]]
      case 11 => TypeTree.of[CaseClass11[_, _, _, _, _, _, _, _, _, _, _, T]]
      case 12 => TypeTree.of[CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 13 => TypeTree.of[CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 14 => TypeTree.of[CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 15 => TypeTree.of[CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 16 => TypeTree.of[CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 17 => TypeTree.of[CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 18 => TypeTree.of[CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 19 => TypeTree.of[CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 20 => TypeTree.of[CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 21 => TypeTree.of[CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 22 => TypeTree.of[CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
    }
    

}

