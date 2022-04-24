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
        deriveEnum[T](mirror)
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
    val constructor = caseClassConstructor[T](mirror).asExpr
    val args = fields ++ Seq(constructor) ++ selects ++ Seq('{ zio.Chunk.empty })
    val terms = Expr.ofTupleFromSeq(args)

    val typeArgs = 
      (types.appended(TypeRepr.of[T])).map { tpe =>
        tpe.asType match
          case '[tt] => TypeTree.of[tt]
      }

    // This seems to work even hard-coded to CaseClass2 🤷‍♂️
    val ctor = TypeRepr.of[CaseClass2[_, _, _]].typeSymbol.primaryConstructor
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



  //   sealed case class Enum2[A1 <: Z, A2 <: Z, Z](
  //      case1: Case[A1, Z], case2: Case[A2, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
  def deriveEnum[T: Type](mirror: Mirror) = {
    val labels = mirror.labels.toList
    val types = mirror.types.toList
    val typesAndLabels = types.zip(labels)

    val cases = typesAndLabels.map { case (tpe, label) => deriveCase[T](tpe, label) }
    val args = cases :+ '{ zio.Chunk.empty }
    val terms = Expr.ofTupleFromSeq(args)
    val ctor = TypeRepr.of[Enum2[_, _, _]].typeSymbol.primaryConstructor

    val typeArgs = 
      (types.appended(TypeRepr.of[T])).map { tpe =>
        tpe.asType match
          case '[tt] => TypeTree.of[tt]
      }

    val typeTree = enumTypeTree[T](labels.length)

    val applied = Apply(
      TypeApply(
        Select(New(typeTree), ctor),
        typeArgs
      ),
      args.map(_.asTerm)
    )
    applied.asExprOf[Schema[T]]
  }

  // Derive Field for a CaseClass
  def deriveField(repr: TypeRepr, label: String) = {
    repr.asType match { case '[t] => 
      val schema = Expr.summon[Schema[t]].getOrElse(deriveSchema[t])
      '{ Field(${Expr(label)}, $schema) }
    }
  }


  // sealed case class Case[A, Z](id: String, codec: Schema[A], unsafeDeconstruct: Z => A, annotations: Chunk[Any] = Chunk.empty) {
  def deriveCase[T: Type](repr: TypeRepr, label: String) = {
    repr.asType match { case '[t] => 
      val schema = Expr.summon[Schema[t]].getOrElse(deriveSchema[t])
      val stringExpr = Expr(label)
      val unsafeDeconstruct = '{ 
        (z: T) => z match {
          case (sub: t) => sub
        }
       }
      '{ Case(${Expr(label)}, $schema, $unsafeDeconstruct) }
    }
  }


  def caseClassConstructor[T: Type](mirror: Mirror) = {
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
      case 0 => TypeTree.of[CaseClass0[T]]
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
    

  def enumTypeTree[T: Type](arity: Int): TypeTree = 
    arity match {
      case 0 => TypeTree.of[CaseClass0[T]]
      case 1 => TypeTree.of[Enum1[_, T]]
      case 2 => TypeTree.of[Enum2[_, _, T]]
      case 3 => TypeTree.of[Enum3[_, _, _, T]]
      case 4 => TypeTree.of[Enum4[_, _, _, _, T]]
      case 5 => TypeTree.of[Enum5[_, _, _, _, _, T]]
      case 6 => TypeTree.of[Enum6[_, _, _, _, _, _, T]]
      case 7 => TypeTree.of[Enum7[_, _, _, _, _, _, _, T]]
      case 8 => TypeTree.of[Enum8[_, _, _, _, _, _, _, _, T]]
      case 9 => TypeTree.of[Enum9[_, _, _, _, _, _, _, _, _, T]]
      case 10 => TypeTree.of[Enum10[_, _, _, _, _, _, _, _, _, _, T]]
      case 11 => TypeTree.of[Enum11[_, _, _, _, _, _, _, _, _, _, _, T]]
      case 12 => TypeTree.of[Enum12[_, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 13 => TypeTree.of[Enum13[_, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 14 => TypeTree.of[Enum14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 15 => TypeTree.of[Enum15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 16 => TypeTree.of[Enum16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 17 => TypeTree.of[Enum17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 18 => TypeTree.of[Enum18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 19 => TypeTree.of[Enum19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 20 => TypeTree.of[Enum20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 21 => TypeTree.of[Enum21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 22 => TypeTree.of[Enum22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
  }

}

