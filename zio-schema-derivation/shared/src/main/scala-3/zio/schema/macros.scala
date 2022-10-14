package zio.schema

import scala.quoted._
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValueTuple}
import Schema._

object DeriveSchema {

  transparent inline def gen[T]: Schema[T] = ${ deriveSchema[T] }

  def deriveSchema[T: Type](using Quotes): Expr[Schema[T]] = 
    DeriveSchema().deriveSchema[T](top = true)
}

private case class DeriveSchema()(using val ctx: Quotes) extends ReflectionUtils(ctx) {
  import ctx.reflect._
   
  case class Frame(ref: Term, tpe: TypeRepr)
  case class Stack(frames: List[Frame]) {
    def find(tpe: TypeRepr): Option[Term] = frames.find(_.tpe =:= tpe).map(_.ref)

    def push(ref: Term, tpe: TypeRepr): Stack = Stack(Frame(ref, tpe) :: frames)

    def pop: Stack = Stack(frames.tail)

    def size = frames.size

    override def toString = 
      frames.map(f => s"${f.ref.show} : ${f.tpe.show}").mkString("Stack(", ", ", ")")
  }

  object Stack {
    val empty: Stack = Stack(Nil)
  }

  var depth = 0

  def deriveSchema[T: Type](stack: Stack = Stack.empty, top: Boolean = false): Expr[Schema[T]] = {
    depth += 1
    // println(s"STACK ${stack} ${TypeRepr.of[T].show}")
    if (depth > 85)
      throw new Exception("Schema derivation exceeded")

    val typeRepr = TypeRepr.of[T]
    val result = stack.find(typeRepr) match {
      case Some(ref) =>
        '{ Schema.defer(${ref.asExprOf[Schema[T]]}) }
      case None => 
        typeRepr.asType match {
          case '[List[a]] =>
            val schema = deriveSchema[a](stack)
            '{ Schema.list(Schema.defer(${schema})) }.asExprOf[Schema[T]]
          case '[scala.util.Either[a, b]] =>
            val schemaA = deriveSchema[a](stack)
            val schemaB = deriveSchema[b](stack)
            '{ Schema.either(Schema.defer(${schemaA}), Schema.defer(${schemaB})) }.asExprOf[Schema[T]]
          case '[Option[a]] =>
            val schema = deriveSchema[a](stack)
            // throw new Error(s"OPITOS ${schema.show}")
            '{ Schema.option(Schema.defer($schema)) }.asExprOf[Schema[T]]
          case '[scala.collection.Set[a]] =>
            val schema = deriveSchema[a](stack)
            '{ Schema.set(Schema.defer(${schema})) }.asExprOf[Schema[T]]
          case '[Vector[a]] =>
            val schema = deriveSchema[a](stack)
            '{ Schema.vector(Schema.defer(${schema})) }.asExprOf[Schema[T]]
          case '[scala.collection.Map[a, b]] =>
            val schemaA = deriveSchema[a](stack)
            val schemaB = deriveSchema[b](stack)
            '{ Schema.map(Schema.defer(${schemaA}), Schema.defer(${schemaB})) }.asExprOf[Schema[T]]
          case '[zio.Chunk[a]] =>
            val schema = deriveSchema[a](stack)
            '{ Schema.chunk(Schema.defer(${schema})) }.asExprOf[Schema[T]]
          case _ => 
            Expr.summon[Schema[T]] match {
              case Some(schema) if !top =>
                // println(s"FOR TYPE ${typeRepr.show}")
                // println(s"STACK ${stack.find(typeRepr)}")
                // println(s"Found schema ${schema.show}")
                schema
              case _ =>
                // println(s"TYPE REPR ${typeRepr.show}")
                val mirror = Mirror(typeRepr).get
                mirror.mirrorType match {
                  case MirrorType.Sum => 
                    deriveEnum[T](mirror, stack)
                  case MirrorType.Product =>
                    deriveCaseClass[T](mirror, stack, top)
                }
            }
        }
    }

    // println()
    // println()
    // println(s"RESULT ${typeRepr.show}")
    // println(s"------")
    // println(s"RESULT ${result.show}")
    // println("HELLO")

    result
  }

  def deriveCaseClass[T: Type](mirror: Mirror, stack: Stack, top: Boolean) = {
    val selfRefSymbol = Symbol.newVal(Symbol.spliceOwner, s"derivedSchema${stack.size}", TypeRepr.of[Schema[T]], Flags.Lazy, Symbol.spliceOwner)
    val selfRef = Ref(selfRefSymbol)
    val newStack = stack.push(selfRef, TypeRepr.of[T])

    val labels = mirror.labels.toList
    val types = mirror.types.toList
    val typesAndLabels = types.zip(labels)

    val paramAnns = fromConstructor(TypeRepr.of[T].typeSymbol)
    val fields = typesAndLabels.map { case (tpe, label) => deriveField[T](tpe, label, paramAnns.getOrElse(label, List.empty), newStack) }
    val constructor = caseClassConstructor[T](mirror).asExpr

    val annotationExprs = TypeRepr.of[T].typeSymbol.annotations.filter(filterAnnotation).map(_.asExpr)
    val annotations = '{ zio.Chunk.fromIterable(${Expr.ofSeq(annotationExprs)}) }
    val typeInfo = '{TypeId.parse(${Expr(TypeRepr.of[T].show)})}
    val args = List(typeInfo) ++ fields ++ Seq(constructor) ++  Seq(annotations)
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

    val lazyValDef = ValDef(selfRefSymbol, Some(applied.changeOwner(selfRefSymbol)))

    applied.asExpr match {
      case '{ type tt <: Schema[T]; $ex : `tt` } =>
        '{
          ${Block(
            List(lazyValDef), 
            selfRef
          ).asExpr}.asInstanceOf[tt]
        }
    }
  }


  private def fromDeclarations(from: Symbol): List[(String, List[Expr[Any]])] = 
    from.declaredFields.map {
      field =>
        field.name -> field.annotations.filter(filterAnnotation).map(_.asExpr)
    }

  private def fromConstructor(from: Symbol): scala.collection.Map[String, List[Expr[Any]]] =
      from.primaryConstructor.paramSymss.flatten.map { field =>
        field.name -> field.annotations
          .filter(filterAnnotation)
          .map(_.asExpr.asInstanceOf[Expr[Any]])
      }.toMap

  //   sealed case class Enum2[A1 <: Z, A2 <: Z, Z](
  //      case1: Case[A1, Z], case2: Case[A2, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
  def deriveEnum[T: Type](mirror: Mirror, stack: Stack) = {
    val selfRefSymbol = Symbol.newVal(Symbol.spliceOwner, s"derivedSchema${stack.size}", TypeRepr.of[Schema[T]], Flags.Lazy, Symbol.spliceOwner)
    val selfRef = Ref(selfRefSymbol)
    val newStack = stack.push(selfRef, TypeRepr.of[T])

    val labels = mirror.labels.toList
    val types = mirror.types.toList
    val typesAndLabels = types.zip(labels)

    val cases = typesAndLabels.map { case (tpe, label) => deriveCase[T](tpe, label, newStack) }

    val annotationExprs = TypeRepr.of[T].typeSymbol.annotations.filter(filterAnnotation).map(_.asExpr)
    val annotations = '{ zio.Chunk.fromIterable(${Expr.ofSeq(annotationExprs)}) }

    val typeInfo = '{TypeId.parse(${Expr(TypeRepr.of[T].show)})}
    val args = List(typeInfo) ++ cases :+ annotations
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

    val lazyValDef = ValDef(selfRefSymbol, Some(applied.changeOwner(selfRefSymbol)))

    applied.asExpr match {
      case '{ type tt <: Schema[T]; $ex : `tt` } =>
        '{
          ${Block(
            List(lazyValDef), 
            selfRef
          ).asExpr}.asInstanceOf[tt]
        }
    }
  }


  // Derive Field for a CaseClass
  def deriveField[T: Type](repr: TypeRepr, name: String, anns: List[Expr[Any]], stack: Stack) = {
    import zio.schema.validation.Validation

    val tpe = TypeRepr.of[T]
    val s = tpe.typeSymbol.declaredFields
    val interestingField = s.find (_.name == name).get
    val ct = tpe.memberType (interestingField)

    ct.asType match { case '[t] =>
      val schema = deriveSchema[t](stack)
      val validators = anns.collect {
        case ann if ann.isExprOf[Validation[t]] => ann.asExprOf[Validation[t]]
      }
      val validator: Expr[Validation[t]] = validators.foldLeft[Expr[Validation[t]]]('{Validation.succeed}){
        case (acc, expr) => '{
          $acc && $expr
        }
      }

      val typeParams = TypeRepr.of[T].dealias match
           case AppliedType (t, params) => params
           case _ => Nil

      val get = '{ (t: T) => ${Select.unique('t.asTerm, name).asExprOf[t]} }
      val set = '{ (ts: T, v: t) => ${Select.overloaded('ts.asTerm, "copy", typeParams, List(NamedArg(name, 'v.asTerm))).asExprOf[T]} }
      val chunk = '{ zio.Chunk.fromIterable(${ Expr.ofSeq(anns.reverse) }) }
      '{ Field(${Expr(name)}, $schema, $chunk, $validator, $get, $set) }
    }
  }

  // sealed case class Case[A, Z](id: String, codec: Schema[A], unsafeDeconstruct: Z => A, annotations: Chunk[Any] = Chunk.empty) {
  def deriveCase[T: Type](repr: TypeRepr, label: String, stack: Stack) = {
    repr.asType match { case '[t] => 
      val schema = deriveSchema[t](stack)
      val stringExpr = Expr(label)

      val annotationExprs = TypeRepr.of[t].typeSymbol.annotations.filter(filterAnnotation).map(_.asExpr)
      val annotations = '{ zio.Chunk.fromIterable(${Expr.ofSeq(annotationExprs)}) }

      val unsafeDeconstruct = '{ 
        (z: T) => z match {
          case (sub: t) => sub
          case other => throw new MatchError(other)
        }
       }
      '{ Case(${Expr(label)}, $schema, $unsafeDeconstruct, $annotations) }
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

  private def filterAnnotation(a: Term): Boolean =
    a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
      a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"

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

