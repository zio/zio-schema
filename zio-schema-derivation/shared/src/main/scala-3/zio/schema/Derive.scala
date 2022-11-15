package zio.schema

import scala.quoted._
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValueTuple}
import Schema._

object Derive {
  inline def derive[F[_], A](deriver: Deriver[F])(implicit schema: Schema[A]): F[A] = ${ deriveInstance[F, A]('deriver, 'schema) }

  private def deriveInstance[F[_]: Type, A: Type](deriver: Expr[Deriver[F]], schema: Expr[Schema[A]])(using Quotes): Expr[F[A]] =
    DeriveInstance().deriveInstance[F, A](deriver, schema, top = true)
}

private case class DeriveInstance()(using val ctx: Quotes) extends ReflectionUtils(ctx) {
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

  def summonOptional[F[_]: Type, A: Type](using Quotes): Expr[Option[F[A]]] =
    Expr.summon[F[A]] match {
      case Some(instance) => '{Some($instance)}
      case None => '{None}
    }

  def createSchemaRef[T: Type, S <: Schema[T]: Type](stack: Stack)(using Quotes): (Ref, Expr[S]) = {
    val schemaSymbol = Symbol.newVal(Symbol.spliceOwner, s"schema${stack.size}", TypeRepr.of[S], Flags.Lazy, Symbol.spliceOwner)
    val schemaRef = Ref(schemaSymbol)
    val schemaRefExpr = schemaRef.asExprOf[S]
    
    (schemaRef, schemaRefExpr)
  }

  def deriveInstance[F[_]: Type, A: Type](deriver: Expr[Deriver[F]], schema: Expr[Schema[A]], stack: Stack = Stack.empty, top: Boolean = false)(using Quotes): Expr[F[A]] = {
    depth += 1
    if (depth > 1024)
      throw new Exception("Type class derivation exceeded")

    val typeRepr = TypeRepr.of[A]
    val typeReprF = TypeRepr.of[F]

    val result = stack.find(typeRepr) match {
      case Some(ref) =>
        '{ ${ref.asExprOf[F[A]]} }
      case None => 
        val selfRefSymbol = Symbol.newVal(Symbol.spliceOwner, s"derivedInstance${stack.size}", TypeRepr.of[F[A]], Flags.Lazy, Symbol.spliceOwner)
        val selfRef = Ref(selfRefSymbol)

        println(typeRepr.show)
        Expr.summon[StandardType[A]] match {
          case Some(st) =>
            val summoned = summonOptional[F, A]

            '{ $deriver.derivePrimitive[A]($st, $summoned) }
          case None =>                    
          typeRepr.asType match {
            case '[List[a]] =>
              val (seqSchemaRef, seqSchemaRefExpr) = createSchemaRef[List[a], Schema.Sequence[List[a], a, _]](stack)              

              val summoned = summonOptional[F, List[a]]
                                
              val elemInstance = deriveInstance[F, a](deriver, '{$seqSchemaRefExpr.elementSchema}, stack)
              '{ lazy val $seqSchemaRef = Schema.force($schema).asInstanceOf[Schema.Sequence[List[a], a, _]]
                 lazy val $selfRef = $deriver.deriveSequence[List, a]($seqSchemaRefExpr, $elemInstance, ${summoned})
                 ${selfRef.asExprOf[F[A]]}
              }
            case '[scala.util.Either[a, b]] =>
              val (schemaRef, schemaRefExpr) = createSchemaRef[scala.util.Either[a, b], Schema.Either[a, b]](stack)              
              val summoned = summonOptional[F, scala.util.Either[a, b]]

              val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.left}, stack)
              val instanceB = deriveInstance[F, b](deriver, '{$schemaRefExpr.right}, stack)
              '{ lazy val $schemaRef = Schema.force($schema).asInstanceOf[Schema.Either[a, b]]
                 lazy val $selfRef = $deriver.deriveEither[a, b]($schemaRefExpr, $instanceA, $instanceB, $summoned)
                 ${selfRef.asExprOf[F[A]]}
              }
            case '[Option[a]] =>
              val (schemaRef, schemaRefExpr) = createSchemaRef[Option[a], Schema.Optional[a]](stack)              
              val summoned = summonOptional[F, Option[a]]

              val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.schema}, stack)              
              '{ lazy val $schemaRef = Schema.force($schema).asInstanceOf[Schema.Optional[a]]
                 lazy val $selfRef = $deriver.deriveOption[a]($schemaRefExpr, $instanceA, $summoned)
                 ${selfRef.asExprOf[F[A]]}
              }
            case '[scala.collection.immutable.Set[a]] =>
              val (schemaRef, schemaRefExpr) = createSchemaRef[scala.collection.immutable.Set[a], Schema.Set[a]](stack)              
              val summoned = summonOptional[F, scala.collection.immutable.Set[a]]

              val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.elementSchema}, stack)              
              '{ lazy val $schemaRef = Schema.force($schema).asInstanceOf[Schema.Set[a]]
                 lazy val $selfRef = $deriver.deriveSet[a]($schemaRefExpr, $instanceA, $summoned)
                 ${selfRef.asExprOf[F[A]]}
              }
            case '[Vector[a]] =>
              val (schemaRef, schemaRefExpr) = createSchemaRef[scala.collection.immutable.Vector[a], Schema.Sequence[Vector[a], a, _]](stack)              
              val summoned = summonOptional[F, scala.collection.immutable.Vector[a]]

              val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.elementSchema}, stack)              
              '{ lazy val $schemaRef = Schema.force($schema).asInstanceOf[Schema.Sequence[Vector[a], a, _]]
                 lazy val $selfRef = $deriver.deriveSequence[Vector, a]($schemaRefExpr, $instanceA, $summoned)
                 ${selfRef.asExprOf[F[A]]}
              }
            // case '[scala.collection.Map[a, b]] =>
            //   val schemaA = deriveSchema[a](stack)
            //   val schemaB = deriveSchema[b](stack)
            //   '{ Schema.map(Schema.defer(${schemaA}), Schema.defer(${schemaB})) }.asExprOf[Schema[T]]
            // case '[zio.Chunk[a]] =>
            //   val schema = deriveSchema[a](stack)
            //   '{ Schema.chunk(Schema.defer(${schema})) }.asExprOf[Schema[T]]
            // case _ => 
            //   Expr.summon[Schema[T]] match {
            //     case Some(schema) if !top =>
            //       // println(s"FOR TYPE ${typeRepr.show}")
            //       // println(s"STACK ${stack.find(typeRepr)}")
            //       // println(s"Found schema ${schema.show}")
            //       schema
            //     case _ =>
            //       Mirror(typeRepr) match {
            //         case Some(mirror) =>
            //           mirror.mirrorType match {
            //             case MirrorType.Sum =>
            //               deriveEnum[T](mirror, stack)
            //             case MirrorType.Product =>
            //              deriveCaseClass[T](mirror, stack, top)
            //           }
            //         case None =>
            //           val sym = typeRepr.typeSymbol
            //           if (sym.isClassDef && sym.flags.is(Flags.Module)) {
            //             deriveCaseObject[T](stack, top)
            //           }
            //           else {
            //             report.errorAndAbort(s"Deriving schema for ${typeRepr.show} is not supported")
            //           }
            //     }
              }
            }
        }

    println()
    println()
    println(s"RESULT ${typeRepr.show}")
    println(s"------")
    println(s"RESULT ${result.show}")

    result
  }

//   def deriveCaseObject[T: Type](stack: Stack, top: Boolean)(using Quotes) = {
//     val selfRefSymbol = Symbol.newVal(Symbol.spliceOwner, s"derivedSchema${stack.size}", TypeRepr.of[Schema[T]], Flags.Lazy, Symbol.spliceOwner)
//     val selfRef = Ref(selfRefSymbol)

//     val typeInfo = '{TypeId.parse(${Expr(TypeRepr.of[T].show)})}
//     val annotationExprs = TypeRepr.of[T].typeSymbol.annotations.filter (filterAnnotation).map (_.asExpr)
//     val annotations = '{zio.Chunk.fromIterable (${Expr.ofSeq (annotationExprs)})}

//     val constructor = '{() => ${Ref(TypeRepr.of[T].typeSymbol.companionModule).asExprOf[T]}}
//     val ctor = typeRprOf[T](0).typeSymbol.companionModule
//     val args = List(typeInfo, constructor, annotations)

//     val applied = Apply(
//       TypeApply(
//         Select.unique(Ref(ctor), "apply"),
//         List(TypeRepr.of[T].asType match {
//           case '[tt] => TypeTree.of[tt]
//         })),
//       args.map(_.asTerm)
//     )

//     val lazyValDef = ValDef(selfRefSymbol, Some(applied.changeOwner(selfRefSymbol)))

//     applied.asExpr match {
//       case '{ type tt <: Schema[T]; $ex : `tt` } =>
//         '{
//           ${Block(
//             List(lazyValDef),
//             selfRef
//           ).asExpr}.asInstanceOf[tt]
//         }
//     }
//   }

//   def deriveCaseClass[T: Type](mirror: Mirror, stack: Stack, top: Boolean)(using Quotes) = {
//     val selfRefSymbol = Symbol.newVal(Symbol.spliceOwner, s"derivedSchema${stack.size}", TypeRepr.of[Schema[T]], Flags.Lazy, Symbol.spliceOwner)
//     val selfRef = Ref(selfRefSymbol)
//     val newStack = stack.push(selfRef, TypeRepr.of[T])

//     val labels = mirror.labels.toList
//     val types = mirror.types.toList
//     val typesAndLabels = types.zip(labels)

//     val paramAnns = fromConstructor(TypeRepr.of[T].typeSymbol)
//     val constructor = caseClassConstructor[T](mirror).asExpr

//     val annotationExprs = TypeRepr.of[T].typeSymbol.annotations.filter(filterAnnotation).map(_.asExpr)
//     val annotations = '{ zio.Chunk.fromIterable(${Expr.ofSeq(annotationExprs)}) }
//     val typeInfo = '{TypeId.parse(${Expr(TypeRepr.of[T].show)})}

//     val applied = if (labels.length <= 22) {
//       val ctor = typeRprOf[T](labels.length).typeSymbol.companionModule

//       val typeArgs =
//         (types.appended(TypeRepr.of[T])).map { tpe =>
//             tpe.asType match
//               case '[tt] => TypeTree.of[tt]
//         }
//       val typeAppliedCtor = TypeApply(
//             Select.unique(Ref(ctor), "apply"),
//             typeArgs
//           )

//       val fields = typesAndLabels.map { case (tpe, label) => deriveField[T](tpe, label, paramAnns.getOrElse(label, List.empty), newStack) }
//       val args = List(typeInfo) ++ fields ++ Seq(constructor) ++  Seq(annotations)
//       val terms = Expr.ofTupleFromSeq(args)

//       Apply(
//         typeAppliedCtor,
//         args.map(_.asTerm)
//       )
//     } else {
//        val fields = typesAndLabels.map { case (tpe, label) => deriveGenericField[T](tpe, label, paramAnns.getOrElse(label, List.empty), newStack) }
//        val genericRecord = '{GenericRecord($typeInfo, FieldSet.fromFields(${Varargs(fields)} : _*), $annotations)}

//        val s = TypeRepr.of[T].typeSymbol.declaredFields

//        def casts(m: Expr[ListMap[String, _]])(using Quotes) =
//          typesAndLabels.map { case (tpe, label) =>
//            val interestingField = s.find (_.name == label)
//            val fieldType = interestingField match {
//              case Some(interestingField) =>
//                val ct = tpe.memberType (interestingField)
//                ct.asType
//              case None =>
//                tpe.asType
//            }
//            fieldType match { case '[t] =>
//             '{ try ${m}.apply(${Expr(label)}).asInstanceOf[t]
//                catch {
//                  case _: ClassCastException => throw new RuntimeException("Field " + ${Expr(label)} + " has invalid type")
//                  case _: Throwable => throw new RuntimeException("Field " + ${Expr(label)} + " is missing")
//                }
//             }.asTerm
//            }
//          }

//        def appliedConstructor(m: Expr[ListMap[String, _]])(using Quotes) = {
//          Apply(Select.unique(Ref(TypeRepr.of[T].typeSymbol.companionModule), "apply"), casts(m)).asExprOf[T]
//        }

//        val fromMap = '{ (m: ListMap[String, _]) =>
//          try { Right(${appliedConstructor('m)}) } catch {
//          case e: Throwable  => Left(e.getMessage)
//        }}

//        def tuples(b: Expr[T])(using Quotes) =
//          typesAndLabels.map { case (tpe, label) =>
//            val interestingField = s.find (_.name == label)
//            val fieldType = interestingField match {
//              case Some(interestingField) =>
//                 val ct = tpe.memberType (interestingField)
//                 ct.asType
//               case None =>
//                 tpe.asType
//             }
//            fieldType match { case '[t] =>
//              '{(${Expr(label)}, ${Select.unique(b.asTerm, label).asExprOf[t]})}
//            }
//          }
//        val toMap = '{(b: T) => Right(ListMap.apply(${Varargs(tuples('b))} :_*)) }

//        '{${genericRecord.asExprOf[GenericRecord]}.transformOrFail[T]($fromMap, $toMap)}.asTerm
//     }

//     val lazyValDef = ValDef(selfRefSymbol, Some(applied.changeOwner(selfRefSymbol)))

//     applied.asExpr match {
//       case '{ type tt <: Schema[T]; $ex : `tt` } =>
//         '{
//           ${Block(
//             List(lazyValDef),
//             selfRef
//           ).asExpr}.asInstanceOf[tt]
//         }
//     }
//   }


//   private def fromDeclarations(from: Symbol): List[(String, List[Expr[Any]])] = 
//     from.declaredFields.map {
//       field =>
//         field.name -> field.annotations.filter(filterAnnotation).map(_.asExpr)
//     }

//   private def fromConstructor(from: Symbol): scala.collection.Map[String, List[Expr[Any]]] =
//       from.primaryConstructor.paramSymss.flatten.map { field =>
//         field.name -> field.annotations
//           .filter(filterAnnotation)
//           .map(_.asExpr.asInstanceOf[Expr[Any]])
//       }.toMap

//   //   sealed case class Enum2[A1 <: Z, A2 <: Z, Z](
//   //      case1: Case[A1, Z], case2: Case[A2, Z], annotations: Chunk[Any] = Chunk.empty) extends Enum[Z] { self =>
//   def deriveEnum[T: Type](mirror: Mirror, stack: Stack)(using Quotes) = {
//     val selfRefSymbol = Symbol.newVal(Symbol.spliceOwner, s"derivedSchema${stack.size}", TypeRepr.of[Schema[T]], Flags.Lazy, Symbol.spliceOwner)
//     val selfRef = Ref(selfRefSymbol)
//     val newStack = stack.push(selfRef, TypeRepr.of[T])

//     val labels = mirror.labels.toList
//     val types = mirror.types.toList
//     val typesAndLabels = types.zip(labels)

//     val cases = typesAndLabels.map { case (tpe, label) => deriveCase[T](tpe, label, newStack) }

//     val annotationExprs = TypeRepr.of[T].typeSymbol.annotations.filter(filterAnnotation).map(_.asExpr)
//     val annotations = '{ zio.Chunk.fromIterable(${Expr.ofSeq(annotationExprs)}) }

//     val typeInfo = '{TypeId.parse(${Expr(TypeRepr.of[T].show)})}
//     val args = List(typeInfo) ++ cases :+ annotations
//     val terms = Expr.ofTupleFromSeq(args)
//     val ctor = TypeRepr.of[Enum2[_, _, _]].typeSymbol.primaryConstructor

//     val typeArgs = 
//       (types.appended(TypeRepr.of[T])).map { tpe =>
//         tpe.asType match
//           case '[tt] => TypeTree.of[tt]
//       }

//     val typeTree = enumTypeTree[T](labels.length)

//     val applied = Apply(
//       TypeApply(
//         Select(New(typeTree), ctor),
//         typeArgs
//       ),
//       args.map(_.asTerm)
//     )

//     val lazyValDef = ValDef(selfRefSymbol, Some(applied.changeOwner(selfRefSymbol)))

//     applied.asExpr match {
//       case '{ type tt <: Schema[T]; $ex : `tt` } =>
//         '{
//           ${Block(
//             List(lazyValDef), 
//             selfRef
//           ).asExpr}.asInstanceOf[tt]
//         }
//     }
//   }


//   // Derive Field for a CaseClass
//   def deriveField[T: Type](repr: TypeRepr, name: String, anns: List[Expr[Any]], stack: Stack)(using Quotes) = {
//     import zio.schema.validation.Validation
//     import zio.schema.annotation.validate

//     val tpe = TypeRepr.of[T]
//     val s = tpe.typeSymbol.declaredFields
//     val interestingField = s.find (_.name == name)

//     val fieldType = interestingField match {
//       case Some(interestingField) =>
//         val ct = tpe.memberType (interestingField)
//         ct.asType
//       case None =>
//         repr.asType
//     }
//     fieldType match { case '[t] =>
//       val schema = deriveSchema[t](stack)
//       val validations = anns.collect {
//         case ann if ann.isExprOf[validate[t]] => ann.asExprOf[validate[t]]
//       }
//       val validator: Expr[Validation[t]] = validations.foldLeft[Expr[Validation[t]]]('{Validation.succeed}){
//         case (acc, expr) => '{
//           $acc && ${expr}.validation
//         }
//       }

//       val typeParams = TypeRepr.of[T].dealias match
//            case AppliedType (t, params) => params
//            case _ => Nil

//       val get = '{ (t: T) => ${Select.unique('t.asTerm, name).asExprOf[t]} }
//       val set = '{ (ts: T, v: t) => ${Select.overloaded('ts.asTerm, "copy", typeParams, List(NamedArg(name, 'v.asTerm))).asExprOf[T]} }
//       val chunk = '{ zio.Chunk.fromIterable(${ Expr.ofSeq(anns.reverse) }) }

//       if (anns.nonEmpty) {
//         val newName = anns.collectFirst {
//           case ann if ann.isExprOf[fieldName] => '{${ann.asExprOf[fieldName]}.name}
//         }.getOrElse(Expr(name))

//         '{ Field($newName, $schema, $chunk, $validator, $get, $set) }
//       } else {
//         '{ Field(${Expr(name)}, $schema, $chunk, $validator, $get, $set) }
//       }
//     }
//   }

// // Derive Field for a GenericRecord
//   def deriveGenericField[T: Type](repr: TypeRepr, name: String, anns: List[Expr[Any]], stack: Stack)(using Quotes) = {
//     import zio.schema.validation.Validation
//     import zio.schema.annotation.validate

//     val tpe = TypeRepr.of[T]
//     val s = tpe.typeSymbol.declaredFields
//     val interestingField = s.find (_.name == name)

//     val fieldType = interestingField match {
//       case Some(interestingField) =>
//         val ct = tpe.memberType (interestingField)
//         ct.asType
//       case None =>
//         repr.asType
//     }
//     fieldType match { case '[t] =>
//       val schema = deriveSchema[t](stack)
//       val validations = anns.collect {
//         case ann if ann.isExprOf[validate[t]] => ann.asExprOf[validate[t]]
//       }
//       val validator: Expr[Validation[t]] = validations.foldLeft[Expr[Validation[t]]]('{Validation.succeed}){
//         case (acc, expr) => '{
//           $acc && ${expr}.validation
//         }
//       }

//       val typeParams = TypeRepr.of[T].dealias match
//            case AppliedType (t, params) => params
//            case _ => Nil

//       val get = '{ (t: ListMap[String, _]) => t.apply(${Expr(name)}).asInstanceOf[t] }
//       val set = '{ (ts: ListMap[String, _], v: t) => ts.updated(${Expr(name)}, v) }
//       val chunk = '{ zio.Chunk.fromIterable(${ Expr.ofSeq(anns.reverse) }) }

//       if (anns.nonEmpty) {
//         val newName = anns.collectFirst {
//           case ann if ann.isExprOf[fieldName] => '{${ann.asExprOf[fieldName]}.name}
//         }.getOrElse(Expr(name))

//         '{ Field($newName, $schema, $chunk, $validator, $get, $set) }
//       } else {
//         '{ Field(${Expr(name)}, $schema, $chunk, $validator, $get, $set) }
//       }
//     }
//   }

//   // sealed case class Case[A, Z](id: String, codec: Schema[A], unsafeDeconstruct: Z => A, annotations: Chunk[Any] = Chunk.empty) {
//   def deriveCase[T: Type](repr: TypeRepr, label: String, stack: Stack)(using Quotes) = {
//     repr.asType match { case '[t] => 
//       val schema = deriveSchema[t](stack)
//       val stringExpr = Expr(label)

//       val annotationExprs = TypeRepr.of[t].typeSymbol.annotations.filter(filterAnnotation).map(_.asExpr)
//       val annotations = '{ zio.Chunk.fromIterable(${Expr.ofSeq(annotationExprs)}) }

//       val unsafeDeconstruct = '{ 
//         (z: T) => z match {
//           case (sub: t) => sub
//           case other => throw new MatchError(other)
//         }
//        }
//        val construct = '{
//         (a: t) => a.asInstanceOf[T]
//        }

//        val isCase = '{ (z: T) => z.isInstanceOf[t] }

//       '{ Case(${Expr(label)}, $schema, $unsafeDeconstruct, $construct, $isCase, $annotations) }
//     }
//   }
}