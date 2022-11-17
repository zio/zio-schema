package zio.schema

import scala.quoted._
import scala.deriving.Mirror
import scala.collection.immutable.ListMap
import scala.compiletime.{erasedValue, summonInline, constValueTuple}
import zio.Chunk
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

  def deriveInstance[F[_]: Type, A: Type](deriver: Expr[Deriver[F]], schema: Expr[Schema[A]], stack: Stack = Stack.empty, top: Boolean = false)(using Quotes): Expr[F[A]] = {
    depth += 1
    if (depth > 1024)
      throw new Exception("Type class derivation exceeded")

    val typeRepr = TypeRepr.of[A]
    val typeReprF = TypeRepr.of[F]

    val result = stack.find(typeRepr) match {
      case Some(ref) =>
        ref.asExprOf[F[A]]
      case None => 
        val selfRefSymbol = Symbol.newVal(Symbol.spliceOwner, s"derivedInstance${stack.size}", TypeRepr.of[F[A]], Flags.Lazy, Symbol.spliceOwner)
        val selfRef = Ref(selfRefSymbol)

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
              lazyVals[F[A]](selfRef,
                seqSchemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Sequence[List[a], a, _]]},
                selfRef -> '{$deriver.deriveSequence[List, a]($seqSchemaRefExpr, $elemInstance, ${summoned})}
              )            
            case '[scala.util.Either[a, b]] =>
              val (schemaRef, schemaRefExpr) = createSchemaRef[scala.util.Either[a, b], Schema.Either[a, b]](stack)              
              val summoned = summonOptional[F, scala.util.Either[a, b]]

              val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.left}, stack)
              val instanceB = deriveInstance[F, b](deriver, '{$schemaRefExpr.right}, stack)
              lazyVals[F[A]](selfRef,
                schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Either[a, b]]},
                selfRef -> '{$deriver.deriveEither[a, b]($schemaRefExpr, $instanceA, $instanceB, $summoned)}
              )
            case '[Option[a]] =>
              val (schemaRef, schemaRefExpr) = createSchemaRef[Option[a], Schema.Optional[a]](stack)              
              val summoned = summonOptional[F, Option[a]]

              val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.schema}, stack)              
              lazyVals[F[A]](selfRef,
                schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Optional[a]]},
                selfRef -> '{$deriver.deriveOption[a]($schemaRefExpr, $instanceA, $summoned)}
              )
            case '[scala.collection.immutable.Set[a]] =>
              val (schemaRef, schemaRefExpr) = createSchemaRef[scala.collection.immutable.Set[a], Schema.Set[a]](stack)              
              val summoned = summonOptional[F, scala.collection.immutable.Set[a]]

              val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.elementSchema}, stack)              
              lazyVals[F[A]](selfRef,
                schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Set[a]]},
                selfRef -> '{$deriver.deriveSet[a]($schemaRefExpr, $instanceA, $summoned)}
              )
            case '[Vector[a]] =>
              val (schemaRef, schemaRefExpr) = createSchemaRef[scala.collection.immutable.Vector[a], Schema.Sequence[Vector[a], a, _]](stack)              
              val summoned = summonOptional[F, scala.collection.immutable.Vector[a]]

              val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.elementSchema}, stack)
              lazyVals[F[A]](selfRef,
                schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Sequence[Vector[a], a, _]]},
                selfRef -> '{$deriver.deriveSequence[Vector, a]($schemaRefExpr, $instanceA, $summoned)}
              )
            case '[scala.collection.immutable.Map[a, b]] =>
              val (schemaRef, schemaRefExpr) = createSchemaRef[scala.collection.immutable.Map[a, b], Schema.Map[a, b]](stack)              
              val summoned = summonOptional[F, scala.collection.immutable.Map[a, b]]

              val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.keySchema}, stack)
              val instanceB = deriveInstance[F, b](deriver, '{$schemaRefExpr.valueSchema}, stack)
              lazyVals[F[A]](selfRef, 
                schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Map[a, b]]},
                selfRef -> '{$deriver.deriveMap[a, b]($schemaRefExpr, $instanceA, $instanceB, $summoned)}
              )              
            case '[zio.Chunk[a]] =>
              val (schemaRef, schemaRefExpr) = createSchemaRef[Chunk[a], Schema.Sequence[Chunk[a], a, _]](stack)              
              val summoned = summonOptional[F, Chunk[a]]

              val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.elementSchema}, stack)              
              lazyVals[F[A]](selfRef,
                schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Sequence[Chunk[a], a, _]]},
                selfRef -> '{$deriver.deriveSequence[Chunk, a]($schemaRefExpr, $instanceA, $summoned)}
              )
            case _ => 
              Mirror(typeRepr) match {
                case Some(mirror) =>
                  mirror.mirrorType match {
                    case MirrorType.Sum =>
                      deriveEnum[F, A](mirror, stack, deriver, schema, selfRef)
                    case MirrorType.Product =>
                      typeRepr.asType match {
                        case '[(a, b)] =>
                          val (schemaRef, schemaRefExpr) = createSchemaRef[(a, b), Schema.Tuple2[a, b]](stack)              
                          val summoned = summonOptional[F, (a, b)]

                          val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.left}, stack)
                          val instanceB = deriveInstance[F, b](deriver, '{$schemaRefExpr.right}, stack)
                          lazyVals[F[A]](selfRef,
                            schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Tuple2[a, b]]},
                            selfRef -> '{$deriver.deriveTupleN[(a, b)](Chunk($schemaRefExpr.left -> Deriver.wrap($instanceA), $schemaRefExpr.right -> Deriver.wrap($instanceB)), $summoned)}
                          )
                        case '[(a, b, c)] =>
                          val (schemaRef, schemaRefExpr) = createSchemaRef[(a, b, c), Schema.Transform[((a, b), c), (a, b, c), _]](stack)
                          val summoned = summonOptional[F, (a, b, c)]

                          val t123Schema = '{$schemaRefExpr.schema.asInstanceOf[Schema.Tuple2[Schema.Tuple2[a, b], c]]}
                          val t12Schema  = '{$t123Schema.left.asInstanceOf[Schema.Tuple2[a, b]]}
                          val t1Schema   = '{$t12Schema.left}
                          val t2Schema   = '{$t12Schema.right}
                          val t3Schema   = '{$t123Schema.right}

                          val t1Instance = deriveInstance[F, a](deriver, t1Schema, stack)
                          val t2Instance = deriveInstance[F, b](deriver, t2Schema, stack)
                          val t3Instance = deriveInstance[F, c](deriver, t3Schema, stack)
                          lazyVals[F[A]](selfRef,
                            schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Transform[((a, b), c), (a, b, c), _]]},
                            selfRef -> '{$deriver.deriveTupleN[(a, b, c)](Chunk($t1Schema -> Deriver.wrap($t1Instance), $t2Schema -> Deriver.wrap($t2Instance), $t3Schema -> Deriver.wrap($t3Instance)), $summoned)}
                          )
                        case '[(a, b, c, d)] =>
                          val (schemaRef, schemaRefExpr) = createSchemaRef[(a, b, c, d), Schema.Transform[(((a, b), c), d), (a, b, c, d), _]](stack)
                          val summoned = summonOptional[F, (a, b, c, d)]

                          val t1234Schema ='{$schemaRefExpr.schema.asInstanceOf[Schema.Tuple2[Schema.Tuple2[Schema.Tuple2[a, b], c], d]]}
                          val t123Schema = '{$t1234Schema.left.asInstanceOf[Schema.Tuple2[Schema.Tuple2[a, b], c]]}
                          val t12Schema  = '{$t123Schema.left.asInstanceOf[Schema.Tuple2[a, b]]}
                          val t1Schema   = '{$t12Schema.left}
                          val t2Schema   = '{$t12Schema.right}
                          val t3Schema   = '{$t123Schema.right}
                          val t4Schema   = '{$t1234Schema.right}

                          val t1Instance = deriveInstance[F, a](deriver, t1Schema, stack)
                          val t2Instance = deriveInstance[F, b](deriver, t2Schema, stack)
                          val t3Instance = deriveInstance[F, c](deriver, t3Schema, stack)
                          val t4Instance = deriveInstance[F, d](deriver, t4Schema, stack)
                          lazyVals[F[A]](selfRef,
                            schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Transform[(((a, b), c), d), (a, b, c, d), _]]},
                            selfRef -> '{$deriver.deriveTupleN[(a, b, c, d)](Chunk($t1Schema -> Deriver.wrap($t1Instance), $t2Schema -> Deriver.wrap($t2Instance), $t3Schema -> Deriver.wrap($t3Instance), $t4Schema -> Deriver.wrap($t4Instance)), $summoned)}
                          )
                        case _ =>
                          deriveCaseClass[F, A](mirror, stack, deriver, schema, selfRef)
                      }
                  }
                case None =>
                  val sym = typeRepr.typeSymbol
                  if (sym.isClassDef && sym.flags.is(Flags.Module)) {
                    deriveCaseObject[F, A](deriver, schema)
                  }
                  else {
                    report.errorAndAbort(s"Deriving schema for ${typeRepr.show} is not supported")
                  }
            }            
        }
      }
    }

    // println()
    // println()
    // println(s"FOR ${typeRepr.show}")
    // println(s"------")
    // println(s"RESULT ${result.show}")

    result
  }

  def deriveCaseObject[F[_]: Type, A: Type](deriver: Expr[Deriver[F]], schema: Expr[Schema[A]])(using Quotes) = {
    val summoned = summonOptional[F, A]
    '{$deriver.deriveRecord[A](Schema.force($schema).asInstanceOf[Schema.Record[A]], Chunk.empty, $summoned)}
   }

  def deriveCaseClass[F[_]: Type, A: Type](mirror: Mirror, stack: Stack, deriver: Expr[Deriver[F]], schema: Expr[Schema[A]], selfRef: Ref)(using Quotes) = {
    val newStack = stack.push(selfRef, TypeRepr.of[A])

    val labels = mirror.labels.toList
    val types = mirror.types.toList
    val typesAndLabels = types.zip(labels)

    if (labels.length <= 22) {     
      val (schemaRef, schemaRefExpr) = createSchemaRef[A, Schema.Record[A]](stack)
 
      val fieldInstances = typesAndLabels.zipWithIndex.map { case ((tpe, name), idx) =>        
        tpe.asType match {
          case '[t] =>
            val fieldSchema = '{$schemaRefExpr.fields(${Expr(idx)}).schema.asInstanceOf[Schema[t]]}
            val f = deriveInstance[F, t](deriver, fieldSchema, newStack)
            '{Deriver.wrap($f)}                
        }
      }

      val summoned = summonOptional[F, A]

      lazyVals[F[A]](selfRef,
        schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Record[A]]},
        selfRef -> '{$deriver.deriveRecord[A]($schemaRefExpr, Chunk(${Varargs(fieldInstances)}: _*), $summoned)}
      )      
    } else {       
      val (schemaRef, schemaRefExpr) = createSchemaRef[A, Schema.Transform[ListMap[String, _], A, _]](stack)
      val (recordSchemaRef, recordSchemaRefExpr) = createSchemaRef[ListMap[String, _], GenericRecord](stack)
 
      val fieldInstances = typesAndLabels.zipWithIndex.map { case ((tpe, name), idx) =>        
        tpe.asType match {
          case '[t] =>
            val fieldSchema = '{$recordSchemaRefExpr.fields(${Expr(idx)}).schema.asInstanceOf[Schema[t]]}
            val f = deriveInstance[F, t](deriver, fieldSchema, newStack)
            '{Deriver.wrap($f)}                
        }
      }

      val summoned = summonOptional[F, A]

      lazyVals[F[A]](selfRef,
        schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Transform[ListMap[String, _], A, _]]},
        recordSchemaRef -> '{$schemaRefExpr.schema.asInstanceOf[Schema.GenericRecord]},
        selfRef -> '{$deriver.deriveTransformedRecord[ListMap[String, _], A]($recordSchemaRefExpr, $schemaRefExpr, Chunk(${Varargs(fieldInstances)}: _*), $summoned)}
      )      
    }
  }

  def deriveEnum[F[_]: Type, A: Type](mirror: Mirror, stack: Stack, deriver: Expr[Deriver[F]], schema: Expr[Schema[A]], selfRef: Ref)(using Quotes) = {    
    val newStack = stack.push(selfRef, TypeRepr.of[A])

    val labels = mirror.labels.toList
    val types = mirror.types.toList
    val typesAndLabels = types.zip(labels)

    val (schemaRef, schemaRefExpr) = createSchemaRef[A, Schema.Enum[A]](stack)
    val caseInstances = typesAndLabels.zipWithIndex.map { case ((tpe, name), idx) =>        
      tpe.asType match {
        case '[t] =>
          val fieldSchema = '{$schemaRefExpr.cases(${Expr(idx)}).schema.asInstanceOf[Schema[t]]}
          val f = deriveInstance[F, t](deriver, fieldSchema, newStack)
          '{Deriver.wrap($f)}                
      }
    }

    val summoned = summonOptional[F, A]

    lazyVals[F[A]](selfRef, 
      schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Enum[A]]},
      selfRef -> '{$deriver.deriveEnum[A]($schemaRefExpr, Chunk(${Varargs(caseInstances)}: _*), $summoned)}      
    )
  }

  def lazyVals[A: Type](result: Ref, defs: (Ref, Expr[_])*)(using Quotes) =
    Block(
      defs.toList.map { case (ref, expr) => 
        ValDef(ref.symbol, Some(expr.asTerm.changeOwner(ref.symbol)))
      },
      result
    ).asExprOf[A]

  def createSchemaRef[T: Type, S <: Schema[T]: Type](stack: Stack)(using Quotes): (Ref, Expr[S]) = {
    val schemaSymbol = Symbol.newVal(Symbol.spliceOwner, s"schema${stack.size}", TypeRepr.of[S], Flags.Lazy, Symbol.spliceOwner)
    val schemaRef = Ref(schemaSymbol)
    val schemaRefExpr = schemaRef.asExprOf[S]
    
    (schemaRef, schemaRefExpr)
  }
}