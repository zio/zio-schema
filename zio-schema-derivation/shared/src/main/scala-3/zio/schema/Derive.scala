package zio.schema

import scala.quoted._
import scala.deriving.Mirror
import scala.collection.immutable.ListMap
import scala.compiletime.{erasedValue, summonInline, constValueTuple}
import zio.Chunk
import Schema._

object Derive {

  inline def derive[F[_], A](deriver: Deriver[F])(implicit schema: Schema[A]): F[A] = ${ deriveInstance[F, A]('deriver, 'schema) }

  private def deriveInstance[F[_]: Type, A: Type](deriver: Expr[Deriver[F]], schema: Expr[Schema[A]])(using ctx: Quotes): Expr[F[A]] =
    DeriveInstance().deriveInstance[F, A](deriver, schema, top = true)
}

private case class DeriveInstance()(using val ctx: Quotes) {
  val reflectionUtils = ReflectionUtils(ctx)
  import reflectionUtils.{MirrorType, Mirror, summonOptional}
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
            val summoned = summonOptionalIfNotTop[F, A](top)

            '{ $deriver.derivePrimitive[A]($st, $summoned) }
          case None =>
            // If it is an opaque type, we try check if it's underlying type is a StandardType
            val typeRef = typeRepr.typeSymbol.typeRef
            if (typeRef.isOpaqueAlias) {
              val underlyingType = typeRef.translucentSuperType
              underlyingType.asType match {
                case '[ut] => 
                  Expr.summon[StandardType[ut]] match {
                    case Some(st) =>
                      val summoned = summonOptionalIfNotTop[F, A](top)
                      Expr.summon[scala.reflect.ClassTag[A]] match {
                          case Some(classTag) =>
                            '{ $deriver.derivePrimitiveAlias[A, ut]($st, $summoned)($classTag) }
                          case None =>
                            report.errorAndAbort(s"Cannot find a ClassTag for ${typeRepr.show}")
                      }                      
                    case None =>
                      report.errorAndAbort(s"Opaque type ${typeRepr.show} is not supported because its underlying type is not a primitive type")
                  }
              }
            } else {
              typeRepr.asType match {
                case '[List[a]] =>
                  val (seqSchemaRef, seqSchemaRefExpr) = createSchemaRef[List[a], Schema.Sequence[List[a], a, _]](stack)              

                  val summoned = summonOptionalIfNotTop[F, List[a]](top)
                                    
                  val elemInstance = deriveInstance[F, a](deriver, '{$seqSchemaRefExpr.elementSchema}, stack)
                  lazyVals[F[A]](selfRef,
                    seqSchemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Sequence[List[a], a, _]]},
                    selfRef -> '{$deriver.deriveSequence[List, a]($seqSchemaRefExpr, $elemInstance, ${summoned})}
                  )            
                case '[scala.util.Either[a, b]] =>
                  val (schemaRef, schemaRefExpr) = createSchemaRef[scala.util.Either[a, b], Schema.Either[a, b]](stack)              
                  val summoned = summonOptionalIfNotTop[F, scala.util.Either[a, b]](top)

                  val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.left}, stack)
                  val instanceB = deriveInstance[F, b](deriver, '{$schemaRefExpr.right}, stack)
                  lazyVals[F[A]](selfRef,
                    schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Either[a, b]]},
                    selfRef -> '{$deriver.deriveEither[a, b]($schemaRefExpr, $instanceA, $instanceB, $summoned)}
                  )
                case '[Option[a]] =>
                  val (schemaRef, schemaRefExpr) = createSchemaRef[Option[a], Schema.Optional[a]](stack)              
                  val summoned = summonOptionalIfNotTop[F, Option[a]](top)

                  val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.schema}, stack)              
                  lazyVals[F[A]](selfRef,
                    schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Optional[a]]},
                    selfRef -> '{$deriver.deriveOption[a]($schemaRefExpr, $instanceA, $summoned)}
                  )
                case '[scala.collection.immutable.Set[a]] =>
                  val (schemaRef, schemaRefExpr) = createSchemaRef[scala.collection.immutable.Set[a], Schema.Set[a]](stack)              
                  val summoned = summonOptionalIfNotTop[F, scala.collection.immutable.Set[a]](top)

                  val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.elementSchema}, stack)              
                  lazyVals[F[A]](selfRef,
                    schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Set[a]]},
                    selfRef -> '{$deriver.deriveSet[a]($schemaRefExpr, $instanceA, $summoned)}
                  )
                case '[Vector[a]] =>
                  val (schemaRef, schemaRefExpr) = createSchemaRef[scala.collection.immutable.Vector[a], Schema.Sequence[Vector[a], a, _]](stack)              
                  val summoned = summonOptionalIfNotTop[F, scala.collection.immutable.Vector[a]](top)

                  val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.elementSchema}, stack)
                  lazyVals[F[A]](selfRef,
                    schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Sequence[Vector[a], a, _]]},
                    selfRef -> '{$deriver.deriveSequence[Vector, a]($schemaRefExpr, $instanceA, $summoned)}
                  )
                case '[scala.collection.immutable.Map[a, b]] =>
                  val (schemaRef, schemaRefExpr) = createSchemaRef[scala.collection.immutable.Map[a, b], Schema.Map[a, b]](stack)              
                  val summoned = summonOptionalIfNotTop[F, scala.collection.immutable.Map[a, b]](top)

                  val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.keySchema}, stack)
                  val instanceB = deriveInstance[F, b](deriver, '{$schemaRefExpr.valueSchema}, stack)
                  lazyVals[F[A]](selfRef, 
                    schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Map[a, b]]},
                    selfRef -> '{$deriver.deriveMap[a, b]($schemaRefExpr, $instanceA, $instanceB, $summoned)}
                  )              
                case '[zio.Chunk[a]] =>
                  val (schemaRef, schemaRefExpr) = createSchemaRef[Chunk[a], Schema.Sequence[Chunk[a], a, _]](stack)              
                  val summoned = summonOptionalIfNotTop[F, Chunk[a]](top)

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
                          deriveEnum[F, A](top, mirror, stack, deriver, schema, selfRef)
                        case MirrorType.Product =>
                          typeRepr.asType match {
                            case '[(a, b)] =>
                              val (schemaRef, schemaRefExpr) = createSchemaRef[(a, b), Schema.Tuple2[a, b]](stack)              
                              val summoned = summonOptionalIfNotTop[F, (a, b)](top)

                              val instanceA = deriveInstance[F, a](deriver, '{$schemaRefExpr.left}, stack)
                              val instanceB = deriveInstance[F, b](deriver, '{$schemaRefExpr.right}, stack)
                              lazyVals[F[A]](selfRef,
                                schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Tuple2[a, b]]},
                                selfRef -> '{$deriver.deriveTupleN[(a, b)](Chunk($schemaRefExpr.left -> Deriver.wrap($instanceA), $schemaRefExpr.right -> Deriver.wrap($instanceB)), $summoned)}
                              )
                            case '[(t1, t2, t3)] =>
                              tupleN[F, (t1, t2, t3)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4)] =>
                              tupleN[F, (t1, t2, t3, t4)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5)] =>
                              tupleN[F, (t1, t2, t3, t4, t5)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10], TypeRepr.of[t11])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10], TypeRepr.of[t11], TypeRepr.of[t12])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10], TypeRepr.of[t11], TypeRepr.of[t12], TypeRepr.of[t13])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10], TypeRepr.of[t11], TypeRepr.of[t12], TypeRepr.of[t13], TypeRepr.of[t14])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10], TypeRepr.of[t11], TypeRepr.of[t12], TypeRepr.of[t13], TypeRepr.of[t14], TypeRepr.of[t15])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10], TypeRepr.of[t11], TypeRepr.of[t12], TypeRepr.of[t13], TypeRepr.of[t14], TypeRepr.of[t15], TypeRepr.of[t16])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10], TypeRepr.of[t11], TypeRepr.of[t12], TypeRepr.of[t13], TypeRepr.of[t14], TypeRepr.of[t15], TypeRepr.of[t16], TypeRepr.of[t17])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10], TypeRepr.of[t11], TypeRepr.of[t12], TypeRepr.of[t13], TypeRepr.of[t14], TypeRepr.of[t15], TypeRepr.of[t16], TypeRepr.of[t17], TypeRepr.of[t18])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10], TypeRepr.of[t11], TypeRepr.of[t12], TypeRepr.of[t13], TypeRepr.of[t14], TypeRepr.of[t15], TypeRepr.of[t16], TypeRepr.of[t17], TypeRepr.of[t18], TypeRepr.of[t19])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10], TypeRepr.of[t11], TypeRepr.of[t12], TypeRepr.of[t13], TypeRepr.of[t14], TypeRepr.of[t15], TypeRepr.of[t16], TypeRepr.of[t17], TypeRepr.of[t18], TypeRepr.of[t19], TypeRepr.of[t20])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10], TypeRepr.of[t11], TypeRepr.of[t12], TypeRepr.of[t13], TypeRepr.of[t14], TypeRepr.of[t15], TypeRepr.of[t16], TypeRepr.of[t17], TypeRepr.of[t18], TypeRepr.of[t19], TypeRepr.of[t20], TypeRepr.of[t21])
                              ).asExprOf[F[A]]
                            case '[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22)] =>
                              tupleN[F, (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22)](
                                top,
                                selfRef,
                                deriver,
                                schema.asExprOf[Schema[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22)]],
                                stack,
                                Chunk(TypeRepr.of[t1], TypeRepr.of[t2], TypeRepr.of[t3], TypeRepr.of[t4], TypeRepr.of[t5], TypeRepr.of[t6], TypeRepr.of[t7], TypeRepr.of[t8], TypeRepr.of[t9], TypeRepr.of[t10], TypeRepr.of[t11], TypeRepr.of[t12], TypeRepr.of[t13], TypeRepr.of[t14], TypeRepr.of[t15], TypeRepr.of[t16], TypeRepr.of[t17], TypeRepr.of[t18], TypeRepr.of[t19], TypeRepr.of[t20], TypeRepr.of[t21], TypeRepr.of[t22])
                              ).asExprOf[F[A]]
                            case _ =>
                              deriveCaseClass[F, A](top, mirror, stack, deriver, schema, selfRef)
                          }
                      }
                    case None =>
                      val sym = typeRepr.typeSymbol
                      if (sym.isClassDef && sym.flags.is(Flags.Module)) {
                        deriveCaseObject[F, A](top, deriver, schema)
                      }
                      else {
                        val summoned = summonOptionalIfNotTop[F, A](top)
                        Expr.summon[scala.reflect.ClassTag[A]] match {
                            case Some(classTag) =>
                              '{ $deriver.deriveUnknown[A]($summoned)($classTag) }
                            case None =>
                              report.errorAndAbort(s"Cannot find a ClassTag for ${typeRepr.show}")
                        }
                      }
                }            
              }
          }
      }
    }

    result
  }

  def deriveCaseObject[F[_]: Type, A: Type](top: Boolean, deriver: Expr[Deriver[F]], schema: Expr[Schema[A]])(using Quotes): Expr[F[A]] = {
    val summoned = summonOptionalIfNotTop[F, A](top)
    Expr.summon[scala.reflect.ClassTag[A]] match {
      case Some(classTag) =>
        '{ $deriver.tryDeriveRecord[A](Schema.force($schema), Chunk.empty, $summoned)($classTag) }
      case None =>
        val typeRepr = TypeRepr.of[A]
        report.errorAndAbort(s"Cannot find a ClassTag for ${typeRepr.show}")
    }
   }

  def deriveCaseClass[F[_]: Type, A: Type](top: Boolean, mirror: Mirror, stack: Stack, deriver: Expr[Deriver[F]], schema: Expr[Schema[A]], selfRef: Ref)(using Quotes): Expr[F[A]] = {
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

      val summoned = summonOptionalIfNotTop[F, A](top)
      Expr.summon[scala.reflect.ClassTag[A]] match {
        case Some(classTag) =>
          lazyVals[F[A]](selfRef,
            schemaRef -> '{
              Schema.force($schema).asInstanceOf[Schema.Record[A]]
            },
            selfRef -> '{
              $deriver.tryDeriveRecord[A](Schema.force($schema), Chunk(${
                Varargs(fieldInstances)
              }: _*), $summoned)($classTag)
            }
          )
        case None =>
          val typeRepr = TypeRepr.of[A]
          report.errorAndAbort(s"Cannot find a ClassTag for ${typeRepr.show}")
      }
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

      val summoned = summonOptionalIfNotTop[F, A](top)

      lazyVals[F[A]](selfRef,
        schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Transform[ListMap[String, _], A, _]]},
        recordSchemaRef -> '{$schemaRefExpr.schema.asInstanceOf[Schema.GenericRecord]},
        selfRef -> '{$deriver.deriveTransformedRecord[ListMap[String, _], A]($recordSchemaRefExpr, $schemaRefExpr, Chunk(${Varargs(fieldInstances)}: _*), $summoned)}
      )      
    }
  }

  def deriveEnum[F[_]: Type, A: Type](top: Boolean, mirror: Mirror, stack: Stack, deriver: Expr[Deriver[F]], schema: Expr[Schema[A]], selfRef: Ref)(using Quotes): Expr[F[A]] = {
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

    val summoned = summonOptionalIfNotTop[F, A](top)

    Expr.summon[scala.reflect.ClassTag[A]] match {
      case Some(classTag) =>
        lazyVals[F[A]](selfRef,
          schemaRef -> '{
            Schema.force($schema).asInstanceOf[Schema.Enum[A]]
          },
          selfRef -> '{
            $deriver.tryDeriveEnum[A](Schema.force($schema), Chunk(${
              Varargs(caseInstances)
            }: _*), $summoned)($classTag)
          }
        )
      case None =>
        val typeRepr = TypeRepr.of[A]
        report.errorAndAbort(s"Cannot find a ClassTag for ${typeRepr.show}")
    }
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

  def toTupleSchemaType(tpes: Chunk[TypeRepr]) =
    tpes.tail.foldLeft(tpes.head) { case (xs, x) => TypeRepr.of[Schema.Tuple2].appliedTo(List(xs, x)) }

  def lefts[T <: Schema.Tuple2[_, _]: Type](tschema: Expr[T], tpes: Chunk[TypeRepr], depth: Int): Expr[Schema.Tuple2[_, _]] =
    if (tpes.size < 2 || depth == 0) tschema
    else toTupleSchemaType(tpes).asType match {
      case '[Schema.Tuple2[a, b]] =>
         lefts('{ $tschema.left.asInstanceOf[Schema.Tuple2[a, b]] }, tpes.init, depth - 1)
    }

  def tupleN[F[_]: Type, A: Type](top: Boolean, selfRef: Ref, deriver: Expr[Deriver[F]], schema: Expr[Schema[A]], stack: Stack, tpes: Chunk[TypeRepr])(using Quotes): Expr[F[A]] = {
    val tschemaType = toTupleSchemaType(tpes)
    val tupleType = tpes.length match {
      case 1 => TypeRepr.of[scala.Tuple1]
      case 2 => TypeRepr.of[scala.Tuple2]
      case 3 => TypeRepr.of[scala.Tuple3]
      case 4 => TypeRepr.of[scala.Tuple4]
      case 5 => TypeRepr.of[scala.Tuple5]
      case 6 => TypeRepr.of[scala.Tuple6]
      case 7 => TypeRepr.of[scala.Tuple7]
      case 8 => TypeRepr.of[scala.Tuple8]
      case 9 => TypeRepr.of[scala.Tuple9]
      case 10 => TypeRepr.of[scala.Tuple10]
      case 11 => TypeRepr.of[scala.Tuple11]
      case 12 => TypeRepr.of[scala.Tuple12]
      case 13 => TypeRepr.of[scala.Tuple13]
      case 14 => TypeRepr.of[scala.Tuple14]
      case 15 => TypeRepr.of[scala.Tuple15]
      case 16 => TypeRepr.of[scala.Tuple16]
      case 17 => TypeRepr.of[scala.Tuple17]
      case 18 => TypeRepr.of[scala.Tuple18]
      case 19 => TypeRepr.of[scala.Tuple19]
      case 20 => TypeRepr.of[scala.Tuple20]
      case 21 => TypeRepr.of[scala.Tuple21]
      case 22 => TypeRepr.of[scala.Tuple22]
    }
    val flatTupleType = tupleType.appliedTo(tpes.toList)
    val nestedTupleType = tpes.tail.foldLeft(tpes.head) { case (xs, x) => TypeRepr.of[scala.Tuple2].appliedTo(List(xs, x)) }

    tschemaType.asType match {
      case '[Schema.Tuple2[a, b]] =>
        flatTupleType.asType match {
          case '[ftt] =>
            nestedTupleType.asType match {
              case '[ntt] =>
                val (schemaRef, schemaRefExpr) = createSchemaRef[ftt, Schema.Transform[ntt, ftt, _]](stack)
                val summoned = summonOptionalIfNotTop[F, ftt](top)

                val tschema = '{$schemaRefExpr.schema.asInstanceOf[Schema.Tuple2[a, b]]}

                val n = tpes.length
                val schemas = (1 to n).map { idx =>
                  if (idx == n) {
                    '{ $tschema.right }
                  } else if (idx == 1) {
                    '{${lefts(tschema, tpes.init, n-1)}.left }
                  } else {
                    '{${lefts(tschema, tpes.init, n-idx)}.right }
                  }
                }

                val pairs = tpes.zip(schemas).map { case (tp, s) =>
                  tp.asType match {
                    case '[t] =>
                      val i = deriveInstance[F, t](deriver, s.asExprOf[Schema[t]], stack)
                      '{ ($s, Deriver.wrap[F, t](${i})) }
                  }
                }

                lazyVals[F[A]](
                  selfRef,
                  schemaRef -> '{Schema.force($schema).asInstanceOf[Schema.Transform[ntt, ftt, _]]},
                  selfRef -> '{$deriver.deriveTupleN[ftt](Chunk(${Varargs(pairs)} : _*), $summoned)}
                )
            }
          }
    }
  }

  def summonOptionalIfNotTop[F[_]: Type, A: Type](top: Boolean)(using Quotes): Expr[Option[F[A]]] =
    if (top) '{None} else summonOptional[F, A]
}