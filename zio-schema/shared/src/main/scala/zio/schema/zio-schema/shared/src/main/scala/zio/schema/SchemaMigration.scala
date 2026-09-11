package zio.schema.migration

import scala.quoted._
import zio.schema._
import zio.Chunk

object GhostMigrationDeriver {
  inline def derive[A, B]: Migration = ${ deriveImpl[A, B] }

  def deriveImpl[A: Type, B: Type](using Quotes): Expr[Migration] = {
    import quotes.reflect._

    def buildMigration(fromTpe: TypeRepr, toTpe: TypeRepr, stack: Set[TypeRepr]): Expr[Migration] = {
      val fromDealiased = fromTpe.dealias
      val toDealiased = toTpe.dealias

      if (fromDealiased =:= toDealiased || stack.exists(_ =:= fromDealiased)) {
        '{ Migration.Identity }
      } else {
        val nextStack = stack + fromDealiased

        if (fromDealiased.typeSymbol.flags.is(Flags.Sealed)) {
          val aChildren = fromDealiased.typeSymbol.children
          val bChildren = toDealiased.typeSymbol.children

          val removals = aChildren.filterNot(a => bChildren.exists(_.name == a.name)).map { aSym =>
            '{ Migration.RemoveCase(${Expr(aSym.name)}) }
          }

          val additions = bChildren.filterNot(b => aChildren.exists(_.name == b.name)).map { bSym =>
            '{ Migration.AddCase(${Expr(bSym.name)}) }
          }

          val transformations = aChildren.flatMap { aChild =>
            bChildren.find(_.name == aChild.name).flatMap { bChild =>
              val aChildTpe = fromDealiased.memberType(aChild)
              val bChildTpe = toDealiased.memberType(bChild)
              
              if (aChildTpe =:= bChildTpe) None
              else {
                val childMigration = buildMigration(aChildTpe, bChildTpe, nextStack)
                Some('{ Migration.Node(${Expr(aChild.name)}, Schema.dynamicValue, Schema.dynamicValue, $childMigration) })
              }
            }
          }

          val allSumSteps = removals ++ additions ++ transformations
          if (allSumSteps.isEmpty) '{ Migration.Identity }
          else '{ Migration.Incremental(Chunk.fromIterable(${Expr.ofList(allSumSteps)})) }
        } 
        else {
          val aFields = fromDealiased.typeSymbol.caseFields
          val bFields = toDealiased.typeSymbol.caseFields

          val removals = aFields.filterNot(a => bFields.exists(_.name == a.name)).map { aSym =>
            val aFTpe = fromDealiased.memberType(aSym)
            aFTpe.asType match {
              case '[at] =>
                val sA = Expr.summon[Schema[at]].getOrElse('{ Schema.dynamicValue.asInstanceOf[Schema[at]] })
                '{ Migration.RemoveField(${Expr(aSym.name)}, $sA, DynamicValue.None) }
            }
          }

          val transformsAndNodes = bFields.flatMap { bSym =>
            val bFTpe = toDealiased.memberType(bSym).dealias
            val aSymOpt = aFields.find(_.name == bSym.name)

            aSymOpt match {
              case Some(aSym) =>
                val aFTpe = fromDealiased.memberType(aSym).dealias
                if (aFTpe =:= bFTpe) None
                else {
                  (aFTpe.asType, bFTpe.asType) match {
                    case ('[at], '[bt]) =>
                      val sA = Expr.summon[Schema[at]].getOrElse('{ Schema.dynamicValue.asInstanceOf[Schema[at]] })
                      val sB = Expr.summon[Schema[bt]].getOrElse('{ Schema.dynamicValue.asInstanceOf[Schema[bt]] })

                      if ((aFTpe <:< TypeRepr.of[Iterable[?]] && bFTpe <:< TypeRepr.of[Iterable[?]]) || 
                          (aFTpe.typeSymbol.isClassDef && !aFTpe.derivesFrom(Symbol.requiredClass("scala.AnyVal")))) {
                        
                        val nestedMigration = if (aFTpe <:< TypeRepr.of[Iterable[?]]) {
                          val aArg = aFTpe.typeArgs.headOption.getOrElse(TypeRepr.of[Any])
                          val bArg = bFTpe.typeArgs.headOption.getOrElse(TypeRepr.of[Any])
                          buildMigration(aArg, bArg, nextStack)
                        } else {
                          buildMigration(aFTpe, bFTpe, nextStack)
                        }
                        Some('{ Migration.Node(${Expr(bSym.name)}, $sA, $sB, $nestedMigration) })
                      } else {
                        val transformFunc: Expr[Any => Any] = '{ (v: Any) =>
                          v match {
                            case dv: DynamicValue => dv.toTypedValue($sA).getOrElse(v)
                            case _ => v
                          }
                        }
                        Some('{ Migration.Transform(${Expr(bSym.name)}, $sA, $sB, $transformFunc) })
                      }
                  }
                }
              case None => None
            }
          }

          val additions = bFields.filterNot(b => aFields.exists(_.name == b.name)).map { bSym =>
            val bFTpe = toDealiased.memberType(bSym).dealias
            bFTpe.asType match {
              case '[bt] =>
                val sB = Expr.summon[Schema[bt]].getOrElse('{ Schema.dynamicValue.asInstanceOf[Schema[bt]] })
                val zeroValue = 
                  if (bFTpe <:< TypeRepr.of[Option[?]]) '{ DynamicValue.Optional(None) }
                  else if (bFTpe <:< TypeRepr.of[String]) '{ DynamicValue.fromSchemaAndValue($sB, "".asInstanceOf[bt]) }
                  else if (bFTpe <:< TypeRepr.of[Int] || bFTpe <:< TypeRepr.of[Long]) '{ DynamicValue.fromSchemaAndValue($sB, 0.asInstanceOf[bt]) }
                  else if (bFTpe <:< TypeRepr.of[Iterable[?]]) '{ DynamicValue.Sequence(Chunk.empty) }
                  else '{ DynamicValue.None }

                '{ Migration.AddField(${Expr(bSym.name)}, $sB, $zeroValue) }
            }
          }

          val allSteps = removals ++ transformsAndNodes ++ additions
          if (allSteps.isEmpty) '{ Migration.Identity }
          else '{ Migration.Incremental(Chunk.fromIterable(${Expr.ofList(allSteps)})) }
        }
      }
    }

    buildMigration(TypeRepr.of[A], TypeRepr.of[B], Set.empty)
  }
}
