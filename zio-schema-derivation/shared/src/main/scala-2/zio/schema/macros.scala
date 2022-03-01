package zio.schema

import scala.annotation.nowarn
import scala.reflect.macros.whitebox

import zio.Chunk

object DeriveSchema {
  import scala.language.experimental.macros

  implicit def gen[T]: Schema[T] = macro genImpl[T]

  def genImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val JavaAnnotationTpe = typeOf[java.lang.annotation.Annotation]

    val tpe = weakTypeOf[T]

    def concreteType(seenFrom: Type, tpe: Type): Type =
      tpe.asSeenFrom(seenFrom, seenFrom.typeSymbol.asClass)

    def isCaseObject(tpe: Type): Boolean = tpe.typeSymbol.asClass.isModuleClass

    def isCaseClass(tpe: Type): Boolean = tpe.typeSymbol.asClass.isCaseClass

    def isSealedTrait(tpe: Type): Boolean = tpe.typeSymbol.asClass.isTrait && tpe.typeSymbol.asClass.isSealed

    def recurse(tpe: Type, stack: List[Frame[c.type]]): Tree =
      if (isCaseObject(tpe))
        q"zio.schema.Schema.singleton(${tpe.typeSymbol.asClass.module})"
      else if (isCaseClass(tpe)) deriveRecord(tpe, stack)
      else if (isSealedTrait(tpe))
        deriveEnum(tpe, stack)
      else
        c.abort(
          c.enclosingPosition,
          s"Failed to derive schema for $tpe. Can only derive Schema for case class or sealed trait"
        )

    def directInferSchema(parentType: Type, schemaType: Type, stack: List[Frame[c.type]]): Tree =
      stack
        .find(_.tpe =:= schemaType)
        .map {
          case Frame(_, ref, _) =>
            val refIdent = Ident(TermName(ref))
            q"zio.schema.Schema.defer($refIdent)"
        }
        .getOrElse {
          if (schemaType =:= parentType)
            c.abort(c.enclosingPosition, "Direct recursion is not supported")
          else {
            c.inferImplicitValue(
              c.typecheck(tq"zio.schema.Schema[$schemaType]", c.TYPEmode).tpe,
              withMacrosDisabled = false
            ) match {
              case EmptyTree =>
                schemaType.typeArgs match {
                  case Nil =>
                    recurse(schemaType, stack)
                  case typeArg1 :: Nil =>
                    if (schemaType <:< c.typeOf[Option[_]])
                      q"zio.schema.Schema.option(zio.schema.Schema.defer(${directInferSchema(parentType, concreteType(parentType, typeArg1), stack)}))"
                    else if (schemaType <:< typeOf[List[_]])
                      q"zio.schema.Schema.list(zio.schema.Schema.defer(${directInferSchema(parentType, concreteType(parentType, typeArg1), stack)}))"
                    else if (schemaType <:< typeOf[Set[_]])
                      q"zio.schema.Schema.set(zio.schema.Schema.defer(${directInferSchema(parentType, concreteType(parentType, typeArg1), stack)}))"
                    else if (schemaType <:< typeOf[Vector[_]])
                      q"zio.schema.Schema.vector(zio.schema.Schema.defer(${directInferSchema(parentType, concreteType(parentType, typeArg1), stack)}))"
                    else if (schemaType <:< typeOf[Chunk[_]])
                      q"zio.schema.Schema.chunk(zio.schema.Schema.defer(${directInferSchema(parentType, concreteType(parentType, typeArg1), stack)}))"
                    else
                      recurse(schemaType, stack)
                  case typeArg1 :: typeArg2 :: Nil =>
                    if (schemaType <:< typeOf[Either[_, _]])
                      q"""zio.schema.Schema.either(
                        zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg1),
                        stack
                      )}),
                        zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg2),
                        stack
                      )})
                      )
                   """
                    else if (schemaType <:< typeOf[(_, _)])
                      q"""zio.schema.Schema.tuple2(
                        zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg1),
                        stack
                      )}),
                        zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg2),
                        stack
                      )})
                      )
                   """
                    else
                      recurse(schemaType, stack)
                  case typeArg1 :: typeArg2 :: typeArg3 :: Nil =>
                    if (schemaType <:< typeOf[(_, _, _)])
                      q"""zio.schema.Schema.tuple3(
                        zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg1),
                        stack
                      )}),
                        zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg2),
                        stack
                      )}),
                        zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg3),
                        stack
                      )})

                      )
                   """
                    else
                      recurse(schemaType, stack)
                  case typeArg1 :: typeArg2 :: typeArg3 :: typeArg4 :: Nil =>
                    if (schemaType <:< typeOf[(_, _, _)])
                      q"""zio.schema.Schema.tuple4(
                        zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg1),
                        stack
                      )}),
                        zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg2),
                        stack
                      )}),
                        zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg3),
                        stack
                      )}),
                        zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg4),
                        stack
                      )})
                      )
                   """
                    else
                      recurse(schemaType, stack)
                  case args => c.abort(c.enclosingPosition, s"Unhandled type args $args")
                }
              case tree =>
                q"zio.schema.Schema.defer($tree)"
            }
          }
        }

    def deriveRecord(tpe: Type, stack: List[Frame[c.type]]): Tree = stack.find(_.tpe =:= tpe) match {
      case Some(Frame(_, ref, _)) =>
        val refIdent = Ident(TermName(ref))
        q"$refIdent"
      case None =>
        val tpeCompanion = tpe.typeSymbol.companion

        val selfRefName = c.freshName("var")
        val selfRef     = Ident(TermName(selfRefName))

        val currentFrame = Frame[c.type](c, selfRefName, tpe)

        val fieldTypes: Iterable[TermSymbol] = tpe.decls.collect {
          case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p
        }
        val arity = fieldTypes.size

        val typeArgs = fieldTypes.map { ft =>
          val fieldType = concreteType(tpe, tpe.decl(ft.name).typeSignature)
          q"$fieldType"
        } ++ Iterable(q"$tpe")

        val fieldAccessors = tpe.decls.collect {
          case p: TermSymbol if p.isCaseAccessor && p.isMethod => p.name
        }

        @nowarn
        val typeAnnotations: List[Tree] =
          tpe.typeSymbol.annotations.collect {
            case annotation if !(annotation.tree.tpe <:< JavaAnnotationTpe) =>
              annotation.tree match {
                case q"new $annConstructor(..$annotationArgs)" =>
                  q"new ${annConstructor.tpe.typeSymbol}(..$annotationArgs)"
                case q"new $annConstructor()" =>
                  q"new ${annConstructor.tpe.typeSymbol}()"
                case tree =>
                  c.warning(c.enclosingPosition, s"Unhandled annotation tree $tree")
                  EmptyTree
              }
            case annotation =>
              c.warning(c.enclosingPosition, s"Unhandled annotation ${annotation.tree}")
              EmptyTree
          }.filter(_ != EmptyTree)

        @nowarn
        val fieldAnnotations: List[List[Tree]] = //List.fill(arity)(Nil)
          tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.headOption.map { symbols =>
            symbols
              .map(_.annotations.collect {
                case annotation if !(annotation.tree.tpe <:< JavaAnnotationTpe) =>
                  annotation.tree match {
                    case q"new $annConstructor(..$annotationArgs)" =>
                      q"new ${annConstructor.tpe.typeSymbol}(..$annotationArgs)"
                    case q"new $annConstructor()" =>
                      q"new ${annConstructor.tpe.typeSymbol}()"
                    case tree =>
                      c.warning(c.enclosingPosition, s"Unhandled annotation tree $tree")
                      EmptyTree
                  }
                case annotation =>
                  c.warning(c.enclosingPosition, s"Unhandled annotation ${annotation.tree}")
                  EmptyTree
              })
              .filter(_ != EmptyTree)
          }.getOrElse(Nil)

        if (arity > 22) {
          val fields = fieldTypes.zip(fieldAnnotations).map {
            case (termSymbol, annotations) =>
              val fieldSchema = directInferSchema(
                tpe,
                concreteType(tpe, termSymbol.typeSignature),
                currentFrame +: stack
              )
              val fieldLabel = termSymbol.name.toString.trim
              q"zio.schema.Schema.Field.apply($fieldLabel,$fieldSchema,zio.Chunk.apply[Any](..$annotations))"
          }
          val fromMap = {
            val casts = fieldTypes.map { termSymbol =>
              q"""
                 try m.apply(${termSymbol.name.toString.trim}).asInstanceOf[${termSymbol.typeSignature}]
                 catch {
                   case _: ClassCastException => throw new RuntimeException("Field " + ${termSymbol.name.toString.trim} + " has invalid type")
                   case _: Throwable  => throw new RuntimeException("Field " + ${termSymbol.name.toString.trim} + " is missing")
                 }
               """
            }
            q"""(m: scala.collection.immutable.ListMap[String, _]) => try { Right($tpeCompanion.apply(..$casts)) } catch { case e: Throwable => Left(e.getMessage) }"""
          }
          val toMap = {
            val tuples = fieldAccessors.map { fieldName =>
              q"(${fieldName.toString},b.$fieldName)"
            }
            q"""(b: $tpe) => Right(scala.collection.immutable.ListMap.apply(..$tuples))"""
          }

          q"""zio.schema.Schema.record(..$fields).transformOrFail[$tpe]($fromMap,$toMap)"""
        } else {
          val schemaType = arity match {
            case 1  => q"zio.schema.Schema.CaseClass1[..$typeArgs]"
            case 2  => q"zio.schema.Schema.CaseClass2[..$typeArgs]"
            case 3  => q"zio.schema.Schema.CaseClass3[..$typeArgs]"
            case 4  => q"zio.schema.Schema.CaseClass4[..$typeArgs]"
            case 5  => q"zio.schema.Schema.CaseClass5[..$typeArgs]"
            case 6  => q"zio.schema.Schema.CaseClass6[..$typeArgs]"
            case 7  => q"zio.schema.Schema.CaseClass7[..$typeArgs]"
            case 8  => q"zio.schema.Schema.CaseClass8[..$typeArgs]"
            case 9  => q"zio.schema.Schema.CaseClass9[..$typeArgs]"
            case 10 => q"zio.schema.Schema.CaseClass10[..$typeArgs]"
            case 11 => q"zio.schema.Schema.CaseClass11[..$typeArgs]"
            case 12 => q"zio.schema.Schema.CaseClass12[..$typeArgs]"
            case 13 => q"zio.schema.Schema.CaseClass13[..$typeArgs]"
            case 14 => q"zio.schema.Schema.CaseClass14[..$typeArgs]"
            case 15 => q"zio.schema.Schema.CaseClass15[..$typeArgs]"
            case 16 => q"zio.schema.Schema.CaseClass16[..$typeArgs]"
            case 17 => q"zio.schema.Schema.CaseClass17[..$typeArgs]"
            case 18 => q"zio.schema.Schema.CaseClass18[..$typeArgs]"
            case 19 => q"zio.schema.Schema.CaseClass19[..$typeArgs]"
            case 20 => q"zio.schema.Schema.CaseClass20[..$typeArgs]"
            case 21 => q"zio.schema.Schema.CaseClass21[..$typeArgs]"
            case 22 => q"zio.schema.Schema.CaseClass22[..$typeArgs]"
          }

          val fieldDefs = fieldTypes.zip(fieldAnnotations).zipWithIndex.map {
            case ((termSymbol, annotations), idx) =>
              val fieldSchema = directInferSchema(
                tpe,
                concreteType(tpe, termSymbol.typeSignature),
                currentFrame +: stack
              )
              val fieldArg   = if (fieldTypes.size > 1) TermName(s"field${idx + 1}") else TermName("field")
              val fieldLabel = termSymbol.name.toString.trim

              if (annotations.nonEmpty)
                q"""$fieldArg = zio.schema.Schema.Field.apply(label = $fieldLabel, schema = $fieldSchema, annotations = zio.Chunk.apply[Any](..$annotations))"""
              else
                q"""$fieldArg = zio.schema.Schema.Field.apply(label = $fieldLabel, schema = $fieldSchema)"""
          }

          val constructArgs = fieldTypes.zipWithIndex.map {
            case (term, idx) =>
              val arg = TermName(s"_$idx")
              q"$arg: ${term.typeSignature.asSeenFrom(tpe, tpe.typeSymbol.asClass)}"
          }
          val constructApplyArgs = fieldTypes.zipWithIndex.map {
            case (_, idx) =>
              val arg = TermName(s"_$idx")
              q"$arg"
          }

          val constructExpr = q"construct = (..$constructArgs) => $tpeCompanion(..$constructApplyArgs)"

          val extractors = fieldAccessors.map(_.toString).zipWithIndex.foldLeft(Seq.empty[c.universe.Tree]) {
            case (acc, (fieldLabel, idx)) =>
              val argName = if (fieldTypes.size > 1) TermName(s"extractField${idx + 1}") else TermName("extractField")
              acc :+ q"$argName = (t: $tpe) => t.${TermName(fieldLabel)}"
          }

          val applyArgs =
            if (typeAnnotations.isEmpty)
              Iterable(q"annotations = zio.Chunk.empty") ++ fieldDefs ++ Iterable(constructExpr) ++ extractors
            else
              Iterable(q"annotations = zio.Chunk.apply(..$typeAnnotations)") ++ fieldDefs ++ Iterable(constructExpr) ++ extractors

          fieldTypes.size match {
            case 1 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass1[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 2 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass2[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 3 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass3[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 4 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass4[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 5 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass5[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 6 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass6[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 7 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass7[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 8 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass8[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 9 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass9[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 10 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass10[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 11 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass11[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 12 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass12[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 13 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass13[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 14 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass14[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 15 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass15[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 16 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass16[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 17 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass17[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 18 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass18[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 19 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass19[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 20 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass20[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 21 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass21[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case 22 =>
              q"{lazy val $selfRef: zio.schema.Schema.CaseClass22[..$typeArgs] = $schemaType(..$applyArgs); $selfRef}"
            case s =>
              c.abort(
                tpe.termSymbol.pos,
                s"Unhandled product arity $s for ${show(tpe)}, ${show(tpeCompanion)}. This should never happen. If you see this error message please report a bug to https://github.com/zio/zio-schema/issues"
              )
          }
        }
    }

    def deriveEnum(tpe: Type, stack: List[Frame[c.type]]): Tree = {

      val appliedTypeArgs: Map[String, Type] =
        tpe.typeConstructor.typeParams.map(_.name.toString).zip(tpe.typeArgs).toMap

      def appliedSubtype(subtype: Type): Type =
        if (subtype.typeArgs.size == 0) subtype
        else {
          val appliedTypes = subtype.typeConstructor.typeParams.map(_.name.toString).map { typeParam =>
            appliedTypeArgs.get(typeParam) match {
              case None =>
                c.abort(
                  c.enclosingPosition,
                  s"Unable to find applied type param  $typeParam for subtype $subtype of $tpe"
                )
              case Some(applyType) => applyType
            }
          }
          appliedType(subtype, appliedTypes: _*)
        }

      def knownSubclassesOf(parent: ClassSymbol): Set[Type] = {
        val (abstractChildren, concreteChildren) = parent.knownDirectSubclasses.partition(_.isAbstract)
        for (child <- concreteChildren) {
          child.typeSignature
          if (!child.isFinal && !child.asClass.isCaseClass)
            c.abort(c.enclosingPosition, s"child $child of $parent is neither final nor a case class")
        }

        concreteChildren.union(abstractChildren).flatMap { child =>
          child.typeSignature
          val childClass = child.asClass
          if (childClass.isSealed && childClass.isTrait) knownSubclassesOf(childClass)
          else if (childClass.isCaseClass) {
            val st = concreteType(concreteType(tpe, parent.asType.toType), child.asType.toType)
            Set(appliedSubtype(st))
          } else c.abort(c.enclosingPosition, s"child $child of $parent is not a sealed trait or case class")
        }
      }

      @nowarn
      val typeAnnotations: List[Tree] =
        tpe.typeSymbol.annotations.collect {
          case annotation if !(annotation.tree.tpe <:< JavaAnnotationTpe) =>
            annotation.tree match {
              case q"new $annConstructor(..$annotationArgs)" =>
                q"new ${annConstructor.tpe.typeSymbol}(..$annotationArgs)"
              case q"new $annConstructor()" =>
                q"new ${annConstructor.tpe.typeSymbol}()"
              case tree =>
                c.warning(c.enclosingPosition, s"Unhandled annotation tree $tree")
                EmptyTree
            }
          case annotation =>
            c.warning(c.enclosingPosition, s"Unhandled annotation ${annotation.tree}")
            EmptyTree
        }.filter(_ != EmptyTree)

      val selfRefName  = c.freshName("ref")
      val selfRefIdent = Ident(TermName(selfRefName))

      val currentFrame = Frame[c.type](c, selfRefName, tpe)

      val subtypes = knownSubclassesOf(tpe.typeSymbol.asClass).toList
        .sortBy(_.typeSymbol.asClass.name.toString.trim)
        .map(concreteType(tpe, _))

      val typeArgs = subtypes ++ Iterable(tpe)

      val cases = subtypes.map { subtype: Type =>
        @nowarn
        val typeAnnotations: List[Tree] =
          subtype.typeSymbol.annotations.collect {
            case annotation if !(annotation.tree.tpe <:< JavaAnnotationTpe) =>
              annotation.tree match {
                case q"new $annConstructor(..$annotationArgs)" =>
                  q"new ${annConstructor.tpe.typeSymbol}(..$annotationArgs)"
                case q"new $annConstructor()" =>
                  q"new ${annConstructor.tpe.typeSymbol}()"
                case tree =>
                  c.warning(c.enclosingPosition, s"Unhandled annotation tree $tree")
                  EmptyTree
              }
            case annotation =>
              c.warning(c.enclosingPosition, s"Unhandled annotation ${annotation.tree}")
              EmptyTree
          }.filter(_ != EmptyTree)

        val caseLabel     = subtype.typeSymbol.name.toString.trim
        val caseSchema    = directInferSchema(tpe, concreteType(tpe, subtype), currentFrame +: stack)
        val deconstructFn = q"(z: $tpe) => z.asInstanceOf[$subtype]"
        if (typeAnnotations.isEmpty)
          q"zio.schema.Schema.Case.apply[$subtype,$tpe]($caseLabel,$caseSchema,$deconstructFn)"
        else {
          val annotationArgs = q"zio.Chunk.apply(..$typeAnnotations)"
          q"zio.schema.Schema.Case.apply[$subtype,$tpe]($caseLabel,$caseSchema,$deconstructFn,$annotationArgs)"
        }
      }

      val applyArgs =
        if (typeAnnotations.isEmpty)
          cases ++ Iterable(q"annotations = zio.Chunk.empty")
        else
          cases ++ Iterable(q"annotations = zio.Chunk.apply(..$typeAnnotations)")

      cases.size match {
        case 0 => c.abort(c.enclosingPosition, s"No subtypes found for ${show(tpe)}")
        case 1 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum1[..$typeArgs] = zio.schema.Schema.Enum1[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 2 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum2[..$typeArgs] = zio.schema.Schema.Enum2[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 3 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum3[..$typeArgs] = zio.schema.Schema.Enum3[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 4 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum4[..$typeArgs] = zio.schema.Schema.Enum4[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 5 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum5[..$typeArgs] = zio.schema.Schema.Enum5[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 6 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum6[..$typeArgs] = zio.schema.Schema.Enum6[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 7 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum7[..$typeArgs] = zio.schema.Schema.Enum7[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 8 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum8[..$typeArgs] = zio.schema.Schema.Enum8[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 9 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum9[..$typeArgs] = zio.schema.Schema.Enum9[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 10 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum10[..$typeArgs] = zio.schema.Schema.Enum10[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 11 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum11[..$typeArgs] = zio.schema.Schema.Enum11[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 12 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum12[..$typeArgs] = zio.schema.Schema.Enum12[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 13 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum13[..$typeArgs] = zio.schema.Schema.Enum13[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 14 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum14[..$typeArgs] = zio.schema.Schema.Enum14[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 15 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum15[..$typeArgs] = zio.schema.Schema.Enum15[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 16 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum16[..$typeArgs] = zio.schema.Schema.Enum16[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 17 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum17[..$typeArgs] = zio.schema.Schema.Enum17[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 18 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum18[..$typeArgs] = zio.schema.Schema.Enum18[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 19 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum19[..$typeArgs] = zio.schema.Schema.Enum19[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 20 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum20[..$typeArgs] = zio.schema.Schema.Enum20[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 21 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum21[..$typeArgs] = zio.schema.Schema.Enum21[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 22 =>
          q"""{lazy val $selfRefIdent: zio.schema.Schema.Enum22[..$typeArgs] = zio.schema.Schema.Enum22[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case _ =>
          val caseSet = q"zio.schema.CaseSet.apply(..$cases).asInstanceOf[zio.schema.CaseSet.Aux[$tpe]]"

          if (typeAnnotations.isEmpty)
            q"""{lazy val $selfRefIdent: zio.schema.Schema.EnumN[$tpe,zio.schema.CaseSet.Aux[$tpe]] = zio.schema.Schema.EnumN.apply[$tpe,zio.schema.CaseSet.Aux[$tpe]]($caseSet); $selfRefIdent}"""
          else {
            val annotationArg = q"zio.Chunk.apply(..$typeAnnotations)"

            q"""{lazy val $selfRefIdent: zio.schema.Schema.EnumN[$tpe,zio.schema.CaseSet.Aux[$tpe]] = zio.schema.Schema.EnumN.apply[$tpe,zio.schema.CaseSet.Aux[$tpe]]($caseSet,$annotationArg); $selfRefIdent}"""
          }
      }
    }

    if (isCaseObject(tpe)) q"zio.schema.Schema.singleton(${tpe.typeSymbol.asClass.module})"
    else if (isCaseClass(tpe)) deriveRecord(tpe, List.empty[Frame[c.type]])
    else if (isSealedTrait(tpe))
      deriveEnum(tpe, List.empty[Frame[c.type]])
    else
      c.abort(
        c.enclosingPosition,
        s"Failed to derive schema for $tpe. Can only derive Schema for case class or sealed trait"
      )
  }

  case class Frame[C <: whitebox.Context](ctx: C, ref: String, tpe: C#Type)

}
