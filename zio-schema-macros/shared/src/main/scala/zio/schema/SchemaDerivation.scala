package zio.schema

import scala.collection.mutable
import scala.reflect.macros.whitebox
import zio.Chunk

object SchemaDerivation {
  import scala.language.experimental.macros

  implicit def gen[T]: Schema[T] = macro genImpl[T]

  def genImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val JavaAnnotationTpe = typeOf[java.lang.annotation.Annotation]

    val stack: mutable.Stack[Frame[c.type]] = new mutable.Stack[Frame[c.type]]()

    val tpe = weakTypeOf[T]

    def resolveTermSchema(tpe: Type, term: TermSymbol, selfRefName: String, selfRef: Ident): Tree =
      stack
        .find(_.tpe =:= term.typeSignature)
        .map {
          case Frame(_, ref, _) =>
            val refIdent = Ident(TermName(ref))
            q"$refIdent"
        }
        .getOrElse {
          if (term.typeSignature =:= tpe)
            c.abort(c.enclosingPosition, "Direct recursion is not supported")
          else {
            term.typeSignature.typeArgs match {
              case typeArg :: _ if typeArg =:= tpe =>
                if (term.typeSignature <:< c.typeOf[Option[_]])
                  q"zio.schema.Schema.option($selfRef)"
                else if (term.typeSignature <:< typeOf[List[_]])
                  q"zio.schema.Schema.list($selfRef)"
                else if (term.typeSignature <:< typeOf[Set[_]])
                  q"zio.schema.Schema.set($selfRef)"
                else if (term.typeSignature <:< typeOf[Vector[_]])
                  q"zio.schema.Schema.vector($selfRef)"
                else if (term.typeSignature <:< typeOf[Chunk[_]])
                  q"zio.schema.Schema.chunk($selfRef)"
                else
                  c.abort(c.enclosingPosition, s"Cannot resolve schema for type ${show(term.typeSignature)}")

              case typeArg :: _ if stack.exists(_.tpe =:= typeArg) =>
                val ref      = stack.find(_.tpe =:= typeArg).get.ref
                val refIdent = Ident(TermName(ref))
                if (term.typeSignature <:< c.typeOf[Option[_]])
                  q"zio.schema.Schema.option($refIdent)"
                else if (term.typeSignature <:< typeOf[List[_]])
                  q"zio.schema.Schema.list($refIdent)"
                else if (term.typeSignature <:< typeOf[Set[_]])
                  q"zio.schema.Schema.set($refIdent)"
                else if (term.typeSignature <:< typeOf[Vector[_]])
                  q"zio.schema.Schema.vector($refIdent)"
                else if (term.typeSignature <:< typeOf[Chunk[_]])
                  q"zio.schema.Schema.chunk($refIdent)"
                else
                  c.abort(c.enclosingPosition, s"Cannot resolve schema for type ${show(term.typeSignature)}")

              case _ =>
                c.inferImplicitValue(
                  c.typecheck(tq"zio.schema.Schema[$term]", c.TYPEmode).tpe,
                  withMacrosDisabled = false
                ) match {
                  case EmptyTree =>
                    stack.push(Frame(c, selfRefName, tpe))
                    deriveRecord(term.typeSignature)
                  case tree =>
                    tree
                }
            }
          }
        }

    def deriveRecord(tpe: Type): c.Tree = stack.find(_.tpe =:= tpe) match {
      case Some(Frame(_, ref, _)) =>
        val refIdent = Ident(TermName(ref))
        q"$refIdent"
      case None =>
        val tpeCompanion = tpe.typeSymbol.companion

        val selfRefName = c.freshName("var")
        val selfRef     = Ident(TermName(selfRefName))

        val fieldTypes: Iterable[TermSymbol] = tpe.decls.collect {
          case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p
        }
        val arity = fieldTypes.size

        val typeArgs = fieldTypes.map(ft => q"${tpe.decl(ft.name).typeSignature}") ++ Iterable(q"$tpe")

        val fieldAccessors = tpe.decls.collect {
          case p: TermSymbol if p.isCaseAccessor && p.isMethod => p.name
        }

        val fieldAnnotations: List[List[Tree]] =
          tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.headOption.map { symbols =>
            symbols.map(_.annotations.collect {
              case annotation if !(annotation.tree.tpe <:< JavaAnnotationTpe) =>
                annotation.tree
            })
          }.getOrElse(Nil)

        if (arity > 22) {
          val fields = fieldTypes.zip(fieldAnnotations).map {
            case (termSymbol, annotations) =>
              val fieldSchema = resolveTermSchema(tpe, termSymbol, selfRefName, selfRef)
              val fieldLabel  = termSymbol.name.toString.trim
              q"zio.schema.Schema.Field[${termSymbol.typeSignature}]($fieldLabel,$fieldSchema,zio.Chunk.apply[Any](..$annotations))"
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
              val fieldSchema = resolveTermSchema(tpe, termSymbol, selfRefName, selfRef)
              val fieldArg    = if (fieldTypes.size > 1) TermName(s"field${idx + 1}") else TermName("field")
              val fieldLabel  = termSymbol.name.toString.trim
              val _           = annotations
//              if (annotations.nonEmpty)
//                q"""$fieldArg = zio.schema.Schema.Field[${termSymbol.typeSignature}](label = $fieldLabel,schema = $fieldSchema, annotations = zio.Chunk.apply[Any](..$annotations))"""
//              else
              q"""$fieldArg = zio.schema.Schema.Field[${termSymbol.typeSignature}](label = $fieldLabel,schema = zio.schema.Schema.defer($fieldSchema))"""
          }

          val constructArgs = fieldTypes.zipWithIndex.map {
            case (term, idx) =>
              val arg = TermName(s"_$idx")
              q"$arg: ${term.typeSignature}"
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

          val applyArgs = Iterable(q"annotations = zio.Chunk.empty") ++ fieldDefs ++ Iterable(constructExpr) ++ extractors

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

    def deriveEnum(tpe: Type): c.Tree = ???

    if (tpe.typeSymbol.asClass.isModuleClass) q"zio.schema.Schema.singleton(${tpe.typeSymbol.asClass.module})"
    else if (tpe.typeSymbol.asClass.isCaseClass) deriveRecord(tpe)
    else if (tpe.typeSymbol.asClass.isTrait && tpe.typeSymbol.asClass.isSealed) deriveEnum(tpe)
    else c.abort(c.enclosingPosition, "Can only derive Schema for case class or sealed trait")
  }

  case class Frame[C <: whitebox.Context](ctx: C, ref: String, tpe: C#Type)

}
