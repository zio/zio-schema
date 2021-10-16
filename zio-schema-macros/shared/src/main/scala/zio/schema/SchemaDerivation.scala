package zio.schema

import zio.Chunk

import scala.reflect.macros.whitebox

object SchemaDerivation {
  import scala.language.experimental.macros

  implicit def gen[T]: Schema[T] = macro genImpl[T]

  def genImpl[T: c.WeakTypeTag](c: whitebox.Context): c.Tree = {
    import c.universe._

    val tpe = weakTypeOf[T]

    def resolveTermSchema(term: TermSymbol): Tree =
      if (term.typeSignature =:= tpe) q"zio.schema.Schema.defer(v)"
      else if (term.typeSignature.typeArgs.exists(_ =:= tpe)) {
        if (term.typeSignature <:< typeOf[Option[_]])
          q"""
             zio.schema.Schema.defer(Schema.option(v))
           """
        else if (term.typeSignature <:< typeOf[List[_]])
          q"""
             zio.schema.Schema.defer(Schema.list(v))
           """
        else if (term.typeSignature <:< typeOf[Set[_]])
          q"""
             zio.schema.Schema.defer(Schema.set(v))
           """
        else if (term.typeSignature <:< typeOf[Vector[_]])
          q"""
             zio.schema.Schema.defer(Schema.vector(v))
           """
        else if (term.typeSignature <:< typeOf[Chunk[_]])
          q"""
             zio.schema.Schema.defer(Schema.chunk(v))
           """
        else
          c.abort(c.enclosingPosition, s"Cannot resolve schema for type ${show(term.typeSignature)}")
      } else {
        c.inferImplicitValue(c.typecheck(tq"zio.schema.Schema[$term]", c.TYPEmode).tpe, withMacrosDisabled = false)
      }
    //      else q"zio.schema.Schema.defer(${c.inferImplicitValue(c.typecheck(fieldSchemaType, c.TYPEmode).tpe)})"

    def deriveRecord(tpe: Type): c.Tree = {
      val tpeCompanion = tpe.typeSymbol.companion

      val fieldTypes: Iterable[TermSymbol] = tpe.decls.collect {
        case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p
      }
      val typeArgs = fieldTypes.map(ft => q"${tpe.decl(ft.name).typeSignature}") ++ Iterable(q"$tpe")

      val schemaType = {
        fieldTypes.size match {
          case 1 => q"zio.schema.Schema.CaseClass1[..$typeArgs]"
          case 2 => q"zio.schema.Schema.CaseClass2[..$typeArgs]"
          case 3 => q"zio.schema.Schema.CaseClass3[..$typeArgs]"
          case 4 => q"zio.schema.Schema.CaseClass4[..$typeArgs]"
          case 5 => q"zio.schema.Schema.CaseClass5[..$typeArgs]"
          case 6 => q"zio.schema.Schema.CaseClass6[..$typeArgs]"
          case 7 => q"zio.schema.Schema.CaseClass7[..$typeArgs]"
          case 8 => q"zio.schema.Schema.CaseClass8[..$typeArgs]"
          case 9 => q"zio.schema.Schema.CaseClass9[..$typeArgs]"
          case _ =>
            c.abort(
              tpe.termSymbol.pos,
              s"Only handling case class 3 right now but got ${show(tpe)}, ${show(tpeCompanion)}"
            )
        }
      }

      val fieldLabels = tpe.decls.collect {
        case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p.name.toString.trim()
      }

      val fieldDefs = fieldLabels.zip(fieldTypes).zipWithIndex.map {
        case ((label, termSymbol), idx) =>
          val fieldSchema = resolveTermSchema(termSymbol)
          val fieldArg    = if (fieldTypes.size > 1) TermName(s"field${idx + 1}") else TermName("field")
          val fieldLabel  = label

          q"""$fieldArg = zio.schema.Schema.Field[${termSymbol.typeSignature}]($fieldLabel,$fieldSchema)"""
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

      val extractors = fieldLabels.zipWithIndex.foldLeft(Seq.empty[c.universe.Tree]) {
        case (acc, (fieldLabel, idx)) =>
          val argName = if (fieldTypes.size > 1) TermName(s"extractField${idx + 1}") else TermName("extractField")
          acc :+ q"$argName = (t: $tpe) => t.${TermName(fieldLabel)}"
      }

      val applyArgs = Iterable(q"annotations = zio.Chunk.empty") ++ fieldDefs ++ Iterable(constructExpr) ++ extractors

      val instance = fieldTypes.size match {
        case 1 =>
          q"""{
            lazy val v: zio.schema.Schema.CaseClass1[..$typeArgs] = $schemaType(..$applyArgs)
            v
          }"""
        case 2 =>
          q"""{
            lazy val v: zio.schema.Schema.CaseClass2[..$typeArgs] = $schemaType(..$applyArgs)
            v
          }"""
        case 3 =>
          q"""{
            lazy val v: zio.schema.Schema.CaseClass3[..$typeArgs] = $schemaType(..$applyArgs)
            v
          }"""
        case 4 =>
          q"""{
            lazy val v: zio.schema.Schema.CaseClass4[..$typeArgs] = $schemaType(..$applyArgs)
            v
          }"""
        case 5 =>
          q"""{
            lazy val v: zio.schema.Schema.CaseClass5[..$typeArgs] = $schemaType(..$applyArgs)
            v
          }"""
        case 6 =>
          q"""{
            lazy val v: zio.schema.Schema.CaseClass6[..$typeArgs] = $schemaType(..$applyArgs)
            v
          }"""
        case 7 =>
          q"""{
            lazy val v: zio.schema.Schema.CaseClass7[..$typeArgs] = $schemaType(..$applyArgs)
            v
          }"""
        case 8 =>
          q"""{
            lazy val v: zio.schema.Schema.CaseClass8[..$typeArgs] = $schemaType(..$applyArgs)
            v
          }"""
        case 9 =>
          q"""{
            lazy val v: zio.schema.Schema.CaseClass9[..$typeArgs] = $schemaType(..$applyArgs)
            v
          }"""
        case _ =>
          c.abort(
            tpe.termSymbol.pos,
            s"Only handling case class 3 right now but got ${show(tpe)}, ${show(tpeCompanion)}"
          )
      }

//      println(s"Instance:\n${show(instance)}")

      instance

    }

    def deriveEnum(tpe: Type): c.Tree = ???

    if (tpe.typeSymbol.asClass.isModuleClass) q"zio.schema.Schema.singleton(${tpe.typeSymbol.asClass.module})"
    else if (tpe.typeSymbol.asClass.isCaseClass) deriveRecord(tpe)
    else if (tpe.typeSymbol.asClass.isTrait && tpe.typeSymbol.asClass.isSealed) deriveEnum(tpe)
    else c.abort(c.enclosingPosition, "Can only derive Schema for case class or sealed trait")
  }

}
