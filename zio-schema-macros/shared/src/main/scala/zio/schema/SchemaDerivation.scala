package zio.schema

object SchemaDerivation {
  import scala.language.experimental.macros
  import scala.reflect.macros.whitebox.Context

  implicit def gen[T]: Schema[T] = macro genImpl[T]

  def genImpl[T: c.WeakTypeTag](c: Context): c.Tree = {
    import c.universe._

    val _tag         = c.weakTypeTag[T]
    val tpe          = weakTypeOf[T]
    val tpeCompanion = tpe.typeSymbol.companion

    val fieldTypes: Iterable[TermSymbol] = tpe.decls.collect {
      case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p
    }

    val schemaType = {
      // val arity = fieldTypes.size
      // if (arity <= 22) {
      //   val typeArgs = fieldTypes.map(ft => q"${tpe.decl(ft.name).typeSignature}") ++ Iterable(q"$tpe")
      //   val className = q"zio.schema.Schema.CaseClass$arity"
      //   val clazz = tq"$className"
      //   q"$clazz[..$typeArgs]"
      // } else {
      //   c.abort(tpe.termSymbol.pos, "GenericRecord not supported yet")
      // }
      fieldTypes.size match {
        case 1 =>
          val typeArgs = fieldTypes.map(ft => q"${tpe.decl(ft.name).typeSignature}") ++ Iterable(q"$tpe")
          q"zio.schema.Schema.CaseClass1[..$typeArgs]"
        case 2 =>
          val typeArgs = fieldTypes.map(ft => q"${tpe.decl(ft.name).typeSignature}") ++ Iterable(q"$tpe")
          q"zio.schema.Schema.CaseClass2[..$typeArgs]"
        case 3 =>
          val typeArgs = fieldTypes.map(ft => q"${tpe.decl(ft.name).typeSignature}") ++ Iterable(q"$tpe")
          q"zio.schema.Schema.CaseClass3[..$typeArgs]"
        case _ => c.abort(tpe.termSymbol.pos, "Only handling case class 3 right now")
      }
    }

    val fieldLabels = _tag.tpe.decls.collect {
      case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p.name.toString().trim()
    }

    val fieldDefs = fieldLabels.zip(fieldTypes).zipWithIndex.map {
      case ((label, termSymbol), idx) =>
        val fieldSchemaType = tq"zio.schema.Schema[$termSymbol]"
        val fieldSchema     = c.inferImplicitValue(c.typecheck(fieldSchemaType, c.TYPEmode).tpe)
        val fieldArg        = if (fieldTypes.size > 1) TermName(s"field${idx + 1}") else TermName("field")
        val fieldLael       = label

        q"""$fieldArg = zio.schema.Schema.Field[${termSymbol.typeSignature}]($fieldLael,$fieldSchema)"""
    }

    println(s"Field Defs:\n${fieldDefs.mkString("\n")}")

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

    println(s"Constructor: $constructExpr")

    val extractors = fieldLabels.zipWithIndex.foldLeft(Seq.empty[c.universe.Tree]) {
      case (acc, (fieldLabel, idx)) =>
        val argName = if (fieldTypes.size > 1) TermName(s"extractField${idx + 1}") else TermName("extractField")
        acc :+ q"$argName = (t: $tpe) => t.${TermName(fieldLabel)}"
    }

    println(s"Extractors:\n${extractors.mkString("\n")}")

    val applyArgs = Iterable(q"annotations = zio.Chunk.empty") ++ fieldDefs ++ Iterable(constructExpr) ++ extractors

    val instance = q"$schemaType(..$applyArgs)"

    println(s"Instance:\n$instance")

    // c.Expr(instance)
    instance
  }

}
