package zio.schema

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

import zio.Chunk

object Derive {

  def derive[F[_], A](deriver: Deriver[F])(implicit schema: Schema[A]): F[A] = macro deriveImpl[F, A]

  def deriveImpl[F[_], A: c.WeakTypeTag](
    c: whitebox.Context
  )(deriver: c.Expr[Deriver[F]])(
    schema: c.Expr[Schema[A]]
  )(implicit ftt: c.WeakTypeTag[F[_]]): c.Tree = {
    import c.universe._

    val standardTypeTpe = c.typeOf[StandardType[_]]
    val optionTpe       = c.typeOf[Option[_]]
    val listTpe         = c.typeOf[List[_]]
    val setTpe          = c.typeOf[Set[_]]
    val vectorTpe       = c.typeOf[Vector[_]]
    val chunkTpe        = c.typeOf[Chunk[_]]
    val eitherTpe       = c.typeOf[Either[_, _]]
    val tuple2Tpe       = c.typeOf[Tuple2[_, _]]
    val tuple3Tpe       = c.typeOf[Tuple3[_, _, _]]
    val tuple4Tpe       = c.typeOf[Tuple4[_, _, _, _]]
    val tuple5Tpe       = c.typeOf[Tuple5[_, _, _, _, _]]
    val tuple6Tpe       = c.typeOf[Tuple6[_, _, _, _, _, _]]
    val tuple7Tpe       = c.typeOf[Tuple7[_, _, _, _, _, _, _]]
    val tuple8Tpe       = c.typeOf[Tuple8[_, _, _, _, _, _, _, _]]
    val tuple9Tpe       = c.typeOf[Tuple9[_, _, _, _, _, _, _, _, _]]
    val tuple10Tpe      = c.typeOf[Tuple10[_, _, _, _, _, _, _, _, _, _]]
    val tuple11Tpe      = c.typeOf[Tuple11[_, _, _, _, _, _, _, _, _, _, _]]
    val tuple12Tpe      = c.typeOf[Tuple12[_, _, _, _, _, _, _, _, _, _, _, _]]
    val tuple13Tpe      = c.typeOf[Tuple13[_, _, _, _, _, _, _, _, _, _, _, _, _]]
    val tuple14Tpe      = c.typeOf[Tuple14[_, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    val tuple15Tpe      = c.typeOf[Tuple15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    val tuple16Tpe      = c.typeOf[Tuple16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    val tuple17Tpe      = c.typeOf[Tuple17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    val tuple18Tpe      = c.typeOf[Tuple18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    val tuple19Tpe      = c.typeOf[Tuple19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    val tuple20Tpe      = c.typeOf[Tuple20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    val tuple21Tpe      = c.typeOf[Tuple21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]
    val tuple22Tpe      = c.typeOf[Tuple22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]]

    def concreteType(seenFrom: Type, tpe: Type): Type =
      tpe.asSeenFrom(seenFrom, seenFrom.typeSymbol.asClass)

    def isCaseObject(tpe: Type): Boolean = tpe.typeSymbol.asClass.isModuleClass

    def isCaseClass(tpe: Type): Boolean = tpe.typeSymbol.asClass.isCaseClass

    def isSealedTrait(tpe: Type): Boolean = tpe.typeSymbol.asClass.isTrait && tpe.typeSymbol.asClass.isSealed

    def isMap(tpe: Type): Boolean = tpe.typeSymbol.fullName == "scala.collection.immutable.Map"

    def appliedSubtype(appliedTypeArgs: Map[String, Type], tpe: Type, subtype: Type): Type =
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

    def knownSubclassesOf(appliedTypeArgs: Map[String, Type], tpe: Type): List[Type] = {
      val parent = tpe.typeSymbol.asClass
      val sortedKnownSubclasses =
        parent.knownDirectSubclasses.toList.sortBy(subclass => (subclass.pos.line, subclass.pos.column))

      sortedKnownSubclasses
        .filter(s => !s.isAbstract)
        .foreach { child =>
          child.typeSignature
          if (!child.isFinal && !child.asClass.isCaseClass)
            c.abort(c.enclosingPosition, s"child $child of $parent is neither final nor a case class")
        }

      sortedKnownSubclasses.flatMap { child =>
        child.typeSignature
        val childClass = child.asClass
        if (childClass.isSealed && childClass.isTrait) knownSubclassesOf(appliedTypeArgs, child.typeSignature)
        else if (childClass.isCaseClass) {
          val st = concreteType(concreteType(tpe, parent.asType.toType), child.asType.toType)
          Set(appliedSubtype(appliedTypeArgs, tpe, st))
        } else c.abort(c.enclosingPosition, s"child $child of $parent is not a sealed trait or case class")
      }
    }

    def toTupleSchemaType(tpes: Chunk[Type]): Tree =
      tpes.tail.foldLeft(tq"${tpes.head}") { case (xs, x) => tq"_root_.zio.schema.Schema.Tuple2[$xs, $x]" }

    def lefts(tschema: Tree, tpes: Chunk[Type], depth: Int): Tree =
      if (tpes.size < 2 || depth == 0) tschema
      else lefts(q"$tschema.left.asInstanceOf[${toTupleSchemaType(tpes)}]", tpes.init, depth - 1)

    def recurse(tpe: Type, schema: c.Tree, stack: List[Frame[c.type]], top: Boolean): Tree =
      stack.find(_.tpe =:= tpe) match {
        case Some(f @ Frame(_, ref, _)) =>
          if (f == stack.head)
            c.abort(c.enclosingPosition, "Direct recursion is not supported")
          else {
            val refIdent = Ident(TermName(ref))
            q"$refIdent"
          }
        case None =>
          val typeClassTpe       = weakTypeOf[F[_]]
          val appliedTpe         = appliedType(typeClassTpe, tpe)
          val appliedStandardTpe = appliedType(standardTypeTpe, tpe)
          val summonedTree       = if (top) EmptyTree else c.inferImplicitValue(appliedTpe)
          val summoned           = if (summonedTree == EmptyTree) q"None" else q"Some[$appliedTpe]($summonedTree)"

          val selfRefName     = c.freshName("instance")
          val selfRef         = Ident(TermName(selfRefName))
          val selfRefWithType = q"$selfRef: $appliedTpe"

          val schemaRefName = c.freshName("schema")
          val schemaRef     = Ident(TermName(schemaRefName))
          val forcedSchema  = q"_root_.zio.schema.Schema.force($schema)"

          val currentFrame = Frame[c.type](c, selfRefName, tpe)

          def tupleN(): Tree = {
            val n    = tpe.typeArgs.size
            val tpes = Chunk.fromIterable(tpe.typeArgs)

            val tschemaType = toTupleSchemaType(tpes)
            val tschema     = q"$schemaRef.schema.asInstanceOf[$tschemaType]"

            val schemas = (1 to n).map { idx =>
              if (idx == n) {
                q"$tschema.right"
              } else if (idx == 1) {
                q"${lefts(q"$tschema", tpes.init, n - 1)}.left"
              } else {
                q"${lefts(q"$tschema", tpes.init, n - idx)}.right"
              }
            }

            val instances = tpes.zip(schemas).map {
              case (t, schema) =>
                recurse(concreteType(tpe, t), schema, stack, top = false)
            }

            val flatTupleType   = tpe
            val nestedTupleType = tpes.tail.foldLeft(q"${tpes.head}") { case (xs, x) => tq"($xs, $x)" }

            val pairs = schemas.zip(instances).map {
              case (schema, instance) => q"($schema, _root_.zio.schema.Deriver.wrap($instance))"
            }

            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Transform[$nestedTupleType, $flatTupleType, _]]
              lazy val $selfRefWithType = $deriver.deriveTupleN[$flatTupleType](Chunk(..$pairs), $summoned)
              $selfRef
            }"""
          }

          val summonedStandardType = c.inferImplicitValue(appliedStandardTpe)

          if (summonedStandardType != EmptyTree) {
            q"$deriver.derivePrimitive[$tpe]($summonedStandardType, $summoned)"
          } else if (tpe <:< optionTpe) {
            val innerTpe      = tpe.typeArgs.head
            val innerSchema   = q"$schemaRef.schema"
            val innerInstance = recurse(concreteType(tpe, innerTpe), innerSchema, currentFrame +: stack, top = false)
            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Optional[$innerTpe]]
              lazy val $selfRefWithType = $deriver.deriveOption[$innerTpe]($schemaRef, $innerInstance, $summoned)
              $selfRef
            }"""
          } else if (tpe <:< listTpe) {
            val innerTpe      = tpe.typeArgs.head
            val innerSchema   = q"$schemaRef.elementSchema"
            val innerInstance = recurse(concreteType(tpe, innerTpe), innerSchema, currentFrame +: stack, top = false)
            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Sequence[${appliedType(
              listTpe,
              innerTpe
            )}, $innerTpe, _]]
              lazy val $selfRefWithType = $deriver.deriveSequence[_root_.scala.collection.immutable.List, $innerTpe]($schemaRef, $innerInstance, $summoned)
              $selfRef
            }"""
          } else if (tpe <:< vectorTpe) {
            val innerTpe      = tpe.typeArgs.head
            val innerSchema   = q"$schemaRef.elementSchema"
            val innerInstance = recurse(concreteType(tpe, innerTpe), innerSchema, currentFrame +: stack, top = false)
            q"""{
               lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Sequence[${appliedType(
              vectorTpe,
              innerTpe
            )}, $innerTpe, _]]
               lazy val $selfRefWithType = $deriver.deriveSequence[_root_.scala.collection.immutable.Vector, $innerTpe]($schemaRef, $innerInstance, $summoned)
               $selfRef
             }"""
          } else if (tpe <:< chunkTpe) {
            val innerTpe      = tpe.typeArgs.head
            val innerSchema   = q"$schemaRef.elementSchema"
            val innerInstance = recurse(concreteType(tpe, innerTpe), innerSchema, currentFrame +: stack, top = false)
            q"""{
                   lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Sequence[${appliedType(
              chunkTpe,
              innerTpe
            )}, $innerTpe, _]]
                   lazy val $selfRefWithType = $deriver.deriveSequence[_root_.zio.Chunk, $innerTpe]($schemaRef, $innerInstance, $summoned)
                   $selfRef
                 }"""
          } else if (tpe <:< setTpe) {
            val innerTpe      = tpe.typeArgs.head
            val innerSchema   = q"$schemaRef.elementSchema"
            val innerInstance = recurse(concreteType(tpe, innerTpe), innerSchema, currentFrame +: stack, top = false)
            q"""{
                  lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Set[$innerTpe]]
                  lazy val $selfRefWithType = $deriver.deriveSet[$innerTpe]($schemaRef, $innerInstance, $summoned)
                  $selfRef
                }"""
          } else if (tpe <:< eitherTpe) {
            val leftTpe  = tpe.typeArgs.head
            val rightTpe = tpe.typeArgs(1)

            val leftSchema  = q"$schemaRef.left"
            val rightSchema = q"$schemaRef.right"

            val leftInstance  = recurse(concreteType(tpe, leftTpe), leftSchema, currentFrame +: stack, top = false)
            val rightInstance = recurse(concreteType(tpe, rightTpe), rightSchema, currentFrame +: stack, top = false)

            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Either[$leftTpe, $rightTpe]]
              lazy val $selfRefWithType = $deriver.deriveEither[$leftTpe, $rightTpe]($schemaRef, $leftInstance, $rightInstance, $summoned)
              $selfRef
            }"""
          } else if (tpe <:< tuple2Tpe) {
            val leftTpe  = tpe.typeArgs.head
            val rightTpe = tpe.typeArgs(1)

            val leftSchema  = q"$schemaRef.left"
            val rightSchema = q"$schemaRef.right"

            val leftInstance  = recurse(concreteType(tpe, leftTpe), leftSchema, currentFrame +: stack, top = false)
            val rightInstance = recurse(concreteType(tpe, rightTpe), rightSchema, currentFrame +: stack, top = false)

            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Tuple2[$leftTpe, $rightTpe]]
              lazy val $selfRefWithType = $deriver.deriveTupleN[$tpe](Chunk($leftSchema -> _root_.zio.schema.Deriver.wrap($leftInstance), $rightSchema -> _root_.zio.schema.Deriver.wrap($rightInstance)), $summoned)
              $selfRef
            }"""
          } else if (tpe <:< tuple3Tpe) {
            tupleN()
          } else if (tpe <:< tuple4Tpe) {
            tupleN()
          } else if (tpe <:< tuple5Tpe) {
            tupleN()
          } else if (tpe <:< tuple6Tpe) {
            tupleN()
          } else if (tpe <:< tuple7Tpe) {
            tupleN()
          } else if (tpe <:< tuple8Tpe) {
            tupleN()
          } else if (tpe <:< tuple9Tpe) {
            tupleN()
          } else if (tpe <:< tuple10Tpe) {
            tupleN()
          } else if (tpe <:< tuple11Tpe) {
            tupleN()
          } else if (tpe <:< tuple12Tpe) {
            tupleN()
          } else if (tpe <:< tuple13Tpe) {
            tupleN()
          } else if (tpe <:< tuple14Tpe) {
            tupleN()
          } else if (tpe <:< tuple15Tpe) {
            tupleN()
          } else if (tpe <:< tuple16Tpe) {
            tupleN()
          } else if (tpe <:< tuple17Tpe) {
            tupleN()
          } else if (tpe <:< tuple18Tpe) {
            tupleN()
          } else if (tpe <:< tuple19Tpe) {
            tupleN()
          } else if (tpe <:< tuple20Tpe) {
            tupleN()
          } else if (tpe <:< tuple21Tpe) {
            tupleN()
          } else if (tpe <:< tuple22Tpe) {
            tupleN()
          } else if (isCaseObject(tpe)) {
            q"$deriver.tryDeriveRecord[$tpe]($forcedSchema.asInstanceOf[_root_.zio.schema.Schema[$tpe]], _root_.zio.Chunk.empty, $summoned)"
          } else if (isCaseClass(tpe)) {
            val fields = tpe.decls.sorted.collect {
              case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p
            }

            if (fields.size > 22) {
              val recordSchemaRef = q"$schemaRef.schema.asInstanceOf[Schema.GenericRecord]"
              val fieldInstances = fields.zipWithIndex.map {
                case (termSymbol, idx) =>
                  val fieldSchema = q"$recordSchemaRef.fields($idx).schema"
                  val f = recurse(
                    concreteType(tpe, termSymbol.typeSignature),
                    fieldSchema,
                    currentFrame +: stack,
                    top = false
                  )
                  q"_root_.zio.schema.Deriver.wrap($f)"
              }

              q"""{
                lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Transform[_root_.scala.collection.immutable.ListMap[String, _], $tpe, _]]
                lazy val $selfRefWithType = $deriver.deriveTransformedRecord[_root_.scala.collection.immutable.ListMap[String, _], $tpe]($recordSchemaRef, $schemaRef, _root_.zio.Chunk(..$fieldInstances), $summoned)
                $selfRef
              }"""
            } else {
              val fieldInstances = fields.zipWithIndex.map {
                case (termSymbol, idx) =>
                  val fieldSchema = q"$schemaRef.fields($idx).schema"
                  val f = recurse(
                    concreteType(tpe, termSymbol.typeSignature),
                    fieldSchema,
                    currentFrame +: stack,
                    top = false
                  )
                  q"_root_.zio.schema.Deriver.wrap($f)"
              }

              q"""{
                  lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Record[$tpe]]
                  lazy val $selfRefWithType = $deriver.tryDeriveRecord[$tpe]($forcedSchema.asInstanceOf[_root_.zio.schema.Schema[$tpe]], _root_.zio.Chunk(..$fieldInstances), $summoned)
                  $selfRef
                }"""
            }
          } else if (isSealedTrait(tpe)) {
            val appliedTypeArgs: Map[String, Type] =
              tpe.typeConstructor.typeParams.map(_.name.toString).zip(tpe.typeArgs).toMap
            val subtypes = knownSubclassesOf(appliedTypeArgs, tpe).map(concreteType(tpe, _))
            val subtypeInstances = subtypes.zipWithIndex.map {
              case (subtype, idx) =>
                val subtypeSchema = q"$schemaRef.cases($idx).schema"
                val f             = recurse(subtype, subtypeSchema, currentFrame +: stack, top = false)
                q"_root_.zio.schema.Deriver.wrap($f)"
            }
            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Enum[$tpe]]
              lazy val $selfRefWithType = $deriver.tryDeriveEnum[$tpe]($forcedSchema.asInstanceOf[_root_.zio.schema.Schema[$tpe]], _root_.zio.Chunk(..$subtypeInstances), $summoned)
              $selfRef
            }"""
          } else if (isMap(tpe)) {
            val keyTpe   = tpe.typeArgs.head
            val valueTpe = tpe.typeArgs(1)

            val keySchema   = q"$schemaRef.keySchema"
            val valueSchema = q"$schemaRef.valueSchema"

            val keyInstance   = recurse(concreteType(tpe, keyTpe), keySchema, currentFrame +: stack, top = false)
            val valueInstance = recurse(concreteType(tpe, valueTpe), valueSchema, currentFrame +: stack, top = false)

            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Map[$keyTpe, $valueTpe]]
              lazy val $selfRefWithType = $deriver.deriveMap[$keyTpe, $valueTpe]($schemaRef, $keyInstance, $valueInstance, $summoned)
              $selfRef
            }"""
          } else
            q"""$deriver.deriveUnknown[$tpe]($summoned)"""
      }

    recurse(weakTypeOf[A], schema.tree, List.empty[Frame[c.type]], top = true)

  }

  final case class Frame[C <: whitebox.Context](ctx: C, ref: String, tpe: C#Type)
}
