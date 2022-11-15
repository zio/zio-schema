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
    // NOTE: DeriveSchema only supports tuple arities 2-4 currently

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

    def recurse(tpe: Type, schema: c.Tree, stack: List[Frame[c.type]]): Tree =
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
          val summonedTree       = c.inferImplicitValue(appliedTpe)
          val summoned           = if (summonedTree == EmptyTree) q"None" else q"Some[$appliedTpe]($summonedTree)"

          val selfRefName     = c.freshName("instance")
          val selfRef         = Ident(TermName(selfRefName))
          val selfRefWithType = q"$selfRef: $appliedTpe"

          val schemaRefName = c.freshName("schema")
          val schemaRef     = Ident(TermName(schemaRefName))
          val forcedSchema  = q"_root_.zio.schema.Schema.force($schema)"

          val currentFrame = Frame[c.type](c, selfRefName, tpe)

          val summonedStandardType = c.inferImplicitValue(appliedStandardTpe)

          if (summonedStandardType != EmptyTree) {
            q"$deriver.derivePrimitive[$tpe]($summonedStandardType, $summoned)"
          } else if (tpe <:< optionTpe) {
            val innerTpe      = tpe.typeArgs.head
            val innerSchema   = q"$schemaRef.schema"
            val innerInstance = recurse(concreteType(tpe, innerTpe), innerSchema, currentFrame +: stack)
            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Optional[$innerTpe]]
              lazy val $selfRefWithType = $deriver.deriveOption[$innerTpe]($schemaRef, $innerInstance, $summoned)
              $selfRef
            }"""
          } else if (tpe <:< listTpe) {
            val innerTpe      = tpe.typeArgs.head
            val innerSchema   = q"$schemaRef.elementSchema"
            val innerInstance = recurse(concreteType(tpe, innerTpe), innerSchema, currentFrame +: stack)
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
            val innerInstance = recurse(concreteType(tpe, innerTpe), innerSchema, currentFrame +: stack)
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
            val innerInstance = recurse(concreteType(tpe, innerTpe), innerSchema, currentFrame +: stack)
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
            val innerInstance = recurse(concreteType(tpe, innerTpe), innerSchema, currentFrame +: stack)
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

            val leftInstance  = recurse(concreteType(tpe, leftTpe), leftSchema, currentFrame +: stack)
            val rightInstance = recurse(concreteType(tpe, rightTpe), rightSchema, currentFrame +: stack)

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

            val leftInstance  = recurse(concreteType(tpe, leftTpe), leftSchema, currentFrame +: stack)
            val rightInstance = recurse(concreteType(tpe, rightTpe), rightSchema, currentFrame +: stack)

            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Tuple2[$leftTpe, $rightTpe]]
              lazy val $selfRefWithType = $deriver.deriveTuple2[$leftTpe, $rightTpe]($schemaRef, $leftInstance, $rightInstance, $summoned)
              $selfRef
            }"""
          } else if (tpe <:< tuple3Tpe) {
            // NOTE: Higher arity tuples cannot be treated as Records because they are constructed in the form of Transform(Tuple2(Tuple2(...))
            val t1Tpe = tpe.typeArgs.head
            val t2Tpe = tpe.typeArgs(1)
            val t3Tpe = tpe.typeArgs(2)

            val t123schema = q"$schemaRef.schema.asInstanceOf[Schema.Tuple2[Schema.Tuple2[$t1Tpe, $t2Tpe], $t3Tpe]]"
            val t12schema  = q"$t123schema.left.asInstanceOf[Schema.Tuple2[$t1Tpe, $t2Tpe]]"
            val t1Schema   = q"$t12schema.left"
            val t2Schema   = q"$t12schema.right"
            val t3Schema   = q"$t123schema.right"

            val t1Instance = recurse(concreteType(tpe, t1Tpe), t1Schema, currentFrame +: stack)
            val t2Instance = recurse(concreteType(tpe, t2Tpe), t2Schema, currentFrame +: stack)
            val t3Instance = recurse(concreteType(tpe, t3Tpe), t3Schema, currentFrame +: stack)

            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Transform[(($t1Tpe, $t2Tpe), $t3Tpe), ($t1Tpe, $t2Tpe, $t3Tpe), _]]
              lazy val $selfRefWithType = $deriver.deriveTuple3[$t1Tpe, $t2Tpe, $t3Tpe]($t123schema, $schemaRef, $t1Instance, $t2Instance, $t3Instance, $summoned)
              $selfRef
            }"""
          } else if (tpe <:< tuple4Tpe) {
            val t1Tpe = tpe.typeArgs.head
            val t2Tpe = tpe.typeArgs(1)
            val t3Tpe = tpe.typeArgs(2)
            val t4Tpe = tpe.typeArgs(2)

            val t1234schema =
              q"$schemaRef.schema.asInstanceOf[Schema.Tuple2[Schema.Tuple2[Schema.Tuple2[$t1Tpe, $t2Tpe], $t3Tpe], $t4Tpe]]"
            val t123schema = q"$t1234schema.left.asInstanceOf[Schema.Tuple2[Schema.Tuple2[$t1Tpe, $t2Tpe], $t3Tpe]]"
            val t12schema  = q"$t123schema.left.asInstanceOf[Schema.Tuple2[$t1Tpe, $t2Tpe]]"
            val t1Schema   = q"$t12schema.left"
            val t2Schema   = q"$t12schema.right"
            val t3Schema   = q"$t123schema.right"
            val t4Schema   = q"$t1234schema.right"

            val t1Instance = recurse(concreteType(tpe, t1Tpe), t1Schema, currentFrame +: stack)
            val t2Instance = recurse(concreteType(tpe, t2Tpe), t2Schema, currentFrame +: stack)
            val t3Instance = recurse(concreteType(tpe, t3Tpe), t3Schema, currentFrame +: stack)
            val t4Instance = recurse(concreteType(tpe, t4Tpe), t4Schema, currentFrame +: stack)

            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Transform[((($t1Tpe, $t2Tpe), $t3Tpe), $t4Tpe), ($t1Tpe, $t2Tpe, $t3Tpe, $t4Tpe), _]]
              lazy val $selfRefWithType = $deriver.deriveTuple4[$t1Tpe, $t2Tpe, $t3Tpe, $t4Tpe]($t1234schema, $schemaRef, $t1Instance, $t2Instance, $t3Instance, $t4Instance, $summoned)
              $selfRef
            }"""
          } else if (isCaseObject(tpe)) {
            q"$deriver.deriveRecord[$tpe]($forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Record[$tpe]], _root_.zio.Chunk.empty, $summoned)"
          } else if (isCaseClass(tpe)) {
            val fields = tpe.decls.sorted.collect {
              case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p
            }

            if (fields.size > 22) {
              val recordSchemaRef = q"$schemaRef.schema.asInstanceOf[Schema.GenericRecord]"
              val fieldInstances = fields.zipWithIndex.map {
                case (termSymbol, idx) =>
                  val fieldSchema = q"$recordSchemaRef.fields($idx).schema"
                  val f           = recurse(concreteType(tpe, termSymbol.typeSignature), fieldSchema, currentFrame +: stack)
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
                  val f           = recurse(concreteType(tpe, termSymbol.typeSignature), fieldSchema, currentFrame +: stack)
                  q"_root_.zio.schema.Deriver.wrap($f)"
              }

              q"""{
                  lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Record[$tpe]]
                  lazy val $selfRefWithType = $deriver.deriveRecord[$tpe]($schemaRef, _root_.zio.Chunk(..$fieldInstances), $summoned)
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
                val f             = recurse(subtype, subtypeSchema, currentFrame +: stack)
                q"_root_.zio.schema.Deriver.wrap($f)"
            }
            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Enum[$tpe]]
              lazy val $selfRefWithType = $deriver.deriveEnum[$tpe]($schemaRef, _root_.zio.Chunk(..$subtypeInstances), $summoned)
              $selfRef
            }"""
          } else if (isMap(tpe)) {
            val keyTpe   = tpe.typeArgs.head
            val valueTpe = tpe.typeArgs(1)

            val keySchema   = q"$schemaRef.keySchema"
            val valueSchema = q"$schemaRef.valueSchema"

            val keyInstance   = recurse(concreteType(tpe, keyTpe), keySchema, currentFrame +: stack)
            val valueInstance = recurse(concreteType(tpe, valueTpe), valueSchema, currentFrame +: stack)

            q"""{
              lazy val $schemaRef = $forcedSchema.asInstanceOf[_root_.zio.schema.Schema.Map[$keyTpe, $valueTpe]]
              lazy val $selfRefWithType = $deriver.deriveMap[$keyTpe, $valueTpe]($schemaRef, $keyInstance, $valueInstance, $summoned)
              $selfRef
            }"""
          } else
            c.abort(
              c.enclosingPosition,
              s"Failed to derive type class for $tpe"
            )
      }

    val tree = recurse(weakTypeOf[A], schema.tree, List.empty[Frame[c.type]])
    tree
  }

  final case class Frame[C <: whitebox.Context](ctx: C, ref: String, tpe: C#Type)
}
