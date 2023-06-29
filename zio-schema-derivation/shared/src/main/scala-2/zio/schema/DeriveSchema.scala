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

    def isMap(tpe: Type): Boolean = tpe.typeSymbol.fullName == "scala.collection.immutable.Map"

    @nowarn def collectTypeAnnotations(tpe: Type): List[Tree] =
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

    def recurse(tpe: Type, stack: List[Frame[c.type]]): Tree =
      if (isCaseObject(tpe)) {
        val typeId          = q"_root_.zio.schema.TypeId.parse(${tpe.typeSymbol.asClass.fullName})"
        val typeAnnotations = collectTypeAnnotations(tpe)
        val annotations =
          if (typeAnnotations.isEmpty) q"_root_.zio.Chunk.empty"
          else q"_root_.zio.Chunk.apply(..$typeAnnotations)"
        q"_root_.zio.schema.Schema.CaseClass0($typeId, () => ${tpe.typeSymbol.asClass.module}, $annotations)"
      } else if (isCaseClass(tpe)) deriveRecord(tpe, stack)
      else if (isSealedTrait(tpe))
        deriveEnum(tpe, stack)
      else if (isMap(tpe)) deriveMap(tpe)
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
            q"_root_.zio.schema.Schema.defer($refIdent)"
        }
        .getOrElse {
          if (schemaType =:= parentType)
            c.abort(c.enclosingPosition, "Direct recursion is not supported")
          else {
            c.inferImplicitValue(
              c.typecheck(tq"_root_.zio.schema.Schema[$schemaType]", c.TYPEmode).tpe,
              withMacrosDisabled = false
            ) match {
              case EmptyTree =>
                schemaType.typeArgs match {
                  case Nil =>
                    recurse(schemaType, stack)
                  case typeArg1 :: Nil =>
                    if (schemaType <:< c.typeOf[Option[_]])
                      q"_root_.zio.schema.Schema.option(_root_.zio.schema.Schema.defer(${directInferSchema(parentType, concreteType(parentType, typeArg1), stack)}))"
                    else if (schemaType <:< typeOf[List[_]])
                      q"_root_.zio.schema.Schema.list(_root_.zio.schema.Schema.defer(${directInferSchema(parentType, concreteType(parentType, typeArg1), stack)}))"
                    else if (schemaType <:< typeOf[Set[_]])
                      q"_root_.zio.schema.Schema.set(_root_.zio.schema.Schema.defer(${directInferSchema(parentType, concreteType(parentType, typeArg1), stack)}))"
                    else if (schemaType <:< typeOf[Vector[_]])
                      q"_root_.zio.schema.Schema.vector(_root_.zio.schema.Schema.defer(${directInferSchema(parentType, concreteType(parentType, typeArg1), stack)}))"
                    else if (schemaType <:< typeOf[Chunk[_]])
                      q"_root_.zio.schema.Schema.chunk(_root_.zio.schema.Schema.defer(${directInferSchema(parentType, concreteType(parentType, typeArg1), stack)}))"
                    else
                      recurse(schemaType, stack)
                  case typeArg1 :: typeArg2 :: Nil =>
                    if (schemaType <:< typeOf[Either[_, _]])
                      q"""_root_.zio.schema.Schema.either(
                        _root_.zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg1),
                        stack
                      )}),
                        _root_.zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg2),
                        stack
                      )})
                      )
                   """
                    else if (schemaType <:< typeOf[(_, _)])
                      q"""_root_.zio.schema.Schema.tuple2(
                        _root_.zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg1),
                        stack
                      )}),
                        _root_.zio.schema.Schema.defer(${directInferSchema(
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
                      q"""_root_.zio.schema.Schema.tuple3(
                        _root_.zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg1),
                        stack
                      )}),
                        _root_.zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg2),
                        stack
                      )}),
                        _root_.zio.schema.Schema.defer(${directInferSchema(
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
                      q"""_root_.zio.schema.Schema.tuple4(
                        _root_.zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg1),
                        stack
                      )}),
                        _root_.zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg2),
                        stack
                      )}),
                        _root_.zio.schema.Schema.defer(${directInferSchema(
                        parentType,
                        concreteType(parentType, typeArg3),
                        stack
                      )}),
                        _root_.zio.schema.Schema.defer(${directInferSchema(
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
                q"_root_.zio.schema.Schema.defer($tree)"
            }
          }
        }

    @nowarn
    def getFieldName(annotations: List[Tree]): Option[String] =
      annotations.collectFirst {
        case q"new fieldName($name1)" => name1.toString
      }.map(s => s.substring(1, s.length() - 1))

    def deriveRecord(tpe: Type, stack: List[Frame[c.type]]): Tree = stack.find(_.tpe =:= tpe) match {
      case Some(Frame(_, ref, _)) =>
        val refIdent = Ident(TermName(ref))
        q"$refIdent"
      case None =>
        val tpeCompanion = tpe.typeSymbol.companion

        val selfRefName = c.freshName("var")
        val selfRef     = Ident(TermName(selfRefName))

        val currentFrame = Frame[c.type](c, selfRefName, tpe)

        val sortedDecls = tpe.decls.sorted
        val fieldTypes: Iterable[TermSymbol] = sortedDecls.collect {
          case p: TermSymbol if p.isCaseAccessor && !p.isMethod => p
        }
        val arity = fieldTypes.size

        val typeArgs = fieldTypes.map { ft =>
          val fieldType = concreteType(tpe, tpe.decl(ft.name).typeSignature)
          q"$fieldType"
        } ++ Iterable(q"$tpe")

        val fieldAccessors = sortedDecls.collect {
          case p: TermSymbol if p.isCaseAccessor && p.isMethod => p.name
        }

        val typeId = q"_root_.zio.schema.TypeId.parse(${tpe.typeSymbol.fullName})"

        val typeAnnotations: List[Tree] = collectTypeAnnotations(tpe)

        val defaultConstructorValues =
          tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.head
            .map(_.asTerm)
            .zipWithIndex
            .flatMap {
              case (symbol, i) =>
                if (symbol.isParamWithDefault) {
                  val defaultInit  = tpe.companion.member(TermName(s"$$lessinit$$greater$$default$$${i + 1}"))
                  val defaultApply = tpe.companion.member(TermName(s"apply$$default$$${i + 1}"))
                  Some(i -> defaultInit)
                    .filter(_ => defaultInit != NoSymbol)
                    .orElse(Some(i -> defaultApply).filter(_ => defaultApply != NoSymbol))
                } else None
            }
            .toMap

        @nowarn
        val fieldAnnotations: List[List[Tree]] = //List.fill(arity)(Nil)
          tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.headOption.map { symbols =>
            symbols.zipWithIndex.map {
              case (symbol, i) =>
                val annotations = symbol.annotations.collect {
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
                }
                val hasDefaultAnnotation =
                  annotations.exists {
                    case q"new _root_.zio.schema.annotation.fieldDefaultValue(..$args)" => true
                    case _                                                              => false
                  }
                if (hasDefaultAnnotation || defaultConstructorValues.get(i).isEmpty) {
                  annotations
                } else {
                  annotations :+
                    q"new _root_.zio.schema.annotation.fieldDefaultValue[${symbol.typeSignature}](${defaultConstructorValues(i)})"

                }

            }.filter(_ != EmptyTree)
          }.getOrElse(Nil)

        @nowarn
        val fieldValidations: List[Tree] =
          tpe.typeSymbol.asClass.primaryConstructor.asMethod.paramLists.headOption.map { symbols =>
            symbols.map { symbol =>
              symbol.annotations.collect {
                case annotation if (annotation.tree.tpe.toString.startsWith("zio.schema.annotation.validate")) =>
                  annotation.tree match {
                    case q"new $annConstructor(..$annotationArgs)" =>
                      q"..$annotationArgs"
                    case tree =>
                      c.warning(c.enclosingPosition, s"Unhandled annotation tree $tree")
                      EmptyTree
                  }
              }
            }.filter(_ != EmptyTree)
              .map(_.foldLeft[c.universe.Tree](q"_root_.zio.schema.validation.Validation.succeed") {
                case (acc, t) => q"$acc && $t"
              })
          }.getOrElse(Nil)

        if (arity > 22) {
          val fields = fieldTypes.zip(fieldAnnotations).map {
            case (termSymbol, annotations) =>
              val fieldType = concreteType(tpe, termSymbol.typeSignature)
              val fieldSchema = directInferSchema(
                tpe,
                concreteType(tpe, termSymbol.typeSignature),
                currentFrame +: stack
              )
              val fieldLabel = termSymbol.name.toString.trim
              val getFunc =
                q" (z: _root_.scala.collection.immutable.ListMap[String, _]) => z.apply($fieldLabel).asInstanceOf[${termSymbol.typeSignature}]"

              val setFunc =
                q" (z: _root_.scala.collection.immutable.ListMap[String, _], v: ${termSymbol.typeSignature}) => z.updated($fieldLabel, v)"

              if (annotations.nonEmpty) {
                val newName       = getFieldName(annotations).getOrElse(fieldLabel)
                val singletonType = tq"${newName}.type"
                q"_root_.zio.schema.Schema.Field.apply(name0 = $newName, schema0 = $fieldSchema, annotations0 = _root_.zio.Chunk.apply[Any](..$annotations), get0 = $getFunc, set0 = $setFunc).asInstanceOf[_root_.zio.schema.Schema.Field.WithFieldName[scala.collection.immutable.ListMap[String, _], $singletonType, ${fieldType}]]"
              } else {
                val singletonType = tq"${fieldLabel}.type"
                q"_root_.zio.schema.Schema.Field.apply(name0 = $fieldLabel, schema0 = $fieldSchema, get0 = $getFunc, set0 = $setFunc).asInstanceOf[_root_.zio.schema.Schema.Field.WithFieldName[scala.collection.immutable.ListMap[String, _], $singletonType, ${fieldType}]]"
              }
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

          val applyArgs =
            if (typeAnnotations.isEmpty) Iterable(q"annotations = _root_.zio.Chunk.empty")
            else Iterable(q"annotations = _root_.zio.Chunk.apply(..$typeAnnotations)")

          q"""_root_.zio.schema.Schema.GenericRecord($typeId, _root_.zio.schema.FieldSet.fromFields(..$fields), ..$applyArgs).transformOrFail[$tpe]($fromMap,$toMap)"""
        } else {
          val schemaType = arity match {
            case 0  => q"_root_.zio.schema.Schema.CaseClass0[..$typeArgs]"
            case 1  => q"_root_.zio.schema.Schema.CaseClass1[..$typeArgs]"
            case 2  => q"_root_.zio.schema.Schema.CaseClass2[..$typeArgs]"
            case 3  => q"_root_.zio.schema.Schema.CaseClass3[..$typeArgs]"
            case 4  => q"_root_.zio.schema.Schema.CaseClass4[..$typeArgs]"
            case 5  => q"_root_.zio.schema.Schema.CaseClass5[..$typeArgs]"
            case 6  => q"_root_.zio.schema.Schema.CaseClass6[..$typeArgs]"
            case 7  => q"_root_.zio.schema.Schema.CaseClass7[..$typeArgs]"
            case 8  => q"_root_.zio.schema.Schema.CaseClass8[..$typeArgs]"
            case 9  => q"_root_.zio.schema.Schema.CaseClass9[..$typeArgs]"
            case 10 => q"_root_.zio.schema.Schema.CaseClass10[..$typeArgs]"
            case 11 => q"_root_.zio.schema.Schema.CaseClass11[..$typeArgs]"
            case 12 => q"_root_.zio.schema.Schema.CaseClass12[..$typeArgs]"
            case 13 => q"_root_.zio.schema.Schema.CaseClass13[..$typeArgs]"
            case 14 => q"_root_.zio.schema.Schema.CaseClass14[..$typeArgs]"
            case 15 => q"_root_.zio.schema.Schema.CaseClass15[..$typeArgs]"
            case 16 => q"_root_.zio.schema.Schema.CaseClass16[..$typeArgs]"
            case 17 => q"_root_.zio.schema.Schema.CaseClass17[..$typeArgs]"
            case 18 => q"_root_.zio.schema.Schema.CaseClass18[..$typeArgs]"
            case 19 => q"_root_.zio.schema.Schema.CaseClass19[..$typeArgs]"
            case 20 => q"_root_.zio.schema.Schema.CaseClass20[..$typeArgs]"
            case 21 => q"_root_.zio.schema.Schema.CaseClass21[..$typeArgs]"
            case 22 => q"_root_.zio.schema.Schema.CaseClass22[..$typeArgs]"
          }

          val fieldDefs = fieldTypes.zip(fieldAnnotations).zip(fieldValidations).zipWithIndex.map {
            case (((termSymbol, annotations), validation), idx) =>
              val fieldType = concreteType(tpe, termSymbol.typeSignature)
              val fieldSchema = directInferSchema(
                tpe,
                fieldType,
                currentFrame +: stack
              )
              val fieldArg   = if (fieldTypes.size > 1) TermName(s"field0${idx + 1}") else TermName("field0")
              val fieldLabel = termSymbol.name.toString.trim
              val getArg     = TermName(fieldLabel)

              val getFunc = q" (z: $tpe) => z.$getArg"
              val setFunc = q" (z: $tpe, v: ${fieldType}) => z.copy($getArg = v)"

              if (annotations.nonEmpty) {
                val newName       = getFieldName(annotations).getOrElse(fieldLabel)
                val singletonType = tq"${newName}.type"
                q"""$fieldArg = _root_.zio.schema.Schema.Field.apply(name0 = $newName, schema0 = $fieldSchema, annotations0 = _root_.zio.Chunk.apply[Any](..$annotations), validation0 = $validation, get0 = $getFunc, set0 = $setFunc).asInstanceOf[_root_.zio.schema.Schema.Field.WithFieldName[$tpe, $singletonType, $fieldType]]"""
              } else {
                val singletonType = tq"${fieldLabel}.type"
                q"""$fieldArg = _root_.zio.schema.Schema.Field.apply(name0 = $fieldLabel, schema0 = $fieldSchema, validation0 = $validation, get0 = $getFunc, set0 = $setFunc).asInstanceOf[_root_.zio.schema.Schema.Field.WithFieldName[$tpe, $singletonType, $fieldType]]"""
              }
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

          val constructExpr =
            if (arity < 2)
              q"defaultConstruct0 = (..$constructArgs) => $tpeCompanion(..$constructApplyArgs)"
            else
              q"construct0 = (..$constructArgs) => $tpeCompanion(..$constructApplyArgs)"

          val applyArgs =
            if (typeAnnotations.isEmpty)
              Iterable(q"annotations0 = _root_.zio.Chunk.empty") ++ Iterable(q"id0 = ${typeId}") ++ fieldDefs ++ Iterable(
                constructExpr
              )
            else
              Iterable(q"annotations0 = _root_.zio.Chunk.apply(..$typeAnnotations)") ++ Iterable(q"id0 = ${typeId}") ++ fieldDefs ++ Iterable(
                constructExpr
              )

          val typeArgsWithFields = fieldTypes.zip(fieldAnnotations).map {
            case (termSymbol, annotations) if annotations.nonEmpty =>
              tq"${getFieldName(annotations).getOrElse(termSymbol.name.toString.trim)}.type"
            case (termSymbol, _) =>
              tq"${termSymbol.name.toString.trim}.type"
          } ++ typeArgs

          fieldTypes.size match {
            case 0 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass0[..$typeArgsWithFields] = $schemaType(..$applyArgs); $selfRef}"
            case 1 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass1.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass1.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 2 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass2.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass2.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 3 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass3.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass3.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 4 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass4.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass4.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 5 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass5.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass5.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 6 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass6.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass6.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 7 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass7.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass7.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 8 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass8.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass8.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 9 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass9.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass9.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 10 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass10.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass10.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 11 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass11.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass11.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 12 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass12.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass12.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 13 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass13.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass13.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 14 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass14.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass14.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 15 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass15.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass15.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 16 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass16.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass16.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 17 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass17.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass17.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 18 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass18.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass18.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 19 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass19.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass19.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 20 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass20.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass20.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 21 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass21.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass21.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case 22 =>
              q"{lazy val $selfRef: _root_.zio.schema.Schema.CaseClass22.WithFields[..$typeArgsWithFields] = $schemaType(..$applyArgs).asInstanceOf[_root_.zio.schema.Schema.CaseClass22.WithFields[..$typeArgsWithFields]]; $selfRef}"
            case s =>
              c.abort(
                tpe.termSymbol.pos,
                s"Unhandled product arity $s for ${show(tpe)}, ${show(tpeCompanion)}. This should never happen. If you see this error message please report a bug to https://github.com/zio/zio-schema/issues"
              )
          }
        }
    }

    def deriveEnum(tpe: Type, stack: List[Frame[c.type]]): Tree = {
      val typeId = q"_root_.zio.schema.TypeId.parse(${tpe.toString()})"

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

      def knownSubclassesOf(parent: ClassSymbol): List[Type] = {

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

      val subtypes = knownSubclassesOf(tpe.typeSymbol.asClass)
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
        val constructFn   = q"(z: $subtype) => z.asInstanceOf[$tpe]"
        val isCaseFn      = q"(z: $tpe) => z.isInstanceOf[$subtype]"
        if (typeAnnotations.isEmpty)
          q"_root_.zio.schema.Schema.Case.apply[$tpe, $subtype]($caseLabel,$caseSchema,$deconstructFn, $constructFn, $isCaseFn)"
        else {
          val annotationArgs = q"zio.Chunk.apply(..$typeAnnotations)"
          q"_root_.zio.schema.Schema.Case.apply[$tpe, $subtype]($caseLabel,$caseSchema,$deconstructFn, $constructFn, $isCaseFn, $annotationArgs)"
        }
      }

      val applyArgs =
        if (typeAnnotations.isEmpty)
          Iterable(q"id = ${typeId}") ++ cases ++ Iterable(q"annotations = zio.Chunk.empty")
        else
          Iterable(q"id = ${typeId}") ++ cases ++ Iterable(q"annotations = zio.Chunk.apply(..$typeAnnotations)")

      cases.size match {
        case 0 => c.abort(c.enclosingPosition, s"No subtypes found for ${show(tpe)}")
        case 1 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum1[..$typeArgs] = _root_.zio.schema.Schema.Enum1[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 2 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum2[..$typeArgs] = _root_.zio.schema.Schema.Enum2[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 3 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum3[..$typeArgs] = _root_.zio.schema.Schema.Enum3[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 4 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum4[..$typeArgs] = _root_.zio.schema.Schema.Enum4[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 5 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum5[..$typeArgs] = _root_.zio.schema.Schema.Enum5[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 6 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum6[..$typeArgs] = _root_.zio.schema.Schema.Enum6[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 7 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum7[..$typeArgs] = _root_.zio.schema.Schema.Enum7[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 8 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum8[..$typeArgs] = _root_.zio.schema.Schema.Enum8[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 9 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum9[..$typeArgs] = _root_.zio.schema.Schema.Enum9[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 10 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum10[..$typeArgs] = _root_.zio.schema.Schema.Enum10[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 11 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum11[..$typeArgs] = _root_.zio.schema.Schema.Enum11[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 12 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum12[..$typeArgs] = _root_.zio.schema.Schema.Enum12[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 13 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum13[..$typeArgs] = _root_.zio.schema.Schema.Enum13[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 14 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum14[..$typeArgs] = _root_.zio.schema.Schema.Enum14[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 15 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum15[..$typeArgs] = _root_.zio.schema.Schema.Enum15[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 16 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum16[..$typeArgs] = _root_.zio.schema.Schema.Enum16[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 17 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum17[..$typeArgs] = _root_.zio.schema.Schema.Enum17[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 18 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum18[..$typeArgs] = _root_.zio.schema.Schema.Enum18[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 19 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum19[..$typeArgs] = _root_.zio.schema.Schema.Enum19[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 20 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum20[..$typeArgs] = _root_.zio.schema.Schema.Enum20[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 21 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum21[..$typeArgs] = _root_.zio.schema.Schema.Enum21[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case 22 =>
          q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Enum22[..$typeArgs] = _root_.zio.schema.Schema.Enum22[..$typeArgs](..$applyArgs); $selfRefIdent}"""
        case _ =>
          val caseSet = q"_root_.zio.schema.CaseSet.apply(..$cases).asInstanceOf[_root_.zio.schema.CaseSet.Aux[$tpe]]"

          if (typeAnnotations.isEmpty)
            q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.EnumN[$tpe,_root_.zio.schema.CaseSet.Aux[$tpe]] = _root_.zio.schema.Schema.EnumN.apply[$tpe,_root_.zio.schema.CaseSet.Aux[$tpe]]($typeId, $caseSet); $selfRefIdent}"""
          else {
            val annotationArg = q"zio.Chunk.apply(..$typeAnnotations)"

            q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.EnumN[$tpe,_root_.zio.schema.CaseSet.Aux[$tpe]] = _root_.zio.schema.Schema.EnumN.apply[$tpe,_root_.zio.schema.CaseSet.Aux[$tpe]]($typeId, $caseSet, $annotationArg); $selfRefIdent}"""
          }
      }
    }

    def deriveMap(tpe: Type): Tree = {
      val selfRefName  = c.freshName("ref")
      val selfRefIdent = Ident(TermName(selfRefName))

      val (keyType, valueType) = tpe match {
        case TypeRef(_, _, List(keyType, valueType)) => (keyType, valueType)
        case _                                       => c.abort(c.enclosingPosition, (s"Invalid type $tpe for @deriveSchema"))
      }

      val keySchema   = q"""_root_.zio.schema.Schema[$keyType]"""
      val valueSchema = q"""_root_.zio.schema.Schema[$valueType]"""

      q"""{lazy val $selfRefIdent: _root_.zio.schema.Schema.Map[$keyType, $valueType] = _root_.zio.schema.Schema.Map.apply[$keyType, $valueType]($keySchema, $valueSchema, zio.Chunk.empty); $selfRefIdent}"""
    }

    recurse(tpe, List.empty[Frame[c.type]])
  }

  case class Frame[C <: whitebox.Context](ctx: C, ref: String, tpe: C#Type)

}
