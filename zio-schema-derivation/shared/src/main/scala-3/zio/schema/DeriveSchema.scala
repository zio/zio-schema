package zio.schema

import scala.quoted._
import scala.deriving.Mirror
import scala.compiletime.{erasedValue, summonInline, constValueTuple}
import scala.collection.immutable.ListMap
import Schema._

import zio.schema.annotation.fieldName

extension (s: Schema.type) transparent inline def derived[A] = DeriveSchema.gen[A]

object DeriveSchema {

  transparent inline def gen[T]: Schema[T] = ${ deriveSchema[T] }

  private def deriveSchema[T: Type](using Quotes): Expr[Schema[T]] =
    DeriveSchema().deriveSchema[T](top = true)
}

private case class DeriveSchema()(using val ctx: Quotes) {
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

  def deriveSchema[T: Type](stack: Stack = Stack.empty, top: Boolean = false)(using Quotes): Expr[Schema[T]] = {
    depth += 1
    if (depth > 1024)
      throw new Exception("Schema derivation exceeded")

    val typeRepr = TypeRepr.of[T]
    val result = stack.find(typeRepr) match {
      case Some(ref) =>
        '{ Schema.defer(${ref.asExprOf[Schema[T]]}) }
      case None =>
        val summoned = if (!top) Expr.summon[Schema[T]] else None
        if (!top && summoned.isDefined) {
          '{
            Schema.defer(${
              summoned.get
            })
          }.asExprOf[Schema[T]]
        } else {
        typeRepr.asType match {
          case '[List[a]] =>
            val schema = deriveSchema[a](stack)
            '{ Schema.list(Schema.defer(${schema})) }.asExprOf[Schema[T]]
          case '[scala.util.Either[a, b]] =>
            val schemaA = deriveSchema[a](stack)
            val schemaB = deriveSchema[b](stack)
            '{ Schema.either(Schema.defer(${schemaA}), Schema.defer(${schemaB})) }.asExprOf[Schema[T]]
          case '[Option[a]] =>
            val schema = deriveSchema[a](stack)
            // throw new Error(s"OPITOS ${schema.show}")
            '{ Schema.option(Schema.defer($schema)) }.asExprOf[Schema[T]]
          case '[scala.collection.Set[a]] =>
            val schema = deriveSchema[a](stack)
            '{ Schema.set(Schema.defer(${schema})) }.asExprOf[Schema[T]]
          case '[Vector[a]] =>
            val schema = deriveSchema[a](stack)
            '{ Schema.vector(Schema.defer(${schema})) }.asExprOf[Schema[T]]
          case '[scala.collection.Map[a, b]] =>
            val schemaA = deriveSchema[a](stack)
            val schemaB = deriveSchema[b](stack)
            '{ Schema.map(Schema.defer(${schemaA}), Schema.defer(${schemaB})) }.asExprOf[Schema[T]]
          case '[zio.Chunk[a]] =>
            val schema = deriveSchema[a](stack)
            '{ Schema.chunk(Schema.defer(${schema})) }.asExprOf[Schema[T]]
          case _ =>
                Mirror(typeRepr) match {
                  case Some(mirror) =>
                    mirror.mirrorType match {
                      case MirrorType.Sum =>
                        deriveEnum[T](mirror, stack)
                      case MirrorType.Product =>
                       deriveCaseClass[T](mirror, stack, top)
                    }
                  case None =>
                    val sym = typeRepr.typeSymbol
                    if (sym.isClassDef && sym.flags.is(Flags.Module)) {
                      deriveCaseObject[T](stack, top)
                    }
                    else {
                      report.errorAndAbort(s"Deriving schema for ${typeRepr.show} is not supported")
                    }
              }
          }
        }
    }

    //println()
    //println()
    //println(s"RESULT ${typeRepr.show}")
    //println(s"------")
    //println(s"RESULT ${result.show}")

    result
  }

  def deriveCaseObject[T: Type](stack: Stack, top: Boolean)(using Quotes) = {
    val selfRefSymbol = Symbol.newVal(Symbol.spliceOwner, s"derivedSchema${stack.size}", TypeRepr.of[Schema[T]], Flags.Lazy, Symbol.noSymbol)
    val selfRef = Ref(selfRefSymbol)

    val typeInfo = '{TypeId.parse(${Expr(TypeRepr.of[T].show)})}
    val isEnumCase = Type.of[T] match {
      case '[reflect.Enum] => true
      case _ => false
    }
    val docAnnotationExpr = if (isEnumCase)
      then TypeRepr.of[T].typeSymbol.children.map(_.docstring.toList).flatten.map { docstring =>
        val docstringExpr = Expr(docstring)
        '{zio.schema.annotation.description(${docstringExpr})}
      }
      else TypeRepr.of[T].typeSymbol.docstring.map { docstring =>
        val docstringExpr = Expr(docstring)
        '{zio.schema.annotation.description(${docstringExpr})}
      }.toList
    val annotationExprs = if isEnumCase
      then TypeRepr.of[T].typeSymbol.children.map(_.annotations).flatten.filter (filterAnnotation).map (_.asExpr)
      else TypeRepr.of[T].typeSymbol.annotations.filter (filterAnnotation).map (_.asExpr)
    val annotations = '{zio.Chunk.fromIterable (${Expr.ofSeq (annotationExprs)}) ++ zio.Chunk.fromIterable(${Expr.ofSeq(docAnnotationExpr)}) }

    val constructor = '{() => ${Ref(TypeRepr.of[T].typeSymbol.companionModule).asExprOf[T]}}
    val ctor = typeRprOf[T](0).typeSymbol.companionModule
    val args = List(typeInfo, constructor, annotations)

    val applied = Apply(
      TypeApply(
        Select.unique(Ref(ctor), "apply"),
        List(TypeRepr.of[T].asType match {
          case '[tt] => TypeTree.of[tt]
        })),
      args.map(_.asTerm)
    )

    val lazyValDef = ValDef(selfRefSymbol, Some(applied.changeOwner(selfRefSymbol)))

    applied.asExpr match {
      case '{ type tt <: Schema[T]; $ex : `tt` } =>
        '{
          ${Block(
            List(lazyValDef),
            selfRef
          ).asExpr}.asInstanceOf[tt]
        }
    }
  }

  def deriveCaseClass[T: Type](mirror: Mirror, stack: Stack, top: Boolean)(using Quotes) = {
    val selfRefSymbol = Symbol.newVal(Symbol.spliceOwner, s"derivedSchema${stack.size}", TypeRepr.of[Schema[T]], Flags.Lazy, Symbol.noSymbol)
    val selfRef = Ref(selfRefSymbol)
    val newStack = stack.push(selfRef, TypeRepr.of[T])

    val labels = mirror.labels.toList
    val types = mirror.types.toList
    val typesAndLabels = types.zip(labels)

    val paramAnns = fromConstructor(TypeRepr.of[T].typeSymbol)
    val constructor = caseClassConstructor[T](mirror).asExpr

    val isEnumCase = Type.of[T] match {
      case '[reflect.Enum] => true
      case _ => false
    }
    val docAnnotationExpr = if (isEnumCase)
      then TypeRepr.of[T].typeSymbol.children.map(_.docstring.toList).flatten.map { docstring =>
        val docstringExpr = Expr(docstring)
        '{zio.schema.annotation.description(${docstringExpr})}
      }
      else TypeRepr.of[T].typeSymbol.docstring.map { docstring =>
        val docstringExpr = Expr(docstring)
        '{zio.schema.annotation.description(${docstringExpr})}
      }.toList
    val annotationExprs = if isEnumCase
      then TypeRepr.of[T].typeSymbol.children.map(_.annotations).flatten.filter (filterAnnotation).map (_.asExpr)
      else TypeRepr.of[T].typeSymbol.annotations.filter (filterAnnotation).map (_.asExpr)

    val annotations = '{ zio.Chunk.fromIterable(${Expr.ofSeq(annotationExprs)}) ++ zio.Chunk.fromIterable(${Expr.ofSeq(docAnnotationExpr)}) }
    val typeInfo = '{TypeId.parse(${Expr(TypeRepr.of[T].show)})}

    val applied = if (labels.length <= 22) {

      val typeArgs =
        (types.appended(TypeRepr.of[T])).map { tpe =>
            tpe.asType match
              case '[tt] => TypeTree.of[tt]
        }

      val ctor = typeRprOf[T](labels.length).typeSymbol.companionModule
      val typeAppliedCtor = TypeApply(
            Select.unique(Ref(ctor), "apply"),
            typeArgs
          )

      val fieldsAndFieldTypes = typesAndLabels.map { case (tpe, label) => deriveField[T](tpe, label, paramAnns.getOrElse(label, List.empty), newStack) }
      val (fields, fieldTypes) = fieldsAndFieldTypes.unzip
      val args = List(typeInfo) ++ fields ++ Seq(constructor) ++  Seq(annotations)
      val terms = Expr.ofTupleFromSeq(args)

      if (labels.length > 0) {
        val caseClassWithFieldsTpe = caseClassWithFieldsType(labels.length)
        val appliedCaseClassWithFieldsTpe = caseClassWithFieldsTpe.appliedTo(
          fieldTypes ++ types ++ List(TypeRepr.of[T])
        )

        Select.unique(Apply(
          typeAppliedCtor,
          args.map(_.asTerm)
        ), "asInstanceOf").appliedToType(appliedCaseClassWithFieldsTpe)
      } else {
        Apply(
          typeAppliedCtor,
          args.map(_.asTerm)
        )
      }
    } else {
       val fieldsAndFieldTypes = typesAndLabels.map { case (tpe, label) => deriveGenericField[T](tpe, label, paramAnns.getOrElse(label, List.empty), newStack) }
       val fields = fieldsAndFieldTypes.map(_._1)
       val genericRecord = '{GenericRecord($typeInfo, FieldSet.fromFields(${Varargs(fields)} : _*), $annotations)}

       val s = TypeRepr.of[T].typeSymbol.declaredFields

       def casts(m: Expr[ListMap[String, _]])(using Quotes) =
         typesAndLabels.map { case (tpe, label) =>
           val interestingField = s.find (_.name == label)
           val fieldType = interestingField match {
             case Some(interestingField) =>
               val ct = tpe.memberType (interestingField)
               ct.asType
             case None =>
               tpe.asType
           }
           fieldType match { case '[t] =>
            '{ try ${m}.apply(${Expr(label)}).asInstanceOf[t]
               catch {
                 case _: ClassCastException => throw new RuntimeException("Field " + ${Expr(label)} + " has invalid type")
                 case _: Throwable => throw new RuntimeException("Field " + ${Expr(label)} + " is missing")
               }
            }.asTerm
           }
         }

       def appliedConstructor(m: Expr[ListMap[String, _]])(using Quotes) = {
         Apply(Select.unique(Ref(TypeRepr.of[T].typeSymbol.companionModule), "apply"), casts(m)).asExprOf[T]
       }

       val fromMap = '{ (m: ListMap[String, _]) =>
         try { Right(${appliedConstructor('m)}) } catch {
         case e: Throwable  => Left(e.getMessage)
       }}

       def tuples(b: Expr[T])(using Quotes) =
         typesAndLabels.map { case (tpe, label) =>
           val interestingField = s.find (_.name == label)
           val fieldType = interestingField match {
             case Some(interestingField) =>
                val ct = tpe.memberType (interestingField)
                ct.asType
              case None =>
                tpe.asType
            }
           fieldType match { case '[t] =>
             '{(${Expr(label)}, ${Select.unique(b.asTerm, label).asExprOf[t]})}
           }
         }
       val toMap = '{(b: T) => Right(ListMap.apply(${Varargs(tuples('b))} :_*)) }

       '{${genericRecord.asExprOf[GenericRecord]}.transformOrFail[T]($fromMap, $toMap)}.asTerm
    }

    val lazyValDef = ValDef(selfRefSymbol, Some(applied.changeOwner(selfRefSymbol)))

    applied.asExpr match {
      case '{ type tt <: Schema[T]; $ex : `tt` } =>
        '{
          ${Block(
            List(lazyValDef),
            selfRef
          ).asExpr}.asInstanceOf[tt]
        }
    }
  }


  private def fromDeclarations(from: Symbol): List[(String, List[Expr[Any]])] =
    from.declaredFields.map {
      field =>
        field.name -> field.annotations.filter(filterAnnotation).map(_.asExpr)
    }

  private def defaultValues(from: Symbol): Predef.Map[String, Expr[Any]] =
    val params = from.primaryConstructor.paramSymss.flatten.filter(!_.isTypeParam)
    val result = (1 to params.size).toList.map(
      i =>
          from
            .companionClass
            .declaredMethod(s"$$lessinit$$greater$$default$$$i")
            .headOption
            .orElse(
              from
                .companionClass
                .declaredMethod(s"$$apply$$default$$$i")
                .headOption
            )
            .map { s =>
              val select = Select(Ref(from.companionModule), s)
              if (select.isExpr) select.asExpr
              else select.appliedToType(TypeRepr.of[Any]).asExpr
            }
      ).zip(params.map(_.name))
    result.collect{ case (Some(expr), name) => name -> expr }.toMap

  private def fromConstructor(from: Symbol): scala.collection.Map[String, List[Expr[Any]]] = {
      val defaults = defaultValues(from)
      from.primaryConstructor.paramSymss.flatten.map { field =>
        field.name -> {
          val annos = field.annotations
            .filter(filterAnnotation)
            .map(_.asExpr.asInstanceOf[Expr[Any]])
          val hasDefaultAnnotation =
            field.annotations.exists(_.tpe <:< TypeRepr.of[zio.schema.annotation.fieldDefaultValue[_]])
          if (hasDefaultAnnotation || defaults.get(field.name).isEmpty) {
            annos
          } else {
            annos :+ '{zio.schema.annotation.fieldDefaultValue(${defaults(field.name)})}.asExprOf[Any]
          }
        }
      }.toMap
  }

  def deriveEnum[T: Type](mirror: Mirror, stack: Stack)(using Quotes) = {
    val selfRefSymbol = Symbol.newVal(Symbol.spliceOwner, s"derivedSchema${stack.size}", TypeRepr.of[Schema[T]], Flags.Lazy, Symbol.noSymbol)
    val selfRef = Ref(selfRefSymbol)
    val newStack = stack.push(selfRef, TypeRepr.of[T])

    val labels = mirror.labels.toList
    val types = mirror.types.toList
    val typesAndLabels = types.zip(labels)

    val cases = typesAndLabels.map { case (tpe, label) => deriveCase[T](tpe, label, newStack) }

    val numParentFields: Int = TypeRepr.of[T].typeSymbol.declaredFields.length
    val childrenFields = TypeRepr.of[T].typeSymbol.children.map(_.declaredFields.length)
    val childrenFieldsConstructor = TypeRepr.of[T].typeSymbol.children.map(_.caseFields.length)
    val isSimpleEnum: Boolean = childrenFieldsConstructor.forall( _ == 0) && childrenFields.forall( _ <= numParentFields)
    val hasSimpleEnumAnn: Boolean = TypeRepr.of[T].typeSymbol.hasAnnotation(TypeRepr.of[_root_.zio.schema.annotation.simpleEnum].typeSymbol)

    val docAnnotationExpr = TypeRepr.of[T].typeSymbol.docstring.map { docstring =>
      val docstringExpr = Expr(docstring)
      '{zio.schema.annotation.description(${docstringExpr})}
    }
    val annotationExprs = (isSimpleEnum, hasSimpleEnumAnn) match {
      case (true, false) => TypeRepr.of[T].typeSymbol.annotations.filter(filterAnnotation).map(_.asExpr).+:('{zio.schema.annotation.simpleEnum(true)})
      case (false, true) => throw new Exception(s"${TypeRepr.of[T].typeSymbol.name} must be a simple Enum")
      case _             => TypeRepr.of[T].typeSymbol.annotations.filter(filterAnnotation).map(_.asExpr)
    }
    val annotations = '{ zio.Chunk.fromIterable(${Expr.ofSeq(annotationExprs)}) ++ zio.Chunk.fromIterable(${Expr.ofSeq(docAnnotationExpr.toList)}) }

    val typeInfo = '{TypeId.parse(${Expr(TypeRepr.of[T].show)})}

    val applied = if (cases.length <= 22) {
      val args = List(typeInfo) ++ cases :+ annotations
      val terms = Expr.ofTupleFromSeq(args)
      val ctor = TypeRepr.of[Enum2[_, _, _]].typeSymbol.primaryConstructor

      val typeArgs =
        (types.appended(TypeRepr.of[T])).map { tpe =>
          tpe.asType match
            case '[tt] => TypeTree.of[tt]
        }

      val typeTree = enumTypeTree[T](labels.length)

      Apply(
        TypeApply(
          Select(New(typeTree), ctor),
          typeArgs
        ),
        args.map(_.asTerm)
      )
    } else {
      '{EnumN($typeInfo, CaseSet(${Varargs(cases)}: _*).asInstanceOf[CaseSet.Aux[T]], $annotations) }.asTerm
    }

    val lazyValDef = ValDef(selfRefSymbol, Some(applied.changeOwner(selfRefSymbol)))

    applied.asExpr match {
      case '{ type tt <: Schema[T]; $ex : `tt` } =>
        '{
          ${Block(
            List(lazyValDef),
            selfRef
          ).asExpr}.asInstanceOf[tt]
        }
    }
  }


  // Derive Field for a CaseClass
  def deriveField[T: Type](repr: TypeRepr, name: String, anns: List[Expr[Any]], stack: Stack)(using Quotes) = {
    import zio.schema.validation.Validation
    import zio.schema.annotation.validate

    val tpe = TypeRepr.of[T]
    val s = tpe.typeSymbol.declaredFields
    val interestingField = s.find (_.name == name)

    val fieldType = interestingField match {
      case Some(interestingField) =>
        val ct = tpe.memberType (interestingField)
        ct.asType
      case None =>
        repr.asType
    }
    fieldType match { case '[t] =>
      val schema = deriveSchema[t](stack)
      val validations = anns.collect {
        case ann if ann.isExprOf[validate[t]] => ann.asExprOf[validate[t]]
      }
      val validator: Expr[Validation[t]] = validations.foldLeft[Expr[Validation[t]]]('{Validation.succeed}){
        case (acc, expr) => '{
          $acc && ${expr}.validation
        }
      }

      val typeParams = TypeRepr.of[T].dealias match
           case AppliedType (t, params) => params
           case _ => Nil

      val get = '{ (t: T) => ${Select.unique('t.asTerm, name).asExprOf[t]} }
      val set = '{ (ts: T, v: t) => ${Select.overloaded('ts.asTerm, "copy", typeParams, List(NamedArg(name, 'v.asTerm))).asExprOf[T]} }
      val chunk = '{ zio.Chunk.fromIterable(${ Expr.ofSeq(anns.reverse) }) }

      if (anns.nonEmpty) {
        val (newName, newNameValue) = anns.collectFirst {
          case ann if ann.isExprOf[fieldName] =>
            val fieldNameAnn = ann.asExprOf[fieldName]
            ('{${fieldNameAnn}.name}, extractFieldNameValue(fieldNameAnn))
        }.getOrElse((Expr(name), name))

        val f = '{ Field($newName, $schema, $chunk, $validator, $get, $set)}
        addFieldName(newNameValue)(f) // TODO: we need to pass the evaluated annotation value instead of name
      } else {
        val f = '{ Field(${Expr(name)}, $schema, $chunk, $validator, $get, $set) }
        addFieldName(name)(f)
      }
    }
  }

// Derive Field for a GenericRecord
  def deriveGenericField[T: Type](repr: TypeRepr, name: String, anns: List[Expr[Any]], stack: Stack)(using Quotes) = {
    import zio.schema.validation.Validation
    import zio.schema.annotation.validate

    val tpe = TypeRepr.of[T]
    val s = tpe.typeSymbol.declaredFields
    val interestingField = s.find (_.name == name)

    val fieldType = interestingField match {
      case Some(interestingField) =>
        val ct = tpe.memberType (interestingField)
        ct.asType
      case None =>
        repr.asType
    }
    fieldType match { case '[t] =>
      val schema = deriveSchema[t](stack)
      val validations = anns.collect {
        case ann if ann.isExprOf[validate[t]] => ann.asExprOf[validate[t]]
      }
      val validator: Expr[Validation[t]] = validations.foldLeft[Expr[Validation[t]]]('{Validation.succeed}){
        case (acc, expr) => '{
          $acc && ${expr}.validation
        }
      }

      val typeParams = TypeRepr.of[T].dealias match
           case AppliedType (t, params) => params
           case _ => Nil

      val get = '{ (t: ListMap[String, _]) => t.apply(${Expr(name)}).asInstanceOf[t] }
      val set = '{ (ts: ListMap[String, _], v: t) => ts.updated(${Expr(name)}, v) }
      val chunk = '{ zio.Chunk.fromIterable(${ Expr.ofSeq(anns.reverse) }) }

      if (anns.nonEmpty) {
        val newName = anns.collectFirst {
          case ann if ann.isExprOf[fieldName] => '{${ann.asExprOf[fieldName]}.name}
        }.getOrElse(Expr(name))

        val f = '{ Field($newName, $schema, $chunk, $validator, $get, $set) }
        addFieldName(name)(f) // TODO: we need to pass the evaluated annotation value instead of name
      } else {
        val f = '{ Field(${Expr(name)}, $schema, $chunk, $validator, $get, $set) }
        addFieldName(name)(f)
      }
    }
  }

  def addFieldName[R: Type, T: Type, F <: Field[R, T]: Type](name: String)(f: Expr[F])(using Quotes) = {
    val withFieldName = TypeRepr.of[Field.WithFieldName]
    val r = TypeRepr.of[R]
    val t = TypeRepr.of[T]
    val nameT = ConstantType(StringConstant(name))
    val fieldWithName = withFieldName.appliedTo(List(r, nameT, t))
    (Select.unique(f.asTerm, "asInstanceOf").appliedToType(fieldWithName).asExprOf[F], nameT)
  }


  // sealed case class Case[A, Z](id: String, codec: Schema[A], unsafeDeconstruct: Z => A, annotations: Chunk[Any] = Chunk.empty) {
  def deriveCase[T: Type](repr: TypeRepr, label: String, stack: Stack)(using Quotes) = {
    repr.asType match { case '[t] =>
      val schema = deriveSchema[t](stack)
      val stringExpr = Expr(label)

      val isEnumCase = Type.of[T] match {
        case '[reflect.Enum] => true
        case _ => false
      }
      val docAnnotationExpr = if (isEnumCase)
        then TypeRepr.of[t].typeSymbol.children.map(_.docstring.toList).flatten.map { docstring =>
          val docstringExpr = Expr(docstring)
          '{zio.schema.annotation.description(${docstringExpr})}
        }
        else TypeRepr.of[t].typeSymbol.docstring.map { docstring =>
          val docstringExpr = Expr(docstring)
          '{zio.schema.annotation.description(${docstringExpr})}
        }.toList
      val annotationExprs = if isEnumCase
        then TypeRepr.of[t].typeSymbol.children.map(_.annotations).flatten.filter (filterAnnotation).map (_.asExpr)
        else TypeRepr.of[t].typeSymbol.annotations.filter (filterAnnotation).map (_.asExpr)
      val annotations = '{ zio.Chunk.fromIterable(${Expr.ofSeq(annotationExprs)}) ++ zio.Chunk.fromIterable(${Expr.ofSeq(docAnnotationExpr)}) }
      val unsafeDeconstruct = '{
        (z: T) => z.asInstanceOf[t]
      }
      val construct = '{
        (a: t) => a.asInstanceOf[T]
      }

      val isCase = '{ (z: T) => z.isInstanceOf[t @unchecked] }

      '{ Case(${Expr(label)}, $schema, $unsafeDeconstruct, $construct, $isCase, $annotations) }
    }
  }


  def caseClassConstructor[T: Type](mirror: Mirror) = {
    val product = Expr.summon[scala.deriving.Mirror.ProductOf[T]].get
    val methodType = MethodType(mirror.labels.toList)(_ => mirror.types.toList, _ => TypeRepr.of[T])
    Lambda(Symbol.spliceOwner, methodType, { (sym, reprs) =>
      val tupled = Expr.ofTupleFromSeq(reprs.map(_.asExpr))
      Select.overloaded(product.asTerm, "fromProduct", List.empty, List(tupled.asTerm))
    })
  }

  private def filterAnnotation(a: Term): Boolean =
    a.tpe.typeSymbol.maybeOwner.isNoSymbol ||
      a.tpe.typeSymbol.owner.fullName != "scala.annotation.internal"

  def extractFieldNameValue(attribute: Expr[fieldName]): String =
    attribute.asTerm match {
      // Apply(Select(New(Ident(fieldName)),<init>),List(Literal(Constant(renamed))))
      case Apply(_, List(Literal(StringConstant(name)))) =>
        name
    }

  def caseClassTypeTree[T: Type](arity: Int): TypeTree =
    arity match {
      case 0 => TypeTree.of[CaseClass0[T]]
      case 1 => TypeTree.of[CaseClass1[_, T]]
      case 2 => TypeTree.of[CaseClass2[_, _, T]]
      case 3 => TypeTree.of[CaseClass3[_, _, _, T]]
      case 4 => TypeTree.of[CaseClass4[_, _, _, _, T]]
      case 5 => TypeTree.of[CaseClass5[_, _, _, _, _, T]]
      case 6 => TypeTree.of[CaseClass6[_, _, _, _, _, _, T]]
      case 7 => TypeTree.of[CaseClass7[_, _, _, _, _, _, _, T]]
      case 8 => TypeTree.of[CaseClass8[_, _, _, _, _, _, _, _, T]]
      case 9 => TypeTree.of[CaseClass9[_, _, _, _, _, _, _, _, _, T]]
      case 10 => TypeTree.of[CaseClass10[_, _, _, _, _, _, _, _, _, _, T]]
      case 11 => TypeTree.of[CaseClass11[_, _, _, _, _, _, _, _, _, _, _, T]]
      case 12 => TypeTree.of[CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 13 => TypeTree.of[CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 14 => TypeTree.of[CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 15 => TypeTree.of[CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 16 => TypeTree.of[CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 17 => TypeTree.of[CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 18 => TypeTree.of[CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 19 => TypeTree.of[CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 20 => TypeTree.of[CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 21 => TypeTree.of[CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 22 => TypeTree.of[CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
    }

  def typeRprOf[T: Type](arity: Int): TypeRepr =
    arity match {
      case 0 => TypeRepr.of[CaseClass0[T]]
      case 1 => TypeRepr.of[CaseClass1[_, T]]
      case 2 => TypeRepr.of[CaseClass2[_, _, T]]
      case 3 => TypeRepr.of[CaseClass3[_, _, _, T]]
      case 4 => TypeRepr.of[CaseClass4[_, _, _, _, T]]
      case 5 => TypeRepr.of[CaseClass5[_, _, _, _, _, T]]
      case 6 => TypeRepr.of[CaseClass6[_, _, _, _, _, _, T]]
      case 7 => TypeRepr.of[CaseClass7[_, _, _, _, _, _, _, T]]
      case 8 => TypeRepr.of[CaseClass8[_, _, _, _, _, _, _, _, T]]
      case 9 => TypeRepr.of[CaseClass9[_, _, _, _, _, _, _, _, _, T]]
      case 10 => TypeRepr.of[CaseClass10[_, _, _, _, _, _, _, _, _, _, T]]
      case 11 => TypeRepr.of[CaseClass11[_, _, _, _, _, _, _, _, _, _, _, T]]
      case 12 => TypeRepr.of[CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 13 => TypeRepr.of[CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 14 => TypeRepr.of[CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 15 => TypeRepr.of[CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 16 => TypeRepr.of[CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 17 => TypeRepr.of[CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 18 => TypeRepr.of[CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 19 => TypeRepr.of[CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 20 => TypeRepr.of[CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 21 => TypeRepr.of[CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 22 => TypeRepr.of[CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
    }

  def caseClassWithFieldsType(arity: Int): TypeRepr =
    arity match {
      case 1 => TypeRepr.of[CaseClass1.WithFields]
      case 2 => TypeRepr.of[CaseClass2.WithFields]
      case 3 => TypeRepr.of[CaseClass3.WithFields]
      case 4 => TypeRepr.of[CaseClass4.WithFields]
      case 5 => TypeRepr.of[CaseClass5.WithFields]
      case 6 => TypeRepr.of[CaseClass6.WithFields]
      case 7 => TypeRepr.of[CaseClass7.WithFields]
      case 8 => TypeRepr.of[CaseClass8.WithFields]
      case 9 => TypeRepr.of[CaseClass9.WithFields]
      case 10 => TypeRepr.of[CaseClass10.WithFields]
      case 11 => TypeRepr.of[CaseClass11.WithFields]
      case 12 => TypeRepr.of[CaseClass12.WithFields]
      case 13 => TypeRepr.of[CaseClass13.WithFields]
      case 14 => TypeRepr.of[CaseClass14.WithFields]
      case 15 => TypeRepr.of[CaseClass15.WithFields]
      case 16 => TypeRepr.of[CaseClass16.WithFields]
      case 17 => TypeRepr.of[CaseClass17.WithFields]
      case 18 => TypeRepr.of[CaseClass18.WithFields]
      case 19 => TypeRepr.of[CaseClass19.WithFields]
      case 20 => TypeRepr.of[CaseClass20.WithFields]
      case 21 => TypeRepr.of[CaseClass21.WithFields]
      case 22 => TypeRepr.of[CaseClass22.WithFields]
    }

  def enumTypeTree[T: Type](arity: Int): TypeTree =
    arity match {
      case 0 => TypeTree.of[CaseClass0[T]]
      case 1 => TypeTree.of[Enum1[_, T]]
      case 2 => TypeTree.of[Enum2[_, _, T]]
      case 3 => TypeTree.of[Enum3[_, _, _, T]]
      case 4 => TypeTree.of[Enum4[_, _, _, _, T]]
      case 5 => TypeTree.of[Enum5[_, _, _, _, _, T]]
      case 6 => TypeTree.of[Enum6[_, _, _, _, _, _, T]]
      case 7 => TypeTree.of[Enum7[_, _, _, _, _, _, _, T]]
      case 8 => TypeTree.of[Enum8[_, _, _, _, _, _, _, _, T]]
      case 9 => TypeTree.of[Enum9[_, _, _, _, _, _, _, _, _, T]]
      case 10 => TypeTree.of[Enum10[_, _, _, _, _, _, _, _, _, _, T]]
      case 11 => TypeTree.of[Enum11[_, _, _, _, _, _, _, _, _, _, _, T]]
      case 12 => TypeTree.of[Enum12[_, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 13 => TypeTree.of[Enum13[_, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 14 => TypeTree.of[Enum14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 15 => TypeTree.of[Enum15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 16 => TypeTree.of[Enum16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 17 => TypeTree.of[Enum17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 18 => TypeTree.of[Enum18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 19 => TypeTree.of[Enum19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 20 => TypeTree.of[Enum20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 21 => TypeTree.of[Enum21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
      case 22 => TypeTree.of[Enum22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, T]]
  }

}

