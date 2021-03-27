package zio.schema

import scala.language.experimental.macros
import magnolia._

object DeriveSchema {

  type Typeclass[A] = Schema[A]

  def combine[A](ctx: CaseClass[Schema, A]): Schema[A] =
    ctx.parameters.size match {
      case 1 => caseClass1(ctx)
      case 2 => caseClass2(ctx)
      case 3 => caseClass3(ctx)
      case _ =>
        Schema
          .record(
            ctx.parameters.map(p => p.label -> p.typeclass).toMap
          )
          .transformOrFail(
            { map =>
              ctx.parameters
                .map(p => map.get(p.label).orElse(p.default).toRight(p.label))
                .collect { case Left(fieldName) => fieldName }
                .toList match {
                case ::(head, next) => Left(s"""Missing field(s): ${(head :: next).mkString(", ")}.""")
                case Nil =>
                  Right(ctx.construct { p =>
                    map.get(p.label).map(_.asInstanceOf[p.PType]).orElse(p.default).get
                  })
              }
            }, { cc =>
              Right(ctx.parameters.map(p => p.label -> p.dereference(cc)).toMap)
            }
          )
    }

  private def caseClass1[Z](ctx: CaseClass[Typeclass,Z]): Typeclass[Z] = {
    val param = ctx.parameters.head
    Schema.CaseClass1[param.PType,Z](
      field = (param.label,param.typeclass),
      construct = (p: param.PType) => ctx.construct(_ => p)
    )
  }

  private def caseClass2[Z](ctx: CaseClass[Typeclass,Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    new Schema.CaseClass2[param1.PType,param2.PType,Z](
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      construct = (p1: param1.PType, p2: param2.PType) => ctx.rawConstruct(Seq(p1,p2))
    )
  }

  private def caseClass3[Z](ctx: CaseClass[Typeclass,Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    val param3 = ctx.parameters(2)
    new Schema.CaseClass3[param1.PType,param2.PType,param3.PType,Z](
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      field3 = param3.label -> param3.typeclass,
      construct = (p1: param1.PType, p2: param2.PType, p3: param3.PType) => ctx.rawConstruct(Seq(p1,p2,p3))
    )
  }

  def dispatch[A](sealedTrait: SealedTrait[Schema, A]): Schema[A] =
    Schema
      .enumeration(sealedTrait.subtypes.map(s => s.typeName.short -> s.typeclass).toMap)
      .transformOrFail(
        { map =>
          val maybeSubtype = sealedTrait.subtypes
            .find(st => map.headOption.exists(_._1 == st.typeName.short))
          Either.cond(
            maybeSubtype.nonEmpty, {
              val st = maybeSubtype.get
              map(st.typeName.short).asInstanceOf[st.SType]
            },
            s"""Expected one of following subtypes: ${sealedTrait.subtypes.map(_.typeName.short).mkString(", ")}"""
          )
        }, { a =>
          sealedTrait.dispatch(a) { subType =>
            Right(Map(subType.typeName.short -> subType.cast(a)))
          }
        }
      )

  implicit def gen[T]: Schema[T] = macro Magnolia.gen[T]
}
