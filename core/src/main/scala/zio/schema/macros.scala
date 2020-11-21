package zio.schema

import magnolia._

import scala.language.experimental.macros

object DeriveSchema {

  type Typeclass[A] = Schema[A]

  def combine[A](caseClass: CaseClass[Schema, A]): Schema[A] =
    caseClass.parameters.size match {
      case 1 => caseClassN1(caseClass)
      case 2 => caseClassN2(caseClass)
      case 3 => caseClassN3(caseClass)
      case _ =>
        Schema
          .record(
            caseClass.parameters.map(p => p.label -> p.typeclass).toMap
          )
          .transformOrFail(
            { map =>
              caseClass.parameters
                .map(p => map.get(p.label).orElse(p.default).toRight(p.label))
                .collect { case Left(fieldName) => fieldName }
                .toList match {
                case ::(head, next) => Left(s"""Missing field(s): ${(head :: next).mkString(", ")}.""")
                case Nil =>
                  Right(caseClass.construct { p =>
                    map.get(p.label).map(_.asInstanceOf[p.PType]).orElse(p.default).get
                  })
              }
            }, { cc =>
              Right(caseClass.parameters.map(p => p.label -> p.dereference(cc)).toMap)
            }
          )
    }

  private def caseClassN1[A](caseClass: CaseClass[Typeclass, A]): Typeclass[A] = {
    val param = caseClass.parameters.head

    Schema.caseClassN[param.PType, A](
      param.label -> param.typeclass
    )(
      p => caseClass.construct(_ => p),
      cc => Option(param.dereference(cc))
    )
  }

  private def caseClassN2[A](caseClass: CaseClass[Typeclass, A]): Typeclass[A] = {
    val param1 = caseClass.parameters.head
    val param2 = caseClass.parameters(1)

    Schema.caseClassN[param1.PType, param2.PType, A](
      param1.label -> param1.typeclass,
      param2.label -> param2.typeclass
    )(
      (p1, p2) =>
        caseClass.construct(_.index match {
          case 0 => p1
          case 1 => p2
        }),
      cc => Option((param1.dereference(cc), param2.dereference(cc)))
    )
  }

  private def caseClassN3[A](caseClass: CaseClass[Typeclass, A]): Typeclass[A] = {
    val param1 = caseClass.parameters.head
    val param2 = caseClass.parameters(1)
    val param3 = caseClass.parameters(2)

    Schema.caseClassN[param1.PType, param2.PType, param3.PType, A](
      param1.label -> param1.typeclass,
      param2.label -> param2.typeclass,
      param3.label -> param3.typeclass
    )(
      (p1, p2, p3) =>
        caseClass.construct(_.index match {
          case 0 => p1
          case 1 => p2
          case 2 => p3
        }),
      cc => Option((param1.dereference(cc), param2.dereference(cc), param3.dereference(cc)))
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