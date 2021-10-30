package zio.schema

import magnolia._
import zio.Chunk

import scala.collection.immutable.ListMap
import scala.language.experimental.macros

object DeriveSchema {

  type Typeclass[A] = Schema[A]

  def combine[A](ctx: CaseClass[Schema, A]): Schema[A] =
    ctx.parameters.size match {
      case 0  => Schema.singleton(ctx.rawConstruct(Seq.empty))
      case 1  => caseClass1(ctx)
      case 2  => caseClass2(ctx)
      case 3  => caseClass3(ctx)
      case 4  => caseClass4(ctx)
      case 5  => caseClass5(ctx)
      case 6  => caseClass6(ctx)
      case 7  => caseClass7(ctx)
      case 8  => caseClass8(ctx)
      case 9  => caseClass9(ctx)
      case 10 => caseClass10(ctx)
      case 11 => caseClass11(ctx)
      case 12 => caseClass12(ctx)
      case 13 => caseClass13(ctx)
      case 14 => caseClass14(ctx)
      case 15 => caseClass15(ctx)
      case 16 => caseClass16(ctx)
      case 17 => caseClass17(ctx)
      case 18 => caseClass18(ctx)
      case 19 => caseClass19(ctx)
      case 20 => caseClass20(ctx)
      case 21 => caseClass21(ctx)
      case 22 => caseClass22(ctx)
      case _ =>
        Schema
          .record(
            Chunk.fromIterable(
              ctx.parameters.map(p => Schema.Field(p.label, p.typeclass, Chunk.fromIterable(p.annotations)))
            ): _*
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
              Right(ListMap.empty ++ ctx.parameters.map(p => p.label -> p.dereference(cc)))
            }
          )
    }

  private def caseClass1[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param = ctx.parameters.head
    new Schema.CaseClass1[param.PType, Z](
      annotations = Chunk.fromIterable(ctx.annotations),
      field = Schema.Field(param.label, Schema.defer(param.typeclass), Chunk.fromIterable(param.annotations)),
      construct = (p: param.PType) => ctx.construct(_ => p),
      extractField = (z: Z) => param.dereference(z)
    )
  }

  private def caseClass2[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    new Schema.CaseClass2[param1.PType, param2.PType, Z](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      construct = (p1: param1.PType, p2: param2.PType) => ctx.rawConstruct(Seq(p1, p2)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z)
    )
  }

  private def caseClass3[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    val param3 = ctx.parameters(2)
    new Schema.CaseClass3[param1.PType, param2.PType, param3.PType, Z](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      construct = (p1: param1.PType, p2: param2.PType, p3: param3.PType) => ctx.rawConstruct(Seq(p1, p2, p3)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z)
    )
  }

  private def caseClass4[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    val param3 = ctx.parameters(2)
    val param4 = ctx.parameters(3)
    new Schema.CaseClass4[param1.PType, param2.PType, param3.PType, param4.PType, Z](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      construct = (p1: param1.PType, p2: param2.PType, p3: param3.PType, p4: param4.PType) =>
        ctx.rawConstruct(Seq(p1, p2, p3, p4)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z)
    )
  }

  private def caseClass5[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    val param3 = ctx.parameters(2)
    val param4 = ctx.parameters(3)
    val param5 = ctx.parameters(4)
    new Schema.CaseClass5[param1.PType, param2.PType, param3.PType, param4.PType, param5.PType, Z](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      construct = (p1: param1.PType, p2: param2.PType, p3: param3.PType, p4: param4.PType, p5: param5.PType) =>
        ctx.rawConstruct(Seq(p1, p2, p3, p4, p5)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z)
    )
  }

  private def caseClass6[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    val param3 = ctx.parameters(2)
    val param4 = ctx.parameters(3)
    val param5 = ctx.parameters(4)
    val param6 = ctx.parameters(5)
    new Schema.CaseClass6[param1.PType, param2.PType, param3.PType, param4.PType, param5.PType, param6.PType, Z](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      construct =
        (p1: param1.PType, p2: param2.PType, p3: param3.PType, p4: param4.PType, p5: param5.PType, p6: param6.PType) =>
          ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z)
    )
  }

  private def caseClass7[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    val param3 = ctx.parameters(2)
    val param4 = ctx.parameters(3)
    val param5 = ctx.parameters(4)
    val param6 = ctx.parameters(5)
    val param7 = ctx.parameters(6)
    new Schema.CaseClass7[
      param1.PType,
      param2.PType,
      param3.PType,
      param4.PType,
      param5.PType,
      param6.PType,
      param7.PType,
      Z
    ](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label, Schema.defer(param7.typeclass), Chunk.fromIterable(param7.annotations)),
      construct = (
        p1: param1.PType,
        p2: param2.PType,
        p3: param3.PType,
        p4: param4.PType,
        p5: param5.PType,
        p6: param6.PType,
        p7: param7.PType
      ) => ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6, p7)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z)
    )
  }

  private def caseClass8[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    val param3 = ctx.parameters(2)
    val param4 = ctx.parameters(3)
    val param5 = ctx.parameters(4)
    val param6 = ctx.parameters(5)
    val param7 = ctx.parameters(6)
    val param8 = ctx.parameters(7)
    new Schema.CaseClass8[
      param1.PType,
      param2.PType,
      param3.PType,
      param4.PType,
      param5.PType,
      param6.PType,
      param7.PType,
      param8.PType,
      Z
    ](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label, Schema.defer(param7.typeclass), Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label, Schema.defer(param8.typeclass), Chunk.fromIterable(param8.annotations)),
      construct = (
        p1: param1.PType,
        p2: param2.PType,
        p3: param3.PType,
        p4: param4.PType,
        p5: param5.PType,
        p6: param6.PType,
        p7: param7.PType,
        p8: param8.PType
      ) => ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6, p7, p8)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z)
    )
  }

  private def caseClass9[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    val param3 = ctx.parameters(2)
    val param4 = ctx.parameters(3)
    val param5 = ctx.parameters(4)
    val param6 = ctx.parameters(5)
    val param7 = ctx.parameters(6)
    val param8 = ctx.parameters(7)
    val param9 = ctx.parameters(8)
    new Schema.CaseClass9[
      param1.PType,
      param2.PType,
      param3.PType,
      param4.PType,
      param5.PType,
      param6.PType,
      param7.PType,
      param8.PType,
      param9.PType,
      Z
    ](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label, Schema.defer(param7.typeclass), Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label, Schema.defer(param8.typeclass), Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label, Schema.defer(param9.typeclass), Chunk.fromIterable(param9.annotations)),
      construct = (
        p1: param1.PType,
        p2: param2.PType,
        p3: param3.PType,
        p4: param4.PType,
        p5: param5.PType,
        p6: param6.PType,
        p7: param7.PType,
        p8: param8.PType,
        p9: param9.PType
      ) => ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z)
    )
  }

  private def caseClass10[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1  = ctx.parameters.head
    val param2  = ctx.parameters(1)
    val param3  = ctx.parameters(2)
    val param4  = ctx.parameters(3)
    val param5  = ctx.parameters(4)
    val param6  = ctx.parameters(5)
    val param7  = ctx.parameters(6)
    val param8  = ctx.parameters(7)
    val param9  = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    new Schema.CaseClass10[
      param1.PType,
      param2.PType,
      param3.PType,
      param4.PType,
      param5.PType,
      param6.PType,
      param7.PType,
      param8.PType,
      param9.PType,
      param10.PType,
      Z
    ](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label, Schema.defer(param7.typeclass), Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label, Schema.defer(param8.typeclass), Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label, Schema.defer(param9.typeclass), Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label, Schema.defer(param10.typeclass), Chunk.fromIterable(param10.annotations)),
      construct = (
        p1: param1.PType,
        p2: param2.PType,
        p3: param3.PType,
        p4: param4.PType,
        p5: param5.PType,
        p6: param6.PType,
        p7: param7.PType,
        p8: param8.PType,
        p9: param9.PType,
        p10: param10.PType
      ) => ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z)
    )
  }

  private def caseClass11[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1  = ctx.parameters.head
    val param2  = ctx.parameters(1)
    val param3  = ctx.parameters(2)
    val param4  = ctx.parameters(3)
    val param5  = ctx.parameters(4)
    val param6  = ctx.parameters(5)
    val param7  = ctx.parameters(6)
    val param8  = ctx.parameters(7)
    val param9  = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    val param11 = ctx.parameters(10)
    new Schema.CaseClass11[
      param1.PType,
      param2.PType,
      param3.PType,
      param4.PType,
      param5.PType,
      param6.PType,
      param7.PType,
      param8.PType,
      param9.PType,
      param10.PType,
      param11.PType,
      Z
    ](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label, Schema.defer(param7.typeclass), Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label, Schema.defer(param8.typeclass), Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label, Schema.defer(param9.typeclass), Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label, Schema.defer(param10.typeclass), Chunk.fromIterable(param10.annotations)),
      field11 = Schema.Field(param11.label, Schema.defer(param11.typeclass), Chunk.fromIterable(param11.annotations)),
      construct = (
        p1: param1.PType,
        p2: param2.PType,
        p3: param3.PType,
        p4: param4.PType,
        p5: param5.PType,
        p6: param6.PType,
        p7: param7.PType,
        p8: param8.PType,
        p9: param9.PType,
        p10: param10.PType,
        p11: param11.PType
      ) => ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z),
      extractField11 = (z: Z) => param11.dereference(z)
    )
  }

  private def caseClass12[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1  = ctx.parameters.head
    val param2  = ctx.parameters(1)
    val param3  = ctx.parameters(2)
    val param4  = ctx.parameters(3)
    val param5  = ctx.parameters(4)
    val param6  = ctx.parameters(5)
    val param7  = ctx.parameters(6)
    val param8  = ctx.parameters(7)
    val param9  = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    val param11 = ctx.parameters(10)
    val param12 = ctx.parameters(11)
    new Schema.CaseClass12[
      param1.PType,
      param2.PType,
      param3.PType,
      param4.PType,
      param5.PType,
      param6.PType,
      param7.PType,
      param8.PType,
      param9.PType,
      param10.PType,
      param11.PType,
      param12.PType,
      Z
    ](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label, Schema.defer(param7.typeclass), Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label, Schema.defer(param8.typeclass), Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label, Schema.defer(param9.typeclass), Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label, Schema.defer(param10.typeclass), Chunk.fromIterable(param10.annotations)),
      field11 = Schema.Field(param11.label, Schema.defer(param11.typeclass), Chunk.fromIterable(param11.annotations)),
      field12 = Schema.Field(param12.label, Schema.defer(param12.typeclass), Chunk.fromIterable(param12.annotations)),
      construct = (
        p1: param1.PType,
        p2: param2.PType,
        p3: param3.PType,
        p4: param4.PType,
        p5: param5.PType,
        p6: param6.PType,
        p7: param7.PType,
        p8: param8.PType,
        p9: param9.PType,
        p10: param10.PType,
        p11: param11.PType,
        p12: param12.PType
      ) => ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z),
      extractField11 = (z: Z) => param11.dereference(z),
      extractField12 = (z: Z) => param12.dereference(z)
    )
  }

  private def caseClass13[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1  = ctx.parameters.head
    val param2  = ctx.parameters(1)
    val param3  = ctx.parameters(2)
    val param4  = ctx.parameters(3)
    val param5  = ctx.parameters(4)
    val param6  = ctx.parameters(5)
    val param7  = ctx.parameters(6)
    val param8  = ctx.parameters(7)
    val param9  = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    val param11 = ctx.parameters(10)
    val param12 = ctx.parameters(11)
    val param13 = ctx.parameters(12)
    new Schema.CaseClass13[
      param1.PType,
      param2.PType,
      param3.PType,
      param4.PType,
      param5.PType,
      param6.PType,
      param7.PType,
      param8.PType,
      param9.PType,
      param10.PType,
      param11.PType,
      param12.PType,
      param13.PType,
      Z
    ](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label, Schema.defer(param7.typeclass), Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label, Schema.defer(param8.typeclass), Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label, Schema.defer(param9.typeclass), Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label, Schema.defer(param10.typeclass), Chunk.fromIterable(param10.annotations)),
      field11 = Schema.Field(param11.label, Schema.defer(param11.typeclass), Chunk.fromIterable(param11.annotations)),
      field12 = Schema.Field(param12.label, Schema.defer(param12.typeclass), Chunk.fromIterable(param12.annotations)),
      field13 = Schema.Field(param13.label, Schema.defer(param13.typeclass), Chunk.fromIterable(param13.annotations)),
      construct = (
        p1: param1.PType,
        p2: param2.PType,
        p3: param3.PType,
        p4: param4.PType,
        p5: param5.PType,
        p6: param6.PType,
        p7: param7.PType,
        p8: param8.PType,
        p9: param9.PType,
        p10: param10.PType,
        p11: param11.PType,
        p12: param12.PType,
        p13: param13.PType
      ) => ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z),
      extractField11 = (z: Z) => param11.dereference(z),
      extractField12 = (z: Z) => param12.dereference(z),
      extractField13 = (z: Z) => param13.dereference(z)
    )
  }

  private def caseClass14[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1  = ctx.parameters.head
    val param2  = ctx.parameters(1)
    val param3  = ctx.parameters(2)
    val param4  = ctx.parameters(3)
    val param5  = ctx.parameters(4)
    val param6  = ctx.parameters(5)
    val param7  = ctx.parameters(6)
    val param8  = ctx.parameters(7)
    val param9  = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    val param11 = ctx.parameters(10)
    val param12 = ctx.parameters(11)
    val param13 = ctx.parameters(12)
    val param14 = ctx.parameters(13)
    new Schema.CaseClass14[
      param1.PType,
      param2.PType,
      param3.PType,
      param4.PType,
      param5.PType,
      param6.PType,
      param7.PType,
      param8.PType,
      param9.PType,
      param10.PType,
      param11.PType,
      param12.PType,
      param13.PType,
      param14.PType,
      Z
    ](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label, Schema.defer(param7.typeclass), Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label, Schema.defer(param8.typeclass), Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label, Schema.defer(param9.typeclass), Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label, Schema.defer(param10.typeclass), Chunk.fromIterable(param10.annotations)),
      field11 = Schema.Field(param11.label, Schema.defer(param11.typeclass), Chunk.fromIterable(param11.annotations)),
      field12 = Schema.Field(param12.label, Schema.defer(param12.typeclass), Chunk.fromIterable(param12.annotations)),
      field13 = Schema.Field(param13.label, Schema.defer(param13.typeclass), Chunk.fromIterable(param13.annotations)),
      field14 = Schema.Field(param14.label, Schema.defer(param14.typeclass), Chunk.fromIterable(param14.annotations)),
      construct = (
        p1: param1.PType,
        p2: param2.PType,
        p3: param3.PType,
        p4: param4.PType,
        p5: param5.PType,
        p6: param6.PType,
        p7: param7.PType,
        p8: param8.PType,
        p9: param9.PType,
        p10: param10.PType,
        p11: param11.PType,
        p12: param12.PType,
        p13: param13.PType,
        p14: param14.PType
      ) => ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z),
      extractField11 = (z: Z) => param11.dereference(z),
      extractField12 = (z: Z) => param12.dereference(z),
      extractField13 = (z: Z) => param13.dereference(z),
      extractField14 = (z: Z) => param14.dereference(z)
    )
  }

  private def caseClass15[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1  = ctx.parameters.head
    val param2  = ctx.parameters(1)
    val param3  = ctx.parameters(2)
    val param4  = ctx.parameters(3)
    val param5  = ctx.parameters(4)
    val param6  = ctx.parameters(5)
    val param7  = ctx.parameters(6)
    val param8  = ctx.parameters(7)
    val param9  = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    val param11 = ctx.parameters(10)
    val param12 = ctx.parameters(11)
    val param13 = ctx.parameters(12)
    val param14 = ctx.parameters(13)
    val param15 = ctx.parameters(14)
    new Schema.CaseClass15[
      param1.PType,
      param2.PType,
      param3.PType,
      param4.PType,
      param5.PType,
      param6.PType,
      param7.PType,
      param8.PType,
      param9.PType,
      param10.PType,
      param11.PType,
      param12.PType,
      param13.PType,
      param14.PType,
      param15.PType,
      Z
    ](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label, Schema.defer(param7.typeclass), Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label, Schema.defer(param8.typeclass), Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label, Schema.defer(param9.typeclass), Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label, Schema.defer(param10.typeclass), Chunk.fromIterable(param10.annotations)),
      field11 = Schema.Field(param11.label, Schema.defer(param11.typeclass), Chunk.fromIterable(param11.annotations)),
      field12 = Schema.Field(param12.label, Schema.defer(param12.typeclass), Chunk.fromIterable(param12.annotations)),
      field13 = Schema.Field(param13.label, Schema.defer(param13.typeclass), Chunk.fromIterable(param13.annotations)),
      field14 = Schema.Field(param14.label, Schema.defer(param14.typeclass), Chunk.fromIterable(param14.annotations)),
      field15 = Schema.Field(param15.label, Schema.defer(param15.typeclass), Chunk.fromIterable(param15.annotations)),
      construct = (
        p1: param1.PType,
        p2: param2.PType,
        p3: param3.PType,
        p4: param4.PType,
        p5: param5.PType,
        p6: param6.PType,
        p7: param7.PType,
        p8: param8.PType,
        p9: param9.PType,
        p10: param10.PType,
        p11: param11.PType,
        p12: param12.PType,
        p13: param13.PType,
        p14: param14.PType,
        p15: param15.PType
      ) => ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z),
      extractField11 = (z: Z) => param11.dereference(z),
      extractField12 = (z: Z) => param12.dereference(z),
      extractField13 = (z: Z) => param13.dereference(z),
      extractField14 = (z: Z) => param14.dereference(z),
      extractField15 = (z: Z) => param15.dereference(z)
    )
  }

  private def caseClass16[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1  = ctx.parameters.head
    val param2  = ctx.parameters(1)
    val param3  = ctx.parameters(2)
    val param4  = ctx.parameters(3)
    val param5  = ctx.parameters(4)
    val param6  = ctx.parameters(5)
    val param7  = ctx.parameters(6)
    val param8  = ctx.parameters(7)
    val param9  = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    val param11 = ctx.parameters(10)
    val param12 = ctx.parameters(11)
    val param13 = ctx.parameters(12)
    val param14 = ctx.parameters(13)
    val param15 = ctx.parameters(14)
    val param16 = ctx.parameters(15)
    new Schema.CaseClass16[
      param1.PType,
      param2.PType,
      param3.PType,
      param4.PType,
      param5.PType,
      param6.PType,
      param7.PType,
      param8.PType,
      param9.PType,
      param10.PType,
      param11.PType,
      param12.PType,
      param13.PType,
      param14.PType,
      param15.PType,
      param16.PType,
      Z
    ](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label, Schema.defer(param7.typeclass), Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label, Schema.defer(param8.typeclass), Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label, Schema.defer(param9.typeclass), Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label, Schema.defer(param10.typeclass), Chunk.fromIterable(param10.annotations)),
      field11 = Schema.Field(param11.label, Schema.defer(param11.typeclass), Chunk.fromIterable(param11.annotations)),
      field12 = Schema.Field(param12.label, Schema.defer(param12.typeclass), Chunk.fromIterable(param12.annotations)),
      field13 = Schema.Field(param13.label, Schema.defer(param13.typeclass), Chunk.fromIterable(param13.annotations)),
      field14 = Schema.Field(param14.label, Schema.defer(param14.typeclass), Chunk.fromIterable(param14.annotations)),
      field15 = Schema.Field(param15.label, Schema.defer(param15.typeclass), Chunk.fromIterable(param15.annotations)),
      field16 = Schema.Field(param16.label, Schema.defer(param16.typeclass), Chunk.fromIterable(param16.annotations)),
      construct = (
        p1: param1.PType,
        p2: param2.PType,
        p3: param3.PType,
        p4: param4.PType,
        p5: param5.PType,
        p6: param6.PType,
        p7: param7.PType,
        p8: param8.PType,
        p9: param9.PType,
        p10: param10.PType,
        p11: param11.PType,
        p12: param12.PType,
        p13: param13.PType,
        p14: param14.PType,
        p15: param15.PType,
        p16: param16.PType
      ) => ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z),
      extractField11 = (z: Z) => param11.dereference(z),
      extractField12 = (z: Z) => param12.dereference(z),
      extractField13 = (z: Z) => param13.dereference(z),
      extractField14 = (z: Z) => param14.dereference(z),
      extractField15 = (z: Z) => param15.dereference(z),
      extractField16 = (z: Z) => param16.dereference(z)
    )
  }

  private def caseClass17[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1  = ctx.parameters.head
    val param2  = ctx.parameters(1)
    val param3  = ctx.parameters(2)
    val param4  = ctx.parameters(3)
    val param5  = ctx.parameters(4)
    val param6  = ctx.parameters(5)
    val param7  = ctx.parameters(6)
    val param8  = ctx.parameters(7)
    val param9  = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    val param11 = ctx.parameters(10)
    val param12 = ctx.parameters(11)
    val param13 = ctx.parameters(12)
    val param14 = ctx.parameters(13)
    val param15 = ctx.parameters(14)
    val param16 = ctx.parameters(15)
    val param17 = ctx.parameters(16)
    new Schema.CaseClass17[
      param1.PType,
      param2.PType,
      param3.PType,
      param4.PType,
      param5.PType,
      param6.PType,
      param7.PType,
      param8.PType,
      param9.PType,
      param10.PType,
      param11.PType,
      param12.PType,
      param13.PType,
      param14.PType,
      param15.PType,
      param16.PType,
      param17.PType,
      Z
    ](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label, Schema.defer(param7.typeclass), Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label, Schema.defer(param8.typeclass), Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label, Schema.defer(param9.typeclass), Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label, Schema.defer(param10.typeclass), Chunk.fromIterable(param10.annotations)),
      field11 = Schema.Field(param11.label, Schema.defer(param11.typeclass), Chunk.fromIterable(param11.annotations)),
      field12 = Schema.Field(param12.label, Schema.defer(param12.typeclass), Chunk.fromIterable(param12.annotations)),
      field13 = Schema.Field(param13.label, Schema.defer(param13.typeclass), Chunk.fromIterable(param13.annotations)),
      field14 = Schema.Field(param14.label, Schema.defer(param14.typeclass), Chunk.fromIterable(param14.annotations)),
      field15 = Schema.Field(param15.label, Schema.defer(param15.typeclass), Chunk.fromIterable(param15.annotations)),
      field16 = Schema.Field(param16.label, Schema.defer(param16.typeclass), Chunk.fromIterable(param16.annotations)),
      field17 = Schema.Field(param17.label, Schema.defer(param17.typeclass), Chunk.fromIterable(param17.annotations)),
      construct = (
        p1: param1.PType,
        p2: param2.PType,
        p3: param3.PType,
        p4: param4.PType,
        p5: param5.PType,
        p6: param6.PType,
        p7: param7.PType,
        p8: param8.PType,
        p9: param9.PType,
        p10: param10.PType,
        p11: param11.PType,
        p12: param12.PType,
        p13: param13.PType,
        p14: param14.PType,
        p15: param15.PType,
        p16: param16.PType,
        p17: param17.PType
      ) => ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z),
      extractField11 = (z: Z) => param11.dereference(z),
      extractField12 = (z: Z) => param12.dereference(z),
      extractField13 = (z: Z) => param13.dereference(z),
      extractField14 = (z: Z) => param14.dereference(z),
      extractField15 = (z: Z) => param15.dereference(z),
      extractField16 = (z: Z) => param16.dereference(z),
      extractField17 = (z: Z) => param17.dereference(z)
    )
  }

  private def caseClass18[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1  = ctx.parameters.head
    val param2  = ctx.parameters(1)
    val param3  = ctx.parameters(2)
    val param4  = ctx.parameters(3)
    val param5  = ctx.parameters(4)
    val param6  = ctx.parameters(5)
    val param7  = ctx.parameters(6)
    val param8  = ctx.parameters(7)
    val param9  = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    val param11 = ctx.parameters(10)
    val param12 = ctx.parameters(11)
    val param13 = ctx.parameters(12)
    val param14 = ctx.parameters(13)
    val param15 = ctx.parameters(14)
    val param16 = ctx.parameters(15)
    val param17 = ctx.parameters(16)
    val param18 = ctx.parameters(17)
    new Schema.CaseClass18[
      param1.PType,
      param2.PType,
      param3.PType,
      param4.PType,
      param5.PType,
      param6.PType,
      param7.PType,
      param8.PType,
      param9.PType,
      param10.PType,
      param11.PType,
      param12.PType,
      param13.PType,
      param14.PType,
      param15.PType,
      param16.PType,
      param17.PType,
      param18.PType,
      Z
    ](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label, Schema.defer(param1.typeclass), Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label, Schema.defer(param2.typeclass), Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label, Schema.defer(param3.typeclass), Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label, Schema.defer(param4.typeclass), Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label, Schema.defer(param5.typeclass), Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label, Schema.defer(param6.typeclass), Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label, Schema.defer(param7.typeclass), Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label, Schema.defer(param8.typeclass), Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label, Schema.defer(param9.typeclass), Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label, Schema.defer(param10.typeclass), Chunk.fromIterable(param10.annotations)),
      field11 = Schema.Field(param11.label, Schema.defer(param11.typeclass), Chunk.fromIterable(param11.annotations)),
      field12 = Schema.Field(param12.label, Schema.defer(param12.typeclass), Chunk.fromIterable(param12.annotations)),
      field13 = Schema.Field(param13.label, Schema.defer(param13.typeclass), Chunk.fromIterable(param13.annotations)),
      field14 = Schema.Field(param14.label, Schema.defer(param14.typeclass), Chunk.fromIterable(param14.annotations)),
      field15 = Schema.Field(param15.label, Schema.defer(param15.typeclass), Chunk.fromIterable(param15.annotations)),
      field16 = Schema.Field(param16.label, Schema.defer(param16.typeclass), Chunk.fromIterable(param16.annotations)),
      field17 = Schema.Field(param17.label, Schema.defer(param17.typeclass), Chunk.fromIterable(param17.annotations)),
      field18 = Schema.Field(param18.label, Schema.defer(param18.typeclass), Chunk.fromIterable(param18.annotations)),
      construct = (
        p1: param1.PType,
        p2: param2.PType,
        p3: param3.PType,
        p4: param4.PType,
        p5: param5.PType,
        p6: param6.PType,
        p7: param7.PType,
        p8: param8.PType,
        p9: param9.PType,
        p10: param10.PType,
        p11: param11.PType,
        p12: param12.PType,
        p13: param13.PType,
        p14: param14.PType,
        p15: param15.PType,
        p16: param16.PType,
        p17: param17.PType,
        p18: param18.PType
      ) => ctx.rawConstruct(Seq(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z),
      extractField11 = (z: Z) => param11.dereference(z),
      extractField12 = (z: Z) => param12.dereference(z),
      extractField13 = (z: Z) => param13.dereference(z),
      extractField14 = (z: Z) => param14.dereference(z),
      extractField15 = (z: Z) => param15.dereference(z),
      extractField16 = (z: Z) => param16.dereference(z),
      extractField17 = (z: Z) => param17.dereference(z),
      extractField18 = (z: Z) => param18.dereference(z)
    )
  }

  // format: off
  private def caseClass19[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    val param3 = ctx.parameters(2)
    val param4 = ctx.parameters(3)
    val param5 = ctx.parameters(4)
    val param6 = ctx.parameters(5)
    val param7 = ctx.parameters(6)
    val param8 = ctx.parameters(7)
    val param9 = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    val param11 = ctx.parameters(10)
    val param12 = ctx.parameters(11)
    val param13 = ctx.parameters(12)
    val param14 = ctx.parameters(13)
    val param15 = ctx.parameters(14)
    val param16 = ctx.parameters(15)
    val param17 = ctx.parameters(16)
    val param18 = ctx.parameters(17)
    val param19 = ctx.parameters(18)
    new Schema.CaseClass19[param1.PType, param2.PType, param3.PType, param4.PType, param5.PType, param6.PType,param7.PType,param8.PType,param9.PType,param10.PType,param11.PType,param12.PType,param13.PType,param14.PType,param15.PType,param16.PType,param17.PType,param18.PType,param19.PType,Z](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label,Schema.defer(param1.typeclass),Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label,Schema.defer(param2.typeclass),Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label,Schema.defer(param3.typeclass),Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label,Schema.defer(param4.typeclass),Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label,Schema.defer(param5.typeclass),Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label,Schema.defer(param6.typeclass),Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label,Schema.defer(param7.typeclass),Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label,Schema.defer(param8.typeclass),Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label,Schema.defer(param9.typeclass),Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label,Schema.defer(param10.typeclass),Chunk.fromIterable(param10.annotations)),
      field11 = Schema.Field(param11.label,Schema.defer(param11.typeclass),Chunk.fromIterable(param11.annotations)),
      field12 = Schema.Field(param12.label,Schema.defer(param12.typeclass),Chunk.fromIterable(param12.annotations)),
      field13 = Schema.Field(param13.label,Schema.defer(param13.typeclass),Chunk.fromIterable(param13.annotations)),
      field14 = Schema.Field(param14.label,Schema.defer(param14.typeclass),Chunk.fromIterable(param14.annotations)),
      field15 = Schema.Field(param15.label,Schema.defer(param15.typeclass),Chunk.fromIterable(param15.annotations)),
      field16 = Schema.Field(param16.label,Schema.defer(param16.typeclass),Chunk.fromIterable(param16.annotations)),
      field17 = Schema.Field(param17.label,Schema.defer(param17.typeclass),Chunk.fromIterable(param17.annotations)),
      field18 = Schema.Field(param18.label,Schema.defer(param18.typeclass),Chunk.fromIterable(param18.annotations)),
      field19 = Schema.Field(param19.label,Schema.defer(param19.typeclass),Chunk.fromIterable(param19.annotations)),
      construct = (p1: param1.PType, p2: param2.PType, p3: param3.PType, p4: param4.PType, p5: param5.PType,p6: param6.PType,p7: param7.PType,p8: param8.PType,p9: param9.PType,p10: param10.PType,p11: param11.PType,p12: param12.PType,p13: param13.PType,p14: param14.PType,p15: param15.PType,p16: param16.PType,p17: param17.PType,p18: param18.PType,p19: param19.PType) => ctx.rawConstruct(Seq(p1, p2, p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z),
      extractField11 = (z: Z) => param11.dereference(z),
      extractField12 = (z: Z) => param12.dereference(z),
      extractField13 = (z: Z) => param13.dereference(z),
      extractField14 = (z: Z) => param14.dereference(z),
      extractField15 = (z: Z) => param15.dereference(z),
      extractField16 = (z: Z) => param16.dereference(z),
      extractField17 = (z: Z) => param17.dereference(z),
      extractField18 = (z: Z) => param18.dereference(z),
      extractField19 = (z: Z) => param19.dereference(z)
    )
  }

  private def caseClass20[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    val param3 = ctx.parameters(2)
    val param4 = ctx.parameters(3)
    val param5 = ctx.parameters(4)
    val param6 = ctx.parameters(5)
    val param7 = ctx.parameters(6)
    val param8 = ctx.parameters(7)
    val param9 = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    val param11 = ctx.parameters(10)
    val param12 = ctx.parameters(11)
    val param13 = ctx.parameters(12)
    val param14 = ctx.parameters(13)
    val param15 = ctx.parameters(14)
    val param16 = ctx.parameters(15)
    val param17 = ctx.parameters(16)
    val param18 = ctx.parameters(17)
    val param19 = ctx.parameters(18)
    val param20 = ctx.parameters(19)
    new Schema.CaseClass20[param1.PType, param2.PType, param3.PType, param4.PType, param5.PType, param6.PType,param7.PType,param8.PType,param9.PType,param10.PType,param11.PType,param12.PType,param13.PType,param14.PType,param15.PType,param16.PType,param17.PType,param18.PType,param19.PType,param20.PType,Z](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label,Schema.defer(param1.typeclass),Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label,Schema.defer(param2.typeclass),Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label,Schema.defer(param3.typeclass),Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label,Schema.defer(param4.typeclass),Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label,Schema.defer(param5.typeclass),Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label,Schema.defer(param6.typeclass),Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label,Schema.defer(param7.typeclass),Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label,Schema.defer(param8.typeclass),Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label,Schema.defer(param9.typeclass),Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label,Schema.defer(param10.typeclass),Chunk.fromIterable(param10.annotations)),
      field11 = Schema.Field(param11.label,Schema.defer(param11.typeclass),Chunk.fromIterable(param11.annotations)),
      field12 = Schema.Field(param12.label,Schema.defer(param12.typeclass),Chunk.fromIterable(param12.annotations)),
      field13 = Schema.Field(param13.label,Schema.defer(param13.typeclass),Chunk.fromIterable(param13.annotations)),
      field14 = Schema.Field(param14.label,Schema.defer(param14.typeclass),Chunk.fromIterable(param14.annotations)),
      field15 = Schema.Field(param15.label,Schema.defer(param15.typeclass),Chunk.fromIterable(param15.annotations)),
      field16 = Schema.Field(param16.label,Schema.defer(param16.typeclass),Chunk.fromIterable(param16.annotations)),
      field17 = Schema.Field(param17.label,Schema.defer(param17.typeclass),Chunk.fromIterable(param17.annotations)),
      field18 = Schema.Field(param18.label,Schema.defer(param18.typeclass),Chunk.fromIterable(param18.annotations)),
      field19 = Schema.Field(param19.label,Schema.defer(param19.typeclass),Chunk.fromIterable(param19.annotations)),
      field20 = Schema.Field(param20.label,Schema.defer(param20.typeclass),Chunk.fromIterable(param20.annotations)),
      construct = (p1: param1.PType, p2: param2.PType, p3: param3.PType, p4: param4.PType, p5: param5.PType,p6: param6.PType,p7: param7.PType,p8: param8.PType,p9: param9.PType,p10: param10.PType,p11: param11.PType,p12: param12.PType,p13: param13.PType,p14: param14.PType,p15: param15.PType,p16: param16.PType,p17: param17.PType,p18: param18.PType,p19: param19.PType,p20: param20.PType) => ctx.rawConstruct(Seq(p1, p2, p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z),
      extractField11 = (z: Z) => param11.dereference(z),
      extractField12 = (z: Z) => param12.dereference(z),
      extractField13 = (z: Z) => param13.dereference(z),
      extractField14 = (z: Z) => param14.dereference(z),
      extractField15 = (z: Z) => param15.dereference(z),
      extractField16 = (z: Z) => param16.dereference(z),
      extractField17 = (z: Z) => param17.dereference(z),
      extractField18 = (z: Z) => param18.dereference(z),
      extractField19 = (z: Z) => param19.dereference(z),
      extractField20 = (z: Z) => param20.dereference(z)
    )
  }

  private def caseClass21[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    val param3 = ctx.parameters(2)
    val param4 = ctx.parameters(3)
    val param5 = ctx.parameters(4)
    val param6 = ctx.parameters(5)
    val param7 = ctx.parameters(6)
    val param8 = ctx.parameters(7)
    val param9 = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    val param11 = ctx.parameters(10)
    val param12 = ctx.parameters(11)
    val param13 = ctx.parameters(12)
    val param14 = ctx.parameters(13)
    val param15 = ctx.parameters(14)
    val param16 = ctx.parameters(15)
    val param17 = ctx.parameters(16)
    val param18 = ctx.parameters(17)
    val param19 = ctx.parameters(18)
    val param20 = ctx.parameters(19)
    val param21 = ctx.parameters(20)
    new Schema.CaseClass21[param1.PType, param2.PType, param3.PType, param4.PType, param5.PType, param6.PType,param7.PType,param8.PType,param9.PType,param10.PType,param11.PType,param12.PType,param13.PType,param14.PType,param15.PType,param16.PType,param17.PType,param18.PType,param19.PType,param20.PType,param21.PType,Z](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label,Schema.defer(param1.typeclass),Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label,Schema.defer(param2.typeclass),Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label,Schema.defer(param3.typeclass),Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label,Schema.defer(param4.typeclass),Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label,Schema.defer(param5.typeclass),Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label,Schema.defer(param6.typeclass),Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label,Schema.defer(param7.typeclass),Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label,Schema.defer(param8.typeclass),Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label,Schema.defer(param9.typeclass),Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label,Schema.defer(param10.typeclass),Chunk.fromIterable(param10.annotations)),
      field11 = Schema.Field(param11.label,Schema.defer(param11.typeclass),Chunk.fromIterable(param11.annotations)),
      field12 = Schema.Field(param12.label,Schema.defer(param12.typeclass),Chunk.fromIterable(param12.annotations)),
      field13 = Schema.Field(param13.label,Schema.defer(param13.typeclass),Chunk.fromIterable(param13.annotations)),
      field14 = Schema.Field(param14.label,Schema.defer(param14.typeclass),Chunk.fromIterable(param14.annotations)),
      field15 = Schema.Field(param15.label,Schema.defer(param15.typeclass),Chunk.fromIterable(param15.annotations)),
      field16 = Schema.Field(param16.label,Schema.defer(param16.typeclass),Chunk.fromIterable(param16.annotations)),
      field17 = Schema.Field(param17.label,Schema.defer(param17.typeclass),Chunk.fromIterable(param17.annotations)),
      field18 = Schema.Field(param18.label,Schema.defer(param18.typeclass),Chunk.fromIterable(param18.annotations)),
      field19 = Schema.Field(param19.label,Schema.defer(param19.typeclass),Chunk.fromIterable(param19.annotations)),
      field20 = Schema.Field(param20.label,Schema.defer(param20.typeclass),Chunk.fromIterable(param20.annotations)),
      field21 = Schema.Field(param21.label,Schema.defer(param21.typeclass),Chunk.fromIterable(param21.annotations)),
      construct = (p1: param1.PType, p2: param2.PType, p3: param3.PType, p4: param4.PType, p5: param5.PType,p6: param6.PType,p7: param7.PType,p8: param8.PType,p9: param9.PType,p10: param10.PType,p11: param11.PType,p12: param12.PType,p13: param13.PType,p14: param14.PType,p15: param15.PType,p16: param16.PType,p17: param17.PType,p18: param18.PType,p19: param19.PType,p20: param20.PType,p21: param21.PType) => ctx.rawConstruct(Seq(p1, p2, p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z),
      extractField11 = (z: Z) => param11.dereference(z),
      extractField12 = (z: Z) => param12.dereference(z),
      extractField13 = (z: Z) => param13.dereference(z),
      extractField14 = (z: Z) => param14.dereference(z),
      extractField15 = (z: Z) => param15.dereference(z),
      extractField16 = (z: Z) => param16.dereference(z),
      extractField17 = (z: Z) => param17.dereference(z),
      extractField18 = (z: Z) => param18.dereference(z),
      extractField19 = (z: Z) => param19.dereference(z),
      extractField20 = (z: Z) => param20.dereference(z),
      extractField21 = (z: Z) => param21.dereference(z)
    )
  }

  private def caseClass22[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    val param3 = ctx.parameters(2)
    val param4 = ctx.parameters(3)
    val param5 = ctx.parameters(4)
    val param6 = ctx.parameters(5)
    val param7 = ctx.parameters(6)
    val param8 = ctx.parameters(7)
    val param9 = ctx.parameters(8)
    val param10 = ctx.parameters(9)
    val param11 = ctx.parameters(10)
    val param12 = ctx.parameters(11)
    val param13 = ctx.parameters(12)
    val param14 = ctx.parameters(13)
    val param15 = ctx.parameters(14)
    val param16 = ctx.parameters(15)
    val param17 = ctx.parameters(16)
    val param18 = ctx.parameters(17)
    val param19 = ctx.parameters(18)
    val param20 = ctx.parameters(19)
    val param21 = ctx.parameters(20)
    val param22 = ctx.parameters(21)
    new Schema.CaseClass22[param1.PType, param2.PType, param3.PType, param4.PType, param5.PType, param6.PType,param7.PType,param8.PType,param9.PType,param10.PType,param11.PType,param12.PType,param13.PType,param14.PType,param15.PType,param16.PType,param17.PType,param18.PType,param19.PType,param20.PType,param21.PType,param22.PType,Z](
      annotations = Chunk.fromIterable(ctx.annotations),
      field1 = Schema.Field(param1.label,Schema.defer(param1.typeclass),Chunk.fromIterable(param1.annotations)),
      field2 = Schema.Field(param2.label,Schema.defer(param2.typeclass),Chunk.fromIterable(param2.annotations)),
      field3 = Schema.Field(param3.label,Schema.defer(param3.typeclass),Chunk.fromIterable(param3.annotations)),
      field4 = Schema.Field(param4.label,Schema.defer(param4.typeclass),Chunk.fromIterable(param4.annotations)),
      field5 = Schema.Field(param5.label,Schema.defer(param5.typeclass),Chunk.fromIterable(param5.annotations)),
      field6 = Schema.Field(param6.label,Schema.defer(param6.typeclass),Chunk.fromIterable(param6.annotations)),
      field7 = Schema.Field(param7.label,Schema.defer(param7.typeclass),Chunk.fromIterable(param7.annotations)),
      field8 = Schema.Field(param8.label,Schema.defer(param8.typeclass),Chunk.fromIterable(param8.annotations)),
      field9 = Schema.Field(param9.label,Schema.defer(param9.typeclass),Chunk.fromIterable(param9.annotations)),
      field10 = Schema.Field(param10.label,Schema.defer(param10.typeclass),Chunk.fromIterable(param10.annotations)),
      field11 = Schema.Field(param11.label,Schema.defer(param11.typeclass),Chunk.fromIterable(param11.annotations)),
      field12 = Schema.Field(param12.label,Schema.defer(param12.typeclass),Chunk.fromIterable(param12.annotations)),
      field13 = Schema.Field(param13.label,Schema.defer(param13.typeclass),Chunk.fromIterable(param13.annotations)),
      field14 = Schema.Field(param14.label,Schema.defer(param14.typeclass),Chunk.fromIterable(param14.annotations)),
      field15 = Schema.Field(param15.label,Schema.defer(param15.typeclass),Chunk.fromIterable(param15.annotations)),
      field16 = Schema.Field(param16.label,Schema.defer(param16.typeclass),Chunk.fromIterable(param16.annotations)),
      field17 = Schema.Field(param17.label,Schema.defer(param17.typeclass),Chunk.fromIterable(param17.annotations)),
      field18 = Schema.Field(param18.label,Schema.defer(param18.typeclass),Chunk.fromIterable(param18.annotations)),
      field19 = Schema.Field(param19.label,Schema.defer(param19.typeclass),Chunk.fromIterable(param19.annotations)),
      field20 = Schema.Field(param20.label,Schema.defer(param20.typeclass),Chunk.fromIterable(param20.annotations)),
      field21 = Schema.Field(param21.label,Schema.defer(param21.typeclass),Chunk.fromIterable(param21.annotations)),
      field22 = Schema.Field(param22.label,Schema.defer(param22.typeclass),Chunk.fromIterable(param22.annotations)),
      construct = (p1: param1.PType, p2: param2.PType, p3: param3.PType, p4: param4.PType, p5: param5.PType,p6: param6.PType,p7: param7.PType,p8: param8.PType,p9: param9.PType,p10: param10.PType,p11: param11.PType,p12: param12.PType,p13: param13.PType,p14: param14.PType,p15: param15.PType,p16: param16.PType,p17: param17.PType,p18: param18.PType,p19: param19.PType,p20: param20.PType,p21: param21.PType,p22: param22.PType) => ctx.rawConstruct(Seq(p1, p2, p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22)),
      extractField1 = (z: Z) => param1.dereference(z),
      extractField2 = (z: Z) => param2.dereference(z),
      extractField3 = (z: Z) => param3.dereference(z),
      extractField4 = (z: Z) => param4.dereference(z),
      extractField5 = (z: Z) => param5.dereference(z),
      extractField6 = (z: Z) => param6.dereference(z),
      extractField7 = (z: Z) => param7.dereference(z),
      extractField8 = (z: Z) => param8.dereference(z),
      extractField9 = (z: Z) => param9.dereference(z),
      extractField10 = (z: Z) => param10.dereference(z),
      extractField11 = (z: Z) => param11.dereference(z),
      extractField12 = (z: Z) => param12.dereference(z),
      extractField13 = (z: Z) => param13.dereference(z),
      extractField14 = (z: Z) => param14.dereference(z),
      extractField15 = (z: Z) => param15.dereference(z),
      extractField16 = (z: Z) => param16.dereference(z),
      extractField17 = (z: Z) => param17.dereference(z),
      extractField18 = (z: Z) => param18.dereference(z),
      extractField19 = (z: Z) => param19.dereference(z),
      extractField20 = (z: Z) => param20.dereference(z),
      extractField21 = (z: Z) => param21.dereference(z),
      extractField22 = (z: Z) => param22.dereference(z)
    )
  }

  def dispatch[A](sealedTrait: SealedTrait[Schema, A]): Schema[A] = {
    type S = Subtype[Schema, A]#SType
    sealedTrait.subtypes.sortBy(_.typeName.short).map { subtype: Subtype[Schema, A] =>
      Schema.Case[S, A](
        id = subtype.typeName.short,
        codec = subtype.typeclass.asInstanceOf[Schema[S]],
        unsafeDeconstruct =
          (a: A) => if (subtype.cast.isDefinedAt(a)) subtype.cast(a) else throw new IllegalArgumentException
      )
    } match {
      case Seq(c)          => Schema.Enum1(c)
      case Seq(c1, c2)     => Schema.Enum2(c1, c2)
      case Seq(c1, c2, c3) => Schema.Enum3(c1, c2, c3)
      case Seq(c1, c2, c3,c4) => Schema.Enum4(c1, c2, c3,c4)
      case Seq(c1, c2, c3,c4,c5) => Schema.Enum5(c1, c2, c3,c4,c5)
      case Seq(c1, c2, c3,c4,c5,c6) => Schema.Enum6(c1, c2, c3,c4,c5,c6)

      case cases           =>
        Schema.EnumN(
          cases.foldRight[CaseSet.Aux[A]](CaseSet.Empty[A]()) {
            case (c, acc) => CaseSet.Cons(c,acc)
          }
        )
    }
  }
  // format: on

  @deprecated(
    message = "Magnolia-based derivation is deprecated. Please use SchemaDerivation.gen instead",
    since = "0.1.3"
  )
  implicit def gen[T]: Schema[T] = macro Magnolia.gen[T]

}
