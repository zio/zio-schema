package zio.schema

import scala.language.experimental.macros

import magnolia._

object DeriveSchema {

  type Typeclass[A] = Schema[A]

  def combine[A](ctx: CaseClass[Schema, A]): Schema[A] =
    ctx.parameters.size match {
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

  private def caseClass1[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param = ctx.parameters.head
    new Schema.CaseClass1[param.PType, Z](
      field = (param.label, param.typeclass),
      construct = (p: param.PType) => ctx.construct(_ => p),
      extractField = (z: Z) => param.dereference(z)
    )
  }

  private def caseClass2[Z](ctx: CaseClass[Typeclass, Z]): Typeclass[Z] = {
    val param1 = ctx.parameters.head
    val param2 = ctx.parameters(1)
    new Schema.CaseClass2[param1.PType, param2.PType, Z](
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
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
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      field3 = param3.label -> param3.typeclass,
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
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      field3 = param3.label -> param3.typeclass,
      field4 = param4.label -> param4.typeclass,
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
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      field3 = param3.label -> param3.typeclass,
      field4 = param4.label -> param4.typeclass,
      field5 = param5.label -> param5.typeclass,
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
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      field3 = param3.label -> param3.typeclass,
      field4 = param4.label -> param4.typeclass,
      field5 = param5.label -> param5.typeclass,
      field6 = param6.label -> param6.typeclass,
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
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      field3 = param3.label -> param3.typeclass,
      field4 = param4.label -> param4.typeclass,
      field5 = param5.label -> param5.typeclass,
      field6 = param6.label -> param6.typeclass,
      field7 = param7.label -> param7.typeclass,
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
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      field3 = param3.label -> param3.typeclass,
      field4 = param4.label -> param4.typeclass,
      field5 = param5.label -> param5.typeclass,
      field6 = param6.label -> param6.typeclass,
      field7 = param7.label -> param7.typeclass,
      field8 = param8.label -> param8.typeclass,
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
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      field3 = param3.label -> param3.typeclass,
      field4 = param4.label -> param4.typeclass,
      field5 = param5.label -> param5.typeclass,
      field6 = param6.label -> param6.typeclass,
      field7 = param7.label -> param7.typeclass,
      field8 = param8.label -> param8.typeclass,
      field9 = param9.label -> param9.typeclass,
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
      field1 = param1.label   -> param1.typeclass,
      field2 = param2.label   -> param2.typeclass,
      field3 = param3.label   -> param3.typeclass,
      field4 = param4.label   -> param4.typeclass,
      field5 = param5.label   -> param5.typeclass,
      field6 = param6.label   -> param6.typeclass,
      field7 = param7.label   -> param7.typeclass,
      field8 = param8.label   -> param8.typeclass,
      field9 = param9.label   -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
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
      field1 = param1.label   -> param1.typeclass,
      field2 = param2.label   -> param2.typeclass,
      field3 = param3.label   -> param3.typeclass,
      field4 = param4.label   -> param4.typeclass,
      field5 = param5.label   -> param5.typeclass,
      field6 = param6.label   -> param6.typeclass,
      field7 = param7.label   -> param7.typeclass,
      field8 = param8.label   -> param8.typeclass,
      field9 = param9.label   -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
      field11 = param11.label -> param11.typeclass,
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
      field1 = param1.label   -> param1.typeclass,
      field2 = param2.label   -> param2.typeclass,
      field3 = param3.label   -> param3.typeclass,
      field4 = param4.label   -> param4.typeclass,
      field5 = param5.label   -> param5.typeclass,
      field6 = param6.label   -> param6.typeclass,
      field7 = param7.label   -> param7.typeclass,
      field8 = param8.label   -> param8.typeclass,
      field9 = param9.label   -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
      field11 = param11.label -> param11.typeclass,
      field12 = param12.label -> param12.typeclass,
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
      field1 = param1.label   -> param1.typeclass,
      field2 = param2.label   -> param2.typeclass,
      field3 = param3.label   -> param3.typeclass,
      field4 = param4.label   -> param4.typeclass,
      field5 = param5.label   -> param5.typeclass,
      field6 = param6.label   -> param6.typeclass,
      field7 = param7.label   -> param7.typeclass,
      field8 = param8.label   -> param8.typeclass,
      field9 = param9.label   -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
      field11 = param11.label -> param11.typeclass,
      field12 = param12.label -> param12.typeclass,
      field13 = param13.label -> param13.typeclass,
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
      field1 = param1.label   -> param1.typeclass,
      field2 = param2.label   -> param2.typeclass,
      field3 = param3.label   -> param3.typeclass,
      field4 = param4.label   -> param4.typeclass,
      field5 = param5.label   -> param5.typeclass,
      field6 = param6.label   -> param6.typeclass,
      field7 = param7.label   -> param7.typeclass,
      field8 = param8.label   -> param8.typeclass,
      field9 = param9.label   -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
      field11 = param11.label -> param11.typeclass,
      field12 = param12.label -> param12.typeclass,
      field13 = param13.label -> param13.typeclass,
      field14 = param14.label -> param14.typeclass,
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
      field1 = param1.label   -> param1.typeclass,
      field2 = param2.label   -> param2.typeclass,
      field3 = param3.label   -> param3.typeclass,
      field4 = param4.label   -> param4.typeclass,
      field5 = param5.label   -> param5.typeclass,
      field6 = param6.label   -> param6.typeclass,
      field7 = param7.label   -> param7.typeclass,
      field8 = param8.label   -> param8.typeclass,
      field9 = param9.label   -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
      field11 = param11.label -> param11.typeclass,
      field12 = param12.label -> param12.typeclass,
      field13 = param13.label -> param13.typeclass,
      field14 = param14.label -> param14.typeclass,
      field15 = param15.label -> param15.typeclass,
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
      field1 = param1.label   -> param1.typeclass,
      field2 = param2.label   -> param2.typeclass,
      field3 = param3.label   -> param3.typeclass,
      field4 = param4.label   -> param4.typeclass,
      field5 = param5.label   -> param5.typeclass,
      field6 = param6.label   -> param6.typeclass,
      field7 = param7.label   -> param7.typeclass,
      field8 = param8.label   -> param8.typeclass,
      field9 = param9.label   -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
      field11 = param11.label -> param11.typeclass,
      field12 = param12.label -> param12.typeclass,
      field13 = param13.label -> param13.typeclass,
      field14 = param14.label -> param14.typeclass,
      field15 = param15.label -> param15.typeclass,
      field16 = param16.label -> param16.typeclass,
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
      field1 = param1.label   -> param1.typeclass,
      field2 = param2.label   -> param2.typeclass,
      field3 = param3.label   -> param3.typeclass,
      field4 = param4.label   -> param4.typeclass,
      field5 = param5.label   -> param5.typeclass,
      field6 = param6.label   -> param6.typeclass,
      field7 = param7.label   -> param7.typeclass,
      field8 = param8.label   -> param8.typeclass,
      field9 = param9.label   -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
      field11 = param11.label -> param11.typeclass,
      field12 = param12.label -> param12.typeclass,
      field13 = param13.label -> param13.typeclass,
      field14 = param14.label -> param14.typeclass,
      field15 = param15.label -> param15.typeclass,
      field16 = param16.label -> param16.typeclass,
      field17 = param17.label -> param17.typeclass,
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
      field1 = param1.label   -> param1.typeclass,
      field2 = param2.label   -> param2.typeclass,
      field3 = param3.label   -> param3.typeclass,
      field4 = param4.label   -> param4.typeclass,
      field5 = param5.label   -> param5.typeclass,
      field6 = param6.label   -> param6.typeclass,
      field7 = param7.label   -> param7.typeclass,
      field8 = param8.label   -> param8.typeclass,
      field9 = param9.label   -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
      field11 = param11.label -> param11.typeclass,
      field12 = param12.label -> param12.typeclass,
      field13 = param13.label -> param13.typeclass,
      field14 = param14.label -> param14.typeclass,
      field15 = param15.label -> param15.typeclass,
      field16 = param16.label -> param16.typeclass,
      field17 = param17.label -> param17.typeclass,
      field18 = param18.label -> param18.typeclass,
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
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      field3 = param3.label -> param3.typeclass,
      field4 = param4.label -> param4.typeclass,
      field5 = param5.label -> param5.typeclass,
      field6 = param6.label -> param6.typeclass,
      field7 = param7.label -> param7.typeclass,
      field8 = param8.label -> param8.typeclass,
      field9 = param9.label -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
      field11 = param11.label -> param11.typeclass,
      field12 = param12.label -> param12.typeclass,
      field13 = param13.label -> param13.typeclass,
      field14 = param14.label -> param14.typeclass,
      field15 = param15.label -> param15.typeclass,
      field16 = param16.label -> param16.typeclass,
      field17 = param17.label -> param17.typeclass,
      field18 = param18.label -> param18.typeclass,
      field19 = param19.label -> param19.typeclass,
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
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      field3 = param3.label -> param3.typeclass,
      field4 = param4.label -> param4.typeclass,
      field5 = param5.label -> param5.typeclass,
      field6 = param6.label -> param6.typeclass,
      field7 = param7.label -> param7.typeclass,
      field8 = param8.label -> param8.typeclass,
      field9 = param9.label -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
      field11 = param11.label -> param11.typeclass,
      field12 = param12.label -> param12.typeclass,
      field13 = param13.label -> param13.typeclass,
      field14 = param14.label -> param14.typeclass,
      field15 = param15.label -> param15.typeclass,
      field16 = param16.label -> param16.typeclass,
      field17 = param17.label -> param17.typeclass,
      field18 = param18.label -> param18.typeclass,
      field19 = param19.label -> param19.typeclass,
      field20 = param20.label -> param20.typeclass,
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
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      field3 = param3.label -> param3.typeclass,
      field4 = param4.label -> param4.typeclass,
      field5 = param5.label -> param5.typeclass,
      field6 = param6.label -> param6.typeclass,
      field7 = param7.label -> param7.typeclass,
      field8 = param8.label -> param8.typeclass,
      field9 = param9.label -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
      field11 = param11.label -> param11.typeclass,
      field12 = param12.label -> param12.typeclass,
      field13 = param13.label -> param13.typeclass,
      field14 = param14.label -> param14.typeclass,
      field15 = param15.label -> param15.typeclass,
      field16 = param16.label -> param16.typeclass,
      field17 = param17.label -> param17.typeclass,
      field18 = param18.label -> param18.typeclass,
      field19 = param19.label -> param19.typeclass,
      field20 = param20.label -> param20.typeclass,
      field21 = param21.label -> param21.typeclass,
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
      field1 = param1.label -> param1.typeclass,
      field2 = param2.label -> param2.typeclass,
      field3 = param3.label -> param3.typeclass,
      field4 = param4.label -> param4.typeclass,
      field5 = param5.label -> param5.typeclass,
      field6 = param6.label -> param6.typeclass,
      field7 = param7.label -> param7.typeclass,
      field8 = param8.label -> param8.typeclass,
      field9 = param9.label -> param9.typeclass,
      field10 = param10.label -> param10.typeclass,
      field11 = param11.label -> param11.typeclass,
      field12 = param12.label -> param12.typeclass,
      field13 = param13.label -> param13.typeclass,
      field14 = param14.label -> param14.typeclass,
      field15 = param15.label -> param15.typeclass,
      field16 = param16.label -> param16.typeclass,
      field17 = param17.label -> param17.typeclass,
      field18 = param18.label -> param18.typeclass,
      field19 = param19.label -> param19.typeclass,
      field20 = param20.label -> param20.typeclass,
      field21 = param21.label -> param21.typeclass,
      field22 = param22.label -> param22.typeclass,
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
  // format: on

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
