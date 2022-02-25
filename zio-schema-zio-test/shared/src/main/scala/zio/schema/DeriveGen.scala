package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.random.Random
import zio.schema.ast.SchemaAst
import zio.test.{ Gen, Sized }

object DeriveGen {

  // scalafmt: { maxColumn = 400 }
  def gen[A](implicit schema: Schema[A]): Gen[Random with Sized, A] =
    schema match {
      case Schema.Enum1(c1, _)                                                                                                                                              => genEnum(c1)
      case Schema.Enum2(c1, c2, _)                                                                                                                                          => genEnum(c1, c2)
      case Schema.Enum3(c1, c2, c3, _)                                                                                                                                      => genEnum(c1, c2, c3)
      case Schema.Enum4(c1, c2, c3, c4, _)                                                                                                                                  => genEnum(c1, c2, c3, c4)
      case Schema.Enum5(c1, c2, c3, c4, c5, _)                                                                                                                              => genEnum(c1, c2, c3, c4, c5)
      case Schema.Enum6(c1, c2, c3, c4, c5, c6, _)                                                                                                                          => genEnum(c1, c2, c3, c4, c5, c6)
      case Schema.Enum7(c1, c2, c3, c4, c5, c6, c7, _)                                                                                                                      => genEnum(c1, c2, c3, c4, c5, c6, c7)
      case Schema.Enum8(c1, c2, c3, c4, c5, c6, c7, c8, _)                                                                                                                  => genEnum(c1, c2, c3, c4, c5, c6, c7, c8)
      case Schema.Enum9(c1, c2, c3, c4, c5, c6, c7, c8, c9, _)                                                                                                              => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9)
      case Schema.Enum10(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _)                                                                                                        => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
      case Schema.Enum11(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _)                                                                                                   => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
      case Schema.Enum12(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _)                                                                                              => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
      case Schema.Enum13(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _)                                                                                         => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
      case Schema.Enum14(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _)                                                                                    => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
      case Schema.Enum15(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _)                                                                               => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
      case Schema.Enum16(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _)                                                                          => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
      case Schema.Enum17(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _)                                                                     => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
      case Schema.Enum18(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _)                                                                => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
      case Schema.Enum19(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _)                                                           => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
      case Schema.Enum20(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _)                                                      => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
      case Schema.Enum21(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _)                                                 => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
      case Schema.Enum22(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _)                                            => genEnum(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
      case Schema.EnumN(caseSet, _)                                                                                                                                         => genEnum(caseSet.toSeq.asInstanceOf[Seq[Schema.Case[_, A]]]: _*)
      case c @ Schema.CaseClass1(_, _, _, _)                                                                                                                                => genCaseClass1(c)
      case c @ Schema.CaseClass2(_, _, _, _, _, _)                                                                                                                          => genCaseClass2(c)
      case c @ Schema.CaseClass3(_, _, _, _, _, _, _, _)                                                                                                                    => genCaseClass3(c)
      case c @ Schema.CaseClass4(_, _, _, _, _, _, _, _, _, _)                                                                                                              => genCaseClass4(c)
      case c @ Schema.CaseClass5(_, _, _, _, _, _, _, _, _, _, _, _)                                                                                                        => genCaseClass5(c)
      case c @ Schema.CaseClass6(_, _, _, _, _, _, _, _, _, _, _, _, _, _)                                                                                                  => genCaseClass6(c)
      case c @ Schema.CaseClass7(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                                                                                            => genCaseClass7(c)
      case c @ Schema.CaseClass8(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                                                                                      => genCaseClass8(c)
      case c @ Schema.CaseClass9(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                                                                                => genCaseClass9(c)
      case c @ Schema.CaseClass10(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                                                                         => genCaseClass10(c)
      case c @ Schema.CaseClass11(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                                                                   => genCaseClass11(c)
      case c @ Schema.CaseClass12(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                                                             => genCaseClass12(c)
      case c @ Schema.CaseClass13(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                                                       => genCaseClass13(c)
      case c @ Schema.CaseClass14(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                                                 => genCaseClass14(c)
      case c @ Schema.CaseClass15(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                                           => genCaseClass15(c)
      case c @ Schema.CaseClass16(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                                     => genCaseClass16(c)
      case c @ Schema.CaseClass17(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                               => genCaseClass17(c)
      case c @ Schema.CaseClass18(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                         => genCaseClass18(c)
      case c @ Schema.CaseClass19(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)                   => genCaseClass19(c)
      case c @ Schema.CaseClass20(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)             => genCaseClass20(c)
      case c @ Schema.CaseClass21(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _)       => genCaseClass21(c)
      case c @ Schema.CaseClass22(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) => genCaseClass22(c)
      case generic @ Schema.GenericRecord(_, _)                                                                                                                             => genGenericRecord(generic).map(_.asInstanceOf[A])
      case seq @ Schema.Sequence(_, _, _, _, _)                                                                                                                             => genSequence(seq)
      case map @ Schema.MapSchema(_, _, _)                                                                                                                                  => genMap(map)
      case set @ Schema.SetSchema(_, _)                                                                                                                                     => genSet(set)
      case transform @ Schema.Transform(_, _, _, _, _)                                                                                                                      => genTransform(transform)
      case Schema.Primitive(standardType, _)                                                                                                                                => genPrimitive(standardType)
      case optional @ Schema.Optional(_, _)                                                                                                                                 => genOptional(optional)
      case fail @ Schema.Fail(_, _)                                                                                                                                         => genFail(fail)
      case tuple @ Schema.Tuple(_, _, _)                                                                                                                                    => genTuple(tuple)
      case either @ Schema.EitherSchema(_, _, _)                                                                                                                            => genEither(either)
      case lazzy @ Schema.Lazy(_)                                                                                                                                           => genLazy(lazzy)
      case Schema.Meta(ast, _)                                                                                                                                              => genMeta(ast)
    } // scalafmt: { maxColumn = 120 }

  private def genEnum[Z](cases: Schema.Case[_, Z]*): Gen[Random with Sized, Z] =
    Gen.elements(cases: _*).flatMap(c => gen(c.codec).map(_.asInstanceOf[Z]))

  private def genCaseClass1[A, Z](caseClass1: Schema.CaseClass1[A, Z]): Gen[Random with Sized, Z] =
    gen(caseClass1.field.schema).map(caseClass1.construct)

  private def genCaseClass2[A1, A2, Z](caseClass2: Schema.CaseClass2[A1, A2, Z]): Gen[Random with Sized, Z] =
    for {
      f1 <- gen(caseClass2.field1.schema)
      f2 <- gen(caseClass2.field2.schema)
    } yield caseClass2.construct(f1, f2)

  private def genCaseClass3[A1, A2, A3, Z](caseClass3: Schema.CaseClass3[A1, A2, A3, Z]): Gen[Random with Sized, Z] =
    for {
      f1 <- gen(caseClass3.field1.schema)
      f2 <- gen(caseClass3.field2.schema)
      f3 <- gen(caseClass3.field3.schema)
    } yield caseClass3.construct(f1, f2, f3)

  private def genCaseClass4[A1, A2, A3, A4, Z](
    caseClass4: Schema.CaseClass4[A1, A2, A3, A4, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1 <- gen(caseClass4.field1.schema)
      f2 <- gen(caseClass4.field2.schema)
      f3 <- gen(caseClass4.field3.schema)
      f4 <- gen(caseClass4.field4.schema)
    } yield caseClass4.construct(f1, f2, f3, f4)

  private def genCaseClass5[A1, A2, A3, A4, A5, Z](
    caseClass5: Schema.CaseClass5[A1, A2, A3, A4, A5, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1 <- gen(caseClass5.field1.schema)
      f2 <- gen(caseClass5.field2.schema)
      f3 <- gen(caseClass5.field3.schema)
      f4 <- gen(caseClass5.field4.schema)
      f5 <- gen(caseClass5.field5.schema)
    } yield caseClass5.construct(f1, f2, f3, f4, f5)

  private def genCaseClass6[A1, A2, A3, A4, A5, A6, Z](
    caseClass6: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1 <- gen(caseClass6.field1.schema)
      f2 <- gen(caseClass6.field2.schema)
      f3 <- gen(caseClass6.field3.schema)
      f4 <- gen(caseClass6.field4.schema)
      f5 <- gen(caseClass6.field5.schema)
      f6 <- gen(caseClass6.field6.schema)
    } yield caseClass6.construct(f1, f2, f3, f4, f5, f6)

  private def genCaseClass7[A1, A2, A3, A4, A5, A6, A7, Z](
    caseClass7: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1 <- gen(caseClass7.field1.schema)
      f2 <- gen(caseClass7.field2.schema)
      f3 <- gen(caseClass7.field3.schema)
      f4 <- gen(caseClass7.field4.schema)
      f5 <- gen(caseClass7.field5.schema)
      f6 <- gen(caseClass7.field6.schema)
      f7 <- gen(caseClass7.field7.schema)
    } yield caseClass7.construct(f1, f2, f3, f4, f5, f6, f7)

  private def genCaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z](
    caseClass8: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1 <- gen(caseClass8.field1.schema)
      f2 <- gen(caseClass8.field2.schema)
      f3 <- gen(caseClass8.field3.schema)
      f4 <- gen(caseClass8.field4.schema)
      f5 <- gen(caseClass8.field5.schema)
      f6 <- gen(caseClass8.field6.schema)
      f7 <- gen(caseClass8.field7.schema)
      f8 <- gen(caseClass8.field8.schema)
    } yield caseClass8.construct(f1, f2, f3, f4, f5, f6, f7, f8)

  private def genCaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](
    caseClass9: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1 <- gen(caseClass9.field1.schema)
      f2 <- gen(caseClass9.field2.schema)
      f3 <- gen(caseClass9.field3.schema)
      f4 <- gen(caseClass9.field4.schema)
      f5 <- gen(caseClass9.field5.schema)
      f6 <- gen(caseClass9.field6.schema)
      f7 <- gen(caseClass9.field7.schema)
      f8 <- gen(caseClass9.field8.schema)
      f9 <- gen(caseClass9.field9.schema)
    } yield caseClass9.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9)

  private def genCaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
    caseClass10: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass10.field1.schema)
      f2  <- gen(caseClass10.field2.schema)
      f3  <- gen(caseClass10.field3.schema)
      f4  <- gen(caseClass10.field4.schema)
      f5  <- gen(caseClass10.field5.schema)
      f6  <- gen(caseClass10.field6.schema)
      f7  <- gen(caseClass10.field7.schema)
      f8  <- gen(caseClass10.field8.schema)
      f9  <- gen(caseClass10.field9.schema)
      f10 <- gen(caseClass10.field10.schema)
    } yield caseClass10.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)

  private def genCaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
    caseClass11: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass11.field1.schema)
      f2  <- gen(caseClass11.field2.schema)
      f3  <- gen(caseClass11.field3.schema)
      f4  <- gen(caseClass11.field4.schema)
      f5  <- gen(caseClass11.field5.schema)
      f6  <- gen(caseClass11.field6.schema)
      f7  <- gen(caseClass11.field7.schema)
      f8  <- gen(caseClass11.field8.schema)
      f9  <- gen(caseClass11.field9.schema)
      f10 <- gen(caseClass11.field10.schema)
      f11 <- gen(caseClass11.field11.schema)
    } yield caseClass11.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)

  private def genCaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
    caseClass12: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass12.field1.schema)
      f2  <- gen(caseClass12.field2.schema)
      f3  <- gen(caseClass12.field3.schema)
      f4  <- gen(caseClass12.field4.schema)
      f5  <- gen(caseClass12.field5.schema)
      f6  <- gen(caseClass12.field6.schema)
      f7  <- gen(caseClass12.field7.schema)
      f8  <- gen(caseClass12.field8.schema)
      f9  <- gen(caseClass12.field9.schema)
      f10 <- gen(caseClass12.field10.schema)
      f11 <- gen(caseClass12.field11.schema)
      f12 <- gen(caseClass12.field12.schema)
    } yield caseClass12.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)

  private def genCaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
    caseClass13: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass13.field1.schema)
      f2  <- gen(caseClass13.field2.schema)
      f3  <- gen(caseClass13.field3.schema)
      f4  <- gen(caseClass13.field4.schema)
      f5  <- gen(caseClass13.field5.schema)
      f6  <- gen(caseClass13.field6.schema)
      f7  <- gen(caseClass13.field7.schema)
      f8  <- gen(caseClass13.field8.schema)
      f9  <- gen(caseClass13.field9.schema)
      f10 <- gen(caseClass13.field10.schema)
      f11 <- gen(caseClass13.field11.schema)
      f12 <- gen(caseClass13.field12.schema)
      f13 <- gen(caseClass13.field13.schema)
    } yield caseClass13.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)

  private def genCaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
    caseClass14: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass14.field1.schema)
      f2  <- gen(caseClass14.field2.schema)
      f3  <- gen(caseClass14.field3.schema)
      f4  <- gen(caseClass14.field4.schema)
      f5  <- gen(caseClass14.field5.schema)
      f6  <- gen(caseClass14.field6.schema)
      f7  <- gen(caseClass14.field7.schema)
      f8  <- gen(caseClass14.field8.schema)
      f9  <- gen(caseClass14.field9.schema)
      f10 <- gen(caseClass14.field10.schema)
      f11 <- gen(caseClass14.field11.schema)
      f12 <- gen(caseClass14.field12.schema)
      f13 <- gen(caseClass14.field13.schema)
      f14 <- gen(caseClass14.field14.schema)
    } yield caseClass14.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)

  private def genCaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
    caseClass15: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass15.field1.schema)
      f2  <- gen(caseClass15.field2.schema)
      f3  <- gen(caseClass15.field3.schema)
      f4  <- gen(caseClass15.field4.schema)
      f5  <- gen(caseClass15.field5.schema)
      f6  <- gen(caseClass15.field6.schema)
      f7  <- gen(caseClass15.field7.schema)
      f8  <- gen(caseClass15.field8.schema)
      f9  <- gen(caseClass15.field9.schema)
      f10 <- gen(caseClass15.field10.schema)
      f11 <- gen(caseClass15.field11.schema)
      f12 <- gen(caseClass15.field12.schema)
      f13 <- gen(caseClass15.field13.schema)
      f14 <- gen(caseClass15.field14.schema)
      f15 <- gen(caseClass15.field15.schema)
    } yield caseClass15.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)

  private def genCaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](
    caseClass16: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass16.field1.schema)
      f2  <- gen(caseClass16.field2.schema)
      f3  <- gen(caseClass16.field3.schema)
      f4  <- gen(caseClass16.field4.schema)
      f5  <- gen(caseClass16.field5.schema)
      f6  <- gen(caseClass16.field6.schema)
      f7  <- gen(caseClass16.field7.schema)
      f8  <- gen(caseClass16.field8.schema)
      f9  <- gen(caseClass16.field9.schema)
      f10 <- gen(caseClass16.field10.schema)
      f11 <- gen(caseClass16.field11.schema)
      f12 <- gen(caseClass16.field12.schema)
      f13 <- gen(caseClass16.field13.schema)
      f14 <- gen(caseClass16.field14.schema)
      f15 <- gen(caseClass16.field15.schema)
      f16 <- gen(caseClass16.field16.schema)
    } yield caseClass16.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)

  private def genCaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](
    caseClass17: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass17.field1.schema)
      f2  <- gen(caseClass17.field2.schema)
      f3  <- gen(caseClass17.field3.schema)
      f4  <- gen(caseClass17.field4.schema)
      f5  <- gen(caseClass17.field5.schema)
      f6  <- gen(caseClass17.field6.schema)
      f7  <- gen(caseClass17.field7.schema)
      f8  <- gen(caseClass17.field8.schema)
      f9  <- gen(caseClass17.field9.schema)
      f10 <- gen(caseClass17.field10.schema)
      f11 <- gen(caseClass17.field11.schema)
      f12 <- gen(caseClass17.field12.schema)
      f13 <- gen(caseClass17.field13.schema)
      f14 <- gen(caseClass17.field14.schema)
      f15 <- gen(caseClass17.field15.schema)
      f16 <- gen(caseClass17.field16.schema)
      f17 <- gen(caseClass17.field17.schema)
    } yield caseClass17.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)

  private def genCaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](
    caseClass18: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass18.field1.schema)
      f2  <- gen(caseClass18.field2.schema)
      f3  <- gen(caseClass18.field3.schema)
      f4  <- gen(caseClass18.field4.schema)
      f5  <- gen(caseClass18.field5.schema)
      f6  <- gen(caseClass18.field6.schema)
      f7  <- gen(caseClass18.field7.schema)
      f8  <- gen(caseClass18.field8.schema)
      f9  <- gen(caseClass18.field9.schema)
      f10 <- gen(caseClass18.field10.schema)
      f11 <- gen(caseClass18.field11.schema)
      f12 <- gen(caseClass18.field12.schema)
      f13 <- gen(caseClass18.field13.schema)
      f14 <- gen(caseClass18.field14.schema)
      f15 <- gen(caseClass18.field15.schema)
      f16 <- gen(caseClass18.field16.schema)
      f17 <- gen(caseClass18.field17.schema)
      f18 <- gen(caseClass18.field18.schema)
    } yield caseClass18.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)

  // scalafmt: { maxColumn = 200 }
  private def genCaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](
    caseClass19: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass19.field1.schema)
      f2  <- gen(caseClass19.field2.schema)
      f3  <- gen(caseClass19.field3.schema)
      f4  <- gen(caseClass19.field4.schema)
      f5  <- gen(caseClass19.field5.schema)
      f6  <- gen(caseClass19.field6.schema)
      f7  <- gen(caseClass19.field7.schema)
      f8  <- gen(caseClass19.field8.schema)
      f9  <- gen(caseClass19.field9.schema)
      f10 <- gen(caseClass19.field10.schema)
      f11 <- gen(caseClass19.field11.schema)
      f12 <- gen(caseClass19.field12.schema)
      f13 <- gen(caseClass19.field13.schema)
      f14 <- gen(caseClass19.field14.schema)
      f15 <- gen(caseClass19.field15.schema)
      f16 <- gen(caseClass19.field16.schema)
      f17 <- gen(caseClass19.field17.schema)
      f18 <- gen(caseClass19.field18.schema)
      f19 <- gen(caseClass19.field19.schema)
    } yield caseClass19.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19)

  private def genCaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](
    caseClass20: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass20.field1.schema)
      f2  <- gen(caseClass20.field2.schema)
      f3  <- gen(caseClass20.field3.schema)
      f4  <- gen(caseClass20.field4.schema)
      f5  <- gen(caseClass20.field5.schema)
      f6  <- gen(caseClass20.field6.schema)
      f7  <- gen(caseClass20.field7.schema)
      f8  <- gen(caseClass20.field8.schema)
      f9  <- gen(caseClass20.field9.schema)
      f10 <- gen(caseClass20.field10.schema)
      f11 <- gen(caseClass20.field11.schema)
      f12 <- gen(caseClass20.field12.schema)
      f13 <- gen(caseClass20.field13.schema)
      f14 <- gen(caseClass20.field14.schema)
      f15 <- gen(caseClass20.field15.schema)
      f16 <- gen(caseClass20.field16.schema)
      f17 <- gen(caseClass20.field17.schema)
      f18 <- gen(caseClass20.field18.schema)
      f19 <- gen(caseClass20.field19.schema)
      f20 <- gen(caseClass20.field20.schema)
    } yield caseClass20.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20)

  private def genCaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](
    caseClass21: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass21.field1.schema)
      f2  <- gen(caseClass21.field2.schema)
      f3  <- gen(caseClass21.field3.schema)
      f4  <- gen(caseClass21.field4.schema)
      f5  <- gen(caseClass21.field5.schema)
      f6  <- gen(caseClass21.field6.schema)
      f7  <- gen(caseClass21.field7.schema)
      f8  <- gen(caseClass21.field8.schema)
      f9  <- gen(caseClass21.field9.schema)
      f10 <- gen(caseClass21.field10.schema)
      f11 <- gen(caseClass21.field11.schema)
      f12 <- gen(caseClass21.field12.schema)
      f13 <- gen(caseClass21.field13.schema)
      f14 <- gen(caseClass21.field14.schema)
      f15 <- gen(caseClass21.field15.schema)
      f16 <- gen(caseClass21.field16.schema)
      f17 <- gen(caseClass21.field17.schema)
      f18 <- gen(caseClass21.field18.schema)
      f19 <- gen(caseClass21.field19.schema)
      f20 <- gen(caseClass21.field20.schema)
      f21 <- gen(caseClass21.field21.schema)
    } yield caseClass21.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21)

  private def genCaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](
    caseClass22: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z]
  ): Gen[Random with Sized, Z] =
    for {
      f1  <- gen(caseClass22.field1.schema)
      f2  <- gen(caseClass22.field2.schema)
      f3  <- gen(caseClass22.field3.schema)
      f4  <- gen(caseClass22.field4.schema)
      f5  <- gen(caseClass22.field5.schema)
      f6  <- gen(caseClass22.field6.schema)
      f7  <- gen(caseClass22.field7.schema)
      f8  <- gen(caseClass22.field8.schema)
      f9  <- gen(caseClass22.field9.schema)
      f10 <- gen(caseClass22.field10.schema)
      f11 <- gen(caseClass22.field11.schema)
      f12 <- gen(caseClass22.field12.schema)
      f13 <- gen(caseClass22.field13.schema)
      f14 <- gen(caseClass22.field14.schema)
      f15 <- gen(caseClass22.field15.schema)
      f16 <- gen(caseClass22.field16.schema)
      f17 <- gen(caseClass22.field17.schema)
      f18 <- gen(caseClass22.field18.schema)
      f19 <- gen(caseClass22.field19.schema)
      f20 <- gen(caseClass22.field20.schema)
      f21 <- gen(caseClass22.field21.schema)
      f22 <- gen(caseClass22.field22.schema)
    } yield caseClass22.construct(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22)
  // scalafmt: { maxColumn = 120 }

  private def genGenericRecord(record: Schema.GenericRecord): Gen[Random with Sized, ListMap[String, _]] =
    record.structure
      .foldLeft[Gen[Random with Sized, ListMap[String, _]]](Gen.const(ListMap.empty)) {
        case (genListMap, field) =>
          for {
            listMap <- genListMap
            value   <- gen(field.schema)
          } yield listMap.updated(field.label, value)
      }

  private def genSequence[Z, A](seq: Schema.Sequence[Z, A, _]): Gen[Random with Sized, Z] =
    Gen.oneOf(Gen.chunkOfN(2)(gen(seq.schemaA)), Gen.const(Chunk.empty)).map(seq.fromChunk(_))

  private def genMap[K, V](map: Schema.MapSchema[K, V]): Gen[Random with Sized, Map[K, V]] =
    Gen.oneOf(Gen.mapOfN(2)(gen(map.ks), gen(map.vs)), Gen.const(Map.empty[K, V]))

  private def genSet[A](set: Schema.SetSchema[A]): Gen[Random with Sized, Set[A]] =
    Gen.oneOf(Gen.setOf(gen(set.as)), Gen.const(Set.empty[A]))

  private def genTransform[A, B, I](transform: Schema.Transform[A, B, I]): Gen[Random with Sized, B] =
    gen(transform.codec).flatMap(a => transform.f(a).fold(_ => Gen.empty, (b: B) => Gen.const(b)))

  def genPrimitive[A](standardType: StandardType[A]): Gen[Random with Sized, A] = {
    val gen = standardType match {
      case StandardType.UnitType              => Gen.unit
      case StandardType.StringType            => Gen.anyString
      case StandardType.BoolType              => Gen.boolean
      case StandardType.ShortType             => Gen.anyShort
      case StandardType.IntType               => Gen.anyInt
      case StandardType.LongType              => Gen.anyLong
      case StandardType.FloatType             => Gen.anyFloat
      case StandardType.DoubleType            => Gen.anyDouble
      case StandardType.BinaryType            => Gen.chunkOf1(Gen.anyByte).map(_.toChunk)
      case StandardType.CharType              => Gen.anyChar
      case StandardType.UUIDType              => Gen.anyUUID
      case StandardType.BigDecimalType        => Gen.anyDouble.map(new java.math.BigDecimal(_))
      case StandardType.BigIntegerType        => Gen.anyLong.map(java.math.BigInteger.valueOf(_))
      case StandardType.DayOfWeekType         => Gen.anyDayOfWeek
      case StandardType.MonthType             => Gen.anyMonth
      case StandardType.MonthDayType          => Gen.anyMonthDay
      case StandardType.PeriodType            => Gen.anyPeriod
      case StandardType.YearType              => Gen.anyYear
      case StandardType.YearMonthType         => Gen.anyYearMonth
      case StandardType.ZoneIdType            => Gen.anyZoneId
      case StandardType.ZoneOffsetType        => Gen.anyZoneOffset
      case StandardType.Duration(_)           => Gen.anyFiniteDuration
      case StandardType.InstantType(_)        => Gen.anyInstant
      case StandardType.LocalDateType(_)      => Gen.anyLocalDate
      case StandardType.LocalTimeType(_)      => Gen.anyLocalTime
      case StandardType.LocalDateTimeType(_)  => Gen.anyLocalDateTime
      case StandardType.OffsetTimeType(_)     => Gen.anyOffsetTime
      case StandardType.OffsetDateTimeType(_) => Gen.anyOffsetDateTime
      case StandardType.ZonedDateTimeType(_)  => Gen.anyZonedDateTime
    }

    gen.map(_.asInstanceOf[A])
  }

  private def genOptional[A](optional: Schema.Optional[A]): Gen[Random with Sized, Option[A]] =
    Gen.option(gen(optional.codec))

  private def genFail[A](fail: Schema.Fail[A]): Gen[Random with Sized, A] = {
    val _ = fail
    Gen.empty
  }

  private def genTuple[A, B](tuple: Schema.Tuple[A, B]): Gen[Random with Sized, (A, B)] =
    gen(tuple.left).zip(gen(tuple.right))

  private def genEither[A, B](either: Schema.EitherSchema[A, B]): Gen[Random with Sized, Either[A, B]] =
    Gen.either(gen(either.left), gen(either.right))

  private def genLazy[A](lazySchema: Schema.Lazy[A]): Gen[Random with Sized, A] =
    Gen.suspend(gen(lazySchema.schema))

  private def genMeta[A](ast: SchemaAst): Gen[Random with Sized, A] =
    gen(ast.toSchema).map(_.asInstanceOf[A])
}
