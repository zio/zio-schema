package zio.schema

import zio.Chunk
import zio.random.Random
import zio.schema.Schema._
import zio.test._

import scala.collection.immutable.ListMap

object DeriveGen {

  def gen[A](implicit schema: Schema[A]): Gen[Sized with Random, A] =
    schema match {
      case GenericRecord(structure)           => listMap(structure)(gen(_))
      case Sequence(schemaA, fromChunk, _)    => Gen.chunkOf(gen(schemaA)).map(fromChunk)
      case Schema.Enumeration(structure)      => Gen.oneOf(structure.toList.map { case (k, v) => gen(v).map(k -> _) }: _*)
      case Transform(codec, f, _)             => gen(codec).flatMap(f(_).fold(_ => Gen.empty, Gen.const(_)))
      case Primitive(standardType)            => PrimitiveGen(standardType).map(_.asInstanceOf[A])
      case Optional(codec)                    => gen(codec).map(_.asInstanceOf[A])
      case Fail(_)                            => Gen.empty
      case Tuple(left, right)                 => gen(left).zip(gen(right))
      case EitherSchema(left, right)          => Gen.either(gen(left), gen(right))
      case Enum1(case1)                       => gen(case1.codec)
      case Enum2(case1, case2)                => Gen.oneOf(gen(case1.codec), gen(case2.codec))
      case Enum3(case1, case2, case3)         => Gen.oneOf(gen(case1.codec), gen(case2.codec), gen(case3.codec))
      case enumN: EnumN[a]                    => Gen.oneOf(enumN.cases.map(c => gen(c.codec).map(_.asInstanceOf[a])): _*)
      case CaseObject(instance)               => Gen.const(instance)
      case CaseClass1(_, field, construct, _) => gen(field.schema).map(construct(_))
      case CaseClass2(_, field1, field2, construct, _, _) =>
        gen(field1.schema).zip(gen(field2.schema)).map { case (a1, a2) => construct(a1, a2) }
      case CaseClass3(_, field1, field2, field3, construct, _, _, _) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .map {
            case ((a1, a2), a3) => construct(a1, a2, a3)
          }
      case CaseClass4(
          _,
          field1,
          field2,
          field3,
          field4,
          construct,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .map {
            case (((a1, a2), a3), a4) => construct(a1, a2, a3, a4)
          }

      case CaseClass5(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          construct,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .map {
            case ((((a1, a2), a3), a4), a5) => construct(a1, a2, a3, a4, a5)
          }
      case CaseClass6(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          construct,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .map {
            case (((((a1, a2), a3), a4), a5), a6) => construct(a1, a2, a3, a4, a5, a6)
          }
      case CaseClass7(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .map {
            case ((((((a1, a2), a3), a4), a5), a6), a7) => construct(a1, a2, a3, a4, a5, a6, a7)
          }
      case CaseClass8(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .map {
            case (((((((a1, a2), a3), a4), a5), a6), a7), a8) => construct(a1, a2, a3, a4, a5, a6, a7, a8)
          }
      case CaseClass9(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .map {
            case ((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9) => construct(a1, a2, a3, a4, a5, a6, a7, a8, a9)
          }
      case CaseClass10(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .map {
            case (((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10) =>
              construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
          }
      case CaseClass11(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          field11,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .zip(gen(field11.schema))
          .map {
            case ((((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11) =>
              construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11)
          }
      case CaseClass12(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          field11,
          field12,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .zip(gen(field11.schema))
          .zip(gen(field12.schema))
          .map {
            case (((((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12) =>
              construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12)
          }
      case CaseClass13(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          field11,
          field12,
          field13,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .zip(gen(field11.schema))
          .zip(gen(field12.schema))
          .zip(gen(field13.schema))
          .map {
            case ((((((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13) =>
              construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13)
          }
      case CaseClass14(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          field11,
          field12,
          field13,
          field14,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .zip(gen(field11.schema))
          .zip(gen(field12.schema))
          .zip(gen(field13.schema))
          .zip(gen(field14.schema))
          .map {
            case (((((((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14) =>
              construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14)
          }
      case CaseClass15(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          field11,
          field12,
          field13,
          field14,
          field15,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .zip(gen(field11.schema))
          .zip(gen(field12.schema))
          .zip(gen(field13.schema))
          .zip(gen(field14.schema))
          .zip(gen(field15.schema))
          .map {
            case ((((((((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15) =>
              construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
          }
      case CaseClass16(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          field11,
          field12,
          field13,
          field14,
          field15,
          field16,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .zip(gen(field11.schema))
          .zip(gen(field12.schema))
          .zip(gen(field13.schema))
          .zip(gen(field14.schema))
          .zip(gen(field15.schema))
          .zip(gen(field16.schema))
          .map {
            case (((((((((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16) =>
              construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16)
          }
      case CaseClass17(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          field11,
          field12,
          field13,
          field14,
          field15,
          field16,
          field17,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .zip(gen(field11.schema))
          .zip(gen(field12.schema))
          .zip(gen(field13.schema))
          .zip(gen(field14.schema))
          .zip(gen(field15.schema))
          .zip(gen(field16.schema))
          .zip(gen(field17.schema))
          .map {
            case (
                (((((((((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16),
                a17
                ) =>
              construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17)
          }
      case CaseClass18(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          field11,
          field12,
          field13,
          field14,
          field15,
          field16,
          field17,
          field18,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .zip(gen(field11.schema))
          .zip(gen(field12.schema))
          .zip(gen(field13.schema))
          .zip(gen(field14.schema))
          .zip(gen(field15.schema))
          .zip(gen(field16.schema))
          .zip(gen(field17.schema))
          .zip(gen(field18.schema))
          .map {
            case (
                (
                  (((((((((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16),
                  a17
                ),
                a18
                ) =>
              construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18)
          }
      case CaseClass19(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          field11,
          field12,
          field13,
          field14,
          field15,
          field16,
          field17,
          field18,
          field19,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .zip(gen(field11.schema))
          .zip(gen(field12.schema))
          .zip(gen(field13.schema))
          .zip(gen(field14.schema))
          .zip(gen(field15.schema))
          .zip(gen(field16.schema))
          .zip(gen(field17.schema))
          .zip(gen(field18.schema))
          .zip(gen(field19.schema))
          .map {
            case (
                (
                  (
                    (((((((((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15), a16),
                    a17
                  ),
                  a18
                ),
                a19
                ) =>
              construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19)
          }
      case CaseClass20(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          field11,
          field12,
          field13,
          field14,
          field15,
          field16,
          field17,
          field18,
          field19,
          field20,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .zip(gen(field11.schema))
          .zip(gen(field12.schema))
          .zip(gen(field13.schema))
          .zip(gen(field14.schema))
          .zip(gen(field15.schema))
          .zip(gen(field16.schema))
          .zip(gen(field17.schema))
          .zip(gen(field18.schema))
          .zip(gen(field19.schema))
          .zip(gen(field20.schema))
          .map {
            case (
                (
                  (
                    (
                      (
                        ((((((((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15),
                        a16
                      ),
                      a17
                    ),
                    a18
                  ),
                  a19
                ),
                a20
                ) =>
              construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20)
          }
      case CaseClass21(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          field11,
          field12,
          field13,
          field14,
          field15,
          field16,
          field17,
          field18,
          field19,
          field20,
          field21,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .zip(gen(field11.schema))
          .zip(gen(field12.schema))
          .zip(gen(field13.schema))
          .zip(gen(field14.schema))
          .zip(gen(field15.schema))
          .zip(gen(field16.schema))
          .zip(gen(field17.schema))
          .zip(gen(field18.schema))
          .zip(gen(field19.schema))
          .zip(gen(field20.schema))
          .zip(gen(field21.schema))
          .map {
            case (
                (
                  (
                    (
                      (
                        (
                          ((((((((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14), a15),
                          a16
                        ),
                        a17
                      ),
                      a18
                    ),
                    a19
                  ),
                  a20
                ),
                a21
                ) =>
              construct(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15, a16, a17, a18, a19, a20, a21)
          }
      case CaseClass22(
          _,
          field1,
          field2,
          field3,
          field4,
          field5,
          field6,
          field7,
          field8,
          field9,
          field10,
          field11,
          field12,
          field13,
          field14,
          field15,
          field16,
          field17,
          field18,
          field19,
          field20,
          field21,
          field22,
          construct,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _,
          _
          ) =>
        gen(field1.schema)
          .zip(gen(field2.schema))
          .zip(gen(field3.schema))
          .zip(gen(field4.schema))
          .zip(gen(field5.schema))
          .zip(gen(field6.schema))
          .zip(gen(field7.schema))
          .zip(gen(field8.schema))
          .zip(gen(field9.schema))
          .zip(gen(field10.schema))
          .zip(gen(field11.schema))
          .zip(gen(field12.schema))
          .zip(gen(field13.schema))
          .zip(gen(field14.schema))
          .zip(gen(field15.schema))
          .zip(gen(field16.schema))
          .zip(gen(field17.schema))
          .zip(gen(field18.schema))
          .zip(gen(field19.schema))
          .zip(gen(field20.schema))
          .zip(gen(field21.schema))
          .zip(gen(field22.schema))
          .map {
            case (
                (
                  (
                    (
                      (
                        (
                          (
                            (
                              (((((((((((((a1, a2), a3), a4), a5), a6), a7), a8), a9), a10), a11), a12), a13), a14),
                              a15
                            ),
                            a16
                          ),
                          a17
                        ),
                        a18
                      ),
                      a19
                    ),
                    a20
                  ),
                  a21
                ),
                a22
                ) =>
              construct(
                a1,
                a2,
                a3,
                a4,
                a5,
                a6,
                a7,
                a8,
                a9,
                a10,
                a11,
                a12,
                a13,
                a14,
                a15,
                a16,
                a17,
                a18,
                a19,
                a20,
                a21,
                a22
              )
          }
    }

  private def listMap[R](structure: Chunk[Schema.Field[_]])(gen: Schema[_] => Gen[R, _]): Gen[R, ListMap[String, _]] =
    structure.foldLeft[Gen[R, ListMap[String, _]]](Gen.const(ListMap.empty)) {
      case (genMap, field) =>
        for {
          map   <- genMap
          value <- gen(field.schema)
        } yield map.updated(field.label, value)
    }
}
