package zio.schema

import java.time.temporal.ChronoUnit

import zio.Chunk

sealed trait Schema[A] {
  self =>
  def ? : Schema[Option[A]] = Schema.Optional(self)

  def toGeneric(value: A): Generic =
    Generic.fromSchemaAndValue(self, value)

  def fromGeneric(generic: Generic): Either[String, A] =
    generic.toTypedValue(self)

  def transform[B](f: A => B, g: B => A): Schema[B] =
    Schema.Transform[A, B](self, a => Right(f(a)), b => Right(g(b)))

  def transformOrFail[B](f: A => Either[String, B], g: B => Either[String, A]): Schema[B] =
    Schema.Transform[A, B](self, f, g)

  def zip[B](that: Schema[B]): Schema[(A, B)] = Schema.Tuple(self, that)

}

object Schema {

  sealed case class Record(structure: Map[String, Schema[_]]) extends Schema[Map[String, _]]

  final case class Sequence[Col[_], A](schemaA: Schema[A], fromChunk: Chunk[A] => Col[A], toChunk: Col[A] => Chunk[A])
      extends Schema[Col[A]]

  sealed case class Enumeration(structure: Map[String, Schema[_]]) extends Schema[Map[String, _]]

  sealed case class Transform[A, B](codec: Schema[A], f: A => Either[String, B], g: B => Either[String, A])
      extends Schema[B]

  sealed case class Primitive[A](standardType: StandardType[A]) extends Schema[A]

  sealed case class Optional[A](codec: Schema[A]) extends Schema[Option[A]]

  final case class Fail[A](message: String) extends Schema[A]

  sealed case class Tuple[A, B](left: Schema[A], right: Schema[B]) extends Schema[(A, B)]

  final case class EitherSchema[A, B](left: Schema[A], right: Schema[B]) extends Schema[Either[A, B]]

  final case class Case[A <: Z, Z](id: String, codec: Schema[A], unsafeDeconstruct: Z => A) {

    def deconstruct(z: Z): Option[A] =
      try {
        Some(unsafeDeconstruct(z))
      } catch { case _: IllegalArgumentException => None }
  }

  final case class Enum1[A <: Z, Z](case1: Case[A, Z])                                extends Schema[Z]
  final case class Enum2[A1 <: Z, A2 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z]) extends Schema[Z]
  final case class Enum3[A1 <: Z, A2 <: Z, A3 <: Z, Z](case1: Case[A1, Z], case2: Case[A2, Z], case3: Case[A3, Z])
      extends Schema[Z]
  final case class EnumN[Z](cases: Seq[Case[_, Z]]) extends Schema[Z]

  sealed trait CaseClass[Z] extends Schema[Z] {
    def toRecord: Record
  }

  final case class CaseClass1[A, Z](field: (String, Schema[A]), construct: A => Z, extractField: Z => A)
      extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field))
  }

  final case class CaseClass2[A1, A2, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    construct: (A1, A2) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2))
  }

  final case class CaseClass3[A1, A2, A3, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    construct: (A1, A2, A3) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3))
  }

  final case class CaseClass4[A1, A2, A3, A4, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    construct: (A1, A2, A3, A4) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3, field4))
  }

  final case class CaseClass5[A1, A2, A3, A4, A5, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    construct: (A1, A2, A3, A4, A5) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3, field4, field5))
  }

  final case class CaseClass6[A1, A2, A3, A4, A5, A6, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    field6: (String, Schema[A6]),
    construct: (A1, A2, A3, A4, A5, A6) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3, field4, field5, field6))
  }

  final case class CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    field6: (String, Schema[A6]),
    field7: (String, Schema[A7]),
    construct: (A1, A2, A3, A4, A5, A6, A7) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3, field4, field5, field6, field7))
  }

  final case class CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    field6: (String, Schema[A6]),
    field7: (String, Schema[A7]),
    field8: (String, Schema[A8]),
    construct: (A1, A2, A3, A4, A5, A6, A7, A8) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3, field4, field5, field6, field7, field8))
  }

  final case class CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    field6: (String, Schema[A6]),
    field7: (String, Schema[A7]),
    field8: (String, Schema[A8]),
    field9: (String, Schema[A9]),
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9))
  }

  final case class CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    field6: (String, Schema[A6]),
    field7: (String, Schema[A7]),
    field8: (String, Schema[A8]),
    field9: (String, Schema[A9]),
    field10: (String, Schema[A10]),
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10))
  }

  final case class CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    field6: (String, Schema[A6]),
    field7: (String, Schema[A7]),
    field8: (String, Schema[A8]),
    field9: (String, Schema[A9]),
    field10: (String, Schema[A10]),
    field11: (String, Schema[A11]),
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11))
  }

  final case class CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    field6: (String, Schema[A6]),
    field7: (String, Schema[A7]),
    field8: (String, Schema[A8]),
    field9: (String, Schema[A9]),
    field10: (String, Schema[A10]),
    field11: (String, Schema[A11]),
    field12: (String, Schema[A12]),
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12))
  }

  final case class CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    field6: (String, Schema[A6]),
    field7: (String, Schema[A7]),
    field8: (String, Schema[A8]),
    field9: (String, Schema[A9]),
    field10: (String, Schema[A10]),
    field11: (String, Schema[A11]),
    field12: (String, Schema[A12]),
    field13: (String, Schema[A13]),
    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => Z,
    extractField1: Z => A1,
    extractField2: Z => A2,
    extractField3: Z => A3,
    extractField4: Z => A4,
    extractField5: Z => A5,
    extractField6: Z => A6,
    extractField7: Z => A7,
    extractField8: Z => A8,
    extractField9: Z => A9,
    extractField10: Z => A10,
    extractField11: Z => A11,
    extractField12: Z => A12,
    extractField13: Z => A13
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(
        Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13)
      )
  }

  // format: off
  final case class CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
                                                                                                field1: (String, Schema[A1]),
                                                                                                field2: (String, Schema[A2]),
                                                                                                field3: (String, Schema[A3]),
                                                                                                field4: (String, Schema[A4]),
                                                                                                field5: (String, Schema[A5]),
                                                                                                field6: (String, Schema[A6]),
                                                                                                field7: (String, Schema[A7]),
                                                                                                field8: (String, Schema[A8]),
                                                                                                field9: (String, Schema[A9]),
                                                                                                field10: (String, Schema[A10]),
                                                                                                field11: (String, Schema[A11]),
                                                                                                field12: (String, Schema[A12]),
                                                                                                field13: (String, Schema[A13]),
                                                                                                field14: (String, Schema[A14]),
                                                                                                construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => Z,
                                                                                                extractField1: Z => A1,
                                                                                                extractField2: Z => A2,
                                                                                                extractField3: Z => A3,
                                                                                                extractField4: Z => A4,
                                                                                                extractField5: Z => A5,
                                                                                                extractField6: Z => A6,
                                                                                                extractField7: Z => A7,
                                                                                                extractField8: Z => A8,
                                                                                                extractField9: Z => A9,
                                                                                                extractField10: Z => A10,
                                                                                                extractField11: Z => A11,
                                                                                                extractField12: Z => A12,
                                                                                                extractField13: Z => A13,
                                                                                                extractField14: Z => A14
                                                                                              ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14))
  }

  final case class CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
                                                                                                     field1: (String, Schema[A1]),
                                                                                                     field2: (String, Schema[A2]),
                                                                                                     field3: (String, Schema[A3]),
                                                                                                     field4: (String, Schema[A4]),
                                                                                                     field5: (String, Schema[A5]),
                                                                                                     field6: (String, Schema[A6]),
                                                                                                     field7: (String, Schema[A7]),
                                                                                                     field8: (String, Schema[A8]),
                                                                                                     field9: (String, Schema[A9]),
                                                                                                     field10: (String, Schema[A10]),
                                                                                                     field11: (String, Schema[A11]),
                                                                                                     field12: (String, Schema[A12]),
                                                                                                     field13: (String, Schema[A13]),
                                                                                                     field14: (String, Schema[A14]),
                                                                                                     field15: (String, Schema[A15]),
                                                                                                     construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => Z,
                                                                                                     extractField1: Z => A1,
                                                                                                     extractField2: Z => A2,
                                                                                                     extractField3: Z => A3,
                                                                                                     extractField4: Z => A4,
                                                                                                     extractField5: Z => A5,
                                                                                                     extractField6: Z => A6,
                                                                                                     extractField7: Z => A7,
                                                                                                     extractField8: Z => A8,
                                                                                                     extractField9: Z => A9,
                                                                                                     extractField10: Z => A10,
                                                                                                     extractField11: Z => A11,
                                                                                                     extractField12: Z => A12,
                                                                                                     extractField13: Z => A13,
                                                                                                     extractField14: Z => A14,
                                                                                                     extractField15: Z => A15
                                                                                                   ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15))
  }

  final case class CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](
                                                                                                          field1: (String, Schema[A1]),
                                                                                                          field2: (String, Schema[A2]),
                                                                                                          field3: (String, Schema[A3]),
                                                                                                          field4: (String, Schema[A4]),
                                                                                                          field5: (String, Schema[A5]),
                                                                                                          field6: (String, Schema[A6]),
                                                                                                          field7: (String, Schema[A7]),
                                                                                                          field8: (String, Schema[A8]),
                                                                                                          field9: (String, Schema[A9]),
                                                                                                          field10: (String, Schema[A10]),
                                                                                                          field11: (String, Schema[A11]),
                                                                                                          field12: (String, Schema[A12]),
                                                                                                          field13: (String, Schema[A13]),
                                                                                                          field14: (String, Schema[A14]),
                                                                                                          field15: (String, Schema[A15]),
                                                                                                          field16: (String, Schema[A16]),
                                                                                                          construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => Z,
                                                                                                          extractField1: Z => A1,
                                                                                                          extractField2: Z => A2,
                                                                                                          extractField3: Z => A3,
                                                                                                          extractField4: Z => A4,
                                                                                                          extractField5: Z => A5,
                                                                                                          extractField6: Z => A6,
                                                                                                          extractField7: Z => A7,
                                                                                                          extractField8: Z => A8,
                                                                                                          extractField9: Z => A9,
                                                                                                          extractField10: Z => A10,
                                                                                                          extractField11: Z => A11,
                                                                                                          extractField12: Z => A12,
                                                                                                          extractField13: Z => A13,
                                                                                                          extractField14: Z => A14,
                                                                                                          extractField15: Z => A15,
                                                                                                          extractField16: Z => A16
                                                                                                        ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16))
  }

  final case class CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](
                                                                                                               field1: (String, Schema[A1]),
                                                                                                               field2: (String, Schema[A2]),
                                                                                                               field3: (String, Schema[A3]),
                                                                                                               field4: (String, Schema[A4]),
                                                                                                               field5: (String, Schema[A5]),
                                                                                                               field6: (String, Schema[A6]),
                                                                                                               field7: (String, Schema[A7]),
                                                                                                               field8: (String, Schema[A8]),
                                                                                                               field9: (String, Schema[A9]),
                                                                                                               field10: (String, Schema[A10]),
                                                                                                               field11: (String, Schema[A11]),
                                                                                                               field12: (String, Schema[A12]),
                                                                                                               field13: (String, Schema[A13]),
                                                                                                               field14: (String, Schema[A14]),
                                                                                                               field15: (String, Schema[A15]),
                                                                                                               field16: (String, Schema[A16]),
                                                                                                               field17: (String, Schema[A17]),
                                                                                                               construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => Z,
                                                                                                               extractField1: Z => A1,
                                                                                                               extractField2: Z => A2,
                                                                                                               extractField3: Z => A3,
                                                                                                               extractField4: Z => A4,
                                                                                                               extractField5: Z => A5,
                                                                                                               extractField6: Z => A6,
                                                                                                               extractField7: Z => A7,
                                                                                                               extractField8: Z => A8,
                                                                                                               extractField9: Z => A9,
                                                                                                               extractField10: Z => A10,
                                                                                                               extractField11: Z => A11,
                                                                                                               extractField12: Z => A12,
                                                                                                               extractField13: Z => A13,
                                                                                                               extractField14: Z => A14,
                                                                                                               extractField15: Z => A15,
                                                                                                               extractField16: Z => A16,
                                                                                                               extractField17: Z => A17
                                                                                                             ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17))
  }

  final case class CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](
                                                                                                                    field1: (String, Schema[A1]),
                                                                                                                    field2: (String, Schema[A2]),
                                                                                                                    field3: (String, Schema[A3]),
                                                                                                                    field4: (String, Schema[A4]),
                                                                                                                    field5: (String, Schema[A5]),
                                                                                                                    field6: (String, Schema[A6]),
                                                                                                                    field7: (String, Schema[A7]),
                                                                                                                    field8: (String, Schema[A8]),
                                                                                                                    field9: (String, Schema[A9]),
                                                                                                                    field10: (String, Schema[A10]),
                                                                                                                    field11: (String, Schema[A11]),
                                                                                                                    field12: (String, Schema[A12]),
                                                                                                                    field13: (String, Schema[A13]),
                                                                                                                    field14: (String, Schema[A14]),
                                                                                                                    field15: (String, Schema[A15]),
                                                                                                                    field16: (String, Schema[A16]),
                                                                                                                    field17: (String, Schema[A17]),
                                                                                                                    field18: (String, Schema[A18]),
                                                                                                                    construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => Z,
                                                                                                                    extractField1: Z => A1,
                                                                                                                    extractField2: Z => A2,
                                                                                                                    extractField3: Z => A3,
                                                                                                                    extractField4: Z => A4,
                                                                                                                    extractField5: Z => A5,
                                                                                                                    extractField6: Z => A6,
                                                                                                                    extractField7: Z => A7,
                                                                                                                    extractField8: Z => A8,
                                                                                                                    extractField9: Z => A9,
                                                                                                                    extractField10: Z => A10,
                                                                                                                    extractField11: Z => A11,
                                                                                                                    extractField12: Z => A12,
                                                                                                                    extractField13: Z => A13,
                                                                                                                    extractField14: Z => A14,
                                                                                                                    extractField15: Z => A15,
                                                                                                                    extractField16: Z => A16,
                                                                                                                    extractField17: Z => A17,
                                                                                                                    extractField18: Z => A18
                                                                                                                  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18))
  }

  final case class CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](
                                                                                                                         field1: (String, Schema[A1]),
                                                                                                                         field2: (String, Schema[A2]),
                                                                                                                         field3: (String, Schema[A3]),
                                                                                                                         field4: (String, Schema[A4]),
                                                                                                                         field5: (String, Schema[A5]),
                                                                                                                         field6: (String, Schema[A6]),
                                                                                                                         field7: (String, Schema[A7]),
                                                                                                                         field8: (String, Schema[A8]),
                                                                                                                         field9: (String, Schema[A9]),
                                                                                                                         field10: (String, Schema[A10]),
                                                                                                                         field11: (String, Schema[A11]),
                                                                                                                         field12: (String, Schema[A12]),
                                                                                                                         field13: (String, Schema[A13]),
                                                                                                                         field14: (String, Schema[A14]),
                                                                                                                         field15: (String, Schema[A15]),
                                                                                                                         field16: (String, Schema[A16]),
                                                                                                                         field17: (String, Schema[A17]),
                                                                                                                         field18: (String, Schema[A18]),
                                                                                                                         field19: (String, Schema[A19]),
                                                                                                                         construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => Z,
                                                                                                                         extractField1: Z => A1,
                                                                                                                         extractField2: Z => A2,
                                                                                                                         extractField3: Z => A3,
                                                                                                                         extractField4: Z => A4,
                                                                                                                         extractField5: Z => A5,
                                                                                                                         extractField6: Z => A6,
                                                                                                                         extractField7: Z => A7,
                                                                                                                         extractField8: Z => A8,
                                                                                                                         extractField9: Z => A9,
                                                                                                                         extractField10: Z => A10,
                                                                                                                         extractField11: Z => A11,
                                                                                                                         extractField12: Z => A12,
                                                                                                                         extractField13: Z => A13,
                                                                                                                         extractField14: Z => A14,
                                                                                                                         extractField15: Z => A15,
                                                                                                                         extractField16: Z => A16,
                                                                                                                         extractField17: Z => A17,
                                                                                                                         extractField18: Z => A18,
                                                                                                                         extractField19: Z => A19
                                                                                                                       ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18, field19))
  }

  final case class CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](
                                                                                                                              field1: (String, Schema[A1]),
                                                                                                                              field2: (String, Schema[A2]),
                                                                                                                              field3: (String, Schema[A3]),
                                                                                                                              field4: (String, Schema[A4]),
                                                                                                                              field5: (String, Schema[A5]),
                                                                                                                              field6: (String, Schema[A6]),
                                                                                                                              field7: (String, Schema[A7]),
                                                                                                                              field8: (String, Schema[A8]),
                                                                                                                              field9: (String, Schema[A9]),
                                                                                                                              field10: (String, Schema[A10]),
                                                                                                                              field11: (String, Schema[A11]),
                                                                                                                              field12: (String, Schema[A12]),
                                                                                                                              field13: (String, Schema[A13]),
                                                                                                                              field14: (String, Schema[A14]),
                                                                                                                              field15: (String, Schema[A15]),
                                                                                                                              field16: (String, Schema[A16]),
                                                                                                                              field17: (String, Schema[A17]),
                                                                                                                              field18: (String, Schema[A18]),
                                                                                                                              field19: (String, Schema[A19]),
                                                                                                                              field20: (String, Schema[A20]),
                                                                                                                              construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => Z,
                                                                                                                              extractField1: Z => A1,
                                                                                                                              extractField2: Z => A2,
                                                                                                                              extractField3: Z => A3,
                                                                                                                              extractField4: Z => A4,
                                                                                                                              extractField5: Z => A5,
                                                                                                                              extractField6: Z => A6,
                                                                                                                              extractField7: Z => A7,
                                                                                                                              extractField8: Z => A8,
                                                                                                                              extractField9: Z => A9,
                                                                                                                              extractField10: Z => A10,
                                                                                                                              extractField11: Z => A11,
                                                                                                                              extractField12: Z => A12,
                                                                                                                              extractField13: Z => A13,
                                                                                                                              extractField14: Z => A14,
                                                                                                                              extractField15: Z => A15,
                                                                                                                              extractField16: Z => A16,
                                                                                                                              extractField17: Z => A17,
                                                                                                                              extractField18: Z => A18,
                                                                                                                              extractField19: Z => A19,
                                                                                                                              extractField20: Z => A20
                                                                                                                            ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18, field19, field20))
  }

  final case class CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](
                                                                                                                                   field1: (String, Schema[A1]),
                                                                                                                                   field2: (String, Schema[A2]),
                                                                                                                                   field3: (String, Schema[A3]),
                                                                                                                                   field4: (String, Schema[A4]),
                                                                                                                                   field5: (String, Schema[A5]),
                                                                                                                                   field6: (String, Schema[A6]),
                                                                                                                                   field7: (String, Schema[A7]),
                                                                                                                                   field8: (String, Schema[A8]),
                                                                                                                                   field9: (String, Schema[A9]),
                                                                                                                                   field10: (String, Schema[A10]),
                                                                                                                                   field11: (String, Schema[A11]),
                                                                                                                                   field12: (String, Schema[A12]),
                                                                                                                                   field13: (String, Schema[A13]),
                                                                                                                                   field14: (String, Schema[A14]),
                                                                                                                                   field15: (String, Schema[A15]),
                                                                                                                                   field16: (String, Schema[A16]),
                                                                                                                                   field17: (String, Schema[A17]),
                                                                                                                                   field18: (String, Schema[A18]),
                                                                                                                                   field19: (String, Schema[A19]),
                                                                                                                                   field20: (String, Schema[A20]),
                                                                                                                                   field21: (String, Schema[A21]),
                                                                                                                                   construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => Z,
                                                                                                                                   extractField1: Z => A1,
                                                                                                                                   extractField2: Z => A2,
                                                                                                                                   extractField3: Z => A3,
                                                                                                                                   extractField4: Z => A4,
                                                                                                                                   extractField5: Z => A5,
                                                                                                                                   extractField6: Z => A6,
                                                                                                                                   extractField7: Z => A7,
                                                                                                                                   extractField8: Z => A8,
                                                                                                                                   extractField9: Z => A9,
                                                                                                                                   extractField10: Z => A10,
                                                                                                                                   extractField11: Z => A11,
                                                                                                                                   extractField12: Z => A12,
                                                                                                                                   extractField13: Z => A13,
                                                                                                                                   extractField14: Z => A14,
                                                                                                                                   extractField15: Z => A15,
                                                                                                                                   extractField16: Z => A16,
                                                                                                                                   extractField17: Z => A17,
                                                                                                                                   extractField18: Z => A18,
                                                                                                                                   extractField19: Z => A19,
                                                                                                                                   extractField20: Z => A20,
                                                                                                                                   extractField21: Z => A21
                                                                                                                                 ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18, field19, field20, field21))
  }

  final case class CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](
                                                                                                                                        field1: (String, Schema[A1]),
                                                                                                                                        field2: (String, Schema[A2]),
                                                                                                                                        field3: (String, Schema[A3]),
                                                                                                                                        field4: (String, Schema[A4]),
                                                                                                                                        field5: (String, Schema[A5]),
                                                                                                                                        field6: (String, Schema[A6]),
                                                                                                                                        field7: (String, Schema[A7]),
                                                                                                                                        field8: (String, Schema[A8]),
                                                                                                                                        field9: (String, Schema[A9]),
                                                                                                                                        field10: (String, Schema[A10]),
                                                                                                                                        field11: (String, Schema[A11]),
                                                                                                                                        field12: (String, Schema[A12]),
                                                                                                                                        field13: (String, Schema[A13]),
                                                                                                                                        field14: (String, Schema[A14]),
                                                                                                                                        field15: (String, Schema[A15]),
                                                                                                                                        field16: (String, Schema[A16]),
                                                                                                                                        field17: (String, Schema[A17]),
                                                                                                                                        field18: (String, Schema[A18]),
                                                                                                                                        field19: (String, Schema[A19]),
                                                                                                                                        field20: (String, Schema[A20]),
                                                                                                                                        field21: (String, Schema[A21]),
                                                                                                                                        field22: (String, Schema[A22]),
                                                                                                                                        construct: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => Z,
                                                                                                                                        extractField1: Z => A1,
                                                                                                                                        extractField2: Z => A2,
                                                                                                                                        extractField3: Z => A3,
                                                                                                                                        extractField4: Z => A4,
                                                                                                                                        extractField5: Z => A5,
                                                                                                                                        extractField6: Z => A6,
                                                                                                                                        extractField7: Z => A7,
                                                                                                                                        extractField8: Z => A8,
                                                                                                                                        extractField9: Z => A9,
                                                                                                                                        extractField10: Z => A10,
                                                                                                                                        extractField11: Z => A11,
                                                                                                                                        extractField12: Z => A12,
                                                                                                                                        extractField13: Z => A13,
                                                                                                                                        extractField14: Z => A14,
                                                                                                                                        extractField15: Z => A15,
                                                                                                                                        extractField16: Z => A16,
                                                                                                                                        extractField17: Z => A17,
                                                                                                                                        extractField18: Z => A18,
                                                                                                                                        extractField19: Z => A19,
                                                                                                                                        extractField20: Z => A20,
                                                                                                                                        extractField21: Z => A21,
                                                                                                                                        extractField22: Z => A22
                                                                                                                                      ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18, field19, field20, field21, field22))
  }

  // format: on

  def fail[A](message: String): Schema[A] = Fail(message)

  def apply[A](implicit codec: Schema[A]): Schema[A] = codec

  implicit val bigInt: Schema[BigInt]         = primitive[java.math.BigInteger].transform(BigInt(_), _.bigInteger)
  implicit val bigDecimal: Schema[BigDecimal] = primitive[java.math.BigDecimal].transform(BigDecimal(_), _.bigDecimal)
  implicit val nilSchema: Schema[Nil.type]    = Schema[Unit].transform(_ => Nil, _ => ())
  implicit val noneSchema: Schema[None.type]  = Schema[Unit].transform(_ => None, _ => ())

  implicit val chronoUnitSchema: Schema[ChronoUnit] = Schema[String].transformOrFail(
    {
      case "SECONDS"   => Right(ChronoUnit.SECONDS)
      case "CENTURIES" => Right(ChronoUnit.CENTURIES)
      case "DAYS"      => Right(ChronoUnit.DAYS)
      case "DECADES"   => Right(ChronoUnit.DECADES)
      case "FOREVER"   => Right(ChronoUnit.FOREVER)
      case "HOURS"     => Right(ChronoUnit.HOURS)
      case "MICROS"    => Right(ChronoUnit.MICROS)
      case "MILLIS"    => Right(ChronoUnit.MILLIS)
      case "MINUTES"   => Right(ChronoUnit.MINUTES)
      case "MONTHS"    => Right(ChronoUnit.MONTHS)
      case "NANOS"     => Right(ChronoUnit.NANOS)
      case "WEEKS"     => Right(ChronoUnit.WEEKS)
      case "YEARS"     => Right(ChronoUnit.YEARS)
      case _           => Left("Failed")
    }, {
      case ChronoUnit.SECONDS   => Right("SECONDS")
      case ChronoUnit.CENTURIES => Right("CENTURIES")
      case ChronoUnit.DAYS      => Right("DAYS")
      case ChronoUnit.DECADES   => Right("DECADES")
      case ChronoUnit.FOREVER   => Right("FOREVER")
      case ChronoUnit.HOURS     => Right("HOURS")
      case ChronoUnit.MICROS    => Right("MICROS")
      case ChronoUnit.MILLIS    => Right("MILLIS")
      case ChronoUnit.MINUTES   => Right("MINUTES")
      case ChronoUnit.MONTHS    => Right("MONTHS")
      case ChronoUnit.NANOS     => Right("NANOS")
      case ChronoUnit.WEEKS     => Right("WEEKS")
      case ChronoUnit.YEARS     => Right("YEARS")
      case _                    => Left("Failed")
    }
  )

  implicit def schemaList[A](implicit schemaA: Schema[A]): Schema[List[A]] =
    Schema.Sequence(schemaA, _.toList, Chunk.fromIterable(_))

  implicit def schemaChunk[A](implicit schemaA: Schema[A]): Schema[Chunk[A]] =
    Schema.Sequence(schemaA, identity, identity)

  implicit def leftSchema[A, B](implicit schemaA: Schema[A]): Schema[Left[A, Nothing]] =
    schemaA.transform(Left(_), _.value)

  implicit def rightSchema[A, B](implicit schemaB: Schema[B]): Schema[Right[Nothing, B]] =
    schemaB.transform(Right(_), _.value)

  def either[A, B](left: Schema[A], right: Schema[B]): Schema[Either[A, B]] =
    EitherSchema(left, right)

  def tuple[A, B](left: Schema[A], right: Schema[B]): Schema[(A, B)] =
    Tuple(left, right)

  def enumeration(structure: Map[String, Schema[_]]): Schema[Map[String, _]] =
    Enumeration(structure)

  def first[A](codec: Schema[(A, Unit)]): Schema[A] =
    codec.transform[A](_._1, a => (a, ()))

  implicit def option[A](implicit element: Schema[A]): Schema[Option[A]] =
    Optional(element)

  implicit def primitive[A](implicit standardType: StandardType[A]): Schema[A] =
    Primitive(standardType)

  def record(structure: Map[String, Schema[_]]): Schema[Map[String, _]] =
    Record(structure)

  implicit def sequence[A](implicit element: Schema[A]): Schema[Chunk[A]] =
    Sequence(element, (c: Chunk[A]) => c, (c: Chunk[A]) => c)

  implicit def set[A](implicit element: Schema[A]): Schema[Set[A]] =
    sequence(element).transform(_.toSet, Chunk.fromIterable(_))

  def second[A](codec: Schema[(Unit, A)]): Schema[A] =
    codec.transform[A](_._2, a => ((), a))

  implicit def vector[A](implicit element: Schema[A]): Schema[Vector[A]] =
    sequence(element).transform(_.toVector, Chunk.fromIterable(_))

  implicit def zip2[A, B](implicit c1: Schema[A], c2: Schema[B]): Schema[(A, B)] =
    c1.zip(c2)

  implicit def zip3[A, B, C](implicit c1: Schema[A], c2: Schema[B], c3: Schema[C]): Schema[(A, B, C)] =
    c1.zip(c2).zip(c3).transform({ case ((a, b), c) => (a, b, c) }, { case (a, b, c) => ((a, b), c) })

  implicit def zip4[A, B, C, D](
    implicit c1: Schema[A],
    c2: Schema[B],
    c3: Schema[C],
    c4: Schema[D]
  ): Schema[(A, B, C, D)] =
    c1.zip(c2)
      .zip(c3)
      .zip(c4)
      .transform({ case (((a, b), c), d) => (a, b, c, d) }, { case (a, b, c, d) => (((a, b), c), d) })
}
