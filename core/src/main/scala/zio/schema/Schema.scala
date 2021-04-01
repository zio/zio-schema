package zio.schema

import zio.Chunk

sealed trait Schema[A] { self =>
  def ? : Schema[Option[A]] = Schema.Optional(self)

  def transform[B](f: A => B, g: B => A): Schema[B] =
    Schema.Transform[A, B](self, a => Right(f(a)), b => Right(g(b)))

  def transformOrFail[B](f: A => Either[String, B], g: B => Either[String, A]): Schema[B] =
    Schema.Transform[A, B](self, f, g)

  def zip[B](that: Schema[B]): Schema[(A, B)] = Schema.Tuple(self, that)

}

object Schema {
  sealed case class Record(structure: Map[String, Schema[_]])      extends Schema[Map[String, _]]
  sealed case class Sequence[A](element: Schema[A])                extends Schema[Chunk[A]]
  sealed case class Enumeration(structure: Map[String, Schema[_]]) extends Schema[Map[String, _]]
  sealed case class Transform[A, B](codec: Schema[A], f: A => Either[String, B], g: B => Either[String, A])
      extends Schema[B]
  sealed case class Primitive[A](standardType: StandardType[A])    extends Schema[A]
  sealed case class Tuple[A, B](left: Schema[A], right: Schema[B]) extends Schema[(A, B)]
  sealed case class Optional[A](codec: Schema[A])                  extends Schema[Option[A]]
  final case class Fail[A](message: String)                        extends Schema[A]

  sealed trait CaseClass[Z] extends Schema[Z] {
    def toRecord: Record
  }
  case class CaseClass1[A, Z](field: (String, Schema[A]), construct: A => Z, extract: Z => Tuple1[A])
      extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field))
  }
  case class CaseClass2[A1, A2, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    construct: (A1, A2) => Z,
    extract: Z => (A1, A2)
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2))
  }
  case class CaseClass3[A1, A2, A3, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    construct: (A1, A2, A3) => Z,
    extract: Z => (A1, A2, A3)
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3))
  }
  case class CaseClass4[A1, A2, A3, A4, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    construct: (A1, A2, A3, A4) => Z,
    extract: Z => (A1, A2, A3, A4)
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3, field4))
  }
  case class CaseClass5[A1, A2, A3, A4, A5, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    construct: (A1, A2, A3, A4, A5) => Z,
    extract: Z => (A1, A2, A3, A4, A5)
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3, field4, field5))
  }
  case class CaseClass6[A1, A2, A3, A4, A5, A6, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    field6: (String, Schema[A6]),
    construct: (A1, A2, A3, A4, A5, A6) => Z,
    extract: Z => (A1, A2, A3, A4, A5, A6)
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3, field4, field5, field6))
  }
  case class CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    field6: (String, Schema[A6]),
    field7: (String, Schema[A7]),
    construct: (A1, A2, A3, A4, A5, A6, A7) => Z,
    extract: Z => (A1, A2, A3, A4, A5, A6, A7)
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3, field4, field5, field6, field7))
  }
  case class CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z](
    field1: (String, Schema[A1]),
    field2: (String, Schema[A2]),
    field3: (String, Schema[A3]),
    field4: (String, Schema[A4]),
    field5: (String, Schema[A5]),
    field6: (String, Schema[A6]),
    field7: (String, Schema[A7]),
    field8: (String, Schema[A8]),
    construct: (A1, A2, A3, A4, A5, A6, A7, A8) => Z,
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8)
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3, field4, field5, field6, field7, field8))
  }
  case class CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9)
  ) extends CaseClass[Z] {
    override def toRecord: Record = Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9))
  }
  case class CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10))
  }
  case class CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11))
  }
  case class CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12))
  }
  case class CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(
        Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13)
      )
  }
  // format: off
  case class CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14))
  }
  case class CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15))
  }
  case class CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16))
  }
  case class CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17))
  }
  case class CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18))
  }
  case class CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18, field19))
  }
  case class CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18, field19, field20))
  }
  case class CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19,A20, A21, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18, field19, field20, field21))
  }
  case class CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](
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
    extract: Z => (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)
  ) extends CaseClass[Z] {
    override def toRecord: Record =
      Record(Map(field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, field15, field16, field17, field18, field19, field20, field21, field22))
  }
  // format: on

  def fail[A](message: String): Schema[A] = Fail(message)

  def apply[A](implicit codec: Schema[A]): Schema[A] = codec

  def caseClassN[A, Z](t1: (String, Schema[A]))(f: A => Z, g: Z => Option[A]): Schema[Z] =
    Schema
      .record(Map(t1))
      .transformOrFail(
        { map =>
          val v1 = map(t1._1).asInstanceOf[A]

          Right(f(v1))
        }, { (z: Z) =>
          g(z).map { a =>
            Map(t1._1 -> a)
          }.toRight("Cannot deconstruct case class")
        }
      )

  def caseClassN[A, B, Z](
    t1: (String, Schema[A]),
    t2: (String, Schema[B])
  )(f: (A, B) => Z, g: Z => Option[(A, B)]): Schema[Z] =
    Schema
      .record(Map[String, Schema[_]](t1, t2))
      .transformOrFail(
        { map =>
          val v1 = map(t1._1).asInstanceOf[A]
          val v2 = map(t2._1).asInstanceOf[B]

          Right(f(v1, v2))
        }, { (z: Z) =>
          g(z).map { case (a, b) => Map(t1._1 -> a, t2._1 -> b) }
            .toRight("Cannot deconstruct case class")
        }
      )

  def caseClassN[A, B, C, Z](
    t1: (String, Schema[A]),
    t2: (String, Schema[B]),
    t3: (String, Schema[C])
  )(f: (A, B, C) => Z, g: Z => Option[(A, B, C)]): Schema[Z] =
    Schema
      .record(Map[String, Schema[_]](t1, t2, t3))
      .transformOrFail(
        { map =>
          val v1 = map(t1._1).asInstanceOf[A]
          val v2 = map(t2._1).asInstanceOf[B]
          val v3 = map(t3._1).asInstanceOf[C]

          Right(f(v1, v2, v3))
        }, { (z: Z) =>
          g(z).map { case (a, b, c) => Map(t1._1 -> a, t2._1 -> b, t3._1 -> c) }
            .toRight("Cannot deconstruct case class")
        }
      )

  def either[A, B](left: Schema[A], right: Schema[B]): Schema[Either[A, B]] =
    enumeration(Map("Left" -> left, "Right" -> right)).transformOrFail(
      { map =>
        map.headOption.map {
          case ("Left", v)  => Right(Left(v.asInstanceOf[A]))
          case ("Right", v) => Right(Right(v.asInstanceOf[B]))
          case _            => Left("Expected left or right of sum")
        }.getOrElse(Left("Expected left or right of sum"))
      }, {
        case Left(v)  => Right(Map("Left"  -> v))
        case Right(v) => Right(Map("Right" -> v))
      }
    )

  def enumeration(structure: Map[String, Schema[_]]): Schema[Map[String, _]] =
    Enumeration(structure)

  def first[A](codec: Schema[(A, Unit)]): Schema[A] =
    codec.transform[A](_._1, a => (a, ()))

  implicit def list[A](implicit element: Schema[A]): Schema[List[A]] =
    sequence(element).transform(_.toList, Chunk.fromIterable(_))

  implicit def option[A](implicit element: Schema[A]): Schema[Option[A]] =
    Optional(element)

  implicit def primitive[A](implicit standardType: StandardType[A]): Schema[A] =
    Primitive(standardType)

  def record(structure: Map[String, Schema[_]]): Schema[Map[String, _]] =
    Record(structure)

  implicit def sequence[A](implicit element: Schema[A]): Schema[Chunk[A]] =
    Sequence(element)

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
