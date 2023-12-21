package zio.schema

import zio.Unsafe
import zio.schema.Schema.Record
import zio.test._

object RecordSpec extends ZIOSpecDefault {

  override def spec: Spec[Environment, Any] = suite("Record Spec")(
    suite("Deconstruct should get the values of a case class' fields")(
      assertDeconstructResults("CaseClass0", Record0()),
      assertDeconstructResults("CaseClass1", Record1()),
      assertDeconstructResults("CaseClass2", Record2()),
      assertDeconstructResults("CaseClass3", Record3()),
      assertDeconstructResults("CaseClass4", Record4()),
      assertDeconstructResults("CaseClass5", Record5()),
      assertDeconstructResults("CaseClass6", Record6()),
      assertDeconstructResults("CaseClass7", Record7()),
      assertDeconstructResults("CaseClass8", Record8()),
      assertDeconstructResults("CaseClass9", Record9()),
      assertDeconstructResults("CaseClass10", Record10()),
      assertDeconstructResults("CaseClass11", Record11()),
      assertDeconstructResults("CaseClass12", Record12()),
      assertDeconstructResults("CaseClass13", Record13()),
      assertDeconstructResults("CaseClass14", Record14()),
      assertDeconstructResults("CaseClass15", Record15()),
      assertDeconstructResults("CaseClass16", Record16()),
      assertDeconstructResults("CaseClass17", Record17()),
      assertDeconstructResults("CaseClass18", Record18()),
      assertDeconstructResults("CaseClass19", Record19()),
      assertDeconstructResults("CaseClass20", Record20()),
      assertDeconstructResults("CaseClass21", Record21()),
      assertDeconstructResults("CaseClass22", Record22())
    )
  )

  private def assertDeconstructResults[A](name: String, value: A)(implicit record: Record[A]): Spec[Any, Nothing] =
    test(name) {
      val deconstructed = Unsafe.unsafe { implicit unsafe =>
        record.deconstruct(value)
      }
      assertTrue(
        deconstructed.forall(_.exists(_ == "dummy-value")),
        deconstructed.size == record.fields.size
      )
    }

  case class Record0()

  object Record0 {
    implicit val record: Schema.Record[Record0] = DeriveSchema.gen[Record0]
  }
  case class Record1(a: String = "dummy-value")

  object Record1 {
    implicit val record: Schema.Record[Record1] = DeriveSchema.gen[Record1]
  }
  case class Record2(a: String = "dummy-value", b: String = "dummy-value")

  object Record2 {
    implicit val record: Schema.Record[Record2] = DeriveSchema.gen[Record2]
  }
  case class Record3(a: String = "dummy-value", b: String = "dummy-value", c: String = "dummy-value")

  object Record3 {
    implicit val record: Schema.Record[Record3] = DeriveSchema.gen[Record3]
  }
  case class Record4(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value"
  )

  object Record4 {
    implicit val record: Schema.Record[Record4] = DeriveSchema.gen[Record4]
  }
  case class Record5(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value"
  )

  object Record5 {
    implicit val record: Schema.Record[Record5] = DeriveSchema.gen[Record5]
  }
  case class Record6(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value"
  )

  object Record6 {
    implicit val record: Schema.Record[Record6] = DeriveSchema.gen[Record6]
  }
  case class Record7(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value"
  )

  object Record7 {
    implicit val record: Schema.Record[Record7] = DeriveSchema.gen[Record7]
  }
  case class Record8(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value"
  )

  object Record8 {
    implicit val record: Schema.Record[Record8] = DeriveSchema.gen[Record8]
  }
  case class Record9(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value"
  )

  object Record9 {
    implicit val record: Schema.Record[Record9] = DeriveSchema.gen[Record9]
  }
  case class Record10(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value"
  )

  object Record10 {
    implicit val record: Schema.Record[Record10] = DeriveSchema.gen[Record10]
  }

  case class Record11(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value",
    k: String = "dummy-value"
  )

  object Record11 {
    implicit val record: Schema.Record[Record11] = DeriveSchema.gen[Record11]
  }

  case class Record12(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value",
    k: String = "dummy-value",
    l: String = "dummy-value"
  )

  object Record12 {
    implicit val record: Schema.Record[Record12] = DeriveSchema.gen[Record12]
  }

  case class Record13(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value",
    k: String = "dummy-value",
    l: String = "dummy-value",
    m: String = "dummy-value"
  )

  object Record13 {
    implicit val record: Schema.Record[Record13] = DeriveSchema.gen[Record13]
  }

  case class Record14(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value",
    k: String = "dummy-value",
    l: String = "dummy-value",
    m: String = "dummy-value",
    n: String = "dummy-value"
  )

  object Record14 {
    implicit val record: Schema.Record[Record14] = DeriveSchema.gen[Record14]
  }

  case class Record15(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value",
    k: String = "dummy-value",
    l: String = "dummy-value",
    m: String = "dummy-value",
    n: String = "dummy-value",
    o: String = "dummy-value"
  )

  object Record15 {
    implicit val record: Schema.Record[Record15] = DeriveSchema.gen[Record15]
  }

  case class Record16(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value",
    k: String = "dummy-value",
    l: String = "dummy-value",
    m: String = "dummy-value",
    n: String = "dummy-value",
    o: String = "dummy-value",
    p: String = "dummy-value"
  )

  object Record16 {
    implicit val record: Schema.Record[Record16] = DeriveSchema.gen[Record16]
  }

  case class Record17(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value",
    k: String = "dummy-value",
    l: String = "dummy-value",
    m: String = "dummy-value",
    n: String = "dummy-value",
    o: String = "dummy-value",
    p: String = "dummy-value",
    q: String = "dummy-value"
  )

  object Record17 {
    implicit val record: Schema.Record[Record17] = DeriveSchema.gen[Record17]
  }

  case class Record18(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value",
    k: String = "dummy-value",
    l: String = "dummy-value",
    m: String = "dummy-value",
    n: String = "dummy-value",
    o: String = "dummy-value",
    p: String = "dummy-value",
    q: String = "dummy-value",
    r: String = "dummy-value"
  )

  object Record18 {
    implicit val record: Schema.Record[Record18] = DeriveSchema.gen[Record18]
  }

  case class Record19(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value",
    k: String = "dummy-value",
    l: String = "dummy-value",
    m: String = "dummy-value",
    n: String = "dummy-value",
    o: String = "dummy-value",
    p: String = "dummy-value",
    q: String = "dummy-value",
    r: String = "dummy-value",
    s: String = "dummy-value"
  )

  object Record19 {
    implicit val record: Schema.Record[Record19] = DeriveSchema.gen[Record19]
  }

  case class Record20(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value",
    k: String = "dummy-value",
    l: String = "dummy-value",
    m: String = "dummy-value",
    n: String = "dummy-value",
    o: String = "dummy-value",
    p: String = "dummy-value",
    q: String = "dummy-value",
    r: String = "dummy-value",
    s: String = "dummy-value",
    t: String = "dummy-value"
  )

  object Record20 {
    implicit val record: Schema.Record[Record20] = DeriveSchema.gen[Record20]
  }

  case class Record21(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value",
    k: String = "dummy-value",
    l: String = "dummy-value",
    m: String = "dummy-value",
    n: String = "dummy-value",
    o: String = "dummy-value",
    p: String = "dummy-value",
    q: String = "dummy-value",
    r: String = "dummy-value",
    s: String = "dummy-value",
    t: String = "dummy-value",
    u: String = "dummy-value"
  )

  object Record21 {
    implicit val record: Schema.Record[Record21] = DeriveSchema.gen[Record21]
  }

  case class Record22(
    a: String = "dummy-value",
    b: String = "dummy-value",
    c: String = "dummy-value",
    d: String = "dummy-value",
    e: String = "dummy-value",
    f: String = "dummy-value",
    g: String = "dummy-value",
    h: String = "dummy-value",
    i: String = "dummy-value",
    j: String = "dummy-value",
    k: String = "dummy-value",
    l: String = "dummy-value",
    m: String = "dummy-value",
    n: String = "dummy-value",
    o: String = "dummy-value",
    p: String = "dummy-value",
    q: String = "dummy-value",
    r: String = "dummy-value",
    s: String = "dummy-value",
    t: String = "dummy-value",
    u: String = "dummy-value",
    v: String = "dummy-value"
  )

  object Record22 {
    implicit val record: Schema.Record[Record22] = DeriveSchema.gen[Record22]
  }
}
