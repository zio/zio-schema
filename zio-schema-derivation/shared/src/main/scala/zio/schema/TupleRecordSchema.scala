package zio.schema

import zio.Chunk

private[schema] object TupleRecordSchema {

  def tupleToRecordSchema[T](schemas: => Chunk[Schema[_]]): Schema.Record[T] = {
    val arity = schemas.length
    arity match {
      case 2  =>
        Schema
          .CaseClass2[Any, Any, (Any, Any)](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any)) => t._1,
              set0 = (t: (Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any)) => t._2,
              set0 = (t: (Any, Any), v: Any) => t.copy(_2 = v)
            ),
            (t1, t2) => (t1, t2)
          )
          .asInstanceOf[Schema.Record[T]]
      case 3  =>
        Schema
          .CaseClass3[Any, Any, Any, (Any, Any, Any)](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            (t1, t2, t3) => (t1, t2, t3)
          )
          .asInstanceOf[Schema.Record[T]]
      case 4  =>
        Schema
          .CaseClass4[Any, Any, Any, Any, (Any, Any, Any, Any)](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any), v: Any) => t.copy(_4 = v)
            ),
            (t1, t2, t3, t4) => (t1, t2, t3, t4)
          )
          .asInstanceOf[Schema.Record[T]]
      case 5  =>
        Schema
          .CaseClass5[Any, Any, Any, Any, Any, (Any, Any, Any, Any, Any)](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any, Any), v: Any) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any)) => t._5,
              set0 = (t: (Any, Any, Any, Any, Any), v: Any) => t.copy(_5 = v)
            ),
            (t1, t2, t3, t4, t5) => (t1, t2, t3, t4, t5)
          )
          .asInstanceOf[Schema.Record[T]]
      case 6  =>
        Schema
          .CaseClass6[Any, Any, Any, Any, Any, Any, (Any, Any, Any, Any, Any, Any)](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any)) => t._5,
              set0 = (t: (Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any)) => t._6,
              set0 = (t: (Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_6 = v)
            ),
            (t1, t2, t3, t4, t5, t6) => (t1, t2, t3, t4, t5, t6)
          )
          .asInstanceOf[Schema.Record[T]]
      case 7  =>
        Schema
          .CaseClass7[Any, Any, Any, Any, Any, Any, Any, (Any, Any, Any, Any, Any, Any, Any)](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any)) => t._5,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any)) => t._6,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any)) => t._7,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_7 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7) => (t1, t2, t3, t4, t5, t6, t7)
          )
          .asInstanceOf[Schema.Record[T]]
      case 8  =>
        Schema
          .CaseClass8[Any, Any, Any, Any, Any, Any, Any, Any, (Any, Any, Any, Any, Any, Any, Any, Any)](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any)) => t._5,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any)) => t._6,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any)) => t._7,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any)) => t._8,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_8 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8) => (t1, t2, t3, t4, t5, t6, t7, t8)
          )
          .asInstanceOf[Schema.Record[T]]
      case 9  =>
        Schema
          .CaseClass9[Any, Any, Any, Any, Any, Any, Any, Any, Any, (Any, Any, Any, Any, Any, Any, Any, Any, Any)](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._5,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._6,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._7,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._8,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._9,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_9 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9) => (t1, t2, t3, t4, t5, t6, t7, t8, t9)
          )
          .asInstanceOf[Schema.Record[T]]
      case 10 =>
        Schema
          .CaseClass10[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._5,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._6,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._7,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._8,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._9,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._10,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_10 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
          )
          .asInstanceOf[Schema.Record[T]]
      case 11 =>
        Schema
          .CaseClass11[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._5,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._6,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._7,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._8,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._9,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._10,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_10 = v)
            ),
            Schema.Field(
              "_11",
              schemas(10).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._11,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_11 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
          )
          .asInstanceOf[Schema.Record[T]]
      case 12 =>
        Schema
          .CaseClass12[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._5,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._6,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._7,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._8,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._9,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._10,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_10 = v)
            ),
            Schema.Field(
              "_11",
              schemas(10).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._11,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_11 = v)
            ),
            Schema.Field(
              "_12",
              schemas(11).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._12,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_12 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) => (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
          )
          .asInstanceOf[Schema.Record[T]]
      case 13 =>
        Schema
          .CaseClass13[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._5,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._6,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._7,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._8,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._9,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._10,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_10 = v)
            ),
            Schema.Field(
              "_11",
              schemas(10).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._11,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_11 = v)
            ),
            Schema.Field(
              "_12",
              schemas(11).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._12,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_12 = v)
            ),
            Schema.Field(
              "_13",
              schemas(12).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._13,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_13 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) =>
              (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)
          )
          .asInstanceOf[Schema.Record[T]]
      case 14 =>
        Schema
          .CaseClass14[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._1,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._2,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._3,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._4,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._5,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._6,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._7,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._8,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._9,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._10,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_10 = v)
            ),
            Schema.Field(
              "_11",
              schemas(10).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._11,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_11 = v)
            ),
            Schema.Field(
              "_12",
              schemas(11).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._12,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_12 = v)
            ),
            Schema.Field(
              "_13",
              schemas(12).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._13,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_13 = v)
            ),
            Schema.Field(
              "_14",
              schemas(13).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._14,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) => t.copy(_14 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) =>
              (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)
          )
          .asInstanceOf[Schema.Record[T]]
      case 15 =>
        Schema
          .CaseClass15[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._5,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._6,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._7,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._8,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._9,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._10,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_10 = v)
            ),
            Schema.Field(
              "_11",
              schemas(10).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._11,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_11 = v)
            ),
            Schema.Field(
              "_12",
              schemas(11).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._12,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_12 = v)
            ),
            Schema.Field(
              "_13",
              schemas(12).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._13,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_13 = v)
            ),
            Schema.Field(
              "_14",
              schemas(13).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._14,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_14 = v)
            ),
            Schema.Field(
              "_15",
              schemas(14).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._15,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_15 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) =>
              (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)
          )
          .asInstanceOf[Schema.Record[T]]
      case 16 =>
        Schema
          .CaseClass16[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._1,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._2,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._3,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._4,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._5,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._6,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._7,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._8,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._9,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._10,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_10 = v)
            ),
            Schema.Field(
              "_11",
              schemas(10).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._11,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_11 = v)
            ),
            Schema.Field(
              "_12",
              schemas(11).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._12,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_12 = v)
            ),
            Schema.Field(
              "_13",
              schemas(12).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._13,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_13 = v)
            ),
            Schema.Field(
              "_14",
              schemas(13).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._14,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_14 = v)
            ),
            Schema.Field(
              "_15",
              schemas(14).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._15,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_15 = v)
            ),
            Schema.Field(
              "_16",
              schemas(15).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._16,
              set0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                t.copy(_16 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16) =>
              (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16)
          )
          .asInstanceOf[Schema.Record[T]]
      case 17 =>
        Schema
          .CaseClass17[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._1,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._2,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._3,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._4,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._5,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._6,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._7,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._8,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._9,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._10,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_10 = v)
            ),
            Schema.Field(
              "_11",
              schemas(10).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._11,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_11 = v)
            ),
            Schema.Field(
              "_12",
              schemas(11).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._12,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_12 = v)
            ),
            Schema.Field(
              "_13",
              schemas(12).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._13,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_13 = v)
            ),
            Schema.Field(
              "_14",
              schemas(13).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._14,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_14 = v)
            ),
            Schema.Field(
              "_15",
              schemas(14).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._15,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_15 = v)
            ),
            Schema.Field(
              "_16",
              schemas(15).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._16,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_16 = v)
            ),
            Schema.Field(
              "_17",
              schemas(16).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._17,
              set0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any), v: Any) =>
                  t.copy(_17 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17) =>
              (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17)
          )
          .asInstanceOf[Schema.Record[T]]
      case 18 =>
        Schema
          .CaseClass18[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._1,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._2,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._3,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._4,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._5,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._6,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._7,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._8,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) => t._9,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                t._10,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_10 = v)
            ),
            Schema.Field(
              "_11",
              schemas(10).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                t._11,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_11 = v)
            ),
            Schema.Field(
              "_12",
              schemas(11).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                t._12,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_12 = v)
            ),
            Schema.Field(
              "_13",
              schemas(12).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                t._13,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_13 = v)
            ),
            Schema.Field(
              "_14",
              schemas(13).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                t._14,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_14 = v)
            ),
            Schema.Field(
              "_15",
              schemas(14).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                t._15,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_15 = v)
            ),
            Schema.Field(
              "_16",
              schemas(15).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                t._16,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_16 = v)
            ),
            Schema.Field(
              "_17",
              schemas(16).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                t._17,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_17 = v)
            ),
            Schema.Field(
              "_18",
              schemas(17).asInstanceOf[Schema[Any]],
              get0 = (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                t._18,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_18 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18) =>
              (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18)
          )
          .asInstanceOf[Schema.Record[T]]
      case 19 =>
        Schema
          .CaseClass19[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._1,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._2,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._3,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._4,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._5,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._6,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._7,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._8,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._9,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._10,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_10 = v)
            ),
            Schema.Field(
              "_11",
              schemas(10).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._11,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_11 = v)
            ),
            Schema.Field(
              "_12",
              schemas(11).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._12,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_12 = v)
            ),
            Schema.Field(
              "_13",
              schemas(12).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._13,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_13 = v)
            ),
            Schema.Field(
              "_14",
              schemas(13).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._14,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_14 = v)
            ),
            Schema.Field(
              "_15",
              schemas(14).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._15,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_15 = v)
            ),
            Schema.Field(
              "_16",
              schemas(15).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._16,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_16 = v)
            ),
            Schema.Field(
              "_17",
              schemas(16).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._17,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_17 = v)
            ),
            Schema.Field(
              "_18",
              schemas(17).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._18,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_18 = v)
            ),
            Schema.Field(
              "_19",
              schemas(18).asInstanceOf[Schema[Any]],
              get0 =
                (t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)) =>
                  t._19,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_19 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19) =>
              (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19)
          )
          .asInstanceOf[Schema.Record[T]]
      case 20 =>
        Schema
          .CaseClass20[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._1,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._2,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._3,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._4,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._5,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._6,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._7,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._8,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._9,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._10,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_10 = v)
            ),
            Schema.Field(
              "_11",
              schemas(10).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._11,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_11 = v)
            ),
            Schema.Field(
              "_12",
              schemas(11).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._12,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_12 = v)
            ),
            Schema.Field(
              "_13",
              schemas(12).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._13,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_13 = v)
            ),
            Schema.Field(
              "_14",
              schemas(13).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._14,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_14 = v)
            ),
            Schema.Field(
              "_15",
              schemas(14).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._15,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_15 = v)
            ),
            Schema.Field(
              "_16",
              schemas(15).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._16,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_16 = v)
            ),
            Schema.Field(
              "_17",
              schemas(16).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._17,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_17 = v)
            ),
            Schema.Field(
              "_18",
              schemas(17).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._18,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_18 = v)
            ),
            Schema.Field(
              "_19",
              schemas(18).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._19,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_19 = v)
            ),
            Schema.Field(
              "_20",
              schemas(19).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._20,
              set0 = (
                t: (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any),
                v: Any
              ) => t.copy(_20 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20) =>
              (
                t1,
                t2,
                t3,
                t4,
                t5,
                t6,
                t7,
                t8,
                t9,
                t10,
                t11,
                t12,
                t13,
                t14,
                t15,
                t16,
                t17,
                t18,
                t19,
                t20
              )
          )
          .asInstanceOf[Schema.Record[T]]
      case 21 =>
        Schema
          .CaseClass21[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any, Any)
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._1,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._2,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._3,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._4,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._5,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._6,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._7,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._8,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._9,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._10,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_10 = v)
            ),
            Schema.Field(
              "_11",
              schemas(10).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._11,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_11 = v)
            ),
            Schema.Field(
              "_12",
              schemas(11).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._12,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_12 = v)
            ),
            Schema.Field(
              "_13",
              schemas(12).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._13,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_13 = v)
            ),
            Schema.Field(
              "_14",
              schemas(13).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._14,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_14 = v)
            ),
            Schema.Field(
              "_15",
              schemas(14).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._15,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_15 = v)
            ),
            Schema.Field(
              "_16",
              schemas(15).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._16,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_16 = v)
            ),
            Schema.Field(
              "_17",
              schemas(16).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._17,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_17 = v)
            ),
            Schema.Field(
              "_18",
              schemas(17).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._18,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_18 = v)
            ),
            Schema.Field(
              "_19",
              schemas(18).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._19,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_19 = v)
            ),
            Schema.Field(
              "_20",
              schemas(19).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._20,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_20 = v)
            ),
            Schema.Field(
              "_21",
              schemas(20).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._21,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_21 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21) =>
              (
                t1,
                t2,
                t3,
                t4,
                t5,
                t6,
                t7,
                t8,
                t9,
                t10,
                t11,
                t12,
                t13,
                t14,
                t15,
                t16,
                t17,
                t18,
                t19,
                t20,
                t21
              )
          )
          .asInstanceOf[Schema.Record[T]]
      case 22 =>
        Schema
          .CaseClass22[
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            Any,
            (
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any,
              Any
            )
          ](
            TypeId.parse(s"zio.schema.Tuple.Tuple$arity"),
            Schema.Field(
              "_1",
              schemas(0).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._1,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_1 = v)
            ),
            Schema.Field(
              "_2",
              schemas(1).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._2,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_2 = v)
            ),
            Schema.Field(
              "_3",
              schemas(2).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._3,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_3 = v)
            ),
            Schema.Field(
              "_4",
              schemas(3).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._4,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_4 = v)
            ),
            Schema.Field(
              "_5",
              schemas(4).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._5,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_5 = v)
            ),
            Schema.Field(
              "_6",
              schemas(5).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._6,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_6 = v)
            ),
            Schema.Field(
              "_7",
              schemas(6).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._7,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_7 = v)
            ),
            Schema.Field(
              "_8",
              schemas(7).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._8,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_8 = v)
            ),
            Schema.Field(
              "_9",
              schemas(8).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._9,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_9 = v)
            ),
            Schema.Field(
              "_10",
              schemas(9).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._10,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_10 = v)
            ),
            Schema.Field(
              "_11",
              schemas(10).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._11,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_11 = v)
            ),
            Schema.Field(
              "_12",
              schemas(11).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._12,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_12 = v)
            ),
            Schema.Field(
              "_13",
              schemas(12).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._13,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_13 = v)
            ),
            Schema.Field(
              "_14",
              schemas(13).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._14,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_14 = v)
            ),
            Schema.Field(
              "_15",
              schemas(14).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._15,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_15 = v)
            ),
            Schema.Field(
              "_16",
              schemas(15).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._16,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_16 = v)
            ),
            Schema.Field(
              "_17",
              schemas(16).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._17,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_17 = v)
            ),
            Schema.Field(
              "_18",
              schemas(17).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._18,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_18 = v)
            ),
            Schema.Field(
              "_19",
              schemas(18).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._19,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_19 = v)
            ),
            Schema.Field(
              "_20",
              schemas(19).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._20,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_20 = v)
            ),
            Schema.Field(
              "_21",
              schemas(20).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._21,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_21 = v)
            ),
            Schema.Field(
              "_22",
              schemas(21).asInstanceOf[Schema[Any]],
              get0 = (t: (
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any,
                Any
              )) => t._22,
              set0 = (
                t: (
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any,
                  Any
                ),
                v: Any
              ) => t.copy(_22 = v)
            ),
            (t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15, t16, t17, t18, t19, t20, t21, t22) =>
              (
                t1,
                t2,
                t3,
                t4,
                t5,
                t6,
                t7,
                t8,
                t9,
                t10,
                t11,
                t12,
                t13,
                t14,
                t15,
                t16,
                t17,
                t18,
                t19,
                t20,
                t21,
                t22
              )
          )
          .asInstanceOf[Schema.Record[T]]
      case _  => throw new IllegalArgumentException(s"Unsupported tuple arity: $arity")
    }
  }
}
