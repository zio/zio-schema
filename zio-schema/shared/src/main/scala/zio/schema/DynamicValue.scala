package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk

trait DynamicValue { self =>

  def toTypedValue[A](schema: Schema[A]): Either[String, A] =
    (self, schema) match {
      case (DynamicValue.Primitive(value, p), Schema.Primitive(p2)) if p == p2 =>
        Right(value.asInstanceOf[A])

      case (DynamicValue.Record(values), Schema.GenericRecord(structure)) =>
        DynamicValue.decodeStructure(values, structure).asInstanceOf[Either[String, A]]

      case (DynamicValue.Record(values), s: Schema.Record[A]) =>
        DynamicValue.decodeStructure(values, s.structure).map(m => Chunk.fromIterable(m.values)).flatMap(s.rawConstruct)

      case (DynamicValue.Enumeration((key, value)), Schema.Enumeration(cases)) =>
        cases.get(key) match {
          case Some(schema) =>
            value.toTypedValue(schema).asInstanceOf[Either[String, A]].map(key -> _)
          case None => Left(s"Failed to find case $key in enumeration $schema")
        }

      case (DynamicValue.Enumeration((key, value)), s @ Schema.Enum1(case1)) =>
        if (case1.id == key)
          value.toTypedValue(case1.codec)
        else
          Left(s"Failed to find case $key in enum $s")

      case (DynamicValue.Enumeration((key, value)), s @ Schema.Enum2(case1, case2)) =>
        Chunk(case1, case2)
          .find(_.id == key) match {
          case Some(c) => value.toTypedValue(c.codec)
          case None    => Left(s"Failed to find case $key in enum $s")
        }

      case (DynamicValue.Enumeration((key, value)), s @ Schema.Enum3(case1, case2, case3)) =>
        Chunk(case1, case2, case3)
          .find(_.id == key) match {
          case Some(c) => value.toTypedValue(c.codec)
          case None    => Left(s"Failed to find case $key in enum $s")
        }

      case (DynamicValue.Enumeration((key, value)), s @ Schema.EnumN(cases)) =>
        cases
          .find(_.id == key) match {
          case Some(c) => value.toTypedValue(c.codec).asInstanceOf[Either[String, A]]
          case None    => Left(s"Failed to find case $key in enum $s")
        }

      case (DynamicValue.LeftValue(value), Schema.EitherSchema(schema1, _)) =>
        value.toTypedValue(schema1).map(Left(_))

      case (DynamicValue.RightValue(value), Schema.EitherSchema(_, schema1)) =>
        value.toTypedValue(schema1).map(Right(_))

      case (DynamicValue.Tuple(leftValue, rightValue), Schema.Tuple(leftSchema, rightSchema)) =>
        val typedLeft  = leftValue.toTypedValue(leftSchema)
        val typedRight = rightValue.toTypedValue(rightSchema)
        (typedLeft, typedRight) match {
          case (Left(e1), Left(e2)) => Left(s"Converting generic tuple to typed value failed with errors $e1 and $e2")
          case (_, Left(e))         => Left(e)
          case (Left(e), _)         => Left(e)
          case (Right(a), Right(b)) => Right(a -> b)
        }

      case (DynamicValue.Sequence(values), Schema.Sequence(schema, f, _)) =>
        values
          .foldLeft[Either[String, Chunk[_]]](Right[String, Chunk[A]](Chunk.empty)) {
            case (err @ Left(_), _) => err
            case (Right(values), value) =>
              value.toTypedValue(schema).map(values :+ _)
          }
          .map(f)

      case (DynamicValue.SomeValue(value), Schema.Optional(schema: Schema[_])) =>
        value.toTypedValue(schema).map(Some(_))

      case (DynamicValue.NoneValue, Schema.Optional(_)) =>
        Right(None)

      case (DynamicValue.Transform(DynamicValue.Error(message)), Schema.Transform(_, _, _)) =>
        Left(message)

      case (DynamicValue.Transform(value), Schema.Transform(schema, f, _)) =>
        value.toTypedValue(schema).flatMap(f)

      case (_, l @ Schema.Lazy(_)) =>
        toTypedValue(l.schema)

      case (DynamicValue.Error(message), _) =>
        Left(message)

      case _ =>
        Left(s"Failed to cast $self to schema $schema")
    }

}

object DynamicValue {

  def fromSchemaAndValue[A](schema: Schema[A], value: A): DynamicValue =
    schema match {

      case l @ Schema.Lazy(_) => fromSchemaAndValue(l.schema, value)

      case Schema.Primitive(p) => DynamicValue.Primitive(value, p)

      case Schema.GenericRecord(structure) =>
        val map: ListMap[String, _] = value
        DynamicValue.Record(
          ListMap.empty ++ structure.map {
            case Schema.Field(key, schema: Schema[a], _) =>
              key -> fromSchemaAndValue(schema, map(key).asInstanceOf[a])
          }
        )

      case Schema.Enumeration(map) =>
        val (key, v) = value
        map(key) match {
          case schema: Schema[a] =>
            val nestedValue = fromSchemaAndValue(schema, v.asInstanceOf[a])
            DynamicValue.Enumeration(key -> nestedValue)
        }

      case Schema.Enum1(case1) =>
        DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, case1.unsafeDeconstruct(value)))

      case Schema.Enum2(case1, case2) =>
        (case1.deconstruct(value), case2.deconstruct(value)) match {
          case (Some(v1), _) => DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2)) => DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum3(case1, case2, case3) =>
        (case1.deconstruct(value), case2.deconstruct(value), case3.deconstruct(value)) match {
          case (Some(v1), _, _) => DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _) => DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3)) => DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case3.codec, v3))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.EnumN(cases) =>
        cases
          .find(_.deconstruct(value).isDefined) match {
          case Some(c) =>
            DynamicValue.Enumeration(
              c.id -> fromSchemaAndValue(c.codec.asInstanceOf[Schema[Any]], c.unsafeDeconstruct(value))
            )
          case None => DynamicValue.NoneValue
        }

      case Schema.Fail(message) => DynamicValue.Error(message)

      case Schema.Sequence(schema, _, toChunk) =>
        DynamicValue.Sequence(toChunk(value).map(fromSchemaAndValue(schema, _)))

      case Schema.EitherSchema(left, right) =>
        value match {
          case Left(a)  => DynamicValue.LeftValue(fromSchemaAndValue(left, a))
          case Right(b) => DynamicValue.RightValue(fromSchemaAndValue(right, b))
        }

      case Schema.Tuple(schemaA, schemaB) =>
        val (a, b) = value
        DynamicValue.Tuple(fromSchemaAndValue(schemaA, a), fromSchemaAndValue(schemaB, b))

      case Schema.Optional(schema) =>
        value match {
          case Some(value) => DynamicValue.SomeValue(fromSchemaAndValue(schema, value))
          case None        => DynamicValue.NoneValue
        }

      case Schema.Transform(schema, _, g) =>
        g(value) match {
          case Left(message) => DynamicValue.Transform(DynamicValue.Error(message))
          case Right(a)      => DynamicValue.Transform(fromSchemaAndValue(schema, a))
        }

      case Schema.Meta(spec) => DynamicValue.DynamicSchema(spec)

      case Schema.CaseClass1(_, f, _, ext) =>
        DynamicValue.Record(ListMap(f.label -> fromSchemaAndValue(f.schema, ext(value))))

      case Schema.CaseClass2(_, f1, f2, _, ext1, ext2) =>
        DynamicValue.Record(
          ListMap(
            f1.label -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label -> fromSchemaAndValue(f2.schema, ext2(value))
          )
        )
      case Schema.CaseClass3(_, f1, f2, f3, _, ext1, ext2, ext3) =>
        DynamicValue.Record(
          ListMap(
            f1.label -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label -> fromSchemaAndValue(f3.schema, ext3(value))
          )
        )
      case Schema.CaseClass4(_, f1, f2, f3, f4, _, ext1, ext2, ext3, ext4) =>
        DynamicValue.Record(
          ListMap(
            f1.label -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label -> fromSchemaAndValue(f4.schema, ext4(value))
          )
        )
      case Schema.CaseClass5(_, f1, f2, f3, f4, f5, _, ext1, ext2, ext3, ext4, ext5) =>
        DynamicValue.Record(
          ListMap(
            f1.label -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label -> fromSchemaAndValue(f5.schema, ext5(value))
          )
        )
      case Schema.CaseClass6(_, f1, f2, f3, f4, f5, f6, _, ext1, ext2, ext3, ext4, ext5, ext6) =>
        DynamicValue.Record(
          ListMap(
            f1.label -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label -> fromSchemaAndValue(f6.schema, ext6(value))
          )
        )
      case Schema.CaseClass7(_, f1, f2, f3, f4, f5, f6, f7, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7) =>
        DynamicValue.Record(
          ListMap(
            f1.label -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label -> fromSchemaAndValue(f7.schema, ext7(value))
          )
        )
      case Schema.CaseClass8(_, f1, f2, f3, f4, f5, f6, f7, f8, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8) =>
        DynamicValue.Record(
          ListMap(
            f1.label -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label -> fromSchemaAndValue(f8.schema, ext8(value))
          )
        )
      case Schema.CaseClass9(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label -> fromSchemaAndValue(f9.schema, ext9(value))
          )
        )
      case Schema.CaseClass10(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value))
          )
        )
      case Schema.CaseClass11(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          f11,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10,
          ext11
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value)),
            f11.label -> fromSchemaAndValue(f11.schema, ext11(value))
          )
        )
      case Schema.CaseClass12(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          f11,
          f12,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10,
          ext11,
          ext12
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value)),
            f11.label -> fromSchemaAndValue(f11.schema, ext11(value)),
            f12.label -> fromSchemaAndValue(f12.schema, ext12(value))
          )
        )
      case Schema.CaseClass13(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          f11,
          f12,
          f13,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10,
          ext11,
          ext12,
          ext13
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value)),
            f11.label -> fromSchemaAndValue(f11.schema, ext11(value)),
            f12.label -> fromSchemaAndValue(f12.schema, ext12(value)),
            f13.label -> fromSchemaAndValue(f13.schema, ext13(value))
          )
        )
      case Schema.CaseClass14(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          f11,
          f12,
          f13,
          f14,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10,
          ext11,
          ext12,
          ext13,
          ext14
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value)),
            f11.label -> fromSchemaAndValue(f11.schema, ext11(value)),
            f12.label -> fromSchemaAndValue(f12.schema, ext12(value)),
            f13.label -> fromSchemaAndValue(f13.schema, ext13(value)),
            f14.label -> fromSchemaAndValue(f14.schema, ext14(value))
          )
        )
      case Schema.CaseClass15(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          f11,
          f12,
          f13,
          f14,
          f15,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10,
          ext11,
          ext12,
          ext13,
          ext14,
          ext15
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value)),
            f11.label -> fromSchemaAndValue(f11.schema, ext11(value)),
            f12.label -> fromSchemaAndValue(f12.schema, ext12(value)),
            f13.label -> fromSchemaAndValue(f13.schema, ext13(value)),
            f14.label -> fromSchemaAndValue(f14.schema, ext14(value)),
            f15.label -> fromSchemaAndValue(f15.schema, ext15(value))
          )
        )
      case Schema.CaseClass16(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          f11,
          f12,
          f13,
          f14,
          f15,
          f16,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10,
          ext11,
          ext12,
          ext13,
          ext14,
          ext15,
          ext16
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value)),
            f11.label -> fromSchemaAndValue(f11.schema, ext11(value)),
            f12.label -> fromSchemaAndValue(f12.schema, ext12(value)),
            f13.label -> fromSchemaAndValue(f13.schema, ext13(value)),
            f14.label -> fromSchemaAndValue(f14.schema, ext14(value)),
            f15.label -> fromSchemaAndValue(f15.schema, ext15(value)),
            f16.label -> fromSchemaAndValue(f16.schema, ext16(value))
          )
        )
      case Schema.CaseClass17(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          f11,
          f12,
          f13,
          f14,
          f15,
          f16,
          f17,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10,
          ext11,
          ext12,
          ext13,
          ext14,
          ext15,
          ext16,
          ext17
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value)),
            f11.label -> fromSchemaAndValue(f11.schema, ext11(value)),
            f12.label -> fromSchemaAndValue(f12.schema, ext12(value)),
            f13.label -> fromSchemaAndValue(f13.schema, ext13(value)),
            f14.label -> fromSchemaAndValue(f14.schema, ext14(value)),
            f15.label -> fromSchemaAndValue(f15.schema, ext15(value)),
            f16.label -> fromSchemaAndValue(f16.schema, ext16(value)),
            f17.label -> fromSchemaAndValue(f17.schema, ext17(value))
          )
        )
      case Schema.CaseClass18(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          f11,
          f12,
          f13,
          f14,
          f15,
          f16,
          f17,
          f18,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10,
          ext11,
          ext12,
          ext13,
          ext14,
          ext15,
          ext16,
          ext17,
          ext18
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value)),
            f11.label -> fromSchemaAndValue(f11.schema, ext11(value)),
            f12.label -> fromSchemaAndValue(f12.schema, ext12(value)),
            f13.label -> fromSchemaAndValue(f13.schema, ext13(value)),
            f14.label -> fromSchemaAndValue(f14.schema, ext14(value)),
            f15.label -> fromSchemaAndValue(f15.schema, ext15(value)),
            f16.label -> fromSchemaAndValue(f16.schema, ext16(value)),
            f17.label -> fromSchemaAndValue(f17.schema, ext17(value)),
            f18.label -> fromSchemaAndValue(f18.schema, ext18(value))
          )
        )
      case Schema.CaseClass19(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          f11,
          f12,
          f13,
          f14,
          f15,
          f16,
          f17,
          f18,
          f19,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10,
          ext11,
          ext12,
          ext13,
          ext14,
          ext15,
          ext16,
          ext17,
          ext18,
          ext19
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value)),
            f11.label -> fromSchemaAndValue(f11.schema, ext11(value)),
            f12.label -> fromSchemaAndValue(f12.schema, ext12(value)),
            f13.label -> fromSchemaAndValue(f13.schema, ext13(value)),
            f14.label -> fromSchemaAndValue(f14.schema, ext14(value)),
            f15.label -> fromSchemaAndValue(f15.schema, ext15(value)),
            f16.label -> fromSchemaAndValue(f16.schema, ext16(value)),
            f17.label -> fromSchemaAndValue(f17.schema, ext17(value)),
            f18.label -> fromSchemaAndValue(f18.schema, ext18(value)),
            f19.label -> fromSchemaAndValue(f19.schema, ext19(value))
          )
        )
      case Schema.CaseClass20(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          f11,
          f12,
          f13,
          f14,
          f15,
          f16,
          f17,
          f18,
          f19,
          f20,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10,
          ext11,
          ext12,
          ext13,
          ext14,
          ext15,
          ext16,
          ext17,
          ext18,
          ext19,
          ext20
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value)),
            f11.label -> fromSchemaAndValue(f11.schema, ext11(value)),
            f12.label -> fromSchemaAndValue(f12.schema, ext12(value)),
            f13.label -> fromSchemaAndValue(f13.schema, ext13(value)),
            f14.label -> fromSchemaAndValue(f14.schema, ext14(value)),
            f15.label -> fromSchemaAndValue(f15.schema, ext15(value)),
            f16.label -> fromSchemaAndValue(f16.schema, ext16(value)),
            f17.label -> fromSchemaAndValue(f17.schema, ext17(value)),
            f18.label -> fromSchemaAndValue(f18.schema, ext18(value)),
            f19.label -> fromSchemaAndValue(f19.schema, ext19(value)),
            f20.label -> fromSchemaAndValue(f20.schema, ext20(value))
          )
        )
      case Schema.CaseClass21(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          f11,
          f12,
          f13,
          f14,
          f15,
          f16,
          f17,
          f18,
          f19,
          f20,
          f21,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10,
          ext11,
          ext12,
          ext13,
          ext14,
          ext15,
          ext16,
          ext17,
          ext18,
          ext19,
          ext20,
          ext21
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value)),
            f11.label -> fromSchemaAndValue(f11.schema, ext11(value)),
            f12.label -> fromSchemaAndValue(f12.schema, ext12(value)),
            f13.label -> fromSchemaAndValue(f13.schema, ext13(value)),
            f14.label -> fromSchemaAndValue(f14.schema, ext14(value)),
            f15.label -> fromSchemaAndValue(f15.schema, ext15(value)),
            f16.label -> fromSchemaAndValue(f16.schema, ext16(value)),
            f17.label -> fromSchemaAndValue(f17.schema, ext17(value)),
            f18.label -> fromSchemaAndValue(f18.schema, ext18(value)),
            f19.label -> fromSchemaAndValue(f19.schema, ext19(value)),
            f20.label -> fromSchemaAndValue(f20.schema, ext20(value)),
            f21.label -> fromSchemaAndValue(f21.schema, ext21(value))
          )
        )
      case Schema.CaseClass22(
          _,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          f9,
          f10,
          f11,
          f12,
          f13,
          f14,
          f15,
          f16,
          f17,
          f18,
          f19,
          f20,
          f21,
          f22,
          _,
          ext1,
          ext2,
          ext3,
          ext4,
          ext5,
          ext6,
          ext7,
          ext8,
          ext9,
          ext10,
          ext11,
          ext12,
          ext13,
          ext14,
          ext15,
          ext16,
          ext17,
          ext18,
          ext19,
          ext20,
          ext21,
          ext22
          ) =>
        DynamicValue.Record(
          ListMap(
            f1.label  -> fromSchemaAndValue(f1.schema, ext1(value)),
            f2.label  -> fromSchemaAndValue(f2.schema, ext2(value)),
            f3.label  -> fromSchemaAndValue(f3.schema, ext3(value)),
            f4.label  -> fromSchemaAndValue(f4.schema, ext4(value)),
            f5.label  -> fromSchemaAndValue(f5.schema, ext5(value)),
            f6.label  -> fromSchemaAndValue(f6.schema, ext6(value)),
            f7.label  -> fromSchemaAndValue(f7.schema, ext7(value)),
            f8.label  -> fromSchemaAndValue(f8.schema, ext8(value)),
            f9.label  -> fromSchemaAndValue(f9.schema, ext9(value)),
            f10.label -> fromSchemaAndValue(f10.schema, ext10(value)),
            f11.label -> fromSchemaAndValue(f11.schema, ext11(value)),
            f12.label -> fromSchemaAndValue(f12.schema, ext12(value)),
            f13.label -> fromSchemaAndValue(f13.schema, ext13(value)),
            f14.label -> fromSchemaAndValue(f14.schema, ext14(value)),
            f15.label -> fromSchemaAndValue(f15.schema, ext15(value)),
            f16.label -> fromSchemaAndValue(f16.schema, ext16(value)),
            f17.label -> fromSchemaAndValue(f17.schema, ext17(value)),
            f18.label -> fromSchemaAndValue(f18.schema, ext18(value)),
            f19.label -> fromSchemaAndValue(f19.schema, ext19(value)),
            f20.label -> fromSchemaAndValue(f20.schema, ext20(value)),
            f21.label -> fromSchemaAndValue(f21.schema, ext21(value)),
            f22.label -> fromSchemaAndValue(f22.schema, ext22(value))
          )
        )
    }

  def decodeStructure(
    values: ListMap[String, DynamicValue],
    structure: Chunk[Schema.Field[_]]
  ): Either[String, ListMap[String, _]] = {
    val keys = values.keySet
    keys.foldLeft[Either[String, ListMap[String, Any]]](Right(ListMap.empty)) {
      case (Right(record), key) =>
        (structure.find(_.label == key), values.get(key)) match {
          case (Some(field), Some(value)) =>
            value.toTypedValue(field.schema) match {
              case Left(error)  => Left(error)
              case Right(value) => Right(record + (key -> value))
            }
          case _ =>
            Left(s"$values and $structure have incompatible shape")
        }
      case (Left(string), _) => Left(string)
    }
  }

  final case class Record(values: ListMap[String, DynamicValue]) extends DynamicValue
  final case class Enumeration(value: (String, DynamicValue))    extends DynamicValue

  final case class Sequence(values: Chunk[DynamicValue]) extends DynamicValue

  sealed case class Primitive[A](value: A, standardType: StandardType[A]) extends DynamicValue

  sealed case class Singleton[A](instance: A) extends DynamicValue

  final case class SomeValue(value: DynamicValue) extends DynamicValue

  final case class Transform(value: DynamicValue) extends DynamicValue

  case object NoneValue extends DynamicValue

  sealed case class Tuple(left: DynamicValue, right: DynamicValue) extends DynamicValue

  final case class LeftValue(value: DynamicValue) extends DynamicValue

  final case class RightValue(value: DynamicValue) extends DynamicValue

  final case class DynamicSchema(spec: MetaSchema) extends DynamicValue

  final case class Error(message: String) extends DynamicValue
}
