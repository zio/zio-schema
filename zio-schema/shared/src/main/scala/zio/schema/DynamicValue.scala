package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk

trait DynamicValue { self =>

  def transform(transforms: Chunk[Migration]): Either[String, DynamicValue] =
    transforms.foldRight[Either[String, DynamicValue]](Right(self)) {
      case (transform, Right(value)) => transform.migrate(value)
      case (_, error @ Left(_))      => error
    }

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

      case (DynamicValue.Enumeration((key, value)), s: Schema.Enum[A]) =>
        s.structure.get(key) match {
          case Some(schema) => value.toTypedValue(schema).asInstanceOf[Either[String, A]]
          case None         => Left(s"Failed to find case $key in enum $s")
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

  //scalafmt: { maxColumn = 400 }
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

      case Schema.Enum4(case1, case2, case3, case4) =>
        (case1.deconstruct(value), case2.deconstruct(value), case3.deconstruct(value), case4.deconstruct(value)) match {
          case (Some(v1), _, _, _) => DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _) => DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _) => DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4)) => DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum5(case1, case2, case3, case4, case5) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _) => DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _) => DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _) => DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _) => DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5)) => DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum6(case1, case2, case3, case4, case5, case6) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _) => DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _) => DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _) => DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _) => DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _) => DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6)) => DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum7(case1, case2, case3, case4, case5, case6, case7) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _) => DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _) => DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _) => DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _) => DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _) => DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _) => DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7)) => DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum8(case1, case2, case3, case4, case5, case6, case7, case8) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8)) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum9(case1, case2, case3, case4, case5, case6, case7, case8, case9) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value),
          case9.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9)) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum10(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value),
          case9.deconstruct(value),
          case10.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10)) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum11(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value),
          case9.deconstruct(value),
          case10.deconstruct(value),
          case11.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11)) =>
            DynamicValue.Enumeration(case11.id -> fromSchemaAndValue(case11.codec, v11))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum12(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value),
          case9.deconstruct(value),
          case10.deconstruct(value),
          case11.deconstruct(value),
          case12.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _) =>
            DynamicValue.Enumeration(case11.id -> fromSchemaAndValue(case11.codec, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12)) =>
            DynamicValue.Enumeration(case12.id -> fromSchemaAndValue(case12.codec, v12))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum13(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13) =>
        (case1.deconstruct(value), case2.deconstruct(value), case3.deconstruct(value), case4.deconstruct(value), case5.deconstruct(value), case6.deconstruct(value), case7.deconstruct(value), case8.deconstruct(value), case9.deconstruct(value), case10.deconstruct(value), case11.deconstruct(value), case12.deconstruct(value), case13.deconstruct(value)) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _) =>
            DynamicValue.Enumeration(case11.id -> fromSchemaAndValue(case11.codec, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _) =>
            DynamicValue.Enumeration(case12.id -> fromSchemaAndValue(case12.codec, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13)) =>
            DynamicValue.Enumeration(case13.id -> fromSchemaAndValue(case13.codec, v13))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum14(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14) =>
        (case1.deconstruct(value), case2.deconstruct(value), case3.deconstruct(value), case4.deconstruct(value), case5.deconstruct(value), case6.deconstruct(value), case7.deconstruct(value), case8.deconstruct(value), case9.deconstruct(value), case10.deconstruct(value), case11.deconstruct(value), case12.deconstruct(value), case13.deconstruct(value), case14.deconstruct(value)) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _) =>
            DynamicValue.Enumeration(case11.id -> fromSchemaAndValue(case11.codec, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _) =>
            DynamicValue.Enumeration(case12.id -> fromSchemaAndValue(case12.codec, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _) =>
            DynamicValue.Enumeration(case13.id -> fromSchemaAndValue(case13.codec, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14)) =>
            DynamicValue.Enumeration(case14.id -> fromSchemaAndValue(case14.codec, v14))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum15(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value),
          case9.deconstruct(value),
          case10.deconstruct(value),
          case11.deconstruct(value),
          case12.deconstruct(value),
          case13.deconstruct(value),
          case14.deconstruct(value),
          case15.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _) =>
            DynamicValue.Enumeration(case11.id -> fromSchemaAndValue(case11.codec, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _) =>
            DynamicValue.Enumeration(case12.id -> fromSchemaAndValue(case12.codec, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _) =>
            DynamicValue.Enumeration(case13.id -> fromSchemaAndValue(case13.codec, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _) =>
            DynamicValue.Enumeration(case14.id -> fromSchemaAndValue(case14.codec, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15)) =>
            DynamicValue.Enumeration(case15.id -> fromSchemaAndValue(case15.codec, v15))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum16(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value),
          case9.deconstruct(value),
          case10.deconstruct(value),
          case11.deconstruct(value),
          case12.deconstruct(value),
          case13.deconstruct(value),
          case14.deconstruct(value),
          case15.deconstruct(value),
          case16.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _) =>
            DynamicValue.Enumeration(case11.id -> fromSchemaAndValue(case11.codec, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _) =>
            DynamicValue.Enumeration(case12.id -> fromSchemaAndValue(case12.codec, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _) =>
            DynamicValue.Enumeration(case13.id -> fromSchemaAndValue(case13.codec, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _) =>
            DynamicValue.Enumeration(case14.id -> fromSchemaAndValue(case14.codec, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _) =>
            DynamicValue.Enumeration(case15.id -> fromSchemaAndValue(case15.codec, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16)) =>
            DynamicValue.Enumeration(case16.id -> fromSchemaAndValue(case16.codec, v16))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum17(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value),
          case9.deconstruct(value),
          case10.deconstruct(value),
          case11.deconstruct(value),
          case12.deconstruct(value),
          case13.deconstruct(value),
          case14.deconstruct(value),
          case15.deconstruct(value),
          case16.deconstruct(value),
          case17.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case11.id -> fromSchemaAndValue(case11.codec, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _, _) =>
            DynamicValue.Enumeration(case12.id -> fromSchemaAndValue(case12.codec, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _, _) =>
            DynamicValue.Enumeration(case13.id -> fromSchemaAndValue(case13.codec, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _, _) =>
            DynamicValue.Enumeration(case14.id -> fromSchemaAndValue(case14.codec, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _, _) =>
            DynamicValue.Enumeration(case15.id -> fromSchemaAndValue(case15.codec, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16), _) =>
            DynamicValue.Enumeration(case16.id -> fromSchemaAndValue(case16.codec, v16))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v17)) =>
            DynamicValue.Enumeration(case17.id -> fromSchemaAndValue(case17.codec, v17))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum18(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value),
          case9.deconstruct(value),
          case10.deconstruct(value),
          case11.deconstruct(value),
          case12.deconstruct(value),
          case13.deconstruct(value),
          case14.deconstruct(value),
          case15.deconstruct(value),
          case16.deconstruct(value),
          case17.deconstruct(value),
          case18.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case11.id -> fromSchemaAndValue(case11.codec, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case12.id -> fromSchemaAndValue(case12.codec, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _, _, _) =>
            DynamicValue.Enumeration(case13.id -> fromSchemaAndValue(case13.codec, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _, _, _) =>
            DynamicValue.Enumeration(case14.id -> fromSchemaAndValue(case14.codec, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _, _, _) =>
            DynamicValue.Enumeration(case15.id -> fromSchemaAndValue(case15.codec, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16), _, _) =>
            DynamicValue.Enumeration(case16.id -> fromSchemaAndValue(case16.codec, v16))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v17), _) =>
            DynamicValue.Enumeration(case17.id -> fromSchemaAndValue(case17.codec, v17))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v18)) =>
            DynamicValue.Enumeration(case18.id -> fromSchemaAndValue(case18.codec, v18))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum19(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value),
          case9.deconstruct(value),
          case10.deconstruct(value),
          case11.deconstruct(value),
          case12.deconstruct(value),
          case13.deconstruct(value),
          case14.deconstruct(value),
          case15.deconstruct(value),
          case16.deconstruct(value),
          case17.deconstruct(value),
          case18.deconstruct(value),
          case19.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case11.id -> fromSchemaAndValue(case11.codec, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case12.id -> fromSchemaAndValue(case12.codec, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case13.id -> fromSchemaAndValue(case13.codec, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _, _, _, _) =>
            DynamicValue.Enumeration(case14.id -> fromSchemaAndValue(case14.codec, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _, _, _, _) =>
            DynamicValue.Enumeration(case15.id -> fromSchemaAndValue(case15.codec, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16), _, _, _) =>
            DynamicValue.Enumeration(case16.id -> fromSchemaAndValue(case16.codec, v16))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v17), _, _) =>
            DynamicValue.Enumeration(case17.id -> fromSchemaAndValue(case17.codec, v17))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v18), _) =>
            DynamicValue.Enumeration(case18.id -> fromSchemaAndValue(case18.codec, v18))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v19)) =>
            DynamicValue.Enumeration(case19.id -> fromSchemaAndValue(case19.codec, v19))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum20(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value),
          case9.deconstruct(value),
          case10.deconstruct(value),
          case11.deconstruct(value),
          case12.deconstruct(value),
          case13.deconstruct(value),
          case14.deconstruct(value),
          case15.deconstruct(value),
          case16.deconstruct(value),
          case17.deconstruct(value),
          case18.deconstruct(value),
          case19.deconstruct(value),
          case20.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case11.id -> fromSchemaAndValue(case11.codec, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case12.id -> fromSchemaAndValue(case12.codec, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case13.id -> fromSchemaAndValue(case13.codec, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case14.id -> fromSchemaAndValue(case14.codec, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _, _, _, _, _) =>
            DynamicValue.Enumeration(case15.id -> fromSchemaAndValue(case15.codec, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16), _, _, _, _) =>
            DynamicValue.Enumeration(case16.id -> fromSchemaAndValue(case16.codec, v16))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v17), _, _, _) =>
            DynamicValue.Enumeration(case17.id -> fromSchemaAndValue(case17.codec, v17))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v18), _, _) =>
            DynamicValue.Enumeration(case18.id -> fromSchemaAndValue(case18.codec, v18))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v19), _) =>
            DynamicValue.Enumeration(case19.id -> fromSchemaAndValue(case19.codec, v19))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v20)) =>
            DynamicValue.Enumeration(case20.id -> fromSchemaAndValue(case20.codec, v20))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum21(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, case21) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value),
          case9.deconstruct(value),
          case10.deconstruct(value),
          case11.deconstruct(value),
          case12.deconstruct(value),
          case13.deconstruct(value),
          case14.deconstruct(value),
          case15.deconstruct(value),
          case16.deconstruct(value),
          case17.deconstruct(value),
          case18.deconstruct(value),
          case19.deconstruct(value),
          case20.deconstruct(value),
          case21.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case11.id -> fromSchemaAndValue(case11.codec, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case12.id -> fromSchemaAndValue(case12.codec, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case13.id -> fromSchemaAndValue(case13.codec, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case14.id -> fromSchemaAndValue(case14.codec, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case15.id -> fromSchemaAndValue(case15.codec, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16), _, _, _, _, _) =>
            DynamicValue.Enumeration(case16.id -> fromSchemaAndValue(case16.codec, v16))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v17), _, _, _, _) =>
            DynamicValue.Enumeration(case17.id -> fromSchemaAndValue(case17.codec, v17))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v18), _, _, _) =>
            DynamicValue.Enumeration(case18.id -> fromSchemaAndValue(case18.codec, v18))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v19), _, _) =>
            DynamicValue.Enumeration(case19.id -> fromSchemaAndValue(case19.codec, v19))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v20), _) =>
            DynamicValue.Enumeration(case20.id -> fromSchemaAndValue(case20.codec, v20))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v21)) =>
            DynamicValue.Enumeration(case21.id -> fromSchemaAndValue(case21.codec, v21))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum22(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, case21, case22) =>
        (
          case1.deconstruct(value),
          case2.deconstruct(value),
          case3.deconstruct(value),
          case4.deconstruct(value),
          case5.deconstruct(value),
          case6.deconstruct(value),
          case7.deconstruct(value),
          case8.deconstruct(value),
          case9.deconstruct(value),
          case10.deconstruct(value),
          case11.deconstruct(value),
          case12.deconstruct(value),
          case13.deconstruct(value),
          case14.deconstruct(value),
          case15.deconstruct(value),
          case16.deconstruct(value),
          case17.deconstruct(value),
          case18.deconstruct(value),
          case19.deconstruct(value),
          case20.deconstruct(value),
          case21.deconstruct(value),
          case22.deconstruct(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case1.id -> fromSchemaAndValue(case1.codec, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case2.id -> fromSchemaAndValue(case2.codec, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case3.id -> fromSchemaAndValue(case3.codec, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case4.id -> fromSchemaAndValue(case4.codec, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case5.id -> fromSchemaAndValue(case5.codec, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case6.id -> fromSchemaAndValue(case6.codec, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case7.id -> fromSchemaAndValue(case7.codec, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case8.id -> fromSchemaAndValue(case8.codec, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case9.id -> fromSchemaAndValue(case9.codec, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case10.id -> fromSchemaAndValue(case10.codec, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case11.id -> fromSchemaAndValue(case11.codec, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case12.id -> fromSchemaAndValue(case12.codec, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case13.id -> fromSchemaAndValue(case13.codec, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case14.id -> fromSchemaAndValue(case14.codec, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case15.id -> fromSchemaAndValue(case15.codec, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(case16.id -> fromSchemaAndValue(case16.codec, v16))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v17), _, _, _, _, _) =>
            DynamicValue.Enumeration(case17.id -> fromSchemaAndValue(case17.codec, v17))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v18), _, _, _, _) =>
            DynamicValue.Enumeration(case18.id -> fromSchemaAndValue(case18.codec, v18))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v19), _, _, _) =>
            DynamicValue.Enumeration(case19.id -> fromSchemaAndValue(case19.codec, v19))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v20), _, _) =>
            DynamicValue.Enumeration(case20.id -> fromSchemaAndValue(case20.codec, v20))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v21), _) =>
            DynamicValue.Enumeration(case21.id -> fromSchemaAndValue(case21.codec, v21))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v22)) =>
            DynamicValue.Enumeration(case22.id -> fromSchemaAndValue(case22.codec, v22))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }
      //scalafmt: { maxColumn = 120 }

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

      case Schema.Meta(ast) => DynamicValue.DynamicAst(ast)

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

  final case class DynamicAst(ast: SchemaAst) extends DynamicValue

  final case class Error(message: String) extends DynamicValue
}
