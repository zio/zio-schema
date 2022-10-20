package zio.schema

import java.math.{ BigDecimal, BigInteger }
import java.time._
import java.time.format.DateTimeFormatter
import java.util.UUID

import scala.collection.immutable.ListMap

import zio.schema.meta.{ MetaSchema, Migration }
import zio.{ Chunk, Unsafe }

sealed trait DynamicValue {
  self =>

  def transform(transforms: Chunk[Migration]): Either[String, DynamicValue] =
    transforms.foldRight[Either[String, DynamicValue]](Right(self)) {
      case (transform, Right(value)) => transform.migrate(value)
      case (_, error @ Left(_))      => error
    }

  def toTypedValue[A](implicit schema: Schema[A]): Either[String, A] =
    toTypedValueLazyError.left.map(_.apply())

  def toTypedValueOption[A](implicit schema: Schema[A]): Option[A] =
    toTypedValueLazyError.toOption

  private def toTypedValueLazyError[A](implicit schema: Schema[A]): Either[() => String, A] =
    (self, schema) match {
      case (DynamicValue.Primitive(value, p), Schema.Primitive(p2, _)) if p == p2 =>
        Right(value.asInstanceOf[A])

      case (DynamicValue.Record(_, values), Schema.GenericRecord(_, structure, _)) =>
        DynamicValue.decodeStructure(values, structure.toChunk).asInstanceOf[Either[() => String, A]]

      case (DynamicValue.Record(_, values), s: Schema.Record[A]) =>
        DynamicValue
          .decodeStructure(values, s.fields)
          .map(m => Chunk.fromIterable(m.values))
          .flatMap(values => s.construct(values)(Unsafe.unsafe).left.map(err => () => err))

      case (DynamicValue.Enumeration(_, (key, value)), s: Schema.Enum[A]) =>
        s.caseOf(key) match {
          case Some(caseValue) => value.toTypedValueLazyError(caseValue.schema).asInstanceOf[Either[() => String, A]]
          case None            => Left(() => s"Failed to find case $key in enumN $s")
        }

      case (DynamicValue.LeftValue(value), Schema.Either(schema1, _, _)) =>
        value.toTypedValueLazyError(schema1).map(Left(_))

      case (DynamicValue.RightValue(value), Schema.Either(_, schema1, _)) =>
        value.toTypedValueLazyError(schema1).map(Right(_))

      case (DynamicValue.Tuple(leftValue, rightValue), Schema.Tuple2(leftSchema, rightSchema, _)) =>
        val typedLeft  = leftValue.toTypedValueLazyError(leftSchema)
        val typedRight = rightValue.toTypedValueLazyError(rightSchema)
        (typedLeft, typedRight) match {
          case (Left(e1), Left(e2)) =>
            Left(() => s"Converting generic tuple to typed value failed with errors ${e1()} and ${e2()}")
          case (_, Left(e))         => Left(e)
          case (Left(e), _)         => Left(e)
          case (Right(a), Right(b)) => Right(a -> b)
        }

      case (DynamicValue.Sequence(values), schema: Schema.Sequence[col, t, _]) =>
        values
          .foldLeft[Either[() => String, Chunk[t]]](Right[() => String, Chunk[t]](Chunk.empty)) {
            case (err @ Left(_), _) => err
            case (Right(values), value) =>
              value.toTypedValueLazyError(schema.elementSchema).map(values :+ _)
          }
          .map(schema.fromChunk)

      case (DynamicValue.SetValue(values), schema: Schema.Set[t]) =>
        values.foldLeft[Either[() => String, Set[t]]](Right[() => String, Set[t]](Set.empty)) {
          case (err @ Left(_), _) => err
          case (Right(values), value) =>
            value.toTypedValueLazyError(schema.elementSchema).map(values + _)
        }

      case (DynamicValue.SomeValue(value), Schema.Optional(schema: Schema[_], _)) =>
        value.toTypedValueLazyError(schema).map(Some(_))

      case (DynamicValue.NoneValue, Schema.Optional(_, _)) =>
        Right(None)

      case (value, Schema.Transform(schema, f, _, _, _)) =>
        value.toTypedValueLazyError(schema).flatMap(value => f(value).left.map(err => () => err))

      case (DynamicValue.Dictionary(entries), schema: Schema.Map[k, v]) =>
        entries.foldLeft[Either[() => String, Map[k, v]]](Right[() => String, Map[k, v]](Map.empty)) {
          case (err @ Left(_), _) => err
          case (Right(map), entry) => {
            for {
              key   <- entry._1.toTypedValueLazyError(schema.keySchema)
              value <- entry._2.toTypedValueLazyError(schema.valueSchema)
            } yield map ++ Map(key -> value)
          }
        }

      case (_, l @ Schema.Lazy(_)) =>
        toTypedValueLazyError(l.schema)

      case (DynamicValue.Error(message), _) =>
        Left(() => message)

      case (DynamicValue.Tuple(dyn, DynamicValue.DynamicAst(ast)), _) =>
        val valueSchema = ast.toSchema.asInstanceOf[Schema[Any]]
        dyn.toTypedValueLazyError(valueSchema).map(a => (a -> valueSchema).asInstanceOf[A])

      case (dyn, Schema.Dynamic(_)) => Right(dyn)

      case _ =>
        Left(() => s"Failed to cast $self to schema $schema")
    }

}

object DynamicValue {

  //scalafmt: { maxColumn = 400 }
  def fromSchemaAndValue[A](schema: Schema[A], value: A): DynamicValue =
    schema match {

      case l @ Schema.Lazy(_) => fromSchemaAndValue(l.schema, value)

      case Schema.Primitive(p, _) => DynamicValue.Primitive(value, p)

      case Schema.GenericRecord(id, structure, _) =>
        val map: ListMap[String, _] = value
        DynamicValue.Record(
          id,
          ListMap.empty ++ structure.toChunk.map {
            case Schema.Field(key, schema: Schema[a], _, _, _, _) =>
              key -> fromSchemaAndValue(schema, map(key).asInstanceOf[a])
          }
        )

      case Schema.Enum1(id, case1, _) =>
        DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, case1.deconstruct(value)))

      case Schema.Enum2(id, case1, case2, _) =>
        (case1.deconstructOption(value), case2.deconstructOption(value)) match {
          case (Some(v1), _) => DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2)) => DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum3(id, case1, case2, case3, _) =>
        (case1.deconstructOption(value), case2.deconstructOption(value), case3.deconstructOption(value)) match {
          case (Some(v1), _, _) => DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _) => DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3)) => DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum4(id, case1, case2, case3, case4, _) =>
        (case1.deconstructOption(value), case2.deconstructOption(value), case3.deconstructOption(value), case4.deconstructOption(value)) match {
          case (Some(v1), _, _, _) => DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _) => DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _) => DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4)) => DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum5(id, case1, case2, case3, case4, case5, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _) => DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _) => DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _) => DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _) => DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5)) => DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum6(id, case1, case2, case3, case4, case5, case6, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _) => DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _) => DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _) => DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _) => DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _) => DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6)) => DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum7(id, case1, case2, case3, case4, case5, case6, case7, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _) => DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _) => DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _) => DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _) => DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _) => DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _) => DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7)) => DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum8(id, case1, case2, case3, case4, case5, case6, case7, case8, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8)) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum9(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9)) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum10(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10)) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum11(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value),
          case11.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11)) =>
            DynamicValue.Enumeration(id, case11.id -> fromSchemaAndValue(case11.schema, v11))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum12(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value),
          case11.deconstructOption(value),
          case12.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _) =>
            DynamicValue.Enumeration(id, case11.id -> fromSchemaAndValue(case11.schema, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12)) =>
            DynamicValue.Enumeration(id, case12.id -> fromSchemaAndValue(case12.schema, v12))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum13(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value),
          case11.deconstructOption(value),
          case12.deconstructOption(value),
          case13.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _) =>
            DynamicValue.Enumeration(id, case11.id -> fromSchemaAndValue(case11.schema, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _) =>
            DynamicValue.Enumeration(id, case12.id -> fromSchemaAndValue(case12.schema, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13)) =>
            DynamicValue.Enumeration(id, case13.id -> fromSchemaAndValue(case13.schema, v13))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum14(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value),
          case11.deconstructOption(value),
          case12.deconstructOption(value),
          case13.deconstructOption(value),
          case14.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _) =>
            DynamicValue.Enumeration(id, case11.id -> fromSchemaAndValue(case11.schema, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _) =>
            DynamicValue.Enumeration(id, case12.id -> fromSchemaAndValue(case12.schema, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _) =>
            DynamicValue.Enumeration(id, case13.id -> fromSchemaAndValue(case13.schema, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14)) =>
            DynamicValue.Enumeration(id, case14.id -> fromSchemaAndValue(case14.schema, v14))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum15(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value),
          case11.deconstructOption(value),
          case12.deconstructOption(value),
          case13.deconstructOption(value),
          case14.deconstructOption(value),
          case15.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _) =>
            DynamicValue.Enumeration(id, case11.id -> fromSchemaAndValue(case11.schema, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _) =>
            DynamicValue.Enumeration(id, case12.id -> fromSchemaAndValue(case12.schema, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _) =>
            DynamicValue.Enumeration(id, case13.id -> fromSchemaAndValue(case13.schema, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _) =>
            DynamicValue.Enumeration(id, case14.id -> fromSchemaAndValue(case14.schema, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15)) =>
            DynamicValue.Enumeration(id, case15.id -> fromSchemaAndValue(case15.schema, v15))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum16(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value),
          case11.deconstructOption(value),
          case12.deconstructOption(value),
          case13.deconstructOption(value),
          case14.deconstructOption(value),
          case15.deconstructOption(value),
          case16.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case11.id -> fromSchemaAndValue(case11.schema, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _) =>
            DynamicValue.Enumeration(id, case12.id -> fromSchemaAndValue(case12.schema, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _) =>
            DynamicValue.Enumeration(id, case13.id -> fromSchemaAndValue(case13.schema, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _) =>
            DynamicValue.Enumeration(id, case14.id -> fromSchemaAndValue(case14.schema, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _) =>
            DynamicValue.Enumeration(id, case15.id -> fromSchemaAndValue(case15.schema, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16)) =>
            DynamicValue.Enumeration(id, case16.id -> fromSchemaAndValue(case16.schema, v16))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum17(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value),
          case11.deconstructOption(value),
          case12.deconstructOption(value),
          case13.deconstructOption(value),
          case14.deconstructOption(value),
          case15.deconstructOption(value),
          case16.deconstructOption(value),
          case17.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case11.id -> fromSchemaAndValue(case11.schema, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case12.id -> fromSchemaAndValue(case12.schema, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _, _) =>
            DynamicValue.Enumeration(id, case13.id -> fromSchemaAndValue(case13.schema, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _, _) =>
            DynamicValue.Enumeration(id, case14.id -> fromSchemaAndValue(case14.schema, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _, _) =>
            DynamicValue.Enumeration(id, case15.id -> fromSchemaAndValue(case15.schema, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16), _) =>
            DynamicValue.Enumeration(id, case16.id -> fromSchemaAndValue(case16.schema, v16))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v17)) =>
            DynamicValue.Enumeration(id, case17.id -> fromSchemaAndValue(case17.schema, v17))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum18(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value),
          case11.deconstructOption(value),
          case12.deconstructOption(value),
          case13.deconstructOption(value),
          case14.deconstructOption(value),
          case15.deconstructOption(value),
          case16.deconstructOption(value),
          case17.deconstructOption(value),
          case18.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case11.id -> fromSchemaAndValue(case11.schema, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case12.id -> fromSchemaAndValue(case12.schema, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case13.id -> fromSchemaAndValue(case13.schema, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _, _, _) =>
            DynamicValue.Enumeration(id, case14.id -> fromSchemaAndValue(case14.schema, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _, _, _) =>
            DynamicValue.Enumeration(id, case15.id -> fromSchemaAndValue(case15.schema, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16), _, _) =>
            DynamicValue.Enumeration(id, case16.id -> fromSchemaAndValue(case16.schema, v16))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v17), _) =>
            DynamicValue.Enumeration(id, case17.id -> fromSchemaAndValue(case17.schema, v17))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v18)) =>
            DynamicValue.Enumeration(id, case18.id -> fromSchemaAndValue(case18.schema, v18))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum19(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value),
          case11.deconstructOption(value),
          case12.deconstructOption(value),
          case13.deconstructOption(value),
          case14.deconstructOption(value),
          case15.deconstructOption(value),
          case16.deconstructOption(value),
          case17.deconstructOption(value),
          case18.deconstructOption(value),
          case19.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case11.id -> fromSchemaAndValue(case11.schema, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case12.id -> fromSchemaAndValue(case12.schema, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case13.id -> fromSchemaAndValue(case13.schema, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case14.id -> fromSchemaAndValue(case14.schema, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _, _, _, _) =>
            DynamicValue.Enumeration(id, case15.id -> fromSchemaAndValue(case15.schema, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16), _, _, _) =>
            DynamicValue.Enumeration(id, case16.id -> fromSchemaAndValue(case16.schema, v16))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v17), _, _) =>
            DynamicValue.Enumeration(id, case17.id -> fromSchemaAndValue(case17.schema, v17))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v18), _) =>
            DynamicValue.Enumeration(id, case18.id -> fromSchemaAndValue(case18.schema, v18))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v19)) =>
            DynamicValue.Enumeration(id, case19.id -> fromSchemaAndValue(case19.schema, v19))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum20(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value),
          case11.deconstructOption(value),
          case12.deconstructOption(value),
          case13.deconstructOption(value),
          case14.deconstructOption(value),
          case15.deconstructOption(value),
          case16.deconstructOption(value),
          case17.deconstructOption(value),
          case18.deconstructOption(value),
          case19.deconstructOption(value),
          case20.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case11.id -> fromSchemaAndValue(case11.schema, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case12.id -> fromSchemaAndValue(case12.schema, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case13.id -> fromSchemaAndValue(case13.schema, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case14.id -> fromSchemaAndValue(case14.schema, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case15.id -> fromSchemaAndValue(case15.schema, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16), _, _, _, _) =>
            DynamicValue.Enumeration(id, case16.id -> fromSchemaAndValue(case16.schema, v16))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v17), _, _, _) =>
            DynamicValue.Enumeration(id, case17.id -> fromSchemaAndValue(case17.schema, v17))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v18), _, _) =>
            DynamicValue.Enumeration(id, case18.id -> fromSchemaAndValue(case18.schema, v18))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v19), _) =>
            DynamicValue.Enumeration(id, case19.id -> fromSchemaAndValue(case19.schema, v19))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v20)) =>
            DynamicValue.Enumeration(id, case20.id -> fromSchemaAndValue(case20.schema, v20))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum21(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, case21, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value),
          case11.deconstructOption(value),
          case12.deconstructOption(value),
          case13.deconstructOption(value),
          case14.deconstructOption(value),
          case15.deconstructOption(value),
          case16.deconstructOption(value),
          case17.deconstructOption(value),
          case18.deconstructOption(value),
          case19.deconstructOption(value),
          case20.deconstructOption(value),
          case21.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case11.id -> fromSchemaAndValue(case11.schema, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case12.id -> fromSchemaAndValue(case12.schema, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case13.id -> fromSchemaAndValue(case13.schema, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case14.id -> fromSchemaAndValue(case14.schema, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case15.id -> fromSchemaAndValue(case15.schema, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case16.id -> fromSchemaAndValue(case16.schema, v16))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v17), _, _, _, _) =>
            DynamicValue.Enumeration(id, case17.id -> fromSchemaAndValue(case17.schema, v17))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v18), _, _, _) =>
            DynamicValue.Enumeration(id, case18.id -> fromSchemaAndValue(case18.schema, v18))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v19), _, _) =>
            DynamicValue.Enumeration(id, case19.id -> fromSchemaAndValue(case19.schema, v19))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v20), _) =>
            DynamicValue.Enumeration(id, case20.id -> fromSchemaAndValue(case20.schema, v20))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v21)) =>
            DynamicValue.Enumeration(id, case21.id -> fromSchemaAndValue(case21.schema, v21))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }

      case Schema.Enum22(id, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, case21, case22, _) =>
        (
          case1.deconstructOption(value),
          case2.deconstructOption(value),
          case3.deconstructOption(value),
          case4.deconstructOption(value),
          case5.deconstructOption(value),
          case6.deconstructOption(value),
          case7.deconstructOption(value),
          case8.deconstructOption(value),
          case9.deconstructOption(value),
          case10.deconstructOption(value),
          case11.deconstructOption(value),
          case12.deconstructOption(value),
          case13.deconstructOption(value),
          case14.deconstructOption(value),
          case15.deconstructOption(value),
          case16.deconstructOption(value),
          case17.deconstructOption(value),
          case18.deconstructOption(value),
          case19.deconstructOption(value),
          case20.deconstructOption(value),
          case21.deconstructOption(value),
          case22.deconstructOption(value)
        ) match {
          case (Some(v1), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case1.id -> fromSchemaAndValue(case1.schema, v1))
          case (_, Some(v2), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case2.id -> fromSchemaAndValue(case2.schema, v2))
          case (_, _, Some(v3), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case3.id -> fromSchemaAndValue(case3.schema, v3))
          case (_, _, _, Some(v4), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case4.id -> fromSchemaAndValue(case4.schema, v4))
          case (_, _, _, _, Some(v5), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case5.id -> fromSchemaAndValue(case5.schema, v5))
          case (_, _, _, _, _, Some(v6), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case6.id -> fromSchemaAndValue(case6.schema, v6))
          case (_, _, _, _, _, _, Some(v7), _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case7.id -> fromSchemaAndValue(case7.schema, v7))
          case (_, _, _, _, _, _, _, Some(v8), _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case8.id -> fromSchemaAndValue(case8.schema, v8))
          case (_, _, _, _, _, _, _, _, Some(v9), _, _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case9.id -> fromSchemaAndValue(case9.schema, v9))
          case (_, _, _, _, _, _, _, _, _, Some(v10), _, _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case10.id -> fromSchemaAndValue(case10.schema, v10))
          case (_, _, _, _, _, _, _, _, _, _, Some(v11), _, _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case11.id -> fromSchemaAndValue(case11.schema, v11))
          case (_, _, _, _, _, _, _, _, _, _, _, Some(v12), _, _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case12.id -> fromSchemaAndValue(case12.schema, v12))
          case (_, _, _, _, _, _, _, _, _, _, _, _, Some(v13), _, _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case13.id -> fromSchemaAndValue(case13.schema, v13))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, Some(v14), _, _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case14.id -> fromSchemaAndValue(case14.schema, v14))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v15), _, _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case15.id -> fromSchemaAndValue(case15.schema, v15))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v16), _, _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case16.id -> fromSchemaAndValue(case16.schema, v16))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v17), _, _, _, _, _) =>
            DynamicValue.Enumeration(id, case17.id -> fromSchemaAndValue(case17.schema, v17))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v18), _, _, _, _) =>
            DynamicValue.Enumeration(id, case18.id -> fromSchemaAndValue(case18.schema, v18))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v19), _, _, _) =>
            DynamicValue.Enumeration(id, case19.id -> fromSchemaAndValue(case19.schema, v19))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v20), _, _) =>
            DynamicValue.Enumeration(id, case20.id -> fromSchemaAndValue(case20.schema, v20))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v21), _) =>
            DynamicValue.Enumeration(id, case21.id -> fromSchemaAndValue(case21.schema, v21))
          case (_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Some(v22)) =>
            DynamicValue.Enumeration(id, case22.id -> fromSchemaAndValue(case22.schema, v22))
          //This should never happen unless someone manually builds an Enum and doesn't include all cases
          case _ => DynamicValue.NoneValue
        }
      //scalafmt: { maxColumn = 120 }

      case Schema.EnumN(id, cases, _) =>
        cases.toSeq
          .find(_.deconstructOption(value).isDefined) match {
          case Some(c) =>
            DynamicValue.Enumeration(
              id,
              c.id -> fromSchemaAndValue(c.schema.asInstanceOf[Schema[Any]], c.deconstruct(value))
            )
          case None => DynamicValue.NoneValue
        }

      case Schema.Fail(message, _) => DynamicValue.Error(message)

      case Schema.Sequence(schema, _, toChunk, _, _) =>
        DynamicValue.Sequence(toChunk(value).map(fromSchemaAndValue(schema, _)))

      case Schema.Map(ks: Schema[k], vs: Schema[v], _) =>
        val entries = value.asInstanceOf[Map[k, v]].map {
          case (key, value) => (fromSchemaAndValue(ks, key), fromSchemaAndValue(vs, value))
        }
        DynamicValue.Dictionary(Chunk.fromIterable(entries))

      case Schema.Set(as: Schema[a], _) =>
        DynamicValue.SetValue(value.asInstanceOf[Set[a]].map(fromSchemaAndValue(as, _)))

      case schema: Schema.Either[l, r] =>
        value.asInstanceOf[Either[l, r]] match {
          case Left(value: l)  => DynamicValue.LeftValue(fromSchemaAndValue(schema.left, value))
          case Right(value: r) => DynamicValue.RightValue(fromSchemaAndValue(schema.right, value))
        }

      case schema: Schema.Tuple2[a, b] =>
        val (a: a, b: b) = value.asInstanceOf[(a, b)]
        DynamicValue.Tuple(fromSchemaAndValue(schema.left, a), fromSchemaAndValue(schema.right, b))

      case schema: Schema.Optional[a] =>
        value.asInstanceOf[Option[a]] match {
          case Some(value: a) => DynamicValue.SomeValue(fromSchemaAndValue(schema.schema, value))
          case None           => DynamicValue.NoneValue
        }

      case Schema.Transform(schema, _, g, _, _) =>
        g(value) match {
          case Left(message) => DynamicValue.Error(message)
          case Right(a)      => fromSchemaAndValue(schema, a)
        }

      case Schema.CaseClass0(id, _, _) =>
        DynamicValue.Record(id, ListMap())

      case Schema.CaseClass1(id, f, _, _) =>
        DynamicValue.Record(id, ListMap(f.name -> fromSchemaAndValue(f.schema, f.get(value))))

      case Schema.CaseClass2(id, f1, f2, _, _) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name -> fromSchemaAndValue(f2.schema, f2.get(value))
          )
        )
      case Schema.CaseClass3(id, f1, f2, f3, _, _) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name -> fromSchemaAndValue(f3.schema, f3.get(value))
          )
        )
      case Schema.CaseClass4(id, f1, f2, f3, f4, _, _) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name -> fromSchemaAndValue(f4.schema, f4.get(value))
          )
        )
      case Schema.CaseClass5(id, f1, f2, f3, f4, f5, _, _) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name -> fromSchemaAndValue(f5.schema, f5.get(value))
          )
        )
      case Schema.CaseClass6(id, f1, f2, f3, f4, f5, f6, _, _) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name -> fromSchemaAndValue(f6.schema, f6.get(value))
          )
        )
      case Schema.CaseClass7(id, f1, f2, f3, f4, f5, f6, f7, _, _) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name -> fromSchemaAndValue(f7.schema, f7.get(value))
          )
        )
      case Schema.CaseClass8(
          id,
          f1,
          f2,
          f3,
          f4,
          f5,
          f6,
          f7,
          f8,
          _,
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name -> fromSchemaAndValue(f8.schema, f8.get(value))
          )
        )
      case Schema.CaseClass9(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name -> fromSchemaAndValue(f9.schema, f9.get(value))
          )
        )
      case Schema.CaseClass10(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value))
          )
        )
      case Schema.CaseClass11(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value)),
            f11.name -> fromSchemaAndValue(f11.schema, f11.get(value))
          )
        )
      case Schema.CaseClass12(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value)),
            f11.name -> fromSchemaAndValue(f11.schema, f11.get(value)),
            f12.name -> fromSchemaAndValue(f12.schema, f12.get(value))
          )
        )
      case Schema.CaseClass13(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value)),
            f11.name -> fromSchemaAndValue(f11.schema, f11.get(value)),
            f12.name -> fromSchemaAndValue(f12.schema, f12.get(value)),
            f13.name -> fromSchemaAndValue(f13.schema, f13.get(value))
          )
        )
      case Schema.CaseClass14(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value)),
            f11.name -> fromSchemaAndValue(f11.schema, f11.get(value)),
            f12.name -> fromSchemaAndValue(f12.schema, f12.get(value)),
            f13.name -> fromSchemaAndValue(f13.schema, f13.get(value)),
            f14.name -> fromSchemaAndValue(f14.schema, f14.get(value))
          )
        )
      case Schema.CaseClass15(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value)),
            f11.name -> fromSchemaAndValue(f11.schema, f11.get(value)),
            f12.name -> fromSchemaAndValue(f12.schema, f12.get(value)),
            f13.name -> fromSchemaAndValue(f13.schema, f13.get(value)),
            f14.name -> fromSchemaAndValue(f14.schema, f14.get(value)),
            f15.name -> fromSchemaAndValue(f15.schema, f15.get(value))
          )
        )
      case Schema.CaseClass16(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value)),
            f11.name -> fromSchemaAndValue(f11.schema, f11.get(value)),
            f12.name -> fromSchemaAndValue(f12.schema, f12.get(value)),
            f13.name -> fromSchemaAndValue(f13.schema, f13.get(value)),
            f14.name -> fromSchemaAndValue(f14.schema, f14.get(value)),
            f15.name -> fromSchemaAndValue(f15.schema, f15.get(value)),
            f16.name -> fromSchemaAndValue(f16.schema, f16.get(value))
          )
        )
      case Schema.CaseClass17(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value)),
            f11.name -> fromSchemaAndValue(f11.schema, f11.get(value)),
            f12.name -> fromSchemaAndValue(f12.schema, f12.get(value)),
            f13.name -> fromSchemaAndValue(f13.schema, f13.get(value)),
            f14.name -> fromSchemaAndValue(f14.schema, f14.get(value)),
            f15.name -> fromSchemaAndValue(f15.schema, f15.get(value)),
            f16.name -> fromSchemaAndValue(f16.schema, f16.get(value)),
            f17.name -> fromSchemaAndValue(f17.schema, f17.get(value))
          )
        )
      case Schema.CaseClass18(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value)),
            f11.name -> fromSchemaAndValue(f11.schema, f11.get(value)),
            f12.name -> fromSchemaAndValue(f12.schema, f12.get(value)),
            f13.name -> fromSchemaAndValue(f13.schema, f13.get(value)),
            f14.name -> fromSchemaAndValue(f14.schema, f14.get(value)),
            f15.name -> fromSchemaAndValue(f15.schema, f15.get(value)),
            f16.name -> fromSchemaAndValue(f16.schema, f16.get(value)),
            f17.name -> fromSchemaAndValue(f17.schema, f17.get(value)),
            f18.name -> fromSchemaAndValue(f18.schema, f18.get(value))
          )
        )
      case Schema.CaseClass19(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value)),
            f11.name -> fromSchemaAndValue(f11.schema, f11.get(value)),
            f12.name -> fromSchemaAndValue(f12.schema, f12.get(value)),
            f13.name -> fromSchemaAndValue(f13.schema, f13.get(value)),
            f14.name -> fromSchemaAndValue(f14.schema, f14.get(value)),
            f15.name -> fromSchemaAndValue(f15.schema, f15.get(value)),
            f16.name -> fromSchemaAndValue(f16.schema, f16.get(value)),
            f17.name -> fromSchemaAndValue(f17.schema, f17.get(value)),
            f18.name -> fromSchemaAndValue(f18.schema, f18.get(value)),
            f19.name -> fromSchemaAndValue(f19.schema, f19.get(value))
          )
        )
      case Schema.CaseClass20(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value)),
            f11.name -> fromSchemaAndValue(f11.schema, f11.get(value)),
            f12.name -> fromSchemaAndValue(f12.schema, f12.get(value)),
            f13.name -> fromSchemaAndValue(f13.schema, f13.get(value)),
            f14.name -> fromSchemaAndValue(f14.schema, f14.get(value)),
            f15.name -> fromSchemaAndValue(f15.schema, f15.get(value)),
            f16.name -> fromSchemaAndValue(f16.schema, f16.get(value)),
            f17.name -> fromSchemaAndValue(f17.schema, f17.get(value)),
            f18.name -> fromSchemaAndValue(f18.schema, f18.get(value)),
            f19.name -> fromSchemaAndValue(f19.schema, f19.get(value)),
            f20.name -> fromSchemaAndValue(f20.schema, f20.get(value))
          )
        )
      case Schema.CaseClass21(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value)),
            f11.name -> fromSchemaAndValue(f11.schema, f11.get(value)),
            f12.name -> fromSchemaAndValue(f12.schema, f12.get(value)),
            f13.name -> fromSchemaAndValue(f13.schema, f13.get(value)),
            f14.name -> fromSchemaAndValue(f14.schema, f14.get(value)),
            f15.name -> fromSchemaAndValue(f15.schema, f15.get(value)),
            f16.name -> fromSchemaAndValue(f16.schema, f16.get(value)),
            f17.name -> fromSchemaAndValue(f17.schema, f17.get(value)),
            f18.name -> fromSchemaAndValue(f18.schema, f18.get(value)),
            f19.name -> fromSchemaAndValue(f19.schema, f19.get(value)),
            f20.name -> fromSchemaAndValue(f20.schema, f20.get(value)),
            f21.name -> fromSchemaAndValue(f21.schema, f21.get(value))
          )
        )
      case Schema.CaseClass22(
          id,
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
          _
          ) =>
        DynamicValue.Record(
          id,
          ListMap(
            f1.name  -> fromSchemaAndValue(f1.schema, f1.get(value)),
            f2.name  -> fromSchemaAndValue(f2.schema, f2.get(value)),
            f3.name  -> fromSchemaAndValue(f3.schema, f3.get(value)),
            f4.name  -> fromSchemaAndValue(f4.schema, f4.get(value)),
            f5.name  -> fromSchemaAndValue(f5.schema, f5.get(value)),
            f6.name  -> fromSchemaAndValue(f6.schema, f6.get(value)),
            f7.name  -> fromSchemaAndValue(f7.schema, f7.get(value)),
            f8.name  -> fromSchemaAndValue(f8.schema, f8.get(value)),
            f9.name  -> fromSchemaAndValue(f9.schema, f9.get(value)),
            f10.name -> fromSchemaAndValue(f10.schema, f10.get(value)),
            f11.name -> fromSchemaAndValue(f11.schema, f11.get(value)),
            f12.name -> fromSchemaAndValue(f12.schema, f12.get(value)),
            f13.name -> fromSchemaAndValue(f13.schema, f13.get(value)),
            f14.name -> fromSchemaAndValue(f14.schema, f14.get(value)),
            f15.name -> fromSchemaAndValue(f15.schema, f15.get(value)),
            f16.name -> fromSchemaAndValue(f16.schema, f16.get(value)),
            f17.name -> fromSchemaAndValue(f17.schema, f17.get(value)),
            f18.name -> fromSchemaAndValue(f18.schema, f18.get(value)),
            f19.name -> fromSchemaAndValue(f19.schema, f19.get(value)),
            f20.name -> fromSchemaAndValue(f20.schema, f20.get(value)),
            f21.name -> fromSchemaAndValue(f21.schema, f21.get(value)),
            f22.name -> fromSchemaAndValue(f22.schema, f22.get(value))
          )
        )
      case Schema.Dynamic(_) => value
    }

  def decodeStructure(
    values: ListMap[String, DynamicValue],
    structure: Chunk[Schema.Field[_, _]]
  ): Either[() => String, ListMap[String, _]] = {
    val keys = values.keySet
    keys.foldLeft[Either[() => String, ListMap[String, Any]]](Right(ListMap.empty)) {
      case (Right(record), key) =>
        (structure.find(_.name == key), values.get(key)) match {
          case (Some(field), Some(value)) =>
            value.toTypedValueLazyError(field.schema).map(value => (record + (key -> value)))
          case _ =>
            Left(() => s"$values and $structure have incompatible shape")
        }
      case (Left(string), _) => Left(string)
    }
  }

  final case class Record(id: TypeId, values: ListMap[String, DynamicValue]) extends DynamicValue

  final case class Enumeration(id: TypeId, value: (String, DynamicValue)) extends DynamicValue

  final case class Sequence(values: Chunk[DynamicValue]) extends DynamicValue

  final case class Dictionary(entries: Chunk[(DynamicValue, DynamicValue)]) extends DynamicValue

  final case class SetValue(values: Set[DynamicValue]) extends DynamicValue

  sealed case class Primitive[A](value: A, standardType: StandardType[A]) extends DynamicValue

  sealed case class Singleton[A](instance: A) extends DynamicValue

  final case class SomeValue(value: DynamicValue) extends DynamicValue

  case object NoneValue extends DynamicValue

  sealed case class Tuple(left: DynamicValue, right: DynamicValue) extends DynamicValue

  final case class LeftValue(value: DynamicValue) extends DynamicValue

  final case class RightValue(value: DynamicValue) extends DynamicValue

  final case class DynamicAst(ast: MetaSchema) extends DynamicValue

  final case class Error(message: String) extends DynamicValue

}

private[schema] object DynamicValueSchema {
  self =>

  def apply(): Schema[DynamicValue] = schema

  lazy val schema: Schema[DynamicValue] =
    Schema.EnumN(
      TypeId.fromTypeName("zio.schema.DynamicValue"),
      CaseSet
        .Cons(errorCase, CaseSet.Empty[DynamicValue]())
        .:+:(noneValueCase)
        .:+:(rightValueCase)
        .:+:(leftValueCase)
        .:+:(tupleCase)
        .:+:(someValueCase)
        .:+:(dictionaryCase)
        .:+:(sequenceCase)
        .:+:(setCase)
        .:+:(enumerationCase)
        .:+:(recordCase)
        .:+:(dynamicAstCase)
        .:+:(primitiveUnitCase)
        .:+:(primitiveStringCase)
        .:+:(primitiveBooleanCase)
        .:+:(primitiveShortCase)
        .:+:(primitiveIntCase)
        .:+:(primitiveLongCase)
        .:+:(primitiveFloatCase)
        .:+:(primitiveDoubleCase)
        .:+:(primitiveBinaryCase)
        .:+:(primitiveCharCase)
        .:+:(primitiveBigDecimalCase)
        .:+:(primitiveBigIntegerCase)
        .:+:(primitiveDayOfWeekCase)
        .:+:(primitiveMonthCase)
        .:+:(primitiveMonthDayCase)
        .:+:(primitivePeriodCase)
        .:+:(primitiveYearCase)
        .:+:(primitiveYearMonthCase)
        .:+:(primitiveZoneIdCase)
        .:+:(primitiveZoneOffsetCase)
        .:+:(primitiveInstantCase)
        .:+:(primitiveDurationCase)
        .:+:(primitiveLocalDateCase)
        .:+:(primitiveLocalTimeCase)
        .:+:(primitiveLocalDateTimeCase)
        .:+:(primitiveOffsetTimeCase)
        .:+:(primitiveOffsetDateTimeCase)
        .:+:(primitiveZonedDateTimeCase)
        .:+:(primitiveUUIDCase)
        .:+:(singletonCase)
    )

  implicit val instantStandardType: StandardType[Instant] =
    StandardType.InstantType(DateTimeFormatter.ISO_INSTANT)

  implicit val localDateStandardType: StandardType[LocalDate] =
    StandardType.LocalDateType(DateTimeFormatter.ISO_LOCAL_DATE)

  implicit val localTimeStandardType: StandardType[LocalTime] =
    StandardType.LocalTimeType(DateTimeFormatter.ISO_LOCAL_TIME)

  implicit val localDateTimeStandardType: StandardType[LocalDateTime] =
    StandardType.LocalDateTimeType(DateTimeFormatter.ISO_LOCAL_DATE_TIME)

  implicit val offsetTimeStandardType: StandardType[OffsetTime] =
    StandardType.OffsetTimeType(DateTimeFormatter.ISO_OFFSET_TIME)

  implicit val offsetDateTimeStandardType: StandardType[OffsetDateTime] =
    StandardType.OffsetDateTimeType(DateTimeFormatter.ISO_OFFSET_DATE_TIME)

  implicit val zonedDateTimeStandardType: StandardType[ZonedDateTime] =
    StandardType.ZonedDateTimeType(DateTimeFormatter.ISO_ZONED_DATE_TIME)

  private val errorCase: Schema.Case[DynamicValue, DynamicValue.Error] =
    Schema.Case(
      "Error",
      Schema.CaseClass1[String, DynamicValue.Error](
        TypeId.parse("zio.schema.DynamicValue.Error"),
        Schema.Field(
          "message",
          Schema.primitive[String],
          get = error => error.message,
          set = (error, message) => error.copy(message = message)
        ),
        message => DynamicValue.Error(message)
      ),
      _.asInstanceOf[DynamicValue.Error],
      _.asInstanceOf[DynamicValue],
      _.isInstanceOf[DynamicValue.Error]
    )

  private val noneValueCase: Schema.Case[DynamicValue, DynamicValue.NoneValue.type] =
    Schema.Case(
      "NoneValue",
      Schema.singleton(None).transform(_ => DynamicValue.NoneValue, _ => None),
      _.asInstanceOf[DynamicValue.NoneValue.type],
      _.asInstanceOf[DynamicValue],
      _.isInstanceOf[DynamicValue.NoneValue.type],
      Chunk("case")
    )

  private val rightValueCase: Schema.Case[DynamicValue, DynamicValue.RightValue] =
    Schema.Case(
      "RightValue",
      Schema.CaseClass1[DynamicValue, DynamicValue.RightValue](
        TypeId.parse("zio.schema.DynamicValue.RightValue"),
        Schema.Field(
          "value",
          Schema.defer(DynamicValueSchema()),
          get = rightValue => rightValue.value,
          set = (rightValue, value) => rightValue.copy(value = value)
        ),
        dynamicValue => DynamicValue.RightValue(dynamicValue)
      ),
      _.asInstanceOf[DynamicValue.RightValue],
      _.asInstanceOf[DynamicValue],
      _.isInstanceOf[DynamicValue.RightValue]
    )

  private val leftValueCase: Schema.Case[DynamicValue, DynamicValue.LeftValue] =
    Schema.Case(
      "LeftValue",
      Schema.CaseClass1[DynamicValue, DynamicValue.LeftValue](
        TypeId.parse("zio.schema.DynamicValue.LeftValue"),
        Schema.Field(
          "value",
          Schema.defer(DynamicValueSchema()),
          get = leftValue => leftValue.value,
          set = (leftValue, value) => leftValue.copy(value = value)
        ),
        dynamicValue => DynamicValue.LeftValue(dynamicValue)
      ),
      _.asInstanceOf[DynamicValue.LeftValue],
      _.asInstanceOf[DynamicValue],
      _.isInstanceOf[DynamicValue.LeftValue]
    )

  private val tupleCase: Schema.Case[DynamicValue, DynamicValue.Tuple] =
    Schema.Case(
      "Tuple",
      Schema.CaseClass2[DynamicValue, DynamicValue, DynamicValue.Tuple](
        TypeId.parse("zio.schema.DynamicValue.Tuple"),
        Schema.Field(
          "left",
          Schema.defer(DynamicValueSchema()),
          get = tuple => tuple.left,
          set = (tuple, left) => tuple.copy(left = left)
        ),
        Schema.Field(
          "right",
          Schema.defer(DynamicValueSchema()),
          get = tuple => tuple.right,
          set = (tuple, right) => tuple.copy(right = right)
        ),
        (left, right) => DynamicValue.Tuple(left, right)
      ),
      _.asInstanceOf[DynamicValue.Tuple],
      _.asInstanceOf[DynamicValue],
      _.isInstanceOf[DynamicValue.Tuple]
    )

  private val someValueCase: Schema.Case[DynamicValue, DynamicValue.SomeValue] =
    Schema.Case(
      "SomeValue",
      Schema.CaseClass1[DynamicValue, DynamicValue.SomeValue](
        TypeId.parse("zio.schema.DynamicValue.SomeValue"),
        Schema
          .Field(
            "value",
            Schema.defer(DynamicValueSchema()),
            get = someValue => someValue.value,
            set = (someValue, value) => someValue.copy(value = value)
          ),
        dv => DynamicValue.SomeValue(dv)
      ),
      _.asInstanceOf[DynamicValue.SomeValue],
      _.asInstanceOf[DynamicValue],
      _.isInstanceOf[DynamicValue.SomeValue]
    )

  private val dictionaryCase: Schema.Case[DynamicValue, DynamicValue.Dictionary] =
    Schema.Case(
      "Dictionary",
      Schema.CaseClass1[Chunk[(DynamicValue, DynamicValue)], DynamicValue.Dictionary](
        TypeId.parse("zio.schema.DynamicValue.Dictionary"),
        Schema.Field(
          "entries",
          Schema.defer(Schema.chunk(Schema.tuple2(DynamicValueSchema(), DynamicValueSchema()))),
          get = dictionary => dictionary.entries,
          set = (dictionary, entries) => dictionary.copy(entries = entries)
        ),
        chunk => DynamicValue.Dictionary(chunk)
      ),
      _.asInstanceOf[DynamicValue.Dictionary],
      _.asInstanceOf[DynamicValue],
      _.isInstanceOf[DynamicValue.Dictionary]
    )

  private val sequenceCase: Schema.Case[DynamicValue, DynamicValue.Sequence] =
    Schema.Case(
      "Sequence",
      Schema.CaseClass1[Chunk[DynamicValue], DynamicValue.Sequence](
        TypeId.parse("zio.schema.DynamicValue.Sequence"),
        Schema.Field(
          "values",
          Schema.defer(Schema.chunk(DynamicValueSchema())),
          get = seq => seq.values,
          set = (seq, values) => seq.copy(values = values)
        ),
        chunk => DynamicValue.Sequence(chunk)
      ),
      _.asInstanceOf[DynamicValue.Sequence],
      _.asInstanceOf[DynamicValue],
      _.isInstanceOf[DynamicValue.Sequence]
    )

  private val setCase: Schema.Case[DynamicValue, DynamicValue.SetValue] =
    Schema.Case(
      "SetValue",
      Schema.CaseClass1[Set[DynamicValue], DynamicValue.SetValue](
        TypeId.parse("zio.schema.DynamicValue.SetValue"),
        Schema.Field(
          "values",
          Schema.defer(Schema.set(DynamicValueSchema())),
          get = seq => seq.values,
          set = (seq, values) => seq.copy(values = values)
        ),
        set => DynamicValue.SetValue(set)
      ),
      _.asInstanceOf[DynamicValue.SetValue],
      _.asInstanceOf[DynamicValue],
      _.isInstanceOf[DynamicValue.SetValue]
    )

  private val enumerationCase: Schema.Case[DynamicValue, DynamicValue.Enumeration] =
    Schema.Case(
      "Enumeration",
      Schema.CaseClass2[TypeId, (String, DynamicValue), DynamicValue.Enumeration](
        TypeId.parse("zio.schema.DynamicValue.Enumeration"),
        Schema.Field(
          "id",
          Schema[TypeId],
          get = enumeration => enumeration.id,
          set = (enumeration, id) => enumeration.copy(id = id)
        ),
        Schema.Field(
          "value",
          Schema.defer(Schema.tuple2(Schema.primitive[String], DynamicValueSchema())),
          get = enumeration => enumeration.value,
          set = (enumeration, value) => enumeration.copy(value = value)
        ),
        (id, value) => DynamicValue.Enumeration(id, value)
      ),
      _.asInstanceOf[DynamicValue.Enumeration],
      _.asInstanceOf[DynamicValue],
      _.isInstanceOf[DynamicValue.Enumeration]
    )

  private val recordCase: Schema.Case[DynamicValue, DynamicValue.Record] =
    Schema.Case(
      "Record",
      Schema.CaseClass2[TypeId, Chunk[(String, DynamicValue)], DynamicValue.Record](
        TypeId.parse("zio.schema.DynamicValue.Record"),
        Schema.Field("id", Schema[TypeId], get = record => record.id, set = (record, id) => record.copy(id = id)),
        Schema
          .Field(
            "values",
            Schema.defer(Schema.chunk(Schema.tuple2(Schema.primitive[String], DynamicValueSchema()))),
            get = record => Chunk.fromIterable(record.values),
            set = (record, values) => record.copy(values = ListMap.from(values.toMap))
          ),
        (id, chunk) => DynamicValue.Record(id, ListMap(chunk.toSeq: _*))
      ),
      _.asInstanceOf[DynamicValue.Record],
      _.asInstanceOf[DynamicValue],
      _.isInstanceOf[DynamicValue.Record]
    )

  private val dynamicAstCase: Schema.Case[DynamicValue, DynamicValue.DynamicAst] =
    Schema.Case(
      "DynamicAst",
      Schema.CaseClass1[MetaSchema, DynamicValue.DynamicAst](
        TypeId.parse("zio.schema.DynamicValue.DynamicAst"),
        Schema.Field(
          "ast",
          MetaSchema.schema,
          get = dynamicAst => dynamicAst.ast,
          set = (dynamicAst, ast) => dynamicAst.copy(ast = ast)
        ),
        schemaAst => DynamicValue.DynamicAst(schemaAst)
      ),
      _.asInstanceOf[DynamicValue.DynamicAst],
      _.asInstanceOf[DynamicValue],
      _.isInstanceOf[DynamicValue.DynamicAst]
    )

  private val singletonCase: Schema.Case[DynamicValue, DynamicValue.Singleton[Any]] =
    Schema.Case(
      "Singleton",
      Schema[Unit].transform(_ => DynamicValue.Singleton(()), _ => ()),
      _.asInstanceOf[DynamicValue.Singleton[Any]],
      _.asInstanceOf[DynamicValue],
      (d: DynamicValue) => d.isInstanceOf[DynamicValue.Singleton[_]]
    )

  private val primitiveUnitCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Unit]] =
    Schema.Case(
      "Unit",
      Schema.primitive[Unit].transform(unit => DynamicValue.Primitive(unit, StandardType[Unit]), _.value), {
        case dv @ DynamicValue.Primitive((), _) => dv.asInstanceOf[DynamicValue.Primitive[Unit]]
        case _                                  => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive((), _) => dv.asInstanceOf[DynamicValue]
        case _                                  => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive((), _) => true
        case _                             => false
      }
    )

  private val primitiveStringCase: Schema.Case[DynamicValue, DynamicValue.Primitive[String]] =
    Schema.Case(
      "String",
      Schema.primitive[String].transform(s => DynamicValue.Primitive(s, StandardType[String]), _.value), {
        case dv @ DynamicValue.Primitive(_: String, _) => dv.asInstanceOf[DynamicValue.Primitive[String]]
        case _                                         => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: String, _) => dv.asInstanceOf[DynamicValue]
        case _                                         => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: String, _) => true
        case _                                    => false
      }
    )

  private val primitiveBooleanCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Boolean]] =
    Schema.Case(
      "Boolean",
      Schema.primitive[Boolean].transform(b => DynamicValue.Primitive(b, StandardType[Boolean]), _.value), {
        case dv @ DynamicValue.Primitive(_: Boolean, _) => dv.asInstanceOf[DynamicValue.Primitive[Boolean]]
        case _                                          => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Boolean, _) => dv.asInstanceOf[DynamicValue]
        case _                                          => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Boolean, _) => true
        case _                                     => false
      }
    )

  private val primitiveShortCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Short]] =
    Schema.Case(
      "Short",
      Schema.primitive[Short].transform(sh => DynamicValue.Primitive(sh, StandardType[Short]), _.value), {
        case dv @ DynamicValue.Primitive(_: Short, _) => dv.asInstanceOf[DynamicValue.Primitive[Short]]
        case _                                        => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Short, _) => dv.asInstanceOf[DynamicValue]
        case _                                        => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Short, _) => true
        case _                                   => false
      }
    )

  private val primitiveIntCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Int]] =
    Schema.Case(
      "Int",
      Schema.primitive[Int].transform(i => DynamicValue.Primitive(i, StandardType[Int]), _.value), {
        case dv @ DynamicValue.Primitive(_: Int, _) => dv.asInstanceOf[DynamicValue.Primitive[Int]]
        case _                                      => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Int, _) => dv.asInstanceOf[DynamicValue]
        case _                                      => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Int, _) => true
        case _                                 => false
      }
    )

  private val primitiveLongCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Long]] =
    Schema.Case(
      "Long",
      Schema.primitive[Long].transform(l => DynamicValue.Primitive(l, StandardType[Long]), _.value), {
        case dv @ DynamicValue.Primitive(_: Long, _) => dv.asInstanceOf[DynamicValue.Primitive[Long]]
        case _                                       => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Long, _) => dv.asInstanceOf[DynamicValue]
        case _                                       => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Long, _) => true
        case _                                  => false
      }
    )

  private val primitiveFloatCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Float]] =
    Schema.Case(
      "Float",
      Schema.primitive[Float].transform(f => DynamicValue.Primitive(f, StandardType[Float]), _.value), {
        case dv @ DynamicValue.Primitive(_: Float, _) => dv.asInstanceOf[DynamicValue.Primitive[Float]]
        case _                                        => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Float, _) => dv.asInstanceOf[DynamicValue]
        case _                                        => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Float, _) => true
        case _                                   => false
      }
    )

  private val primitiveDoubleCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Double]] =
    Schema.Case(
      "Double",
      Schema.primitive[Double].transform(d => DynamicValue.Primitive(d, StandardType[Double]), _.value), {
        case dv @ DynamicValue.Primitive(_: Double, _) => dv.asInstanceOf[DynamicValue.Primitive[Double]]
        case _                                         => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Double, _) => dv.asInstanceOf[DynamicValue]
        case _                                         => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Double, _) => true
        case _                                    => false
      }
    )

  private val primitiveBinaryCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Chunk[Byte]]] =
    Schema.Case(
      "Binary",
      Schema.primitive[Chunk[Byte]].transform(ch => DynamicValue.Primitive(ch, StandardType[Chunk[Byte]]), _.value), {
        case dv @ DynamicValue.Primitive(_: Chunk[_], _) => dv.asInstanceOf[DynamicValue.Primitive[Chunk[Byte]]]
        case _                                           => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Chunk[_], _) => dv.asInstanceOf[DynamicValue]
        case _                                           => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Chunk[_], _) => true
        case _                                      => false
      }
    )

  private val primitiveCharCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Char]] =
    Schema.Case(
      "Char",
      Schema.primitive[Char].transform(ch => DynamicValue.Primitive(ch, StandardType[Char]), _.value), {
        case dv @ DynamicValue.Primitive(_: Char, _) => dv.asInstanceOf[DynamicValue.Primitive[Char]]
        case _                                       => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Char, _) => dv.asInstanceOf[DynamicValue]
        case _                                       => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Char, _) => true
        case _                                  => false
      }
    )

  private val primitiveBigDecimalCase: Schema.Case[DynamicValue, DynamicValue.Primitive[BigDecimal]] =
    Schema.Case(
      "BigDecimal",
      Schema.primitive[BigDecimal].transform(bd => DynamicValue.Primitive(bd, StandardType[BigDecimal]), _.value), {
        case dv @ DynamicValue.Primitive(_: BigDecimal, _) => dv.asInstanceOf[DynamicValue.Primitive[BigDecimal]]
        case _                                             => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: BigDecimal, _) => dv.asInstanceOf[DynamicValue]
        case _                                             => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: BigDecimal, _) => true
        case _                                        => false
      }
    )

  private val primitiveBigIntegerCase: Schema.Case[DynamicValue, DynamicValue.Primitive[BigInteger]] =
    Schema.Case(
      "BigInteger",
      Schema.primitive[BigInteger].transform(bi => DynamicValue.Primitive(bi, StandardType[BigInteger]), _.value), {
        case dv @ DynamicValue.Primitive(_: BigInteger, _) => dv.asInstanceOf[DynamicValue.Primitive[BigInteger]]
        case _                                             => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: BigInteger, _) => dv.asInstanceOf[DynamicValue]
        case _                                             => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: BigInteger, _) => true
        case _                                        => false
      }
    )

  private val primitiveDayOfWeekCase: Schema.Case[DynamicValue, DynamicValue.Primitive[DayOfWeek]] =
    Schema.Case(
      "DayOfWeek",
      Schema.primitive[DayOfWeek].transform(dw => DynamicValue.Primitive(dw, StandardType[DayOfWeek]), _.value), {
        case dv @ DynamicValue.Primitive(_: DayOfWeek, _) => dv.asInstanceOf[DynamicValue.Primitive[DayOfWeek]]
        case _                                            => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: DayOfWeek, _) => dv.asInstanceOf[DynamicValue]
        case _                                            => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: DayOfWeek, _) => true
        case _                                       => false
      }
    )

  private val primitiveMonthCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Month]] =
    Schema.Case(
      "Month",
      Schema.primitive[Month].transform(m => DynamicValue.Primitive(m, StandardType[Month]), _.value), {
        case dv @ DynamicValue.Primitive(_: Month, _) => dv.asInstanceOf[DynamicValue.Primitive[Month]]
        case _                                        => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Month, _) => dv.asInstanceOf[DynamicValue]
        case _                                        => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Month, _) => true
        case _                                   => false
      }
    )

  private val primitiveMonthDayCase: Schema.Case[DynamicValue, DynamicValue.Primitive[MonthDay]] =
    Schema.Case(
      "MonthDay",
      Schema.primitive[MonthDay].transform(md => DynamicValue.Primitive(md, StandardType[MonthDay]), _.value), {
        case dv @ DynamicValue.Primitive(_: MonthDay, _) => dv.asInstanceOf[DynamicValue.Primitive[MonthDay]]
        case _                                           => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: MonthDay, _) => dv.asInstanceOf[DynamicValue]
        case _                                           => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: MonthDay, _) => true
        case _                                      => false
      }
    )

  private val primitivePeriodCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Period]] =
    Schema.Case(
      "Period",
      Schema.primitive[Period].transform(p => DynamicValue.Primitive(p, StandardType[Period]), _.value), {
        case dv @ DynamicValue.Primitive(_: Period, _) => dv.asInstanceOf[DynamicValue.Primitive[Period]]
        case _                                         => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Period, _) => dv.asInstanceOf[DynamicValue]
        case _                                         => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Period, _) => true
        case _                                    => false
      }
    )

  private val primitiveYearCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Year]] =
    Schema.Case(
      "Year",
      Schema.primitive[Year].transform(y => DynamicValue.Primitive(y, StandardType[Year]), _.value), {
        case dv @ DynamicValue.Primitive(_: Year, _) => dv.asInstanceOf[DynamicValue.Primitive[Year]]
        case _                                       => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Year, _) => dv.asInstanceOf[DynamicValue]
        case _                                       => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Year, _) => true
        case _                                  => false
      }
    )

  private val primitiveYearMonthCase: Schema.Case[DynamicValue, DynamicValue.Primitive[YearMonth]] =
    Schema.Case(
      "YearMonth",
      Schema.primitive[YearMonth].transform(ym => DynamicValue.Primitive(ym, StandardType[YearMonth]), _.value), {
        case dv @ DynamicValue.Primitive(_: YearMonth, _) => dv.asInstanceOf[DynamicValue.Primitive[YearMonth]]
        case _                                            => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: YearMonth, _) => dv.asInstanceOf[DynamicValue]
        case _                                            => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: YearMonth, _) => true
        case _                                       => false
      }
    )

  private val primitiveZoneIdCase: Schema.Case[DynamicValue, DynamicValue.Primitive[ZoneId]] =
    Schema.Case(
      "ZoneId",
      Schema.primitive[ZoneId].transform(zid => DynamicValue.Primitive(zid, StandardType[ZoneId]), _.value), {
        case dv @ DynamicValue.Primitive(_: ZoneId, _) => dv.asInstanceOf[DynamicValue.Primitive[ZoneId]]
        case _                                         => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: ZoneId, _) => dv.asInstanceOf[DynamicValue]
        case _                                         => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: ZoneId, _) => true
        case _                                    => false
      }
    )

  private val primitiveZoneOffsetCase: Schema.Case[DynamicValue, DynamicValue.Primitive[ZoneOffset]] =
    Schema.Case(
      "ZoneOffset",
      Schema.primitive[ZoneOffset].transform(zo => DynamicValue.Primitive(zo, StandardType[ZoneOffset]), _.value), {
        case dv @ DynamicValue.Primitive(_: ZoneOffset, _) => dv.asInstanceOf[DynamicValue.Primitive[ZoneOffset]]
        case _                                             => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: ZoneOffset, _) => dv.asInstanceOf[DynamicValue]
        case _                                             => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: ZoneOffset, _) => true
        case _                                        => false
      }
    )

  private val primitiveInstantCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Instant]] =
    Schema.Case(
      "Instant",
      Schema.primitive[Instant].transform(i => DynamicValue.Primitive(i, StandardType[Instant]), _.value), {
        case dv @ DynamicValue.Primitive(_: Instant, _) => dv.asInstanceOf[DynamicValue.Primitive[Instant]]
        case _                                          => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Instant, _) => dv.asInstanceOf[DynamicValue]
        case _                                          => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Instant, _) => true
        case _                                     => false
      }
    )

  private val primitiveDurationCase: Schema.Case[DynamicValue, DynamicValue.Primitive[Duration]] =
    Schema.Case(
      "Duration",
      Schema.primitive[Duration].transform(i => DynamicValue.Primitive(i, StandardType[Duration]), _.value), {
        case dv @ DynamicValue.Primitive(_: Duration, _) => dv.asInstanceOf[DynamicValue.Primitive[Duration]]
        case _                                           => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: Duration, _) => dv.asInstanceOf[DynamicValue]
        case _                                           => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: Duration, _) => true
        case _                                      => false
      }
    )

  private val primitiveLocalDateCase: Schema.Case[DynamicValue, DynamicValue.Primitive[LocalDate]] =
    Schema.Case(
      "LocalDate",
      Schema.primitive[LocalDate].transform(ld => DynamicValue.Primitive(ld, StandardType[LocalDate]), _.value), {
        case dv @ DynamicValue.Primitive(_: LocalDate, _) => dv.asInstanceOf[DynamicValue.Primitive[LocalDate]]
        case _                                            => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: LocalDate, _) => dv.asInstanceOf[DynamicValue]
        case _                                            => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: LocalDate, _) => true
        case _                                       => false
      }
    )

  private val primitiveLocalTimeCase: Schema.Case[DynamicValue, DynamicValue.Primitive[LocalTime]] =
    Schema.Case(
      "LocalTime",
      Schema.primitive[LocalTime].transform(lt => DynamicValue.Primitive(lt, StandardType[LocalTime]), _.value), {
        case dv @ DynamicValue.Primitive(_: LocalTime, _) => dv.asInstanceOf[DynamicValue.Primitive[LocalTime]]
        case _                                            => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: LocalTime, _) => dv.asInstanceOf[DynamicValue]
        case _                                            => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: LocalTime, _) => true
        case _                                       => false
      }
    )

  private val primitiveLocalDateTimeCase: Schema.Case[DynamicValue, DynamicValue.Primitive[LocalDateTime]] =
    Schema.Case(
      "LocalDateTime",
      Schema
        .primitive[LocalDateTime]
        .transform(ldt => DynamicValue.Primitive(ldt, StandardType[LocalDateTime]), _.value), {
        case dv @ DynamicValue.Primitive(_: LocalDateTime, _) => dv.asInstanceOf[DynamicValue.Primitive[LocalDateTime]]
        case _                                                => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: LocalDateTime, _) => dv.asInstanceOf[DynamicValue]
        case _                                                => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: LocalDateTime, _) => true
        case _                                           => false
      }
    )

  private val primitiveOffsetTimeCase: Schema.Case[DynamicValue, DynamicValue.Primitive[OffsetTime]] =
    Schema.Case(
      "OffsetTime",
      Schema.primitive[OffsetTime].transform(ot => DynamicValue.Primitive(ot, StandardType[OffsetTime]), _.value), {
        case dv @ DynamicValue.Primitive(_: OffsetTime, _) => dv.asInstanceOf[DynamicValue.Primitive[OffsetTime]]
        case _                                             => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: OffsetTime, _) => dv.asInstanceOf[DynamicValue]
        case _                                             => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: OffsetTime, _) => true
        case _                                        => false
      }
    )

  private val primitiveOffsetDateTimeCase: Schema.Case[DynamicValue, DynamicValue.Primitive[OffsetDateTime]] =
    Schema.Case(
      "OffsetDateTime",
      Schema
        .primitive[OffsetDateTime]
        .transform(odt => DynamicValue.Primitive(odt, StandardType[OffsetDateTime]), _.value), {
        case dv @ DynamicValue.Primitive(_: OffsetDateTime, _) =>
          dv.asInstanceOf[DynamicValue.Primitive[OffsetDateTime]]
        case _ => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: OffsetDateTime, _) =>
          dv.asInstanceOf[DynamicValue]
        case _ => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: OffsetDateTime, _) => true
        case _                                            => false
      }
    )

  private val primitiveZonedDateTimeCase: Schema.Case[DynamicValue, DynamicValue.Primitive[ZonedDateTime]] =
    Schema.Case(
      "ZonedDateTime",
      Schema
        .primitive[ZonedDateTime]
        .transform(zdt => DynamicValue.Primitive(zdt, StandardType[ZonedDateTime]), _.value), {
        case dv @ DynamicValue.Primitive(_: ZonedDateTime, _) => dv.asInstanceOf[DynamicValue.Primitive[ZonedDateTime]]
        case _                                                => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: ZonedDateTime, _) => dv.asInstanceOf[DynamicValue]
        case _                                                => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: ZonedDateTime, _) => true
        case _                                           => false
      }
    )

  private val primitiveUUIDCase: Schema.Case[DynamicValue, DynamicValue.Primitive[UUID]] =
    Schema.Case(
      "UUID",
      Schema.primitive[UUID].transform(uuid => DynamicValue.Primitive(uuid, StandardType[UUID]), _.value), {
        case dv @ DynamicValue.Primitive(_: UUID, _) => dv.asInstanceOf[DynamicValue.Primitive[UUID]]
        case _                                       => throw new IllegalArgumentException
      }, {
        case dv @ DynamicValue.Primitive(_: UUID, _) => dv.asInstanceOf[DynamicValue]
        case _                                       => throw new IllegalArgumentException
      }, {
        case DynamicValue.Primitive(_: UUID, _) => true
        case _                                  => false
      }
    )

}
