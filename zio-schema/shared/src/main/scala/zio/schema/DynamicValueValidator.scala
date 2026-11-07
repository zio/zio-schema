package zio.schema

import scala.collection.immutable.ListMap

import zio.prelude.Validation
import zio.schema.codec.DecodeError
import zio.{Cause, Chunk, Unsafe}

/**
 * Accumulates all structural and field-level `DecodeError` instances encountered while converting a `DynamicValue`
 * into a typed domain value using `zio.prelude.Validation`.
 */
private[schema] object DynamicValueValidator {

  def validate[A](dynamic: DynamicValue, schema: Schema[A]): Validation[DecodeError, A] =
    (dynamic, schema) match {
      case (DynamicValue.Primitive(value, actualType), Schema.Primitive(expectedType, _))
          if actualType == expectedType =>
        Validation.succeed(value.asInstanceOf[A])
      case (DynamicValue.Record(_, values), Schema.GenericRecord(_, structure, _)) =>
        decodeStructure(values, structure.toChunk).asInstanceOf[Validation[DecodeError, A]]
      case (DynamicValue.Record(_, values), recordSchema: Schema.Record[A]) =>
        decodeRecord(values, recordSchema)
      case (DynamicValue.Enumeration(_, entry), enumSchema: Schema.Enum[A]) =>
        decodeEnum(entry, enumSchema)
      case (DynamicValue.LeftValue(value), Schema.Either(leftSchema, _, _)) =>
        validate(value, leftSchema).map(Left(_))
      case (DynamicValue.RightValue(value), Schema.Either(_, rightSchema, _)) =>
        validate(value, rightSchema).map(Right(_))
      case (tuple: DynamicValue.Tuple, schemaTuple: Schema.Tuple2[_, _]) =>
        decodeTuple(tuple, schemaTuple).asInstanceOf[Validation[DecodeError, A]]
      case (DynamicValue.Sequence(values), seqSchema: Schema.Sequence[_, _, _]) =>
        decodeSequence(values, seqSchema).asInstanceOf[Validation[DecodeError, A]]
      case (DynamicValue.SetValue(values), setSchema: Schema.Set[_]) =>
        decodeSet(values, setSchema).asInstanceOf[Validation[DecodeError, A]]
      case _ =>
        validateRemaining(dynamic, schema)
    }

  private def validateRemaining[A](dynamic: DynamicValue, schema: Schema[A]): Validation[DecodeError, A] =
    (dynamic, schema) match {
      case (DynamicValue.SomeValue(value), Schema.Optional(innerSchema: Schema[_], _)) =>
        validate(value, innerSchema).map(Some(_))
      case (DynamicValue.NoneValue, Schema.Optional(_, _)) =>
        Validation.succeed(None)
      case (value, transformSchema: Schema.Transform[_, A, _]) =>
        decodeTransform(value, transformSchema)
      case (DynamicValue.Dictionary(entries), mapSchema: Schema.Map[_, _]) =>
        decodeDictionary(entries, mapSchema).asInstanceOf[Validation[DecodeError, A]]
      case (_, lazySchema @ Schema.Lazy(_)) =>
        validate(dynamic, lazySchema.schema)
      case (DynamicValue.Error(message), _) =>
        Validation.fail(DecodeError.ReadError(Cause.empty, message))
      case (DynamicValue.Tuple(dyn, DynamicValue.DynamicAst(ast)), _) =>
        val valueSchema = ast.toSchema.asInstanceOf[Schema[Any]]
        validate(dyn, valueSchema).map(v => (v, valueSchema).asInstanceOf[A])
      case (dyn, Schema.Dynamic(_)) =>
        Validation.succeed(dyn)
      case _ =>
        Validation.fail(DecodeError.CastError(dynamic, schema))
    }

  def decodeStructure(
    values: ListMap[String, DynamicValue],
    structure: Chunk[Schema.Field[_, _]]
  ): Validation[DecodeError, ListMap[String, _]] = {
    val fieldValidations = values.keySet.toList.map { key =>
      (structure.find(_.name == key), values.get(key)) match {
        case (Some(field), Some(value)) =>
          validate(value, field.schema).map(typed => (key, typed))
        case _ =>
          Validation.fail(DecodeError.IncompatibleShape(values, structure))
      }
    }
    Validation.validateAll(fieldValidations).map(entries => ListMap(entries: _*))
  }

  private def decodeRecord[A](
    values: ListMap[String, DynamicValue],
    schema: Schema.Record[A]
  ): Validation[DecodeError, A] =
    decodeStructure(values, schema.fields).flatMap { mapped =>
      val constructed = schema.construct(Chunk.fromIterable(mapped.values))(Unsafe.unsafe)
      Validation.fromEither(constructed.left.map(err => DecodeError.MalformedField(schema, err)))
    }

  private def decodeEnum[A](
    entry: (String, DynamicValue),
    schema: Schema.Enum[A]
  ): Validation[DecodeError, A] =
    schema.caseOf(entry._1) match {
      case Some(caseValue) =>
        validate(entry._2, caseValue.schema).asInstanceOf[Validation[DecodeError, A]]
      case None =>
        Validation.fail(DecodeError.MissingCase(entry._1, schema))
    }

  private def decodeTuple[A, B](
    tuple: DynamicValue.Tuple,
    schema: Schema.Tuple2[A, B]
  ): Validation[DecodeError, (A, B)] =
    Validation.validateWith(validate(tuple.left, schema.left), validate(tuple.right, schema.right))((a, b) => (a, b))

  private def decodeSequence[Col, Elem](
    values: Chunk[DynamicValue],
    schema: Schema.Sequence[Col, Elem, _]
  ): Validation[DecodeError, Col] =
    Validation.validateAll(values.map(validate(_, schema.elementSchema))).map(schema.fromChunk)

  private def decodeSet[Elem](
    values: Set[DynamicValue],
    schema: Schema.Set[Elem]
  ): Validation[DecodeError, Set[Elem]] =
    Validation.validateAll(Chunk.fromIterable(values).map(validate(_, schema.elementSchema))).map(_.toSet)

  private def decodeTransform[In, Out](
    value: DynamicValue,
    schema: Schema.Transform[In, Out, _]
  ): Validation[DecodeError, Out] =
    validate(value, schema.schema).flatMap { decoded =>
      Validation.fromEither(schema.f(decoded).left.map(err => DecodeError.MalformedField(schema.schema, err)))
    }

  private def decodeDictionary[K, V](
    entries: Chunk[(DynamicValue, DynamicValue)],
    schema: Schema.Map[K, V]
  ): Validation[DecodeError, Map[K, V]] = {
    val validatedEntries = entries.map { entry =>
      Validation.validateWith(validate(entry._1, schema.keySchema), validate(entry._2, schema.valueSchema)) {
        (k, v) => (k, v)
      }
    }
    Validation.validateAll(validatedEntries).map(_.toMap)
  }
}
