package zio.schema

import zio.schema.codec.DecodeError

import java.math.{ BigDecimal, BigInteger }
import java.time._
import java.util.UUID
import scala.collection.immutable.ListMap
import zio.schema.meta.{ MetaSchema, Migration }
import zio.{ Cause, Chunk, Unsafe }

sealed trait DynamicValue {
  self =>

  def transform(transforms: Chunk[Migration]): Either[String, DynamicValue] =
    transforms.foldRight[Either[String, DynamicValue]](Right(self)) {
      case (transform, Right(value)) => transform.migrate(value)
      case (_, error @ Left(_))      => error
    }

  def toTypedValue[A](implicit schema: Schema[A]): Either[String, A] =
    toTypedValueLazyError.left.map(_.message)

  def toValue[A](implicit schema: Schema[A]): Either[DecodeError, A] = toTypedValueLazyError

  def toTypedValueOption[A](implicit schema: Schema[A]): Option[A] =
    toTypedValueLazyError.toOption

  private def toTypedValueLazyError[A](implicit schema: Schema[A]): Either[DecodeError, A] =
    (self, schema) match {
      case (DynamicValue.Primitive(value, p), Schema.Primitive(p2, _)) if p == p2 =>
        Right(value.asInstanceOf[A])

      case (DynamicValue.Record(_, values), Schema.GenericRecord(_, structure, _)) =>
        DynamicValue.decodeStructure(values, structure.toChunk).asInstanceOf[Either[DecodeError, A]]

      case (DynamicValue.Record(_, values), s: Schema.Record[A]) =>
        DynamicValue
          .decodeStructure(values, s.fields)
          .map(m => Chunk.fromIterable(m.values))
          .flatMap(values => s.construct(values)(Unsafe.unsafe).left.map(err => DecodeError.MalformedField(s, err)))

      case (DynamicValue.Enumeration(_, (key, value)), s: Schema.Enum[A]) =>
        s.caseOf(key) match {
          case Some(caseValue) =>
            value.toTypedValueLazyError(caseValue.schema).asInstanceOf[Either[DecodeError, A]]
          case None => Left(DecodeError.MissingCase(key, s))
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
            Left(DecodeError.And(e1, e2))
          case (_, Left(e))         => Left(e)
          case (Left(e), _)         => Left(e)
          case (Right(a), Right(b)) => Right(a -> b)
        }

      case (DynamicValue.Sequence(values), schema: Schema.Sequence[col, t, _]) =>
        values
          .foldLeft[Either[DecodeError, Chunk[t]]](Right[DecodeError, Chunk[t]](Chunk.empty)) {
            case (err @ Left(_), _) => err
            case (Right(values), value) =>
              value.toTypedValueLazyError(schema.elementSchema).map(values :+ _)
          }
          .map(schema.fromChunk)

      case (DynamicValue.SetValue(values), schema: Schema.Set[t]) =>
        values.foldLeft[Either[DecodeError, Set[t]]](Right[DecodeError, Set[t]](Set.empty)) {
          case (err @ Left(_), _) => err
          case (Right(values), value) =>
            value.toTypedValueLazyError(schema.elementSchema).map(values + _)
        }

      case (DynamicValue.SomeValue(value), Schema.Optional(schema: Schema[_], _)) =>
        value.toTypedValueLazyError(schema).map(Some(_))

      case (DynamicValue.NoneValue, Schema.Optional(_, _)) =>
        Right(None)

      case (value, Schema.Transform(schema, f, _, _, _)) =>
        value
          .toTypedValueLazyError(schema)
          .flatMap(value => f(value).left.map(err => DecodeError.MalformedField(schema, err)))

      case (DynamicValue.Dictionary(entries), schema: Schema.Map[k, v]) =>
        entries.foldLeft[Either[DecodeError, Map[k, v]]](Right[DecodeError, Map[k, v]](Map.empty)) {
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
        Left(DecodeError.ReadError(Cause.empty, message))

      case (DynamicValue.Tuple(dyn, DynamicValue.DynamicAst(ast)), _) =>
        val valueSchema = ast.toSchema.asInstanceOf[Schema[Any]]
        dyn.toTypedValueLazyError(valueSchema).map(a => (a -> valueSchema).asInstanceOf[A])

      case (dyn, Schema.Dynamic(_)) => Right(dyn)

      case _ =>
        Left(DecodeError.CastError(self, schema))
    }

}

object DynamicValue {

  private object FromSchemaAndValue extends SimpleMutableSchemaBasedValueProcessor[DynamicValue] {
    override protected def processPrimitive(value: Any, typ: StandardType[Any]): DynamicValue =
      DynamicValue.Primitive(value, typ)

    override protected def processRecord(schema: Schema.Record[_], value: ListMap[String, DynamicValue]): DynamicValue =
      DynamicValue.Record(schema.id, value)

    override protected def processEnum(schema: Schema.Enum[_], tuple: (String, DynamicValue)): DynamicValue =
      DynamicValue.Enumeration(schema.id, tuple)

    override protected def processSequence(schema: Schema.Sequence[_, _, _], value: Chunk[DynamicValue]): DynamicValue =
      DynamicValue.Sequence(value)

    override protected def processDictionary(
      schema: Schema.Map[_, _],
      value: Chunk[(DynamicValue, DynamicValue)]
    ): DynamicValue =
      DynamicValue.Dictionary(value)

    override protected def processSet(schema: Schema.Set[_], value: Set[DynamicValue]): DynamicValue =
      DynamicValue.SetValue(value)

    override protected def processEither(
      schema: Schema.Either[_, _],
      value: Either[DynamicValue, DynamicValue]
    ): DynamicValue =
      value match {
        case Left(value)  => DynamicValue.LeftValue(value)
        case Right(value) => DynamicValue.RightValue(value)
      }

    override protected def processOption(schema: Schema.Optional[_], value: Option[DynamicValue]): DynamicValue =
      value match {
        case Some(value) => DynamicValue.SomeValue(value)
        case None        => DynamicValue.NoneValue
      }

    override protected def processTuple(
      schema: Schema.Tuple2[_, _],
      left: DynamicValue,
      right: DynamicValue
    ): DynamicValue =
      DynamicValue.Tuple(left, right)

    override protected def processDynamic(value: DynamicValue): Option[DynamicValue] =
      Some(value)

    override protected def fail(message: String): DynamicValue =
      DynamicValue.Error(message)
  }

  def apply[A](a: A)(implicit ev: Schema[A]): DynamicValue = ev.toDynamic(a)

  //scalafmt: { maxColumn = 400 }
  def fromSchemaAndValue[A](schema: Schema[A], value: A): DynamicValue =
    FromSchemaAndValue.process(schema, value)

  def decodeStructure(
    values: ListMap[String, DynamicValue],
    structure: Chunk[Schema.Field[_, _]]
  ): Either[DecodeError, ListMap[String, _]] = {
    val keys = values.keySet
    keys.foldLeft[Either[DecodeError, ListMap[String, Any]]](Right(ListMap.empty)) {
      case (Right(record), key) =>
        (structure.find(_.name == key), values.get(key)) match {
          case (Some(field), Some(value)) =>
            value.toTypedValueLazyError(field.schema).map(value => (record + (key -> value)))
          case _ =>
            Left(DecodeError.IncompatibleShape(values, structure))
        }
      case (Left(err), _) => Left(err)
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

  lazy val typeId: TypeId = TypeId.parse("zio.schema.DynamicValue")

  lazy val schema: Schema[DynamicValue] =
    Schema.EnumN(
      typeId,
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
    StandardType.InstantType

  implicit val localDateStandardType: StandardType[LocalDate] =
    StandardType.LocalDateType

  implicit val localTimeStandardType: StandardType[LocalTime] =
    StandardType.LocalTimeType

  implicit val localDateTimeStandardType: StandardType[LocalDateTime] =
    StandardType.LocalDateTimeType

  implicit val offsetTimeStandardType: StandardType[OffsetTime] =
    StandardType.OffsetTimeType

  implicit val offsetDateTimeStandardType: StandardType[OffsetDateTime] =
    StandardType.OffsetDateTimeType

  implicit val zonedDateTimeStandardType: StandardType[ZonedDateTime] =
    StandardType.ZonedDateTimeType

  private val errorCase: Schema.Case[DynamicValue, DynamicValue.Error] =
    Schema.Case(
      "Error",
      Schema.CaseClass1[String, DynamicValue.Error](
        TypeId.parse("zio.schema.DynamicValue.Error"),
        Schema.Field(
          "message",
          Schema.primitive[String],
          get0 = error => error.message,
          set0 = (error, message) => error.copy(message = message)
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
          Schema.defer(DynamicValue.schema),
          get0 = rightValue => rightValue.value,
          set0 = (rightValue, value) => rightValue.copy(value = value)
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
          Schema.defer(DynamicValue.schema),
          get0 = leftValue => leftValue.value,
          set0 = (leftValue, value) => leftValue.copy(value = value)
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
          Schema.defer(DynamicValue.schema),
          get0 = tuple => tuple.left,
          set0 = (tuple, left) => tuple.copy(left = left)
        ),
        Schema.Field(
          "right",
          Schema.defer(DynamicValue.schema),
          get0 = tuple => tuple.right,
          set0 = (tuple, right) => tuple.copy(right = right)
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
            Schema.defer(DynamicValue.schema),
            get0 = someValue => someValue.value,
            set0 = (someValue, value) => someValue.copy(value = value)
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
          Schema.defer(Schema.chunk(Schema.tuple2(DynamicValue.schema, DynamicValue.schema))),
          get0 = dictionary => dictionary.entries,
          set0 = (dictionary, entries) => dictionary.copy(entries = entries)
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
          Schema.defer(Schema.chunk(DynamicValue.schema)),
          get0 = seq => seq.values,
          set0 = (seq, values) => seq.copy(values = values)
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
          Schema.defer(Schema.set(DynamicValue.schema)),
          get0 = seq => seq.values,
          set0 = (seq, values) => seq.copy(values = values)
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
          get0 = enumeration => enumeration.id,
          set0 = (enumeration, id) => enumeration.copy(id = id)
        ),
        Schema.Field(
          "value",
          Schema.defer(Schema.tuple2(Schema.primitive[String], DynamicValue.schema)),
          get0 = enumeration => enumeration.value,
          set0 = (enumeration, value) => enumeration.copy(value = value)
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
        Schema.Field("id", Schema[TypeId], get0 = record => record.id, set0 = (record, id) => record.copy(id = id)),
        Schema
          .Field(
            "values",
            Schema.defer(Schema.chunk(Schema.tuple2(Schema.primitive[String], DynamicValue.schema))),
            get0 = record => Chunk.fromIterable(record.values),
            set0 = (record, values) => record.copy(values = values.foldRight(ListMap.empty[String, DynamicValue])((a, b) => b + a))
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
          get0 = dynamicAst => dynamicAst.ast,
          set0 = (dynamicAst, ast) => dynamicAst.copy(ast = ast)
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
      },
      (dv: DynamicValue.Primitive[Unit]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[String]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Boolean]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Short]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Int]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Long]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Float]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Double]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Chunk[Byte]]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Char]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[BigDecimal]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[BigInteger]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[DayOfWeek]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Month]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[MonthDay]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Period]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Year]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[YearMonth]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[ZoneId]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[ZoneOffset]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Instant]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[Duration]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[LocalDate]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[LocalTime]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[LocalDateTime]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[OffsetTime]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[OffsetDateTime]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[ZonedDateTime]) => dv.asInstanceOf[DynamicValue], {
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
      },
      (dv: DynamicValue.Primitive[UUID]) => dv.asInstanceOf[DynamicValue], {
        case DynamicValue.Primitive(_: UUID, _) => true
        case _                                  => false
      }
    )

}
