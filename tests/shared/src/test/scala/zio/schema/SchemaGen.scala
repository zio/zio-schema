package zio.schema

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.random.Random
import zio.test.{ Gen, Sized }

object SchemaGen {

  val anyLabel: Gen[Random with Sized, String] = Gen.alphaNumericStringBounded(1, 3)

  def anyStructure(schemaGen: Gen[Random with Sized, Schema[_]]): Gen[Random with Sized, Seq[Schema.Field[_]]] =
    Gen.setOfBounded(1, 8)(anyLabel).flatMap { keySet =>
      Gen.setOfN(keySet.size)(schemaGen).map { schemas =>
        keySet
          .zip(schemas)
          .map {
            case (label, schema) => Schema.Field(label, schema)
          }
          .toSeq
      }
    }

  def anyStructure[A](schema: Schema[A]): Gen[Random with Sized, Seq[Schema.Field[A]]] =
    Gen
      .setOfBounded(1, 8)(
        anyLabel.map(Schema.Field(_, schema))
      )
      .map(_.toSeq)

  def anyEnumeration(schemaGen: Gen[Random with Sized, Schema[_]]): Gen[Random with Sized, ListMap[String, Schema[_]]] =
    Gen
      .setOfBounded(1, 8)(
        anyLabel.zip(schemaGen)
      )
      .map(ListMap.empty ++ _)

  def anyEnumeration[A](schema: Schema[A]): Gen[Random with Sized, ListMap[String, Schema[A]]] =
    Gen.setOfBounded(1, 8)(anyLabel.map(_ -> schema)).map(ListMap.empty ++ _)

  val anyPrimitive: Gen[Random, Schema.Primitive[_]] =
    StandardTypeGen.anyStandardType.map(Schema.Primitive(_, Chunk.empty))

  type PrimitiveAndGen[A] = (Schema.Primitive[A], Gen[Random with Sized, A])

  val anyPrimitiveAndGen: Gen[Random, PrimitiveAndGen[_]] =
    StandardTypeGen.anyStandardTypeAndGen.map {
      case (standardType, gen) => Schema.Primitive(standardType, Chunk.empty) -> gen
    }

  type PrimitiveAndValue[A] = (Schema.Primitive[A], A)

  val anyPrimitiveAndValue: Gen[Random with Sized, PrimitiveAndValue[_]] =
    for {
      (schema, gen) <- anyPrimitiveAndGen
      value         <- gen
    } yield schema -> value

  def anyOptional(schemaGen: Gen[Random with Sized, Schema[_]]): Gen[Random with Sized, Schema.Optional[_]] =
    schemaGen.map(Schema.Optional(_))

  type OptionalAndGen[A] = (Schema.Optional[A], Gen[Random with Sized, Option[A]])

  val anyOptionalAndGen: Gen[Random with Sized, OptionalAndGen[_]] =
    anyPrimitiveAndGen.map {
      case (schema, gen) => Schema.Optional(schema) -> Gen.option(gen)
    }

  type OptionalAndValue[A] = (Schema.Optional[A], Option[A])

  val anyOptionalAndValue: Gen[Random with Sized, OptionalAndValue[_]] =
    for {
      (schema, gen) <- anyOptionalAndGen
      value         <- gen
    } yield schema -> value

  val anyEither: Gen[Random with Sized, Schema.EitherSchema[_, _]] =
    for {
      left  <- anyPrimitive
      right <- anyPrimitive
    } yield Schema.EitherSchema(left, right)

  type EitherAndGen[A, B] = (Schema.EitherSchema[A, B], Gen[Random with Sized, Either[A, B]])

  val anyEitherAndGen: Gen[Random with Sized, EitherAndGen[_, _]] =
    for {
      (leftSchema, leftGen)   <- anyPrimitiveAndGen
      (rightSchema, rightGen) <- anyPrimitiveAndGen
    } yield (Schema.EitherSchema(leftSchema, rightSchema), Gen.either(leftGen, rightGen))

  type EitherAndValue[A, B] = (Schema.EitherSchema[A, B], Either[A, B])

  val anyEitherAndValue: Gen[Random with Sized, EitherAndValue[_, _]] =
    for {
      (schema, gen) <- anyEitherAndGen
      value         <- gen
    } yield (schema, value)

  lazy val anyTuple: Gen[Random with Sized, Schema.Tuple[_, _]] =
    anySchema.zipWith(anySchema) { (a, b) =>
      Schema.Tuple(a, b)
    }

  type TupleAndGen[A, B] = (Schema.Tuple[A, B], Gen[Random with Sized, (A, B)])

  val anyTupleAndGen: Gen[Random with Sized, TupleAndGen[_, _]] =
    for {
      (schemaA, genA) <- anyPrimitiveAndGen
      (schemaB, genB) <- anyPrimitiveAndGen
    } yield Schema.Tuple(schemaA, schemaB) -> genA.zip(genB)

  type TupleAndValue[A, B] = (Schema.Tuple[A, B], (A, B))

  val anyTupleAndValue: Gen[Random with Sized, TupleAndValue[_, _]] =
    for {
      (schema, gen)    <- anyTupleAndGen
      (valueA, valueB) <- gen
    } yield schema -> ((valueA, valueB))

  val anySequence: Gen[Random with Sized, Schema[Chunk[Any]]] =
    anySchema.map(Schema.chunk(_).asInstanceOf[Schema[Chunk[Any]]])

  type SequenceAndGen[A] = (Schema[Chunk[A]], Gen[Random with Sized, Chunk[A]])

  val anySequenceAndGen: Gen[Random with Sized, SequenceAndGen[_]] =
    anyPrimitiveAndGen.map {
      case (schema, gen) =>
        Schema.chunk(schema) -> Gen.chunkOf(gen)
    }

  type SequenceAndValue[A] = (Schema[Chunk[A]], Chunk[A])

  val anySequenceAndValue: Gen[Random with Sized, SequenceAndValue[_]] =
    for {
      (schema, gen) <- anySequenceAndGen
      value         <- gen
    } yield schema -> value

  def toCaseSet(cases: ListMap[String, Schema[_]]): CaseSet.Aux[Any] =
    cases.foldRight[CaseSet.Aux[Any]](CaseSet.Empty[Any]()) {
      case ((id, codec), acc) =>
        val _case = Schema.Case[Any, Any](id, codec.asInstanceOf[Schema[Any]], _.asInstanceOf[Any], Chunk.empty)
        CaseSet.Cons(_case, acc)
    }

  lazy val anyMap: Gen[Random with Sized, Schema.MapSchema[_, _]] =
    anySchema.zipWith(anySchema) { (a, b) =>
      Schema.MapSchema(a, b, Chunk.empty)
    }
  type MapAndGen[K, V] = (Schema.MapSchema[K, V], Gen[Random with Sized, Map[K, V]])

  val anyMapAndGen: Gen[Random with Sized, MapAndGen[_, _]] =
    for {
      (schemaK, genK) <- anyPrimitiveAndGen
      (schemaV, genV) <- anyPrimitiveAndGen
    } yield Schema.MapSchema(schemaK, schemaV, Chunk.empty) -> (genK.flatMap(k => genV.map(v => Map(k -> v))))

  type MapAndValue[K, V] = (Schema.MapSchema[K, V], Map[K, V])

  val anyMapAndValue: Gen[Random with Sized, MapAndValue[_, _]] =
    for {
      (schema, gen) <- anyMapAndGen
      map           <- gen
    } yield schema -> map

  type SetAndGen[A] = (Schema.SetSchema[A], Gen[Random with Sized, Set[A]])

  val anySetAndGen: Gen[Random with Sized, SetAndGen[_]] =
    anyPrimitiveAndGen.map {
      case (schema, gen) =>
        Schema.SetSchema(schema, Chunk.empty) -> Gen.setOf(gen)
    }

  type SetAndValue[A] = (Schema.SetSchema[A], Set[A])

  val anySetAndValue: Gen[Random with Sized, SetAndValue[_]] =
    for {
      (schema, gen) <- anySetAndGen
      value         <- gen
    } yield schema -> value

  val anyEnumeration: Gen[Random with Sized, Schema[Any]] =
    anyEnumeration(anySchema).map(toCaseSet).map(Schema.enumeration[Any, CaseSet.Aux[Any]](_))

  type EnumerationAndGen = (Schema[Any], Gen[Random with Sized, Any])

  val anyEnumerationAndGen: Gen[Random with Sized, EnumerationAndGen] =
    for {
      primitiveAndGen <- anyPrimitiveAndGen
      structure       <- anyEnumeration(primitiveAndGen._1)
      primitiveValue  <- primitiveAndGen._2
    } yield {
      val gen = Gen.oneOf(structure.keys.map(Gen.const(_)).toSeq: _*).map(_ => primitiveValue)
      Schema.enumeration[Any, CaseSet.Aux[Any]](toCaseSet(structure)) -> gen
    }

  type EnumerationAndValue = (Schema[Any], Any)

  val anyEnumerationAndValue: Gen[Random with Sized, EnumerationAndValue] =
    for {
      (schema, gen) <- anyEnumerationAndGen
      value         <- gen
    } yield schema -> value

  val anyRecord: Gen[Random with Sized, Schema[ListMap[String, _]]] =
    anyStructure(anySchema).map(Schema.record)

  type GenericRecordAndGen = (Schema[ListMap[String, _]], Gen[Random with Sized, ListMap[String, _]])

  val anyGenericRecordAndGen: Gen[Random with Sized, GenericRecordAndGen] =
    for {
      (schema, gen) <- anyPrimitiveAndGen
      structure     <- anyStructure(schema)
    } yield {
      val valueGen = Gen
        .const(structure.map(_.label))
        .zip(Gen.listOfN(structure.size)(gen))
        .map {
          case (labels, values) =>
            labels.zip(values)
        }
        .map(ListMap.empty ++ _)

      Schema.record(structure: _*) -> valueGen
    }

  type RecordAndValue = (Schema[ListMap[String, _]], ListMap[String, _])

  val anyRecordAndValue: Gen[Random with Sized, RecordAndValue] =
    for {
      (schema, gen) <- anyGenericRecordAndGen
      value         <- gen
    } yield schema -> value

  val anyRecordOfRecordsAndValue: Gen[Random with Sized, RecordAndValue] =
    for {
      (schema1, gen1) <- anyGenericRecordAndGen
      (schema2, gen2) <- anyGenericRecordAndGen
      (schema3, gen3) <- anyGenericRecordAndGen
      keys            <- Gen.setOfN(3)(anyLabel).map(_.toSeq)
      (key1, value1)  <- Gen.const(keys(0)).zip(gen1)
      (key2, value2)  <- Gen.const(keys(1)).zip(gen2)
      (key3, value3)  <- Gen.const(keys(2)).zip(gen3)
    } yield Schema.record(Schema.Field(key1, schema1), Schema.Field(key2, schema2), Schema.Field(key3, schema3)) -> ListMap(
      (key1, value1),
      (key2, value2),
      (key3, value3)
    )

  type SequenceTransform[A] = Schema.Transform[Chunk[A], List[A]]

  val anySequenceTransform: Gen[Random with Sized, SequenceTransform[_]] = {
    anySequence.map(schema => transformSequence(schema))
  }

  type SequenceTransformAndGen[A] = (SequenceTransform[A], Gen[Random with Sized, List[A]])

  val anySequenceTransformAndGen: Gen[Random with Sized, SequenceTransformAndGen[_]] =
    anyPrimitiveAndGen.map {
      case (schema, gen) =>
        transformSequence(Schema.chunk(schema)) -> Gen.listOf(gen)
    }

  // TODO: Add some random Left values.
  private def transformSequence[A](schema: Schema[Chunk[A]]): SequenceTransform[A] =
    Schema.Transform[Chunk[A], List[A]](
      schema,
      chunk => Right(chunk.toList),
      list => Right(Chunk.fromIterable(list)),
      Chunk.empty
    )

  type SequenceTransformAndValue[A] = (SequenceTransform[A], List[A])

  val anySequenceTransformAndValue: Gen[Random with Sized, SequenceTransformAndValue[_]] =
    for {
      (schema, gen) <- anySequenceTransformAndGen
      value         <- gen
    } yield schema -> value

  type RecordTransform[A] = Schema.Transform[ListMap[String, _], A]

  val anyRecordTransform: Gen[Random with Sized, RecordTransform[_]] = {
    anyRecord.map(schema => transformRecord(schema))
  }

  type RecordTransformAndGen[A] = (RecordTransform[A], Gen[Random with Sized, A])

  // TODO: How do we generate a value of a type that we know nothing about?
  val anyRecordTransformAndGen: Gen[Random with Sized, RecordTransformAndGen[_]] =
    Gen.empty
  //    anyRecordAndGen.map {
  //      case (schema, gen) => transformRecord(schema) -> gen
  //    }

  // TODO: Dynamically generate a case class.
  def transformRecord[A](schema: Schema[ListMap[String, _]]): RecordTransform[A] =
    Schema.Transform[ListMap[String, _], A](
      schema,
      _ => Left("Not implemented."),
      _ => Left("Not implemented."),
      Chunk.empty
    )

  type RecordTransformAndValue[A] = (RecordTransform[A], A)

  val anyRecordTransformAndValue: Gen[Random with Sized, RecordTransformAndValue[_]] =
    for {
      (schema, gen) <- anyRecordTransformAndGen
      value         <- gen
    } yield schema -> value

  type EnumerationTransform[A] = Schema.Transform[Any, A]

  val anyEnumerationTransform: Gen[Random with Sized, EnumerationTransform[_]] = {
    anyEnumeration.map(schema => transformEnumeration(schema))
  }

  type EnumerationTransformAndGen[A] = (EnumerationTransform[A], Gen[Random with Sized, A])

  // TODO: How do we generate a value of a type that we know nothing about?
  val anyEnumerationTransformAndGen: Gen[Random with Sized, EnumerationTransformAndGen[_]] =
    Gen.empty
  //    anyEnumerationAndGen.map {
  //      case (schema, gen) => transformEnumeration(schema) -> gen
  //    }

  // TODO: Dynamically generate a sealed trait and case/value classes.
  def transformEnumeration[A](schema: Schema[Any]): EnumerationTransform[_] =
    Schema.Transform[Any, A](schema, _ => Left("Not implemented."), _ => Left("Not implemented."), Chunk.empty)

  type EnumerationTransformAndValue[A] = (EnumerationTransform[A], A)

  val anyEnumerationTransformAndValue: Gen[Random with Sized, EnumerationTransformAndValue[_]] =
    for {
      (schema, gen) <- anyEnumerationTransformAndGen
      value         <- gen
    } yield schema -> value

  val anyTransform: Gen[Random with Sized, Schema.Transform[_, _]] = Gen.oneOf(
    anySequenceTransform,
    anyRecordTransform,
    anyEnumerationTransform
  )

  type TransformAndValue[A] = (Schema.Transform[_, A], A)

  val anyTransformAndValue: Gen[Random with Sized, TransformAndValue[_]] =
    Gen.oneOf[Random with Sized, TransformAndValue[_]](
      anySequenceTransformAndValue
      // anyRecordTransformAndValue,
      // anyEnumerationTransformAndValue
    )

  type TransformAndGen[A] = (Schema.Transform[_, A], Gen[Random with Sized, A])

  val anyTransformAndGen: Gen[Random with Sized, TransformAndGen[_]] =
    Gen.oneOf[Random with Sized, TransformAndGen[_]](
      anySequenceTransformAndGen,
      anyRecordTransformAndGen,
      anyEnumerationTransformAndGen
    )

  lazy val anySchema: Gen[Random with Sized, Schema[_]] =
    for {
      treeDepth <- Gen.bounded(0, 2)(Gen.const(_))
      tree      <- anyTree(treeDepth)
    } yield tree

  def anyValueForSchema[A](schema: Schema[A]): Gen[Random with Sized, (Schema[A], A)] =
    DynamicValueGen
      .anyDynamicValueOfSchema(schema)
      .map { dynamic =>
        schema -> schema.fromDynamic(dynamic).toOption.get
      }

  type SchemaAndValue[A] = (Schema[A], A)

  lazy val anySchemaAndValue: Gen[Random with Sized, SchemaAndValue[_]] =
    for {
      schema  <- anySchema
      dynamic <- DynamicValueGen.anyDynamicValueOfSchema(schema)
    } yield (schema -> schema.fromDynamic(dynamic).toOption.get).asInstanceOf[SchemaAndValue[Any]]

  sealed trait Arity
  case object Arity0                  extends Arity
  final case class Arity1(value: Int) extends Arity

  object Arity1 {
    implicit val schema: Schema[Arity1] = DeriveSchema.gen[Arity1]
  }
  final case class Arity2(value1: String, value2: Arity1) extends Arity

  object Arity2 {
    implicit val schema: Schema[Arity2] = DeriveSchema.gen[Arity2]
  }
  final case class Arity3(value1: String, value2: Arity2, value3: Arity1) extends Arity

  object Arity3 {
    implicit val schema: Schema[Arity3] = DeriveSchema.gen[Arity3]
  }
  final case class Arity24(
    a1: Arity1,
    a2: Arity2,
    a3: Arity3,
    f4: Int = 4,
    f5: Int = 5,
    f6: Int = 6,
    f7: Int = 7,
    f8: Int = 8,
    f9: Int = 9,
    f10: Int = 10,
    f11: Int = 11,
    f12: Int = 12,
    f13: Int = 13,
    f14: Int = 14,
    f15: Int = 15,
    f16: Int = 16,
    f17: Int = 17,
    f18: Int = 18,
    f19: Int = 19,
    f20: Int = 20,
    f21: Int = 21,
    f22: Int = 22,
    f23: Int = 23,
    f24: Int = 24
  ) extends Arity

  object Arity24 {
    implicit val schema: Schema[Arity24] = DeriveSchema.gen[Arity24]
  }

  object Arity {
    implicit val arityEnumSchema: Schema.Enum[Arity] = DeriveSchema.gen[Arity].asInstanceOf[Schema.Enum[Arity]]
  }

  lazy val anyArity1: Gen[Random with Sized, Arity1] = Gen.anyInt.map(Arity1(_))

  lazy val anyArity2: Gen[Random with Sized, Arity2] =
    for {
      s  <- Gen.anyString
      a1 <- anyArity1
    } yield Arity2(s, a1)

  lazy val anyArity3: Gen[Random with Sized, Arity3] =
    for {
      s  <- Gen.anyString
      a1 <- anyArity1
      a2 <- anyArity2
    } yield Arity3(s, a2, a1)

  lazy val anyArity24: Gen[Random with Sized, Arity24] =
    for {
      a1 <- anyArity1
      a2 <- anyArity2
      a3 <- anyArity3
    } yield Arity24(a1, a2, a3)

  lazy val anyArity: Gen[Random with Sized, Arity] = Gen.oneOf(anyArity1, anyArity2, anyArity3, anyArity24)

  type CaseClassAndGen[A] = (Schema[A], Gen[Sized with Random, A])

  type CaseClassAndValue[A] = (Schema[A], A)

  lazy val anyCaseClassSchema: Gen[Random with Sized, Schema[_]] =
    Gen.oneOf(
      Gen.const(Schema[Arity1]),
      Gen.const(Schema[Arity2]),
      Gen.const(Schema[Arity3]),
      Gen.const(Schema[Arity24])
    )

  val anyCaseClassAndGen: Gen[Random with Sized, CaseClassAndGen[_]] =
    anyCaseClassSchema.map {
      case s @ Schema.CaseClass1(_, _, _, _)             => (s -> anyArity1).asInstanceOf[CaseClassAndGen[Any]]
      case s @ Schema.CaseClass2(_, _, _, _, _, _)       => (s -> anyArity2).asInstanceOf[CaseClassAndGen[Any]]
      case s @ Schema.CaseClass3(_, _, _, _, _, _, _, _) => (s -> anyArity3).asInstanceOf[CaseClassAndGen[Any]]
      case s                                             => (s -> anyArity24).asInstanceOf[CaseClassAndGen[Any]]
    }

  val anyCaseClassAndValue: Gen[Random with Sized, CaseClassAndValue[_]] =
    for {
      (schema, gen) <- anyCaseClassAndGen
      value         <- gen
    } yield (schema -> value)

  type EnumAndGen[A] = (Schema[A], Gen[Random with Sized, A])

  type EnumAndValue[A] = (Schema[A], A)

  lazy val anyEnumSchema: Gen[Any, Schema.Enum[Arity]] = Gen.const(Arity.arityEnumSchema)

  val anyEnumAndGen: Gen[Random with Sized, EnumAndGen[_]] =
    anyEnumSchema.map(_ -> anyArity)

  val anyEnumAndValue: Gen[Random with Sized, EnumAndValue[_]] =
    for {
      (schema, gen) <- anyEnumAndGen
      value         <- gen
    } yield schema -> value

  lazy val anyLeaf: Gen[Random with Sized, Schema[_]] =
    Gen.oneOf(
      anyPrimitive,
      anyPrimitive.map(Schema.list(_)),
      anyPrimitive.map(_.optional),
      anyPrimitive.zip(anyPrimitive).map { case (l, r) => Schema.either(l, r) },
      anyPrimitive.zip(anyPrimitive).map { case (l, r) => Schema.tuple2(l, r) },
      anyStructure(anyPrimitive).map(fields => Schema.record(fields: _*)),
      Gen.const(Schema[Json]),
//      anyEnumeration(anyPrimitive).map(toCaseSet).map(Schema.enumeration[Any, CaseSet.Aux[Any]](_)),
      anyCaseClassSchema,
      anyEnumSchema
    )

  def anyTree(depth: Int): Gen[Random with Sized, Schema[_]] =
    if (depth == 0)
      anyLeaf
    else
      Gen.oneOf(
        anyTree(depth - 1).map(Schema.list(_)),
        // Nested optional cause some issues. Ignore them for now: See https://github.com/zio/zio-schema/issues/68
        anyTree(depth - 1).map {
          case s @ Schema.Optional(_, _) => s
          case s                         => Schema.option(s)
        },
        anyTree(depth - 1).zip(anyTree(depth - 1)).map { case (l, r) => Schema.either(l, r) },
        anyTree(depth - 1).zip(anyTree(depth - 1)).map { case (l, r) => Schema.tuple2(l, r) },
        anyStructure(anyTree(depth - 1)).map(fields => Schema.record(fields: _*)),
        Gen.const(Schema[Json])
//        anyEnumeration(anyTree(depth - 1)).map(toCaseSet).map(Schema.enumeration[Any, CaseSet.Aux[Any]](_))
      )

  type SchemaAndDerivedValue[A, B] = (Schema[A], Schema[B], Chunk[Either[A, B]])

  lazy val anyLeafAndValue: Gen[Random with Sized, SchemaAndValue[_]] =
    for {
      schema <- anyLeaf
      value  <- DynamicValueGen.anyDynamicValueOfSchema(schema)
    } yield (schema -> schema.fromDynamic(value).toOption.get).asInstanceOf[SchemaAndValue[Any]]

  lazy val anyTreeAndValue: Gen[Random with Sized, SchemaAndValue[_]] =
    for {
      schema <- anyTree(1)
      value  <- DynamicValueGen.anyDynamicValueOfSchema(schema)
    } yield (schema -> schema.fromDynamic(value).toOption.get).asInstanceOf[SchemaAndValue[Any]]

  sealed trait Json
  case object JNull                                extends Json
  case class JString(s: String)                    extends Json
  case class JNumber(l: Int)                       extends Json
  case class JDecimal(d: Double)                   extends Json
  case class JObject(fields: List[(String, Json)]) extends Json
  case class JArray(fields: List[Json])            extends Json

  object Json {
    implicit lazy val schema: Schema.Enum6[JArray, JDecimal, JNull.type, JNumber, JObject, JString, Json] =
      DeriveSchema.gen[Json]

    val leafGen: Gen[Random with Sized, Json] =
      Gen.oneOf(
        Gen.const(JNull),
        Gen.anyString.map(JString(_)),
        Gen.anyInt.map(JNumber(_))
      )

    val gen: Gen[Random with Sized, Json] =
      for {
        keys   <- Gen.setOfN(3)(Gen.anyString)
        values <- Gen.setOfN(3)(leafGen)
      } yield JObject(keys.zip(values).toList)
  }

  lazy val anyRecursiveType: Gen[Random with Sized, Schema[_]] =
    Gen.const(Schema[Json])

  lazy val anyRecursiveTypeAndValue: Gen[Random with Sized, SchemaAndValue[_]] =
    for {
      schema <- Gen.const(Schema[Json])
      value  <- Json.gen
    } yield (schema, value)

  case class SchemaTest[A](name: String, schema: StandardType[A], gen: Gen[Sized with Random, A])

  def schemasAndGens: List[SchemaTest[_]] = List(
    SchemaTest("String", StandardType.StringType, Gen.anyString),
    SchemaTest("Bool", StandardType.BoolType, Gen.boolean),
    SchemaTest("Short", StandardType.ShortType, Gen.anyShort),
    SchemaTest("Int", StandardType.IntType, Gen.anyInt),
    SchemaTest("Long", StandardType.LongType, Gen.anyLong),
    SchemaTest("Float", StandardType.FloatType, Gen.anyFloat),
    SchemaTest("Double", StandardType.DoubleType, Gen.anyDouble),
    SchemaTest("Binary", StandardType.BinaryType, Gen.chunkOf(Gen.anyByte)),
    SchemaTest("Char", StandardType.CharType, Gen.anyASCIIChar),
    SchemaTest("UUID", StandardType.UUIDType, Gen.anyUUID),
    SchemaTest(
      "BigDecimal",
      StandardType.BigDecimalType,
      Gen.anyDouble.map(d => java.math.BigDecimal.valueOf(d))
    ),
    SchemaTest(
      "BigInteger",
      StandardType.BigIntegerType,
      Gen.anyLong.map(n => java.math.BigInteger.valueOf(n))
    ),
    SchemaTest("DayOfWeek", StandardType.DayOfWeekType, JavaTimeGen.anyDayOfWeek),
    SchemaTest("Duration", StandardType.Duration(ChronoUnit.SECONDS), JavaTimeGen.anyDuration),
    SchemaTest("Instant", StandardType.InstantType(DateTimeFormatter.ISO_DATE_TIME), JavaTimeGen.anyInstant),
    SchemaTest("LocalDate", StandardType.LocalDateType(DateTimeFormatter.ISO_DATE), JavaTimeGen.anyLocalDate),
    SchemaTest(
      "LocalDateTime",
      StandardType.LocalDateTimeType(DateTimeFormatter.ISO_LOCAL_DATE_TIME),
      JavaTimeGen.anyLocalDateTime
    ),
    SchemaTest(
      "LocalTime",
      StandardType.LocalTimeType(DateTimeFormatter.ISO_LOCAL_TIME),
      JavaTimeGen.anyLocalTime
    ),
    SchemaTest("Month", StandardType.MonthType, JavaTimeGen.anyMonth),
    SchemaTest("MonthDay", StandardType.MonthDayType, JavaTimeGen.anyMonthDay),
    SchemaTest(
      "OffsetDateTime",
      StandardType.OffsetDateTimeType(DateTimeFormatter.ISO_OFFSET_DATE_TIME),
      JavaTimeGen.anyOffsetDateTime
    ),
    SchemaTest(
      "OffsetTime",
      StandardType.OffsetTimeType(DateTimeFormatter.ISO_OFFSET_TIME),
      JavaTimeGen.anyOffsetTime
    ),
    SchemaTest("Period", StandardType.PeriodType, JavaTimeGen.anyPeriod),
    SchemaTest("Year", StandardType.YearType, JavaTimeGen.anyYear),
    SchemaTest("YearMonth", StandardType.YearMonthType, JavaTimeGen.anyYearMonth),
    SchemaTest(
      "ZonedDateTime",
      StandardType.ZonedDateTimeType(DateTimeFormatter.ISO_ZONED_DATE_TIME),
      JavaTimeGen.anyZonedDateTime
    ),
    SchemaTest("ZoneId", StandardType.ZoneIdType, JavaTimeGen.anyZoneId),
    SchemaTest("ZoneOffset", StandardType.ZoneOffsetType, JavaTimeGen.anyZoneOffset),
    SchemaTest("UnitType", StandardType.UnitType, Gen.unit)
  )

}
