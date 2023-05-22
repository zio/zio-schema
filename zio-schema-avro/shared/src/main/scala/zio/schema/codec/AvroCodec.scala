package zio.schema.codec

import org.apache.avro.generic.{ GenericData, GenericDatumWriter, GenericRecordBuilder }
import org.apache.avro.io.EncoderFactory
import org.apache.avro.util.Utf8
import org.apache.avro.{ Conversions, LogicalTypes, Schema => SchemaAvro }
import zio.Chunk
import zio.schema.{ FieldSet, Schema, StandardType, TypeId }
import zio.stream.ZPipeline

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.util.UUID
import scala.collection.immutable.ListMap

object AvroCodec {

  implicit def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {

      val avroSchema: SchemaAvro =
        AvroSchemaCodec.encodeToApacheAvro(schema).getOrElse(throw new Exception("Avro schema could not be generated."))

      override def encode(value: A): Chunk[Byte] = {
        val baos        = new ByteArrayOutputStream()
        val datumWriter = new GenericDatumWriter[Any](avroSchema)
        val datum       = encodeValue(value, schema)
        val serializer  = EncoderFactory.get().directBinaryEncoder(baos, null)
        datumWriter.write(datum, serializer)
        val encoded = Chunk.fromArray(baos.toByteArray)
        serializer.flush()
        baos.close()
        encoded
      }

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] = ???

      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] = ???

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] = ???
    }

  private def encodeValue[A](a: A, schema: Schema[A]): Any = schema match {
    case Schema.Enum1(_, c1, _)                      => encodeEnum(a, schema, c1)
    case Schema.Enum2(_, c1, c2, _)                  => encodeEnum(a, schema, c1, c2)
    case Schema.Enum3(_, c1, c2, c3, _)              => encodeEnum(a, schema, c1, c2, c3)
    case Schema.GenericRecord(typeId, structure, _)  => encodeGenericRecord(a, typeId, structure)
    case Schema.Primitive(standardType, annotations) => encodePrimitive(a, standardType, annotations)
    case Schema.Sequence(element, _, g, _, _)        => encodeSequence(element, g(a))
    case Schema.Set(element, _)                      => encodeSet(element, a)
    case mapSchema: Schema.Map[_, _] =>
      encodeMap(mapSchema.asInstanceOf[Schema.Map[Any, Any]], a.asInstanceOf[scala.collection.immutable.Map[Any, Any]])
    case Schema.Transform(schema, _, g, _, _) =>
      g(a).map(encodeValue(_, schema)).getOrElse(throw new Exception("Transform failed."))
    case Schema.Optional(schema, annotations)               => encodeOption(schema, a)
    case Schema.Tuple2(left, right, _)                      => encodeTuple2(left, right, a)
    case Schema.Either(left, right, _)                      => encodeEither(left, right, a)
    case Schema.Lazy(schema0)                               => encodeValue(a, schema0())
    case Schema.CaseClass0(_, _, _)                         => encodePrimitive((), StandardType.UnitType, Chunk.empty)
    case Schema.CaseClass1(_, f, _, _)                      => encodeCaseClass(schema, a, f)
    case Schema.CaseClass2(_, f0, f1, _, _)                 => encodeCaseClass(schema, a, f0, f1)
    case Schema.CaseClass3(_, f0, f1, f2, _, _)             => encodeCaseClass(schema, a, f0, f1, f2)
    case Schema.CaseClass4(_, f0, f1, f2, f3, _, _)         => encodeCaseClass(schema, a, f0, f1, f2, f3)
    case Schema.CaseClass5(_, f0, f1, f2, f3, f4, _, _)     => encodeCaseClass(schema, a, f0, f1, f2, f3, f4)
    case Schema.CaseClass6(_, f0, f1, f2, f3, f4, f5, _, _) => encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5)
    case Schema.CaseClass7(_, f0, f1, f2, f3, f4, f5, f6, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6)
    case Schema.CaseClass8(_, f0, f1, f2, f3, f4, f5, f6, f7, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6, f7)
    case Schema.CaseClass9(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6, f7, f8)
    case Schema.CaseClass10(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9)
    case Schema.CaseClass11(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
    case Schema.CaseClass12(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11)
    case Schema.CaseClass13(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12)
    case Schema.CaseClass14(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13)
    case Schema.CaseClass15(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14)
    case Schema.CaseClass16(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15)
    case Schema.CaseClass17(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16)
    case Schema.CaseClass18(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17)
    case Schema
          .CaseClass19(_, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, _, _) =>
      encodeCaseClass(schema, a, f0, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18)
    case Schema.CaseClass20(
        _,
        f0,
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
        _
        ) =>
      encodeCaseClass(
        schema,
        a,
        f0,
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
        f19
      )
    case Schema.CaseClass21(
        _,
        f0,
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
        tail
        ) =>
      encodeCaseClass(
        schema,
        a,
        f0,
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
        tail._1
      )
    case Schema.CaseClass22(
        _,
        f0,
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
        tail
        ) =>
      encodeCaseClass(
        schema,
        a,
        f0,
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
        tail._1,
        tail._2
      )

  }

  private def encodePrimitive[A](a: A, standardType: StandardType[A], annotations: Chunk[Any]): Any =
    standardType match {
      case StandardType.UnitType   => null
      case StandardType.StringType => new Utf8(a.asInstanceOf[String])
      case StandardType.BoolType   => java.lang.Boolean.valueOf(a.asInstanceOf[Boolean])
      case StandardType.ByteType   => java.lang.Byte.valueOf(a.asInstanceOf[Byte])
      case StandardType.ShortType  => java.lang.Short.valueOf(a.asInstanceOf[Short])
      case StandardType.IntType    => java.lang.Integer.valueOf(a.asInstanceOf[Int])
      case StandardType.LongType   => java.lang.Long.valueOf(a.asInstanceOf[Long])
      case StandardType.FloatType  => java.lang.Float.valueOf(a.asInstanceOf[Float])
      case StandardType.DoubleType => java.lang.Double.valueOf(a.asInstanceOf[Double])
      case StandardType.BinaryType => ByteBuffer.wrap(a.asInstanceOf[Chunk[Byte]].toArray)
      case StandardType.CharType   => java.lang.Short.valueOf(a.asInstanceOf[Char].toShort)
      case StandardType.UUIDType   => new Utf8(a.asInstanceOf[UUID].toString)
      case StandardType.BigDecimalType =>
        val converter = new Conversions.DecimalConversion()
        val schema = AvroSchemaCodec
          .encodeToApacheAvro(Schema.Primitive(StandardType.BigDecimalType, Chunk.empty))
          .getOrElse(throw new Exception("Avro schema could not be generated for BigDecimal."))
        converter.toBytes(a.asInstanceOf[java.math.BigDecimal], schema, LogicalTypes.decimal(48, 24))

      case StandardType.BigIntegerType =>
        val converter = new Conversions.DecimalConversion()
        val schema = AvroSchemaCodec
          .encodeToApacheAvro(Schema.Primitive(StandardType.BigIntegerType, Chunk.empty))
          .getOrElse(throw new Exception("Avro schema could not be generated for BigInteger."))
        val transformed = BigDecimal(a.asInstanceOf[java.math.BigInteger])
        converter.toBytes(transformed.underlying(), schema, LogicalTypes.decimal(48, 24))

      case StandardType.DayOfWeekType =>
        a.asInstanceOf[java.time.DayOfWeek].getValue

      case StandardType.MonthType =>
        a.asInstanceOf[java.time.Month].getValue
      case StandardType.MonthDayType =>
        val monthDay = a.asInstanceOf[java.time.MonthDay]
        monthDay.toString
      case StandardType.PeriodType =>
        val period = a.asInstanceOf[java.time.Period]
        period.toString
      case StandardType.YearType =>
        a.asInstanceOf[java.time.Year].getValue
      case StandardType.YearMonthType =>
        val yearMonth = a.asInstanceOf[java.time.YearMonth]
        yearMonth.toString
      case StandardType.ZoneIdType =>
        a.asInstanceOf[java.time.ZoneId].toString
      case StandardType.ZoneOffsetType =>
        a.asInstanceOf[java.time.ZoneOffset].getTotalSeconds
      case StandardType.DurationType =>
        val duration = a.asInstanceOf[java.time.Duration]
        duration.toString
      case StandardType.InstantType =>
        val instant = a.asInstanceOf[java.time.Instant]
        instant.toString
      case StandardType.LocalDateType =>
        val localDate = a.asInstanceOf[java.time.LocalDate]
        localDate.toString
      case StandardType.LocalTimeType =>
        val localTime = a.asInstanceOf[java.time.LocalTime]
        localTime.toString
      case StandardType.LocalDateTimeType =>
        val localDateTime = a.asInstanceOf[java.time.LocalDateTime]
        localDateTime.toString
      case StandardType.OffsetTimeType =>
        val offsetTime = a.asInstanceOf[java.time.OffsetTime]
        offsetTime.toString
      case StandardType.OffsetDateTimeType =>
        val offsetDateTime = a.asInstanceOf[java.time.OffsetDateTime]
        offsetDateTime.toString
      case StandardType.ZonedDateTimeType =>
        val zonedDateTime = a.asInstanceOf[java.time.ZonedDateTime]
        zonedDateTime.toString
    }

  private def encodeSequence[A](schema: Schema[A], v: Chunk[A]): Any = {
    val array = new Array[Any](v.size)
    v.zipWithIndex.foreach {
      case (a, i) =>
        array(i) = encodeValue(a, schema)
    }
    java.util.Arrays.asList(array: _*)

  }

  private def encodeSet[A](schema: Schema[A], v: scala.collection.immutable.Set[A]): Any = {
    val array = new Array[Any](v.size)
    v.zipWithIndex.foreach {
      case (a, i) =>
        array(i) = encodeValue(a, schema)
    }
    java.util.Arrays.asList(array: _*)
  }

  private def encodeMap[K, V](schema: Schema.Map[K, V], v: Map[K, V]): Any = {
    import scala.jdk.CollectionConverters._
    val map = v.map {
      case (k, v) =>
        encodeValue(k, schema.keySchema) -> encodeValue(v, schema.valueSchema)
    }

    map.asJava

  }

  private def encodeOption[A](schema: Schema[A], v: Option[A]): Any =
    v.map(encodeValue(_, schema)).orNull

  private def encodeEither[A, B](left: Schema[A], right: Schema[B], either: scala.util.Either[A, B]): Any = {
    val schema = AvroSchemaCodec
      .encodeToApacheAvro(Schema.Either(left, right, Chunk.empty))
      .getOrElse(throw new Exception("Avro schema could not be generated for Either."))

    val record = new GenericRecordBuilder(schema)
    val result = either match {
      case Left(a)  => record.set("value", encodeValue(a, left))
      case Right(b) => record.set("value", encodeValue(b, right))
    }

    result.build()
  }

  private def encodeTuple2[A](schema1: Schema[Any], schema2: Schema[Any], a: A) = {
    val schema = AvroSchemaCodec
      .encodeToApacheAvro(Schema.Tuple2(schema1, schema2, Chunk.empty))
      .getOrElse(throw new Exception("Avro schema could not be generated for Tuple2."))
    val record = new GenericData.Record(schema)
    val tuple  = a.asInstanceOf[(Any, Any)]
    record.put("_1", encodeValue(tuple._1, schema1))
    record.put("_2", encodeValue(tuple._2, schema2))
    record
  }

  private def encodeGenericRecord[A](a: A, typeId: TypeId, structure: FieldSet): Any = {
    val schema = AvroSchemaCodec
      .encodeToApacheAvro(Schema.GenericRecord(typeId, structure, Chunk.empty))
      .getOrElse(throw new Exception("Avro schema could not be generated for GenericRecord."))
    val record = new GenericData.Record(schema)
    val data   = a.asInstanceOf[ListMap[String, _]]
    structure.toChunk
      .map(schema => schema.name -> encodeValue(data(schema.name), schema.schema.asInstanceOf[Schema[Any]]))
      .foreach {
        case (name, value) => record.put(name, value)
      }
    record
  }

  private def encodeCaseClass[Z](schemaRaw: Schema[Z], value: Z, fields: (Schema.Field[Z, _])*): Any = {
    val schema = AvroSchemaCodec
      .encodeToApacheAvro(schemaRaw)
      .getOrElse(throw new Exception("Avro schema could not be generated for CaseClass."))
    val record = new GenericData.Record(schema)
    fields.foreach { field =>
      record.put(field.name, encodeValue(field.get(value), field.schema.asInstanceOf[Schema[Any]]))
    }
    record
  }

  private def encodeEnum[Z](value: Z, schemaRaw: Schema[Z], cases: Schema.Case[Z, _]*): Any = {
    //val schema = AvroSchemaCodec.encodeToApacheAvro(schemaRaw).getOrElse(throw new Exception("Avro schema could not be generated for CaseClass."))
    val fieldIndex = cases.indexWhere(c => c.deconstructOption(value).isDefined)
    if (fieldIndex >= 0) {
      val subtypeCase = cases(fieldIndex)
      encodeValue(subtypeCase.deconstruct(value), subtypeCase.schema.asInstanceOf[Schema[Any]])
    } else {
      throw new Exception("Could not find matching case for enum value.")
    }
  }
}
