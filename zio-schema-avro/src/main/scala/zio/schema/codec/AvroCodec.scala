package zio.schema.codec

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.util.UUID

import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters._
import scala.util.Try

import org.apache.avro.generic.{
  GenericData,
  GenericDatumReader,
  GenericDatumWriter,
  GenericRecord,
  GenericRecordBuilder
}
import org.apache.avro.io.{ DecoderFactory, EncoderFactory }
import org.apache.avro.util.Utf8
import org.apache.avro.{ Conversions, LogicalTypes, Schema => SchemaAvro }

import zio.schema.{ Fallback, FieldSet, Schema, StandardType, TypeId }
import zio.stream.ZPipeline
import zio.{ Chunk, Unsafe, ZIO }

object AvroCodec {

  trait ExtendedBinaryCodec[A] extends BinaryCodec[A] {
    def encodeGenericRecord(value: A)(implicit schema: Schema[A]): GenericData.Record
    def decodeGenericRecord(value: GenericRecord)(implicit schema: Schema[A]): Either[DecodeError, A]
  }

  implicit def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): ExtendedBinaryCodec[A] =
    new ExtendedBinaryCodec[A] {

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

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] = ZPipeline.mapChunks { chunk =>
        chunk.flatMap(encode)
      }

      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] = {
        val datumReader = new GenericDatumReader[Any](avroSchema)
        val decoder     = DecoderFactory.get().binaryDecoder(whole.toArray, null)
        val decoded     = datumReader.read(null, decoder)
        decodeValue(decoded, schema)
      }

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] = ZPipeline.mapChunksZIO { chunk =>
        ZIO.fromEither(
          decode(chunk).map(Chunk(_))
        )
      }

      override def encodeGenericRecord(value: A)(implicit schema: Schema[A]): GenericData.Record =
        encodeValue(value, schema).asInstanceOf[GenericData.Record]

      override def decodeGenericRecord(value: GenericRecord)(implicit schema: Schema[A]): Either[DecodeError, A] =
        decodeValue(value, schema)
    }

  private def decodeValue[A](raw: Any, schema: Schema[A]): Either[DecodeError, A] = schema match {
    case Schema.Enum1(_, c1, _)                     => decodeEnum(raw, c1).map(_.asInstanceOf[A])
    case Schema.Enum2(_, c1, c2, _)                 => decodeEnum(raw, c1, c2).map(_.asInstanceOf[A])
    case Schema.Enum3(_, c1, c2, c3, _)             => decodeEnum(raw, c1, c2, c3).map(_.asInstanceOf[A])
    case Schema.Enum4(_, c1, c2, c3, c4, _)         => decodeEnum(raw, c1, c2, c3, c4).map(_.asInstanceOf[A])
    case Schema.Enum5(_, c1, c2, c3, c4, c5, _)     => decodeEnum(raw, c1, c2, c3, c4, c5).map(_.asInstanceOf[A])
    case Schema.Enum6(_, c1, c2, c3, c4, c5, c6, _) => decodeEnum(raw, c1, c2, c3, c4, c5, c6).map(_.asInstanceOf[A])
    case Schema.Enum7(_, c1, c2, c3, c4, c5, c6, c7, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7).map(_.asInstanceOf[A])
    case Schema.Enum8(_, c1, c2, c3, c4, c5, c6, c7, c8, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8).map(_.asInstanceOf[A])
    case Schema.Enum9(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9).map(_.asInstanceOf[A])
    case Schema.Enum10(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10).map(_.asInstanceOf[A])
    case Schema.Enum11(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11).map(_.asInstanceOf[A])
    case Schema.Enum12(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12).map(_.asInstanceOf[A])
    case Schema.Enum13(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13).map(_.asInstanceOf[A])
    case Schema.Enum14(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14).map(_.asInstanceOf[A])
    case Schema.Enum15(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15).map(_.asInstanceOf[A])
    case Schema.Enum16(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16).map(_.asInstanceOf[A])
    case Schema.Enum17(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17).map(_.asInstanceOf[A])
    case Schema.Enum18(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18).map(
        _.asInstanceOf[A]
      )
    case Schema.Enum19(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19).map(
        _.asInstanceOf[A]
      )
    case Schema
          .Enum20(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20).map(
        _.asInstanceOf[A]
      )
    case Schema.Enum21(
        _,
        c1,
        c2,
        c3,
        c4,
        c5,
        c6,
        c7,
        c8,
        c9,
        c10,
        c11,
        c12,
        c13,
        c14,
        c15,
        c16,
        c17,
        c18,
        c19,
        c20,
        c21,
        _
        ) =>
      decodeEnum(raw, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
        .map(_.asInstanceOf[A])
    case Schema.Enum22(
        _,
        c1,
        c2,
        c3,
        c4,
        c5,
        c6,
        c7,
        c8,
        c9,
        c10,
        c11,
        c12,
        c13,
        c14,
        c15,
        c16,
        c17,
        c18,
        c19,
        c20,
        c21,
        c22,
        _
        ) =>
      decodeEnum(
        raw,
        c1,
        c2,
        c3,
        c1,
        c2,
        c3,
        c4,
        c5,
        c6,
        c7,
        c8,
        c9,
        c10,
        c11,
        c12,
        c13,
        c14,
        c15,
        c16,
        c17,
        c18,
        c19,
        c20,
        c21,
        c22
      ).map(_.asInstanceOf[A])
    case s0 @ Schema.CaseClass0(_, _, _) =>
      decodePrimitiveValues(raw, StandardType.UnitType).map(_ => s0.defaultConstruct())
    case s1 @ Schema.CaseClass1(_, _, _, _) => decodeCaseClass1(raw, s1)
    case record: Schema.Record[_]           => decodeRecord(raw, record).map(_.asInstanceOf[A])
    case Schema.Sequence(element, f, _, _, _) =>
      decodeSequence(raw, element.asInstanceOf[Schema[Any]]).map(f.asInstanceOf[Chunk[Any] => A])
    case Schema.Set(element, _) => decodeSequence(raw, element.asInstanceOf[Schema[Any]]).map(_.toSet.asInstanceOf[A])
    case mapSchema: Schema.Map[_, _] =>
      decodeMap(raw, mapSchema.asInstanceOf[Schema.Map[Any, Any]]).map(_.asInstanceOf[A])
    case Schema.Transform(schema, f, _, _, _) =>
      decodeValue(raw, schema).flatMap(
        a => f(a).left.map(msg => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), msg))
      )
    case Schema.Primitive(standardType, _) => decodePrimitiveValues(raw, standardType)
    case Schema.Optional(schema, _)        => decodeOptionalValue(raw, schema)
    case Schema.Fail(message, _)           => Left(DecodeError.MalformedFieldWithPath(Chunk.empty, message))
    case Schema.Tuple2(left, right, _)     => decodeTuple2(raw, left, right).map(_.asInstanceOf[A])
    case Schema.Either(left, right, _)     => decodeEitherValue(raw, left, right)
    case s @ Schema.Fallback(_, _, _, _)   => decodeFallbackValue(raw, s)
    case lzy @ Schema.Lazy(_)              => decodeValue(raw, lzy.schema)
    case unknown                           => Left(DecodeError.MalformedFieldWithPath(Chunk.empty, s"Unknown schema: $unknown"))
  }

  private def decodeCaseClass1[A, Z](raw: Any, schema: Schema.CaseClass1[A, Z]) =
    decodeValue(raw, schema.field.schema).map(schema.defaultConstruct)

  private def decodeEnum[Z](raw: Any, cases: Schema.Case[Z, _]*): Either[DecodeError, Any] =
    raw match {
      case enums: GenericData.EnumSymbol =>
        decodeGenericEnum(enums.toString, None, cases: _*)
      case gr: GenericData.Record =>
        val enumCaseName = gr.getSchema.getFullName
        if (gr.hasField("value")) {
          val enumCaseValue = gr.get("value")
          decodeGenericEnum[Z](enumCaseName, Some(enumCaseValue), cases: _*)
        } else {
          decodeGenericEnum[Z](enumCaseName, None, cases: _*)
        }
      case _ => Left(DecodeError.MalformedFieldWithPath(Chunk.single("Error"), s"Unknown enum: $raw"))
    }

  private def decodeGenericEnum[Z](
    enumCaseName: String,
    enumCaseValue: Option[AnyRef],
    cases: Schema.Case[Z, _]*
  ): Either[DecodeError, Any] =
    cases
      .find(_.id == enumCaseName)
      .map(s => decodeValue(enumCaseValue.getOrElse(s), s.schema))
      .toRight(DecodeError.MalformedFieldWithPath(Chunk.single("Error"), s"Unknown enum value: $enumCaseName"))
      .flatMap(identity)

  private def decodeRecord[A](value: A, schema: Schema.Record[_]) = {
    val record = value.asInstanceOf[GenericRecord]
    val fields = schema.fields
    val decodedFields: Either[DecodeError, ListMap[String, Any]] =
      fields.foldLeft[Either[DecodeError, ListMap[String, Any]]](Right(ListMap.empty)) {
        case (Right(acc), field) =>
          val fieldName  = field.name
          val fieldValue = record.get(fieldName)
          val decodedField = decodeValue(fieldValue, field.schema).map { value =>
            acc + (fieldName -> value)
          }
          decodedField
        case (Left(error), _) => Left(error)
      }
    implicit val unsafe: Unsafe = Unsafe.unsafe
    decodedFields.flatMap { fields =>
      schema.construct(Chunk.fromIterable(fields.values)).left.map { error =>
        DecodeError.MalformedFieldWithPath(Chunk.single("Error"), error)
      }
    }
  }

  private def decodePrimitiveValues[A](value: Any, standardTypeSchema: StandardType[A]): Either[DecodeError, A] =
    standardTypeSchema match {
      case StandardType.UnitType =>
        Try(()).toEither.left.map(e => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), e.getMessage))
      case StandardType.StringType =>
        Try(value.asInstanceOf[Utf8].toString).toEither.left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), e.getMessage))
      case StandardType.BoolType =>
        Try(value.asInstanceOf[Boolean]).toEither.left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), e.getMessage))
      case StandardType.ByteType =>
        Try(value.asInstanceOf[Integer]).toEither
          .map(_.toByte)
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), e.getMessage))
      case StandardType.ShortType =>
        Try(value.asInstanceOf[Integer]).toEither
          .map(_.toShort)
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), e.getMessage))
      case StandardType.IntType =>
        Try(value.asInstanceOf[Integer]).toEither.left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), e.getMessage))
          .map(_.asInstanceOf[A])
      case StandardType.LongType =>
        Try(value.asInstanceOf[Long]).toEither.left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), e.getMessage))
      case StandardType.FloatType =>
        Try(value.asInstanceOf[Float]).toEither.left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), e.getMessage))
      case StandardType.DoubleType =>
        Try(value.asInstanceOf[Double]).toEither.left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), e.getMessage))
      case StandardType.BinaryType =>
        Try(value.asInstanceOf[ByteBuffer].array().asInstanceOf[A]).toEither.left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), e.getMessage))
      case StandardType.CharType =>
        Try(value.asInstanceOf[Integer]).toEither
          .map(_.toChar)
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), e.getMessage))
      case StandardType.UUIDType =>
        Try(UUID.fromString(value.asInstanceOf[Utf8].toString).asInstanceOf[A]).toEither.left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.single("Error"), e.getMessage))
      case StandardType.BigDecimalType =>
        val converter = new Conversions.DecimalConversion()
        val schema = AvroSchemaCodec
          .encodeToApacheAvro(Schema.Primitive(StandardType.BigDecimalType, Chunk.empty))
          .getOrElse(throw new Exception("Avro schema could not be generated for BigDecimal."))
        Try(converter.fromBytes(value.asInstanceOf[ByteBuffer], schema, LogicalTypes.decimal(48, 24))).toEither.left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.BigIntegerType =>
        val converter = new Conversions.DecimalConversion()
        val schema = AvroSchemaCodec
          .encodeToApacheAvro(Schema.Primitive(StandardType.BigIntegerType, Chunk.empty))
          .getOrElse(throw new Exception("Avro schema could not be generated for BigInteger."))
        Try(converter.fromBytes(value.asInstanceOf[ByteBuffer], schema, LogicalTypes.decimal(48, 24))).toEither.left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
          .map(_.toBigInteger)
      case StandardType.DayOfWeekType =>
        Try(value.asInstanceOf[Integer])
          .map(java.time.DayOfWeek.of(_))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.MonthType =>
        Try(value.asInstanceOf[Integer])
          .map(java.time.Month.of(_))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.MonthDayType =>
        Try(value.asInstanceOf[Utf8])
          .map(raw => java.time.MonthDay.parse(raw.toString))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.PeriodType =>
        Try(value.asInstanceOf[Utf8])
          .map(raw => java.time.Period.parse(raw.toString))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.YearType =>
        Try(value.asInstanceOf[Integer])
          .map(java.time.Year.of(_))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.YearMonthType =>
        Try(value.asInstanceOf[Utf8])
          .map(raw => java.time.YearMonth.parse(raw.toString))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.ZoneIdType =>
        Try(value.asInstanceOf[Utf8])
          .map(raw => java.time.ZoneId.of(raw.toString))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.ZoneOffsetType =>
        Try(value.asInstanceOf[Integer])
          .map(java.time.ZoneOffset.ofTotalSeconds(_))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.DurationType =>
        Try(value.asInstanceOf[Utf8])
          .map(raw => java.time.Duration.parse(raw.toString))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.InstantType =>
        Try(value.asInstanceOf[Utf8])
          .map(java.time.Instant.parse(_))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.LocalDateType =>
        Try(value.asInstanceOf[Utf8])
          .map(java.time.LocalDate.parse(_))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.LocalTimeType =>
        Try(value.asInstanceOf[Utf8])
          .map(java.time.LocalTime.parse(_))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.LocalDateTimeType =>
        Try(value.asInstanceOf[Utf8])
          .map(java.time.LocalDateTime.parse(_))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.OffsetTimeType =>
        Try(value.asInstanceOf[Utf8])
          .map(java.time.OffsetTime.parse(_))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.OffsetDateTimeType =>
        Try(value.asInstanceOf[Utf8])
          .map(java.time.OffsetDateTime.parse(_))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
      case StandardType.ZonedDateTimeType =>
        Try(value.asInstanceOf[Utf8])
          .map(java.time.ZonedDateTime.parse(_))
          .toEither
          .left
          .map(e => DecodeError.MalformedFieldWithPath(Chunk.empty, e.getMessage))
    }

  private def decodeMap(value: Any, schema: Schema.Map[Any, Any]) = {
    val map = value.asInstanceOf[java.util.Map[Any, Any]]
    val result: List[(Either[DecodeError, Any], Either[DecodeError, Any])] = map.asScala.toList.map {
      case (k, v) => (decodeValue(k, schema.keySchema), decodeValue(v, schema.valueSchema))
    }
    val traversed: Either[List[DecodeError], List[(Any, Any)]] = result.partition {
      case (k, v) => k.isLeft || v.isLeft
    } match {
      case (Nil, decoded) => Right(for ((Right(k), Right(v)) <- decoded) yield (k, v))
      case (errors, _)    => Left(for ((Left(s), _)          <- errors) yield s)
    }

    val combined: Either[DecodeError, List[(Any, Any)]] = traversed.left.map { errors =>
      errors.foldLeft[DecodeError](DecodeError.MalformedFieldWithPath(Chunk.empty, "Map decoding failed."))(
        (acc, error) => acc.and(DecodeError.MalformedFieldWithPath(Chunk.empty, s"${error.message}"))
      )
    }

    combined.map(_.toMap)

  }
  private def decodeSequence[A](a: A, schema: Schema[A]) = {
    val array  = a.asInstanceOf[GenericData.Array[Any]]
    val result = array.asScala.toList.map(decodeValue(_, schema))
    val traversed: Either[List[DecodeError], List[A]] = result.partition(_.isLeft) match {
      case (Nil, decoded) => Right(for (Right(i) <- decoded) yield i)
      case (errors, _)    => Left(for (Left(s)   <- errors) yield s)
    }
    val combined: Either[DecodeError, List[A]] = traversed.left.map { errors =>
      errors.foldLeft[DecodeError](DecodeError.MalformedFieldWithPath(Chunk.empty, "Sequence decoding failed."))(
        (acc, error) => acc.and(DecodeError.MalformedFieldWithPath(Chunk.empty, s"${error.message}"))
      )
    }

    combined.map(Chunk.fromIterable(_))
  }

  private def decodeTuple2[A, B](value: Any, schemaLeft: Schema[A], schemaRight: Schema[B]) = {
    val record  = value.asInstanceOf[GenericRecord]
    val result1 = decodeValue(record.get("_1"), schemaLeft)
    val result2 = decodeValue(record.get("_2"), schemaRight)
    result1.flatMap(a => result2.map(b => (a, b)))
  }

  private def decodeEitherValue[A, B](value: Any, schemaLeft: Schema[A], schemaRight: Schema[B]) = {
    val record = value.asInstanceOf[GenericRecord]
    val v      = record.get("value")
    val result = decodeInWrapper(schemaLeft, v)
    if (result.isRight) result.map(Left(_))
    else decodeInWrapper(schemaRight, v).map(Right(_))
  }

  private def decodeFallbackValue[A, B](value: Any, schema: Schema.Fallback[A, B]) = {
    var error: Option[DecodeError] = None

    val record = value.asInstanceOf[GenericRecord]
    val left: Option[A] = decodeValue(record.get("_1"), Schema.Optional(schema.left)) match {
      case Right(value) => value
      case Left(err) => {
        error = Some(err)
        None
      }
    }

    val right = left match {
      case Some(_) =>
        if (schema.fullDecode) decodeValue(record.get("_2"), Schema.Optional(schema.right)).getOrElse(None)
        else None
      case _ =>
        decodeValue(record.get("_2"), Schema.Optional(schema.right)).getOrElse(None)
    }

    (left, right) match {
      case (Some(a), Some(b)) => Right(Fallback.Both(a, b))
      case (_, Some(b))       => Right(Fallback.Right(b))
      case (Some(a), _)       => Right(Fallback.Left(a))
      case _                  => Left(error.get)
    }
  }

  private def decodeInWrapper[A](schema: Schema[A], value: Any) =
    if (isUnion(schema)) {
      decodeValue(value.asInstanceOf[GenericData.Record].get("value"), schema)
    } else decodeValue(value, schema)

  private def decodeOptionalValue[A](value: Any, schema: Schema[A]) =
    if (value == null) Right(None)
    else decodeInWrapper(schema, value).map(Some(_))

  private def encodeValue[A](a: A, schema: Schema[A]): Any = schema match {
    case Schema.Enum1(_, c1, _)                     => encodeEnum(schema, a, c1)
    case Schema.Enum2(_, c1, c2, _)                 => encodeEnum(schema, a, c1, c2)
    case Schema.Enum3(_, c1, c2, c3, _)             => encodeEnum(schema, a, c1, c2, c3)
    case Schema.Enum4(_, c1, c2, c3, c4, _)         => encodeEnum(schema, a, c1, c2, c3, c4)
    case Schema.Enum5(_, c1, c2, c3, c4, c5, _)     => encodeEnum(schema, a, c1, c2, c3, c4, c5)
    case Schema.Enum6(_, c1, c2, c3, c4, c5, c6, _) => encodeEnum(schema, a, c1, c2, c3, c4, c5, c6)
    case Schema.Enum7(_, c1, c2, c3, c4, c5, c6, c7, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7)
    case Schema.Enum8(_, c1, c2, c3, c4, c5, c6, c7, c8, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8)
    case Schema.Enum9(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8, c9)
    case Schema.Enum10(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
    case Schema.Enum11(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
    case Schema.Enum12(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
    case Schema.Enum13(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
    case Schema.Enum14(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
    case Schema.Enum15(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
    case Schema.Enum16(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
    case Schema.Enum17(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
    case Schema.Enum18(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
    case Schema.Enum19(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
    case Schema
          .Enum20(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _) =>
      encodeEnum(schema, a, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
    case Schema.Enum21(
        _,
        c1,
        c2,
        c3,
        c4,
        c5,
        c6,
        c7,
        c8,
        c9,
        c10,
        c11,
        c12,
        c13,
        c14,
        c15,
        c16,
        c17,
        c18,
        c19,
        c20,
        c21,
        _
        ) =>
      encodeEnum(
        schema,
        a,
        c1,
        c2,
        c3,
        c4,
        c5,
        c6,
        c7,
        c8,
        c9,
        c10,
        c11,
        c12,
        c13,
        c14,
        c15,
        c16,
        c17,
        c18,
        c19,
        c20,
        c21
      )
    case Schema.Enum22(
        _,
        c1,
        c2,
        c3,
        c4,
        c5,
        c6,
        c7,
        c8,
        c9,
        c10,
        c11,
        c12,
        c13,
        c14,
        c15,
        c16,
        c17,
        c18,
        c19,
        c20,
        c21,
        c22,
        _
        ) =>
      encodeEnum(
        schema,
        a,
        c1,
        c2,
        c3,
        c1,
        c2,
        c3,
        c4,
        c5,
        c6,
        c7,
        c8,
        c9,
        c10,
        c11,
        c12,
        c13,
        c14,
        c15,
        c16,
        c17,
        c18,
        c19,
        c20,
        c21,
        c22
      )
    case Schema.GenericRecord(typeId, structure, _) => encodeGenericRecord(a, typeId, structure)
    case Schema.Primitive(standardType, _)          => encodePrimitive(a, standardType)
    case Schema.Sequence(element, _, g, _, _)       => encodeSequence(element, g(a))
    case Schema.Set(element, _)                     => encodeSet(element, a)
    case mapSchema: Schema.Map[_, _] =>
      encodeMap(mapSchema.asInstanceOf[Schema.Map[Any, Any]], a.asInstanceOf[scala.collection.immutable.Map[Any, Any]])
    case Schema.Transform(schema, _, g, _, _) =>
      g(a).map(encodeValue(_, schema)).getOrElse(throw new Exception("Transform failed."))
    case Schema.Optional(schema, _) => encodeOption(schema, a)
    case Schema.Tuple2(left, right, _) =>
      encodeTuple2(left.asInstanceOf[Schema[Any]], right.asInstanceOf[Schema[Any]], a)
    case Schema.Either(left, right, _)   => encodeEither(left, right, a)
    case s @ Schema.Fallback(_, _, _, _) => encodeFallback(s, a)
    case Schema.Lazy(schema0)            => encodeValue(a, schema0())
    case Schema.CaseClass0(_, _, _) =>
      encodeCaseClass(schema, a, Seq.empty: _*) //encodePrimitive((), StandardType.UnitType)
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

    case _ => throw new Exception(s"Unsupported schema $schema")

  }

  private def encodePrimitive[A](a: A, standardType: StandardType[A]): Any =
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

  private def encodeUnionWrapper[A](schema: Schema[A], value: Any): Any =
    if (isUnion(schema)) {
      val s = AvroSchemaCodec
        .encodeToApacheAvro(schema)
        .getOrElse(throw new Exception("Avro schema could not be generated for Optional."))
      val name = AvroSchemaCodec
        .getName(schema)
        .getOrElse(throw new Exception("Avro schema could not be generated for Optional."))
      val record = new GenericRecordBuilder(
        AvroSchemaCodec.wrapAvro(s, name, AvroPropMarker.UnionWrapper)
      )
      record.set("value", value)
      record.build()
    } else value

  private def encodeOption[A](schema: Schema[A], v: Option[A]): Any =
    v.map { value =>
      val a = encodeValue(value, schema)

      // if `schema` is converted to an Avro Union, then it is wrapped.
      encodeUnionWrapper(schema, a)
    }.orNull

  private def encodeEither[A, B](left: Schema[A], right: Schema[B], either: scala.util.Either[A, B]): Any = {
    val schema = AvroSchemaCodec
      .encodeToApacheAvro(Schema.Either(left, right, Chunk.empty))
      .getOrElse(throw new Exception("Avro schema could not be generated for Either."))

    val record = new GenericRecordBuilder(schema)
    val result = either match {
      case Left(a)  => record.set("value", encodeUnionWrapper(left, encodeValue(a, left)))
      case Right(b) => record.set("value", encodeUnionWrapper(right, encodeValue(b, right)))
    }

    result.build()
  }

  private def encodeFallback[A, B](s: Schema.Fallback[A, B], f: zio.schema.Fallback[A, B]): Any = {
    val schema = AvroSchemaCodec
      .encodeToApacheAvro(s)
      .getOrElse(throw new Exception("Avro schema could not be generated for Fallback."))

    val value: (Option[A], Option[B]) = f match {
      case zio.schema.Fallback.Left(a)    => (Some(a), None)
      case zio.schema.Fallback.Right(b)   => (None, Some(b))
      case zio.schema.Fallback.Both(a, b) => (Some(a), Some(b))
    }

    val left  = encodeOption[A](s.left, value._1)
    val right = encodeOption[B](s.right, value._2)

    val record = new GenericData.Record(schema)
    record.put("_1", left)
    record.put("_2", right)
    record
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

  private def encodeEnum[Z](schemaRaw: Schema[Z], value: Z, cases: Schema.Case[Z, _]*): Any = {
    val schema = AvroSchemaCodec
      .encodeToApacheAvro(schemaRaw)
      .getOrElse(throw new Exception("Avro schema could not be generated for Enum."))
    val fieldIndex = cases.indexWhere(c => c.deconstructOption(value).isDefined)
    if (fieldIndex >= 0) {
      val subtypeCase = cases(fieldIndex)
      if (schema.getType == SchemaAvro.Type.ENUM) {
        GenericData.get.createEnum(schema.getEnumSymbols.get(fieldIndex), schema)
      } else {

        // must check if it's wrapped
        val caseSchema = subtypeCase.schema.asInstanceOf[Schema[Any]]
        val v          = encodeValue(subtypeCase.deconstruct(value), caseSchema)
        encodeUnionWrapper(caseSchema, v)

      }
    } else {
      throw new Exception("Could not find matching case for enum value.")
    }
  }

  /**
   * Returns `true` if the corresponding AvroSchema is an union.
   */
  private def isUnion[A](schema: Schema[A]): Boolean =
    schema match {
      case _: Schema.Optional[_] => true
      case enu: Schema.Enum[_] => {
        val avroEnumAnnotationExists = AvroSchemaCodec.hasAvroEnumAnnotation(enu.annotations)
        val isAvroEnumEquivalent = enu.cases.map(_.schema).forall {
          case (Schema.Transform(Schema.Primitive(standardType, _), _, _, _, _))
              if standardType == StandardType.UnitType && avroEnumAnnotationExists =>
            true
          case (Schema.Primitive(standardType, _)) if standardType == StandardType.StringType => true
          case (Schema.CaseClass0(_, _, _)) if avroEnumAnnotationExists                       => true
          case _                                                                              => false
        }
        !isAvroEnumEquivalent
      }
      case _ => false
    }

}
