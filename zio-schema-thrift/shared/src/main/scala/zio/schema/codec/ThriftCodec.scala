package zio.schema.codec

import java.math.{ BigInteger, MathContext }
import java.nio.ByteBuffer
import java.time._
import java.util.UUID
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }
import org.apache.thrift.protocol._
import zio.schema._
import zio.schema.annotation.optionalField
import zio.schema.codec.BinaryCodec._
import zio.schema.codec.ThriftCodec.Thrift.{
  bigDecimalStructure,
  durationStructure,
  monthDayStructure,
  periodStructure,
  yearMonthStructure
}
import zio.stream.ZPipeline
import zio.{ Chunk, ChunkBuilder, ZIO }

object ThriftCodec extends BinaryCodec {

  override def encoderFor[A](schema: Schema[A]): BinaryEncoder[A] =
    new BinaryEncoder[A] {

      override def encode(value: A): Chunk[Byte] =
        new ThriftEncoder().encode(schema, value)

      override def streamEncoder: BinaryStreamEncoder[A] = {
        val encoder = new ThriftEncoder()
        ZPipeline.mapChunks { chunk =>
          chunk.flatMap(encoder.encode(schema, _))
        }
      }

    }

  override def decoderFor[A](schema: Schema[A]): BinaryDecoder[A] =
    new BinaryDecoder[A] {

      override def decode(chunk: Chunk[Byte]): Either[String, A] =
        if (chunk.isEmpty)
          Left("No bytes to decode")
        else
          decodeChunk(chunk)

      override def streamDecoder: BinaryStreamDecoder[A] =
        ZPipeline.mapChunksZIO { chunk =>
          ZIO.fromEither(
            decodeChunk(chunk).map(Chunk(_))
          )
        }

      private def decodeChunk(chunk: Chunk[Byte]) =
        new ThriftDecoder(chunk)
          .decode(Chunk.empty, schema)
          .left
          .map(
            err => s"Error at path /${err.path.mkString(".")}: ${err.error}"
          )

    }

  object Thrift {

    val bigDecimalStructure: Seq[Schema.Field[java.math.BigDecimal, _]] =
      Seq(
        Schema.Field(
          "unscaled",
          Schema.Primitive(StandardType.BigIntegerType),
          get = _.unscaledValue(),
          set = (a, b: BigInteger) => new java.math.BigDecimal(b, a.scale)
        ),
        Schema.Field(
          "precision",
          Schema.Primitive(StandardType.IntType),
          get = _.precision(),
          set = (a, b: Int) => new java.math.BigDecimal(a.unscaledValue, new MathContext(b))
        ),
        Schema
          .Field("scale", Schema.Primitive(StandardType.IntType), get = _.scale(), set = (a, b: Int) => a.setScale(b))
      )

    val monthDayStructure: Seq[Schema.Field[MonthDay, Int]] =
      Seq(
        Schema.Field(
          "month",
          Schema.Primitive(StandardType.IntType),
          get = (v: MonthDay) => v.getMonthValue,
          set = (a, b: Int) => a.`with`(Month.of(b))
        ),
        Schema
          .Field(
            "day",
            Schema.Primitive(StandardType.IntType),
            get = _.getDayOfMonth,
            set = (a, b: Int) => a.withDayOfMonth(b)
          )
      )

    val periodStructure: Seq[Schema.Field[Period, Int]] = Seq(
      Schema
        .Field("years", Schema.Primitive(StandardType.IntType), get = _.getYears, set = (a, b: Int) => a.withYears(b)),
      Schema.Field(
        "months",
        Schema.Primitive(StandardType.IntType),
        get = _.getMonths,
        set = (a, b: Int) => a.withMonths(b)
      ),
      Schema.Field("days", Schema.Primitive(StandardType.IntType), get = _.getDays, set = (a, b: Int) => a.withDays(b))
    )

    val yearMonthStructure: Seq[Schema.Field[YearMonth, Int]] =
      Seq(
        Schema.Field(
          "year",
          Schema.Primitive(StandardType.IntType),
          get = _.getYear,
          set = (a, b: Int) => a.`with`(Year.of(b))
        ),
        Schema.Field(
          "month",
          Schema.Primitive(StandardType.IntType),
          get = _.getMonthValue,
          set = (a, b: Int) => a.`with`(Month.of(b))
        )
      )

    val durationStructure: Seq[Schema.Field[Duration, _]] =
      Seq(
        Schema.Field(
          "seconds",
          Schema.Primitive(StandardType.LongType),
          get = _.getSeconds,
          set = (a, b: Long) => a.plusSeconds(b)
        ),
        Schema
          .Field(
            "nanos",
            Schema.Primitive(StandardType.IntType),
            get = _.getNano,
            set = (a, b: Int) => a.plusNanos(b.toLong)
          )
      )
  }

  final class ThriftEncoder {

    val write = new ChunkTransport.Write()
    val p     = new TBinaryProtocol(write)

    def encode[A](schema: Schema[A], value: A): Chunk[Byte] = {
      encodeValue(None, schema, value)
      write.chunk
    }

    @tailrec
    def getType[A](schema: Schema[A]): Byte = schema match {
      case _: Schema.Record[A]                  => TType.STRUCT
      case Schema.Sequence(_, _, _, _, _)       => TType.LIST
      case Schema.Map(_, _, _)                  => TType.MAP
      case Schema.Set(_, _)                     => TType.SET
      case Schema.Transform(schema, _, _, _, _) => getType(schema)
      case Schema.Primitive(standardType, _)    => getPrimitiveType(standardType)
      case Schema.Tuple2(_, _, _)               => TType.STRUCT
      case Schema.Optional(schema, _)           => getType(schema)
      case Schema.Either(_, _, _)               => TType.STRUCT
      case Schema.Lazy(lzy)                     => getType(lzy())
      case _: Schema.Enum[A]                    => TType.STRUCT
      case _                                    => TType.VOID
    }

    def getPrimitiveType[A](standardType: StandardType[A]): Byte =
      standardType match {
        case StandardType.UnitType => TType.VOID
        case StandardType.StringType =>
          TType.STRING
        case StandardType.BoolType =>
          TType.BOOL
        case StandardType.ShortType =>
          TType.I16
        case StandardType.IntType =>
          TType.I32
        case StandardType.LongType =>
          TType.I64
        case StandardType.FloatType =>
          TType.DOUBLE
        case StandardType.DoubleType =>
          TType.DOUBLE
        case StandardType.BigIntegerType =>
          TType.STRING
        case StandardType.BigDecimalType =>
          TType.STRUCT
        case StandardType.BinaryType =>
          TType.STRING
        case StandardType.CharType =>
          TType.STRING
        case StandardType.UUIDType =>
          TType.STRING
        case StandardType.DayOfWeekType =>
          TType.BYTE
        case StandardType.MonthType =>
          TType.BYTE
        case StandardType.MonthDayType          => TType.STRUCT
        case StandardType.PeriodType            => TType.STRUCT
        case StandardType.YearType              => TType.I32
        case StandardType.YearMonthType         => TType.STRUCT
        case StandardType.ZoneIdType            => TType.STRING
        case StandardType.ZoneOffsetType        => TType.I32
        case StandardType.DurationType          => TType.STRUCT
        case StandardType.InstantType(_)        => TType.STRING
        case StandardType.LocalDateType(_)      => TType.STRING
        case StandardType.LocalTimeType(_)      => TType.STRING
        case StandardType.LocalDateTimeType(_)  => TType.STRING
        case StandardType.OffsetTimeType(_)     => TType.STRING
        case StandardType.OffsetDateTimeType(_) => TType.STRING
        case StandardType.ZonedDateTimeType(_)  => TType.STRING
        case _                                  => TType.VOID
      }

    def writePrimitiveType[A](standardType: StandardType[A], value: A): Unit =
      (standardType, value) match {
        case (StandardType.UnitType, _) => ()
        case (StandardType.StringType, str: String) =>
          p.writeString(str)
        case (StandardType.BoolType, b: Boolean) =>
          p.writeBool(b)
        case (StandardType.ByteType, v: Byte) =>
          p.writeByte(v)
        case (StandardType.ShortType, v: Short) =>
          p.writeI16(v)
        case (StandardType.IntType, v: Int) =>
          p.writeI32(v)
        case (StandardType.LongType, v: Long) =>
          p.writeI64(v)
        case (StandardType.FloatType, v: Float) =>
          p.writeDouble(v.toDouble)
        case (StandardType.DoubleType, v: Double) =>
          p.writeDouble(v.toDouble)
        case (StandardType.BigIntegerType, v: java.math.BigInteger) =>
          p.writeBinary(ByteBuffer.wrap(v.toByteArray))
        case (StandardType.BigDecimalType, v: java.math.BigDecimal) =>
          val unscaled  = v.unscaledValue()
          val precision = v.precision()
          val scale     = v.scale()
          encodeRecord(
            None,
            bigDecimalStructure,
            ListMap("unscaled" -> unscaled, "precision" -> precision, "scale" -> scale)
          )
        case (StandardType.BinaryType, bytes: Chunk[Byte]) =>
          p.writeBinary(ByteBuffer.wrap(bytes.toArray))
        case (StandardType.CharType, c: Char) =>
          p.writeString(c.toString)
        case (StandardType.UUIDType, u: UUID) =>
          p.writeString(u.toString)
        case (StandardType.DayOfWeekType, v: DayOfWeek) =>
          p.writeByte(v.getValue.toByte)
        case (StandardType.MonthType, v: Month) =>
          p.writeByte(v.getValue.toByte)
        case (StandardType.MonthDayType, v: MonthDay) =>
          encodeRecord(None, monthDayStructure, ListMap("month" -> v.getMonthValue, "day" -> v.getDayOfMonth))
        case (StandardType.PeriodType, v: Period) =>
          encodeRecord(
            None,
            periodStructure,
            ListMap("years" -> v.getYears, "months" -> v.getMonths, "days" -> v.getDays)
          )
        case (StandardType.YearType, v: Year) =>
          p.writeI32(v.getValue)
        case (StandardType.YearMonthType, v: YearMonth) =>
          encodeRecord(None, yearMonthStructure, ListMap("year" -> v.getYear, "month" -> v.getMonthValue))
        case (StandardType.ZoneIdType, v: ZoneId) =>
          p.writeString(v.getId)
        case (StandardType.ZoneOffsetType, v: ZoneOffset) =>
          p.writeI32(v.getTotalSeconds)
        case (StandardType.DurationType, v: Duration) =>
          encodeRecord(None, durationStructure, ListMap("seconds" -> v.getSeconds, "nanos" -> v.getNano))
        case (StandardType.InstantType(formatter), v: Instant) =>
          p.writeString(formatter.format(v))
        case (StandardType.LocalDateType(formatter), v: LocalDate) =>
          p.writeString(formatter.format(v))
        case (StandardType.LocalTimeType(formatter), v: LocalTime) =>
          p.writeString(formatter.format(v))
        case (StandardType.LocalDateTimeType(formatter), v: LocalDateTime) =>
          p.writeString(formatter.format(v))
        case (StandardType.OffsetTimeType(formatter), v: OffsetTime) =>
          p.writeString(formatter.format(v))
        case (StandardType.OffsetDateTimeType(formatter), v: OffsetDateTime) =>
          p.writeString(formatter.format(v))
        case (StandardType.ZonedDateTimeType(formatter), v: ZonedDateTime) =>
          p.writeString(formatter.format(v))
        case (_, _) =>
          throw new NotImplementedError(s"No encoder for $standardType")
      }

    def writeFieldBegin(fieldNumber: Option[Short], ttype: Byte): Unit =
      fieldNumber.foreach(
        num =>
          p.writeFieldBegin(
            new TField("", ttype, num)
          )
      )

    def encodePrimitive[A](fieldNumber: Option[Short], standardType: StandardType[A], value: A): Unit = {
      writeFieldBegin(fieldNumber, getPrimitiveType(standardType))
      writePrimitiveType(standardType, value)
    }

    def encodeSequence[A](fieldNumber: Option[Short], schema: Schema[A], v: Chunk[A]): Unit = {
      writeFieldBegin(fieldNumber, TType.LIST)
      p.writeListBegin(new org.apache.thrift.protocol.TList(getType(schema), v.size))
      v.foreach(encodeValue(None, schema, _))
    }

    def encodeMap[K, V](fieldNumber: Option[Short], schema: Schema.Map[K, V], v: Map[K, V]): Unit = {
      writeFieldBegin(fieldNumber, TType.MAP)
      p.writeMapBegin(
        new org.apache.thrift.protocol.TMap(getType(schema.keySchema), getType(schema.valueSchema), v.size)
      )
      v.foreach {
        case (k, v) =>
          encodeValue(None, schema.keySchema, k)
          encodeValue(None, schema.valueSchema, v)
      }
    }

    def encodeSet[A](fieldNumber: Option[Short], schema: Schema[A], v: scala.collection.immutable.Set[A]): Unit = {
      writeFieldBegin(fieldNumber, TType.SET)
      p.writeSetBegin(new org.apache.thrift.protocol.TSet(getType(schema), v.size))
      v.foreach(encodeValue(None, schema, _))
    }

    def encodeOptional[A](fieldNumber: Option[Short], schema: Schema[A], v: Option[A]): Unit = {
      writeFieldBegin(fieldNumber, TType.STRUCT)
      v match {
        case None =>
          encodeValue(Some(1), Schema.primitive(StandardType.UnitType), ())
        case Some(value) =>
          encodeValue(Some(2), schema, value)
      }
      p.writeFieldStop()
    }

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    def encodeValue[A](fieldNumber: Option[Short], schema: Schema[A], value: A): Unit =
      (schema, value) match {
        case (Schema.GenericRecord(_, structure, _), v: Map[String, _])      => encodeRecord(fieldNumber, structure.toChunk, v)
        case (Schema.Sequence(element, _, g, _, _), v)                       => encodeSequence(fieldNumber, element, g(v))
        case (mapSchema: Schema.Map[_, _], map: Map[_, _])                   => encodeMap(fieldNumber, mapSchema.asInstanceOf[Schema.Map[Any, Any]], map.asInstanceOf[scala.collection.immutable.Map[Any, Any]])
        case (setSchema: Schema.Set[_], set: Set[_])                         => encodeSet(fieldNumber, setSchema.asInstanceOf[Schema.Set[Any]].elementSchema, set.asInstanceOf[scala.collection.immutable.Set[Any]])
        case (Schema.Transform(schema, _, g, _, _), _)                       => g(value).foreach(encodeValue(fieldNumber, schema, _))
        case (Schema.Primitive(standardType, _), v)                          => encodePrimitive(fieldNumber, standardType, v)
        case (Schema.Tuple2(left, right, _), v @ (_, _))                     => encodeTuple(fieldNumber, left, right, v)
        case (optSchema: Schema.Optional[_], v: Option[_])                   => encodeOptional(fieldNumber, optSchema.asInstanceOf[Schema.Optional[Any]].schema, v.asInstanceOf[Option[Any]])
        case (eitherSchema: Schema.Either[_, _], v: scala.util.Either[_, _]) => encodeEither(fieldNumber, eitherSchema.asInstanceOf[Schema.Either[Any, Any]].left, eitherSchema.asInstanceOf[Schema.Either[Any, Any]].right, v.asInstanceOf[scala.util.Either[Any, Any]])
        case (lzy @ Schema.Lazy(_), v)                                       => encodeValue(fieldNumber, lzy.schema, v)
        //  case (Schema.Meta(ast, _), _)                                        => encodeValue(fieldNumber, Schema[MetaSchema], ast)
        case ProductEncoder(encode) =>
          writeFieldBegin(fieldNumber, TType.STRUCT)
          encode()
        case (Schema.Enum1(_, c, _), v)                          => encodeEnum(fieldNumber, v, c)
        case (Schema.Enum2(_, c1, c2, _), v)                     => encodeEnum(fieldNumber, v, c1, c2)
        case (Schema.Enum3(_, c1, c2, c3, _), v)                 => encodeEnum(fieldNumber, v, c1, c2, c3)
        case (Schema.Enum4(_, c1, c2, c3, c4, _), v)             => encodeEnum(fieldNumber, v, c1, c2, c3, c4)
        case (Schema.Enum5(_, c1, c2, c3, c4, c5, _), v)         => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5)
        case (Schema.Enum6(_, c1, c2, c3, c4, c5, c6, _), v)     => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6)
        case (Schema.Enum7(_, c1, c2, c3, c4, c5, c6, c7, _), v) => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7)
        case (Schema.Enum8(_, c1, c2, c3, c4, c5, c6, c7, c8, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8)
        case (Schema.Enum9(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9)
        case (Schema.Enum10(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
        case (Schema.Enum11(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
        case (Schema.Enum12(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
        case (Schema.Enum13(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
        case (Schema.Enum14(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
        case (Schema.Enum15(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
        case (Schema.Enum16(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
        case (Schema.Enum17(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
        case (Schema.Enum18(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
        case (Schema.Enum19(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
        case (Schema.Enum20(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
        case (Schema.Enum21(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
        case (Schema.Enum22(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
        case (Schema.EnumN(_, cs, _), v) => encodeEnum(fieldNumber, v, cs.toSeq: _*)
        case (Schema.Dynamic(_), v)      => encodeDynamic(fieldNumber, DynamicValueSchema.schema, v)
        case (_, _)                      => ()
      }

    private def encodeDynamic(fieldNumber: Option[Short], schema: Schema[DynamicValue], v: DynamicValue): Unit =
      encodeValue(fieldNumber, schema, v)

    private def encodeEnum[Z](fieldNumber: Option[Short], value: Z, cases: Schema.Case[Z, _]*): Unit = {
      writeFieldBegin(fieldNumber, TType.STRUCT)
      val fieldIndex = cases.indexWhere(c => c.deconstructOption(value).isDefined)
      if (fieldIndex >= 0) {
        val subtypeCase = cases(fieldIndex)
        encodeValue(Some((fieldIndex + 1).shortValue), subtypeCase.schema.asInstanceOf[Schema[Any]], subtypeCase.deconstruct(value))
      }
      p.writeFieldStop()
    }

    private def encodeEither[A, B](fieldNumber: Option[Short], left: Schema[A], right: Schema[B], either: scala.util.Either[A, B]): Unit = {
      writeFieldBegin(fieldNumber, TType.STRUCT)
      either match {
        case Left(value)  => encodeValue(Some(1), left, value)
        case Right(value) => encodeValue(Some(2), right, value)
      }
    }

    def tupleSchema[A, B](first: Schema[A], second: Schema[B]): Seq[Schema.Field[(A, B), _]] =
      Seq(Schema.Field("first", first, get = _._1, set = (ab: (A, B), a: A) => (a, ab._2)), Schema.Field("second", second, get = _._2, set = (a: (A, B), b: B) => (a._1, b)))

    private def encodeTuple[A, B](fieldNumber: Option[Short], left: Schema[A], right: Schema[B], tuple: (A, B)): Unit =
      encodeRecord(fieldNumber, tupleSchema(left, right), ListMap[String, Any]("first" -> tuple._1, "second" -> tuple._2))

    private def writeStructure(fields: Seq[(Schema.Field[_, _], Any)]): Unit = {
      fields.zipWithIndex.foreach {
        case ((fieldSchema: Schema.Field[_, Any], value), fieldNumber) =>
          encodeValue(Some((fieldNumber + 1).shortValue), fieldSchema.schema, value)
      }
      p.writeFieldStop()
    }

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[codec] object ProductEncoder {

      def unapply[A](schemaAndValue: (Schema[A], A)): Option[() => Unit] = schemaAndValue match {
        case (Schema.CaseClass1(_, f, _, _), v)      => Some(encodeCaseClass(v, f))
        case (Schema.CaseClass2(_, f1, f2, _, _), v) => Some(encodeCaseClass(v, f1, f2))
        case (Schema.CaseClass3(_, f1, f2, f3, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3))
        case (Schema.CaseClass4(_, f1, f2, f3, f4, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4))
        case (Schema.CaseClass5(_, f1, f2, f3, f4, f5, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5))
        case (Schema.CaseClass6(_, f1, f2, f3, f4, f5, f6, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6))
        case (Schema.CaseClass7(_, f1, f2, f3, f4, f5, f6, f7, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7))
        case (Schema.CaseClass8(_, f1, f2, f3, f4, f5, f6, f7, f8, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8))
        case (Schema.CaseClass9(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9))
        case (Schema.CaseClass10(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10))
        case (Schema.CaseClass11(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11))
        case (Schema.CaseClass12(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12))
        case (Schema.CaseClass13(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13))
        case (Schema.CaseClass14(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14))
        case (Schema.CaseClass15(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15))
        case (Schema.CaseClass16(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16))
        case (Schema.CaseClass17(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17))
        case (Schema.CaseClass18(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18))
        case (Schema.CaseClass19(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19))
        case (Schema.CaseClass20(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20))
        case (Schema.CaseClass21(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21))
        case (Schema.CaseClass22(_, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, _, _), v) =>
          Some(encodeCaseClass(v, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22))
        case _ => None
      }

      private def encodeCaseClass[Z](value: Z, fields: (Schema.Field[Z, _])*): () => Unit = () => writeStructure(fields.map { case field => (field, field.get(value)) })

      object OptionalSchema {

        def unapply(schema: Schema[Any]): Option[Schema[Any]] =
          if (schema.isInstanceOf[Schema.Optional[_]])
            Some(schema.asInstanceOf[Schema.Optional[Any]].schema)
          else
            None
      }

    }

    def encodeRecord(fieldNumber: Option[Short], structure: Seq[Schema.Field[_, _]], data: ListMap[String, _]): Unit = {
      writeFieldBegin(fieldNumber, TType.STRUCT)
      writeStructure(structure.map(schema => (schema, data(schema.name))))
    }
  }

  final class ThriftDecoder(chunk: Chunk[Byte]) {

    type Path = Chunk[String]
    case class Error(path: Path, error: String)

    type Result[A]          = scala.util.Either[Error, A]
    type PrimitiveResult[A] = Path => Result[A]

    val read = new ChunkTransport.Read(chunk)
    val p    = new TBinaryProtocol(read)

    def succeed[A](a: => A): Result[A] = Right(a)

    def fail(path: Path, failure: String): Result[Nothing] = Left(Error(path, failure))

    def decodePrimitive[A](f: TProtocol => A, name: String): PrimitiveResult[A] =
      path =>
        Try {
          f(p)
        }.toEither.left.map(_ => Error(path, s"Unable to decode $name"))

    def decodeString: PrimitiveResult[String] =
      decodePrimitive(_.readString(), "String")

    def decodeByte: PrimitiveResult[Byte] =
      decodePrimitive(_.readByte(), "Byte")

    def decodeBoolean: PrimitiveResult[Boolean] =
      decodePrimitive(_.readBool(), "Boolean")

    def decodeShort: PrimitiveResult[Short] =
      decodePrimitive(_.readI16(), "Short")

    def decodeInt: PrimitiveResult[Int] =
      decodePrimitive(_.readI32(), "Int")

    def decodeLong: PrimitiveResult[Long] =
      decodePrimitive(_.readI64(), "Long")

    def decodeFloat: PrimitiveResult[Float] =
      decodePrimitive(_.readDouble().toFloat, "Float")

    def decodeDouble: PrimitiveResult[Double] =
      decodePrimitive(_.readDouble(), "Double")

    def decodeBigInteger: PrimitiveResult[java.math.BigInteger] =
      decodePrimitive(p => new java.math.BigInteger(p.readBinary().array()), "BigInteger")

    def decodeBinary: PrimitiveResult[Chunk[Byte]] =
      decodePrimitive(p => Chunk.fromByteBuffer(p.readBinary()), "Binary")

    def decode[A](path: Path, schema: Schema[A]): Result[A] =
      schema match {
        case Schema.GenericRecord(_, structure, _) => {
          val fields = structure.toChunk
          decodeRecord(path, fields).map(_.map { case (index, value) => (fields(index - 1).name, value) })
        }
        case seqSchema @ Schema.Sequence(_, _, _, _, _) => decodeSequence(path, seqSchema)
        case mapSchema @ Schema.Map(_, _, _)            => decodeMap(path, mapSchema)
        case setSchema @ Schema.Set(_, _)               => decodeSet(path, setSchema)
        case Schema.Transform(schema, f, _, _, _)       => transformDecoder(path, schema, f)
        case Schema.Primitive(standardType, _)          => primitiveDecoder(path, standardType)
        case Schema.Tuple2(left, right, _)              => tupleDecoder(path, left, right)
        case optionalSchema @ Schema.Optional(_, _)     => optionalDecoder(path, optionalSchema)
        case Schema.Fail(message, _)                    => fail(path, message)
        case Schema.Either(left, right, _)              => eitherDecoder(path, left, right)
        case lzy @ Schema.Lazy(_)                       => decode(path, lzy.schema)
        //case Schema.Meta(_, _)                                                                                                        => decode(path, Schema[MetaSchema]).map(_.toSchema)
        case ProductDecoder(decoder)                                                                                                  => decoder(path)
        case Schema.Enum1(_, c, _)                                                                                                    => enumDecoder(path, c)
        case Schema.Enum2(_, c1, c2, _)                                                                                               => enumDecoder(path, c1, c2)
        case Schema.Enum3(_, c1, c2, c3, _)                                                                                           => enumDecoder(path, c1, c2, c3)
        case Schema.Enum4(_, c1, c2, c3, c4, _)                                                                                       => enumDecoder(path, c1, c2, c3, c4)
        case Schema.Enum5(_, c1, c2, c3, c4, c5, _)                                                                                   => enumDecoder(path, c1, c2, c3, c4, c5)
        case Schema.Enum6(_, c1, c2, c3, c4, c5, c6, _)                                                                               => enumDecoder(path, c1, c2, c3, c4, c5, c6)
        case Schema.Enum7(_, c1, c2, c3, c4, c5, c6, c7, _)                                                                           => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7)
        case Schema.Enum8(_, c1, c2, c3, c4, c5, c6, c7, c8, _)                                                                       => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8)
        case Schema.Enum9(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, _)                                                                   => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9)
        case Schema.Enum10(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _)                                                             => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
        case Schema.Enum11(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _)                                                        => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
        case Schema.Enum12(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _)                                                   => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
        case Schema.Enum13(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _)                                              => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
        case Schema.Enum14(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _)                                         => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
        case Schema.Enum15(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _)                                    => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
        case Schema.Enum16(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _)                               => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
        case Schema.Enum17(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _)                          => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
        case Schema.Enum18(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _)                     => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
        case Schema.Enum19(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _)                => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
        case Schema.Enum20(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _)           => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
        case Schema.Enum21(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _)      => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
        case Schema.Enum22(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _) => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
        case Schema.EnumN(_, cs, _)                                                                                                   => enumDecoder(path, cs.toSeq: _*)
        case Schema.Dynamic(_)                                                                                                        => dynamicDecoder(path, DynamicValueSchema.schema)
        case _                                                                                                                        => fail(path, s"Unknown schema ${schema.getClass.getName}")
      }

    private def dynamicDecoder(path: Path, schema: Schema[DynamicValue]): Result[DynamicValue] =
      decode(path, schema)

    private def optionalDecoder[A](path: Path, schema: Schema.Optional[A]): Result[Option[A]] =
      Try {
        val readField = p.readFieldBegin()
        val res = readField.id match {
          case 1 => succeed(None)
          case 2 => decode(path :+ "Some", schema.schema).map(Some(_))
          case _ =>
            fail(path, s"Error decoding optional, wrong field id ${readField.id}")
        }
        p.readFieldBegin()
        res.asInstanceOf[Result[Option[A]]]
      }.fold(err => fail(path, s"Error decoding optional ${err.getMessage}"), identity)

    private def enumDecoder[Z, A](path: Path, cases: Schema.Case[Z, _]*): Result[Z] =
      Try {
        val readField = p.readFieldBegin()
        if (readField.id > cases.length)
          fail(path, s"Error decoding enum with cases ${cases.map(_.id).mkString(", ")}, enum id out of range: ${readField.id}")
        else {
          val subtypeCase = cases(readField.id - 1)
          val res         = decode(path :+ s"[case:${subtypeCase.id}]", subtypeCase.schema)
          res.foreach { _ =>
            p.readFieldBegin()
          }
          res.asInstanceOf[Result[Z]]
        }
      }.fold(err => fail(path, s"Error decoding enum with cases ${cases.map(_.id).mkString(", ")}: ${err.getMessage}"), identity)

    private def eitherDecoder[A, B](path: Path, left: Schema[A], right: Schema[B]): Result[scala.util.Either[A, B]] = {
      val readField = p.readFieldBegin()
      readField.id match {
        case 1 => decode(path :+ "either:left", left).map(Left(_))
        case 2 => decode(path :+ "either:right", right).map(Right(_))
        case _ => fail(path, "Failed to decode either.")
      }
    }

    private def tupleDecoder[A, B](path: Path, left: Schema[A], right: Schema[B]): Result[(A, B)] =
      structDecoder(Seq(left, right), path)
        .flatMap(
          record =>
            (record.get(1), record.get(2)) match {
              case (Some(first), Some(second)) => Right((first.asInstanceOf[A], second.asInstanceOf[B]))
              case _                           => fail(path, "Error while decoding tuple.")
            }
        )

    private def transformDecoder[A, B](path: Path, schema: Schema[B], f: B => scala.util.Either[String, A]): Result[A] =
      decode(path, schema).flatMap(a => f(a).left.map(msg => Error(path, msg)))

    private def primitiveDecoder[A](path: Path, standardType: StandardType[A]): Result[A] =
      standardType match {
        case StandardType.UnitType       => Right(())
        case StandardType.StringType     => decodeString(path)
        case StandardType.BoolType       => decodeBoolean(path)
        case StandardType.ByteType       => decodeByte(path)
        case StandardType.ShortType      => decodeShort(path)
        case StandardType.IntType        => decodeInt(path)
        case StandardType.LongType       => decodeLong(path)
        case StandardType.FloatType      => decodeFloat(path)
        case StandardType.DoubleType     => decodeDouble(path)
        case StandardType.BigIntegerType => decodeBigInteger(path)
        case StandardType.BigDecimalType =>
          decodeRecord(path, bigDecimalStructure).flatMap { data =>
            val opt = for {
              unscaled  <- data.get(1).asInstanceOf[Option[java.math.BigInteger]]
              precision <- data.get(2).asInstanceOf[Option[Int]]
              scale     <- data.get(3).asInstanceOf[Option[Int]]
              ctx       = new java.math.MathContext(precision)
            } yield new java.math.BigDecimal(unscaled, scale, ctx)

            opt match {
              case Some(value) => Right(value)
              case None        => fail(path, s"Invalid big decimal record $data")
            }
          }
        case StandardType.BinaryType => decodeBinary(path)
        case StandardType.CharType =>
          decodeString(path).flatMap(
            decoded =>
              if (decoded.size == 1)
                succeed(decoded.charAt(0))
              else {
                fail(path, s"""Expected character, found string "$decoded"""")
              }
          )
        case StandardType.UUIDType =>
          decodeString(path).flatMap { uuid =>
            try succeed(UUID.fromString(uuid))
            catch {
              case NonFatal(_) => fail(path, "Invalid UUID string")
            }
          }
        case StandardType.DayOfWeekType =>
          decodeByte(path).map(_.toInt).map(DayOfWeek.of)
        case StandardType.MonthType =>
          decodeByte(path).map(_.toInt).map(Month.of)
        case StandardType.MonthDayType =>
          decodeRecord(path, monthDayStructure)
            .map(data => MonthDay.of(data.getOrElse(1, 0).asInstanceOf[Int], data.getOrElse(2, 0).asInstanceOf[Int]))
        case StandardType.PeriodType =>
          decodeRecord(path, periodStructure)
            .map(data => Period.of(data.getOrElse(1, 0).asInstanceOf[Int], data.getOrElse(2, 0).asInstanceOf[Int], data.getOrElse(3, 0).asInstanceOf[Int]))
        case StandardType.YearType =>
          decodeInt(path).map(_.intValue).map(Year.of)
        case StandardType.YearMonthType =>
          decodeRecord(path, yearMonthStructure)
            .map(data => YearMonth.of(data.getOrElse(1, 0).asInstanceOf[Int], data.getOrElse(2, 0).asInstanceOf[Int]))
        case StandardType.ZoneIdType => decodeString(path).map(ZoneId.of)
        case StandardType.ZoneOffsetType =>
          decodeInt(path)
            .map(_.intValue)
            .map(ZoneOffset.ofTotalSeconds)
        case StandardType.DurationType =>
          decodeRecord(path, durationStructure)
            .map(data => Duration.ofSeconds(data.getOrElse(1, 0L).asInstanceOf[Long], data.getOrElse(2, 0L).asInstanceOf[Int].toLong))
        case StandardType.InstantType(formatter) =>
          decodeString(path).map(v => Instant.from(formatter.parse(v)))
        case StandardType.LocalDateType(formatter) =>
          decodeString(path).map(LocalDate.parse(_, formatter))
        case StandardType.LocalTimeType(formatter) =>
          decodeString(path).map(LocalTime.parse(_, formatter))
        case StandardType.LocalDateTimeType(formatter) =>
          decodeString(path).map(LocalDateTime.parse(_, formatter))
        case StandardType.OffsetTimeType(formatter) =>
          decodeString(path).map(OffsetTime.parse(_, formatter))
        case StandardType.OffsetDateTimeType(formatter) =>
          decodeString(path).map(OffsetDateTime.parse(_, formatter))
        case StandardType.ZonedDateTimeType(formatter) =>
          decodeString(path).map(ZonedDateTime.parse(_, formatter))
        case _ => fail(path, s"Unsupported primitive type $standardType")
      }

    private def emptyValue[A](schema: Schema[A]): Option[A] = schema match {
      case Schema.Lazy(s)                             => emptyValue(s())
      case Schema.Optional(_, _)                      => Some(None)
      case Schema.Sequence(_, fromChunk, _, _, _)     => Some(fromChunk(Chunk.empty))
      case Schema.Primitive(StandardType.UnitType, _) => Some(())
      case _                                          => None
    }

    private def decodeRecord[Z](path: Path, fields: Seq[Schema.Field[Z, _]]): Result[ListMap[Short, _]] =
      structDecoder(fields.map(_.schema), path)

    def structDecoder(fields: Seq[Schema[_]], path: Path): Result[ListMap[Short, Any]] = {
      val fieldSchemas = fields.zipWithIndex.map { case (schema, idx) => (idx + 1) -> schema }.toMap[Int, Schema[_]]

      @tailrec
      def readFields(m: ListMap[Short, Any]): Result[ListMap[Short, Any]] =
        Try { p.readFieldBegin() } match {
          case Failure(err) => fail(path, s"Error reading field begin: ${err.getMessage}")
          case Success(readField) => {
            if (readField.`type` == TType.STOP)
              succeed(m)
            else {
              val actualPath = path :+ s"fieldId:${readField.id}"
              fieldSchemas.get(readField.id.toInt) match {
                case Some(fieldSchema) =>
                  decode(actualPath, fieldSchema) match {
                    case Left(err)    => Left(err)
                    case Right(value) => readFields(m.updated(readField.id, value))
                  }
                case None =>
                  fail(actualPath, s"Could not find schema for field ID ${readField.id}")
              }
            }
          }
        }

      readFields(ListMap.empty)
    }

    def decodeSequence[Col, Elem](path: Path, schema: Schema.Sequence[Col, Elem, _]): Result[Col] = {
      @tailrec
      def decodeElements(n: Int, cb: ChunkBuilder[Elem]): Result[Chunk[Elem]] =
        if (n > 0)
          decode(path, schema.elementSchema) match {
            case Right(elem)   => decodeElements(n - 1, cb += (elem))
            case Left(failure) => fail(path, s"Error decoding Sequence element: $failure")
          } else
          succeed(cb.result())

      Try { p.readListBegin() }.fold(_ => fail(path, "Can not decode Sequence begin"), begin => decodeElements(begin.size, ChunkBuilder.make[Elem]()).map(schema.fromChunk))
    }

    def decodeMap[K, V](path: Path, schema: Schema.Map[K, V]): Result[scala.collection.immutable.Map[K, V]] = {
      @tailrec
      def decodeElements(n: Int, m: scala.collection.mutable.Map[K, V]): Result[scala.collection.immutable.Map[K, V]] =
        if (n > 0)
          (decode(path, schema.keySchema), decode(path, schema.valueSchema)) match {
            case (Right(key), Right(value)) => decodeElements(n - 1, m += ((key, value)))
            case (l, r) =>
              val key   = l.fold(_.error, _.toString)
              val value = r.fold(_.error, _.toString)
              fail(path, s"Error decoding Map element (key: $key; value: $value)")
          } else
          succeed(m.toMap)

      Try {
        p.readMapBegin()
      }.fold(_ => fail(path, "Can not decode Map begin"), begin => decodeElements(begin.size, scala.collection.mutable.Map.empty[K, V]))
    }

    def decodeSet[A](path: Path, schema: Schema.Set[A]): Result[scala.collection.immutable.Set[A]] = {
      @tailrec
      def decodeElements(n: Int, cb: ChunkBuilder[A]): Result[Chunk[A]] =
        if (n > 0)
          decode(path, schema.elementSchema) match {
            case Right(elem) => decodeElements(n - 1, cb += (elem))
            case Left(_)     => fail(path, "Error decoding Set element")
          } else
          succeed(cb.result())

      Try { p.readSetBegin() }.fold(_ => fail(path, "Can not decode Set begin"), begin => decodeElements(begin.size, ChunkBuilder.make[A]()).map(_.toSet))
    }

    private[codec] object ProductDecoder {

      def unapply[A](schema: Schema[A]): Option[Path => Result[A]] = schema match {
        case s: Schema.CaseClass1[_, A]                                                        => Some(caseClass1Decoder(s))
        case s: Schema.CaseClass2[_, _, A]                                                     => Some(caseClass2Decoder(s))
        case s: Schema.CaseClass3[_, _, _, A]                                                  => Some(caseClass3Decoder(s))
        case s: Schema.CaseClass4[_, _, _, _, A]                                               => Some(caseClass4Decoder(s))
        case s: Schema.CaseClass5[_, _, _, _, _, A]                                            => Some(caseClass5Decoder(s))
        case s: Schema.CaseClass6[_, _, _, _, _, _, A]                                         => Some(caseClass6Decoder(s))
        case s: Schema.CaseClass7[_, _, _, _, _, _, _, A]                                      => Some(caseClass7Decoder(s))
        case s: Schema.CaseClass8[_, _, _, _, _, _, _, _, A]                                   => Some(caseClass8Decoder(s))
        case s: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, A]                                => Some(caseClass9Decoder(s))
        case s: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, A]                            => Some(caseClass10Decoder(s))
        case s: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, A]                         => Some(caseClass11Decoder(s))
        case s: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, A]                      => Some(caseClass12Decoder(s))
        case s: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, A]                   => Some(caseClass13Decoder(s))
        case s: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, A]                => Some(caseClass14Decoder(s))
        case s: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]             => Some(caseClass15Decoder(s))
        case s: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]          => Some(caseClass16Decoder(s))
        case s: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]       => Some(caseClass17Decoder(s))
        case s: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]    => Some(caseClass18Decoder(s))
        case s: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] => Some(caseClass19Decoder(s))
        case s: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
          Some(caseClass20Decoder(s))
        case s: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
          Some(caseClass21Decoder(s))
        case s: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
          Some(caseClass22Decoder(s))
        case _ => None
      }

      private def unsafeDecodeFields[Z](path: Path, fields: Schema.Field[Z, _]*): Result[Array[Any]] = {
        val buffer = Array.ofDim[Any](fields.size)

        @tailrec
        def addFields(values: ListMap[Short, Any], index: Int): Result[Array[Any]] =
          if (index >= fields.size) Right(buffer)
          else {
            val Schema.Field(label, schema, annotations, _, _, _) = fields(index)
            val rawValue                                          = values.get((index + 1).toShort)
            rawValue match {
              case Some(value) =>
                buffer.update(index, value)
                addFields(values, index + 1)
              case None =>
                emptyValue(schema) match {
                  case Some(value) =>
                    buffer.update(index, value)
                    addFields(values, index + 1)
                  case None =>
                    val optionalFieldAnnotation = annotations.collectFirst({ case a: optionalField => a })
                    if (optionalFieldAnnotation.isDefined) {
                      buffer.update(index, schema.defaultValue.toOption.get)
                      addFields(values, index + 1)
                    } else fail(path :+ label, "Missing value")
                }
            }
          }

        structDecoder(fields.map(_.schema), path).flatMap(addFields(_, 0))
      }

      @tailrec
      private def validateBuffer(path: Path, index: Int, buffer: Array[Any]): Result[Array[Any]] =
        if (index == buffer.length - 1 && buffer(index) != null)
          succeed(buffer)
        else if (buffer(index) == null)
          fail(path, s"Missing field number $index.")
        else
          validateBuffer(path, index + 1, buffer)

      private def caseClass1Decoder[A, Z](schema: Schema.CaseClass1[A, Z])(path: Path): Result[Z] =
        unsafeDecodeFields(path, schema.field).flatMap { buffer =>
          if (buffer(0) == null)
            fail(path, "Missing field 1.")
          else
            succeed(schema.defaultConstruct(buffer(0).asInstanceOf[A]))
        }

      private def caseClass2Decoder[A1, A2, Z](schema: Schema.CaseClass2[A1, A2, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2])

      private def caseClass3Decoder[A1, A2, A3, Z](schema: Schema.CaseClass3[A1, A2, A3, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3])

      private def caseClass4Decoder[A1, A2, A3, A4, Z](schema: Schema.CaseClass4[A1, A2, A3, A4, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4])

      private def caseClass5Decoder[A1, A2, A3, A4, A5, Z](schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5])

      private def caseClass6Decoder[A1, A2, A3, A4, A5, A6, Z](schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6])

      private def caseClass7Decoder[A1, A2, A3, A4, A5, A6, A7, Z](schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7])

      private def caseClass8Decoder[A1, A2, A3, A4, A5, A6, A7, A8, Z](schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8])

      private def caseClass9Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9])

      private def caseClass10Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10])

      private def caseClass11Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11])

      private def caseClass12Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12])

      private def caseClass13Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13]
        )

      private def caseClass14Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14]
        )

      private def caseClass15Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15]
        )

      private def caseClass16Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16]
        )

      private def caseClass17Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16],
          buffer(16).asInstanceOf[A17]
        )

      private def caseClass18Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16],
          buffer(16).asInstanceOf[A17],
          buffer(17).asInstanceOf[A18]
        )

      private def caseClass19Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](schema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16],
          buffer(16).asInstanceOf[A17],
          buffer(17).asInstanceOf[A18],
          buffer(18).asInstanceOf[A19]
        )

      private def caseClass20Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](schema: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16],
          buffer(16).asInstanceOf[A17],
          buffer(17).asInstanceOf[A18],
          buffer(18).asInstanceOf[A19],
          buffer(19).asInstanceOf[A20]
        )

      private def caseClass21Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](schema: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20, schema.field21)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16],
          buffer(16).asInstanceOf[A17],
          buffer(17).asInstanceOf[A18],
          buffer(18).asInstanceOf[A19],
          buffer(19).asInstanceOf[A20],
          buffer(20).asInstanceOf[A21]
        )

      private def caseClass22Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](schema: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20, schema.field21, schema.field22)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9],
          buffer(9).asInstanceOf[A10],
          buffer(10).asInstanceOf[A11],
          buffer(11).asInstanceOf[A12],
          buffer(12).asInstanceOf[A13],
          buffer(13).asInstanceOf[A14],
          buffer(14).asInstanceOf[A15],
          buffer(15).asInstanceOf[A16],
          buffer(16).asInstanceOf[A17],
          buffer(17).asInstanceOf[A18],
          buffer(18).asInstanceOf[A19],
          buffer(19).asInstanceOf[A20],
          buffer(20).asInstanceOf[A21],
          buffer(21).asInstanceOf[A22]
        )
    }
  }

}
