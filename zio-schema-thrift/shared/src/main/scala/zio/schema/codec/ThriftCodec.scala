package zio.schema.codec
import java.nio.ByteBuffer
import java.time.{
  DayOfWeek,
  Duration,
  Instant,
  LocalDate,
  LocalDateTime,
  LocalTime,
  Month,
  MonthDay,
  OffsetDateTime,
  OffsetTime,
  Period,
  Year,
  YearMonth,
  ZoneId,
  ZoneOffset,
  ZonedDateTime
}
import java.util.UUID

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.util.control.NonFatal
import scala.util.{ Failure, Success, Try }

import org.apache.thrift.protocol.{ TBinaryProtocol, TField, TProtocol, TType }

import zio.schema.ast.SchemaAst
import zio.schema.codec.ThriftCodec.Thrift.{ durationStructure, monthDayStructure, periodStructure, yearMonthStructure }
import zio.schema.{ Schema, StandardType }
import zio.stream.ZTransducer
import zio.{ Chunk, ChunkBuilder, ZIO }

object ThriftCodec extends Codec {
  override def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte] =
    ZTransducer.fromPush(
      (opt: Option[Chunk[A]]) =>
        ZIO.succeed(opt.map(values => values.flatMap(new Encoder().encode(schema, _))).getOrElse(Chunk.empty))
    )

  override def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A] =
    ZTransducer.fromPush(
      (opt: Option[Chunk[Byte]]) =>
        ZIO.fromEither(
          opt
            .map(
              chunk =>
                new Decoder(chunk)
                  .decode(Chunk.empty, schema)
                  .map(Chunk(_))
                  .left
                  .map(err => s"Error at path /${err.path.mkString(".")}: ${err.error}")
            )
            .getOrElse(Right(Chunk.empty))
        )
    )

  override def encode[A](schema: Schema[A]): A => Chunk[Byte] = a => new Encoder().encode(schema, a)

  override def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A] =
    ch =>
      if (ch.isEmpty)
        Left("No bytes to decode")
      else
        new Decoder(ch)
          .decode(Chunk.empty, schema)
          .left
          .map(
            err => s"Error at path /${err.path.mkString(".")}: ${err.error}"
          )

  object Thrift {

    val monthDayStructure: Seq[Schema.Field[Int]] =
      Seq(
        Schema.Field("month", Schema.Primitive(StandardType.IntType)),
        Schema.Field("day", Schema.Primitive(StandardType.IntType))
      )

    val periodStructure: Seq[Schema.Field[Int]] = Seq(
      Schema.Field("years", Schema.Primitive(StandardType.IntType)),
      Schema.Field("months", Schema.Primitive(StandardType.IntType)),
      Schema.Field("days", Schema.Primitive(StandardType.IntType))
    )

    val yearMonthStructure: Seq[Schema.Field[Int]] =
      Seq(
        Schema.Field("year", Schema.Primitive(StandardType.IntType)),
        Schema.Field("month", Schema.Primitive(StandardType.IntType))
      )

    val durationStructure: Seq[Schema.Field[_]] =
      Seq(
        Schema.Field("seconds", Schema.Primitive(StandardType.LongType)),
        Schema.Field("nanos", Schema.Primitive(StandardType.IntType))
      )
  }

  private def unwrapLazy(schema: Schema[_]) = schema match {
    case s @ Schema.Lazy(_) => s.schema
    case s                  => s
  }

  private def isOptional(schema: Schema[_]): Boolean = schema match {
    case Schema.Optional(_, _) => true
    case Schema.Lazy(s)        => isOptional(s())
    case _                     => false
  }

  class Encoder {
    val write = new ChunkTransport.Write()
    val p     = new TBinaryProtocol(write)

    def encode[A](schema: Schema[A], value: A): Chunk[Byte] = {
      encodeValue(None, schema, value)
      write.chunk
    }

    @tailrec
    final def getType[A](schema: Schema[A]): Byte = schema match {
      case _: Schema.Record[A]                 => TType.STRUCT
      case Schema.Sequence(_, _, _, _, _)      => TType.LIST
      case Schema.MapSchema(_, _, _)           => TType.MAP
      case Schema.SetSchema(_, _)              => TType.SET
      case Schema.Transform(codec, _, _, _, _) => getType(codec)
      case Schema.Primitive(standardType, _)   => getPrimitiveType(standardType)
      case Schema.Tuple(_, _, _)               => TType.STRUCT
      case Schema.Optional(codec, _)           => getType(codec)
      case Schema.EitherSchema(_, _, _)        => TType.STRUCT
      case Schema.Lazy(lzy)                    => getType(lzy())
      case Schema.Meta(_, _)                   => getType(Schema[SchemaAst])
      case _: Schema.Enum[A]                   => TType.STRUCT
      case _                                   => TType.VOID
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
        case StandardType.Duration(_)           => TType.STRUCT
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
        case (StandardType.Duration(_), v: Duration) =>
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
        case (_, _) => ()
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

    def encodeMap[K, V](fieldNumber: Option[Short], schema: Schema.MapSchema[K, V], v: Map[K, V]): Unit = {
      writeFieldBegin(fieldNumber, TType.MAP)
      p.writeMapBegin(new org.apache.thrift.protocol.TMap(getType(schema.ks), getType(schema.vs), v.size))
      v.foreach {
        case (k, v) =>
          encodeValue(None, schema.ks, k)
          encodeValue(None, schema.vs, v)
      }
    }

    def encodeSet[A](fieldNumber: Option[Short], schema: Schema[A], v: Set[A]): Unit = {
      writeFieldBegin(fieldNumber, TType.SET)
      p.writeSetBegin(new org.apache.thrift.protocol.TSet(getType(schema), v.size))
      v.foreach(encodeValue(None, schema, _))
    }

    def encodeOptional[A](fieldNumber: Option[Short], codec: Schema[A], v: Option[A]): Unit = {
      writeFieldBegin(fieldNumber, TType.STRUCT)
      v match {
        case None =>
          encodeValue(Some(1), Schema.primitive(StandardType.UnitType), ())
        case Some(value) =>
          encodeValue(Some(2), codec, value)
      }
      p.writeFieldStop()
    }

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    def encodeValue[A](fieldNumber: Option[Short], schema: Schema[A], value: A): Unit =
      (schema, value) match {
        case (Schema.GenericRecord(structure, _), v: Map[String, _]) => encodeRecord(fieldNumber, structure.toChunk, v)
        case (Schema.Sequence(element, _, g, _, _), v)               => encodeSequence(fieldNumber, element, g(v))
        case (mapSchema @ Schema.MapSchema(_, _, _), map: Map[k, v]) => encodeMap(fieldNumber, mapSchema, map)
        case (Schema.SetSchema(s, _), set: Set[_])                   => encodeSet(fieldNumber, s, set)
        case (Schema.Transform(codec, _, g, _, _), _)                => g(value).foreach(encodeValue(fieldNumber, codec, _))
        case (Schema.Primitive(standardType, _), v)                  => encodePrimitive(fieldNumber, standardType, v)
        case (Schema.Tuple(left, right, _), v @ (_, _))              => encodeTuple(fieldNumber, left, right, v)
        case (Schema.Optional(codec, _), v: Option[_])               => encodeOptional(fieldNumber, codec, v)
        case (Schema.EitherSchema(left, right, _), v: Either[_, _])  => encodeEither(fieldNumber, left, right, v)
        case (lzy @ Schema.Lazy(_), v)                               => encodeValue(fieldNumber, lzy.schema, v)
        case (Schema.Meta(ast, _), _)                                => encodeValue(fieldNumber, Schema[SchemaAst], ast)
        case ProductEncoder(encode) =>
          writeFieldBegin(fieldNumber, TType.STRUCT)
          encode()
        case (Schema.Enum1(c, _), v)                          => encodeEnum(fieldNumber, v, c)
        case (Schema.Enum2(c1, c2, _), v)                     => encodeEnum(fieldNumber, v, c1, c2)
        case (Schema.Enum3(c1, c2, c3, _), v)                 => encodeEnum(fieldNumber, v, c1, c2, c3)
        case (Schema.Enum4(c1, c2, c3, c4, _), v)             => encodeEnum(fieldNumber, v, c1, c2, c3, c4)
        case (Schema.Enum5(c1, c2, c3, c4, c5, _), v)         => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5)
        case (Schema.Enum6(c1, c2, c3, c4, c5, c6, _), v)     => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6)
        case (Schema.Enum7(c1, c2, c3, c4, c5, c6, c7, _), v) => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7)
        case (Schema.Enum8(c1, c2, c3, c4, c5, c6, c7, c8, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8)
        case (Schema.Enum9(c1, c2, c3, c4, c5, c6, c7, c8, c9, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9)
        case (Schema.Enum10(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
        case (Schema.Enum11(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
        case (Schema.Enum12(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
        case (Schema.Enum13(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
        case (Schema.Enum14(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
        case (Schema.Enum15(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
        case (Schema.Enum16(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
        case (Schema.Enum17(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
        case (Schema.Enum18(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
        case (Schema.Enum19(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
        case (Schema.Enum20(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
        case (Schema.Enum21(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
        case (Schema.Enum22(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _), v) =>
          encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
        case (Schema.EnumN(cs, _), v) => encodeEnum(fieldNumber, v, cs.toSeq: _*)
        case (_, _)                   => ()
      }

    private def encodeEnum[Z, A](fieldNumber: Option[Short], value: Z, cases: Schema.Case[_, Z]*): Unit = {
      writeFieldBegin(fieldNumber, TType.STRUCT)
      val fieldIndex = cases.indexWhere(c => c.deconstruct(value).isDefined)
      if (fieldIndex >= 0) {
        val subtypeCase = cases(fieldIndex)
        encodeValue(Some((fieldIndex + 1).shortValue), subtypeCase.codec.asInstanceOf[Schema[Any]], subtypeCase.unsafeDeconstruct(value))
      }
      p.writeFieldStop()
    }

    private def encodeEither[A, B](fieldNumber: Option[Short], left: Schema[A], right: Schema[B], either: Either[A, B]): Unit = {
      writeFieldBegin(fieldNumber, TType.STRUCT)
      either match {
        case Left(value)  => encodeValue(Some(1), left, value)
        case Right(value) => encodeValue(Some(2), right, value)
      }
    }

    def tupleSchema[A, B](first: Schema[A], second: Schema[B]): Seq[Schema.Field[_]] =
      Seq(Schema.Field("first", first), Schema.Field("second", second))

    private def encodeTuple[A, B](fieldNumber: Option[Short], left: Schema[A], right: Schema[B], tuple: (A, B)): Unit =
      encodeRecord(fieldNumber, tupleSchema(left, right), ListMap[String, Any]("first" -> tuple._1, "second" -> tuple._2))

    private def writeStructure(fields: Seq[(Schema.Field[_], Any)]): Unit = {
      fields.zipWithIndex.foreach {
        case ((Schema.Field(_, schema, _), value), fieldNumber) =>
          encodeValue(Some((fieldNumber + 1).shortValue), schema, value)
      }
      p.writeFieldStop()
    }

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[codec] object ProductEncoder {

      def unapply[A](schemaAndValue: (Schema[A], A)): Option[() => Unit] = schemaAndValue match {
        case (Schema.CaseClass1(f, _, ext, _), v)             => Some(encodeCaseClass(v, f  -> ext))
        case (Schema.CaseClass2(f1, f2, _, ext1, ext2, _), v) => Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2))
        case (Schema.CaseClass3(f1, f2, f3, _, ext1, ext2, ext3, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3))
        case (Schema.CaseClass4(f1, f2, f3, f4, _, ext1, ext2, ext3, ext4, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4))
        case (Schema.CaseClass5(f1, f2, f3, f4, f5, _, ext1, ext2, ext3, ext4, ext5, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5))
        case (Schema.CaseClass6(f1, f2, f3, f4, f5, f6, _, ext1, ext2, ext3, ext4, ext5, ext6, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6))
        case (Schema.CaseClass7(f1, f2, f3, f4, f5, f6, f7, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7))
        case (Schema.CaseClass8(f1, f2, f3, f4, f5, f6, f7, f8, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8))
        case (Schema.CaseClass9(f1, f2, f3, f4, f5, f6, f7, f8, f9, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9))
        case (Schema.CaseClass10(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10))
        case (Schema.CaseClass11(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11))
        case (Schema.CaseClass12(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12))
        case (Schema.CaseClass13(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13))
        case (Schema.CaseClass14(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14))
        case (Schema.CaseClass15(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15))
        case (Schema.CaseClass16(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16))
        case (Schema.CaseClass17(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17))
        case (Schema.CaseClass18(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18))
        case (Schema.CaseClass19(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19))
        case (Schema.CaseClass20(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, ext20, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19, f20 -> ext20))
        case (Schema.CaseClass21(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, ext20, ext21, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19, f20 -> ext20, f21 -> ext21))
        case (Schema.CaseClass22(f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12, f13, f14, f15, f16, f17, f18, f19, f20, f21, f22, _, ext1, ext2, ext3, ext4, ext5, ext6, ext7, ext8, ext9, ext10, ext11, ext12, ext13, ext14, ext15, ext16, ext17, ext18, ext19, ext20, ext21, ext22, _), v) =>
          Some(encodeCaseClass(v, f1 -> ext1, f2 -> ext2, f3 -> ext3, f4 -> ext4, f5 -> ext5, f6 -> ext6, f7 -> ext7, f8 -> ext8, f9 -> ext9, f10 -> ext10, f11 -> ext11, f12 -> ext12, f13 -> ext13, f14 -> ext14, f15 -> ext15, f16 -> ext16, f17 -> ext17, f18 -> ext18, f19 -> ext19, f20 -> ext20, f21 -> ext21, f22 -> ext22))
        case _ => None
      }

      private def encodeCaseClass[Z](value: Z, fields: (Schema.Field[_], Z => Any)*): () => Unit = () => writeStructure(fields.map { case (schema, ext) => (schema, ext(value)) })

      object OptionalSchema {

        def unapply(schema: Schema[Any]): Option[Schema[Any]] =
          if (schema.isInstanceOf[Schema.Optional[_]])
            Some(schema.asInstanceOf[Schema.Optional[Any]].codec)
          else
            None
      }

    }

    def encodeRecord(fieldNumber: Option[Short], structure: Seq[Schema.Field[_]], data: ListMap[String, _]): Unit = {
      writeFieldBegin(fieldNumber, TType.STRUCT)
      writeStructure(structure.map(schema => (schema, data(schema.label))))
    }
  }

  class Decoder(chunk: Chunk[Byte]) {
    type Path = Chunk[String]
    case class Error(path: Path, error: String)
    type Result[A]          = Either[Error, A]
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

    def decodeVarInt: PrimitiveResult[Int] =
      decodePrimitive(
        p =>
          Try(p.readI64())
            .map(_.intValue)
            .orElse(Try(p.readI32()))
            .orElse(Try(p.readI16()).map(_.intValue))
            .orElse(Try(p.readByte()).map(_.intValue))
            .get,
        "VarInt"
      )

    def decodeLong: PrimitiveResult[Long] =
      decodePrimitive(_.readI64(), "Long")

    def decodeFloat: PrimitiveResult[Float] =
      decodePrimitive(_.readDouble().toFloat, "Float")

    def decodeDouble: PrimitiveResult[Double] =
      decodePrimitive(_.readDouble(), "Double")

    def decodeBinary: PrimitiveResult[Chunk[Byte]] =
      decodePrimitive(p => Chunk.fromByteBuffer(p.readBinary()), "Binary")

    def decode[A](path: Path, schema: Schema[A]): Result[A] =
      schema match {
        case Schema.GenericRecord(structure, _) => {
          val fields = structure.toChunk
          decodeRecord(path, fields).map(_.map { case (index, value) => (fields(index - 1).label, value) })
        }
        case seqSchema @ Schema.Sequence(_, _, _, _, _)                                                                            => decodeSequence(path, seqSchema)
        case mapSchema @ Schema.MapSchema(_, _, _)                                                                                 => decodeMap(path, mapSchema)
        case setSchema @ Schema.SetSchema(_, _)                                                                                    => decodeSet(path, setSchema)
        case Schema.Transform(codec, f, _, _, _)                                                                                   => transformDecoder(path, codec, f)
        case Schema.Primitive(standardType, _)                                                                                     => primitiveDecoder(path, standardType)
        case Schema.Tuple(left, right, _)                                                                                          => tupleDecoder(path, left, right)
        case optionalSchema @ Schema.Optional(_, _)                                                                                => optionalDecoder(path, optionalSchema)
        case Schema.Fail(message, _)                                                                                               => fail(path, message)
        case Schema.EitherSchema(left, right, _)                                                                                   => eitherDecoder(path, left, right)
        case lzy @ Schema.Lazy(_)                                                                                                  => decode(path, lzy.schema)
        case Schema.Meta(_, _)                                                                                                     => decode(path, Schema[SchemaAst]).map(_.toSchema)
        case ProductDecoder(decoder)                                                                                               => decoder(path)
        case Schema.Enum1(c, _)                                                                                                    => enumDecoder(path, c)
        case Schema.Enum2(c1, c2, _)                                                                                               => enumDecoder(path, c1, c2)
        case Schema.Enum3(c1, c2, c3, _)                                                                                           => enumDecoder(path, c1, c2, c3)
        case Schema.Enum4(c1, c2, c3, c4, _)                                                                                       => enumDecoder(path, c1, c2, c3, c4)
        case Schema.Enum5(c1, c2, c3, c4, c5, _)                                                                                   => enumDecoder(path, c1, c2, c3, c4, c5)
        case Schema.Enum6(c1, c2, c3, c4, c5, c6, _)                                                                               => enumDecoder(path, c1, c2, c3, c4, c5, c6)
        case Schema.Enum7(c1, c2, c3, c4, c5, c6, c7, _)                                                                           => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7)
        case Schema.Enum8(c1, c2, c3, c4, c5, c6, c7, c8, _)                                                                       => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8)
        case Schema.Enum9(c1, c2, c3, c4, c5, c6, c7, c8, c9, _)                                                                   => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9)
        case Schema.Enum10(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _)                                                             => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
        case Schema.Enum11(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _)                                                        => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
        case Schema.Enum12(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _)                                                   => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
        case Schema.Enum13(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _)                                              => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
        case Schema.Enum14(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _)                                         => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
        case Schema.Enum15(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _)                                    => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
        case Schema.Enum16(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _)                               => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
        case Schema.Enum17(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _)                          => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
        case Schema.Enum18(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _)                     => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
        case Schema.Enum19(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _)                => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
        case Schema.Enum20(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _)           => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
        case Schema.Enum21(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _)      => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
        case Schema.Enum22(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _) => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
        case Schema.EnumN(cs, _)                                                                                                   => enumDecoder(path, cs.toSeq: _*)
        case _                                                                                                                     => fail(path, s"Unknown schema ${schema.getClass.getName}")
      }

    private def optionalDecoder[A](path: Path, schema: Schema.Optional[A]): Result[Option[A]] =
      Try {
        val readField = p.readFieldBegin()
        val res = readField.id match {
          case 1 => succeed(None)
          case 2 => decode(path :+ "Some", schema.codec).map(Some(_))
          case _ =>
            fail(path, s"Error decoding optional, wrong field id ${readField.id}")
        }
        p.readFieldBegin()
        res.asInstanceOf[Result[Option[A]]]
      }.fold(err => fail(path, s"Error decoding optional ${err.getMessage}"), identity)

    private def enumDecoder[Z, A](path: Path, cases: Schema.Case[_, Z]*): Result[Z] =
      Try {
        val readField = p.readFieldBegin()
        if (readField.id > cases.length)
          fail(path, s"Error decoding enum with cases ${cases.map(_.id).mkString(", ")}, enum id out of range: ${readField.id}")
        else {
          val subtypeCase = cases(readField.id - 1)
          val res         = decode(path :+ s"[case:${subtypeCase.id}]", subtypeCase.codec)
          res.foreach { _ =>
            p.readFieldBegin()
          }
          res.asInstanceOf[Result[Z]]
        }
      }.fold(err => fail(path, s"Error decoding enum with cases ${cases.map(_.id).mkString(", ")} ${err.getMessage}"), identity)

    private def eitherDecoder[A, B](path: Path, left: Schema[A], right: Schema[B]): Result[Either[A, B]] = {
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

    private def transformDecoder[A, B](path: Path, schema: Schema[B], f: B => Either[String, A]): Result[A] =
      decode(path, schema).flatMap(a => f(a).left.map(msg => Error(path, msg)))

    private def primitiveDecoder[A](path: Path, standardType: StandardType[A]): Result[A] =
      standardType match {
        case StandardType.UnitType   => Right(())
        case StandardType.StringType => decodeString(path)
        case StandardType.BoolType   => decodeBoolean(path)
        case StandardType.ShortType  => decodeShort(path)
        case StandardType.IntType    => decodeInt(path)
        case StandardType.LongType   => decodeLong(path)
        case StandardType.FloatType  => decodeFloat(path)
        case StandardType.DoubleType => decodeDouble(path)
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
          decodeVarInt(path).map(_.intValue).map(DayOfWeek.of)
        case StandardType.MonthType =>
          decodeVarInt(path).map(_.intValue).map(Month.of)
        case StandardType.MonthDayType =>
          decodeRecord(path, monthDayStructure)
            .map(data => MonthDay.of(data.getOrElse(1, 0).asInstanceOf[Int], data.getOrElse(2, 0).asInstanceOf[Int]))
        case StandardType.PeriodType =>
          decodeRecord(path, periodStructure)
            .map(data => Period.of(data.getOrElse(1, 0).asInstanceOf[Int], data.getOrElse(2, 0).asInstanceOf[Int], data.getOrElse(3, 0).asInstanceOf[Int]))
        case StandardType.YearType =>
          decodeVarInt(path).map(_.intValue).map(Year.of)
        case StandardType.YearMonthType =>
          decodeRecord(path, yearMonthStructure)
            .map(data => YearMonth.of(data.getOrElse(1, 0).asInstanceOf[Int], data.getOrElse(2, 0).asInstanceOf[Int]))
        case StandardType.ZoneIdType => decodeString(path).map(ZoneId.of)
        case StandardType.ZoneOffsetType =>
          decodeVarInt(path)
            .map(_.intValue)
            .map(ZoneOffset.ofTotalSeconds)
        case StandardType.Duration(_) =>
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
        case _ => fail(path, "Unsupported primitive type")
      }

    private def emptyValue[A](schema: Schema[A]): Option[A] = schema match {
      case Schema.Lazy(s)                             => emptyValue(s())
      case Schema.Optional(_, _)                      => Some(None)
      case Schema.Sequence(_, fromChunk, _, _, _)     => Some(fromChunk(Chunk.empty))
      case Schema.Primitive(StandardType.UnitType, _) => Some(())
      case _                                          => None
    }

    private def decodeRecord(path: Path, fields: Seq[Schema.Field[_]]): Result[ListMap[Short, _]] =
      structDecoder(fields.map(_.schema), path)

    def structDecoder(fields: Seq[Schema[_]], path: Path): Result[ListMap[Short, Any]] = {
      def safeRead[A](path: Path, name: String, f: TProtocol => A) =
        Try {
          f(p)
        }.toEither.left.map(_ => Error(path, s"Unable to decode $name"))

      @tailrec
      def readFields(m: ListMap[Short, Any]): Result[ListMap[Short, Any]] =
        Try { p.readFieldBegin() } match {
          case Failure(_) => fail(path, "Error reading field begin")
          case Success(readField) => {
            if (readField.`type` == TType.STOP)
              succeed(m)
            else {
              val actualPath = path :+ s"fieldId:${readField.id}"
              (readField.`type` match {
                case TType.BOOL   => safeRead(actualPath, "Bool", _.readBool())
                case TType.BYTE   => safeRead(actualPath, "Byte", _.readByte())
                case TType.DOUBLE => safeRead(actualPath, "Double", _.readDouble())
                case TType.I16    => safeRead(actualPath, "Short", _.readI16())
                case TType.I32    => safeRead(actualPath, "Int", _.readI32())
                case TType.I64    => safeRead(actualPath, "Long", _.readI64())
                case TType.STRING => safeRead(actualPath, "String", _.readString())
                case TType.STRUCT => decode(actualPath, fields(readField.id - 1))
                case TType.MAP    => decodeMap(actualPath, unwrapLazy(fields(readField.id - 1)).asInstanceOf[Schema.MapSchema[_, _]])
                case TType.SET    => decodeSet(actualPath, unwrapLazy(fields(readField.id - 1)).asInstanceOf[Schema.SetSchema[_]])
                case TType.LIST   => decodeSequence(actualPath, unwrapLazy(fields(readField.id - 1)).asInstanceOf[Schema.Sequence[_, _, _]])
                case TType.ENUM   => safeRead(actualPath, "Enum", _.readI32())
                case _            => fail(actualPath, s"Unknown type ${readField.`type`}")
              }) match {
                case Right(value) => readFields(m.updated(readField.id, value))
                case Left(err)    => Left(err)
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
          decode(path, schema.schemaA) match {
            case Right(elem) => decodeElements(n - 1, cb += (elem))
            case Left(_)     => fail(path, "Error decoding Sequence element")
          } else
          succeed(cb.result())

      Try { p.readListBegin() }.fold(_ => fail(path, "Can not decode Sequence begin"), begin => decodeElements(begin.size, ChunkBuilder.make[Elem]()).map(schema.fromChunk))
    }

    def decodeMap[K, V](path: Path, schema: Schema.MapSchema[K, V]): Result[Map[K, V]] = {
      @tailrec
      def decodeElements(n: Int, m: scala.collection.mutable.Map[K, V]): Result[Map[K, V]] =
        if (n > 0)
          (decode(path, schema.ks), decode(path, schema.vs)) match {
            case (Right(key), Right(value)) => decodeElements(n - 1, m += ((key, value)))
            case _                          => fail(path, "Error decoding Map element")
          } else
          succeed(m.toMap)

      Try {
        p.readMapBegin()
      }.fold(_ => fail(path, "Can not decode Map begin"), begin => decodeElements(begin.size, scala.collection.mutable.Map.empty[K, V]))
    }

    def decodeSet[A](path: Path, schema: Schema.SetSchema[A]): Result[Set[A]] = {
      @tailrec
      def decodeElements(n: Int, cb: ChunkBuilder[A]): Result[Chunk[A]] =
        if (n > 0)
          decode(path, schema.as) match {
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

      private def unsafeDecodeFields(path: Path, fields: Schema.Field[_]*): Result[Array[Any]] = {
        val buffer = Array.ofDim[Any](fields.size)

        @tailrec
        def addFields(values: ListMap[Short, Any], index: Int): Result[Array[Any]] =
          if (index >= fields.size) Right(buffer)
          else {
            val Schema.Field(label, schema, _) = fields(index)
            val rawValue                       = values.get((index + 1).toShort)
            rawValue match {
              case Some(value) =>
                buffer.update(index, value)
                addFields(values, index + 1)
              case None =>
                emptyValue(schema) match {
                  case Some(value) =>
                    buffer.update(index, value)
                    addFields(values, index + 1)
                  case None => fail(path :+ label, "Missing value")
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
            succeed(schema.construct(buffer(0).asInstanceOf[A]))
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
