package zio.schema.codec
import org.apache.thrift.protocol.{TBinaryProtocol, TField, TProtocol, TType}
import zio.schema.codec.ThriftCodec.Thrift.{durationStructure, monthDayStructure, periodStructure, yearMonthStructure}
import zio.{Chunk, ChunkBuilder, ZIO}
import zio.schema.{Schema, StandardType}
import zio.stream.ZTransducer

import java.nio.ByteBuffer
import java.time.{DayOfWeek, Duration, Instant, LocalDate, LocalDateTime, LocalTime, Month, MonthDay, OffsetDateTime, OffsetTime, Period, Year, YearMonth, ZoneId, ZoneOffset, ZonedDateTime}
import java.util.UUID
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.util.Try
import scala.util.control.NonFatal

object ThriftCodec extends Codec {
  override def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte] =
    ZTransducer.fromPush(
      (opt: Option[Chunk[A]]) =>
        ZIO.succeed(opt.map(values => values.flatMap(new Encoder().encode(schema, _))).getOrElse(Chunk.empty))
    )

  override def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A] =
    ZTransducer.fromPush(
      (opt: Option[Chunk[Byte]]) =>
        ZIO.fromEither(opt.map(chunk =>
          new Decoder(chunk).decode(schema, Chunk.empty).map(Chunk(_)).left.map(err => s"Error at path /${err.path.mkString(".")}: ${err.error}")
        ).getOrElse(Right(Chunk.empty)))
    )


  override def encode[A](schema: Schema[A]): A => Chunk[Byte] = a => new Encoder().encode(schema, a)

  override def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A] =
    ch =>
      if (ch.isEmpty)
        Left("No bytes to decode")
      else
        new Decoder(ch).decode(schema, Chunk.empty).left.map(
          err => s"Error at path /${err.path.mkString(".")}: ${err.error}"
        )

  object Thrift {

    def tupleSchema[A, B](first: Schema[A], second: Schema[B]): Schema[ListMap[String, _]] =
      Schema.record(Schema.Field("first", first), Schema.Field("second", second))

    def singleSchema[A](codec: Schema[A]): Schema[ListMap[String, _]] = Schema.record(Schema.Field("value", codec))

    def monthDayStructure(): Seq[Schema.Field[Int]] =
      Seq(
        Schema.Field("month", Schema.Primitive(StandardType.IntType)),
        Schema.Field("day", Schema.Primitive(StandardType.IntType))
      )

    def periodStructure(): Seq[Schema.Field[Int]] = Seq(
      Schema.Field("years", Schema.Primitive(StandardType.IntType)),
      Schema.Field("months", Schema.Primitive(StandardType.IntType)),
      Schema.Field("days", Schema.Primitive(StandardType.IntType))
    )

    def yearMonthStructure(): Seq[Schema.Field[Int]] =
      Seq(
        Schema.Field("year", Schema.Primitive(StandardType.IntType)),
        Schema.Field("month", Schema.Primitive(StandardType.IntType))
      )

    def durationStructure(): Seq[Schema.Field[_]] =
      Seq(
        Schema.Field("seconds", Schema.Primitive(StandardType.LongType)),
        Schema.Field("nanos", Schema.Primitive(StandardType.IntType))
      )
  }

  private def unwrapLazy(schema: Schema[_]) = schema match {
    case Schema.Lazy(s) =>
      s()
    case s => s
  }

  private def isOptional(schema: Schema[_]): Boolean = schema match {
    case Schema.Optional(_, _) => true
    case Schema.Lazy(s) => isOptional(s())
    case _ => false
  }

  class Encoder {
    val write = new ChunkTransport.Write()
    val p = new TBinaryProtocol(write)

    def encode[A](schema: Schema[A], value: A): Chunk[Byte] = {
      encodeValue(None, schema, value)
      write.chunk
    }

    @tailrec
    final def getType[A](schema: Schema[A]): Byte = schema match {
      case Schema.Sequence(_, _, _, _)                  => TType.LIST
      case Schema.SetSchema(_, _)                       => TType.SET
      //        case (Schema.Transform(codec, _, g), _)                   => g(value).map(encode(codec, _)).getOrElse(Chunk.empty)
      case Schema.Primitive(standardType, _)                  => getPrimitiveType(standardType)
      //        case (Schema.Tuple(left, right), v @ (_, _))              => encodeTuple(left, right, v)
      case Schema.Optional(codec, _)              => getType(codec)
      //        case (Schema.EitherSchema(left, right), v: Either[_, _])  => encodeEither(left, right, v)
      case Schema.Lazy(lzy)                           => getType(lzy())
      //        case (Schema.Meta(ast), _)                                => encode(Schema[SchemaAst], ast)
      //FIXME add fieldNumber
      case _ : Schema.Record[A] => TType.STRUCT
      //        case (Schema.Enum1(c), v)                                 => encodeEnum(v, c)
      //        case (Schema.Enum2(c1, c2), v)                            => encodeEnum(v, c1, c2)
      //        case (Schema.Enum3(c1, c2, c3), v)                        => encodeEnum(v, c1, c2, c3)
      //        case (Schema.EnumN(cs), v)                                => encodeEnum(v, cs.toSeq: _*)
      case _                                               => TType.VOID
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
        case StandardType.MonthDayType => TType.STRUCT
        case StandardType.PeriodType => TType.STRUCT
        case StandardType.YearType => TType.I32
        case StandardType.YearMonthType => TType.STRUCT
        case StandardType.ZoneIdType => TType.STRING
        case StandardType.ZoneOffsetType => TType.I32
        case StandardType.Duration(_) => TType.STRUCT
        case StandardType.InstantType(_) => TType.STRING
        case StandardType.LocalDateType(_) => TType.STRING
        case StandardType.LocalTimeType(_) => TType.STRING
        case StandardType.LocalDateTimeType(_) => TType.STRING
        case StandardType.OffsetTimeType(_) => TType.STRING
        case StandardType.OffsetDateTimeType(_) => TType.STRING
        case StandardType.ZonedDateTimeType(_) => TType.STRING
        case _ => TType.VOID
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
          encodeRecord(None, monthDayStructure(), ListMap("month" -> v.getMonthValue, "day" -> v.getDayOfMonth))
        case (StandardType.PeriodType, v: Period) =>
          encodeRecord(
            None,
            periodStructure(),
            ListMap("years" -> v.getYears, "months" -> v.getMonths, "days" -> v.getDays)
          )
        case (StandardType.YearType, v: Year) =>
          p.writeI32(v.getValue)
        case (StandardType.YearMonthType, v: YearMonth) =>
          encodeRecord(None, yearMonthStructure(), ListMap("year" -> v.getYear, "month" -> v.getMonthValue))
        case (StandardType.ZoneIdType, v: ZoneId) =>
          p.writeString(v.getId)
        case (StandardType.ZoneOffsetType, v: ZoneOffset) =>
          p.writeI32(v.getTotalSeconds)
        case (StandardType.Duration(_), v: Duration) =>
          encodeRecord(None, durationStructure(), ListMap("seconds" -> v.getSeconds, "nanos" -> v.getNano))
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
      
    def encodePrimitive[A](fieldNumber: Option[Short], standardType: StandardType[A], value: A): Unit = {
      fieldNumber.foreach(num =>
        p.writeFieldBegin(
          new TField("", getPrimitiveType(standardType), num)
        )
      )
      writePrimitiveType(standardType, value)
      fieldNumber.foreach(_ =>
        p.writeFieldEnd()
      )
    }

    def encodeSequence[A](fieldNumber: Option[Short], schema: Schema[A], v: Chunk[A]): Unit = {
      fieldNumber.foreach(
        num => p.writeFieldBegin(new TField("", TType.LIST, num))
      )
      p.writeListBegin(new org.apache.thrift.protocol.TList(getType(schema), v.size))
      v.foreach(encodeValue(None, schema, _))
      p.writeListEnd();
      fieldNumber.foreach(
        _ => p.writeFieldEnd()
      )
    }

    def encodeSet[A](fieldNumber: Option[Short], schema: Schema[A], v: Set[A]): Unit = {
      fieldNumber.foreach(
        num => p.writeFieldBegin(new TField("", TType.SET, num))
      )
      p.writeSetBegin(new org.apache.thrift.protocol.TSet(getType(schema), v.size))
      v.foreach(encodeValue(None, schema, _))
    }


    def encodeOptional[A](fieldNumber: Option[Short], codec: Schema[A], v: Option[A]): Unit =
      v.foreach(encodeValue(fieldNumber, codec, _))

    def encodeValue[A](fieldNumber: Option[Short], schema: Schema[A], value: A): Unit =
      (schema, value) match {
        case (Schema.GenericRecord(structure, _), v: Map[String, _]) => encodeRecord(fieldNumber, structure.toChunk, v)
        case (Schema.Sequence(element, _, g, _), v)                  => encodeSequence(fieldNumber, element, g(v))
//        case (Schema.MapSchema(ks, vs, _), map: Map[k, v])           => encodeMap(fieldNumber, ks <*> vs, Chunk.fromIterable(map))
        case (Schema.SetSchema(s, _), set: Set[_])                   => encodeSet(fieldNumber, s, set)
        case (Schema.Transform(codec, _, g, _), _)                   => g(value).map(encodeValue(fieldNumber, codec, _)).getOrElse(Chunk.empty)
        case (Schema.Primitive(standardType, _), v)                  => encodePrimitive(fieldNumber, standardType, v)
        case (Schema.Tuple(left, right, _), v @ (_, _))              => encodeTuple(fieldNumber, left, right, v)
        case (Schema.Optional(codec, _), v: Option[_])               => encodeOptional(fieldNumber, codec, v)
        case (Schema.EitherSchema(left, right, _), v: Either[_, _])  => encodeEither(fieldNumber, left, right, v)
        case (lzy @ Schema.Lazy(_), v)                            => encodeValue(fieldNumber, lzy.schema, v)
//        case (Schema.Meta(ast), _)                                => encode(Schema[SchemaAst], ast)
        //FIXME add fieldNumber
        case ProductEncoder(encode)                               =>
          fieldNumber.foreach(
            num => p.writeFieldBegin(new TField("", TType.STRUCT, num))
          )
          encode()
          fieldNumber.foreach(
            _ => p.writeFieldEnd()
          )
        case (Schema.Enum1(c, _), v)                                                                                                    => encodeEnum(fieldNumber, v, c)
        case (Schema.Enum2(c1, c2, _), v)                                                                                               => encodeEnum(fieldNumber, v, c1, c2)
        case (Schema.Enum3(c1, c2, c3, _), v)                                                                                           => encodeEnum(fieldNumber, v, c1, c2, c3)
        case (Schema.Enum4(c1, c2, c3, c4, _), v)                                                                                       => encodeEnum(fieldNumber, v, c1, c2, c3, c4)
        case (Schema.Enum5(c1, c2, c3, c4, c5, _), v)                                                                                   => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5)
        case (Schema.Enum6(c1, c2, c3, c4, c5, c6, _), v)                                                                               => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6)
        case (Schema.Enum7(c1, c2, c3, c4, c5, c6, c7, _), v)                                                                           => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7)
        case (Schema.Enum8(c1, c2, c3, c4, c5, c6, c7, c8, _), v)                                                                       => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8)
        case (Schema.Enum9(c1, c2, c3, c4, c5, c6, c7, c8, c9, _), v)                                                                   => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9)
        case (Schema.Enum10(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _), v)                                                             => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
        case (Schema.Enum11(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _), v)                                                        => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
        case (Schema.Enum12(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _), v)                                                   => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
        case (Schema.Enum13(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _), v)                                              => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
        case (Schema.Enum14(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _), v)                                         => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
        case (Schema.Enum15(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _), v)                                    => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
        case (Schema.Enum16(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _), v)                               => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
        case (Schema.Enum17(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _), v)                          => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
        case (Schema.Enum18(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _), v)                     => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
        case (Schema.Enum19(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _), v)                => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
        case (Schema.Enum20(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _), v)           => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
        case (Schema.Enum21(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _), v)      => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
        case (Schema.Enum22(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _), v) => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
        case (Schema.EnumN(cs, _), v)                                                                                                   => encodeEnum(fieldNumber, v, cs.toSeq: _*)
        case (_, _)                                               => ()
      }

    private def encodeEnum[Z, A](
      fieldNumber: Option[Short], value: Z, cases: Schema.Case[_, Z]*
    ): Unit = {
      fieldNumber.foreach(num =>
        p.writeFieldBegin(
          new org.apache.thrift.protocol.TField("", TType.STRUCT, num)
        )
      )
      p.writeStructBegin(new org.apache.thrift.protocol.TStruct())
      val fieldIndex = cases.indexWhere(c => c.deconstruct(value).isDefined)
      if (fieldIndex >= 0) {
        val subtypeCase = cases(fieldIndex)
        encodeValue(
          Some((fieldIndex + 1).shortValue),
          subtypeCase.codec.asInstanceOf[Schema[Any]],
          subtypeCase.unsafeDeconstruct(value)
        )
      }
      p.writeFieldStop()
      p.writeStructEnd()
      fieldNumber.foreach(
        _ => p.writeFieldEnd()
      )
    }

    private def encodeEither[A, B](
      fieldNumber: Option[Short],
      left: Schema[A],
      right: Schema[B],
      either: Either[A, B]
    ): Unit = {
      fieldNumber.foreach(num =>
        p.writeFieldBegin(
          new org.apache.thrift.protocol.TField("", TType.STRUCT, num)
        )
      )
      either match {
        case Left(value)  => encodeValue(Some(1), left, value)
        case Right(value) => encodeValue(Some(2), right, value)
      }
      fieldNumber.foreach(
        _ => p.writeFieldEnd()
      )
    }

    def tupleSchema[A, B](first: Schema[A], second: Schema[B]): Seq[Schema.Field[_]] =
      Seq(Schema.Field("first", first), Schema.Field("second", second))

    private def encodeTuple[A, B](
      fieldNumber: Option[Short],
      left: Schema[A],
      right: Schema[B],
      tuple: (A, B)
    ): Unit =
      encodeRecord(
        fieldNumber,
        tupleSchema(left, right),
        ListMap[String, Any]("first" -> tuple._1, "second" -> tuple._2)
      )

    private def writeStructure(fields: Seq[(Schema.Field[_], Any)]): Unit = {
      p.writeStructBegin(new org.apache.thrift.protocol.TStruct())
      fields.zipWithIndex.foreach {
        case ((Schema.Field(_, schema, _), value), fieldNumber) =>

          encodeValue(Some((fieldNumber + 1).shortValue), schema, value)
      }
      p.writeFieldStop()
      p.writeStructEnd()
    }

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[codec] object ProductEncoder {

      def unapply[A](schemaAndValue: (Schema[A], A)): Option[() => Unit] = schemaAndValue match {
        case (Schema.CaseClass1(f, _, ext, _), v) => Some(encodeCaseClass(v, f -> ext))
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

      private def encodeCaseClass[Z](value: Z, fields: (Schema.Field[_], Z => Any)*): () => Unit = () =>
        writeStructure(fields.map{ case (schema, ext) => (schema, ext(value)) })
//      {
//        p.writeStructBegin(new org.apache.thrift.protocol.TStruct())
//        fields.zipWithIndex.foreach {
//          case ((Schema.Field(_, schema, _), ext), fieldNumber) =>
//            encodeValue(Some((fieldNumber + 1).shortValue), schema, ext(value))
//        }
//        p.writeFieldStop()
//        p.writeStructEnd()
//      }

      object OptionalSchema {
        def unapply(schema: Schema[Any]): Option[Schema[Any]] = {
          if(schema.isInstanceOf[Schema.Optional[_]])
            Some(schema.asInstanceOf[Schema.Optional[Any]].codec)
          else
            None
        }
      }

    }


    def encodeRecord(
      fieldNumber: Option[Short],
      structure: Seq[Schema.Field[_]],
      data: ListMap[String, _]
    ) = {
      fieldNumber.foreach(num =>
        p.writeFieldBegin(
          new org.apache.thrift.protocol.TField("", TType.STRUCT, num)
        )
      )
      writeStructure(structure.map(schema =>
        (schema,
          if(isOptional(schema.schema)) data.get(schema.label) else data.get(schema.label).get
        )))
      fieldNumber.foreach(_ => p.writeFieldEnd()
      )
    }
  }

  class Decoder(chunk: Chunk[Byte]) {
    type Path = Chunk[String]
    case class Error(path: Path, error: String)
    type Result[A] = Either[Error, A]
    type PrimitiveResult[A] = Path => Result[A]

    val read = new ChunkTransport.Read(chunk)
    val p = new TBinaryProtocol(read)

    def succeed[A](a: => A): Result[A] = Right(a)

    def fail(path: Path, failure: String): Result[Nothing] = Left(Error(path, failure))

    def decodePrimitive[A](f: TProtocol => A, name: String): PrimitiveResult[A] =
      path => Try {
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
      decodePrimitive(p =>
        Try(p.readI64()).map(_.intValue)
          .orElse(
            Try(p.readI32())
          ).orElse(
          Try(p.readI16()).map(_.intValue)
        ).orElse(
          Try(p.readByte()).map(_.intValue)
        ).get
        , "VarInt")

    def decodeLong: PrimitiveResult[Long] =
      decodePrimitive(_.readI64(), "Long")

    def decodeFloat: PrimitiveResult[Float] =
      decodePrimitive(_.readDouble().toFloat, "Float")

    def decodeDouble: PrimitiveResult[Double] =
      decodePrimitive(_.readDouble(), "Double")

    def decodeBinary: PrimitiveResult[Chunk[Byte]] =
      decodePrimitive(p => Chunk.fromByteBuffer(p.readBinary()), "Binary")

    //    def optionalDecoder[A](scheme: Schema[A]): Decoder[Option[A]] =


    def decode[A](schema: Schema[A], path: Path): Result[A] =
      (schema match {
        case Schema.GenericRecord(structure, _) => {
          val fields = structure.toChunk
          decodeRecord(fields, path).map(
            _.map{ case (index, value) => (fields(index -1).label, value) }
          )
        }
        case seqSchema@Schema.Sequence(_, _, _, _) => decodeSequence(seqSchema, path)
        case setSchema@Schema.SetSchema(_, _) => decodeSet(setSchema, path)
        case Schema.Transform(codec, f, _, _) => transformDecoder(path, codec, f)
        case Schema.Primitive(standardType, _) => primitiveDecoder(standardType, path)
        case Schema.Tuple(left, right, _) => tupleDecoder(left, right, path)
        // FIXME what if is missing
        case Schema.Optional(codec, _) => decode(codec, path).map(Some(_))
        //        case Schema.Fail(message)             => fail(message)
        case Schema.EitherSchema(left, right, _) => eitherDecoder(path, left, right)
        case lzy@Schema.Lazy(_) => decode(lzy.schema, path)
        //        case Schema.Meta(_)                   => astDecoder
        case ProductDecoder(decoder) => decoder(path)
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
        case _ => ???
      })
//        .map(
//        FIXME
//        x => {println(s"decoded: $x"); x}
//      )

    private def enumDecoder[Z, A](path: Path, cases: Schema.Case[_, Z]*): Result[Z] =
      Try {
        p.readStructBegin()
        val readField = p.readFieldBegin()
        if(readField.id > cases.length)
          fail(path, s"Error decoding enum with cases ${cases.map(_.id).mkString(", ")}, enum id out of range: ${readField.id}")
        else {
          val subtypeCase = cases(readField.id - 1)
          val res = decode(subtypeCase.codec, path.appended(s"[case:${subtypeCase.id}]"))
          res.foreach { _ =>
            p.readFieldEnd()
            p.readFieldBegin()
            p.readStructEnd()
          }
          res.asInstanceOf[Result[Z]]
        }
      }.fold(err =>
        fail(path, s"Error decoding enum with cases ${cases.map(_.id).mkString(", ")} ${err.getMessage}")
      ,
          identity
      )

    private def eitherDecoder[A, B](path: Path, left: Schema[A], right: Schema[B]): Result[Either[A, B]] = {
      p.readStructBegin()
      val readField = p.readFieldBegin()
      val res = readField.id match {
        case 1 => decode(left, path.appended("either:left")).map(Left(_))
        case 2 => decode(right, path.appended("either:right")).map(Right(_))
        case _ => fail(path, "Failed to decode either.")
      }
      res.foreach { _ =>
        p.readFieldEnd()
        p.readStructEnd()
      }
      res
    }

    private def tupleDecoder[A, B](left: Schema[A], right: Schema[B], path: Path): Result[(A, B)] =
      StructDecoder(Seq(left, right), path)
        .flatMap(
          record =>
            (record.get(1), record.get(2)) match {
              case (Some(first), Some(second)) => Right((first.asInstanceOf[A], second.asInstanceOf[B]))
              case _ => fail(path, "Error while decoding tuple.")
            }
        )

    private def transformDecoder[A, B](path: Path, schema: Schema[B], f: B => Either[String, A]): Result[A] =
      decode(schema, path).flatMap(a => f(a).left.map(msg => Error(path, msg)))

    private def primitiveDecoder[A](standardType: StandardType[A], path: Path): Result[A] =
      standardType match {
        case StandardType.UnitType   => Right(())
        case StandardType.StringType => decodeString(path)
        case StandardType.BoolType => decodeBoolean(path)
        case StandardType.ShortType => decodeShort(path)
        case StandardType.IntType => decodeInt(path)
        case StandardType.LongType => decodeLong(path)
        case StandardType.FloatType => decodeFloat(path)
        case StandardType.DoubleType => decodeDouble(path)
        case StandardType.BinaryType => decodeBinary(path)
        case StandardType.CharType => decodeString(path).map(_.charAt(0))
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
          decodeRecord(monthDayStructure(), path)
            .map(
              data =>
                MonthDay.of(data.getOrElse(1, 0).asInstanceOf[Int], data.getOrElse(2, 0).asInstanceOf[Int])
            )
        case StandardType.PeriodType =>
          decodeRecord(periodStructure(), path)
            .map(
              data =>
                Period.of(
                  data.getOrElse(1, 0).asInstanceOf[Int],
                  data.getOrElse(2, 0).asInstanceOf[Int],
                  data.getOrElse(3, 0).asInstanceOf[Int]
                )
            )
        case StandardType.YearType =>
          decodeVarInt(path).map(_.intValue).map(Year.of)
        case StandardType.YearMonthType =>
          decodeRecord(yearMonthStructure(), path)
            .map(
              data =>
                YearMonth.of(data.getOrElse(1, 0).asInstanceOf[Int], data.getOrElse(2, 0).asInstanceOf[Int])
            )
        case StandardType.ZoneIdType => decodeString(path).map(ZoneId.of)
        case StandardType.ZoneOffsetType =>
          decodeVarInt(path)
            .map(_.intValue)
            .map(ZoneOffset.ofTotalSeconds)
        case StandardType.Duration(_) =>
          decodeRecord(durationStructure(), path)
            .map(
              data =>
                Duration.ofSeconds(
                  data.getOrElse(1, 0L).asInstanceOf[Long],
                  data.getOrElse(2, 0L).asInstanceOf[Int].toLong
                )
            )
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
      case Schema.Lazy(s) => emptyValue(s())
      case Schema.Optional(_, _) => Some(None)
      case Schema.Sequence(_, fromChunk, _, _) => Some(fromChunk(Chunk.empty))
      case Schema.Primitive(StandardType.UnitType, _) => Some(())
      case _ => None
    }

    private def decodeRecord(fields: Seq[Schema.Field[_]], path: Path): Result[ListMap[Short, _]] =
      StructDecoder(fields.map(_.schema), path)

    def StructDecoder(fields: Seq[Schema[_]], path: Path): Result[ListMap[Short, Any]] =  Try {
      p.readStructBegin()
      //FIXME return Left on error
      var values = ListMap.empty[Short, Any]
      var readField = p.readFieldBegin()
      while (readField.`type` != TType.STOP) {
        val actualPath = path.appended(s"fieldId:${readField.id}")
        def safeRead[A](f: TProtocol => A, name: String) =
          Try {
            f(p)
          }.toEither.left.map(_ => Error(actualPath, s"Unable to decode $name"))
          val value = readField.`type` match {
            case TType.BOOL => safeRead(_.readBool(), "Bool")
            case TType.BYTE => safeRead(_.readByte(), "Byte")
            case TType.DOUBLE => safeRead(_.readDouble(), "Double")
            case TType.I16 => safeRead(_.readI16(), "Short")
            case TType.I32 => safeRead(_.readI32(), "Int")
            case TType.I64 => safeRead(_.readI64(), "Long")
            case TType.STRING => safeRead(_.readString(), "String")
            //FIXME
            case TType.STRUCT => decode(fields(readField.id -1), actualPath)
            case TType.MAP => ???
            case TType.SET => ???
            //FIXME
            case TType.LIST => decodeSequence(unwrapLazy(fields(readField.id - 1)).asInstanceOf[Schema.Sequence[_, _]], actualPath)
            case TType.ENUM => ???
          }
          // FIXME
          value match {
            case v@Left(_) => return v.asInstanceOf[Result[ListMap[Short, Any]]]
            case Right(v) => values = values.updated(readField.id, v)
          }
        p.readFieldEnd()
        readField = p.readFieldBegin()
      }
      p.readStructEnd()
      Right(values)
    }.getOrElse(Left(Error(path, "Error reading struct.")))

    def decodeSequence[Col, Elem](schema: Schema.Sequence[Col, Elem], path: Path): Result[Col] = {
      val listBegin = p.readListBegin()
      val cb = ChunkBuilder.make[Elem]()
      var numberToDecode = listBegin.size
      while (numberToDecode > 0) {
        val res = decode(schema.schemaA, path)
        // FIXME
        cb.addOne(res.right.get)
        numberToDecode = numberToDecode - 1
      }
      p.readListEnd()
      val res = schema.fromChunk(cb.result())
      // FIXME
      Right(res)
    }

    def decodeSet[A](schema: Schema.SetSchema[A], path: Path): Result[Set[A]] = {
      val setBegin = p.readSetBegin()
      val cb = ChunkBuilder.make[A]()
      var numberToDecode = setBegin.size
      while (numberToDecode > 0) {
        val res = decode(schema.as, path)
        // FIXME
        cb.addOne(res.right.get)
        numberToDecode = numberToDecode - 1
      }
      val res = cb.result().toSet
      // FIXME
      Right(res)
    }


    private[codec] object ProductDecoder {

      def unapply[A](schema: Schema[A]): Option[Path => Result[A]] = schema match {
        case s: Schema.CaseClass1[_, A] => Some(caseClass1Decoder(s))
        case s: Schema.CaseClass2[_, _, A] => Some(caseClass2Decoder(s))
        case s: Schema.CaseClass3[_, _, _, A] => Some(caseClass3Decoder(s))
        case s: Schema.CaseClass4[_, _, _, _, A] => Some(caseClass4Decoder(s))
        case s: Schema.CaseClass5[_, _, _, _, _, A] => Some(caseClass5Decoder(s))
        case s: Schema.CaseClass6[_, _, _, _, _, _, A] => Some(caseClass6Decoder(s))
        case s: Schema.CaseClass7[_, _, _, _, _, _, _, A] => Some(caseClass7Decoder(s))
        case s: Schema.CaseClass8[_, _, _, _, _, _, _, _, A] => Some(caseClass8Decoder(s))
        case s: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, A] => Some(caseClass9Decoder(s))
        case s: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, A] => Some(caseClass10Decoder(s))
        case s: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, A] => Some(caseClass11Decoder(s))
        case s: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, A] => Some(caseClass12Decoder(s))
        case s: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, A] => Some(caseClass13Decoder(s))
        case s: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, A] => Some(caseClass14Decoder(s))
        case s: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] => Some(caseClass15Decoder(s))
        case s: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] => Some(caseClass16Decoder(s))
        case s: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] => Some(caseClass17Decoder(s))
        case s: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] => Some(caseClass18Decoder(s))
        case s: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] => Some(caseClass19Decoder(s))
        case s: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
          Some(caseClass20Decoder(s))
        case s: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
          Some(caseClass21Decoder(s))
        case s: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
          Some(caseClass22Decoder(s))
        case _ => None
      }

      private def unsafeDecodeFields(path: Path, buffer: Array[Any], fields: Schema.Field[_]*): Result[Array[Any]] =
        StructDecoder(fields.map(_.schema), path).flatMap {
          values =>
            fields.zipWithIndex.foreach {
              case (Schema.Field(_, schema, _), index) =>
                val rawValue = values.get((index + 1).toShort)
                val value =
                  if (isOptional(schema))
                    rawValue
                  else
                    rawValue.fold {
                      val x = emptyValue(schema)
                      x.get
                    }(identity)
                // FIXME no get
                buffer.update(index, value)
            }
            Right(buffer)
        }

      @tailrec
      private def validateBuffer(index: Int, buffer: Array[Any], path: Path): Result[Array[Any]] =
        if (index == buffer.length - 1 && buffer(index) != null)
          succeed(buffer)
        else if (buffer(index) == null)
          fail(path, s"Missing field number $index.")
        else
          validateBuffer(index + 1, buffer, path)

      private def caseClass1Decoder[A, Z](schema: Schema.CaseClass1[A, Z])(path: Path): Result[Z] =
        unsafeDecodeFields(path, Array.ofDim[Any](1), schema.field).flatMap { buffer =>
          if (buffer(0) == null)
            fail(path,"Missing field 1.")
          else
            succeed(schema.construct(buffer(0).asInstanceOf[A]))
        }

      private def caseClass2Decoder[A1, A2, Z](schema: Schema.CaseClass2[A1, A2, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](2), schema.field1, schema.field2)
          _ <- validateBuffer(0, buffer, path)
        } yield
          schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2])

      private def caseClass3Decoder[A1, A2, A3, Z](schema: Schema.CaseClass3[A1, A2, A3, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](3), schema.field1, schema.field2, schema.field3)
          _ <- validateBuffer(0, buffer, path)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3])

      private def caseClass4Decoder[A1, A2, A3, A4, Z](schema: Schema.CaseClass4[A1, A2, A3, A4, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](4), schema.field1, schema.field2, schema.field3, schema.field4)
          _ <- validateBuffer(0, buffer, path)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4])

      private def caseClass5Decoder[A1, A2, A3, A4, A5, Z](schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](5), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5)
          _ <- validateBuffer(0, buffer, path)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5])

      private def caseClass6Decoder[A1, A2, A3, A4, A5, A6, Z](schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](6), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6)
          _ <- validateBuffer(0, buffer, path)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6])

      private def caseClass7Decoder[A1, A2, A3, A4, A5, A6, A7, Z](schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](7), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7)
          _ <- validateBuffer(0, buffer, path)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7])

      private def caseClass8Decoder[A1, A2, A3, A4, A5, A6, A7, A8, Z](schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](8), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8)
          _ <- validateBuffer(0, buffer, path)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8])

      private def caseClass9Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](9), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9)
          _ <- validateBuffer(0, buffer, path)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9])

      private def caseClass10Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](10), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10)
          _ <- validateBuffer(0, buffer, path)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10])

      private def caseClass11Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](11), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11)
          _ <- validateBuffer(0, buffer, path)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11])

      private def caseClass12Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](12), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12)
          _ <- validateBuffer(0, buffer, path)
        } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12])

      private def caseClass13Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z])(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](13), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13)
          _ <- validateBuffer(0, buffer, path)
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
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](14), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14)
          _ <- validateBuffer(0, buffer, path)
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
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](15), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15)
          _ <- validateBuffer(0, buffer, path)
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
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](16), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16)
          _ <- validateBuffer(0, buffer, path)
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
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](17), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17)
          _ <- validateBuffer(0, buffer, path)
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
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](18), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18)
          _ <- validateBuffer(0, buffer, path)
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
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](19), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19)
          _ <- validateBuffer(0, buffer, path)
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
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](20), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20)
          _ <- validateBuffer(0, buffer, path)
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
          buffer <- unsafeDecodeFields(path, Array.ofDim[Any](21), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20, schema.field21)
          _ <- validateBuffer(0, buffer, path)
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
          buffer <- unsafeDecodeFields(
            path,
            Array.ofDim[Any](22),
            schema.field1,
            schema.field2,
            schema.field3,
            schema.field4,
            schema.field5,
            schema.field6,
            schema.field7,
            schema.field9,
            schema.field9,
            schema.field10,
            schema.field11,
            schema.field12,
            schema.field13,
            schema.field14,
            schema.field15,
            schema.field16,
            schema.field17,
            schema.field18,
            schema.field19,
            schema.field20,
            schema.field21,
            schema.field22
          )
          _ <- validateBuffer(0, buffer, path)
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
