package zio.schema.codec

import java.math.{ BigInteger, MathContext }
import java.nio.charset.StandardCharsets
import java.nio.{ ByteBuffer, ByteOrder }
import java.time._
import java.util.UUID

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.util.control.NonFatal

import zio.schema._
import zio.schema.codec.BinaryCodec._
import zio.schema.codec.ProtobufCodec.Protobuf.WireType.LengthDelimited
import zio.stream.ZPipeline
import zio.{ Chunk, ZIO }

object ProtobufCodec extends BinaryCodec {

  override def encoderFor[A](schema: Schema[A]): BinaryEncoder[A] =
    new BinaryEncoder[A] {

      override def encode(value: A): Chunk[Byte] =
        ProtobufEncoder.encode(None, schema, value)

      override def streamEncoder: BinaryStreamEncoder[A] =
        ZPipeline.mapChunks(
          _.flatMap(encode)
        )

    }

  override def decoderFor[A](schema: Schema[A]): BinaryDecoder[A] =
    new BinaryDecoder[A] {

      override def decode(chunk: Chunk[Byte]): Either[String, A] =
        ProtobufDecoder.decode(schema, chunk)

      override def streamDecoder: BinaryStreamDecoder[A] =
        ZPipeline.mapChunksZIO(
          chunk =>
            ZIO.fromEither(
              decode(chunk).map(Chunk(_))
            )
        )

    }

  object Protobuf {

    sealed trait WireType

    object WireType {
      case object VarInt                     extends WireType
      case object Bit64                      extends WireType
      case class LengthDelimited(width: Int) extends WireType
      case object StartGroup                 extends WireType
      case object EndGroup                   extends WireType
      case object Bit32                      extends WireType
    }

    private[codec] val bigDecimalStructure: Seq[Schema.Field[java.math.BigDecimal, _]] =
      Seq(
        Schema.Field(
          "unscaled",
          Schema.Primitive(StandardType.BigIntegerType),
          get = _.unscaledValue,
          set = (a, b: BigInteger) => new java.math.BigDecimal(b, a.scale)
        ),
        Schema
          .Field(
            "precision",
            Schema.Primitive(StandardType.IntType),
            get = _.precision(),
            set = (a, b: Int) => new java.math.BigDecimal(a.unscaledValue, new MathContext(b))
          ),
        Schema
          .Field("scale", Schema.Primitive(StandardType.IntType), get = _.scale(), set = (a, b: Int) => a.setScale(b))
      )

    private[codec] val monthDayStructure: Seq[Schema.Field[MonthDay, Int]] =
      Seq(
        Schema.Field(
          "month",
          Schema.Primitive(StandardType.IntType),
          get = _.getMonthValue,
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

    private[codec] val periodStructure: Seq[Schema.Field[Period, Int]] = Seq(
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

    private[codec] val yearMonthStructure: Seq[Schema.Field[YearMonth, Int]] =
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

    private[codec] val durationStructure: Seq[Schema.Field[Duration, _]] =
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

    /**
     * Used when encoding sequence of values to decide whether each value need its own key or values can be packed together without keys (for example numbers).
     */
    @scala.annotation.tailrec
    private[codec] def canBePacked(schema: Schema[_]): Boolean = schema match {
      case Schema.Sequence(element, _, _, _, _) => canBePacked(element)
      case Schema.Transform(codec, _, _, _, _)  => canBePacked(codec)
      case Schema.Primitive(standardType, _)    => canBePacked(standardType)
      case _: Schema.Tuple2[_, _]               => false
      case _: Schema.Optional[_]                => false
      case _: Schema.Fail[_]                    => false
      case _: Schema.Either[_, _]               => false
      case lzy @ Schema.Lazy(_)                 => canBePacked(lzy.schema)
      case _                                    => false
    }

    private def canBePacked(standardType: StandardType[_]): Boolean = standardType match {
      case StandardType.UnitType              => false
      case StandardType.StringType            => false
      case StandardType.BoolType              => true
      case StandardType.ByteType              => true
      case StandardType.ShortType             => true
      case StandardType.IntType               => true
      case StandardType.LongType              => true
      case StandardType.FloatType             => true
      case StandardType.DoubleType            => true
      case StandardType.BinaryType            => false
      case StandardType.CharType              => true
      case StandardType.BigIntegerType        => false
      case StandardType.BigDecimalType        => false
      case StandardType.UUIDType              => false
      case StandardType.DayOfWeekType         => true
      case StandardType.MonthType             => true
      case StandardType.MonthDayType          => false
      case StandardType.PeriodType            => false
      case StandardType.YearType              => true
      case StandardType.YearMonthType         => false
      case StandardType.ZoneIdType            => false
      case StandardType.ZoneOffsetType        => true
      case StandardType.DurationType          => true
      case StandardType.InstantType(_)        => false
      case StandardType.LocalDateType(_)      => false
      case StandardType.LocalTimeType(_)      => false
      case StandardType.LocalDateTimeType(_)  => false
      case StandardType.OffsetTimeType(_)     => false
      case StandardType.OffsetDateTimeType(_) => false
      case StandardType.ZonedDateTimeType(_)  => false
    }
  }

  object ProtobufEncoder {

    import ProductEncoder._
    import Protobuf._

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    def encode[A](fieldNumber: Option[Int], schema: Schema[A], value: A): Chunk[Byte] =
      (schema, value) match {
        case (Schema.GenericRecord(_, structure, _), v: Map[String, _]) =>
          encodeRecord(fieldNumber, structure.toChunk, v)
        case (Schema.Sequence(element, _, g, _, _), v)                                         => encodeSequence(fieldNumber, element, g(v))
        case (Schema.Map(ks, vs, _), map)                                                      => encodeSequence(fieldNumber, ks <*> vs, Chunk.fromIterable(map))
        case (Schema.Set(s, _), set)                                                           => encodeSequence(fieldNumber, s, Chunk.fromIterable(set))
        case (Schema.Transform(codec, _, g, _, _), _)                                          => g(value).map(encode(fieldNumber, codec, _)).getOrElse(Chunk.empty)
        case (Schema.Primitive(standardType, _), v)                                            => encodePrimitive(fieldNumber, standardType, v)
        case (Schema.Tuple2(left, right, _), v @ (_, _))                                       => encodeTuple(fieldNumber, left, right, v)
        case (Schema.Optional(codec: Schema[a], _), v: Option[_])                              => encodeOptional(fieldNumber, codec, v.asInstanceOf[Option[a]])
        case (Schema.Either(left: Schema[a], right: Schema[b], _), v: scala.util.Either[_, _]) => encodeEither(fieldNumber, left, right, v.asInstanceOf[scala.util.Either[a, b]])
        case (lzy @ Schema.Lazy(_), v)                                                         => encode(fieldNumber, lzy.schema, v)
        case (_: Schema.CaseClass0[_], v) =>
          encodeCaseClass(v)(fieldNumber)
        case (cc: Schema.CaseClass1[_, _], v) =>
          encodeCaseClass(v, cc.field)(fieldNumber)
        case (cc: Schema.CaseClass2[_, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2)(fieldNumber)
        case (cc: Schema.CaseClass3[_, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3)(fieldNumber)
        case (cc: Schema.CaseClass4[_, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4)(fieldNumber)
        case (cc: Schema.CaseClass5[_, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5)(fieldNumber)
        case (cc: Schema.CaseClass6[_, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6)(fieldNumber)
        case (cc: Schema.CaseClass7[_, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7)(fieldNumber)
        case (cc: Schema.CaseClass8[_, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8)(fieldNumber)
        case (cc: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9)(fieldNumber)
        case (cc: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10)(fieldNumber)
        case (cc: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10, cc.field11)(fieldNumber)
        case (cc: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10, cc.field11, cc.field12)(fieldNumber)
        case (cc: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10, cc.field11, cc.field12, cc.field13)(fieldNumber)
        case (cc: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10, cc.field11, cc.field12, cc.field13, cc.field14)(fieldNumber)
        case (cc: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10, cc.field11, cc.field12, cc.field13, cc.field14, cc.field15)(fieldNumber)
        case (cc: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10, cc.field11, cc.field12, cc.field13, cc.field14, cc.field15, cc.field16)(fieldNumber)
        case (cc: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10, cc.field11, cc.field12, cc.field13, cc.field14, cc.field15, cc.field16, cc.field17)(fieldNumber)
        case (cc: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10, cc.field11, cc.field12, cc.field13, cc.field14, cc.field15, cc.field16, cc.field17, cc.field18)(fieldNumber)
        case (cc: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10, cc.field11, cc.field12, cc.field13, cc.field14, cc.field15, cc.field16, cc.field17, cc.field18, cc.field19)(fieldNumber)
        case (cc: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10, cc.field11, cc.field12, cc.field13, cc.field14, cc.field15, cc.field16, cc.field17, cc.field18, cc.field19, cc.field20)(fieldNumber)
        case (cc: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10, cc.field11, cc.field12, cc.field13, cc.field14, cc.field15, cc.field16, cc.field17, cc.field18, cc.field19, cc.field20, cc.field21)(fieldNumber)
        case (cc: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _], v) =>
          encodeCaseClass(v, cc.field1, cc.field2, cc.field3, cc.field4, cc.field5, cc.field6, cc.field7, cc.field8, cc.field9, cc.field10, cc.field11, cc.field12, cc.field13, cc.field14, cc.field15, cc.field16, cc.field17, cc.field18, cc.field19, cc.field20, cc.field21, cc.field22)(fieldNumber)
        case (Schema.Enum1(_, c, _), v)                                                                                                    => encodeEnum(fieldNumber, v, c)
        case (Schema.Enum2(_, c1, c2, _), v)                                                                                               => encodeEnum(fieldNumber, v, c1, c2)
        case (Schema.Enum3(_, c1, c2, c3, _), v)                                                                                           => encodeEnum(fieldNumber, v, c1, c2, c3)
        case (Schema.Enum4(_, c1, c2, c3, c4, _), v)                                                                                       => encodeEnum(fieldNumber, v, c1, c2, c3, c4)
        case (Schema.Enum5(_, c1, c2, c3, c4, c5, _), v)                                                                                   => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5)
        case (Schema.Enum6(_, c1, c2, c3, c4, c5, c6, _), v)                                                                               => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6)
        case (Schema.Enum7(_, c1, c2, c3, c4, c5, c6, c7, _), v)                                                                           => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7)
        case (Schema.Enum8(_, c1, c2, c3, c4, c5, c6, c7, c8, _), v)                                                                       => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8)
        case (Schema.Enum9(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, _), v)                                                                   => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9)
        case (Schema.Enum10(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _), v)                                                             => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
        case (Schema.Enum11(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _), v)                                                        => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
        case (Schema.Enum12(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _), v)                                                   => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
        case (Schema.Enum13(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _), v)                                              => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
        case (Schema.Enum14(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _), v)                                         => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
        case (Schema.Enum15(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _), v)                                    => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
        case (Schema.Enum16(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _), v)                               => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
        case (Schema.Enum17(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _), v)                          => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
        case (Schema.Enum18(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _), v)                     => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
        case (Schema.Enum19(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _), v)                => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
        case (Schema.Enum20(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _), v)           => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
        case (Schema.Enum21(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _), v)      => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
        case (Schema.Enum22(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _), v) => encodeEnum(fieldNumber, v, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
        case (Schema.EnumN(_, cs, _), v)                                                                                                   => encodeEnum(fieldNumber, v, cs.toSeq: _*)
        case (Schema.Dynamic(_), v)                                                                                                        => dynamicEncode(fieldNumber, DynamicValueSchema.schema, v)
        case (_, _)                                                                                                                        => Chunk.empty
      }
    //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

    private def dynamicEncode(
      fieldNumber: Option[Int],
      schema: Schema[DynamicValue],
      value: DynamicValue
    ): Chunk[Byte] =
      encode(fieldNumber, schema, value)

    private def encodeEnum[Z](fieldNumber: Option[Int], value: Z, cases: Schema.Case[Z, _]*): Chunk[Byte] = {
      val fieldIndex = cases.indexWhere(c => c.deconstructOption(value).isDefined)
      val encoded = Chunk.fromIterable(
        if (fieldIndex == -1) {
          Chunk.empty
        } else {
          val subtypeCase = cases(fieldIndex)
          encode(
            Some(fieldIndex + 1),
            subtypeCase.schema.asInstanceOf[Schema[Any]],
            subtypeCase.deconstruct(value)
          )
        }
      )
      encodeKey(WireType.LengthDelimited(encoded.size), fieldNumber) ++ encoded
    }

    private def encodeRecord(
      fieldNumber: Option[Int],
      structure: Seq[Schema.Field[_, _]],
      data: ListMap[String, _]
    ): Chunk[Byte] = {
      val encodedRecord = Chunk
        .fromIterable(structure.zipWithIndex.map {
          case (Schema.Field(label, schema, _, _, _, _), fieldNumber) =>
            data
              .get(label)
              .map(value => encode(Some(fieldNumber + 1), schema.asInstanceOf[Schema[Any]], value))
              .getOrElse(Chunk.empty)
        })
        .flatten

      encodeKey(WireType.LengthDelimited(encodedRecord.size), fieldNumber) ++ encodedRecord
    }

    /**
     * Lists of lists are not really a type that is "native" to protobuf so
     * we have to encode lists as enums in order to distinguish between List.empty,
     * List(List.empty) and so forth.
     *
     * This adds a few extra bytes to the encoding but it does not seem like there
     * is any way around it.
     */
    private def encodeSequence[A](
      fieldNumber: Option[Int],
      element: Schema[A],
      sequence: Chunk[A]
    ): Chunk[Byte] =
      if (sequence.isEmpty) {
        val data = encodeKey(WireType.LengthDelimited(0), Some(1))
        encodeKey(WireType.LengthDelimited(data.size), fieldNumber) ++ encodeKey(WireType.LengthDelimited(0), Some(1))
      } else {
        val data = if (canBePacked(element)) {
          val chunk = sequence.flatMap(value => encode(None, element, value))
          encodeKey(WireType.LengthDelimited(chunk.size), Some(2)) ++ chunk
        } else {
          val chunk = sequence.zipWithIndexFrom(1).flatMap {
            case (a, i) => encode(Some(i), element, a)
          }
          encodeKey(WireType.LengthDelimited(chunk.size), Some(2)) ++ chunk
        }

        encodeKey(WireType.LengthDelimited(data.size), fieldNumber) ++ data
      }

    @scala.annotation.tailrec
    private def encodePrimitive[A](
      fieldNumber: Option[Int],
      standardType: StandardType[A],
      value: A
    ): Chunk[Byte] =
      (standardType, value) match {
        case (StandardType.UnitType, _) =>
          encodeKey(WireType.LengthDelimited(0), fieldNumber)
        case (StandardType.StringType, str: String) =>
          val encoded = Chunk.fromArray(str.getBytes(StandardCharsets.UTF_8))
          encodeKey(WireType.LengthDelimited(encoded.size), fieldNumber) ++ encoded
        case (StandardType.BoolType, b: Boolean) =>
          encodeKey(WireType.VarInt, fieldNumber) ++ encodeVarInt(if (b) 1 else 0)
        case (StandardType.ShortType, v: Short) =>
          encodeKey(WireType.VarInt, fieldNumber) ++ encodeVarInt(v.toLong)
        case (StandardType.ByteType, v: Byte) =>
          encodeKey(WireType.VarInt, fieldNumber) ++ encodeVarInt(v.toLong)
        case (StandardType.IntType, v: Int) =>
          encodeKey(WireType.VarInt, fieldNumber) ++ encodeVarInt(v)
        case (StandardType.LongType, v: Long) =>
          encodeKey(WireType.VarInt, fieldNumber) ++ encodeVarInt(v)
        case (StandardType.BigDecimalType, v: java.math.BigDecimal) =>
          val unscaled  = v.unscaledValue()
          val precision = v.precision()
          val scale     = v.scale()
          encodeRecord(
            fieldNumber,
            bigDecimalStructure,
            ListMap("unscaled" -> unscaled, "precision" -> precision, "scale" -> scale)
          )
        case (StandardType.BigIntegerType, v: java.math.BigInteger) =>
          val encoded = Chunk.fromArray(v.toByteArray)
          encodeKey(WireType.LengthDelimited(encoded.size), fieldNumber) ++ encoded

        case (StandardType.FloatType, v: Float) =>
          val byteBuffer = ByteBuffer.allocate(4)
          byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
          byteBuffer.putFloat(v)
          encodeKey(WireType.Bit32, fieldNumber) ++ Chunk.fromArray(byteBuffer.array)
        case (StandardType.DoubleType, v: Double) =>
          val byteBuffer = ByteBuffer.allocate(8)
          byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
          byteBuffer.putDouble(v)
          encodeKey(WireType.Bit64, fieldNumber) ++ Chunk.fromArray(byteBuffer.array)
        case (StandardType.BinaryType, bytes: Chunk[Byte]) =>
          encodeKey(WireType.LengthDelimited(bytes.length), fieldNumber) ++ bytes
        case (StandardType.CharType, c: Char) =>
          encodePrimitive(fieldNumber, StandardType.StringType, c.toString)
        case (StandardType.UUIDType, u: UUID) =>
          encodePrimitive(fieldNumber, StandardType.StringType, u.toString)
        case (StandardType.DayOfWeekType, v: DayOfWeek) =>
          encodePrimitive(fieldNumber, StandardType.IntType, v.getValue)
        case (StandardType.MonthType, v: Month) =>
          encodePrimitive(fieldNumber, StandardType.IntType, v.getValue)
        case (StandardType.MonthDayType, v: MonthDay) =>
          encodeRecord(fieldNumber, monthDayStructure, ListMap("month" -> v.getMonthValue, "day" -> v.getDayOfMonth))
        case (StandardType.PeriodType, v: Period) =>
          encodeRecord(
            fieldNumber,
            periodStructure,
            ListMap("years" -> v.getYears, "months" -> v.getMonths, "days" -> v.getDays)
          )
        case (StandardType.YearType, v: Year) =>
          encodePrimitive(fieldNumber, StandardType.IntType, v.getValue)
        case (StandardType.YearMonthType, v: YearMonth) =>
          encodeRecord(fieldNumber, yearMonthStructure, ListMap("year" -> v.getYear, "month" -> v.getMonthValue))
        case (StandardType.ZoneIdType, v: ZoneId) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.getId)
        case (StandardType.ZoneOffsetType, v: ZoneOffset) =>
          encodePrimitive(fieldNumber, StandardType.IntType, v.getTotalSeconds)
        case (StandardType.DurationType, v: Duration) =>
          encodeRecord(fieldNumber, durationStructure, ListMap("seconds" -> v.getSeconds, "nanos" -> v.getNano))
        case (StandardType.InstantType(formatter), v: Instant) =>
          encodePrimitive(fieldNumber, StandardType.StringType, formatter.format(v))
        case (StandardType.LocalDateType(formatter), v: LocalDate) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.format(formatter))
        case (StandardType.LocalTimeType(formatter), v: LocalTime) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.format(formatter))
        case (StandardType.LocalDateTimeType(formatter), v: LocalDateTime) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.format(formatter))
        case (StandardType.OffsetTimeType(formatter), v: OffsetTime) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.format(formatter))
        case (StandardType.OffsetDateTimeType(formatter), v: OffsetDateTime) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.format(formatter))
        case (StandardType.ZonedDateTimeType(formatter), v: ZonedDateTime) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.format(formatter))
        case (_, _) =>
          throw new NotImplementedError(s"No encoder for $standardType")
      }

    private def encodeTuple[A, B](
      fieldNumber: Option[Int],
      left: Schema[A],
      right: Schema[B],
      tuple: (A, B)
    ): Chunk[Byte] = {
      val data = encode(Some(1), left, tuple._1) ++ encode(Some(2), right, tuple._2)

      encodeKey(WireType.LengthDelimited(data.size), fieldNumber) ++ data
    }

    private def encodeEither[A, B](
      fieldNumber: Option[Int],
      left: Schema[A],
      right: Schema[B],
      either: scala.util.Either[A, B]
    ): Chunk[Byte] = {
      val encodedEither = either match {
        case Left(value)  => encode(Some(1), left, value)
        case Right(value) => encode(Some(2), right, value)
      }

      encodeKey(WireType.LengthDelimited(encodedEither.size), fieldNumber) ++ encodedEither
    }

    private def encodeOptional[A](fieldNumber: Option[Int], schema: Schema[A], value: Option[A]): Chunk[Byte] = {
      val data = value match {
        case Some(a) => encode(Some(2), schema, a)
        case None    => encodeKey(WireType.LengthDelimited(0), Some(1))
      }

      encodeKey(WireType.LengthDelimited(data.size), fieldNumber) ++ data
    }

    private def encodeVarInt(value: Int): Chunk[Byte] =
      encodeVarInt(value.toLong)

    private def encodeVarInt(value: Long): Chunk[Byte] = {
      val base128    = value & 0x7F
      val higherBits = value >>> 7

      if (higherBits != 0x00) {
        (0x80 | base128).byteValue() +: encodeVarInt(higherBits)
      } else {
        Chunk(base128.byteValue())
      }
    }

    /**
     * Encodes key. Key contains field number out of flatten schema structure and wire type.
     * 1 << 3 => 8, 2 << 3 => 16, 3 << 3 => 24
     *
     * More info:
     * https://developers.google.com/protocol-buffers/docs/encoding#structure
     */
    private[codec] def encodeKey(wireType: WireType, fieldNumber: Option[Int]): Chunk[Byte] =
      fieldNumber.map { fieldNumber =>
        val encode = (baseWireType: Int) => encodeVarInt(fieldNumber << 3 | baseWireType)
        wireType match {
          case WireType.VarInt                  => encode(0)
          case WireType.Bit64                   => encode(1)
          case WireType.LengthDelimited(length) => encode(2) ++ encodeVarInt(length)
          case WireType.StartGroup              => encode(3)
          case WireType.EndGroup                => encode(4)
          case WireType.Bit32                   => encode(5)
        }
      }.getOrElse(Chunk.empty)
  }

  final case class ProtobufDecoder[+A](run: Chunk[Byte] => scala.util.Either[String, (Chunk[Byte], A)]) {
    self =>

    def map[B](f: A => B): ProtobufDecoder[B] =
      ProtobufDecoder { bytes =>
        self.run(bytes).map {
          case (remainder, a) =>
            (remainder, f(a))
        }
      }

    def flatMap[B](f: A => ProtobufDecoder[B]): ProtobufDecoder[B] =
      ProtobufDecoder { bytes =>
        self.run(bytes).flatMap {
          case (remainder, a) =>
            f(a).run(remainder)
        }
      }

    def loop: ProtobufDecoder[Chunk[A]] =
      self.flatMap(
        a0 =>
          ProtobufDecoder(bytes => {
            if (bytes.isEmpty) {
              Right((bytes, Chunk(a0)))
            } else {
              loop.run(bytes) match {
                case Left(value)           => Left(value)
                case Right((remainder, a)) => Right((remainder, Chunk(a0) ++ a))
              }
            }
          })
      )

    def take(n: Int): ProtobufDecoder[A] =
      ProtobufDecoder(bytes => {
        val (before, after) = bytes.splitAt(n)
        self.run(before) match {
          case Left(value)   => Left(value)
          case Right((_, a)) => Right((after, a))
        }
      })
  }

  object ProtobufDecoder {

    import ProductDecoder._
    import Protobuf._

    def fail(failure: String): ProtobufDecoder[Nothing] = ProtobufDecoder(_ => Left(failure))

    def succeedNow[A](a: A): ProtobufDecoder[A] = ProtobufDecoder(bytes => Right((bytes, a)))

    def succeed[A](a: => A): ProtobufDecoder[A] = ProtobufDecoder(bytes => Right((bytes, a)))

    def binaryDecoder: ProtobufDecoder[Chunk[Byte]] = ProtobufDecoder(bytes => Right((Chunk.empty, bytes)))

    def collectAll[A](chunk: Chunk[ProtobufDecoder[A]]): ProtobufDecoder[Chunk[A]] = ???

    def failWhen(cond: Boolean, message: String): ProtobufDecoder[Unit] =
      if (cond) ProtobufDecoder.fail(message) else ProtobufDecoder.succeed(())

    private[codec] val stringDecoder: ProtobufDecoder[String] =
      ProtobufDecoder(bytes => Right((Chunk.empty, new String(bytes.toArray, StandardCharsets.UTF_8))))

    def decode[A](schema: Schema[A], chunk: Chunk[Byte]): scala.util.Either[String, A] =
      decoder(schema)
        .run(chunk)
        .map(_._2)

    private[codec] def decoder[A](schema: Schema[A]): ProtobufDecoder[A] =
      //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
      schema match {
        case Schema.GenericRecord(_, structure, _) => recordDecoder(structure.toChunk)
        case Schema.Sequence(elementSchema, fromChunk, _, _, _) =>
          if (canBePacked(elementSchema)) packedSequenceDecoder(elementSchema).map(fromChunk)
          else nonPackedSequenceDecoder(elementSchema).map(fromChunk)
        case Schema.Map(ks: Schema[k], vs: Schema[v], _) => decoder(Schema.Sequence(ks <*> vs, (c: Chunk[(k, v)]) => Map(c: _*), (m: Map[k, v]) => Chunk.fromIterable(m), identity = "Map"))
        case Schema.Set(schema: Schema[s], _)            => decoder(Schema.Sequence(schema, (c: Chunk[s]) => scala.collection.immutable.Set(c: _*), (m: Set[s]) => Chunk.fromIterable(m), identity = "Set"))
        case Schema.Transform(codec, f, _, _, _)         => transformDecoder(codec, f)
        case Schema.Primitive(standardType, _)           => primitiveDecoder(standardType)
        case Schema.Tuple2(left, right, _)               => tupleDecoder(left, right)
        case Schema.Optional(codec, _)                   => optionalDecoder(codec)
        case Schema.Fail(message, _)                     => fail(message)
        case Schema.Either(left, right, _)               => eitherDecoder(left, right)
        case lzy @ Schema.Lazy(_)                        => decoder(lzy.schema)
        // case Schema.Meta(_, _)                                                                 => astDecoder
        case s: Schema.CaseClass0[A]                                                           => caseClass0Decoder(s)
        case s: Schema.CaseClass1[_, A]                                                        => caseClass1Decoder(s)
        case s: Schema.CaseClass2[_, _, A]                                                     => caseClass2Decoder(s)
        case s: Schema.CaseClass3[_, _, _, A]                                                  => caseClass3Decoder(s)
        case s: Schema.CaseClass4[_, _, _, _, A]                                               => caseClass4Decoder(s)
        case s: Schema.CaseClass5[_, _, _, _, _, A]                                            => caseClass5Decoder(s)
        case s: Schema.CaseClass6[_, _, _, _, _, _, A]                                         => caseClass6Decoder(s)
        case s: Schema.CaseClass7[_, _, _, _, _, _, _, A]                                      => caseClass7Decoder(s)
        case s: Schema.CaseClass8[_, _, _, _, _, _, _, _, A]                                   => caseClass8Decoder(s)
        case s: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, A]                                => caseClass9Decoder(s)
        case s: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, A]                            => caseClass10Decoder(s)
        case s: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, A]                         => caseClass11Decoder(s)
        case s: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, A]                      => caseClass12Decoder(s)
        case s: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, A]                   => caseClass13Decoder(s)
        case s: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, A]                => caseClass14Decoder(s)
        case s: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]             => caseClass15Decoder(s)
        case s: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]          => caseClass16Decoder(s)
        case s: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]       => caseClass17Decoder(s)
        case s: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]    => caseClass18Decoder(s)
        case s: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] => caseClass19Decoder(s)
        case s: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
          caseClass20Decoder(s)
        case s: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
          caseClass21Decoder(s)
        case s: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
          caseClass22Decoder(s)
        case Schema.Enum1(_, c, _)                                                                                                    => enumDecoder(c)
        case Schema.Enum2(_, c1, c2, _)                                                                                               => enumDecoder(c1, c2)
        case Schema.Enum3(_, c1, c2, c3, _)                                                                                           => enumDecoder(c1, c2, c3)
        case Schema.Enum4(_, c1, c2, c3, c4, _)                                                                                       => enumDecoder(c1, c2, c3, c4)
        case Schema.Enum5(_, c1, c2, c3, c4, c5, _)                                                                                   => enumDecoder(c1, c2, c3, c4, c5)
        case Schema.Enum6(_, c1, c2, c3, c4, c5, c6, _)                                                                               => enumDecoder(c1, c2, c3, c4, c5, c6)
        case Schema.Enum7(_, c1, c2, c3, c4, c5, c6, c7, _)                                                                           => enumDecoder(c1, c2, c3, c4, c5, c6, c7)
        case Schema.Enum8(_, c1, c2, c3, c4, c5, c6, c7, c8, _)                                                                       => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8)
        case Schema.Enum9(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, _)                                                                   => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9)
        case Schema.Enum10(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _)                                                             => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
        case Schema.Enum11(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _)                                                        => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
        case Schema.Enum12(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _)                                                   => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
        case Schema.Enum13(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _)                                              => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
        case Schema.Enum14(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _)                                         => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
        case Schema.Enum15(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _)                                    => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
        case Schema.Enum16(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _)                               => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
        case Schema.Enum17(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _)                          => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
        case Schema.Enum18(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _)                     => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
        case Schema.Enum19(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _)                => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
        case Schema.Enum20(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _)           => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
        case Schema.Enum21(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _)      => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
        case Schema.Enum22(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _) => enumDecoder(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
        case Schema.EnumN(_, cs, _)                                                                                                   => enumDecoder(cs.toSeq: _*)
        case Schema.Dynamic(_)                                                                                                        => dynamicDecoder
      }
    //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

    private val dynamicDecoder: ProtobufDecoder[DynamicValue] =
      decoder(DynamicValueSchema.schema)

    private def enumDecoder[Z](cases: Schema.Case[Z, _]*): ProtobufDecoder[Z] =
      keyDecoder.flatMap {
        case (wt, fieldNumber) if fieldNumber <= cases.length =>
          val subtypeCase = cases(fieldNumber - 1)
          wt match {
            case LengthDelimited(width) =>
              decoder(subtypeCase.schema)
                .take(width)
                .asInstanceOf[ProtobufDecoder[Z]]
            case _ =>
              decoder(subtypeCase.schema)
                .asInstanceOf[ProtobufDecoder[Z]]
          }
        case (_, fieldNumber) =>
          fail(s"Failed to decode enumeration. Schema does not contain field number $fieldNumber.")
      }

    private def recordDecoder[Z](
      fields: Seq[Schema.Field[Z, _]],
      decoded: Int = 0
    ): ProtobufDecoder[ListMap[String, _]] =
      if (fields.isEmpty || (fields.size == decoded))
        ProtobufDecoder.succeed(ListMap.empty)
      else
        keyDecoder.flatMap {
          case (wt, fieldNumber) =>
            if (fields.isDefinedAt(fieldNumber - 1)) {
              val Schema.Field(fieldName, schema, _, _, _, _) = fields(fieldNumber - 1)

              wt match {
                case LengthDelimited(width) =>
                  for {
                    fieldValue <- decoder(schema).take(width)
                    remainder  <- recordDecoder(fields, decoded + 1)
                  } yield (remainder.updated(fieldName, fieldValue))

                case _ =>
                  for {
                    fieldValue <- decoder(schema)
                    remainder  <- recordDecoder(fields, decoded + 1)
                  } yield (remainder.updated(fieldName, fieldValue))
              }
            } else {
              fail(s"Failed to decode record. Schema does not contain field number $fieldNumber.")
            }
        }

    private def packedSequenceDecoder[A](schema: Schema[A]): ProtobufDecoder[Chunk[A]] =
      keyDecoder.flatMap {
        case (LengthDelimited(0), 1) => succeed(Chunk.empty)
        case (LengthDelimited(width), 2) =>
          schema match {
            case lzy @ Schema.Lazy(_) => decoder(lzy.schema).loop.take(width)
            case _                    => decoder(schema).loop.take(width)
          }
        case (wt, fieldNumber) => fail(s"Invalid wire type ($wt) or field number ($fieldNumber) for packed sequence")
      }

    private def nonPackedSequenceDecoder[A](schema: Schema[A]): ProtobufDecoder[Chunk[A]] =
      keyDecoder.flatMap {
        case (LengthDelimited(0), 1) => succeed(Chunk.empty)
        case (LengthDelimited(width), 2) =>
          keyDecoder.flatMap {
            case (wt, _) =>
              wt match {
                case LengthDelimited(width) => decoder(schema).take(width)
                case _                      => fail(s"Unexpected wire type $wt for non-packed sequence")
              }
          }.loop.take(width)
        case (wt, fieldNumber) =>
          fail(s"Invalid wire type ($wt) or field number ($fieldNumber) for non-packed sequence")
      }

    private def tupleDecoder[A, B](left: Schema[A], right: Schema[B]): ProtobufDecoder[(A, B)] = {
      def elementDecoder[A](schema: Schema[A], wt: WireType): ProtobufDecoder[A] = wt match {
        case LengthDelimited(width) => decoder(schema).take(width)
        case _                      => decoder(schema)
      }

      keyDecoder.flatMap {
        case (wt, 1) =>
          elementDecoder(left, wt).flatMap { leftValue =>
            keyDecoder.flatMap {
              case (wt, 2) =>
                elementDecoder(right, wt).map(rightValue => (leftValue, rightValue))
              case (_, fieldNumber) => fail(s"Invalid field number ($fieldNumber) for tuple")
            }
          }
        case (wt, 2) =>
          elementDecoder(right, wt).flatMap { rightValue =>
            keyDecoder.flatMap {
              case (wt, 1) =>
                elementDecoder(left, wt).map(leftValue => (leftValue, rightValue))
              case (_, fieldNumber) => fail(s"Invalid field number ($fieldNumber) for tuple")
            }
          }
        case (_, fieldNumber) => fail(s"Invalid field number ($fieldNumber) for tuple")
      }
    }

    private def eitherDecoder[A, B](left: Schema[A], right: Schema[B]): ProtobufDecoder[scala.util.Either[A, B]] =
      keyDecoder.flatMap {
        case (_, fieldNumber) if fieldNumber == 1 => decoder(left).map(Left(_))
        case (_, fieldNumber) if fieldNumber == 2 => decoder(right).map(Right(_))
        case (_, fieldNumber)                     => fail(s"Invalid field number ($fieldNumber) for either")
      }

    private def optionalDecoder[A](schema: Schema[A]): ProtobufDecoder[Option[A]] =
      keyDecoder.flatMap {
        case (LengthDelimited(0), 1)     => succeed(None)
        case (LengthDelimited(width), 2) => decoder(schema).take(width).map(Some(_))
        case (_, 2)                      => decoder(schema).map(Some(_))
        case (_, fieldNumber)            => fail(s"Invalid field number $fieldNumber for option")
      }

    private def floatDecoder: ProtobufDecoder[Float] =
      ProtobufDecoder(bytes => {
        if (bytes.size < 4) {
          Left(s"Invalid number of bytes for Float. Expected 4, got ${bytes.size}")
        } else {
          Right((bytes, ByteBuffer.wrap(bytes.toArray).order(ByteOrder.LITTLE_ENDIAN).getFloat()))
        }
      }).take(4)

    private def doubleDecoder: ProtobufDecoder[Double] =
      ProtobufDecoder(bytes => {
        if (bytes.size < 8) {
          Left(s"Invalid number of bytes for Double. Expected 8, got ${bytes.size}")
        } else {
          Right((bytes, ByteBuffer.wrap(bytes.toArray).order(ByteOrder.LITTLE_ENDIAN).getDouble()))
        }
      }).take(8)

    private def transformDecoder[A, B](schema: Schema[B], f: B => scala.util.Either[String, A]): ProtobufDecoder[A] =
      schema match {
        case Schema.Primitive(typ, _) if typ == StandardType.UnitType =>
          ProtobufDecoder { (chunk: Chunk[Byte]) =>
            f(().asInstanceOf[B]) match {
              case Left(err) => Left(err)
              case Right(b)  => Right(chunk -> b)
            }
          }
        case _ => decoder(schema).flatMap(a => ProtobufDecoder(chunk => f(a).map(b => (chunk, b))))
      }

    private def primitiveDecoder[A](standardType: StandardType[A]): ProtobufDecoder[A] =
      standardType match {
        case StandardType.UnitType   => ProtobufDecoder((chunk: Chunk[Byte]) => Right((chunk, ())))
        case StandardType.StringType => stringDecoder
        case StandardType.BoolType   => varIntDecoder.map(_ != 0)
        case StandardType.ShortType  => varIntDecoder.map(_.shortValue())
        case StandardType.ByteType   => varIntDecoder.map(_.byteValue())
        case StandardType.IntType    => varIntDecoder.map(_.intValue())
        case StandardType.LongType   => varIntDecoder
        case StandardType.FloatType  => floatDecoder
        case StandardType.DoubleType => doubleDecoder
        case StandardType.BigIntegerType =>
          binaryDecoder.map { bytes =>
            new java.math.BigInteger(bytes.toArray)
          }
        case StandardType.BigDecimalType =>
          recordDecoder(bigDecimalStructure).flatMap { data =>
            val opt = for {
              unscaled  <- data.get("unscaled").asInstanceOf[Option[java.math.BigInteger]]
              scale     <- data.get("scale").asInstanceOf[Option[Int]]
              precision <- data.get("precision").asInstanceOf[Option[Int]]
              ctx       = new java.math.MathContext(precision)
            } yield new java.math.BigDecimal(unscaled, scale, ctx)

            opt match {
              case Some(value) => succeedNow(value)
              case None        => fail(s"Invalid big decimal record $data")
            }
          }
        case StandardType.BinaryType => binaryDecoder
        case StandardType.CharType   => stringDecoder.map(_.charAt(0))
        case StandardType.UUIDType =>
          stringDecoder.flatMap { uuid =>
            try succeedNow(UUID.fromString(uuid))
            catch {
              case NonFatal(_) => fail(s"Invalid UUID string $uuid")
            }
          }
        case StandardType.DayOfWeekType =>
          varIntDecoder.map(_.intValue).map(DayOfWeek.of)
        case StandardType.MonthType =>
          varIntDecoder.map(_.intValue).map(Month.of)
        case StandardType.MonthDayType =>
          recordDecoder(monthDayStructure)
            .map(
              data =>
                MonthDay.of(data.getOrElse("month", 0).asInstanceOf[Int], data.getOrElse("day", 0).asInstanceOf[Int])
            )
        case StandardType.PeriodType =>
          recordDecoder(periodStructure)
            .map(
              data =>
                Period.of(
                  data.getOrElse("years", 0).asInstanceOf[Int],
                  data.getOrElse("months", 0).asInstanceOf[Int],
                  data.getOrElse("days", 0).asInstanceOf[Int]
                )
            )
        case StandardType.YearType =>
          varIntDecoder.map(_.intValue).map(Year.of)
        case StandardType.YearMonthType =>
          recordDecoder(yearMonthStructure)
            .map(
              data =>
                YearMonth.of(data.getOrElse("year", 0).asInstanceOf[Int], data.getOrElse("month", 0).asInstanceOf[Int])
            )
        case StandardType.ZoneIdType => stringDecoder.map(ZoneId.of)
        case StandardType.ZoneOffsetType =>
          varIntDecoder
            .map(_.intValue)
            .map(ZoneOffset.ofTotalSeconds)
        case StandardType.DurationType =>
          recordDecoder(durationStructure)
            .map(
              data =>
                Duration.ofSeconds(
                  data.getOrElse("seconds", 0).asInstanceOf[Long],
                  data.getOrElse("nanos", 0).asInstanceOf[Int].toLong
                )
            )
        case StandardType.InstantType(formatter) =>
          stringDecoder.map(v => Instant.from(formatter.parse(v)))
        case StandardType.LocalDateType(formatter) =>
          stringDecoder.map(LocalDate.parse(_, formatter))
        case StandardType.LocalTimeType(formatter) =>
          stringDecoder.map(LocalTime.parse(_, formatter))
        case StandardType.LocalDateTimeType(formatter) =>
          stringDecoder.map(LocalDateTime.parse(_, formatter))
        case StandardType.OffsetTimeType(formatter) =>
          stringDecoder.map(OffsetTime.parse(_, formatter))
        case StandardType.OffsetDateTimeType(formatter) =>
          stringDecoder.map(OffsetDateTime.parse(_, formatter))
        case StandardType.ZonedDateTimeType(formatter) =>
          stringDecoder.map(ZonedDateTime.parse(_, formatter))
        case st => fail(s"Unsupported primitive type $st")
      }

    /**
     * Decodes key which consist out of field type (wire type) and a field number.
     *
     * 8 >>> 3 => 1, 16 >>> 3 => 2, 24 >>> 3 => 3, 32 >>> 3 => 4
     * 0 & 0x07 => 0, 1 & 0x07 => 1, 2 & 0x07 => 2, 9 & 0x07 => 1, 15 & 0x07 => 7
     */
    private[codec] def keyDecoder: ProtobufDecoder[(WireType, Int)] =
      varIntDecoder.flatMap { key =>
        val fieldNumber = (key >>> 3).toInt
        if (fieldNumber < 1) {
          fail(s"Failed decoding key. Invalid field number $fieldNumber")
        } else {
          key & 0x07 match {
            case 0 => succeed((WireType.VarInt, fieldNumber))
            case 1 => succeed((WireType.Bit64, fieldNumber))
            case 2 =>
              varIntDecoder.map(length => (WireType.LengthDelimited(length.toInt), fieldNumber))
            case 3 => succeed((WireType.StartGroup, fieldNumber))
            case 4 => succeed((WireType.EndGroup, fieldNumber))
            case 5 => succeed((WireType.Bit32, fieldNumber))
            case n => fail(s"Failed decoding key. Unknown wire type $n")
          }
        }
      }

    /**
     * Decodes bytes to following types: int32, int64, uint32, uint64, sint32, sint64, bool, enumN.
     * Takes index of first byte which is inside 0 - 127 range.
     * Returns remainder of the bytes together with computed value.
     *
     * (0 -> 127) & 0x80 => 0, (128 -> 255) & 0x80 => 128
     * (0 << 7 => 0, 1 << 7 => 128, 2 << 7 => 256, 3 << 7 => 384
     * 1 & 0X7F => 1, 127 & 0x7F => 127, 128 & 0x7F => 0, 129 & 0x7F => 1
     */
    private def varIntDecoder: ProtobufDecoder[Long] =
      ProtobufDecoder(
        (chunk) =>
          if (chunk.isEmpty) {
            Left("Failed to decode VarInt. Unexpected end of chunk")
          } else {
            val length = chunk.indexWhere(octet => (octet.longValue() & 0x80) != 0x80) + 1
            if (length <= 0) {
              Left("Failed to decode VarInt. No byte within the range 0 - 127 are present")
            } else {
              val value = chunk.take(length).foldRight(0L)((octet, v) => (v << 7) + (octet & 0x7F))
              Right((chunk.drop(length), value))
            }
          }
      )
  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private[codec] object ProductEncoder {

    private[codec] def encodeCaseClass[Z](value: Z, fields: (Schema.Field[Z, _])*): Option[Int] => Chunk[Byte] = { (fieldNumber: Option[Int]) =>
      {
        val encoded = Chunk
          .fromIterable(fields.zipWithIndex.map {
            case ((Schema.Field(_, schema, _, _, get, _)), fieldNumber) =>
              ProtobufEncoder.encode(Some(fieldNumber + 1), schema.asInstanceOf[Schema[Any]], get(value))
          })
          .flatten
        ProtobufEncoder.encodeKey(Protobuf.WireType.LengthDelimited(encoded.size), fieldNumber) ++ encoded
      }
    }
  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private[codec] object ProductDecoder {
    import ProtobufDecoder.{ fail, keyDecoder, succeed }
    import Protobuf.WireType._

    private def unsafeDecodeFields[Z](buffer: Array[Any], fields: Schema.Field[Z, _]*): ProtobufDecoder[Array[Any]] =
      keyDecoder.flatMap {
        case (wt, fieldNumber) if fieldNumber == fields.length =>
          wt match {
            case LengthDelimited(width) =>
              ProtobufDecoder
                .decoder(fields(fieldNumber - 1).schema)
                .take(width)
                .map(fieldValue => buffer.updated(fieldNumber - 1, fieldValue))
            case _ =>
              ProtobufDecoder
                .decoder(fields(fieldNumber - 1).schema)
                .map(fieldValue => buffer.updated(fieldNumber - 1, fieldValue))
          }
        case (wt, fieldNumber) =>
          if (fieldNumber <= fields.length) {
            wt match {
              case LengthDelimited(width) =>
                for {
                  fieldValue <- ProtobufDecoder.decoder(fields(fieldNumber - 1).schema).take(width)
                  remainder  <- unsafeDecodeFields(buffer, fields: _*)
                } yield remainder.updated(fieldNumber - 1, fieldValue)
              case _ =>
                for {
                  fieldValue <- ProtobufDecoder.decoder(fields(fieldNumber - 1).schema)
                  remainder  <- unsafeDecodeFields(buffer, fields: _*)
                } yield remainder.updated(fieldNumber - 1, fieldValue)
            }
          } else {
            fail(s"Failed to decode record. Schema does not contain field number $fieldNumber.")
          }
      }

    @tailrec
    private def validateBuffer(index: Int, buffer: Array[Any]): ProtobufDecoder[Array[Any]] =
      if (index == buffer.length - 1 && buffer(index) != null)
        succeed(buffer)
      else if (buffer(index) == null)
        fail(s"Failed to decode record. Missing field number $index.")
      else
        validateBuffer(index + 1, buffer)

    private[codec] def caseClass0Decoder[Z](schema: Schema.CaseClass0[Z]): ProtobufDecoder[Z] =
      succeed(schema.defaultConstruct())

    private[codec] def caseClass1Decoder[A, Z](schema: Schema.CaseClass1[A, Z]): ProtobufDecoder[Z] =
      unsafeDecodeFields(Array.ofDim[Any](1), schema.field).flatMap { buffer =>
        if (buffer(0) == null)
          fail("Failed to decode record. Missing field 1.")
        else
          succeed(schema.defaultConstruct(buffer(0).asInstanceOf[A]))
      }

    private[codec] def caseClass2Decoder[A1, A2, Z](schema: Schema.CaseClass2[A1, A2, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](2), schema.field1, schema.field2)
        _      <- validateBuffer(0, buffer)
      } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2])

    private[codec] def caseClass3Decoder[A1, A2, A3, Z](schema: Schema.CaseClass3[A1, A2, A3, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](3), schema.field1, schema.field2, schema.field3)
        _      <- validateBuffer(0, buffer)
      } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3])

    private[codec] def caseClass4Decoder[A1, A2, A3, A4, Z](schema: Schema.CaseClass4[A1, A2, A3, A4, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](4), schema.field1, schema.field2, schema.field3, schema.field4)
        _      <- validateBuffer(0, buffer)
      } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4])

    private[codec] def caseClass5Decoder[A1, A2, A3, A4, A5, Z](schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](5), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5)
        _      <- validateBuffer(0, buffer)
      } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5])

    private[codec] def caseClass6Decoder[A1, A2, A3, A4, A5, A6, Z](schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](6), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6)
        _      <- validateBuffer(0, buffer)
      } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6])

    private[codec] def caseClass7Decoder[A1, A2, A3, A4, A5, A6, A7, Z](schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](7), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7)
        _      <- validateBuffer(0, buffer)
      } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7])

    private[codec] def caseClass8Decoder[A1, A2, A3, A4, A5, A6, A7, A8, Z](schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](8), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8)
        _      <- validateBuffer(0, buffer)
      } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8])

    private[codec] def caseClass9Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](9), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9)
        _      <- validateBuffer(0, buffer)
      } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9])

    private[codec] def caseClass10Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](10), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10)
        _      <- validateBuffer(0, buffer)
      } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10])

    private[codec] def caseClass11Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](11), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11)
        _      <- validateBuffer(0, buffer)
      } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11])

    private[codec] def caseClass12Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](12), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12)
        _      <- validateBuffer(0, buffer)
      } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12])

    private[codec] def caseClass13Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](13), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13)
        _      <- validateBuffer(0, buffer)
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

    private[codec] def caseClass14Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](14), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14)
        _      <- validateBuffer(0, buffer)
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

    private[codec] def caseClass15Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](15), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15)
        _      <- validateBuffer(0, buffer)
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

    private[codec] def caseClass16Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](16), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16)
        _      <- validateBuffer(0, buffer)
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

    private[codec] def caseClass17Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](17), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17)
        _      <- validateBuffer(0, buffer)
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

    private[codec] def caseClass18Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](18), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18)
        _      <- validateBuffer(0, buffer)
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

    private[codec] def caseClass19Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](schema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](19), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19)
        _      <- validateBuffer(0, buffer)
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

    private[codec] def caseClass20Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](schema: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](20), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20)
        _      <- validateBuffer(0, buffer)
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

    private[codec] def caseClass21Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](schema: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(Array.ofDim[Any](21), schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20, schema.field21)
        _      <- validateBuffer(0, buffer)
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

    private[codec] def caseClass22Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](schema: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z]): ProtobufDecoder[Z] =
      for {
        buffer <- unsafeDecodeFields(
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
        _ <- validateBuffer(0, buffer)
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
