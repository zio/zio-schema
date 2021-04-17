package zio.schema.codec

import java.nio.charset.StandardCharsets
import java.nio.{ ByteBuffer, ByteOrder }
import java.time._

import zio.schema._
import zio.stream.ZTransducer
import zio.{ Chunk, ZIO }

object ProtobufCodec extends Codec {
  override def encoder[A](schema: Schema[A]): ZTransducer[Any, Nothing, A, Byte] =
    ZTransducer.fromPush(
      (opt: Option[Chunk[A]]) =>
        ZIO.succeed(opt.map(values => values.flatMap(Encoder.encode(None, schema, _))).getOrElse(Chunk.empty))
    )

  override def encode[A](schema: Schema[A]): A => Chunk[Byte] = a => Encoder.encode(None, schema, a)

  override def decoder[A](schema: Schema[A]): ZTransducer[Any, String, Byte, A] =
    ZTransducer.fromPush(
      (opt: Option[Chunk[Byte]]) =>
        ZIO.fromEither(opt.map(chunk => Decoder.decode(schema, chunk).map(Chunk(_))).getOrElse(Right(Chunk.empty)))
    )

  override def decode[A](schema: Schema[A]): Chunk[Byte] => Either[String, A] =
    ch =>
      if (ch.isEmpty)
        Left("No bytes to decode")
      else
        Decoder.decode(schema, ch)

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

    def flatFields(
      structure: Map[String, Schema[_]],
      nextFieldNumber: Int = 1
    ): Map[Int, (String, Schema[_])] =
      structure.toSeq
        .foldLeft((nextFieldNumber, Map[Int, (String, Schema[_])]())) { (numAndMap, fieldAndSchema) =>
          nestedFields(fieldAndSchema._1, fieldAndSchema._2, nextFieldNumber) match {
            case Some(fields) => (numAndMap._1 + fields.size, numAndMap._2 ++ fields)
            case None         => (numAndMap._1 + 1, numAndMap._2 + (numAndMap._1 -> fieldAndSchema))
          }
        }
        ._2

    private def nestedFields(
      baseField: String,
      schema: Schema[_],
      nextFieldNumber: Int
    ): Option[Map[Int, (String, Schema[_])]] =
      schema match {
        case Schema.Transform(codec, f, g) =>
          nestedFields(baseField, codec, nextFieldNumber).map(_.map {
            case (fieldNumber, fieldAndSchema) =>
              (fieldNumber, (fieldAndSchema._1, Schema.Transform(fieldAndSchema._2.asInstanceOf[Schema[Any]], f, g)))
          })
        case _ => None
      }

    def tupleSchema[A, B](first: Schema[A], second: Schema[B]): Schema[Map[String, _]] =
      Schema.record(Map("first" -> first, "second" -> second))

    def optionalSchema[A](codec: Schema[A]): Schema[Map[String, _]] = Schema.record(Map("value" -> codec))

    def monthDayStructure(): Map[String, Schema[Int]] =
      Map("month" -> Schema.Primitive(StandardType.IntType), "day" -> Schema.Primitive(StandardType.IntType))

    def periodStructure(): Map[String, Schema[Int]] = Map(
      "years"  -> Schema.Primitive(StandardType.IntType),
      "months" -> Schema.Primitive(StandardType.IntType),
      "days"   -> Schema.Primitive(StandardType.IntType)
    )

    def yearMonthStructure(): Map[String, Schema[Int]] =
      Map("year" -> Schema.Primitive(StandardType.IntType), "month" -> Schema.Primitive(StandardType.IntType))

    def durationStructure(): Map[String, Schema[_]] =
      Map("seconds" -> Schema.Primitive(StandardType.LongType), "nanos" -> Schema.Primitive(StandardType.IntType))
  }

  object Encoder {
    import Protobuf._

    def encode[A](fieldNumber: Option[Int], schema: Schema[A], value: A): Chunk[Byte] =
      (schema, value) match {
        case (Schema.Record(structure), v: Map[String, _])       => encodeRecord(fieldNumber, structure, v)
        case (Schema.Sequence(element), v: Chunk[_])             => encodeSequence(fieldNumber, element, v)
        case (Schema.Enumeration(structure), v: Map[String, _])  => encodeEnumeration(fieldNumber, structure, v)
        case (Schema.Transform(codec, _, g), _)                  => g(value).map(encode(fieldNumber, codec, _)).getOrElse(Chunk.empty)
        case (Schema.Primitive(standardType), v)                 => encodePrimitive(fieldNumber, standardType, v)
        case (Schema.Tuple(left, right), v @ (_, _))             => encodeTuple(fieldNumber, left, right, v)
        case (Schema.Optional(codec), v: Option[_])              => encodeOptional(fieldNumber, codec, v)
        case (Schema.EitherSchema(left, right), v: Either[_, _]) => encodeEither(fieldNumber, left, right, v)
        case (_, _)                                              => Chunk.empty
      }

    private def encodeRecord(
      fieldNumber: Option[Int],
      structure: Map[String, Schema[_]],
      data: Map[String, _]
    ): Chunk[Byte] = {
      val encodedRecord = Chunk
        .fromIterable(flatFields(structure).toSeq.map {
          case (fieldNumber, (field, schema)) =>
            data
              .get(field)
              .map(value => encode(Some(fieldNumber), schema.asInstanceOf[Schema[Any]], value))
              .getOrElse(Chunk.empty)
        })
        .flatten

      encodeKey(WireType.LengthDelimited(encodedRecord.size), fieldNumber) ++ encodedRecord
    }

    private def encodeSequence[A](
      fieldNumber: Option[Int],
      element: Schema[A],
      sequence: Chunk[A]
    ): Chunk[Byte] =
      if (canBePacked(element)) {
        val chunk = sequence.flatMap(value => encode(None, element, value))
        encodeKey(WireType.LengthDelimited(chunk.size), fieldNumber) ++ chunk
      } else {
        sequence.flatMap(value => encode(fieldNumber, element, value))
      }

    private def encodeEnumeration(
      fieldNumber: Option[Int],
      structure: Map[String, Schema[_]],
      valueMap: Map[String, _]
    ): Chunk[Byte] = {
      val encodedEnum = if (valueMap.isEmpty) {
        Chunk.empty
      } else {
        val (field, value) = valueMap.toSeq.head
        structure.zipWithIndex
          .find(v => v._1._1 == field)
          .map(v => encode(Some(v._2 + 1), v._1._2.asInstanceOf[Schema[Any]], value))
          .getOrElse(Chunk.empty)
      }
      encodeKey(WireType.LengthDelimited(encodedEnum.size), fieldNumber) ++ encodedEnum
    }

    @scala.annotation.tailrec
    private def encodePrimitive[A](
      fieldNumber: Option[Int],
      standardType: StandardType[A],
      value: A
    ): Chunk[Byte] =
      (standardType, value) match {
        case (StandardType.UnitType, _) =>
          Chunk.empty
        case (StandardType.StringType, str: String) =>
          val encoded = Chunk.fromArray(str.getBytes(StandardCharsets.UTF_8))
          encodeKey(WireType.LengthDelimited(encoded.size), fieldNumber) ++ encoded
        case (StandardType.BoolType, b: Boolean) =>
          encodeKey(WireType.VarInt, fieldNumber) ++ encodeVarInt(if (b) 1 else 0)
        case (StandardType.ShortType, v: Short) =>
          encodeKey(WireType.VarInt, fieldNumber) ++ encodeVarInt(v.toLong)
        case (StandardType.IntType, v: Int) =>
          encodeKey(WireType.VarInt, fieldNumber) ++ encodeVarInt(v)
        case (StandardType.LongType, v: Long) =>
          encodeKey(WireType.VarInt, fieldNumber) ++ encodeVarInt(v)
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
        case (StandardType.DayOfWeekType, v: DayOfWeek) =>
          encodePrimitive(fieldNumber, StandardType.IntType, v.getValue)
        case (StandardType.Month, v: Month) =>
          encodePrimitive(fieldNumber, StandardType.IntType, v.getValue)
        case (StandardType.MonthDay, v: MonthDay) =>
          encodeRecord(fieldNumber, monthDayStructure(), Map("month" -> v.getMonthValue, "day" -> v.getDayOfMonth))
        case (StandardType.Period, v: Period) =>
          encodeRecord(
            fieldNumber,
            periodStructure(),
            Map("years" -> v.getYears, "months" -> v.getMonths, "days" -> v.getDays)
          )
        case (StandardType.Year, v: Year) =>
          encodePrimitive(fieldNumber, StandardType.IntType, v.getValue)
        case (StandardType.YearMonth, v: YearMonth) =>
          encodeRecord(fieldNumber, yearMonthStructure(), Map("year" -> v.getYear, "month" -> v.getMonthValue))
        case (StandardType.ZoneId, v: ZoneId) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.getId)
        case (StandardType.ZoneOffset, v: ZoneOffset) =>
          encodePrimitive(fieldNumber, StandardType.IntType, v.getTotalSeconds)
        case (StandardType.Duration(_), v: Duration) =>
          encodeRecord(fieldNumber, durationStructure(), Map("seconds" -> v.getSeconds, "nanos" -> v.getNano))
        case (StandardType.Instant(formatter), v: Instant) =>
          encodePrimitive(fieldNumber, StandardType.StringType, formatter.format(v))
        case (StandardType.LocalDate(formatter), v: LocalDate) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.format(formatter))
        case (StandardType.LocalTime(formatter), v: LocalTime) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.format(formatter))
        case (StandardType.LocalDateTime(formatter), v: LocalDateTime) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.format(formatter))
        case (StandardType.OffsetTime(formatter), v: OffsetTime) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.format(formatter))
        case (StandardType.OffsetDateTime(formatter), v: OffsetDateTime) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.format(formatter))
        case (StandardType.ZonedDateTime(formatter), v: ZonedDateTime) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.format(formatter))
        case (_, _) =>
          Chunk.empty
      }

    private def encodeTuple[A, B](
      fieldNumber: Option[Int],
      left: Schema[A],
      right: Schema[B],
      tuple: (A, B)
    ): Chunk[Byte] =
      encode(
        fieldNumber,
        tupleSchema(left, right),
        Map[String, Any]("first" -> tuple._1, "second" -> tuple._2)
      )

    private def encodeEither[A, B](
      fieldNumber: Option[Int],
      left: Schema[A],
      right: Schema[B],
      either: Either[A, B]
    ): Chunk[Byte] = {
      val encodedEither = either match {
        case Left(value)  => encode(Some(1), left, value)
        case Right(value) => encode(Some(2), right, value)
      }

      encodeKey(WireType.LengthDelimited(encodedEither.size), fieldNumber) ++ encodedEither
    }

    private def encodeOptional[A](fieldNumber: Option[Int], schema: Schema[A], value: Option[A]): Chunk[Byte] =
      value match {
        case Some(v) =>
          encode(
            fieldNumber,
            optionalSchema(schema),
            Map("value" -> v)
          )
        case None => Chunk.empty
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
    private def encodeKey(wireType: WireType, fieldNumber: Option[Int]): Chunk[Byte] =
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

    @scala.annotation.tailrec
    private def canBePacked(schema: Schema[_]): Boolean = schema match {
      case _: Schema.Record               => false
      case Schema.Sequence(element)       => canBePacked(element)
      case _: Schema.Enumeration          => false
      case Schema.Transform(codec, _, _)  => canBePacked(codec)
      case Schema.Primitive(standardType) => canBePacked(standardType)
      case _: Schema.Tuple[_, _]          => false
      case _: Schema.Optional[_]          => false
      case _: Schema.Fail[_]              => false
      case _: Schema.EitherSchema[_, _]   => false
      case _                              => ???
    }

    private def canBePacked(standardType: StandardType[_]): Boolean = standardType match {
      case StandardType.UnitType          => false
      case StandardType.StringType        => false
      case StandardType.BoolType          => true
      case StandardType.ShortType         => true
      case StandardType.IntType           => true
      case StandardType.LongType          => true
      case StandardType.FloatType         => true
      case StandardType.DoubleType        => true
      case StandardType.BinaryType        => false
      case StandardType.CharType          => true
      case StandardType.BigIntegerType    => false
      case StandardType.BigDecimalType    => false
      case StandardType.DayOfWeekType     => true
      case StandardType.Month             => true
      case StandardType.MonthDay          => false
      case StandardType.Period            => false
      case StandardType.Year              => true
      case StandardType.YearMonth         => false
      case StandardType.ZoneId            => false
      case StandardType.ZoneOffset        => true
      case StandardType.Duration(_)       => true
      case StandardType.Instant(_)        => false
      case StandardType.LocalDate(_)      => false
      case StandardType.LocalTime(_)      => false
      case StandardType.LocalDateTime(_)  => false
      case StandardType.OffsetTime(_)     => false
      case StandardType.OffsetDateTime(_) => false
      case StandardType.ZonedDateTime(_)  => false
    }
  }

  object Decoder {
    import Protobuf._

    trait Decoder[A] { self =>
      def run(chunk: Chunk[Byte], wireType: WireType): Either[String, (Chunk[Byte], A)]

      def map[B](f: A => B): Decoder[B] =
        (chunk: Chunk[Byte], wireType: WireType) =>
          self.run(chunk, wireType).map {
            case (remainder, value) => (remainder, f(value))
          }

      def flatMap[B](f: A => Decoder[B]): Decoder[B] =
        (chunk: Chunk[Byte], wireType: WireType) =>
          self.run(chunk, wireType).flatMap {
            case (remainder, value) => f(value).run(remainder, wireType)
          }
    }

    object FailDecoder {
      def apply[A](message: String): Decoder[A] = (_, _) => Left(message)
    }

    def decode[A](schema: Schema[A], chunk: Chunk[Byte]): Either[String, A] =
      decoder(schema)
        .run(chunk, WireType.LengthDelimited(chunk.size))
        .map(_._2)

    private def decoder[A](schema: Schema[A]): Decoder[A] =
      schema match {
        case Schema.Record(structure)         => recordDecoder(structure).asInstanceOf[Decoder[A]]
        case Schema.Sequence(element)         => sequenceDecoder(element).asInstanceOf[Decoder[A]]
        case Schema.Enumeration(structure)    => enumDecoder(structure).asInstanceOf[Decoder[A]]
        case Schema.Transform(codec, f, _)    => transformDecoder(codec, f)
        case Schema.Primitive(standardType)   => primitiveDecoder(standardType)
        case Schema.Tuple(left, right)        => tupleDecoder(left, right).asInstanceOf[Decoder[A]]
        case Schema.Optional(codec)           => optionalDecoder(codec).asInstanceOf[Decoder[A]]
        case Schema.Fail(message)             => (_, _) => Left(message)
        case Schema.EitherSchema(left, right) => eitherDecoder(left, right).asInstanceOf[Decoder[A]]
        case _ => ???
      }

    private def enumDecoder(structure: Map[String, Schema[_]]): Decoder[Map[String, _]] =
      loopDecoder(flatFields(structure), Map())

    private def recordDecoder(structure: Map[String, Schema[_]]): Decoder[Map[String, _]] =
      loopDecoder(flatFields(structure), defaultMap(structure))

    private def loopDecoder(
      fields: Map[Int, (String, Schema[_])],
      result: Map[String, _]
    ): Decoder[Map[String, _]] =
      (chunk, wireType) =>
        if (chunk.isEmpty) {
          Right((chunk, result))
        } else {
          loopStepDecoder(fields, result).run(chunk, wireType)
        }

    /**
     * Decoder that consumes remaining bytes in loop when .
     * - takes matching schema based on field number.
     * - if field type is length delimited, takes length of bytes from chunk and decodes them.
     * - populates result map with decoded values.
     */
    private def loopStepDecoder(
      fields: Map[Int, (String, Schema[_])],
      result: Map[String, _]
    ): Decoder[Map[String, _]] =
      (bytes, wireType) => {
        keyDecoder.run(bytes, wireType).flatMap {
          case (remainder, (wt, fieldNumber)) =>
            fields
              .get(fieldNumber)
              .map {
                case (field, schema) =>
                  wt match {
                    case WireType.LengthDelimited(width) => {
                      decoder(schema)
                        .run(remainder.take(width), wt)
                        .flatMap {
                          case (chunk, value: Seq[_]) =>
                            val values: Seq[_] =
                              result.get(field).asInstanceOf[Option[Seq[_]]].map(_ ++ value).getOrElse(value)
                            loopDecoder(fields, result + (field -> values))
                              .run(chunk, wt)
                          case (chunk, value) =>
                            loopDecoder(fields, result + (field -> value))
                              .run(chunk, wt)
                        }
                        .flatMap {
                          case (chunk, value) =>
                            loopDecoder(fields, value)
                              .run(remainder.drop(width), wt)
                              .map {
                                case (nextChunk, nextValue) => (chunk ++ nextChunk, value ++ nextValue)
                              }
                        }
                    }
                    case _ =>
                      decoder(schema).run(remainder, wt).map {
                        case (chunk, value: Seq[_]) =>
                          val values: Seq[_] =
                            result.get(field).asInstanceOf[Option[Seq[_]]].map(_ ++ value).getOrElse(value)
                          loopDecoder(fields, result + (field -> values))
                            .run(chunk, wt)
                            .getOrElse((Chunk.empty, result))
                        case (chunk, value) =>
                          loopDecoder(fields, result + (field -> value))
                            .run(chunk, wt)
                            .getOrElse((Chunk.empty, result))
                      }
                  }
              }
              .get
        }
      }

    private def sequenceDecoder[A](schema: Schema[A]): Decoder[Chunk[A]] =
      (chunk, wireType) =>
        wireType match {
          case WireType.LengthDelimited(length) =>
            sequenceLoopDecoder(schema, Chunk.empty).run(chunk.take(length), wireType).map {
              case (_, values) => (chunk.drop(length), values)
            }
          case _ =>
            Left("Invalid wire type")
        }

    private def sequenceLoopDecoder[A](schema: Schema[A], values: Chunk[A]): Decoder[Chunk[A]] =
      (chunk, wireType) =>
        if (chunk.isEmpty)
          Right((chunk, values))
        else
          decoder(schema)
            .run(chunk, wireType)
            .flatMap {
              case (remainder, value) =>
                sequenceLoopDecoder(schema, values :+ value).run(remainder, wireType)
            }

    private def transformDecoder[A, B](schema: Schema[B], f: B => Either[String, A]): Decoder[A] =
      decoder(schema).flatMap(a => (chunk, _) => f(a).map(b => (chunk, b)))

    private def primitiveDecoder[A](standardType: StandardType[_]): Decoder[A] =
      standardType match {
        case StandardType.UnitType => ((chunk: Chunk[Byte]) => Right((chunk, ()))).asInstanceOf[Decoder[A]]
        case StandardType.StringType =>
          stringDecoder.asInstanceOf[Decoder[A]]
        case StandardType.BoolType => packedDecoder(WireType.VarInt, varIntDecoder).map(_ != 0).asInstanceOf[Decoder[A]]
        case StandardType.ShortType =>
          packedDecoder(WireType.VarInt, varIntDecoder).map(_.shortValue()).asInstanceOf[Decoder[A]]
        case StandardType.IntType =>
          packedDecoder(WireType.VarInt, varIntDecoder).map(_.intValue()).asInstanceOf[Decoder[A]]
        case StandardType.LongType   => packedDecoder(WireType.VarInt, varIntDecoder).asInstanceOf[Decoder[A]]
        case StandardType.FloatType  => floatDecoder.asInstanceOf[Decoder[A]]
        case StandardType.DoubleType => doubleDecoder.asInstanceOf[Decoder[A]]
        case StandardType.BinaryType => binaryDecoder.asInstanceOf[Decoder[A]]
        case StandardType.CharType   => stringDecoder.map(_.charAt(0)).asInstanceOf[Decoder[A]]
        case StandardType.DayOfWeekType =>
          packedDecoder(WireType.VarInt, varIntDecoder).map(_.intValue).map(DayOfWeek.of).asInstanceOf[Decoder[A]]
        case StandardType.Month =>
          packedDecoder(WireType.VarInt, varIntDecoder).map(_.intValue).map(Month.of).asInstanceOf[Decoder[A]]
        case StandardType.MonthDay =>
          recordDecoder(monthDayStructure())
            .map(
              data =>
                MonthDay.of(data.getOrElse("month", 0).asInstanceOf[Int], data.getOrElse("day", 0).asInstanceOf[Int])
            )
            .asInstanceOf[Decoder[A]]
        case StandardType.Period =>
          recordDecoder(periodStructure())
            .map(
              data =>
                Period.of(
                  data.getOrElse("years", 0).asInstanceOf[Int],
                  data.getOrElse("months", 0).asInstanceOf[Int],
                  data.getOrElse("days", 0).asInstanceOf[Int]
                )
            )
            .asInstanceOf[Decoder[A]]
        case StandardType.Year =>
          packedDecoder(WireType.VarInt, varIntDecoder).map(_.intValue).map(Year.of).asInstanceOf[Decoder[A]]
        case StandardType.YearMonth =>
          recordDecoder(yearMonthStructure())
            .map(
              data =>
                YearMonth.of(data.getOrElse("year", 0).asInstanceOf[Int], data.getOrElse("month", 0).asInstanceOf[Int])
            )
            .asInstanceOf[Decoder[A]]
        case StandardType.ZoneId => stringDecoder.map(ZoneId.of).asInstanceOf[Decoder[A]]
        case StandardType.ZoneOffset =>
          packedDecoder(WireType.VarInt, varIntDecoder)
            .map(_.intValue)
            .map(ZoneOffset.ofTotalSeconds)
            .asInstanceOf[Decoder[A]]
        case StandardType.Duration(_) =>
          recordDecoder(durationStructure())
            .map(
              data =>
                Duration.ofSeconds(
                  data.getOrElse("seconds", 0).asInstanceOf[Long],
                  data.getOrElse("nanos", 0).asInstanceOf[Int].toLong
                )
            )
            .asInstanceOf[Decoder[A]]
        case StandardType.Instant(formatter) =>
          stringDecoder.map(v => Instant.from(formatter.parse(v))).asInstanceOf[Decoder[A]]
        case StandardType.LocalDate(formatter) =>
          stringDecoder.map(LocalDate.parse(_, formatter)).asInstanceOf[Decoder[A]]
        case StandardType.LocalTime(formatter) =>
          stringDecoder.map(LocalTime.parse(_, formatter)).asInstanceOf[Decoder[A]]
        case StandardType.LocalDateTime(formatter) =>
          stringDecoder.map(LocalDateTime.parse(_, formatter)).asInstanceOf[Decoder[A]]
        case StandardType.OffsetTime(formatter) =>
          stringDecoder.map(OffsetTime.parse(_, formatter)).asInstanceOf[Decoder[A]]
        case StandardType.OffsetDateTime(formatter) =>
          stringDecoder.map(OffsetDateTime.parse(_, formatter)).asInstanceOf[Decoder[A]]
        case StandardType.ZonedDateTime(formatter) =>
          stringDecoder.map(ZonedDateTime.parse(_, formatter)).asInstanceOf[Decoder[A]]
        case _ =>
          (_, _) => Left("Unsupported primitive type")
      }

    private def tupleDecoder[A, B](left: Schema[A], right: Schema[B]): Decoder[(A, B)] =
      decoder(tupleSchema(left, right))
        .flatMap(
          record =>
            (chunk, _) =>
              (record.get("first"), record.get("second")) match {
                case (Some(first), Some(second)) => Right((chunk, (first.asInstanceOf[A], second.asInstanceOf[B])))
                case _                           => Left("Error decoding tuple")
              }
        )

    private def eitherDecoder[A, B](left: Schema[A], right: Schema[B]): Decoder[Either[A, B]] =
      (chunk, wireType) => {
        keyDecoder.run(chunk, wireType).flatMap {
          case (remainder, (wireType, fieldNumber)) if fieldNumber == 1 => {
            decoder(left).run(remainder, wireType).map { case (chunk, v) => (chunk, Left(v)) }
          }
          case (remainder, (wireType, fieldNumber)) if fieldNumber == 2 => {
            decoder(right).run(remainder, wireType).map { case (chunk, v) => (chunk, Right(v)) }
          }
        }
      }

    private def optionalDecoder[A](schema: Schema[_]): Decoder[Option[A]] =
      decoder(optionalSchema(schema))
        .map(record => record.get("value").asInstanceOf[Option[A]])

    private def stringDecoder: Decoder[String] = lengthDelimitedDecoder { length => (chunk, _) =>
      decodeChunk(length, chunk, bytes => new String(bytes.toArray, StandardCharsets.UTF_8))
    }

    private def floatDecoder: Decoder[Float] =
      packedDecoder(
        WireType.Bit32,
        (chunk, _) => {
          decodeChunk(4, chunk, bytes => {
            val byteBuffer = ByteBuffer.wrap(bytes.toArray)
            byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
            byteBuffer.getFloat
          })
        }
      )

    private def doubleDecoder: Decoder[Double] =
      packedDecoder(
        WireType.Bit64,
        (chunk, _) => {
          decodeChunk(8, chunk, bytes => {
            val byteBuffer = ByteBuffer.wrap(bytes.toArray)
            byteBuffer.order(ByteOrder.LITTLE_ENDIAN)
            byteBuffer.getDouble
          })
        }
      )

    private def binaryDecoder: Decoder[Chunk[Byte]] = lengthDelimitedDecoder { length => (chunk, _) =>
      decodeChunk(length, chunk, identity)
    }

    private def lengthDelimitedDecoder[A](decoder: Int => Decoder[A]): Decoder[A] =
      (chunk, wireType) =>
        wireType match {
          case WireType.LengthDelimited(length) => decoder(length).run(chunk, wireType)
          case _                                => Left("Invalid wire type")
        }

    private def packedDecoder[A](decoderWireType: WireType, decoder: Decoder[A]): Decoder[A] =
      (chunk, wireType) =>
        wireType match {
          case WireType.LengthDelimited(_)      => decoder.run(chunk, wireType)
          case _ if decoderWireType == wireType => decoder.run(chunk, wireType)
          case _                                => Left("Invalid wire type")
        }

    /**
     * Decodes key which consist out of field type and field number.
     *
     * 8 >>> 3 => 1, 16 >>> 3 => 2, 24 >>> 3 => 3, 32 >>> 3 => 4
     * 0 & 0x07 => 0, 1 & 0x07 => 1, 2 & 0x07 => 2, 9 & 0x07 => 1, 15 & 0x07 => 7
     */
    private def keyDecoder: Decoder[(WireType, Int)] =
      varIntDecoder.flatMap { key => (chunk, wireType) =>
        val fieldNumber = (key >>> 3).toInt
        if (fieldNumber < 1) {
          Left("Failed decoding key: invalid field number")
        } else {
          key & 0x07 match {
            case 0 => Right((chunk, (WireType.VarInt, fieldNumber)))
            case 1 => Right((chunk, (WireType.Bit64, fieldNumber)))
            case 2 =>
              varIntDecoder.map(length => (WireType.LengthDelimited(length.toInt), fieldNumber)).run(chunk, wireType)
            case 3 => Right((chunk, (WireType.StartGroup, fieldNumber)))
            case 4 => Right((chunk, (WireType.EndGroup, fieldNumber)))
            case 5 => Right((chunk, (WireType.Bit32, fieldNumber)))
            case _ => Left("Failed decoding key: unknown wire type")
          }
        }
      }

    /**
     * Decodes bytes to following types: int32, int64, uint32, uint64, sint32, sint64, bool, enum.
     * Takes index of first byte which is inside 0 - 127 range.
     * Returns remainder of the bytes together with computed value.
     *
     * (0 -> 127) & 0x80 => 0, (128 -> 255) & 0x80 => 128
     * (0 << 7 => 0, 1 << 7 => 128, 2 << 7 => 256, 3 << 7 => 384
     * 1 & 0X7F => 1, 127 & 0x7F => 127, 128 & 0x7F => 0, 129 & 0x7F => 1
     */
    private def varIntDecoder: Decoder[Long] =
      (chunk, _) =>
        if (chunk.isEmpty) {
          Left("Unexpected end of chunk")
        } else {
          val length = chunk.indexWhere(octet => (octet.longValue() & 0x80) != 0x80) + 1
          if (length <= 0) {
            Left("Unexpected end of chunk")
          } else {
            val value = chunk.take(length).foldRight(0L)((octet, v) => (v << 7) + (octet & 0x7F))
            Right((chunk.drop(length), value))
          }
        }

    private def defaultMap(structure: Map[String, Schema[_]]): Map[String, _] =
      structure.foldLeft(Map[String, Any]())(
        (result, fieldAndSchema) =>
          defaultValue(fieldAndSchema._2).map(default => result + (fieldAndSchema._1 -> default)).getOrElse(result)
      )

    private def defaultValue(schema: Schema[_]): Option[Any] = schema match {
      case Schema.Record(structure)       => Some(defaultMap(structure))
      case Schema.Sequence(_)             => Some(Chunk())
      case Schema.Enumeration(structure)  => Some(defaultMap(structure))
      case Schema.Transform(codec, f, _)  => defaultValue(codec).flatMap(f(_).toOption)
      case Schema.Primitive(standardType) => defaultValue(standardType)
      case Schema.Tuple(left, right) =>
        if (defaultValue(left).isEmpty || defaultValue(right).isEmpty) None else Some((left, right))
      case Schema.Optional(schema) => defaultValue(schema)
      case _: Schema.Fail[_]       => Some(None)
      case Schema.EitherSchema(schema1, schema2) =>
        defaultValue(schema1) match {
          case s: Some[Any] => s
          case None         => defaultValue(schema2)
        }
      case _                     => ???
    }

    private def defaultValue(standardType: StandardType[_]): Option[Any] = standardType match {
      case StandardType.UnitType          => Some(())
      case StandardType.StringType        => Some("")
      case StandardType.BoolType          => Some(false)
      case StandardType.ShortType         => Some(0)
      case StandardType.IntType           => Some(0)
      case StandardType.LongType          => Some(0L)
      case StandardType.FloatType         => Some(0.0f)
      case StandardType.DoubleType        => Some(0.0)
      case StandardType.BinaryType        => Some(Chunk.empty)
      case StandardType.CharType          => None
      case StandardType.BigIntegerType    => Some(java.math.BigInteger.ZERO)
      case StandardType.BigDecimalType    => Some(java.math.BigDecimal.ZERO)
      case StandardType.DayOfWeekType     => None
      case StandardType.Month             => None
      case StandardType.MonthDay          => None
      case StandardType.Period            => None
      case StandardType.Year              => None
      case StandardType.YearMonth         => None
      case StandardType.ZoneId            => None
      case StandardType.ZoneOffset        => None
      case StandardType.Duration(_)       => None
      case StandardType.Instant(_)        => None
      case StandardType.LocalDate(_)      => None
      case StandardType.LocalTime(_)      => None
      case StandardType.LocalDateTime(_)  => None
      case StandardType.OffsetTime(_)     => None
      case StandardType.OffsetDateTime(_) => None
      case StandardType.ZonedDateTime(_)  => None
    }

    private def decodeChunk[A](index: Int, chunk: Chunk[Byte], fn: Chunk[Byte] => A): Either[String, (Chunk[Byte], A)] =
      if (index > chunk.size) {
        Left("Unexpected end of chunk");
      } else {
        val (bs, remainder) = chunk.splitAt(index);
        Right((remainder, fn(bs)))
      }
  }
}
