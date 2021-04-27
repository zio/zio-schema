package zio.schema.codec

import java.nio.charset.StandardCharsets
import java.nio.{ ByteBuffer, ByteOrder }
import java.time._

import zio.schema._
import zio.schema.codec.ProtobufCodec.Protobuf.WireType.LengthDelimited
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

    def singleSchema[A](codec: Schema[A]): Schema[Map[String, _]] = Schema.record(Map("value" -> codec))

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

    /**
     * Used when encoding sequence of values to decide whether each value need its own key or values can be packed together without keys (for example numbers).
     */
    @scala.annotation.tailrec
    def canBePacked(schema: Schema[_]): Boolean = schema match {
      case _: Schema.Record               => false
      case Schema.Sequence(element)       => canBePacked(element)
      case _: Schema.Enumeration          => false
      case Schema.Transform(codec, _, _)  => canBePacked(codec)
      case Schema.Primitive(standardType) => canBePacked(standardType)
      case _: Schema.Tuple[_, _]          => false
      case _: Schema.Optional[_]          => false
      case _: Schema.Fail[_]              => false
      case _: Schema.EitherSchema[_, _]   => false
      case _: Schema.ListSchema[_]        => false
      case _: Schema.CaseClass[_]         => false
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
        val chunk = sequence.zipWithIndexFrom(1).flatMap {
          case (a, i) => encode(Some(i), element, a)
        }
        encodeKey(WireType.LengthDelimited(chunk.size), fieldNumber) ++ chunk
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
            singleSchema(schema),
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
  }

  final case class Decoder[+A](run: Chunk[Byte] => Either[String, (Chunk[Byte], A)]) { self =>

    def map[B](f: A => B): Decoder[B] =
      Decoder(
        bytes =>
          self.run(bytes).map {
            case (remainder, a) => (remainder, f(a))
          }
      )

    def flatMap[B](f: A => Decoder[B]): Decoder[B] =
      Decoder(
        bytes =>
          if (bytes.isEmpty) {
            Left("Unexpected end of bytes")
          } else {
            self.run(bytes).flatMap {
              case (remainder, a) => f(a).run(remainder)
            }
          }
      )

    def loop: Decoder[Chunk[A]] =
      self.flatMap(
        a0 =>
          Decoder(bytes => {
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

    def take(n: Int): Decoder[A] =
      Decoder(bytes => {
        val (before, after) = bytes.splitAt(n)
        self.run(before) match {
          case Left(value)           => Left(value)
          case Right((remainder, a)) => Right((remainder ++ after, a))
        }
      })
  }

  object Decoder {
    import Protobuf._

    def fail(failure: String): Decoder[Nothing] = Decoder(_ => Left(failure))

    def succeed[A](a: => A): Decoder[A] = Decoder(bytes => Right((bytes, a)))

    def binaryDecoder: Decoder[Chunk[Byte]] = Decoder(bytes => Right((Chunk.empty, bytes)))

    def collectAll[A](chunk: Chunk[Decoder[A]]): Decoder[Chunk[A]] = ???

    def stringDecoder: Decoder[String] =
      Decoder(bytes => Right((Chunk.empty, new String(bytes.toArray, StandardCharsets.UTF_8))))

    def decode[A](schema: Schema[A], chunk: Chunk[Byte]): Either[String, A] =
      decoder(schema)
        .run(chunk)
        .map(_._2)

    private def decoder[A](schema: Schema[A]): Decoder[A] =
      schema match {
        case Schema.Record(structure) => recordDecoder(flatFields(structure))
        case Schema.Sequence(element) =>
          if (canBePacked(element)) packedSequenceDecoder(element) else nonPackedSequenceDecoder(element)
        case Schema.Enumeration(structure)    => enumDecoder(flatFields(structure))
        case Schema.Transform(codec, f, _)    => transformDecoder(codec, f)
        case Schema.Primitive(standardType)   => primitiveDecoder(standardType)
        case Schema.Tuple(left, right)        => tupleDecoder(left, right)
        case Schema.Optional(codec)           => optionalDecoder(codec)
        case Schema.Fail(message)             => fail(message)
        case Schema.EitherSchema(left, right) => eitherDecoder(left, right)
        case Schema.ListSchema(codec)         => listDecoder(codec)
        case _: Schema.CaseClass[A]           => ???

      }

    private def enumDecoder(fields: Map[Int, (String, Schema[_])]): Decoder[Map[String, _]] =
      keyDecoder.flatMap {
        case (_, fieldNumber) =>
          if (fields.contains(fieldNumber)) {
            val (fieldName, schema) = fields(fieldNumber)

            decoder(schema).map(fieldValue => Map(fieldName -> fieldValue))
          } else {
            fail(s"Schema doesn't contain field number $fieldNumber.")
          }
      }

    private def recordDecoder(fields: Map[Int, (String, Schema[_])]): Decoder[Map[String, _]] =
      if (fields.isEmpty)
        Decoder.succeed(Map())
      else
        keyDecoder.flatMap {
          case (wt, fieldNumber) =>
            if (fields.contains(fieldNumber)) {
              val (fieldName, schema) = fields(fieldNumber)

              wt match {
                case LengthDelimited(width) =>
                  for {
                    fieldValue <- decoder(schema).take(width)
                    remainder  <- recordDecoder(fields - fieldNumber)
                  } yield (remainder.updated(fieldName, fieldValue))

                case _ =>
                  for {
                    fieldValue <- decoder(schema)
                    remainder  <- recordDecoder(fields - fieldNumber)
                  } yield (remainder.updated(fieldName, fieldValue))
              }
            } else {
              fail(s"Schema doesn't contain field number $fieldNumber.")
            }
        }

    private def packedSequenceDecoder[A](schema: Schema[A]): Decoder[Chunk[A]] =
      decoder(schema).loop

    private def nonPackedSequenceDecoder[A](schema: Schema[A]): Decoder[Chunk[A]] =
      keyDecoder.flatMap {
        case (wt, _) =>
          wt match {
            case LengthDelimited(width) => decoder(schema).take(width)
            case _                      => fail("Unexpected wire type")
          }
      }.loop

    private def tupleDecoder[A, B](left: Schema[A], right: Schema[B]): Decoder[(A, B)] =
      decoder(tupleSchema(left, right))
        .flatMap(
          record =>
            new Decoder(
              chunk =>
                (record.get("first"), record.get("second")) match {
                  case (Some(first), Some(second)) => Right((chunk, (first.asInstanceOf[A], second.asInstanceOf[B])))
                  case _                           => Left("Error while decoding tuple.")
                }
            )
        )

    private def eitherDecoder[A, B](left: Schema[A], right: Schema[B]): Decoder[Either[A, B]] =
      keyDecoder.flatMap {
        case (_, fieldNumber) if fieldNumber == 1 => decoder(left).map(Left(_))
        case (_, fieldNumber) if fieldNumber == 2 => decoder(right).map(Right(_))
        case _                                    => fail("Failed to decode either.")
      }

    private def optionalDecoder[A](schema: Schema[A]): Decoder[Option[A]] =
      decoder(singleSchema(schema))
        .map(record => record.get("value").asInstanceOf[Option[A]])

    private def listDecoder[A](codec: Schema[A]): Decoder[List[A]] =
      decoder(singleSchema(codec))
        .map(record => record.get("value").asInstanceOf[List[A]])

    private def floatDecoder: Decoder[Float] =
      Decoder(bytes => {
        if (bytes.size < 4) {
          Left("Unable to decode Float")
        } else {
          Right((bytes, ByteBuffer.wrap(bytes.toArray).order(ByteOrder.LITTLE_ENDIAN).getFloat()))
        }
      }).take(4)

    private def doubleDecoder: Decoder[Double] =
      Decoder(bytes => {
        if (bytes.size < 8) {
          Left("Unable to decode Double")
        } else {
          Right((bytes, ByteBuffer.wrap(bytes.toArray).order(ByteOrder.LITTLE_ENDIAN).getDouble()))
        }
      }).take(8)

    private def transformDecoder[A, B](schema: Schema[B], f: B => Either[String, A]): Decoder[A] =
      decoder(schema).flatMap(a => Decoder(chunk => f(a).map(b => (chunk, b))))

    private def primitiveDecoder[A](standardType: StandardType[A]): Decoder[A] =
      standardType match {
        case StandardType.UnitType   => ((chunk: Chunk[Byte]) => Right((chunk, ()))).asInstanceOf[Decoder[A]]
        case StandardType.StringType => stringDecoder
        case StandardType.BoolType   => varIntDecoder.map(_ != 0)
        case StandardType.ShortType =>
          varIntDecoder.map(_.shortValue())
        case StandardType.IntType =>
          varIntDecoder.map(_.intValue())
        case StandardType.LongType   => varIntDecoder
        case StandardType.FloatType  => floatDecoder
        case StandardType.DoubleType => doubleDecoder
        case StandardType.BinaryType => binaryDecoder
        case StandardType.CharType   => stringDecoder.map(_.charAt(0))
        case StandardType.DayOfWeekType =>
          varIntDecoder.map(_.intValue).map(DayOfWeek.of)
        case StandardType.Month =>
          varIntDecoder.map(_.intValue).map(Month.of)
        case StandardType.MonthDay =>
          recordDecoder(flatFields(monthDayStructure()))
            .map(
              data =>
                MonthDay.of(data.getOrElse("month", 0).asInstanceOf[Int], data.getOrElse("day", 0).asInstanceOf[Int])
            )
        case StandardType.Period =>
          recordDecoder(flatFields(periodStructure()))
            .map(
              data =>
                Period.of(
                  data.getOrElse("years", 0).asInstanceOf[Int],
                  data.getOrElse("months", 0).asInstanceOf[Int],
                  data.getOrElse("days", 0).asInstanceOf[Int]
                )
            )
        case StandardType.Year =>
          varIntDecoder.map(_.intValue).map(Year.of)
        case StandardType.YearMonth =>
          recordDecoder(flatFields(yearMonthStructure()))
            .map(
              data =>
                YearMonth.of(data.getOrElse("year", 0).asInstanceOf[Int], data.getOrElse("month", 0).asInstanceOf[Int])
            )
        case StandardType.ZoneId => stringDecoder.map(ZoneId.of)
        case StandardType.ZoneOffset =>
          varIntDecoder
            .map(_.intValue)
            .map(ZoneOffset.ofTotalSeconds)
        case StandardType.Duration(_) =>
          recordDecoder(flatFields(durationStructure()))
            .map(
              data =>
                Duration.ofSeconds(
                  data.getOrElse("seconds", 0).asInstanceOf[Long],
                  data.getOrElse("nanos", 0).asInstanceOf[Int].toLong
                )
            )
        case StandardType.Instant(formatter) =>
          stringDecoder.map(v => Instant.from(formatter.parse(v)))
        case StandardType.LocalDate(formatter) =>
          stringDecoder.map(LocalDate.parse(_, formatter))
        case StandardType.LocalTime(formatter) =>
          stringDecoder.map(LocalTime.parse(_, formatter))
        case StandardType.LocalDateTime(formatter) =>
          stringDecoder.map(LocalDateTime.parse(_, formatter))
        case StandardType.OffsetTime(formatter) =>
          stringDecoder.map(OffsetTime.parse(_, formatter))
        case StandardType.OffsetDateTime(formatter) =>
          stringDecoder.map(OffsetDateTime.parse(_, formatter))
        case StandardType.ZonedDateTime(formatter) =>
          stringDecoder.map(ZonedDateTime.parse(_, formatter))
        case _ => fail("Unsupported primitive type")
      }

    /**
     * Decodes key which consist out of field type (wire type) and a field number.
     *
     * 8 >>> 3 => 1, 16 >>> 3 => 2, 24 >>> 3 => 3, 32 >>> 3 => 4
     * 0 & 0x07 => 0, 1 & 0x07 => 1, 2 & 0x07 => 2, 9 & 0x07 => 1, 15 & 0x07 => 7
     */
    private def keyDecoder: Decoder[(WireType, Int)] =
      varIntDecoder.flatMap { key =>
        val fieldNumber = (key >>> 3).toInt
        if (fieldNumber < 1) {
          fail("Failed decoding key: invalid field number")
        } else {
          key & 0x07 match {
            case 0 => succeed((WireType.VarInt, fieldNumber))
            case 1 => succeed((WireType.Bit64, fieldNumber))
            case 2 =>
              varIntDecoder.map(length => (WireType.LengthDelimited(length.toInt), fieldNumber))
            case 3 => succeed((WireType.StartGroup, fieldNumber))
            case 4 => succeed((WireType.EndGroup, fieldNumber))
            case 5 => succeed((WireType.Bit32, fieldNumber))
            case _ => fail("Failed decoding key: unknown wire type")
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
      Decoder(
        (chunk) =>
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
      )
  }
}
