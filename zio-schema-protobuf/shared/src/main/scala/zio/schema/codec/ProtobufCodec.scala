package zio.schema.codec

import java.math.{ BigInteger, MathContext }
import java.nio.charset.StandardCharsets
import java.nio.{ ByteBuffer, ByteOrder }
import java.time._
import java.util.UUID
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.util.control.{ NoStackTrace, NonFatal }
import zio.schema._
import zio.schema.codec.ProtobufCodec.Protobuf.WireType.LengthDelimited
import zio.stream.ZPipeline
import zio.{ Chunk, Unsafe, ZIO }

object ProtobufCodec extends Codec {
  override def encoder[A](schema: Schema[A]): ZPipeline[Any, Nothing, A, Byte] =
    ZPipeline.mapChunks(values => values.flatMap(Encoder.process(schema, _)))

  override def encode[A](schema: Schema[A]): A => Chunk[Byte] = a => Encoder.process(schema, a)

  override def decoder[A](schema: Schema[A]): ZPipeline[Any, String, Byte, A] =
    ZPipeline.mapChunksZIO(chunk => ZIO.fromEither(new Decoder(chunk).decode(schema).map(Chunk(_))))

  override def decode[A](schema: Schema[A]): Chunk[Byte] => scala.util.Either[String, A] =
    ch => new Decoder(ch).decode(schema)

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

  final private[codec] case class EncoderState(fieldNumber: Option[Int])

  object Encoder extends ProcessValueWithSchema[Chunk[Byte], EncoderState] {
    import Protobuf._

    override protected def processPrimitive(state: EncoderState, value: Any, typ: StandardType[Any]): Chunk[Byte] =
      encodePrimitive(state.fieldNumber, typ, value)

    override protected def processRecord(
      state: EncoderState,
      schema: Schema.Record[_],
      value: ListMap[String, Chunk[Byte]]
    ): Chunk[Byte] = {
      val encodedRecord = Chunk.fromIterable(value.values).flatten
      encodeKey(WireType.LengthDelimited(encodedRecord.size), state.fieldNumber) ++ encodedRecord
    }

    override protected def processEnum(
      state: EncoderState,
      schema: Schema.Enum[_],
      tuple: (String, Chunk[Byte])
    ): Chunk[Byte] = {
      val encoded = tuple._2
      encodeKey(WireType.LengthDelimited(encoded.size), state.fieldNumber) ++ encoded
    }

    override protected def processSequence(
      state: EncoderState,
      schema: Schema.Sequence[_, _, _],
      value: Chunk[Chunk[Byte]]
    ): Chunk[Byte] =
      if (value.isEmpty) {
        val data = encodeKey(WireType.LengthDelimited(0), Some(1))
        encodeKey(WireType.LengthDelimited(data.size), state.fieldNumber) ++ encodeKey(
          WireType.LengthDelimited(0),
          Some(1)
        )
      } else {
        val chunk = value.flatten
        val data = encodeKey(
          WireType.LengthDelimited(chunk.size),
          Some(2)
        ) ++ chunk

        encodeKey(WireType.LengthDelimited(data.size), state.fieldNumber) ++ data
      }

    override protected def processDictionary(
      state: EncoderState,
      schema: Schema.Map[_, _],
      value: Chunk[(Chunk[Byte], Chunk[Byte])]
    ): Chunk[Byte] =
      if (value.isEmpty) {
        val data = encodeKey(WireType.LengthDelimited(0), Some(1))
        encodeKey(WireType.LengthDelimited(data.size), state.fieldNumber) ++ encodeKey(
          WireType.LengthDelimited(0),
          Some(1)
        )
      } else {
        val chunk = value.map {
          case (left, right) =>
            val leftDecoder  = new Decoder(left)
            val rightDecoder = new Decoder(right)

            (
              leftDecoder.keyDecoder(DecoderContext(None, packed = false)),
              rightDecoder.keyDecoder(DecoderContext(None, packed = false))
            ) match {
              case ((leftWireType, seqIndex), (rightWireType, _)) =>
                val data =
                  encodeKey(leftWireType, Some(1)) ++
                    leftDecoder.remainder ++
                    encodeKey(rightWireType, Some(2)) ++
                    rightDecoder.remainder
                encodeKey(WireType.LengthDelimited(data.size), Some(seqIndex)) ++ data
              case other =>
                throw new IllegalStateException(s"Invalid state in processDictionary: $other")
            }
        }.flatten
        val data = encodeKey(
          WireType.LengthDelimited(chunk.size),
          Some(2)
        ) ++ chunk

        encodeKey(WireType.LengthDelimited(data.size), state.fieldNumber) ++ data
      }

    override protected def processSet(
      state: EncoderState,
      schema: Schema.Set[_],
      value: Set[Chunk[Byte]]
    ): Chunk[Byte] =
      if (value.isEmpty) {
        val data = encodeKey(WireType.LengthDelimited(0), Some(1))
        encodeKey(WireType.LengthDelimited(data.size), state.fieldNumber) ++ encodeKey(
          WireType.LengthDelimited(0),
          Some(1)
        )
      } else {
        val chunk = Chunk.fromIterable(value).flatten
        val data = encodeKey(
          WireType.LengthDelimited(chunk.size),
          Some(2)
        ) ++ chunk

        encodeKey(WireType.LengthDelimited(data.size), state.fieldNumber) ++ data
      }

    override protected def processEither(
      state: EncoderState,
      schema: Schema.Either[_, _],
      value: Either[Chunk[Byte], Chunk[Byte]]
    ): Chunk[Byte] = {
      val encodedEither = value.merge
      encodeKey(WireType.LengthDelimited(encodedEither.size), state.fieldNumber) ++ encodedEither
    }

    override protected def processOption(
      state: EncoderState,
      schema: Schema.Optional[_],
      value: Option[Chunk[Byte]]
    ): Chunk[Byte] = {
      val data = value match {
        case Some(bytes) => bytes
        case None        => encodeKey(WireType.LengthDelimited(0), Some(1))
      }

      encodeKey(WireType.LengthDelimited(data.size), state.fieldNumber) ++ data
    }

    override protected def processTuple(
      state: EncoderState,
      schema: Schema.Tuple2[_, _],
      left: Chunk[Byte],
      right: Chunk[Byte]
    ): Chunk[Byte] = {
      val data = left ++ right
      encodeKey(WireType.LengthDelimited(data.size), state.fieldNumber) ++ data
    }

    override protected def processDynamic(state: EncoderState, value: DynamicValue): Option[Chunk[Byte]] =
      None

    override protected def fail(state: EncoderState, message: String): Chunk[Byte] =
      throw new RuntimeException(message)

    override protected val initialState: EncoderState = EncoderState(None)

    override protected def stateForRecordField(
      state: EncoderState,
      index: Int,
      field: Schema.Field[_, _]
    ): EncoderState =
      state.copy(fieldNumber = Some(index + 1))

    override protected def stateForTuple(state: EncoderState, index: Int): EncoderState =
      state.copy(fieldNumber = Some(index))

    override protected def stateForEnumConstructor(
      state: EncoderState,
      index: Int,
      c: Schema.Case[_, _]
    ): EncoderState =
      state.copy(fieldNumber = Some(index + 1))

    override protected def stateForEither(state: EncoderState, e: Either[Unit, Unit]): EncoderState =
      e match {
        case Left(_)  => state.copy(fieldNumber = Some(1))
        case Right(_) => state.copy(fieldNumber = Some(2))
      }

    override protected def stateForOption(state: EncoderState, o: Option[Unit]): EncoderState =
      o match {
        case None    => state.copy(fieldNumber = Some(1))
        case Some(_) => state.copy(fieldNumber = Some(2))
      }

    override protected def stateForSequence(
      state: EncoderState,
      s: Schema.Sequence[_, _, _],
      index: Int
    ): EncoderState =
      if (canBePacked(s.elementSchema)) state.copy(fieldNumber = None)
      else state.copy(fieldNumber = Some(index + 1))

    override protected def stateForMap(state: EncoderState, s: Schema.Map[_, _], index: Int): EncoderState =
      if (canBePacked(s.keySchema <*> s.valueSchema)) state.copy(fieldNumber = None)
      else state.copy(fieldNumber = Some(index + 1))

    override protected def stateForSet(state: EncoderState, s: Schema.Set[_], index: Int): EncoderState =
      if (canBePacked(s.elementSchema)) state.copy(fieldNumber = None)
      else state.copy(fieldNumber = Some(index + 1))

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

          val encodedRecord =
            encodePrimitive(Some(1), StandardType.BigIntegerType, unscaled) ++
              encodePrimitive(Some(2), StandardType.IntType, precision) ++
              encodePrimitive(Some(3), StandardType.IntType, scale)

          encodeKey(WireType.LengthDelimited(encodedRecord.size), fieldNumber) ++ encodedRecord

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
          val encodedRecord =
            encodePrimitive(Some(1), StandardType.IntType, v.getMonthValue) ++
              encodePrimitive(Some(2), StandardType.IntType, v.getDayOfMonth)

          encodeKey(WireType.LengthDelimited(encodedRecord.size), fieldNumber) ++ encodedRecord
        case (StandardType.PeriodType, v: Period) =>
          val encodedRecord =
            encodePrimitive(Some(1), StandardType.IntType, v.getYears) ++
              encodePrimitive(Some(2), StandardType.IntType, v.getMonths) ++
              encodePrimitive(Some(3), StandardType.IntType, v.getDays)

          encodeKey(WireType.LengthDelimited(encodedRecord.size), fieldNumber) ++ encodedRecord
        case (StandardType.YearType, v: Year) =>
          encodePrimitive(fieldNumber, StandardType.IntType, v.getValue)
        case (StandardType.YearMonthType, v: YearMonth) =>
          val encodedRecord =
            encodePrimitive(Some(1), StandardType.IntType, v.getYear) ++
              encodePrimitive(Some(2), StandardType.IntType, v.getMonthValue)

          encodeKey(WireType.LengthDelimited(encodedRecord.size), fieldNumber) ++ encodedRecord
        case (StandardType.ZoneIdType, v: ZoneId) =>
          encodePrimitive(fieldNumber, StandardType.StringType, v.getId)
        case (StandardType.ZoneOffsetType, v: ZoneOffset) =>
          encodePrimitive(fieldNumber, StandardType.IntType, v.getTotalSeconds)
        case (StandardType.DurationType, v: Duration) =>
          val encodedRecord =
            encodePrimitive(Some(1), StandardType.LongType, v.getSeconds) ++
              encodePrimitive(Some(2), StandardType.IntType, v.getNano)

          encodeKey(WireType.LengthDelimited(encodedRecord.size), fieldNumber) ++ encodedRecord
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

  final class DecoderState(chunk: Chunk[Byte], private var position: Int) {
    def length(context: DecoderContext): Int = context.limit.getOrElse(chunk.length) - position

    def read(count: Int): Chunk[Byte] = {
      val oldPosition = position
      position += count
      chunk.slice(oldPosition, position)
    }

    def all(context: DecoderContext): Chunk[Byte] = read(length(context))

    def peek(context: DecoderContext): Chunk[Byte] =
      chunk.slice(position, position + length(context))

    def move(count: Int): Unit =
      position += count

    def currentPosition: Int = position
  }

  final case class DecoderContext(limit: Option[Int], packed: Boolean) {

    def limitedTo(state: DecoderState, w: Int): DecoderContext =
      copy(limit = Some(state.currentPosition + w))
  }

  final case class DecoderException(message: String) extends RuntimeException(message) with NoStackTrace

  class Decoder(chunk: Chunk[Byte]) extends CreateValueFromSchema[Any, DecoderContext] {

    import Protobuf._

    private val state: DecoderState = new DecoderState(chunk, 0)

    def decode[A](schema: Schema[A]): scala.util.Either[String, A] =
      try {
        Right(create(schema).asInstanceOf[A])
      } catch {
        case DecoderException(message) =>
          Left(message)
      }

    private def createTypedPrimitive[A](context: DecoderContext, standardType: StandardType[A]): A =
      createPrimitive(context, standardType).asInstanceOf[A]

    override protected def createPrimitive(context: DecoderContext, typ: StandardType[_]): Any =
      typ match {
        case StandardType.UnitType   => ()
        case StandardType.StringType => stringDecoder(context)
        case StandardType.BoolType   => varIntDecoder(context) != 0
        case StandardType.ShortType  => varIntDecoder(context).shortValue
        case StandardType.ByteType   => varIntDecoder(context).byteValue
        case StandardType.IntType    => varIntDecoder(context).intValue
        case StandardType.LongType   => varIntDecoder(context)
        case StandardType.FloatType  => floatDecoder(context)
        case StandardType.DoubleType => doubleDecoder(context)
        case StandardType.BigIntegerType =>
          val bytes = binaryDecoder(context)
          new java.math.BigInteger(bytes.toArray)
        case StandardType.BigDecimalType =>
          val unscaled  = createTypedPrimitive(rawFieldDecoder(context, 1), StandardType.BigIntegerType)
          val scale     = createTypedPrimitive(rawFieldDecoder(context, 2), StandardType.IntType)
          val precision = createTypedPrimitive(rawFieldDecoder(context, 3), StandardType.IntType)
          val ctx       = new java.math.MathContext(precision)
          new java.math.BigDecimal(unscaled, scale, ctx)

        case StandardType.BinaryType => binaryDecoder(context)
        case StandardType.CharType   => stringDecoder(context).charAt(0)
        case StandardType.UUIDType =>
          val uuid = stringDecoder(context)
          try UUID.fromString(uuid)
          catch {
            case NonFatal(_) => throw DecoderException(s"Invalid UUID string $uuid")
          }
        case StandardType.DayOfWeekType =>
          DayOfWeek.of(varIntDecoder(context).intValue)
        case StandardType.MonthType =>
          Month.of(varIntDecoder(context).intValue)
        case StandardType.MonthDayType =>
          val month = createTypedPrimitive(rawFieldDecoder(context, 1), StandardType.IntType)
          val day   = createTypedPrimitive(rawFieldDecoder(context, 2), StandardType.IntType)
          MonthDay.of(month, day)

        case StandardType.PeriodType =>
          val years  = createTypedPrimitive(rawFieldDecoder(context, 1), StandardType.IntType)
          val months = createTypedPrimitive(rawFieldDecoder(context, 2), StandardType.IntType)
          val days   = createTypedPrimitive(rawFieldDecoder(context, 3), StandardType.IntType)
          Period.of(years, months, days)
        case StandardType.YearType =>
          Year.of(varIntDecoder(context).intValue)
        case StandardType.YearMonthType =>
          val year  = createTypedPrimitive(rawFieldDecoder(context, 1), StandardType.IntType)
          val month = createTypedPrimitive(rawFieldDecoder(context, 2), StandardType.IntType)
          YearMonth.of(year, month)
        case StandardType.ZoneIdType => ZoneId.of(stringDecoder(context))
        case StandardType.ZoneOffsetType =>
          ZoneOffset.ofTotalSeconds(varIntDecoder(context).intValue)
        case StandardType.DurationType =>
          val seconds = createTypedPrimitive(rawFieldDecoder(context, 1), StandardType.LongType)
          val nanos   = createTypedPrimitive(rawFieldDecoder(context, 2), StandardType.IntType)
          Duration.ofSeconds(seconds, nanos.toLong)
        case StandardType.InstantType(formatter) =>
          Instant.from(formatter.parse(stringDecoder(context)))
        case StandardType.LocalDateType(formatter) =>
          LocalDate.parse(stringDecoder(context), formatter)
        case StandardType.LocalTimeType(formatter) =>
          LocalTime.parse(stringDecoder(context), formatter)
        case StandardType.LocalDateTimeType(formatter) =>
          LocalDateTime.parse(stringDecoder(context), formatter)
        case StandardType.OffsetTimeType(formatter) =>
          OffsetTime.parse(stringDecoder(context), formatter)
        case StandardType.OffsetDateTimeType(formatter) =>
          OffsetDateTime.parse(stringDecoder(context), formatter)
        case StandardType.ZonedDateTimeType(formatter) =>
          ZonedDateTime.parse(stringDecoder(context), formatter)
        case st => throw DecoderException(s"Unsupported primitive type $st")
      }

    override protected def startCreatingRecord(context: DecoderContext, record: Schema.Record[_]): DecoderContext =
      context

    override protected def startReadingField(
      context: DecoderContext,
      record: Schema.Record[_],
      index: Int
    ): (DecoderContext, Option[Int]) =
      if (index == record.fields.size) {
        (context, None)
      } else {
        keyDecoder(context) match {
          case (wt, fieldNumber) =>
            if (record.fields.isDefinedAt(fieldNumber - 1)) {
              wt match {
                case LengthDelimited(width) =>
                  (context.limitedTo(state, width), Some(fieldNumber - 1))
                case _ =>
                  (context, Some(fieldNumber - 1))
              }
            } else {
              throw DecoderException(s"Failed to decode record. Schema does not contain field number $fieldNumber.")
            }
        }
      }

    override protected def createRecord(
      context: DecoderContext,
      record: Schema.Record[_],
      values: Chunk[(Int, Any)]
    ): Any =
      Unsafe.unsafe { implicit u =>
        record.construct(values.map(_._2)) match {
          case Right(result) => result
          case Left(message) => throw DecoderException(message)
        }
      }

    override protected def startCreatingEnum(
      context: DecoderContext,
      cases: Chunk[Schema.Case[_, _]]
    ): (DecoderContext, Int) =
      keyDecoder(context) match {
        case (wt, fieldNumber) if fieldNumber <= cases.length =>
          wt match {
            case LengthDelimited(width) =>
              (context.limitedTo(state, width), fieldNumber - 1)
            case _ =>
              (context, fieldNumber - 1)
          }
        case (_, fieldNumber) =>
          throw DecoderException(s"Failed to decode enumeration. Schema does not contain field number $fieldNumber.")
      }

    override protected def createEnum(
      context: DecoderContext,
      cases: Chunk[Schema.Case[_, _]],
      index: Int,
      value: Any
    ): Any =
      value

    override protected def startCreatingSequence(
      context: DecoderContext,
      schema: Schema.Sequence[_, _, _]
    ): Option[DecoderContext] =
      keyDecoder(context) match {
        case (LengthDelimited(0), 1) =>
          None
        case (LengthDelimited(width), 2) =>
          Some(context.limitedTo(state, width).copy(packed = canBePacked(schema.elementSchema)))
        case (wt, fieldNumber) =>
          throw DecoderException(s"Invalid wire type ($wt) or field number ($fieldNumber) for packed sequence")
      }

    override protected def startReadingOneSequenceElement(
      context: DecoderContext,
      schema: Schema.Sequence[_, _, _]
    ): DecoderContext =
      if (context.packed)
        context
      else {
        keyDecoder(context) match {
          case (wt, _) =>
            wt match {
              case LengthDelimited(elemWidth) =>
                context.limitedTo(state, elemWidth)
              case _ =>
                throw DecoderException(s"Unexpected wire type $wt for non-packed sequence")
            }
        }
      }

    override protected def readOneSequenceElement(
      context: DecoderContext,
      schema: Schema.Sequence[_, _, _],
      index: Int
    ): (DecoderContext, Boolean) =
      (context, state.length(context) > 0)

    override protected def createSequence(
      context: DecoderContext,
      schema: Schema.Sequence[_, _, _],
      values: Chunk[Any]
    ): Any =
      schema.fromChunk.asInstanceOf[Chunk[Any] => Any](values)

    override protected def startCreatingDictionary(
      context: DecoderContext,
      schema: Schema.Map[_, _]
    ): Option[DecoderContext] =
      keyDecoder(context) match {
        case (LengthDelimited(0), 1) =>
          None
        case (LengthDelimited(width), 2) =>
          Some(context.limitedTo(state, width).copy(packed = canBePacked(schema.keySchema.zip(schema.valueSchema))))
        case (wt, fieldNumber) =>
          throw DecoderException(s"Invalid wire type ($wt) or field number ($fieldNumber) for packed sequence")
      }

    override protected def startReadingOneDictionaryElement(
      context: DecoderContext,
      schema: Schema.Map[_, _]
    ): DecoderContext =
      if (context.packed) {
        context
      } else {
        keyDecoder(context) match {
          case (wt, _) =>
            wt match {
              case LengthDelimited(elemWidth) =>
                context.limitedTo(state, elemWidth)
              case _ =>
                throw DecoderException(s"Unexpected wire type $wt for non-packed sequence")
            }
        }
      }

    override protected def readOneDictionaryElement(
      context: DecoderContext,
      schema: Schema.Map[_, _],
      index: Int
    ): (DecoderContext, Boolean) =
      (context, state.length(context) > 0)

    override protected def createDictionary(
      context: DecoderContext,
      schema: Schema.Map[_, _],
      values: Chunk[(Any, Any)]
    ): Any =
      values.toMap

    override protected def startCreatingSet(context: DecoderContext, schema: Schema.Set[_]): Option[DecoderContext] =
      keyDecoder(context) match {
        case (LengthDelimited(0), 1) =>
          None
        case (LengthDelimited(width), 2) =>
          Some(context.limitedTo(state, width).copy(packed = canBePacked(schema.elementSchema)))
        case (wt, fieldNumber) =>
          throw DecoderException(s"Invalid wire type ($wt) or field number ($fieldNumber) for packed sequence")
      }

    override protected def startReadingOneSetElement(context: DecoderContext, schema: Schema.Set[_]): DecoderContext =
      if (context.packed) {
        context
      } else {
        keyDecoder(context) match {
          case (wt, _) =>
            wt match {
              case LengthDelimited(elemWidth) =>
                context.limitedTo(state, elemWidth)
              case _ =>
                throw DecoderException(s"Unexpected wire type $wt for non-packed sequence")
            }
        }
      }

    override protected def readOneSetElement(
      context: DecoderContext,
      schema: Schema.Set[_],
      index: Int
    ): (DecoderContext, Boolean) =
      (context, state.length(context) > 0)

    override protected def createSet(context: DecoderContext, schema: Schema.Set[_], values: Chunk[Any]): Any =
      values.toSet

    override protected def startCreatingOptional(
      context: DecoderContext,
      schema: Schema.Optional[_]
    ): Option[DecoderContext] =
      keyDecoder(context) match {
        case (LengthDelimited(0), 1)     => None
        case (LengthDelimited(width), 2) => Some(context.limitedTo(state, width))
        case (_, 2)                      => Some(context)
        case (_, fieldNumber)            => throw DecoderException(s"Invalid field number $fieldNumber for option")
      }

    override protected def createOptional(
      context: DecoderContext,
      schema: Schema.Optional[_],
      value: Option[Any]
    ): Any =
      value

    override protected def startCreatingEither(
      context: DecoderContext,
      schema: Schema.Either[_, _]
    ): Either[DecoderContext, DecoderContext] =
      keyDecoder(context) match {
        case (_, fieldNumber) if fieldNumber == 1 => Left(context)
        case (_, fieldNumber) if fieldNumber == 2 => Right(context)
        case (_, fieldNumber)                     => throw DecoderException(s"Invalid field number ($fieldNumber) for either")
      }

    override protected def createEither(
      context: DecoderContext,
      schema: Schema.Either[_, _],
      value: Either[Any, Any]
    ): Any =
      value

    override protected def startCreatingTuple(context: DecoderContext, schema: Schema.Tuple2[_, _]): DecoderContext =
      keyDecoder(context) match {
        case (wt, 1) =>
          wt match {
            case LengthDelimited(width) => context.limitedTo(state, width)
            case _                      => context
          }
        case (_, fieldNumber) =>
          throw DecoderException(s"Invalid field number ($fieldNumber) for tuple's first field")
      }

    override protected def startReadingSecondTupleElement(
      context: DecoderContext,
      schema: Schema.Tuple2[_, _]
    ): DecoderContext =
      keyDecoder(context) match {
        case (wt, 2) =>
          wt match {
            case LengthDelimited(width) => context.limitedTo(state, width)
            case _                      => context
          }
        case (_, fieldNumber) =>
          throw DecoderException(s"Invalid field number ($fieldNumber) for tuple's second field")
      }

    override protected def createTuple(
      context: DecoderContext,
      schema: Schema.Tuple2[_, _],
      left: Any,
      right: Any
    ): Any =
      (left, right)

    override protected def createDynamic(context: DecoderContext): Option[Any] =
      None

    override protected def transform(context: DecoderContext, value: Any, f: Any => Either[String, Any]): Any =
      f(value) match {
        case Left(value)  => throw DecoderException(value)
        case Right(value) => value
      }

    override protected def fail(context: DecoderContext, message: String): Any =
      throw DecoderException(message)

    override protected val initialState: DecoderContext = DecoderContext(limit = None, packed = false)

    /**
     * Decodes key which consist out of field type (wire type) and a field number.
     *
     * 8 >>> 3 => 1, 16 >>> 3 => 2, 24 >>> 3 => 3, 32 >>> 3 => 4
     * 0 & 0x07 => 0, 1 & 0x07 => 1, 2 & 0x07 => 2, 9 & 0x07 => 1, 15 & 0x07 => 7
     */
    private[codec] def keyDecoder(context: DecoderContext): (WireType, Int) = {
      val key         = varIntDecoder(context)
      val fieldNumber = (key >>> 3).toInt
      if (fieldNumber < 1) {
        throw DecoderException(s"Failed decoding key. Invalid field number $fieldNumber")
      } else {
        key & 0x07 match {
          case 0 => (WireType.VarInt, fieldNumber)
          case 1 => (WireType.Bit64, fieldNumber)
          case 2 =>
            val length = varIntDecoder(context)
            (WireType.LengthDelimited(length.toInt), fieldNumber)
          case 3 => (WireType.StartGroup, fieldNumber)
          case 4 => (WireType.EndGroup, fieldNumber)
          case 5 => (WireType.Bit32, fieldNumber)
          case n => throw DecoderException(s"Failed decoding key. Unknown wire type $n")
        }
      }
    }

    private def rawFieldDecoder(context: DecoderContext, expectedFieldNumber: Int): DecoderContext =
      keyDecoder(context) match {
        case (wt, fieldNumber) if fieldNumber == expectedFieldNumber =>
          wt match {
            case LengthDelimited(width) =>
              context.limitedTo(state, width)
            case _ =>
              context
          }
        case _ =>
          throw DecoderException(s"Failed to decode record. Schema does not contain field number $expectedFieldNumber.")
      }

    private def floatDecoder(context: DecoderContext): Float =
      if (state.length(context) < 4)
        throw DecoderException(s"Invalid number of bytes for Float. Expected 4, got ${state.length(context)}")
      else {
        val bytes = state.read(4)
        ByteBuffer.wrap(bytes.toArray).order(ByteOrder.LITTLE_ENDIAN).getFloat()
      }

    private def doubleDecoder(context: DecoderContext): Double =
      if (state.length(context) < 8)
        throw DecoderException(s"Invalid number of bytes for Double. Expected 8, got ${state.length(context)}")
      else {
        val bytes = state.read(8)
        ByteBuffer.wrap(bytes.toArray).order(ByteOrder.LITTLE_ENDIAN).getDouble()
      }

    private def stringDecoder(context: DecoderContext): String = {
      val bytes = state.all(context)
      new String(bytes.toArray, StandardCharsets.UTF_8)
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
    private def varIntDecoder(context: DecoderContext): Long =
      if (state.length(context) == 0) {
        throw DecoderException("Failed to decode VarInt. Unexpected end of chunk")
      } else {
        val chunk  = state.peek(context)
        val length = chunk.indexWhere(octet => (octet.longValue() & 0x80) != 0x80) + 1
        if (length <= 0) {
          throw DecoderException("Failed to decode VarInt. No byte within the range 0 - 127 are present")
        } else {
          state.move(length)
          chunk.take(length).foldRight(0L)((octet, v) => (v << 7) + (octet & 0x7F))
        }
      }

    private def binaryDecoder(context: DecoderContext): Chunk[Byte] =
      state.all(context)

    private[codec] def remainder: Chunk[Byte] =
      state.peek(DecoderContext(None, packed = false))
  }

}
