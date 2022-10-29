package zio.schema.codec

import java.math.{ BigInteger, MathContext }
import java.nio.ByteBuffer
import java.time._
import java.util.UUID

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.util.Try
import scala.util.control.NonFatal

import org.apache.thrift.protocol._

import zio.schema._
import zio.stream.ZPipeline
import zio.{ Chunk, Unsafe, ZIO }

object ThriftCodec extends Codec {
  override def encoder[A](schema: Schema[A]): ZPipeline[Any, Nothing, A, Byte] = {
    val encoder = new Encoder()
    ZPipeline.mapChunks { chunk =>
      chunk.flatMap(encoder.encode(schema, _))
    }
  }

  override def decoder[A](schema: Schema[A]): ZPipeline[Any, String, Byte, A] =
    ZPipeline.mapChunksZIO { chunk =>
      ZIO.fromEither(
        new Decoder(chunk)
          .create(schema)
          .map(Chunk(_))
          .map(_.asInstanceOf[Chunk[A]])
          .left
          .map(err => s"Error at path /${err.path.mkString(".")}: ${err.error}")
      )
    }

  override def encode[A](schema: Schema[A]): A => Chunk[Byte] = a => new Encoder().encode(schema, a)

  override def decode[A](schema: Schema[A]): Chunk[Byte] => scala.util.Either[String, A] =
    ch =>
      if (ch.isEmpty)
        Left("No bytes to decode")
      else
        new Decoder(ch)
          .create(schema)
          .map(_.asInstanceOf[A])
          .left
          .map(
            err => s"Error at path /${err.path.mkString(".")}: ${err.error}"
          )

  class Encoder extends ProcessValueWithSchema[Unit, Encoder.State] {
    import Encoder._

    override protected def processPrimitive(state: State, value: Any, typ: StandardType[Any]): Unit = {
      writeFieldBegin(state.fieldNumber, getPrimitiveType(typ))
      writePrimitiveType(typ, value)
    }

    override protected def startProcessingRecord(state: State, schema: Schema.Record[_]): Unit =
      writeFieldBegin(state.fieldNumber, TType.STRUCT)

    override protected def processRecord(
      state: State,
      schema: Schema.Record[_],
      value: ListMap[String, Unit]
    ): Unit =
      writeFieldEnd()

    override protected def startProcessingEnum(state: State, schema: Schema.Enum[_]): Unit =
      writeFieldBegin(state.fieldNumber, TType.STRUCT)

    override protected def processEnum(state: State, schema: Schema.Enum[_], tuple: (String, Unit)): Unit =
      writeFieldEnd()

    override protected def startProcessingSequence(state: State, schema: Schema.Sequence[_, _, _], size: Int): Unit = {
      writeFieldBegin(state.fieldNumber, TType.LIST)
      writeListBegin(getType(schema.elementSchema), size)
    }

    override protected def processSequence(
      state: State,
      schema: Schema.Sequence[_, _, _],
      value: Chunk[Unit]
    ): Unit = {}

    override protected def startProcessingDictionary(state: State, schema: Schema.Map[_, _], size: Int): Unit = {
      writeFieldBegin(state.fieldNumber, TType.MAP)
      writeMapBegin(getType(schema.keySchema), getType(schema.valueSchema), size)
    }

    override protected def processDictionary(
      state: State,
      schema: Schema.Map[_, _],
      value: Chunk[(Unit, Unit)]
    ): Unit = {}

    override protected def startProcessingSet(state: State, schema: Schema.Set[_], size: Int): Unit = {
      writeFieldBegin(state.fieldNumber, TType.SET)
      writeSetBegin(getType(schema.elementSchema), size)
    }

    override protected def processSet(state: State, schema: Schema.Set[_], value: Set[Unit]): Unit = {}

    override protected def startProcessingEither(state: State, schema: Schema.Either[_, _]): Unit =
      writeFieldBegin(state.fieldNumber, TType.STRUCT)

    override protected def processEither(
      state: State,
      schema: Schema.Either[_, _],
      value: Either[Unit, Unit]
    ): Unit =
      writeFieldEnd()

    override def startProcessingOption(state: State, schema: Schema.Optional[_]): Unit =
      writeFieldBegin(state.fieldNumber, TType.STRUCT)

    override protected def processOption(state: State, schema: Schema.Optional[_], value: Option[Unit]): Unit = {
      value match {
        case None =>
          processPrimitive(
            state.copy(fieldNumber = Some(1)),
            (),
            StandardType.UnitType.asInstanceOf[StandardType[Any]]
          )
        case _ =>
      }
      writeFieldEnd()
    }

    override protected def startProcessingTuple(state: State, schema: Schema.Tuple2[_, _]): Unit =
      writeFieldBegin(state.fieldNumber, TType.STRUCT)

    override protected def processTuple(
      state: State,
      schema: Schema.Tuple2[_, _],
      left: Unit,
      right: Unit
    ): Unit =
      writeFieldEnd()

    override protected def fail(state: State, message: String): Unit =
      fail(message)

    override protected def processDynamic(state: State, value: DynamicValue): Option[Unit] =
      None

    override protected val initialState: State = State(None)

    override protected def stateForRecordField(state: State, index: Int, field: Schema.Field[_, _]): State =
      state.copy(fieldNumber = Some((index + 1).toShort))

    override protected def stateForEnumConstructor(state: State, index: Int, c: Schema.Case[_, _]): State =
      state.copy(fieldNumber = Some((index + 1).toShort))

    override protected def stateForEither(state: State, e: Either[Unit, Unit]): State =
      e match {
        case Left(_)  => state.copy(fieldNumber = Some(1))
        case Right(_) => state.copy(fieldNumber = Some(2))
      }

    override protected def stateForOption(state: State, o: Option[Unit]): State =
      o match {
        case None    => state.copy(fieldNumber = Some(1))
        case Some(_) => state.copy(fieldNumber = Some(2))
      }

    override protected def stateForTuple(state: State, index: Int): State =
      state.copy(fieldNumber = Some(index.toShort))

    override protected def stateForSequence(state: State, schema: Schema.Sequence[_, _, _], index: Int): State =
      state.copy(fieldNumber = None)

    override protected def stateForMap(state: State, schema: Schema.Map[_, _], index: Int): State =
      state.copy(fieldNumber = None)

    override protected def stateForSet(state: State, schema: Schema.Set[_], index: Int): State =
      state.copy(fieldNumber = None)

    private[codec] def encode[A](schema: Schema[A], value: A): Chunk[Byte] = {
      process(schema, value)
      write.chunk
    }

    private val write = new ChunkTransport.Write()
    private val p     = new TBinaryProtocol(write)

    private def writeFieldBegin(fieldNumber: Option[Short], ttype: Byte): Unit =
      fieldNumber match {
        case Some(num) =>
          p.writeFieldBegin(
            new TField("", ttype, num)
          )
        case None =>
      }

    private def writeFieldEnd(): Unit =
      p.writeFieldStop()

    private def writeString(value: String): Unit =
      p.writeString(value)

    private def writeBool(value: Boolean): Unit =
      p.writeBool(value)

    private def writeByte(value: Byte): Unit =
      p.writeByte(value)

    private def writeI16(value: Short): Unit =
      p.writeI16(value)

    private def writeI32(value: Int): Unit =
      p.writeI32(value)

    private def writeI64(value: Long): Unit =
      p.writeI64(value)

    private def writeDouble(value: Double): Unit =
      p.writeDouble(value)

    private def writeBinary(value: Chunk[Byte]): Unit =
      p.writeBinary(ByteBuffer.wrap(value.toArray))

    private def writeListBegin(ttype: Byte, count: Int): Unit =
      p.writeListBegin(new TList(ttype, count))

    private def writeSetBegin(ttype: Byte, count: Int): Unit =
      p.writeSetBegin(new TSet(ttype, count))

    private def writeMapBegin(keyType: Byte, valueType: Byte, count: Int): Unit =
      p.writeMapBegin(new TMap(keyType, valueType, count))

    private def fail(message: String): Unit = throw new RuntimeException(message)

    private def writePrimitiveType[A](standardType: StandardType[A], value: A): Unit =
      (standardType, value) match {
        case (StandardType.UnitType, _) =>
        case (StandardType.StringType, str: String) =>
          writeString(str)
        case (StandardType.BoolType, b: Boolean) =>
          writeBool(b)
        case (StandardType.ByteType, v: Byte) =>
          writeByte(v)
        case (StandardType.ShortType, v: Short) =>
          writeI16(v)
        case (StandardType.IntType, v: Int) =>
          writeI32(v)
        case (StandardType.LongType, v: Long) =>
          writeI64(v)
        case (StandardType.FloatType, v: Float) =>
          writeDouble(v.toDouble)
        case (StandardType.DoubleType, v: Double) =>
          writeDouble(v.toDouble)
        case (StandardType.BigIntegerType, v: java.math.BigInteger) =>
          writeBinary(Chunk.fromArray(v.toByteArray))
        case (StandardType.BigDecimalType, v: java.math.BigDecimal) =>
          val unscaled  = v.unscaledValue()
          val precision = v.precision()
          val scale     = v.scale()
          writeFieldBegin(Some(1), getPrimitiveType(StandardType.BigIntegerType))
          writePrimitiveType(StandardType.BigIntegerType, unscaled)
          writeFieldBegin(Some(2), getPrimitiveType(StandardType.IntType))
          writePrimitiveType(StandardType.IntType, precision)
          writeFieldBegin(Some(3), getPrimitiveType(StandardType.IntType))
          writePrimitiveType(StandardType.IntType, scale)
          writeFieldEnd()

        case (StandardType.BinaryType, bytes: Chunk[Byte]) =>
          writeBinary(Chunk.fromArray(bytes.toArray))
        case (StandardType.CharType, c: Char) =>
          writeString(c.toString)
        case (StandardType.UUIDType, u: UUID) =>
          writeString(u.toString)
        case (StandardType.DayOfWeekType, v: DayOfWeek) =>
          writeByte(v.getValue.toByte)
        case (StandardType.MonthType, v: Month) =>
          writeByte(v.getValue.toByte)
        case (StandardType.MonthDayType, v: MonthDay) =>
          writeFieldBegin(Some(1), getPrimitiveType(StandardType.IntType))
          writePrimitiveType(StandardType.IntType, v.getMonthValue)
          writeFieldBegin(Some(2), getPrimitiveType(StandardType.IntType))
          writePrimitiveType(StandardType.IntType, v.getDayOfMonth)
          writeFieldEnd()

        case (StandardType.PeriodType, v: Period) =>
          writeFieldBegin(Some(1), getPrimitiveType(StandardType.IntType))
          writePrimitiveType(StandardType.IntType, v.getYears)
          writeFieldBegin(Some(2), getPrimitiveType(StandardType.IntType))
          writePrimitiveType(StandardType.IntType, v.getMonths)
          writeFieldBegin(Some(3), getPrimitiveType(StandardType.IntType))
          writePrimitiveType(StandardType.IntType, v.getDays)
          writeFieldEnd()

        case (StandardType.YearType, v: Year) =>
          writeI32(v.getValue)
        case (StandardType.YearMonthType, v: YearMonth) =>
          writeFieldBegin(Some(1), getPrimitiveType(StandardType.IntType))
          writePrimitiveType(StandardType.IntType, v.getYear)
          writeFieldBegin(Some(2), getPrimitiveType(StandardType.IntType))
          writePrimitiveType(StandardType.IntType, v.getMonthValue)
          writeFieldEnd()
        case (StandardType.ZoneIdType, v: ZoneId) =>
          writeString(v.getId)
        case (StandardType.ZoneOffsetType, v: ZoneOffset) =>
          writeI32(v.getTotalSeconds)
        case (StandardType.DurationType, v: Duration) =>
          writeFieldBegin(Some(1), getPrimitiveType(StandardType.LongType))
          writePrimitiveType(StandardType.LongType, v.getSeconds)
          writeFieldBegin(Some(2), getPrimitiveType(StandardType.IntType))
          writePrimitiveType(StandardType.IntType, v.getNano)
          writeFieldEnd()

        case (StandardType.InstantType(formatter), v: Instant) =>
          writeString(formatter.format(v))
        case (StandardType.LocalDateType(formatter), v: LocalDate) =>
          writeString(formatter.format(v))
        case (StandardType.LocalTimeType(formatter), v: LocalTime) =>
          writeString(formatter.format(v))
        case (StandardType.LocalDateTimeType(formatter), v: LocalDateTime) =>
          writeString(formatter.format(v))
        case (StandardType.OffsetTimeType(formatter), v: OffsetTime) =>
          writeString(formatter.format(v))
        case (StandardType.OffsetDateTimeType(formatter), v: OffsetDateTime) =>
          writeString(formatter.format(v))
        case (StandardType.ZonedDateTimeType(formatter), v: ZonedDateTime) =>
          writeString(formatter.format(v))
        case (_, _) =>
          fail(s"No encoder for $standardType")
      }
  }

  object Encoder {
    final case class State(fieldNumber: Option[Short])

    private def getPrimitiveType[A](standardType: StandardType[A]): Byte =
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

    @tailrec
    final private def getType[A](schema: Schema[A]): Byte = schema match {
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
  }

  case class Error(path: Path, error: String)

  type Path               = Chunk[String]
  type Result[A]          = scala.util.Either[Error, A]
  type PrimitiveResult[A] = Path => Result[A]

  final case class DecoderState(path: Path, remainingCount: Option[Int])

  class Decoder(chunk: Chunk[Byte]) extends CreateValueFromSchema[Result[Any], DecoderState] {

    val read = new ChunkTransport.Read(chunk)
    val p    = new TBinaryProtocol(read)

    def succeed[A](a: => A): Result[A] = Right(a)

    def flip[A](as: Chunk[Result[A]]): Result[Chunk[A]] =
      as.foldLeft[Result[Chunk[A]]](Right(Chunk.empty[A])) {
        case (Right(as), Right(a)) => Right(as :+ a)
        case (Left(failure), _)    => Left(failure)
        case (_, Left(failure))    => Left(failure)
      }

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

    override protected def createPrimitive(state: DecoderState, typ: StandardType[_]): Result[Any] =
      typ match {
        case StandardType.UnitType       => Right(())
        case StandardType.StringType     => decodeString(state.path)
        case StandardType.BoolType       => decodeBoolean(state.path)
        case StandardType.ByteType       => decodeByte(state.path)
        case StandardType.ShortType      => decodeShort(state.path)
        case StandardType.IntType        => decodeInt(state.path)
        case StandardType.LongType       => decodeLong(state.path)
        case StandardType.FloatType      => decodeFloat(state.path)
        case StandardType.DoubleType     => decodeDouble(state.path)
        case StandardType.BigIntegerType => decodeBigInteger(state.path)
        case StandardType.BigDecimalType =>
          p.readFieldBegin()
          decodeBigInteger(state.path).flatMap { unscaled =>
            p.readFieldBegin()
            decodeInt(state.path).flatMap { precision =>
              p.readFieldBegin()
              decodeInt(state.path).map { scale =>
                p.readFieldBegin()
                new java.math.BigDecimal(unscaled, scale, new java.math.MathContext(precision))
              }
            }
          }
        case StandardType.BinaryType => decodeBinary(state.path)
        case StandardType.CharType =>
          decodeString(state.path).flatMap(
            decoded =>
              if (decoded.length == 1)
                succeed(decoded.charAt(0))
              else {
                fail(state, s"""Expected character, found string "$decoded"""")
              }
          )
        case StandardType.UUIDType =>
          decodeString(state.path).flatMap { uuid =>
            try succeed(UUID.fromString(uuid))
            catch {
              case NonFatal(_) => fail(state, "Invalid UUID string")
            }
          }
        case StandardType.DayOfWeekType =>
          decodeByte(state.path).map(_.toInt).map(DayOfWeek.of)
        case StandardType.MonthType =>
          decodeByte(state.path).map(_.toInt).map(Month.of)
        case StandardType.MonthDayType =>
          p.readFieldBegin()
          decodeInt(state.path).flatMap { month =>
            p.readFieldBegin()
            decodeInt(state.path).map { day =>
              p.readFieldBegin()
              MonthDay.of(month, day)
            }
          }
        case StandardType.PeriodType =>
          p.readFieldBegin()
          decodeInt(state.path).flatMap { year =>
            p.readFieldBegin()
            decodeInt(state.path).flatMap { month =>
              p.readFieldBegin()
              decodeInt(state.path).map { day =>
                p.readFieldBegin()
                Period.of(year, month, day)
              }
            }
          }
        case StandardType.YearType =>
          decodeInt(state.path).map(_.intValue).map(Year.of)
        case StandardType.YearMonthType =>
          p.readFieldBegin()
          decodeInt(state.path).flatMap { year =>
            p.readFieldBegin()
            decodeInt(state.path).map { month =>
              p.readFieldBegin()
              YearMonth.of(year, month)
            }
          }
        case StandardType.ZoneIdType => decodeString(state.path).map(ZoneId.of)
        case StandardType.ZoneOffsetType =>
          decodeInt(state.path)
            .map(_.intValue)
            .map(ZoneOffset.ofTotalSeconds)
        case StandardType.DurationType =>
          p.readFieldBegin()
          decodeLong(state.path).flatMap { seconds =>
            p.readFieldBegin()
            decodeInt(state.path).map { nano =>
              p.readFieldBegin()
              Duration.ofSeconds(seconds, nano.toLong)
            }
          }
        case StandardType.InstantType(formatter) =>
          decodeString(state.path).map(v => Instant.from(formatter.parse(v)))
        case StandardType.LocalDateType(formatter) =>
          decodeString(state.path).map(LocalDate.parse(_, formatter))
        case StandardType.LocalTimeType(formatter) =>
          decodeString(state.path).map(LocalTime.parse(_, formatter))
        case StandardType.LocalDateTimeType(formatter) =>
          decodeString(state.path).map(LocalDateTime.parse(_, formatter))
        case StandardType.OffsetTimeType(formatter) =>
          decodeString(state.path).map(OffsetTime.parse(_, formatter))
        case StandardType.OffsetDateTimeType(formatter) =>
          decodeString(state.path).map(OffsetDateTime.parse(_, formatter))
        case StandardType.ZonedDateTimeType(formatter) =>
          decodeString(state.path).map(ZonedDateTime.parse(_, formatter))
        case _ => fail(state, s"Unsupported primitive type $typ")
      }

    override protected def startCreatingRecord(state: DecoderState, record: Schema.Record[_]): DecoderState =
      state

    override protected def startReadingField(
      state: DecoderState,
      record: Schema.Record[_],
      index: Int
    ): (DecoderState, Option[Int]) = {
      val tfield = p.readFieldBegin()
      (state, if (tfield.`type` == TType.STOP) None else Some(tfield.id - 1))
    }

    override protected def createRecord(
      state: DecoderState,
      record: Schema.Record[_],
      values: Chunk[(Int, Result[Any])]
    ): Result[Any] = {
      val valuesMap = values.toMap
      val allValues =
        record.fields.zipWithIndex.map {
          case (field, idx) =>
            valuesMap.get(idx) match {
              case Some(value) => value
              case None =>
                emptyValue(field.schema) match {
                  case Some(value) => succeed(value)
                  case None        => fail(state, s"Missing value for field ${field.name}")
                }
            }
        }
      Unsafe.unsafe { implicit u =>
        flip(allValues).flatMap { vs =>
          record.construct(vs) match {
            case Left(message) => fail(state, message)
            case Right(value)  => succeed(value)
          }
        }
      }
    }

    override protected def startCreatingEnum(
      state: DecoderState,
      cases: Chunk[Schema.Case[_, _]]
    ): (DecoderState, Int) = {
      val readField   = p.readFieldBegin()
      val consIdx     = readField.id - 1
      val subtypeCase = cases(consIdx)
      (state.copy(path = state.path :+ s"[case:${subtypeCase.id}]"), consIdx)
    }

    override protected def createEnum(
      state: DecoderState,
      cases: Chunk[Schema.Case[_, _]],
      index: Int,
      value: Result[Any]
    ): Result[Any] = {
      value.foreach { _ =>
        p.readFieldBegin()
      }
      value
    }

    override protected def startCreatingSequence(
      state: DecoderState,
      schema: Schema.Sequence[_, _, _]
    ): Option[DecoderState] = {
      val begin = p.readListBegin()
      if (begin.size == 0) None
      else
        Some(state.copy(remainingCount = Some(begin.size)))
    }

    override protected def readOneSequenceElement(
      state: DecoderState,
      schema: Schema.Sequence[_, _, _]
    ): (DecoderState, Boolean) = {
      val updatedState = state.copy(remainingCount = state.remainingCount.map(_ - 1))
      val continue     = updatedState.remainingCount.exists(_ > 0)
      (updatedState, continue)
    }

    override protected def createSequence(
      state: DecoderState,
      schema: Schema.Sequence[_, _, _],
      values: Chunk[Result[Any]]
    ): Result[Any] =
      flip(values).map(chunk => schema.fromChunk.asInstanceOf[Chunk[Any] => Any](chunk))

    override protected def startCreatingDictionary(
      state: DecoderState,
      schema: Schema.Map[_, _]
    ): Option[DecoderState] = {
      val begin = p.readMapBegin()
      if (begin.size == 0) None
      else
        Some(state.copy(remainingCount = Some(begin.size)))
    }

    override protected def readOneDictionaryElement(
      state: DecoderState,
      schema: Schema.Map[_, _]
    ): (DecoderState, Boolean) = {
      val updatedState = state.copy(remainingCount = state.remainingCount.map(_ - 1))
      val continue     = updatedState.remainingCount.exists(_ > 0)
      (updatedState, continue)
    }

    override protected def createDictionary(
      state: DecoderState,
      schema: Schema.Map[_, _],
      values: Chunk[(Result[Any], Result[Any])]
    ): Result[Any] =
      flip(values.map {
        case (aa, bb) =>
          aa.flatMap { a =>
            bb.map { b =>
              (a, b)
            }
          }
      }).map(_.toMap)

    override protected def startCreatingSet(state: DecoderState, schema: Schema.Set[_]): Option[DecoderState] = {
      val begin = p.readSetBegin()
      if (begin.size == 0) None
      else Some(state.copy(remainingCount = Some(begin.size)))
    }

    override protected def readOneSetElement(state: DecoderState, schema: Schema.Set[_]): (DecoderState, Boolean) = {
      val updatedState = state.copy(remainingCount = state.remainingCount.map(_ - 1))
      val continue     = updatedState.remainingCount.exists(_ > 0)
      (updatedState, continue)
    }

    override protected def createSet(
      state: DecoderState,
      schema: Schema.Set[_],
      values: Chunk[Result[Any]]
    ): Result[Any] =
      flip(values).map(_.toSet)

    override protected def startCreatingOptional(
      state: DecoderState,
      schema: Schema.Optional[_]
    ): Option[DecoderState] = {
      val field = p.readFieldBegin()
      field.id match {
        case 1 => None
        case 2 => Some(state.copy(path = state.path :+ "Some"))
        // TODO
      }
    }

    override protected def createOptional(
      state: DecoderState,
      schema: Schema.Optional[_],
      value: Option[Result[Any]]
    ): Result[Any] = {
      p.readFieldBegin()
      value match {
        case Some(value) => value.map(Some(_))
        case None        => succeed(None)
      }
    }

    override protected def startCreatingEither(
      state: DecoderState,
      schema: Schema.Either[_, _]
    ): Either[DecoderState, DecoderState] = {
      val readField = p.readFieldBegin()
      readField.id match {
        case 1 => Left(state.copy(path = state.path :+ "either:left"))
        case 2 => Right(state.copy(path = state.path :+ "either:right"))
        //case _ => fail(path, "Failed to decode either.") // TODO
      }
    }

    override protected def createEither(
      state: DecoderState,
      schema: Schema.Either[_, _],
      value: Either[Result[Any], Result[Any]]
    ): Result[Any] =
      value match {
        case Left(value)  => value.map(Left(_))
        case Right(value) => value.map(Right(_))
      }

    override protected def startCreatingTuple(state: DecoderState, schema: Schema.Tuple2[_, _]): DecoderState = {
      p.readFieldBegin()
      state
    }

    override protected def startReadingSecondTupleElement(
      state: DecoderState,
      schema: Schema.Tuple2[_, _]
    ): DecoderState = {
      p.readFieldBegin()
      state
    }

    override protected def createTuple(
      state: DecoderState,
      schema: Schema.Tuple2[_, _],
      left: Result[Any],
      right: Result[Any]
    ): Result[Any] = {
      p.readFieldBegin()
      left.flatMap { l =>
        right.map { r =>
          (l, r)
        }
      }
    }

    override protected def createDynamic(state: DecoderState): Option[Result[Any]] =
      None

    override protected def transform(
      state: DecoderState,
      value: Result[Any],
      f: Any => Either[String, Any]
    ): Result[Any] =
      value.flatMap { v =>
        f(v) match {
          case Left(value)  => fail(state, value)
          case Right(value) => succeed(value)
        }
      }

    override protected def fail(state: DecoderState, message: String): Result[Any] =
      Left(Error(state.path, message))

    override protected val initialState: DecoderState = DecoderState(Chunk.empty, None)

    private def emptyValue[A](schema: Schema[A]): Option[A] = schema match {
      case Schema.Lazy(s)                             => emptyValue(s())
      case Schema.Optional(_, _)                      => Some(None)
      case Schema.Sequence(_, fromChunk, _, _, _)     => Some(fromChunk(Chunk.empty))
      case Schema.Primitive(StandardType.UnitType, _) => Some(())
      case _                                          => None
    }
  }
}
