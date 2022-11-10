package zio.schema.codec

import java.nio.ByteBuffer
import java.time._
import java.util.UUID

import scala.annotation.{ nowarn, tailrec }
import scala.collection.immutable.ListMap
import scala.util.control.NonFatal

import org.apache.thrift.protocol._

import zio.schema.MutableSchemaBasedValueBuilder.CreateValueFromSchemaError
import zio.schema._
import zio.schema.annotation.{ fieldDefaultValue, optionalField, transientField }
import zio.schema.codec.BinaryCodec.{ BinaryDecoder, BinaryEncoder, BinaryStreamDecoder, BinaryStreamEncoder }
import zio.schema.codec.DecodeError.{ EmptyContent, MalformedFieldWithPath, ReadError, ReadErrorWithPath }
import zio.stream.ZPipeline
import zio.{ Cause, Chunk, Unsafe, ZIO }

object ThriftCodec extends BinaryCodec {

  override def encoderFor[A](schema: Schema[A]): BinaryEncoder[A] =
    new BinaryEncoder[A] {

      override def encode(value: A): Chunk[Byte] =
        new Encoder().encode(schema, value)

      override def streamEncoder: BinaryStreamEncoder[A] = {
        val encoder = new Encoder()
        ZPipeline.mapChunks { chunk =>
          chunk.flatMap(encoder.encode(schema, _))
        }
      }

    }

  override def decoderFor[A](schema: Schema[A]): BinaryDecoder[A] =
    new BinaryDecoder[A] {

      override def decode(chunk: Chunk[Byte]): Either[DecodeError, A] =
        if (chunk.isEmpty)
          Left(EmptyContent("No bytes to decode"))
        else
          decodeChunk(chunk)

      override def streamDecoder: BinaryStreamDecoder[A] =
        ZPipeline.mapChunksZIO { chunk =>
          ZIO.fromEither(
            decodeChunk(chunk).map(Chunk(_))
          )
        }

      private def decodeChunk(chunk: Chunk[Byte]): Either[DecodeError, A] =
        if (chunk.isEmpty)
          Left(EmptyContent("No bytes to decode"))
        else {
          try {
            Right(
              new Decoder(chunk)
                .create(schema)
                .asInstanceOf[A]
            )
          } catch {
            case error: CreateValueFromSchemaError[DecoderContext] =>
              error.cause match {
                case error: DecodeError => Left(error)
                case _ =>
                  Left(
                    ReadErrorWithPath(error.context.path, Cause.fail(error.cause), error.cause.getMessage)
                  )
              }
            case NonFatal(err) =>
              Left(ReadError(Cause.fail(err), err.getMessage))
          }
        }: @nowarn

    }

  class Encoder extends MutableSchemaBasedValueProcessor[Unit, Encoder.Context] {
    import Encoder._

    override protected def processPrimitive(context: Context, value: Any, typ: StandardType[Any]): Unit = {
      writeFieldBegin(context.fieldNumber, getPrimitiveType(typ))
      writePrimitiveType(typ, value)
    }

    override protected def startProcessingRecord(context: Context, schema: Schema.Record[_]): Unit =
      if (schema.fields.nonEmpty) {
        writeFieldBegin(context.fieldNumber, TType.STRUCT)
      } else {
        writeFieldBegin(context.fieldNumber, TType.BYTE)
        writeByte(0)
      }

    override protected def processRecord(
      context: Context,
      schema: Schema.Record[_],
      value: ListMap[String, Unit]
    ): Unit =
      if (schema.fields.nonEmpty) {
        writeFieldEnd()
      }

    override protected def startProcessingEnum(context: Context, schema: Schema.Enum[_]): Unit =
      writeFieldBegin(context.fieldNumber, TType.STRUCT)

    override protected def processEnum(context: Context, schema: Schema.Enum[_], tuple: (String, Unit)): Unit =
      writeFieldEnd()

    override protected def startProcessingSequence(
      context: Context,
      schema: Schema.Sequence[_, _, _],
      size: Int
    ): Unit = {
      writeFieldBegin(context.fieldNumber, TType.LIST)
      writeListBegin(getType(schema.elementSchema), size)
    }

    override protected def processSequence(
      context: Context,
      schema: Schema.Sequence[_, _, _],
      value: Chunk[Unit]
    ): Unit = {}

    override protected def startProcessingDictionary(context: Context, schema: Schema.Map[_, _], size: Int): Unit = {
      writeFieldBegin(context.fieldNumber, TType.MAP)
      writeMapBegin(getType(schema.keySchema), getType(schema.valueSchema), size)
    }

    override protected def processDictionary(
      context: Context,
      schema: Schema.Map[_, _],
      value: Chunk[(Unit, Unit)]
    ): Unit = {}

    override protected def startProcessingSet(context: Context, schema: Schema.Set[_], size: Int): Unit = {
      writeFieldBegin(context.fieldNumber, TType.SET)
      writeSetBegin(getType(schema.elementSchema), size)
    }

    override protected def processSet(context: Context, schema: Schema.Set[_], value: Set[Unit]): Unit = {}

    override protected def startProcessingEither(context: Context, schema: Schema.Either[_, _]): Unit =
      writeFieldBegin(context.fieldNumber, TType.STRUCT)

    override protected def processEither(
      context: Context,
      schema: Schema.Either[_, _],
      value: Either[Unit, Unit]
    ): Unit =
      writeFieldEnd()

    override def startProcessingOption(context: Context, schema: Schema.Optional[_]): Unit =
      writeFieldBegin(context.fieldNumber, TType.STRUCT)

    override protected def processOption(context: Context, schema: Schema.Optional[_], value: Option[Unit]): Unit = {
      value match {
        case None =>
          processPrimitive(
            context.copy(fieldNumber = Some(1)),
            (),
            StandardType.UnitType.asInstanceOf[StandardType[Any]]
          )
        case _ =>
      }
      writeFieldEnd()
    }

    override protected def startProcessingTuple(context: Context, schema: Schema.Tuple2[_, _]): Unit =
      writeFieldBegin(context.fieldNumber, TType.STRUCT)

    override protected def processTuple(
      context: Context,
      schema: Schema.Tuple2[_, _],
      left: Unit,
      right: Unit
    ): Unit =
      writeFieldEnd()

    override protected def fail(context: Context, message: String): Unit =
      fail(message)

    override protected def processDynamic(context: Context, value: DynamicValue): Option[Unit] =
      None

    override protected val initialContext: Context = Context(None)

    override protected def contextForRecordField(context: Context, index: Int, field: Schema.Field[_, _]): Context =
      context.copy(fieldNumber = Some((index + 1).toShort))

    override protected def contextForEnumConstructor(context: Context, index: Int, c: Schema.Case[_, _]): Context =
      context.copy(fieldNumber = Some((index + 1).toShort))

    override protected def contextForEither(context: Context, e: Either[Unit, Unit]): Context =
      e match {
        case Left(_)  => context.copy(fieldNumber = Some(1))
        case Right(_) => context.copy(fieldNumber = Some(2))
      }

    override protected def contextForOption(context: Context, o: Option[Unit]): Context =
      o match {
        case None    => context.copy(fieldNumber = Some(1))
        case Some(_) => context.copy(fieldNumber = Some(2))
      }

    override protected def contextForTuple(context: Context, index: Int): Context =
      context.copy(fieldNumber = Some(index.toShort))

    override protected def contextForSequence(context: Context, schema: Schema.Sequence[_, _, _], index: Int): Context =
      context.copy(fieldNumber = None)

    override protected def contextForMap(context: Context, schema: Schema.Map[_, _], index: Int): Context =
      context.copy(fieldNumber = None)

    override protected def contextForSet(context: Context, schema: Schema.Set[_], index: Int): Context =
      context.copy(fieldNumber = None)

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
    final case class Context(fieldNumber: Option[Short])

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

  type Path                = Chunk[String]
  type PrimitiveDecoder[A] = Path => A

  final case class DecoderContext(path: Path, expectedCount: Option[Int])

  class Decoder(chunk: Chunk[Byte]) extends MutableSchemaBasedValueBuilder[Any, DecoderContext] {

    val read = new ChunkTransport.Read(chunk)
    val p    = new TBinaryProtocol(read)

    def decodePrimitive[A](f: TProtocol => A, name: String): PrimitiveDecoder[A] =
      path =>
        try {
          f(p)
        } catch {
          case NonFatal(_) => throw MalformedFieldWithPath(path, s"Unable to decode $name")
        }

    def decodeString: PrimitiveDecoder[String] =
      decodePrimitive(_.readString(), "String")

    def decodeUUID: PrimitiveDecoder[UUID] =
      decodePrimitive(protocol => UUID.fromString(protocol.readString()), "UUID")

    def decodeByte: PrimitiveDecoder[Byte] =
      decodePrimitive(_.readByte(), "Byte")

    def decodeBoolean: PrimitiveDecoder[Boolean] =
      decodePrimitive(_.readBool(), "Boolean")

    def decodeShort: PrimitiveDecoder[Short] =
      decodePrimitive(_.readI16(), "Short")

    def decodeInt: PrimitiveDecoder[Int] =
      decodePrimitive(_.readI32(), "Int")

    def decodeLong: PrimitiveDecoder[Long] =
      decodePrimitive(_.readI64(), "Long")

    def decodeFloat: PrimitiveDecoder[Float] =
      decodePrimitive(_.readDouble().toFloat, "Float")

    def decodeDouble: PrimitiveDecoder[Double] =
      decodePrimitive(_.readDouble(), "Double")

    def decodeBigInteger: PrimitiveDecoder[java.math.BigInteger] =
      decodePrimitive(p => new java.math.BigInteger(p.readBinary().array()), "BigInteger")

    def decodeBinary: PrimitiveDecoder[Chunk[Byte]] =
      decodePrimitive(p => Chunk.fromByteBuffer(p.readBinary()), "Binary")

    override protected def createPrimitive(context: DecoderContext, typ: StandardType[_]): Any =
      typ match {
        case StandardType.UnitType       => ()
        case StandardType.StringType     => decodeString(context.path)
        case StandardType.BoolType       => decodeBoolean(context.path)
        case StandardType.ByteType       => decodeByte(context.path)
        case StandardType.ShortType      => decodeShort(context.path)
        case StandardType.IntType        => decodeInt(context.path)
        case StandardType.LongType       => decodeLong(context.path)
        case StandardType.FloatType      => decodeFloat(context.path)
        case StandardType.DoubleType     => decodeDouble(context.path)
        case StandardType.BigIntegerType => decodeBigInteger(context.path)
        case StandardType.BigDecimalType =>
          p.readFieldBegin()
          val unscaled = decodeBigInteger(context.path)
          p.readFieldBegin()
          val precision = decodeInt(context.path)
          p.readFieldBegin()
          val scale = decodeInt(context.path)
          p.readFieldBegin()
          new java.math.BigDecimal(unscaled, scale, new java.math.MathContext(precision))

        case StandardType.BinaryType => decodeBinary(context.path)
        case StandardType.CharType =>
          val decoded = decodeString(context.path)

          if (decoded.length == 1)
            decoded.charAt(0)
          else {
            fail(context, s"""Expected character, found string "$decoded"""")
          }

        case StandardType.UUIDType =>
          decodeUUID(context.path)
        case StandardType.DayOfWeekType =>
          DayOfWeek.of(decodeByte(context.path).toInt)
        case StandardType.MonthType =>
          Month.of(decodeByte(context.path).toInt)
        case StandardType.MonthDayType =>
          p.readFieldBegin()
          val month = decodeInt(context.path)
          p.readFieldBegin()
          val day = decodeInt(context.path)
          p.readFieldBegin()
          MonthDay.of(month, day)

        case StandardType.PeriodType =>
          p.readFieldBegin()
          val year = decodeInt(context.path)
          p.readFieldBegin()
          val month = decodeInt(context.path)
          p.readFieldBegin()
          val day = decodeInt(context.path)
          p.readFieldBegin()
          Period.of(year, month, day)

        case StandardType.YearType =>
          Year.of(decodeInt(context.path).intValue)
        case StandardType.YearMonthType =>
          p.readFieldBegin()
          val year = decodeInt(context.path)
          p.readFieldBegin()
          val month = decodeInt(context.path)
          p.readFieldBegin()
          YearMonth.of(year, month)

        case StandardType.ZoneIdType =>
          ZoneId.of(decodeString(context.path))

        case StandardType.ZoneOffsetType =>
          ZoneOffset.ofTotalSeconds(decodeInt(context.path).intValue)
        case StandardType.DurationType =>
          p.readFieldBegin()
          val seconds = decodeLong(context.path)
          p.readFieldBegin()
          val nano = decodeInt(context.path)
          p.readFieldBegin()
          Duration.ofSeconds(seconds, nano.toLong)

        case StandardType.InstantType(formatter) =>
          Instant.from(formatter.parse(decodeString(context.path)))
        case StandardType.LocalDateType(formatter) =>
          LocalDate.parse(decodeString(context.path), formatter)
        case StandardType.LocalTimeType(formatter) =>
          LocalTime.parse(decodeString(context.path), formatter)
        case StandardType.LocalDateTimeType(formatter) =>
          LocalDateTime.parse(decodeString(context.path), formatter)
        case StandardType.OffsetTimeType(formatter) =>
          OffsetTime.parse(decodeString(context.path), formatter)
        case StandardType.OffsetDateTimeType(formatter) =>
          OffsetDateTime.parse(decodeString(context.path), formatter)
        case StandardType.ZonedDateTimeType(formatter) =>
          ZonedDateTime.parse(decodeString(context.path), formatter)
        case _ => fail(context, s"Unsupported primitive type $typ")
      }

    override protected def startCreatingRecord(context: DecoderContext, record: Schema.Record[_]): DecoderContext =
      context

    override protected def startReadingField(
      context: DecoderContext,
      record: Schema.Record[_],
      index: Int
    ): Option[(DecoderContext, Int)] =
      if (record.fields.nonEmpty) {
        val tfield = p.readFieldBegin()
        if (tfield.`type` == TType.STOP) None
        else Some((context.copy(path = context.path :+ s"fieldId:${tfield.id}"), tfield.id - 1))
      } else {
        val _ = p.readByte()
        None
      }

    override protected def createRecord(
      context: DecoderContext,
      record: Schema.Record[_],
      values: Chunk[(Int, Any)]
    ): Any =
      if (record.fields.nonEmpty) {
        val valuesMap = values.toMap
        val allValues =
          record.fields.zipWithIndex.map {
            case (field, idx) =>
              valuesMap.get(idx) match {
                case Some(value) => value
                case None =>
                  emptyValue(field.schema) match {
                    case Some(value) =>
                      value
                    case None =>
                      val optionalFieldAnnotation  = field.annotations.collectFirst({ case a: optionalField  => a })
                      val transientFieldAnnotation = field.annotations.collectFirst({ case a: transientField => a })
                      val fieldDefaultValueAnnotation = field.annotations.collectFirst {
                        case a: fieldDefaultValue[_] => a
                      }
                      if (optionalFieldAnnotation.isDefined || transientFieldAnnotation.isDefined) {
                        field.schema.defaultValue.toOption.get
                      } else if (fieldDefaultValueAnnotation.isDefined) {
                        fieldDefaultValueAnnotation.get.value
                      } else {
                        fail(context.copy(path = context.path :+ field.name), s"Missing value")
                      }
                  }
              }
          }
        Unsafe.unsafe { implicit u =>
          record.construct(allValues) match {
            case Left(message) => fail(context, message)
            case Right(value)  => value
          }
        }
      } else {
        Unsafe.unsafe { implicit u =>
          record.construct(Chunk.empty) match {
            case Left(message) => fail(context, message)
            case Right(value)  => value
          }
        }
      }

    override protected def startCreatingEnum(
      context: DecoderContext,
      cases: Chunk[Schema.Case[_, _]]
    ): (DecoderContext, Int) = {
      val readField   = p.readFieldBegin()
      val consIdx     = readField.id - 1
      val subtypeCase = cases(consIdx)
      (context.copy(path = context.path :+ s"[case:${subtypeCase.id}]"), consIdx)
    }

    override protected def createEnum(
      context: DecoderContext,
      cases: Chunk[Schema.Case[_, _]],
      index: Int,
      value: Any
    ): Any = {
      p.readFieldBegin()
      value
    }

    override protected def startCreatingSequence(
      context: DecoderContext,
      schema: Schema.Sequence[_, _, _]
    ): Option[DecoderContext] = {
      val begin = p.readListBegin()
      if (begin.size == 0) None
      else
        Some(context.copy(expectedCount = Some(begin.size)))
    }

    override protected def startCreatingOneSequenceElement(
      context: DecoderContext,
      schema: Schema.Sequence[_, _, _]
    ): DecoderContext =
      context

    override protected def finishedCreatingOneSequenceElement(
      context: DecoderContext,
      schema: Schema.Sequence[_, _, _],
      index: Int
    ): Boolean =
      context.expectedCount.map(_ - (index + 1)).exists(_ > 0)

    override protected def createSequence(
      context: DecoderContext,
      schema: Schema.Sequence[_, _, _],
      values: Chunk[Any]
    ): Any =
      schema.fromChunk.asInstanceOf[Chunk[Any] => Any](values)

    override protected def startCreatingDictionary(
      context: DecoderContext,
      schema: Schema.Map[_, _]
    ): Option[DecoderContext] = {
      val begin = p.readMapBegin()
      if (begin.size == 0) None
      else
        Some(context.copy(expectedCount = Some(begin.size)))
    }

    override protected def startCreatingOneDictionaryElement(
      context: DecoderContext,
      schema: Schema.Map[_, _]
    ): DecoderContext =
      context

    override protected def startCreatingOneDictionaryValue(
      context: DecoderContext,
      schema: Schema.Map[_, _]
    ): DecoderContext =
      context

    override protected def finishedCreatingOneDictionaryElement(
      context: DecoderContext,
      schema: Schema.Map[_, _],
      index: Int
    ): Boolean =
      context.expectedCount.map(_ - (index + 1)).exists(_ > 0)

    override protected def createDictionary(
      context: DecoderContext,
      schema: Schema.Map[_, _],
      values: Chunk[(Any, Any)]
    ): Any =
      values.toMap

    override protected def startCreatingSet(context: DecoderContext, schema: Schema.Set[_]): Option[DecoderContext] = {
      val begin = p.readSetBegin()
      if (begin.size == 0) None
      else Some(context.copy(expectedCount = Some(begin.size)))
    }

    override protected def startCreatingOneSetElement(context: DecoderContext, schema: Schema.Set[_]): DecoderContext =
      context

    override protected def finishedCreatingOneSetElement(
      context: DecoderContext,
      schema: Schema.Set[_],
      index: Int
    ): Boolean =
      context.expectedCount.map(_ - (index + 1)).exists(_ > 0)

    override protected def createSet(
      context: DecoderContext,
      schema: Schema.Set[_],
      values: Chunk[Any]
    ): Any =
      values.toSet

    override protected def startCreatingOptional(
      context: DecoderContext,
      schema: Schema.Optional[_]
    ): Option[DecoderContext] = {
      val field = p.readFieldBegin()
      field.id match {
        case 1 => None
        case 2 => Some(context.copy(path = context.path :+ "Some"))
        case id =>
          fail(context, s"Error decoding optional, wrong field id $id").asInstanceOf[Option[DecoderContext]]
      }
    }

    override protected def createOptional(
      context: DecoderContext,
      schema: Schema.Optional[_],
      value: Option[Any]
    ): Any = {
      p.readFieldBegin()
      value
    }

    override protected def startCreatingEither(
      context: DecoderContext,
      schema: Schema.Either[_, _]
    ): Either[DecoderContext, DecoderContext] = {
      val readField = p.readFieldBegin()
      readField.id match {
        case 1 => Left(context.copy(path = context.path :+ "either:left"))
        case 2 => Right(context.copy(path = context.path :+ "either:right"))
        case _ => fail(context, "Failed to decode either.").asInstanceOf[Either[DecoderContext, DecoderContext]]
      }
    }

    override protected def createEither(
      context: DecoderContext,
      schema: Schema.Either[_, _],
      value: Either[Any, Any]
    ): Any =
      value

    override protected def startCreatingTuple(context: DecoderContext, schema: Schema.Tuple2[_, _]): DecoderContext = {
      p.readFieldBegin()
      context
    }

    override protected def startReadingSecondTupleElement(
      context: DecoderContext,
      schema: Schema.Tuple2[_, _]
    ): DecoderContext = {
      p.readFieldBegin()
      context
    }

    override protected def createTuple(
      context: DecoderContext,
      schema: Schema.Tuple2[_, _],
      left: Any,
      right: Any
    ): Any = {
      p.readFieldBegin()
      (left, right)
    }

    override protected def createDynamic(context: DecoderContext): Option[Any] =
      None

    override protected def transform(
      context: DecoderContext,
      value: Any,
      f: Any => Either[String, Any],
      schema: Schema[_]
    ): Any =
      f(value) match {
        case Left(value)  => fail(context, value)
        case Right(value) => value
      }

    override protected def fail(context: DecoderContext, message: String): Any =
      throw MalformedFieldWithPath(context.path, message)

    override protected val initialContext: DecoderContext = DecoderContext(Chunk.empty, None)

    private def emptyValue[A](schema: Schema[A]): Option[A] = schema match {
      case Schema.Lazy(s)                             => emptyValue(s())
      case Schema.Optional(_, _)                      => Some(None)
      case Schema.Sequence(_, fromChunk, _, _, _)     => Some(fromChunk(Chunk.empty))
      case Schema.Primitive(StandardType.UnitType, _) => Some(())
      case _                                          => None
    }
  }
}
