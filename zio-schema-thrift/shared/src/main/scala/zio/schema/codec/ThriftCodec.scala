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
import zio.schema.codec.ThriftCodec.Thrift.{
  bigDecimalStructure,
  durationStructure,
  monthDayStructure,
  periodStructure,
  yearMonthStructure
}
import zio.stream.ZPipeline
import zio.{ Chunk, ChunkBuilder, ZIO }

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
          .decode(Chunk.empty, schema)
          .map(Chunk(_))
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
          .decode(Chunk.empty, schema)
          .left
          .map(
            err => s"Error at path /${err.path.mkString(".")}: ${err.error}"
          )

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

  // TODO: delete Encoder.Command and write directly in processXXX
  class Encoder extends ProcessValueWithSchema[Encoder.Command, Encoder.State] {
    import Encoder._

    override protected def processPrimitive(state: State, value: Any, typ: StandardType[Any]): Encoder.Command =
      Command.Sequence(Command.WriteFieldBegin(state.fieldNumber, getPrimitiveType(typ))) ++
        writePrimitiveType(typ, value)

    override protected def processRecord(
      state: State,
      schema: Schema.Record[_],
      value: ListMap[String, Command]
    ): Command =
      Command.Sequence(Command.WriteFieldBegin(state.fieldNumber, TType.STRUCT)) ++
        Chunk.fromIterable(value.values) ++
        Command.WriteFieldEnd

    override protected def processEnum(state: State, schema: Schema.Enum[_], tuple: (String, Command)): Command =
      Command.Sequence(
        Command.WriteFieldBegin(state.fieldNumber, TType.STRUCT),
        tuple._2,
        Command.WriteFieldEnd
      )

    override protected def processSequence(
      state: State,
      schema: Schema.Sequence[_, _, _],
      value: Chunk[Command]
    ): Command =
      Command.Sequence(
        Command.WriteFieldBegin(state.fieldNumber, TType.LIST),
        Command.WriteListBegin(getType(schema.elementSchema), value.size)
      ) ++ value

    override protected def processDictionary(
      state: State,
      schema: Schema.Map[_, _],
      value: Chunk[(Command, Command)]
    ): Command =
      value
        .foldLeft(
          Command.Sequence(
            Command.WriteFieldBegin(state.fieldNumber, TType.MAP),
            Command.WriteMapBegin(getType(schema.keySchema), getType(schema.valueSchema), value.size)
          )
        ) { case (c, (c1, c2)) => c ++ c1 ++ c2 }

    override protected def processSet(state: State, schema: Schema.Set[_], value: Set[Command]): Command =
      Command.Sequence(
        Command.WriteFieldBegin(state.fieldNumber, TType.SET),
        Command.WriteSetBegin(getType(schema.elementSchema), value.size)
      ) ++ Chunk.fromIterable(value)

    override protected def processEither(
      state: State,
      schema: Schema.Either[_, _],
      value: Either[Command, Command]
    ): Command =
      Command.Sequence(Command.WriteFieldBegin(state.fieldNumber, TType.STRUCT)) ++ value.merge

    override protected def processOption(state: State, schema: Schema.Optional[_], value: Option[Command]): Command =
      Command.Sequence(Command.WriteFieldBegin(state.fieldNumber, TType.STRUCT)) ++
        (value match {
          case Some(value) => value
          case None =>
            processPrimitive(
              state.copy(fieldNumber = Some(1)),
              (),
              StandardType.UnitType.asInstanceOf[StandardType[Any]]
            )
        }) ++ Command.WriteFieldEnd

    override protected def processTuple(
      state: State,
      schema: Schema.Tuple2[_, _],
      left: Command,
      right: Command
    ): Command =
      Command.Sequence(
        Command.WriteFieldBegin(state.fieldNumber, TType.STRUCT),
        left,
        right,
        Command.WriteFieldEnd
      )

    override protected def fail(state: State, message: String): Command =
      Command.Fail(message)

    override protected def processDynamic(state: State, value: DynamicValue): Option[Command] =
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

    def encode[A](schema: Schema[A], value: A): Chunk[Byte] = {
      val command = process(schema, value)
      val write   = new ChunkTransport.Write()
      val p       = new TBinaryProtocol(write)

      execute(p, command)

      write.chunk
    }
  }

  object Encoder {
    final case class State(fieldNumber: Option[Short])

    sealed trait Command

    object Command {
      final case class Sequence(commands: Chunk[Command]) extends Command { self =>

        def ++(other: Command): Sequence =
          other match {
            case Sequence(otherCommands) => Sequence(commands ++ otherCommands)
            case _                       => Sequence(commands :+ other)
          }

        def ++(others: Chunk[Command]): Sequence =
          others.foldLeft(self)(_ ++ _)
      }

      object Sequence {
        def apply(commands: Command*): Sequence = Sequence(Chunk.fromIterable(commands))
      }
      final case class WriteFieldBegin(fieldNumber: Option[Short], ttype: Byte)  extends Command
      case object WriteFieldEnd                                                  extends Command
      case object Noop                                                           extends Command
      final case class WriteString(value: String)                                extends Command
      final case class WriteBool(value: Boolean)                                 extends Command
      final case class WriteByte(value: Byte)                                    extends Command
      final case class WriteI16(value: Short)                                    extends Command
      final case class WriteI32(value: Int)                                      extends Command
      final case class WriteI64(value: Long)                                     extends Command
      final case class WriteDouble(value: Double)                                extends Command
      final case class WriteBinary(value: Chunk[Byte])                           extends Command
      final case class WriteListBegin(ttype: Byte, count: Int)                   extends Command
      final case class WriteSetBegin(ttype: Byte, count: Int)                    extends Command
      final case class WriteMapBegin(keyType: Byte, valueType: Byte, count: Int) extends Command
      final case class Fail(message: String)                                     extends Command
    }

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

    private def writePrimitiveType[A](standardType: StandardType[A], value: A): Command =
      (standardType, value) match {
        case (StandardType.UnitType, _) =>
          Command.Noop
        case (StandardType.StringType, str: String) =>
          Command.WriteString(str)
        case (StandardType.BoolType, b: Boolean) =>
          Command.WriteBool(b)
        case (StandardType.ByteType, v: Byte) =>
          Command.WriteByte(v)
        case (StandardType.ShortType, v: Short) =>
          Command.WriteI16(v)
        case (StandardType.IntType, v: Int) =>
          Command.WriteI32(v)
        case (StandardType.LongType, v: Long) =>
          Command.WriteI64(v)
        case (StandardType.FloatType, v: Float) =>
          Command.WriteDouble(v.toDouble)
        case (StandardType.DoubleType, v: Double) =>
          Command.WriteDouble(v.toDouble)
        case (StandardType.BigIntegerType, v: java.math.BigInteger) =>
          Command.WriteBinary(Chunk.fromArray(v.toByteArray))
        case (StandardType.BigDecimalType, v: java.math.BigDecimal) =>
          val unscaled  = v.unscaledValue()
          val precision = v.precision()
          val scale     = v.scale()
          Command.Sequence(Command.WriteFieldBegin(Some(1), getPrimitiveType(StandardType.BigIntegerType))) ++
            writePrimitiveType(StandardType.BigIntegerType, unscaled) ++
            Command.WriteFieldBegin(Some(2), getPrimitiveType(StandardType.IntType)) ++
            writePrimitiveType(StandardType.IntType, precision) ++
            Command.WriteFieldBegin(Some(3), getPrimitiveType(StandardType.IntType)) ++
            writePrimitiveType(StandardType.IntType, scale) ++
            Command.WriteFieldEnd

        case (StandardType.BinaryType, bytes: Chunk[Byte]) =>
          Command.WriteBinary(Chunk.fromArray(bytes.toArray))
        case (StandardType.CharType, c: Char) =>
          Command.WriteString(c.toString)
        case (StandardType.UUIDType, u: UUID) =>
          Command.WriteString(u.toString)
        case (StandardType.DayOfWeekType, v: DayOfWeek) =>
          Command.WriteByte(v.getValue.toByte)
        case (StandardType.MonthType, v: Month) =>
          Command.WriteByte(v.getValue.toByte)
        case (StandardType.MonthDayType, v: MonthDay) =>
          Command.Sequence(Command.WriteFieldBegin(Some(1), getPrimitiveType(StandardType.IntType))) ++
            writePrimitiveType(StandardType.IntType, v.getMonthValue) ++
            Command.WriteFieldBegin(Some(2), getPrimitiveType(StandardType.IntType)) ++
            writePrimitiveType(StandardType.IntType, v.getDayOfMonth) ++
            Command.WriteFieldEnd

        case (StandardType.PeriodType, v: Period) =>
          Command.Sequence(Command.WriteFieldBegin(Some(1), getPrimitiveType(StandardType.IntType))) ++
            writePrimitiveType(StandardType.IntType, v.getYears) ++
            Command.WriteFieldBegin(Some(2), getPrimitiveType(StandardType.IntType)) ++
            writePrimitiveType(StandardType.IntType, v.getMonths) ++
            Command.WriteFieldBegin(Some(3), getPrimitiveType(StandardType.IntType)) ++
            writePrimitiveType(StandardType.IntType, v.getDays) ++
            Command.WriteFieldEnd

        case (StandardType.YearType, v: Year) =>
          Command.WriteI32(v.getValue)
        case (StandardType.YearMonthType, v: YearMonth) =>
          Command.Sequence(Command.WriteFieldBegin(Some(1), getPrimitiveType(StandardType.IntType))) ++
            writePrimitiveType(StandardType.IntType, v.getYear) ++
            Command.WriteFieldBegin(Some(2), getPrimitiveType(StandardType.IntType)) ++
            writePrimitiveType(StandardType.IntType, v.getMonthValue) ++
            Command.WriteFieldEnd
        case (StandardType.ZoneIdType, v: ZoneId) =>
          Command.WriteString(v.getId)
        case (StandardType.ZoneOffsetType, v: ZoneOffset) =>
          Command.WriteI32(v.getTotalSeconds)
        case (StandardType.DurationType, v: Duration) =>
          Command.Sequence(Command.WriteFieldBegin(Some(1), getPrimitiveType(StandardType.LongType))) ++
            writePrimitiveType(StandardType.LongType, v.getSeconds) ++
            Command.WriteFieldBegin(Some(2), getPrimitiveType(StandardType.IntType)) ++
            writePrimitiveType(StandardType.IntType, v.getNano) ++
            Command.WriteFieldEnd

        case (StandardType.InstantType(formatter), v: Instant) =>
          Command.WriteString(formatter.format(v))
        case (StandardType.LocalDateType(formatter), v: LocalDate) =>
          Command.WriteString(formatter.format(v))
        case (StandardType.LocalTimeType(formatter), v: LocalTime) =>
          Command.WriteString(formatter.format(v))
        case (StandardType.LocalDateTimeType(formatter), v: LocalDateTime) =>
          Command.WriteString(formatter.format(v))
        case (StandardType.OffsetTimeType(formatter), v: OffsetTime) =>
          Command.WriteString(formatter.format(v))
        case (StandardType.OffsetDateTimeType(formatter), v: OffsetDateTime) =>
          Command.WriteString(formatter.format(v))
        case (StandardType.ZonedDateTimeType(formatter), v: ZonedDateTime) =>
          Command.WriteString(formatter.format(v))
        case (_, _) =>
          Command.Fail(s"No encoder for $standardType")
      }

    private def execute(p: TBinaryProtocol, command: Command): Unit =
      command match {
        case Command.Sequence(commands) =>
          for (command <- commands)
            execute(p, command)
        case Command.WriteFieldBegin(fieldNumber, ttype) =>
          fieldNumber match {
            case Some(num) =>
              p.writeFieldBegin(
                new TField("", ttype, num)
              )
            case None =>
          }
        case Command.WriteFieldEnd =>
          p.writeFieldStop()
        case Command.Noop =>
        case Command.WriteString(value) =>
          p.writeString(value)
        case Command.WriteBool(value) =>
          p.writeBool(value)
        case Command.WriteByte(value) =>
          p.writeByte(value)
        case Command.WriteI16(value) =>
          p.writeI16(value)
        case Command.WriteI32(value) =>
          p.writeI32(value)
        case Command.WriteI64(value) =>
          p.writeI64(value)
        case Command.WriteDouble(value) =>
          p.writeDouble(value)
        case Command.WriteBinary(value) =>
          p.writeBinary(ByteBuffer.wrap(value.toArray))
        case Command.WriteListBegin(ttype, count) =>
          p.writeListBegin(new TList(ttype, count))
        case Command.WriteSetBegin(ttype, count) =>
          p.writeSetBegin(new TSet(ttype, count))
        case Command.WriteMapBegin(keyType, valueType, count) =>
          p.writeMapBegin(new TMap(keyType, valueType, count))
        case Command.Fail(message) =>
          throw new RuntimeException(message)
      }
  }

  class Decoder(chunk: Chunk[Byte]) {
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
        case ProductDecoder(decoder)                            => decoder(path)
        case Schema.Enum1(_, c, _)                              => enumDecoder(path, c)
        case Schema.Enum2(_, c1, c2, _)                         => enumDecoder(path, c1, c2)
        case Schema.Enum3(_, c1, c2, c3, _)                     => enumDecoder(path, c1, c2, c3)
        case Schema.Enum4(_, c1, c2, c3, c4, _)                 => enumDecoder(path, c1, c2, c3, c4)
        case Schema.Enum5(_, c1, c2, c3, c4, c5, _)             => enumDecoder(path, c1, c2, c3, c4, c5)
        case Schema.Enum6(_, c1, c2, c3, c4, c5, c6, _)         => enumDecoder(path, c1, c2, c3, c4, c5, c6)
        case Schema.Enum7(_, c1, c2, c3, c4, c5, c6, c7, _)     => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7)
        case Schema.Enum8(_, c1, c2, c3, c4, c5, c6, c7, c8, _) => enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8)
        case Schema.Enum9(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, _) =>
          enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9)
        case Schema.Enum10(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _) =>
          enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
        case Schema.Enum11(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _) =>
          enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
        case Schema.Enum12(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _) =>
          enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
        case Schema.Enum13(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _) =>
          enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
        case Schema.Enum14(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _) =>
          enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
        case Schema.Enum15(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _) =>
          enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
        case Schema.Enum16(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _) =>
          enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
        case Schema.Enum17(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _) =>
          enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
        case Schema.Enum18(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _) =>
          enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
        case Schema.Enum19(
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
            _
            ) =>
          enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
        case Schema.Enum20(
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
            _
            ) =>
          enumDecoder(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
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
          enumDecoder(
            path,
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
          enumDecoder(
            path,
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
        case Schema.EnumN(_, cs, _) => enumDecoder(path, cs.toSeq: _*)
        case Schema.Dynamic(_)      => dynamicDecoder(path, DynamicValueSchema.schema)
        case _                      => fail(path, s"Unknown schema ${schema.getClass.getName}")
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
          fail(
            path,
            s"Error decoding enum with cases ${cases.map(_.id).mkString(", ")}, enum id out of range: ${readField.id}"
          )
        else {
          val subtypeCase = cases(readField.id - 1)
          val res         = decode(path :+ s"[case:${subtypeCase.id}]", subtypeCase.schema)
          res.foreach { _ =>
            p.readFieldBegin()
          }
          res.asInstanceOf[Result[Z]]
        }
      }.fold(
        err => fail(path, s"Error decoding enum with cases ${cases.map(_.id).mkString(", ")}: ${err.getMessage}"),
        identity
      )

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
            .map(
              data =>
                Period.of(
                  data.getOrElse(1, 0).asInstanceOf[Int],
                  data.getOrElse(2, 0).asInstanceOf[Int],
                  data.getOrElse(3, 0).asInstanceOf[Int]
                )
            )
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
            .map(
              data =>
                Duration
                  .ofSeconds(data.getOrElse(1, 0L).asInstanceOf[Long], data.getOrElse(2, 0L).asInstanceOf[Int].toLong)
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

      Try { p.readListBegin() }.fold(
        _ => fail(path, "Can not decode Sequence begin"),
        begin => decodeElements(begin.size, ChunkBuilder.make[Elem]()).map(schema.fromChunk)
      )
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
      }.fold(
        _ => fail(path, "Can not decode Map begin"),
        begin => decodeElements(begin.size, scala.collection.mutable.Map.empty[K, V])
      )
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

      Try { p.readSetBegin() }.fold(
        _ => fail(path, "Can not decode Set begin"),
        begin => decodeElements(begin.size, ChunkBuilder.make[A]()).map(_.toSet)
      )
    }

    private[codec] object ProductDecoder {

      def unapply[A](schema: Schema[A]): Option[Path => Result[A]] = schema match {
        case s: Schema.CaseClass1[_, A]                                                  => Some(caseClass1Decoder(s))
        case s: Schema.CaseClass2[_, _, A]                                               => Some(caseClass2Decoder(s))
        case s: Schema.CaseClass3[_, _, _, A]                                            => Some(caseClass3Decoder(s))
        case s: Schema.CaseClass4[_, _, _, _, A]                                         => Some(caseClass4Decoder(s))
        case s: Schema.CaseClass5[_, _, _, _, _, A]                                      => Some(caseClass5Decoder(s))
        case s: Schema.CaseClass6[_, _, _, _, _, _, A]                                   => Some(caseClass6Decoder(s))
        case s: Schema.CaseClass7[_, _, _, _, _, _, _, A]                                => Some(caseClass7Decoder(s))
        case s: Schema.CaseClass8[_, _, _, _, _, _, _, _, A]                             => Some(caseClass8Decoder(s))
        case s: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, A]                          => Some(caseClass9Decoder(s))
        case s: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, A]                      => Some(caseClass10Decoder(s))
        case s: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, A]                   => Some(caseClass11Decoder(s))
        case s: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, A]                => Some(caseClass12Decoder(s))
        case s: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, A]             => Some(caseClass13Decoder(s))
        case s: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, A]          => Some(caseClass14Decoder(s))
        case s: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]       => Some(caseClass15Decoder(s))
        case s: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]    => Some(caseClass16Decoder(s))
        case s: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] => Some(caseClass17Decoder(s))
        case s: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
          Some(caseClass18Decoder(s))
        case s: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
          Some(caseClass19Decoder(s))
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
            val Schema.Field(label, schema, _, _, _, _) = fields(index)
            val rawValue                                = values.get((index + 1).toShort)
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

      private def caseClass4Decoder[A1, A2, A3, A4, Z](
        schema: Schema.CaseClass4[A1, A2, A3, A4, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4]
        )

      private def caseClass5Decoder[A1, A2, A3, A4, A5, Z](
        schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5)
          _      <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5]
        )

      private def caseClass6Decoder[A1, A2, A3, A4, A5, A6, Z](
        schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
                     schema.field1,
                     schema.field2,
                     schema.field3,
                     schema.field4,
                     schema.field5,
                     schema.field6
                   )
          _ <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6]
        )

      private def caseClass7Decoder[A1, A2, A3, A4, A5, A6, A7, Z](
        schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
                     schema.field1,
                     schema.field2,
                     schema.field3,
                     schema.field4,
                     schema.field5,
                     schema.field6,
                     schema.field7
                   )
          _ <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7]
        )

      private def caseClass8Decoder[A1, A2, A3, A4, A5, A6, A7, A8, Z](
        schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
                     schema.field1,
                     schema.field2,
                     schema.field3,
                     schema.field4,
                     schema.field5,
                     schema.field6,
                     schema.field7,
                     schema.field8
                   )
          _ <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8]
        )

      private def caseClass9Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](
        schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
                     schema.field1,
                     schema.field2,
                     schema.field3,
                     schema.field4,
                     schema.field5,
                     schema.field6,
                     schema.field7,
                     schema.field9,
                     schema.field9
                   )
          _ <- validateBuffer(path, 0, buffer)
        } yield schema.construct(
          buffer(0).asInstanceOf[A1],
          buffer(1).asInstanceOf[A2],
          buffer(2).asInstanceOf[A3],
          buffer(3).asInstanceOf[A4],
          buffer(4).asInstanceOf[A5],
          buffer(5).asInstanceOf[A6],
          buffer(6).asInstanceOf[A7],
          buffer(7).asInstanceOf[A8],
          buffer(8).asInstanceOf[A9]
        )

      private def caseClass10Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
        schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
                     schema.field1,
                     schema.field2,
                     schema.field3,
                     schema.field4,
                     schema.field5,
                     schema.field6,
                     schema.field7,
                     schema.field9,
                     schema.field9,
                     schema.field10
                   )
          _ <- validateBuffer(path, 0, buffer)
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
          buffer(9).asInstanceOf[A10]
        )

      private def caseClass11Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
        schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
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
                     schema.field11
                   )
          _ <- validateBuffer(path, 0, buffer)
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
          buffer(10).asInstanceOf[A11]
        )

      private def caseClass12Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
        schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
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
                     schema.field12
                   )
          _ <- validateBuffer(path, 0, buffer)
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
          buffer(11).asInstanceOf[A12]
        )

      private def caseClass13Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
        schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
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
                     schema.field13
                   )
          _ <- validateBuffer(path, 0, buffer)
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

      private def caseClass14Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
        schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
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
                     schema.field14
                   )
          _ <- validateBuffer(path, 0, buffer)
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

      private def caseClass15Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
        schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
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
                     schema.field15
                   )
          _ <- validateBuffer(path, 0, buffer)
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

      private def caseClass16Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](
        schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
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
                     schema.field16
                   )
          _ <- validateBuffer(path, 0, buffer)
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

      private def caseClass17Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](
        schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
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
                     schema.field17
                   )
          _ <- validateBuffer(path, 0, buffer)
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

      private def caseClass18Decoder[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        Z
      ](
        schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
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
                     schema.field18
                   )
          _ <- validateBuffer(path, 0, buffer)
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

      private def caseClass19Decoder[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        Z
      ](
        schema: Schema.CaseClass19[
          A1,
          A2,
          A3,
          A4,
          A5,
          A6,
          A7,
          A8,
          A9,
          A10,
          A11,
          A12,
          A13,
          A14,
          A15,
          A16,
          A17,
          A18,
          A19,
          Z
        ]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
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
                     schema.field19
                   )
          _ <- validateBuffer(path, 0, buffer)
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

      private def caseClass20Decoder[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        Z
      ](
        schema: Schema.CaseClass20[
          A1,
          A2,
          A3,
          A4,
          A5,
          A6,
          A7,
          A8,
          A9,
          A10,
          A11,
          A12,
          A13,
          A14,
          A15,
          A16,
          A17,
          A18,
          A19,
          A20,
          Z
        ]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
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
                     schema.field20
                   )
          _ <- validateBuffer(path, 0, buffer)
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

      private def caseClass21Decoder[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        Z
      ](
        schema: Schema.CaseClass21[
          A1,
          A2,
          A3,
          A4,
          A5,
          A6,
          A7,
          A8,
          A9,
          A10,
          A11,
          A12,
          A13,
          A14,
          A15,
          A16,
          A17,
          A18,
          A19,
          A20,
          A21,
          Z
        ]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
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
                     schema.field21
                   )
          _ <- validateBuffer(path, 0, buffer)
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

      private def caseClass22Decoder[
        A1,
        A2,
        A3,
        A4,
        A5,
        A6,
        A7,
        A8,
        A9,
        A10,
        A11,
        A12,
        A13,
        A14,
        A15,
        A16,
        A17,
        A18,
        A19,
        A20,
        A21,
        A22,
        Z
      ](
        schema: Schema.CaseClass22[
          A1,
          A2,
          A3,
          A4,
          A5,
          A6,
          A7,
          A8,
          A9,
          A10,
          A11,
          A12,
          A13,
          A14,
          A15,
          A16,
          A17,
          A18,
          A19,
          A20,
          A21,
          A22,
          Z
        ]
      )(path: Path): Result[Z] =
        for {
          buffer <- unsafeDecodeFields(
                     path,
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
          _ <- validateBuffer(path, 0, buffer)
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
