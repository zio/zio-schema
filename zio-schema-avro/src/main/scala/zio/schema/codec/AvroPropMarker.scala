package zio.schema.codec

import java.time.format.DateTimeFormatter
import java.time.temporal.{ ChronoUnit, TemporalUnit }

import scala.jdk.CollectionConverters._
import scala.util.Try

import org.apache.avro.{ LogicalType, LogicalTypes, Schema => SchemaAvro }

sealed trait AvroPropMarker {
  def propName: String
  def value: Any = true
}

object AvroPropMarker {
  val wrapperNamePrefix = "wrapper"
  val wrapperNamespace  = "zio.schema.codec.avro"

  case object UnionWrapper extends AvroPropMarker {
    override def propName: String = "zio.schema.codec.avro.wrapper"
  }

  case object EitherWrapper extends AvroPropMarker {
    override def propName: String = "zio.schema.codec.avro.either"
  }

  case object CurrencyWrapper extends AvroPropMarker {
    override def propName: String = "zio.schema.codec.avro.currency"
  }

  final case class DurationChronoUnit(chronoUnit: ChronoUnit) extends AvroPropMarker {
    override def propName: String = DurationChronoUnit.propName
    override def value: Any       = chronoUnit.name()
  }

  final case class Formatter(dateTimeFormatter: DateTimeFormatter) extends AvroPropMarker {
    override def propName: String = Formatter.propName
    override def value: Any = dateTimeFormatter match {
      case f if f.equals(DateTimeFormatter.ISO_LOCAL_DATE_TIME)  => "ISO_LOCAL_DATE_TIME"
      case f if f.equals(DateTimeFormatter.ISO_DATE)             => "ISO_DATE"
      case f if f.equals(DateTimeFormatter.ISO_TIME)             => "ISO_TIME"
      case f if f.equals(DateTimeFormatter.ISO_LOCAL_TIME)       => "ISO_LOCAL_TIME"
      case f if f.equals(DateTimeFormatter.ISO_LOCAL_DATE)       => "ISO_LOCAL_DATE"
      case f if f.equals(DateTimeFormatter.ISO_OFFSET_DATE_TIME) => "ISO_OFFSET_DATE_TIME"
      case f if f.equals(DateTimeFormatter.ISO_OFFSET_DATE)      => "ISO_OFFSET_DATE"
      case f if f.equals(DateTimeFormatter.ISO_OFFSET_TIME)      => "ISO_OFFSET_TIME"
      case f if f.equals(DateTimeFormatter.ISO_ZONED_DATE_TIME)  => "ISO_ZONED_DATE_TIME"
      case f if f.equals(DateTimeFormatter.ISO_ORDINAL_DATE)     => "ISO_ORDINAL_DATE"
      case f if f.equals(DateTimeFormatter.ISO_WEEK_DATE)        => "ISO_WEEK_DATE"
      case f if f.equals(DateTimeFormatter.ISO_INSTANT)          => "ISO_INSTANT"
      case f if f.equals(DateTimeFormatter.ISO_DATE_TIME)        => "ISO_DATE_TIME"
      case f if f.equals(DateTimeFormatter.RFC_1123_DATE_TIME)   => "RFC_1123_DATE_TIME"
      case f if f.equals(DateTimeFormatter.BASIC_ISO_DATE)       => "BASIC_ISO_DATE"
      case _                                                     => dateTimeFormatter.toString
    }
  }

  object Formatter {
    // TODO: use only a single property marker and distinguish by value.
    //  A json with "zio.schema.codec.avro.dateTimeFormatter": false is confusing
    //  that is still uses the dateTimeFormatter regardless of the value
    val propName           = "zio.schema.codec.avro.dateTimeFormatter"
    val default: Formatter = Formatter(DateTimeFormatter.ISO_INSTANT)

    def fromAvroStringOrDefault(avroSchema: SchemaAvro, stringType: StringType): Either[String, Formatter] =
      fromAvroString(avroSchema).map {
        case Some(value) => value
        case None        => getDefaultByStringType(stringType)
      }

    def fromAvroStringOrDefault(avroSchema: SchemaAvro, logicalType: LogicalType): Either[String, Formatter] =
      fromAvroString(avroSchema).map {
        case Some(value) => value
        case None        => getDefaultByLogicalType(logicalType)
      }

    private def getDefaultByLogicalType(logicalType: LogicalType): Formatter =
      logicalType match {
        case _: LogicalTypes.Date                 => Formatter(DateTimeFormatter.ISO_DATE)
        case _: LogicalTypes.LocalTimestampMicros => Formatter(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        case _: LogicalTypes.LocalTimestampMillis => Formatter(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        case _: LogicalTypes.TimeMicros           => Formatter(DateTimeFormatter.ISO_LOCAL_TIME)
        case _: LogicalTypes.TimeMillis           => Formatter(DateTimeFormatter.ISO_LOCAL_TIME)
        case _: LogicalTypes.TimestampMicros      => Formatter(DateTimeFormatter.ISO_INSTANT)
        case _: LogicalTypes.TimestampMillis      => Formatter(DateTimeFormatter.ISO_INSTANT)
        case _                                    => Formatter(DateTimeFormatter.ISO_INSTANT)
      }

    private def getDefaultByStringType(stringType: StringType): Formatter =
      stringType match {
        case StringType.ZoneId         => Formatter(DateTimeFormatter.ISO_ZONED_DATE_TIME)
        case StringType.Instant        => Formatter(DateTimeFormatter.ISO_INSTANT)
        case StringType.LocalDate      => Formatter(DateTimeFormatter.ISO_LOCAL_DATE)
        case StringType.LocalTime      => Formatter(DateTimeFormatter.ISO_LOCAL_TIME)
        case StringType.LocalDateTime  => Formatter(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        case StringType.OffsetTime     => Formatter(DateTimeFormatter.ISO_OFFSET_TIME)
        case StringType.OffsetDateTime => Formatter(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        case StringType.ZoneDateTime   => Formatter(DateTimeFormatter.ISO_ZONED_DATE_TIME)
      }

    private def fromAvroString(avroSchema: SchemaAvro): Either[String, Option[Formatter]] =
      avroSchema.getObjectProps.asScala.get(propName).collect {
        case "ISO_LOCAL_DATE_TIME"  => Right(Formatter(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
        case "ISO_DATE"             => Right(Formatter(DateTimeFormatter.ISO_DATE))
        case "ISO_TIME"             => Right(Formatter(DateTimeFormatter.ISO_TIME))
        case "ISO_LOCAL_TIME"       => Right(Formatter(DateTimeFormatter.ISO_LOCAL_TIME))
        case "ISO_LOCAL_DATE"       => Right(Formatter(DateTimeFormatter.ISO_LOCAL_DATE))
        case "ISO_OFFSET_DATE_TIME" => Right(Formatter(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
        case "ISO_OFFSET_DATE"      => Right(Formatter(DateTimeFormatter.ISO_OFFSET_DATE))
        case "ISO_OFFSET_TIME"      => Right(Formatter(DateTimeFormatter.ISO_OFFSET_TIME))
        case "ISO_ZONED_DATE_TIME"  => Right(Formatter(DateTimeFormatter.ISO_ZONED_DATE_TIME))
        case "ISO_ORDINAL_DATE"     => Right(Formatter(DateTimeFormatter.ISO_ORDINAL_DATE))
        case "ISO_WEEK_DATE"        => Right(Formatter(DateTimeFormatter.ISO_WEEK_DATE))
        case "ISO_INSTANT"          => Right(Formatter(DateTimeFormatter.ISO_INSTANT))
        case "ISO_DATE_TIME"        => Right(Formatter(DateTimeFormatter.ISO_DATE_TIME))
        case "RFC_1123_DATE_TIME"   => Right(Formatter(DateTimeFormatter.RFC_1123_DATE_TIME))
        case "BASIC_ISO_DATE"       => Right(Formatter(DateTimeFormatter.BASIC_ISO_DATE))
        case s: String =>
          Try {
            Formatter(DateTimeFormatter.ofPattern(s))
          }.toEither.left.map(_.getMessage)
      } match {
        case Some(value) => value.map(Some(_))
        case None        => Right(None)
      }

  }

  object DurationChronoUnit {
    val default: DurationChronoUnit = DurationChronoUnit(ChronoUnit.MILLIS)
    val propName                    = "zio.schema.codec.avro.durationChronoUnit"

    def fromAvroDuration(avroSchema: SchemaAvro): Option[DurationChronoUnit] =
      avroSchema.getObjectProps.asScala.get(propName).collect {
        case "NANOS"     => DurationChronoUnit(ChronoUnit.NANOS)
        case "MICROS"    => DurationChronoUnit(ChronoUnit.MICROS)
        case "MILLIS"    => DurationChronoUnit(ChronoUnit.MILLIS)
        case "SECONDS"   => DurationChronoUnit(ChronoUnit.SECONDS)
        case "MINUTES"   => DurationChronoUnit(ChronoUnit.MINUTES)
        case "HOURS"     => DurationChronoUnit(ChronoUnit.HOURS)
        case "HALF_DAYS" => DurationChronoUnit(ChronoUnit.HALF_DAYS)
        case "DAYS"      => DurationChronoUnit(ChronoUnit.DAYS)
        case "WEEKS"     => DurationChronoUnit(ChronoUnit.WEEKS)
        case "MONTHS"    => DurationChronoUnit(ChronoUnit.MONTHS)
        case "YEARS"     => DurationChronoUnit(ChronoUnit.YEARS)
        case "DECADES"   => DurationChronoUnit(ChronoUnit.DECADES)
        case "CENTURIES" => DurationChronoUnit(ChronoUnit.CENTURIES)
        case "MILLENNIA" => DurationChronoUnit(ChronoUnit.MILLENNIA)
        case "ERAS"      => DurationChronoUnit(ChronoUnit.ERAS)
        case "FOREVER"   => DurationChronoUnit(ChronoUnit.FOREVER)
      }

    def fromTemporalUnit(temporalUnit: TemporalUnit): Option[DurationChronoUnit] =
      temporalUnit match {
        case unit: ChronoUnit => Some(DurationChronoUnit(unit))
        case _                => None
      }
  }

  final case class IntDiscriminator(intType: IntType) extends AvroPropMarker {
    override def propName: String = IntType.propName
    override def value: Any       = intType.value
  }

  final case class StringDiscriminator(stringType: StringType) extends AvroPropMarker {
    override def propName: String = StringType.propName
    override def value: Any       = stringType.value
  }

  final case class RecordDiscriminator(recordType: RecordType) extends AvroPropMarker {
    override def propName: String = RecordType.propName
    override def value: Any       = recordType.value
  }
}

sealed trait RecordType { self =>

  def value: String = self match {
    case RecordType.MonthDay  => "monthDay"
    case RecordType.Period    => "period"
    case RecordType.YearMonth => "yearMonth"
    case RecordType.Duration  => "duration"
    case RecordType.Tuple     => "tuple"
  }
}

object RecordType {
  val propName: String = "zio.schema.codec.recordType"

  def fromAvroRecord(avroSchema: SchemaAvro): Option[RecordType] =
    if (avroSchema.getType == SchemaAvro.Type.RECORD) {
      avroSchema.getObjectProps.asScala.get(propName).collect {
        case "monthDay"  => RecordType.MonthDay
        case "period"    => RecordType.Period
        case "yearMonth" => RecordType.YearMonth
        case "duration"  => RecordType.Duration
        case "tuple"     => RecordType.Tuple
      }
    } else None

  case object MonthDay  extends RecordType
  case object Period    extends RecordType
  case object YearMonth extends RecordType
  case object Duration  extends RecordType
  case object Tuple     extends RecordType
}

sealed trait StringType { self =>

  def value: String = self match {
    case StringType.ZoneId         => "zoneId"
    case StringType.Instant        => "instant"
    case StringType.LocalDate      => "localDate"
    case StringType.LocalTime      => "localTime"
    case StringType.LocalDateTime  => "localDateTime"
    case StringType.OffsetTime     => "offsetTime"
    case StringType.OffsetDateTime => "offsetDateTime"
    case StringType.ZoneDateTime   => "zoneDateTime"
  }
}

object StringType {
  val propName = "zio.schema.codec.stringType"

  def fromAvroString(avroSchema: SchemaAvro): Option[StringType] =
    if (avroSchema.getType == SchemaAvro.Type.STRING) {
      avroSchema.getObjectProps.asScala.get(propName).collect {
        case "zoneId"         => StringType.ZoneId
        case "instant"        => StringType.Instant
        case "localDate"      => StringType.LocalDate
        case "localTime"      => StringType.LocalTime
        case "localDateTime"  => StringType.LocalDateTime
        case "offsetTime"     => StringType.OffsetTime
        case "offsetDateTime" => StringType.OffsetDateTime
        case "zoneDateTime"   => StringType.ZoneDateTime
      }
    } else None

  case object ZoneId         extends StringType
  case object Instant        extends StringType
  case object LocalDate      extends StringType
  case object LocalTime      extends StringType
  case object LocalDateTime  extends StringType
  case object OffsetTime     extends StringType
  case object OffsetDateTime extends StringType
  case object ZoneDateTime   extends StringType
}

sealed trait IntType { self =>

  def value: String = self match {
    case IntType.Char       => "char"
    case IntType.Short      => "short"
    case IntType.DayOfWeek  => "dayOfWeek"
    case IntType.Month      => "month"
    case IntType.Year       => "year"
    case IntType.ZoneOffset => "zoneOffset"
  }
}

object IntType {
  val propName = "zio.schema.codec.intType"

  def fromAvroInt(avroSchema: SchemaAvro): Option[IntType] =
    if (avroSchema.getType == SchemaAvro.Type.INT) {
      avroSchema.getObjectProps.asScala.get(propName).collect {
        case "char"       => IntType.Char
        case "short"      => IntType.Short
        case "dayOfWeek"  => IntType.DayOfWeek
        case "month"      => IntType.Month
        case "year"       => IntType.Year
        case "zoneOffset" => IntType.ZoneOffset
      }
    } else None

  case object Char       extends IntType
  case object Short      extends IntType
  case object DayOfWeek  extends IntType
  case object Month      extends IntType
  case object Year       extends IntType
  case object ZoneOffset extends IntType
}
