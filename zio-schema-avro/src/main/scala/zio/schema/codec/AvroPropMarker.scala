package zio.schema.codec

import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoUnit, TemporalUnit}

import scala.jdk.CollectionConverters._
import scala.util.Try

import org.apache.avro.{LogicalType, LogicalTypes, Schema => SchemaAvro}

/**
 * A custom property that `AvroSchemaCodec.encode` adds to a generated Avro
 * schema, so that `AvroSchemaCodec.decode` can tell which ZIO Schema type the
 * Avro schema came from, such as a `Char` written as an Avro `int`.
 */
sealed trait AvroPropMarker {

  /** The name of the Avro schema property. */
  def propName: String

  /**
   * The value of the Avro schema property; `true` unless a marker overrides it.
   */
  def value: Any = true
}

/** The property markers. */
object AvroPropMarker {

  /**
   * The prefix `wrapper` of the names of the records that
   * `AvroSchemaCodec.wrapAvro` creates; a wrapper record for `name` is named
   * `wrapper_name`.
   */
  val wrapperNamePrefix = "wrapper"

  /**
   * The namespace `zio.schema.codec.avro` of the records that
   * `AvroSchemaCodec.wrapAvro` creates.
   */
  val wrapperNamespace = "zio.schema.codec.avro"

  /**
   * Marks a record that wraps a union, written where Avro does not allow a
   * union directly, such as a union inside an optional value or inside another
   * union. Its property is `zio.schema.codec.avro.wrapper`.
   */
  case object UnionWrapper extends AvroPropMarker {
    override def propName: String = "zio.schema.codec.avro.wrapper"
  }

  /**
   * Marks the record that wraps the union generated for an `Either`. Its
   * property is `zio.schema.codec.avro.either`.
   */
  case object EitherWrapper extends AvroPropMarker {
    override def propName: String = "zio.schema.codec.avro.either"
  }

  /**
   * Marks the Avro `string` schema generated for `java.util.Currency`. Its
   * property is `zio.schema.codec.avro.currency`.
   */
  case object CurrencyWrapper extends AvroPropMarker {
    override def propName: String = "zio.schema.codec.avro.currency"
  }

  /**
   * Records a `ChronoUnit` as the property
   * `zio.schema.codec.avro.durationChronoUnit`, whose value is the unit's name,
   * such as `DAYS`. `AvroSchemaCodec.encode` does not add this property: a
   * `Duration` is written as an Avro `string`.
   */
  final case class DurationChronoUnit(chronoUnit: ChronoUnit) extends AvroPropMarker {
    override def propName: String = DurationChronoUnit.propName
    override def value: Any       = chronoUnit.name()
  }

  /**
   * Records a `DateTimeFormatter` as the property
   * `zio.schema.codec.avro.dateTimeFormatter`. The value is the constant's name
   * for the predefined `DateTimeFormatter` constants (for example `ISO_DATE`),
   * and the formatter's `toString` for any other formatter (for
   * `DateTimeFormatter.ofPattern("yyyy")` that is
   * `Value(YearOfEra,4,19,EXCEEDS_PAD)`). `AvroSchemaCodec.encode` does not add
   * this property; `AvroSchemaCodec.decode` reads it.
   */
  final case class Formatter(dateTimeFormatter: DateTimeFormatter) extends AvroPropMarker {
    override def propName: String = Formatter.propName
    override def value: Any       = dateTimeFormatter match {
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

  /** Reads the `Formatter` property from Avro schemas. */
  object Formatter {
    // TODO: use only a single property marker and distinguish by value.
    //  A json with "zio.schema.codec.avro.dateTimeFormatter": false is confusing
    //  that is still uses the dateTimeFormatter regardless of the value
    /** The property name `zio.schema.codec.avro.dateTimeFormatter`. */
    val propName = "zio.schema.codec.avro.dateTimeFormatter"

    /** A `Formatter` for `DateTimeFormatter.ISO_INSTANT`. */
    val default: Formatter = Formatter(DateTimeFormatter.ISO_INSTANT)

    /**
     * Reads the formatter property of `avroSchema`. When the property is
     * missing, returns the ISO formatter that matches `stringType`, such as
     * `ISO_LOCAL_DATE` for `StringType.LocalDate`. The value is either a
     * constant name such as `ISO_DATE` or a pattern for
     * `DateTimeFormatter.ofPattern`; an invalid pattern gives `Left` with the
     * exception's message, such as
     * `Left("Pattern includes reserved character: '{'")`.
     */
    def fromAvroStringOrDefault(avroSchema: SchemaAvro, stringType: StringType): Either[String, Formatter] =
      fromAvroString(avroSchema).map {
        case Some(value) => value
        case None        => getDefaultByStringType(stringType)
      }

    /**
     * Reads the formatter property of `avroSchema`. When the property is
     * missing, returns the ISO formatter that matches `logicalType`: `ISO_DATE`
     * for `date`, `ISO_LOCAL_TIME` for `time-millis` and `time-micros`,
     * `ISO_LOCAL_DATE_TIME` for `local-timestamp-millis` and
     * `local-timestamp-micros`, and `ISO_INSTANT` for any other logical type.
     * The value is either a constant name such as `ISO_DATE` or a pattern for
     * `DateTimeFormatter.ofPattern`; an invalid pattern gives `Left` with the
     * exception's message.
     */
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
        case s: String              =>
          Try {
            Formatter(DateTimeFormatter.ofPattern(s))
          }.toEither.left.map(_.getMessage)
      } match {
        case Some(value) => value.map(Some(_))
        case None        => Right(None)
      }

  }

  /** Reads the `DurationChronoUnit` property from Avro schemas. */
  object DurationChronoUnit {

    /** A `DurationChronoUnit` for `ChronoUnit.MILLIS`. */
    val default: DurationChronoUnit = DurationChronoUnit(ChronoUnit.MILLIS)

    /** The property name `zio.schema.codec.avro.durationChronoUnit`. */
    val propName = "zio.schema.codec.avro.durationChronoUnit"

    /**
     * Reads the property of `avroSchema`; returns `None` when it is missing or
     * is not the name of a `ChronoUnit`.
     */
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

    /**
     * Wraps `temporalUnit` when it is a `ChronoUnit`; returns `None` for any
     * other `TemporalUnit`.
     */
    def fromTemporalUnit(temporalUnit: TemporalUnit): Option[DurationChronoUnit] =
      temporalUnit match {
        case unit: ChronoUnit => Some(DurationChronoUnit(unit))
        case _                => None
      }
  }

  /**
   * Records which ZIO type an Avro `int` schema holds, as the property
   * `zio.schema.codec.intType`.
   */
  final case class IntDiscriminator(intType: IntType) extends AvroPropMarker {
    override def propName: String = IntType.propName
    override def value: Any       = intType.value
  }

  /**
   * Records which ZIO type an Avro `string` schema holds, as the property
   * `zio.schema.codec.stringType`.
   */
  final case class StringDiscriminator(stringType: StringType) extends AvroPropMarker {
    override def propName: String = StringType.propName
    override def value: Any       = stringType.value
  }

  /**
   * Records which ZIO type an Avro schema holds, as the property
   * `zio.schema.codec.recordType`: `tuple` on the record of a tuple, and
   * `monthDay`, `period`, `yearMonth` or `duration` on the Avro `string` schema
   * generated for those types.
   */
  final case class RecordDiscriminator(recordType: RecordType) extends AvroPropMarker {
    override def propName: String = RecordType.propName
    override def value: Any       = recordType.value
  }
}

/** The ZIO types that `AvroPropMarker.RecordDiscriminator` names. */
sealed trait RecordType { self =>

  /**
   * Returns the property value, such as `monthDay` for `RecordType.MonthDay`.
   */
  def value: String = self match {
    case RecordType.MonthDay  => "monthDay"
    case RecordType.Period    => "period"
    case RecordType.YearMonth => "yearMonth"
    case RecordType.Duration  => "duration"
    case RecordType.Tuple     => "tuple"
  }
}

/** The record types and the property that names them. */
object RecordType {

  /** The property name `zio.schema.codec.recordType`. */
  val propName: String = "zio.schema.codec.recordType"

  /**
   * Reads the property of an Avro `record` schema. Returns `None` for other
   * Avro types and for a missing or unknown value. `AvroSchemaCodec.encode`
   * writes `MonthDay`, `Period`, `YearMonth` and `Duration` as Avro `string`
   * schemas, so this returns `None` for them.
   */
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

  /** A `java.time.MonthDay`. */
  case object MonthDay extends RecordType

  /** A `java.time.Period`. */
  case object Period extends RecordType

  /** A `java.time.YearMonth`. */
  case object YearMonth extends RecordType

  /** A `java.time.Duration`. */
  case object Duration extends RecordType

  /** A `Tuple2`. */
  case object Tuple extends RecordType
}

/**
 * The ZIO types written as an Avro `string` schema that
 * `AvroPropMarker.StringDiscriminator` names.
 */
sealed trait StringType { self =>

  /**
   * Returns the property value, such as `localDate` for `StringType.LocalDate`.
   */
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

/** The string types and the property that names them. */
object StringType {

  /** The property name `zio.schema.codec.stringType`. */
  val propName = "zio.schema.codec.stringType"

  /**
   * Reads the property of an Avro `string` schema. Returns `None` for other
   * Avro types and for a missing or unknown value.
   */
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

  /** A `java.time.ZoneId`. */
  case object ZoneId extends StringType

  /** A `java.time.Instant`. */
  case object Instant extends StringType

  /** A `java.time.LocalDate`. */
  case object LocalDate extends StringType

  /** A `java.time.LocalTime`. */
  case object LocalTime extends StringType

  /** A `java.time.LocalDateTime`. */
  case object LocalDateTime extends StringType

  /** A `java.time.OffsetTime`. */
  case object OffsetTime extends StringType

  /** A `java.time.OffsetDateTime`. */
  case object OffsetDateTime extends StringType

  /** A `java.time.ZonedDateTime`. */
  case object ZoneDateTime extends StringType
}

/**
 * The ZIO types written as an Avro `int` schema that
 * `AvroPropMarker.IntDiscriminator` names.
 */
sealed trait IntType { self =>

  /**
   * Returns the property value, such as `dayOfWeek` for `IntType.DayOfWeek`.
   */
  def value: String = self match {
    case IntType.Char       => "char"
    case IntType.Short      => "short"
    case IntType.DayOfWeek  => "dayOfWeek"
    case IntType.Month      => "month"
    case IntType.Year       => "year"
    case IntType.ZoneOffset => "zoneOffset"
  }
}

/** The int types and the property that names them. */
object IntType {

  /** The property name `zio.schema.codec.intType`. */
  val propName = "zio.schema.codec.intType"

  /**
   * Reads the property of an Avro `int` schema. Returns `None` for other Avro
   * types and for a missing or unknown value.
   */
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

  /** A `Char`. */
  case object Char extends IntType

  /** A `Short`. */
  case object Short extends IntType

  /** A `java.time.DayOfWeek`. */
  case object DayOfWeek extends IntType

  /** A `java.time.Month`. */
  case object Month extends IntType

  /** A `java.time.Year`. */
  case object Year extends IntType

  /** A `java.time.ZoneOffset`. */
  case object ZoneOffset extends IntType
}
