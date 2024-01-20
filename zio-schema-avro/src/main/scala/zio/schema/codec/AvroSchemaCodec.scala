package zio.schema.codec

import java.nio.charset.StandardCharsets
import java.time.format.DateTimeFormatter
import java.time.{ Duration, Month, MonthDay, Period, Year, YearMonth }

import scala.annotation.StaticAnnotation
import scala.collection.immutable.ListMap
import scala.jdk.CollectionConverters._
import scala.util.{ Right, Try }

import org.apache.avro.{ LogicalTypes, Schema => SchemaAvro }

import zio.Chunk
import zio.schema.CaseSet.Aux
import zio.schema.Schema.{ Record, _ }
import zio.schema._
import zio.schema.codec.AvroAnnotations._
import zio.schema.codec.AvroPropMarker._
import zio.schema.meta.MetaSchema

trait AvroSchemaCodec {
  def encode(schema: Schema[_]): scala.util.Either[String, String]

  def decode(bytes: Chunk[Byte]): scala.util.Either[String, Schema[_]]
}

object AvroSchemaCodec extends AvroSchemaCodec {

  def encode(schema: Schema[_]): scala.util.Either[String, String] =
    toAvroSchema(schema).map(_.toString)

  def encodeToApacheAvro(schema: Schema[_]): scala.util.Either[String, SchemaAvro] =
    toAvroSchema(schema)

  def decode(bytes: Chunk[Byte]): scala.util.Either[String, Schema[_]] = {
    val avroSchemaParser = new SchemaAvro.Parser()
    val avroSchema = Try {
      avroSchemaParser.parse(new String(bytes.toArray, StandardCharsets.UTF_8))
    }.fold(
      e => Left(e.getMessage),
      s => Right(s)
    )
    avroSchema.flatMap(toZioSchema)
  }

  def decodeFromApacheAvro: SchemaAvro => scala.util.Either[String, Schema[_]] = toZioSchema

  private def toZioSchema(avroSchema: SchemaAvro): scala.util.Either[String, Schema[_]] =
    for {
      // make sure to parse logical types with throwing exceptions enabled,
      // otherwise parsing errors on invalid logical types might be lost
      _ <- Try {
            LogicalTypes.fromSchema(avroSchema)
          }.toEither.left.map(e => e.getMessage)
      result <- avroSchema.getType match {
                 case SchemaAvro.Type.RECORD =>
                   RecordType.fromAvroRecord(avroSchema) match {
                     case Some(RecordType.Period)    => Right(Schema.primitive(StandardType.PeriodType))
                     case Some(RecordType.YearMonth) => Right(Schema.primitive(StandardType.YearMonthType))
                     case Some(RecordType.Tuple)     => toZioTuple(avroSchema)
                     case Some(RecordType.MonthDay)  => Right(Schema.primitive(StandardType.MonthDayType))
                     case Some(RecordType.Duration)  => Right(Schema.primitive(StandardType.DurationType))
                     case None                       => toZioRecord(avroSchema)
                   }
                 case SchemaAvro.Type.ENUM => toZioStringEnum(avroSchema)
                 case SchemaAvro.Type.ARRAY =>
                   toZioSchema(avroSchema.getElementType).map(Schema.list(_))
                 case SchemaAvro.Type.MAP =>
                   toZioSchema(avroSchema.getValueType).map(Schema.map(Schema.primitive(StandardType.StringType), _))
                 case SchemaAvro.Type.UNION =>
                   avroSchema match {
                     case OptionUnion(optionSchema) => toZioSchema(optionSchema).map(Schema.option(_))
                     case EitherUnion(left, right) =>
                       toZioSchema(left).flatMap(l => toZioSchema(right).map(r => Schema.either(l, r)))
                     case _ => toZioEnumeration(avroSchema)
                   }
                 case SchemaAvro.Type.FIXED =>
                   val fixed = if (avroSchema.getLogicalType == null) {
                     Right(Schema.primitive(StandardType.BinaryType))
                   } else if (avroSchema.getLogicalType.isInstanceOf[LogicalTypes.Decimal]) {
                     val size = avroSchema.getFixedSize
                     toZioDecimal(avroSchema, DecimalType.Fixed(size))
                   } else {
                     // TODO: Java implementation of Apache Avro does not support logical type Duration yet:
                     //  AVRO-2123 with PR https://github.com/apache/avro/pull/1263
                     Left(s"Unsupported fixed logical type ${avroSchema.getLogicalType}")
                   }
                   fixed.map(_.addAllAnnotations(buildZioAnnotations(avroSchema)))
                 case SchemaAvro.Type.STRING =>
                   StringType.fromAvroString(avroSchema) match {
                     case Some(stringType) =>
                       val dateTimeFormatter = Formatter.fromAvroStringOrDefault(avroSchema, stringType)
                       dateTimeFormatter
                         .map(_.dateTimeFormatter)
                         .flatMap(_ => {
                           stringType match {
                             case StringType.ZoneId => Right(Schema.primitive(StandardType.ZoneIdType))
                             case StringType.Instant =>
                               Right(
                                 Schema
                                   .primitive(StandardType.InstantType)
                                   .annotate(AvroAnnotations.formatToString)
                               )
                             case StringType.LocalDate =>
                               Right(
                                 Schema
                                   .primitive(StandardType.LocalDateType)
                                   .annotate(AvroAnnotations.formatToString)
                               )
                             case StringType.LocalTime =>
                               Right(
                                 Schema
                                   .primitive(StandardType.LocalTimeType)
                                   .annotate(AvroAnnotations.formatToString)
                               )
                             case StringType.LocalDateTime =>
                               Right(
                                 Schema
                                   .primitive(StandardType.LocalDateTimeType)
                                   .annotate(AvroAnnotations.formatToString)
                               )
                             case StringType.OffsetTime =>
                               Right(Schema.primitive(StandardType.OffsetTimeType))
                             case StringType.OffsetDateTime =>
                               Right(Schema.primitive(StandardType.OffsetDateTimeType))
                             case StringType.ZoneDateTime =>
                               Right(Schema.primitive(StandardType.ZonedDateTimeType))
                           }
                         })
                     case None =>
                       if (avroSchema.getLogicalType == null) {
                         Right(Schema.primitive(StandardType.StringType))
                       } else if (avroSchema.getLogicalType.getName == LogicalTypes.uuid().getName) {
                         Right(Schema.primitive(StandardType.UUIDType))
                       } else {
                         Left(s"Unsupported string logical type: ${avroSchema.getLogicalType.getName}")
                       }
                   }
                 case SchemaAvro.Type.BYTES =>
                   if (avroSchema.getLogicalType == null) {
                     Right(Schema.primitive(StandardType.BinaryType))
                   } else if (avroSchema.getLogicalType.isInstanceOf[LogicalTypes.Decimal]) {
                     toZioDecimal(avroSchema, DecimalType.Bytes)
                   } else {
                     Left(s"Unsupported bytes logical type ${avroSchema.getLogicalType.getName}")
                   }
                 case SchemaAvro.Type.INT =>
                   IntType.fromAvroInt(avroSchema) match {
                     case Some(IntType.Char)       => Right(Schema.primitive(StandardType.CharType))
                     case Some(IntType.DayOfWeek)  => Right(Schema.primitive(StandardType.DayOfWeekType))
                     case Some(IntType.Year)       => Right(Schema.primitive(StandardType.YearType))
                     case Some(IntType.Short)      => Right(Schema.primitive(StandardType.ShortType))
                     case Some(IntType.Month)      => Right(Schema.primitive(StandardType.MonthType))
                     case Some(IntType.ZoneOffset) => Right(Schema.primitive(StandardType.ZoneOffsetType))
                     case None =>
                       if (avroSchema.getLogicalType == null) {
                         Right(Schema.primitive(StandardType.IntType))
                       } else
                         avroSchema.getLogicalType match {
                           case _: LogicalTypes.TimeMillis =>
                             val formatter = Formatter.fromAvroStringOrDefault(avroSchema, avroSchema.getLogicalType)
                             formatter.map(
                               _ => Schema.primitive(StandardType.LocalTimeType)
                             )
                           case _: LogicalTypes.Date =>
                             val formatter = Formatter.fromAvroStringOrDefault(avroSchema, avroSchema.getLogicalType)
                             formatter.map(
                               _ => Schema.primitive(StandardType.LocalDateType)
                             )
                           case _ => Left(s"Unsupported int logical type ${avroSchema.getLogicalType.getName}")
                         }
                   }
                 case SchemaAvro.Type.LONG =>
                   if (avroSchema.getLogicalType == null) {
                     Right(Schema.primitive(StandardType.LongType))
                   } else
                     avroSchema.getLogicalType match {
                       case _: LogicalTypes.TimeMicros =>
                         val formatter = Formatter.fromAvroStringOrDefault(avroSchema, avroSchema.getLogicalType)
                         formatter.map(
                           _ => Schema.primitive(StandardType.LocalTimeType)
                         )
                       case _: LogicalTypes.TimestampMillis =>
                         val formatter = Formatter.fromAvroStringOrDefault(avroSchema, avroSchema.getLogicalType)
                         formatter.map(
                           _ => Schema.primitive(StandardType.InstantType)
                         )
                       case _: LogicalTypes.TimestampMicros =>
                         val formatter = Formatter.fromAvroStringOrDefault(avroSchema, avroSchema.getLogicalType)
                         formatter.map(
                           _ => Schema.primitive(StandardType.InstantType)
                         )
                       case _: LogicalTypes.LocalTimestampMillis =>
                         val formatter = Formatter.fromAvroStringOrDefault(avroSchema, avroSchema.getLogicalType)
                         formatter.map(
                           _ => Schema.primitive(StandardType.LocalDateTimeType)
                         )
                       case _: LogicalTypes.LocalTimestampMicros =>
                         val formatter = Formatter.fromAvroStringOrDefault(avroSchema, avroSchema.getLogicalType)
                         formatter.map(
                           _ => Schema.primitive(StandardType.LocalDateTimeType)
                         )
                       case _ => Left(s"Unsupported long logical type ${avroSchema.getLogicalType.getName}")
                     }
                 case SchemaAvro.Type.FLOAT   => Right(Schema.primitive(StandardType.FloatType))
                 case SchemaAvro.Type.DOUBLE  => Right(Schema.primitive(StandardType.DoubleType))
                 case SchemaAvro.Type.BOOLEAN => Right(Schema.primitive(StandardType.BoolType))
                 case SchemaAvro.Type.NULL    => Right(Schema.primitive(StandardType.UnitType))
                 case null                    => Left(s"Unsupported type ${avroSchema.getType}")
               }
    } yield result

  def toAvroBinary(schema: Schema[_]): SchemaAvro =
    schema.annotations.collectFirst {
      case AvroAnnotations.bytes(BytesType.Fixed(size, name, doc, space)) =>
        SchemaAvro.createFixed(name, doc, space, size)
    }.getOrElse(SchemaAvro.create(SchemaAvro.Type.BYTES))

  private[codec] lazy val monthDayStructure: Seq[Schema.Field[MonthDay, Int]] = Seq(
    Schema.Field(
      "month",
      Schema.Primitive(StandardType.IntType),
      get0 = _.getMonthValue,
      set0 = (a, b: Int) => a.`with`(Month.of(b))
    ),
    Schema.Field(
      "day",
      Schema.Primitive(StandardType.IntType),
      get0 = _.getDayOfMonth,
      set0 = (a, b: Int) => a.withDayOfMonth(b)
    )
  )

  private[codec] lazy val periodStructure: Seq[Schema.Field[Period, Int]] = Seq(
    Schema
      .Field("years", Schema.Primitive(StandardType.IntType), get0 = _.getYears, set0 = (a, b: Int) => a.withYears(b)),
    Schema
      .Field(
        "months",
        Schema.Primitive(StandardType.IntType),
        get0 = _.getMonths,
        set0 = (a, b: Int) => a.withMonths(b)
      ),
    Schema.Field("days", Schema.Primitive(StandardType.IntType), get0 = _.getDays, set0 = (a, b: Int) => a.withDays(b))
  )

  private[codec] lazy val yearMonthStructure: Seq[Schema.Field[YearMonth, Int]] = Seq(
    Schema.Field(
      "year",
      Schema.Primitive(StandardType.IntType),
      get0 = _.getYear,
      set0 = (a, b: Int) => a.`with`(Year.of(b))
    ),
    Schema.Field(
      "month",
      Schema.Primitive(StandardType.IntType),
      get0 = _.getMonthValue,
      set0 = (a, b: Int) => a.`with`(Month.of(b))
    )
  )

  private[codec] lazy val durationStructure: Seq[Schema.Field[Duration, _]] = Seq(
    Schema.Field(
      "seconds",
      Schema.Primitive(StandardType.LongType),
      get0 = _.getSeconds,
      set0 = (a, b: Long) => a.plusSeconds(b)
    ),
    Schema.Field(
      "nanos",
      Schema.Primitive(StandardType.IntType),
      get0 = _.getNano,
      set0 = (a, b: Int) => a.plusNanos(b.toLong)
    )
  )

  private def toAvroSchema(schema: Schema[_]): scala.util.Either[String, SchemaAvro] = {
    schema match {
      case e: Enum[_]                    => toAvroEnum(e)
      case record: Record[_]             => toAvroRecord(record)
      case map: Schema.Map[_, _]         => toAvroMap(map)
      case seq: Schema.Sequence[_, _, _] => toAvroSchema(seq.elementSchema).map(SchemaAvro.createArray)
      case set: Schema.Set[_]            => toAvroSchema(set.elementSchema).map(SchemaAvro.createArray)
      case Transform(codec, _, _, _, _)  => toAvroSchema(codec)
      case Primitive(standardType, _) =>
        standardType match {
          case StandardType.UnitType   => Right(SchemaAvro.create(SchemaAvro.Type.NULL))
          case StandardType.StringType => Right(SchemaAvro.create(SchemaAvro.Type.STRING))
          case StandardType.BoolType   => Right(SchemaAvro.create(SchemaAvro.Type.BOOLEAN))
          case StandardType.ShortType =>
            Right(SchemaAvro.create(SchemaAvro.Type.INT).addMarkerProp(IntDiscriminator(IntType.Short)))
          case StandardType.ByteType   => Right(SchemaAvro.create(SchemaAvro.Type.INT))
          case StandardType.IntType    => Right(SchemaAvro.create(SchemaAvro.Type.INT))
          case StandardType.LongType   => Right(SchemaAvro.create(SchemaAvro.Type.LONG))
          case StandardType.FloatType  => Right(SchemaAvro.create(SchemaAvro.Type.FLOAT))
          case StandardType.DoubleType => Right(SchemaAvro.create(SchemaAvro.Type.DOUBLE))
          case StandardType.BinaryType => Right(toAvroBinary(schema))
          case StandardType.CharType =>
            Right(SchemaAvro.create(SchemaAvro.Type.INT).addMarkerProp(IntDiscriminator(IntType.Char)))
          case StandardType.UUIDType =>
            Right(LogicalTypes.uuid().addToSchema(SchemaAvro.create(SchemaAvro.Type.STRING)))
          case StandardType.BigDecimalType => toAvroDecimal(schema)
          case StandardType.BigIntegerType => toAvroDecimal(schema)
          case StandardType.DayOfWeekType =>
            Right(SchemaAvro.create(SchemaAvro.Type.INT).addMarkerProp(IntDiscriminator(IntType.DayOfWeek)))
          case StandardType.MonthType =>
            Right(SchemaAvro.create(SchemaAvro.Type.INT).addMarkerProp(IntDiscriminator(IntType.Month)))
          case StandardType.YearType =>
            Right(SchemaAvro.create(SchemaAvro.Type.INT).addMarkerProp(IntDiscriminator(IntType.Year)))
          case StandardType.ZoneIdType =>
            Right(SchemaAvro.create(SchemaAvro.Type.STRING).addMarkerProp(StringDiscriminator(StringType.ZoneId)))
          case StandardType.ZoneOffsetType =>
            Right(SchemaAvro.create(SchemaAvro.Type.INT).addMarkerProp(IntDiscriminator(IntType.ZoneOffset)))
          case StandardType.MonthDayType =>
            //TODO 1
            Right(SchemaAvro.create(SchemaAvro.Type.STRING).addMarkerProp(RecordDiscriminator(RecordType.MonthDay)))
          //Right(SchemaAvro.create(SchemaAvro.Type.RECORD))
          case StandardType.PeriodType =>
            //TODO 2
            Right(SchemaAvro.create(SchemaAvro.Type.STRING).addMarkerProp(RecordDiscriminator(RecordType.Period)))
          //Right(SchemaAvro.create(SchemaAvro.Type.RECORD))
          case StandardType.YearMonthType =>
            //TODO 3
            Right(SchemaAvro.create(SchemaAvro.Type.STRING).addMarkerProp(RecordDiscriminator(RecordType.YearMonth)))
          //toAvroSchema(yearMonthStructure).map(_.addMarkerProp(RecordDiscriminator(RecordType.YearMonth)))
          //Right(SchemaAvro.create(SchemaAvro.Type.RECORD))
          case StandardType.DurationType =>
            // TODO: Java implementation of Apache Avro does not support logical type Duration yet:
            //  AVRO-2123 with PR https://github.com/apache/avro/pull/1263
            //TODO 4
            //val chronoUnitMarker =
            //DurationChronoUnit.fromTemporalUnit(temporalUnit).getOrElse(DurationChronoUnit.default)
            //toAvroSchema(durationStructure).map(
            //  _.addMarkerProp(RecordDiscriminator(RecordType.Duration)).addMarkerProp(chronoUnitMarker))
            Right(SchemaAvro.create(SchemaAvro.Type.STRING).addMarkerProp(RecordDiscriminator(RecordType.Duration)))
          //Right(SchemaAvro.create(SchemaAvro.Type.RECORD))

          case StandardType.InstantType =>
            Right(
              SchemaAvro
                .create(SchemaAvro.Type.STRING)
                .addMarkerProp(StringDiscriminator(StringType.Instant))
            )
          case StandardType.LocalDateType =>
            Right(
              SchemaAvro
                .create(SchemaAvro.Type.STRING)
                .addMarkerProp(StringDiscriminator(StringType.LocalDate))
            )
          case StandardType.LocalTimeType =>
            Right(
              SchemaAvro
                .create(SchemaAvro.Type.STRING)
                .addMarkerProp(StringDiscriminator(StringType.LocalTime))
            )
          case StandardType.LocalDateTimeType =>
            Right(
              SchemaAvro
                .create(SchemaAvro.Type.STRING)
                .addMarkerProp(StringDiscriminator(StringType.LocalDateTime))
            )
          case StandardType.OffsetTimeType =>
            Right(
              SchemaAvro
                .create(SchemaAvro.Type.STRING)
                .addMarkerProp(StringDiscriminator(StringType.OffsetTime))
            )
          case StandardType.OffsetDateTimeType =>
            Right(
              SchemaAvro
                .create(SchemaAvro.Type.STRING)
                .addMarkerProp(StringDiscriminator(StringType.OffsetDateTime))
            )
          case StandardType.ZonedDateTimeType =>
            Right(
              SchemaAvro
                .create(SchemaAvro.Type.STRING)
                .addMarkerProp(StringDiscriminator(StringType.ZoneDateTime))
            )
        }
      case Optional(codec, _) =>
        for {
          codecName       <- getName(codec)
          codecAvroSchema <- toAvroSchema(codec)
          wrappedAvroSchema = codecAvroSchema match {
            case schema: SchemaAvro if schema.getType == SchemaAvro.Type.NULL =>
              wrapAvro(schema, codecName, UnionWrapper)
            case schema: SchemaAvro if schema.getType == SchemaAvro.Type.UNION =>
              wrapAvro(schema, codecName, UnionWrapper)
            case schema => schema
          }
        } yield SchemaAvro.createUnion(SchemaAvro.create(SchemaAvro.Type.NULL), wrappedAvroSchema)
      case Fail(message, _) => Left(message)
      case tuple: Tuple2[_, _] =>
        toAvroSchema(tuple.toRecord).map(
          _.addMarkerProp(RecordDiscriminator(RecordType.Tuple))
        )
      case e @ Schema.Either(left, right, _) =>
        val eitherUnion = for {
          l           <- toAvroSchema(left)
          r           <- toAvroSchema(right)
          lname       <- getName(left)
          rname       <- getName(right)
          leftSchema  = if (l.getType == SchemaAvro.Type.UNION) wrapAvro(l, lname, UnionWrapper) else l
          rightSchema = if (r.getType == SchemaAvro.Type.UNION) wrapAvro(r, rname, UnionWrapper) else r
          _ <- if (leftSchema.getFullName == rightSchema.getFullName)
                Left(s"Left and right schemas of either must have different fullnames: ${leftSchema.getFullName}")
              else Right(())
        } yield SchemaAvro.createUnion(leftSchema, rightSchema)

        // Unions in Avro can not hold additional properties, so we need to wrap the union in a record
        for {
          union <- eitherUnion
          name  <- getName(e)
        } yield wrapAvro(union, name, EitherWrapper)

      case Schema.Fallback(left, right, _, _) =>
        toAvroSchema(Schema.Tuple2(Schema.Optional(left), Schema.Optional(right)))

      case Lazy(schema0) => toAvroSchema(schema0())
      case Dynamic(_)    => toAvroSchema(Schema[MetaSchema])
    }
  }

  private def hasFormatToStringAnnotation(value: Chunk[Any]) = value.exists {
    case AvroAnnotations.formatToString => true
    case _                              => false
  }

  private def getTimeprecisionType(value: Chunk[Any]): Option[TimePrecisionType] = value.collectFirst {
    case AvroAnnotations.timeprecision(precision) => precision
  }

  private[codec] def toAvroInstant(
    formatter: DateTimeFormatter,
    annotations: Chunk[Any]
  ): scala.util.Either[String, SchemaAvro] =
    if (hasFormatToStringAnnotation(annotations)) {
      Right(
        SchemaAvro
          .create(SchemaAvro.Type.STRING)
          .addMarkerProp(StringDiscriminator(StringType.Instant))
          .addMarkerProp(Formatter(formatter))
      )
    } else {
      val baseSchema = SchemaAvro.create(SchemaAvro.Type.LONG)
      getTimeprecisionType(annotations).getOrElse(TimePrecisionType.default) match {
        case TimePrecisionType.Millis =>
          Right(LogicalTypes.timestampMillis().addToSchema(baseSchema).addMarkerProp(Formatter(formatter)))
        case TimePrecisionType.Micros =>
          Right(LogicalTypes.timestampMicros().addToSchema(baseSchema).addMarkerProp(Formatter(formatter)))
      }
    }

  private[codec] def toAvroLocalDate(
    formatter: DateTimeFormatter,
    annotations: Chunk[Any]
  ): scala.util.Either[String, SchemaAvro] =
    if (hasFormatToStringAnnotation(annotations)) {
      Right(
        SchemaAvro
          .create(SchemaAvro.Type.STRING)
          .addMarkerProp(StringDiscriminator(StringType.LocalDate))
          .addMarkerProp(Formatter(formatter))
      )
    } else {
      Right(LogicalTypes.date().addToSchema(SchemaAvro.create(SchemaAvro.Type.INT)).addMarkerProp(Formatter(formatter)))
    }

  private[codec] def toAvroLocalTime(
    formatter: DateTimeFormatter,
    annotations: Chunk[Any]
  ): scala.util.Either[String, SchemaAvro] =
    if (hasFormatToStringAnnotation(annotations)) {
      Right(
        SchemaAvro
          .create(SchemaAvro.Type.STRING)
          .addMarkerProp(StringDiscriminator(StringType.LocalTime))
          .addMarkerProp(Formatter(formatter))
      )
    } else {
      getTimeprecisionType(annotations).getOrElse(TimePrecisionType.default) match {
        case TimePrecisionType.Millis =>
          Right(
            LogicalTypes
              .timeMillis()
              .addToSchema(SchemaAvro.create(SchemaAvro.Type.INT))
              .addMarkerProp(Formatter(formatter))
          )
        case TimePrecisionType.Micros =>
          Right(
            LogicalTypes
              .timeMicros()
              .addToSchema(SchemaAvro.create(SchemaAvro.Type.LONG))
              .addMarkerProp(Formatter(formatter))
          )
      }
    }

  private[codec] def toAvroLocalDateTime(
    formatter: DateTimeFormatter,
    annotations: Chunk[Any]
  ): scala.util.Either[String, SchemaAvro] =
    if (hasFormatToStringAnnotation(annotations)) {
      Right(
        SchemaAvro
          .create(SchemaAvro.Type.STRING)
          .addMarkerProp(StringDiscriminator(StringType.LocalDateTime))
          .addMarkerProp(Formatter(formatter))
      )
    } else {
      val baseSchema = SchemaAvro.create(SchemaAvro.Type.LONG)
      getTimeprecisionType(annotations).getOrElse(TimePrecisionType.default) match {
        case TimePrecisionType.Millis =>
          Right(LogicalTypes.localTimestampMillis().addToSchema(baseSchema).addMarkerProp(Formatter(formatter)))
        case TimePrecisionType.Micros =>
          Right(LogicalTypes.localTimestampMicros().addToSchema(baseSchema).addMarkerProp(Formatter(formatter)))
      }
    }

  def hasAvroEnumAnnotation(annotations: Chunk[Any]): Boolean = annotations.exists {
    case AvroAnnotations.avroEnum() => true
    case _                          => false
  }

  def wrapAvro(schemaAvro: SchemaAvro, name: String, marker: AvroPropMarker): SchemaAvro = {
    val field  = new SchemaAvro.Field("value", schemaAvro)
    val fields = new java.util.ArrayList[SchemaAvro.Field]()
    fields.add(field)
    val prefixedName = s"${AvroPropMarker.wrapperNamePrefix}_$name"
    SchemaAvro
      .createRecord(prefixedName, null, AvroPropMarker.wrapperNamespace, false, fields)
      .addMarkerProp(marker)
  }

  private[codec] def toAvroEnum(enu: Enum[_]): scala.util.Either[String, SchemaAvro] = {
    val avroEnumAnnotationExists = hasAvroEnumAnnotation(enu.annotations)
    val isAvroEnumEquivalent = enu.cases.map(_.schema).forall {
      case (Transform(Primitive(standardType, _), _, _, _, _))
          if standardType == StandardType.UnitType && avroEnumAnnotationExists =>
        true
      case (Primitive(standardType, _)) if standardType == StandardType.StringType => true
      case (CaseClass0(_, _, _)) if avroEnumAnnotationExists                       => true
      case _                                                                       => false
    }
    if (isAvroEnumEquivalent) {
      for {
        name            <- getName(enu)
        doc             = getDoc(enu.annotations).orNull
        namespaceOption <- getNamespace(enu.annotations)
        symbols = enu.cases.map {
          case caseValue => getNameOption(caseValue.annotations).getOrElse(caseValue.id)
        }.toList
        result = SchemaAvro.createEnum(name, doc, namespaceOption.orNull, symbols.asJava)
      } yield result
    } else {
      val cases = enu.cases.map(c => (c.id, (c.schema, c.annotations))).map {
        case (symbol, (Transform(Primitive(standardType, _), _, _, _, _), annotations))
            if standardType == StandardType.UnitType =>
          val name = getNameOption(annotations).getOrElse(symbol)
          Right(SchemaAvro.createRecord(name, null, null, false, new java.util.ArrayList[SchemaAvro.Field]))
        case (symbol, (CaseClass0(_, _, _), annotations)) =>
          val name = getNameOption(annotations).getOrElse(symbol)
          Right(SchemaAvro.createRecord(name, null, null, false, new java.util.ArrayList[SchemaAvro.Field]))
        case (symbol, (schema, annotations)) =>
          val name           = getNameOption(annotations).getOrElse(symbol)
          val schemaWithName = addNameAnnotationIfMissing(schema, name)
          toAvroSchema(schemaWithName).map {
            case schema: SchemaAvro if schema.getType == SchemaAvro.Type.UNION =>
              wrapAvro(schema, name, UnionWrapper) // handle nested unions
            case schema => schema
          }
      }
      cases.toList.map(_.merge).partition {
        case _: String => true
        case _         => false
      } match {
        case (Nil, right: List[org.apache.avro.Schema @unchecked]) => Right(SchemaAvro.createUnion(right.asJava))
        case (left, _)                                             => Left(left.mkString("\n"))
      }
    }
  }

  private def extractAvroFields(record: Record[_]): List[org.apache.avro.Schema.Field] =
    record.fields.map(toAvroRecordField).toList.map(_.merge).partition {
      case _: String => true
      case _         => false
    } match {
      case (Nil, right: List[org.apache.avro.Schema.Field @unchecked]) => right
      case _                                                           => null
    }

  private[codec] def toAvroRecord(record: Record[_]): scala.util.Either[String, SchemaAvro] =
    for {
      name            <- getName(record)
      namespaceOption <- getNamespace(record.annotations)
      result <- Right(
                 SchemaAvro.createRecord(
                   name,
                   getDoc(record.annotations).orNull,
                   namespaceOption.orNull,
                   isErrorRecord(record),
                   extractAvroFields(record).asJava
                 )
               )
    } yield result

  private[codec] def toAvroMap(map: Map[_, _]): scala.util.Either[String, SchemaAvro] =
    map.keySchema match {
      case p: Schema.Primitive[_] if p.standardType == StandardType.StringType =>
        toAvroSchema(map.valueSchema).map(SchemaAvro.createMap)
      case _ =>
        val tupleSchema = Schema
          .Tuple2(map.keySchema, map.valueSchema)
          .annotate(AvroAnnotations.name("Tuple"))
          .annotate(AvroAnnotations.namespace("scala"))
        toAvroSchema(tupleSchema).map(SchemaAvro.createArray)
    }

  private[codec] def toAvroDecimal(schema: Schema[_]): scala.util.Either[String, SchemaAvro] = {
    val scale = schema.annotations.collectFirst { case AvroAnnotations.scale(s) => s }
      .getOrElse(AvroAnnotations.scale().scale)
    val precision = schema match {
      case Primitive(StandardType.BigDecimalType, _) =>
        schema.annotations.collectFirst { case AvroAnnotations.precision(p) => p }
          .getOrElse(Math.max(scale, AvroAnnotations.precision().precision))
      case _ => scale
    }

    val baseAvroType = schema.annotations.collectFirst { case AvroAnnotations.decimal(decimalType) => decimalType }
      .getOrElse(DecimalType.default) match {
      case DecimalType.Fixed(size) =>
        for {
          namespaceOption <- getNamespace(schema.annotations)
          name            = getNameOption(schema.annotations).getOrElse(s"Decimal_${precision}_$scale")
          doc             = getDoc(schema.annotations).orNull
          result          = SchemaAvro.createFixed(name, doc, namespaceOption.orNull, size)
        } yield result
      case DecimalType.Bytes => Right(SchemaAvro.create(SchemaAvro.Type.BYTES))
    }
    baseAvroType.map(
      LogicalTypes
        .decimal(precision, scale)
        .addToSchema(_)
    )
  }

  private[codec] def toErrorMessage(err: Throwable, at: AnyRef) =
    s"Error mapping to Apache Avro schema: $err at ${at.toString}"

  private[codec] def toAvroRecordField[Z](value: Field[Z, _]): scala.util.Either[String, SchemaAvro.Field] =
    toAvroSchema(value.schema).map(
      schema =>
        new SchemaAvro.Field(
          getNameOption(value.annotations).getOrElse(value.name),
          schema,
          getDoc(value.annotations).orNull,
          getDefault(value.annotations).orNull,
          getFieldOrder(value.annotations).map(_.toAvroOrder).getOrElse(FieldOrderType.default.toAvroOrder)
        )
    )

  private[codec] def getFieldOrder(annotations: Chunk[Any]): Option[FieldOrderType] =
    annotations.collectFirst { case AvroAnnotations.fieldOrder(fieldOrderType) => fieldOrderType }

  private[codec] def getName(schema: Schema[_]): scala.util.Either[String, String] = {
    val validNameRegex = raw"[A-Za-z_][A-Za-z0-9_]*".r

    schema.annotations.collectFirst { case AvroAnnotations.name(name) => name } match {
      case Some(s) =>
        s match {
          case validNameRegex() => Right(s)
          case _ =>
            Left(s"Invalid Avro name: $s")
        }
      case None =>
        schema match {
          case r: Record[_] => Right(r.id.name)
          case e: Enum[_]   => Right(e.id.name)
          case _            => Right(s"hashed_${schema.ast.toString.hashCode().toString.replaceFirst("-", "n")}")
          // TODO: better way to generate a (maybe stable) name?
        }
    }
  }

  private[codec] def getNameOption(annotations: Chunk[Any]): Option[String] =
    annotations.collectFirst { case AvroAnnotations.name(name) => name }

  private[codec] def getDoc(annotations: Chunk[Any]): Option[String] =
    annotations.collectFirst { case AvroAnnotations.doc(doc) => doc }

  private[codec] def getDefault(annotations: Chunk[Any]): Option[java.lang.Object] =
    annotations.collectFirst { case AvroAnnotations.default(javaDefaultObject) => javaDefaultObject }

  private[codec] def getNamespace(annotations: Chunk[Any]): scala.util.Either[String, Option[String]] = {
    val validNamespaceRegex = raw"[A-Za-z_][A-Za-z0-9_]*(\.[A-Za-z_][A-Za-z0-9_]*)*".r

    annotations.collectFirst { case AvroAnnotations.namespace(ns) => ns } match {
      case Some(s) =>
        s match {
          case validNamespaceRegex(_) => Right(Some(s))
          case _                      => Left(s"Invalid Avro namespace: $s")
        }
      case None => Right(None)
    }
  }

  private[codec] def isErrorRecord(record: Record[_]): Boolean =
    record.annotations.collectFirst { case AvroAnnotations.error => () }.nonEmpty

  private[codec] def addNameAnnotationIfMissing[B <: StaticAnnotation](schema: Schema[_], name: String): Schema[_] =
    schema.annotations.collectFirst { case AvroAnnotations.name(_) => schema }
      .getOrElse(schema.annotate(AvroAnnotations.name(name)))

  private[codec] def toZioDecimal(
    avroSchema: SchemaAvro,
    decimalType: DecimalType
  ): scala.util.Either[String, Schema[_]] = {
    val decimalTypeAnnotation = AvroAnnotations.decimal(decimalType)
    val decimalLogicalType    = avroSchema.getLogicalType.asInstanceOf[LogicalTypes.Decimal]
    val precision             = decimalLogicalType.getPrecision
    val scale                 = decimalLogicalType.getScale
    if (precision - scale > 0) {
      Right(
        Schema
          .primitive(StandardType.BigDecimalType)
          .annotate(AvroAnnotations.scale(scale))
          .annotate(AvroAnnotations.precision(precision))
          .annotate(decimalTypeAnnotation)
      )
    } else {
      Right(
        Schema
          .primitive(StandardType.BigIntegerType)
          .annotate(AvroAnnotations.scale(scale))
          .annotate(decimalTypeAnnotation)
      )
    }
  }

  private[codec] def toZioEnumeration[A, Z](avroSchema: SchemaAvro): scala.util.Either[String, Schema[Z]] = {
    val cases = avroSchema.getTypes.asScala
      .map(t => {
        val inner =
          if (t.getType == SchemaAvro.Type.RECORD && t.getFields.size() == 1 && t
                .getObjectProp(UnionWrapper.propName) == true) {
            t.getFields.asScala.head.schema() // unwrap nested union
          } else t
        toZioSchema(inner).map(
          s =>
            Schema.Case[Z, A](
              t.getFullName,
              s.asInstanceOf[Schema[A]],
              _.asInstanceOf[A],
              _.asInstanceOf[Z],
              (z: Z) => z.isInstanceOf[A @unchecked]
            )
        )
      })
    val caseSet = cases.toList.map(_.merge).partition {
      case _: String => true
      case _         => false
    } match {
      case (Nil, right: Seq[Case[_, _] @unchecked]) =>
        Try {
          CaseSet(right: _*).asInstanceOf[CaseSet { type EnumType = Z }]
        }.toEither.left.map(_.getMessage)
      case (left, _) => Left(left.mkString("\n"))
    }
    caseSet.map(cs => Schema.enumeration(TypeId.parse(avroSchema.getName), cs))
  }

  private[codec] def toZioRecord(avroSchema: SchemaAvro): scala.util.Either[String, Schema[_]] =
    if (avroSchema.getObjectProp(UnionWrapper.propName) != null) {
      avroSchema.getFields.asScala.headOption match {
        case Some(value) => toZioSchema(value.schema())
        case None        => Left("ZIO schema wrapped record must have a single field")
      }
    } else if (avroSchema.getObjectProp(EitherWrapper.propName) != null) {
      avroSchema.getFields.asScala.headOption match {
        case Some(value) =>
          toZioSchema(value.schema()).flatMap {
            case enu: Enum[_] =>
              enu.cases.toList match {
                case first :: second :: Nil => Right(Schema.either(first.schema, second.schema))
                case _                      => Left("ZIO schema wrapped either must have exactly two cases")
              }
            case e: Schema.Either[_, _]                                                              => Right(e)
            case f: Schema.Fallback[_, _]                                                            => Right(f)
            case c: CaseClass0[_]                                                                    => Right(c)
            case c: CaseClass1[_, _]                                                                 => Right(c)
            case c: CaseClass2[_, _, _]                                                              => Right(c)
            case c: CaseClass3[_, _, _, _]                                                           => Right(c)
            case c: CaseClass4[_, _, _, _, _]                                                        => Right(c)
            case c: CaseClass5[_, _, _, _, _, _]                                                     => Right(c)
            case c: CaseClass6[_, _, _, _, _, _, _]                                                  => Right(c)
            case c: CaseClass7[_, _, _, _, _, _, _, _]                                               => Right(c)
            case c: CaseClass8[_, _, _, _, _, _, _, _, _]                                            => Right(c)
            case c: CaseClass9[_, _, _, _, _, _, _, _, _, _]                                         => Right(c)
            case c: CaseClass10[_, _, _, _, _, _, _, _, _, _, _]                                     => Right(c)
            case c: CaseClass11[_, _, _, _, _, _, _, _, _, _, _, _]                                  => Right(c)
            case c: CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, _]                               => Right(c)
            case c: CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, _]                            => Right(c)
            case c: CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                         => Right(c)
            case c: CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                      => Right(c)
            case c: CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                   => Right(c)
            case c: CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]                => Right(c)
            case c: CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]             => Right(c)
            case c: CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]          => Right(c)
            case c: CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]       => Right(c)
            case c: CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _]    => Right(c)
            case c: CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _] => Right(c)
            case c: Dynamic                                                                          => Right(c)
            case c: GenericRecord                                                                    => Right(c)
            case c: Map[_, _]                                                                        => Right(c)
            case c: Sequence[_, _, _]                                                                => Right(c)
            case c: Set[_]                                                                           => Right(c)
            case c: Fail[_]                                                                          => Right(c)
            case c: Lazy[_]                                                                          => Right(c)
            case c: Optional[_]                                                                      => Right(c)
            case c: Primitive[_]                                                                     => Right(c)
            case c: Transform[_, _, _]                                                               => Right(c)
            case c: Tuple2[_, _]                                                                     => Right(c)

          }
        case None => Left("ZIO schema wrapped record must have a single field")
      }
    } else {
      val annotations = buildZioAnnotations(avroSchema)
      extractZioFields(avroSchema).map { (fs: List[Field[ListMap[String, _], _]]) =>
        Schema.record(TypeId.parse(avroSchema.getName), fs: _*).addAllAnnotations(annotations)
      }
    }

  private def extractZioFields[Z](avroSchema: SchemaAvro): scala.util.Either[String, List[Field[Z, _]]] =
    avroSchema.getFields.asScala.map(toZioField).toList.map(_.merge).partition {
      case _: String => true
      case _         => false
    } match {
      case (Nil, right: List[Field[Z, _] @unchecked]) => Right(right)
      case (left, _)                                  => Left(left.mkString("\n"))
    }

  private[codec] def toZioField(field: SchemaAvro.Field): scala.util.Either[String, Field[ListMap[String, _], _]] =
    toZioSchema(field.schema())
      .map(
        (s: Schema[_]) =>
          Field(
            field.name(),
            s.asInstanceOf[Schema[Any]],
            buildZioAnnotations(field),
            get0 = (p: ListMap[String, _]) => p(field.name()),
            set0 = (p: ListMap[String, _], v: Any) => p.updated(field.name(), v)
          )
      )

  private[codec] def toZioTuple(schema: SchemaAvro): scala.util.Either[String, Schema[_]] =
    for {
      _ <- scala.util.Either
            .cond(schema.getFields.size() == 2, (), "Tuple must have exactly 2 fields:" + schema.toString(false))
      _1 <- toZioSchema(schema.getFields.get(0).schema())
      _2 <- toZioSchema(schema.getFields.get(1).schema())
    } yield Schema.Tuple2(_1, _2, buildZioAnnotations(schema))

  private[codec] def buildZioAnnotations(schema: SchemaAvro): Chunk[StaticAnnotation] = {
    val name = AvroAnnotations.name(schema.getName)
    val namespace = Try {
      Option(schema.getNamespace).map(AvroAnnotations.namespace.apply)
    }.toOption.flatten
    val doc = if (schema.getDoc != null) Some(AvroAnnotations.doc(schema.getDoc)) else None
    val aliases = Try {
      if (schema.getAliases != null && !schema.getAliases.isEmpty)
        Some(AvroAnnotations.aliases(schema.getAliases.asScala.toSet))
      else None
    }.toOption.flatten
    val error = Try {
      if (schema.isError) Some(AvroAnnotations.error) else None
    }.toOption.flatten
    val default = Try {
      if (schema.getEnumDefault != null) Some(AvroAnnotations.default(schema.getEnumDefault)) else None
    }.toOption.flatten
    Chunk(name) ++ namespace ++ doc ++ aliases ++ error ++ default
  }

  private[codec] def buildZioAnnotations(field: SchemaAvro.Field): Chunk[Any] = {
    val nameAnnotation = Some(AvroAnnotations.name(field.name))
    val docAnnotation  = if (field.doc() != null) Some(AvroAnnotations.doc(field.doc)) else None
    val aliasesAnnotation =
      if (!field.aliases().isEmpty) Some(AvroAnnotations.aliases(field.aliases.asScala.toSet)) else None
    val default = Try {
      if (field.hasDefaultValue) Some(AvroAnnotations.default(field.defaultVal())) else None
    }.toOption.flatten
    val orderAnnotation = Some(AvroAnnotations.fieldOrder(FieldOrderType.fromAvroOrder(field.order())))
    val annotations: Seq[StaticAnnotation] =
      List(nameAnnotation, docAnnotation, aliasesAnnotation, orderAnnotation, default).flatten
    Chunk.fromIterable(annotations)
  }

  private[codec] def toZioStringEnum(avroSchema: SchemaAvro): scala.util.Either[String, Schema[_]] = {
    val cases =
      avroSchema.getEnumSymbols.asScala
        .map(s => Schema.Case[String, String](s, Schema[String], identity, identity, _.isInstanceOf[String]))
        .toSeq
    val caseSet                     = CaseSet[String](cases: _*).asInstanceOf[Aux[String]]
    val enumeration: Schema[String] = Schema.enumeration(TypeId.parse("org.apache.avro.Schema"), caseSet)
    Right(enumeration.addAllAnnotations(buildZioAnnotations(avroSchema)))
  }

  private[codec] case object OptionUnion {

    def unapply(schema: SchemaAvro): Option[SchemaAvro] =
      if (schema.getType == SchemaAvro.Type.UNION) {
        val types = schema.getTypes
        if (types.size == 2) {
          if (types.get(0).getType == SchemaAvro.Type.NULL ||
              types.get(1).getType == SchemaAvro.Type.NULL) {
            if (types.get(1).getType != SchemaAvro.Type.NULL) {
              Some(types.get(1))
            } else if (types.get(0).getType != SchemaAvro.Type.NULL) {
              Some(types.get(0))
            } else {
              None
            }
          } else {
            None
          }
        } else {
          None
        }
      } else {
        None
      }
  }

  private case object EitherUnion {

    def unapply(schema: SchemaAvro): Option[(SchemaAvro, SchemaAvro)] =
      if (schema.getType == SchemaAvro.Type.UNION &&
          schema.getObjectProp(EitherWrapper.propName) == EitherWrapper.value) {
        val types = schema.getTypes
        if (types.size == 2) {
          Some(types.get(0) -> types.get(1))
        } else {
          None
        }
      } else {
        None
      }
  }

  implicit private class SchemaExtensions(schema: Schema[_]) {

    def addAllAnnotations(annotations: Chunk[Any]): Schema[_] =
      annotations.foldLeft(schema)((schema, annotation) => schema.annotate(annotation))
  }

  implicit private class SchemaAvroExtensions(schemaAvro: SchemaAvro) {

    def addMarkerProp(propMarker: AvroPropMarker): SchemaAvro = {
      schemaAvro.addProp(propMarker.propName, propMarker.value)
      schemaAvro
    }
  }
}
