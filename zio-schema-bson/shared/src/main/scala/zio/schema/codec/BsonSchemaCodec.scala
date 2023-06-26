package zio.schema.codec

import java.nio.charset.StandardCharsets
import java.time.Instant

import scala.collection.compat._
import scala.collection.immutable.{ HashMap, ListMap }
import scala.jdk.CollectionConverters._

import org.bson.{ BsonDocument, BsonNull, BsonReader, BsonType, BsonValue, BsonWriter }

import zio.bson.BsonBuilder._
import zio.bson.DecoderUtils._
import zio.bson.{
  BsonCodec,
  BsonDecoder,
  BsonEncoder,
  BsonEncoderOps,
  BsonFieldDecoder,
  BsonFieldEncoder,
  BsonTrace,
  bsonDiscriminator,
  bsonExclude,
  bsonField,
  bsonHint,
  bsonNoExtraFields
}
import zio.schema.annotation.{
  caseName,
  caseNameAliases,
  directDynamicMapping,
  discriminatorName,
  fieldDefaultValue,
  fieldNameAliases,
  noDiscriminator,
  rejectExtraFields,
  transientCase,
  transientField
}
import zio.schema.{ DynamicValue, Schema, StandardType, TypeId }
import zio.{ Chunk, ChunkBuilder, Unsafe }

object BsonSchemaCodec {

  {
    // TODO: better way to prevent scalafix from removing the import
    val _ = IterableOnce
  }

  def bsonEncoder[A](schema: Schema[A]): BsonEncoder[A] =
    BsonSchemaEncoder.schemaEncoder(schema)

  def bsonDecoder[A](schema: Schema[A]): BsonDecoder[A] =
    BsonSchemaDecoder.schemaDecoder(schema)

  def bsonCodec[A](schema: Schema[A]): BsonCodec[A] =
    BsonCodec(bsonEncoder(schema), bsonDecoder(schema))

  object Codecs {
    protected[codec] val unitEncoder: BsonEncoder[Unit] = new BsonEncoder[Unit] {
      override def encode(writer: BsonWriter, value: Unit, ctx: BsonEncoder.EncoderContext): Unit =
        if (!ctx.inlineNextObject) {
          writer.writeStartDocument()
          writer.writeEndDocument()
        }

      override def toBsonValue(value: Unit): BsonValue = doc()
    }

    private[codec] val unitDecoder: BsonDecoder[Unit] =
      new BsonDecoder[Unit] {
        private val noExtra = true // TODO: configuration

        override def decodeUnsafe(
          reader: BsonReader,
          trace: List[BsonTrace],
          ctx: BsonDecoder.BsonDecoderContext
        ): Unit = unsafeCall(trace) {
          reader.readStartDocument()

          while (reader.readBsonType() != BsonType.END_OF_DOCUMENT) {
            val name = reader.readName()

            if (noExtra && !ctx.ignoreExtraField.contains(name)) {
              throw BsonDecoder.Error(BsonTrace.Field(name) :: trace, "Invalid extra field.")
            } else reader.skipValue()
          }

          reader.readEndDocument()

          ()
        }

        override def fromBsonValueUnsafe(
          value: BsonValue,
          trace: List[BsonTrace],
          ctx: BsonDecoder.BsonDecoderContext
        ): Unit =
          assumeType(trace)(BsonType.DOCUMENT, value) { value =>
            if (noExtra) {
              value.asDocument().asScala.keys.foreach { name =>
                if (!ctx.ignoreExtraField.contains(name))
                  throw BsonDecoder.Error(BsonTrace.Field(name) :: trace, "Invalid extra field.")
              }
            }

            ()
          }

      }

    protected[codec] val unitCodec: BsonCodec[Unit] = BsonCodec(unitEncoder, unitDecoder)

    protected[codec] def tuple2Encoder[A: BsonEncoder, B: BsonEncoder]: BsonEncoder[(A, B)] =
      new BsonEncoder[(A, B)] {
        override def encode(writer: BsonWriter, value: (A, B), ctx: BsonEncoder.EncoderContext): Unit = {
          val nextCtx = BsonEncoder.EncoderContext.default

          if (!ctx.inlineNextObject) writer.writeStartDocument()

          writer.writeName("_1")
          BsonEncoder[A].encode(writer, value._1, nextCtx)

          writer.writeName("_2")
          BsonEncoder[B].encode(writer, value._2, nextCtx)

          if (!ctx.inlineNextObject) writer.writeEndDocument()
        }

        override def toBsonValue(value: (A, B)): BsonValue =
          doc(
            "_1" -> value._1.toBsonValue,
            "_2" -> value._2.toBsonValue
          )
      }

    protected[codec] def tuple2Decoder[A: BsonDecoder, B: BsonDecoder]: BsonDecoder[(A, B)] =
      new BsonDecoder[(A, B)] {
        private val noExtra = true // TODO: configuration

        override def decodeUnsafe(
          reader: BsonReader,
          trace: List[BsonTrace],
          ctx: BsonDecoder.BsonDecoderContext
        ): (A, B) = unsafeCall(trace) {
          val nextCtx = BsonDecoder.BsonDecoderContext.default
          var _1: A   = null.asInstanceOf[A]
          var has_1   = false
          var _2: B   = null.asInstanceOf[B]
          var has_2   = false

          reader.readStartDocument()

          while (reader.readBsonType() != BsonType.END_OF_DOCUMENT) {
            val name = reader.readName()

            def fieldTrace = BsonTrace.Field(name) :: trace

            if (name == "_1") {
              _1 = BsonDecoder[A].decodeUnsafe(reader, fieldTrace, nextCtx)
              has_1 = true
            } else if (name == "_2") {
              _2 = BsonDecoder[B].decodeUnsafe(reader, fieldTrace, nextCtx)
              has_2 = true
            } else if (noExtra & !ctx.ignoreExtraField.contains(name)) {
              throw BsonDecoder.Error(fieldTrace, "Invalid extra field.")
            } else reader.skipValue()
          }

          reader.readEndDocument()

          if (!has_1) _1 = BsonDecoder[A].decodeMissingUnsafe(trace)
          if (!has_2) _2 = BsonDecoder[B].decodeMissingUnsafe(trace)

          (_1, _2)
        }

        override def fromBsonValueUnsafe(
          value: BsonValue,
          trace: List[BsonTrace],
          ctx: BsonDecoder.BsonDecoderContext
        ): (A, B) =
          assumeType(trace)(BsonType.DOCUMENT, value) { value =>
            val nextCtx = BsonDecoder.BsonDecoderContext.default
            var _1: A   = null.asInstanceOf[A]
            var has_1   = false
            var _2: B   = null.asInstanceOf[B]
            var has_2   = false

            value.asDocument().asScala.foreachEntry { (name, value) =>
              def fieldTrace = BsonTrace.Field(name) :: trace

              if (name == "_1") {
                _1 = BsonDecoder[A].fromBsonValueUnsafe(value, fieldTrace, nextCtx)
                has_1 = true
              } else if (name == "_2") {
                _2 = BsonDecoder[B].fromBsonValueUnsafe(value, fieldTrace, nextCtx)
                has_2 = true
              } else if (noExtra & !ctx.ignoreExtraField.contains(name)) {
                throw BsonDecoder.Error(fieldTrace, "Invalid extra field.")
              }
            }

            if (!has_1) _1 = BsonDecoder[A].decodeMissingUnsafe(trace)
            if (!has_2) _2 = BsonDecoder[B].decodeMissingUnsafe(trace)

            (_1, _2)
          }
      }

    protected[codec] def tuple2Codec[A: BsonEncoder: BsonDecoder, B: BsonEncoder: BsonDecoder]: BsonCodec[(A, B)] =
      BsonCodec(tuple2Encoder, tuple2Decoder)

    protected[codec] def eitherEncoder[A: BsonEncoder, B: BsonEncoder]: BsonEncoder[Either[A, B]] =
      new BsonEncoder[Either[A, B]] {
        override def encode(writer: BsonWriter, value: Either[A, B], ctx: BsonEncoder.EncoderContext): Unit = {
          val nextCtx = BsonEncoder.EncoderContext.default

          if (!ctx.inlineNextObject) writer.writeStartDocument()

          value match {
            case Left(value) =>
              writer.writeName("left")
              BsonEncoder[A].encode(writer, value, nextCtx)
            case Right(value) =>
              writer.writeName("right")
              BsonEncoder[B].encode(writer, value, nextCtx)
          }

          if (!ctx.inlineNextObject) writer.writeEndDocument()
        }

        override def toBsonValue(value: Either[A, B]): BsonValue = value match {
          case Left(value)  => doc("left"  -> value.toBsonValue)
          case Right(value) => doc("right" -> value.toBsonValue)
        }
      }

    protected[codec] def eitherDecoder[A: BsonDecoder, B: BsonDecoder]: BsonDecoder[Either[A, B]] =
      new BsonDecoder[Either[A, B]] {
        private val noExtra = true // TODO: configuration

        override def decodeUnsafe(
          reader: BsonReader,
          trace: List[BsonTrace],
          ctx: BsonDecoder.BsonDecoderContext
        ): Either[A, B] = unsafeCall(trace) {
          val nextCtx  = BsonDecoder.BsonDecoderContext.default
          var left: A  = null.asInstanceOf[A]
          var hasLeft  = false
          var right: B = null.asInstanceOf[B]
          var hasRight = false

          reader.readStartDocument()

          while (reader.readBsonType() != BsonType.END_OF_DOCUMENT) {
            val name = reader.readName()

            def fieldTrace = BsonTrace.Field(name) :: trace

            if (name == "left") {
              left = BsonDecoder[A].decodeUnsafe(reader, fieldTrace, nextCtx)
              hasLeft = true
            } else if (name == "right") {
              right = BsonDecoder[B].decodeUnsafe(reader, fieldTrace, nextCtx)
              hasRight = true
            } else if (noExtra & !ctx.ignoreExtraField.contains(name)) {
              throw BsonDecoder.Error(fieldTrace, "Invalid extra field.")
            } else reader.skipValue()
          }

          reader.readEndDocument()

          if (hasLeft && hasRight) throw BsonDecoder.Error(trace, "Both `left` and `right` cases found.")

          if (hasLeft) Left(left)
          else if (hasRight) Right(right)
          else throw BsonDecoder.Error(trace, "Both `left` and `right` cases missing.")
        }

        override def fromBsonValueUnsafe(
          value: BsonValue,
          trace: List[BsonTrace],
          ctx: BsonDecoder.BsonDecoderContext
        ): Either[A, B] =
          assumeType(trace)(BsonType.DOCUMENT, value) { value =>
            val nextCtx  = BsonDecoder.BsonDecoderContext.default
            var left: A  = null.asInstanceOf[A]
            var hasLeft  = false
            var right: B = null.asInstanceOf[B]
            var hasRight = false

            value.asDocument().asScala.foreachEntry { (name, value) =>
              def fieldTrace = BsonTrace.Field(name) :: trace

              if (name == "left") {
                left = BsonDecoder[A].fromBsonValueUnsafe(value, fieldTrace, nextCtx)
                hasLeft = true
              } else if (name == "right") {
                right = BsonDecoder[B].fromBsonValueUnsafe(value, fieldTrace, nextCtx)
                hasRight = true
              } else if (noExtra & !ctx.ignoreExtraField.contains(name)) {
                throw BsonDecoder.Error(fieldTrace, "Invalid extra field.")
              }
            }

            if (hasLeft && hasRight) throw BsonDecoder.Error(trace, "Both `left` and `right` cases found.")

            if (hasLeft) Left(left)
            else if (hasRight) Right(right)
            else throw BsonDecoder.Error(trace, "Both `left` and `right` cases missing.")
          }
      }

    protected[codec] def failDecoder[A](message: String): BsonDecoder[A] =
      new BsonDecoder[A] {
        override def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): A =
          throw BsonDecoder.Error(trace, message)

        override def fromBsonValueUnsafe(
          value: BsonValue,
          trace: List[BsonTrace],
          ctx: BsonDecoder.BsonDecoderContext
        ): A =
          throw BsonDecoder.Error(trace, message)
      }

    // TODO: remove asInstanceOf somehow
    private[codec] def primitiveCodec[A](standardType: StandardType[A]): BsonCodec[A] =
      standardType match {
        case StandardType.UnitType       => unitCodec.asInstanceOf[BsonCodec[A]]
        case StandardType.StringType     => BsonCodec.string.asInstanceOf[BsonCodec[A]]
        case StandardType.BoolType       => BsonCodec.boolean.asInstanceOf[BsonCodec[A]]
        case StandardType.ByteType       => BsonCodec.byte.asInstanceOf[BsonCodec[A]]
        case StandardType.ShortType      => BsonCodec.short.asInstanceOf[BsonCodec[A]]
        case StandardType.IntType        => BsonCodec.int.asInstanceOf[BsonCodec[A]]
        case StandardType.LongType       => BsonCodec.long.asInstanceOf[BsonCodec[A]]
        case StandardType.FloatType      => BsonCodec.float.asInstanceOf[BsonCodec[A]]
        case StandardType.DoubleType     => BsonCodec.double.asInstanceOf[BsonCodec[A]]
        case StandardType.BinaryType     => BsonCodec.byteIterable[Chunk].asInstanceOf[BsonCodec[A]]
        case StandardType.CharType       => BsonCodec.char.asInstanceOf[BsonCodec[A]]
        case StandardType.BigIntegerType => BsonCodec.bigInteger.asInstanceOf[BsonCodec[A]]
        case StandardType.BigDecimalType => BsonCodec.bigDecimal.asInstanceOf[BsonCodec[A]]
        case StandardType.UUIDType       => BsonCodec.uuid.asInstanceOf[BsonCodec[A]]
        case StandardType.DayOfWeekType =>
          BsonCodec.dayOfWeek.asInstanceOf[BsonCodec[A]] // BsonCodec[java.time.DayOfWeek]
        case StandardType.DurationType =>
          BsonCodec.duration.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.Duration]
        case StandardType.InstantType => BsonCodec.instant.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.Instant]
        case StandardType.LocalDateType =>
          BsonCodec.localDate.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.LocalDate]
        case StandardType.LocalDateTimeType =>
          BsonCodec.localDateTime.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.LocalDateTime]
        case StandardType.LocalTimeType =>
          BsonCodec.localTime.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.LocalTime]
        case StandardType.MonthType => BsonCodec.month.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.Month]
        case StandardType.MonthDayType =>
          BsonCodec.monthDay.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.MonthDay]
        case StandardType.OffsetDateTimeType =>
          BsonCodec.offsetDateTime.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.OffsetDateTime]
        case StandardType.OffsetTimeType =>
          BsonCodec.offsetTime.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.OffsetTime]
        case StandardType.PeriodType => BsonCodec.period.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.Period]
        case StandardType.YearType   => BsonCodec.year.asInstanceOf[BsonCodec[A]]   //BsonCodec[java.time.Year]
        case StandardType.YearMonthType =>
          BsonCodec.yearMonth.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.YearMonth]
        case StandardType.ZonedDateTimeType =>
          BsonCodec.zonedDateTime.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.ZonedDateTime]
        case StandardType.ZoneIdType => BsonCodec.zoneId.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.ZoneId]
        case StandardType.ZoneOffsetType =>
          BsonCodec.zoneOffset.asInstanceOf[BsonCodec[A]] //BsonCodec[java.time.ZoneOffset]
        // case StandardType.CurrencyType => BsonCodec.currency.asInstanceOf[BsonCodec[A]] //BsonCodec[java.util.Currency] // TODO: needs implementation in zio-bson
      }
  }

  object BsonSchemaEncoder {

    import Codecs._
    import ProductEncoder._

    private[codec] val CHARSET = StandardCharsets.UTF_8

    private def chunkEncoder[A: BsonEncoder]: BsonEncoder[Chunk[A]] = BsonEncoder.iterable[A, Chunk]

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[codec] def schemaEncoder[A](schema: Schema[A]): BsonEncoder[A] =
      schema match {
        case Schema.Primitive(standardType, _)     => primitiveCodec(standardType).encoder
        case Schema.Sequence(schema, _, g, _, _)   => chunkEncoder(schemaEncoder(schema)).contramap(g)
        case Schema.Map(ks, vs, _)                 => mapEncoder(ks, vs)
        case Schema.Set(s, _)                      => chunkEncoder(schemaEncoder(s)).contramap(m => Chunk.fromIterable(m))
        case Schema.Transform(c, _, g, _, _)       => transformEncoder(c, g)
        case Schema.Tuple2(l, r, _)                => tuple2Encoder(schemaEncoder(l), schemaEncoder(r))
        case Schema.Optional(schema, _)            => BsonEncoder.option(schemaEncoder(schema))
        case Schema.Fail(_, _)                     => unitEncoder.contramap(_ => ())
        case Schema.GenericRecord(_, structure, _) => genericRecordEncoder(structure.toChunk)
        case Schema.Either(left, right, _)         => eitherEncoder(schemaEncoder(left), schemaEncoder(right))
        case l @ Schema.Lazy(_)                    => schemaEncoder(l.schema)
        case r: Schema.Record[A]                   => caseClassEncoder(r)
        case e: Schema.Enum[A]                     => enumEncoder(e, e.cases)
        case d @ Schema.Dynamic(_)                 => dynamicEncoder(d)
        case null                                  => throw new Exception(s"A captured schema is null, most likely due to wrong field initialization order")
      }
    //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

    private[codec] def bsonFieldEncoder[A](schema: Schema[A]): Option[BsonFieldEncoder[A]] =
      schema match {
        case Schema.Primitive(StandardType.StringType, _) => Option(BsonFieldEncoder.string)
        case Schema.Primitive(StandardType.LongType, _)   => Option(BsonFieldEncoder.long)
        case Schema.Primitive(StandardType.IntType, _)    => Option(BsonFieldEncoder.int)
        case _                                            => None
      }

    private[codec] def mapEncoder[K, V](ks: Schema[K], vs: Schema[V]): BsonEncoder[Map[K, V]] = {
      val valueEncoder = BsonSchemaEncoder.schemaEncoder(vs)
      bsonFieldEncoder(ks) match {
        case Some(bsonFieldEncoder) =>
          BsonEncoder.map(bsonFieldEncoder, valueEncoder)
        case None =>
          chunkEncoder(tuple2Encoder(schemaEncoder(ks), schemaEncoder(vs)))
            .contramap(m => Chunk.fromIterable(m))
      }
    }

    private def dynamicEncoder(schema: Schema.Dynamic): BsonEncoder[DynamicValue] = {
      val directMapping = schema.annotations.exists {
        case directDynamicMapping() => true
        case _                      => false
      }

      if (directMapping) {
        new BsonEncoder[DynamicValue] {
          directEncoder =>
          override def encode(writer: BsonWriter, value: DynamicValue, ctx: BsonEncoder.EncoderContext): Unit =
            value match {
              case DynamicValue.Record(_, values) =>
                val nextCtx = BsonEncoder.EncoderContext.default

                if (!ctx.inlineNextObject) writer.writeStartDocument()

                values.foreach {
                  case (key, value) =>
                    writer.writeName(key)
                    directEncoder.encode(writer, value, nextCtx)
                }

                if (!ctx.inlineNextObject) writer.writeEndDocument()

              case DynamicValue.Enumeration(_, _) =>
                throw new RuntimeException(s"DynamicValue.Enumeration is not supported in directDynamicMapping mode")
              case DynamicValue.Sequence(values) =>
                chunkEncoder(directEncoder).encode(writer, values, ctx)
              case DynamicValue.Dictionary(_) =>
                throw new Exception(s"DynamicValue.Dictionary is not supported in directDynamicMapping mode")
              case DynamicValue.SetValue(values) =>
                BsonEncoder.iterable[DynamicValue, Set](directEncoder).encode(writer, values, ctx)
              case DynamicValue.Primitive(value, standardType) =>
                primitiveCodec(standardType).encoder.encode(writer, value, ctx)
              case DynamicValue.Singleton(_)     => unitEncoder.encode(writer, (), ctx)
              case DynamicValue.SomeValue(value) => directEncoder.encode(writer, value, ctx)
              case DynamicValue.NoneValue        => writer.writeNull()
              case DynamicValue.Tuple(_, _) =>
                throw new Exception(s"DynamicValue.Tuple is not supported in directDynamicMapping mode")
              case DynamicValue.LeftValue(_) =>
                throw new Exception(s"DynamicValue.LeftValue is not supported in directDynamicMapping mode")
              case DynamicValue.RightValue(_) =>
                throw new Exception(s"DynamicValue.RightValue is not supported in directDynamicMapping mode")
              case DynamicValue.DynamicAst(_) =>
                throw new Exception(s"DynamicValue.DynamicAst is not supported in directDynamicMapping mode")
              case DynamicValue.Error(message) =>
                throw new Exception(message)
            }

          override def toBsonValue(value: DynamicValue): BsonValue =
            value match {
              case DynamicValue.Record(_, values) =>
                new BsonDocument(values.view.map {
                  case (key, value) => element(key, directEncoder.toBsonValue(value))
                }.to(Chunk).asJava)

              case DynamicValue.Enumeration(_, _) =>
                throw new RuntimeException(s"DynamicValue.Enumeration is not supported in directDynamicMapping mode")
              case DynamicValue.Sequence(values) =>
                chunkEncoder(directEncoder).toBsonValue(values)
              case DynamicValue.Dictionary(_) =>
                throw new Exception(s"DynamicValue.Dictionary is not supported in directDynamicMapping mode")
              case DynamicValue.SetValue(values) =>
                BsonEncoder.iterable[DynamicValue, Set](directEncoder).toBsonValue(values)
              case DynamicValue.Primitive(value, standardType) =>
                primitiveCodec(standardType).encoder.toBsonValue(value)
              case DynamicValue.Singleton(_)     => doc()
              case DynamicValue.SomeValue(value) => directEncoder.toBsonValue(value)
              case DynamicValue.NoneValue        => BsonNull.VALUE
              case DynamicValue.Tuple(_, _) =>
                throw new Exception(s"DynamicValue.Tuple is not supported in directDynamicMapping mode")
              case DynamicValue.LeftValue(_) =>
                throw new Exception(s"DynamicValue.LeftValue is not supported in directDynamicMapping mode")
              case DynamicValue.RightValue(_) =>
                throw new Exception(s"DynamicValue.RightValue is not supported in directDynamicMapping mode")
              case DynamicValue.DynamicAst(_) =>
                throw new Exception(s"DynamicValue.DynamicAst is not supported in directDynamicMapping mode")
              case DynamicValue.Error(message) =>
                throw new Exception(message)
            }
        }
      } else {
        schemaEncoder(DynamicValue.schema)
      }
    }

    private def transformEncoder[A, B](schema: Schema[A], g: B => Either[String, A]): BsonEncoder[B] =
      new BsonEncoder[B] {
        private lazy val innerEncoder = schemaEncoder(schema)

        override def encode(writer: BsonWriter, b: B, ctx: BsonEncoder.EncoderContext): Unit =
          g(b) match {
            case Left(_)  => ()
            case Right(a) => innerEncoder.encode(writer, a, ctx)
          }

        override def toBsonValue(b: B): BsonValue =
          g(b) match {
            case Left(_)  => BsonNull.VALUE
            case Right(a) => innerEncoder.toBsonValue(a)
          }
      }

    private def enumEncoder[Z](parentSchema: Schema.Enum[Z], cases: Chunk[Schema.Case[Z, _]]): BsonEncoder[Z] =
      // if all cases are CaseClass0, encode as a String
      if (cases.forall(_.schema.isInstanceOf[Schema.CaseClass0[_]])) {
        val caseMap: Map[Z, String] = cases
          .filterNot(_.annotations.exists(_.isInstanceOf[transientCase]))
          .map { case_ =>
            val manualBsonHint = case_.annotations.collectFirst { case bsonHint(name) => name }
            val manualCaseName = case_.annotations.collectFirst { case caseName(name) => name }
            case_.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct() ->
              manualBsonHint.orElse(manualCaseName).getOrElse(case_.id)
          }
          .toMap
        BsonEncoder.string.contramap(caseMap(_))
      } else {
        val bsonDiscriminator   = parentSchema.annotations.collectFirst { case d: bsonDiscriminator => d.name }
        val schemaDiscriminator = parentSchema.annotations.collectFirst { case d: discriminatorName => d.tag }
        val discriminator       = bsonDiscriminator.orElse(schemaDiscriminator)

        def getCaseName(case_ : Schema.Case[Z, _]) = {
          val manualBsonHint = case_.annotations.collectFirst { case bsonHint(name) => name }
          val manualCaseName = case_.annotations.collectFirst { case caseName(name) => name }
          manualBsonHint.orElse(manualCaseName).getOrElse(case_.id)
        }

        val noDiscriminators = parentSchema.annotations.exists {
          case noDiscriminator() => true
          case _                 => false
        }

        def nonTransientCase(value: Z) =
          try cases.collectFirst {
            case c @ Schema.Case(_, _, _, _, _, annotations) if annotations.collectFirst {
                  case _: transientCase => ()
                }.isEmpty && c.deconstructOption(value).isDefined =>
              c
          } catch {
            case ex: Exception => throw new RuntimeException(s"Failed to encode enum type $parentSchema", ex)
          }

        if (noDiscriminators) new BsonEncoder[Z] {
          override def encode(writer: BsonWriter, value: Z, ctx: BsonEncoder.EncoderContext): Unit =
            nonTransientCase(value) match {
              case Some(case_) =>
                val encoder = schemaEncoder(case_.schema).asInstanceOf[BsonEncoder[Z]]
                encoder.encode(writer, value, BsonEncoder.EncoderContext.default)
              case None =>
                writer.writeStartDocument()
                writer.writeEndDocument()
            }

          override def toBsonValue(value: Z): BsonValue =
            nonTransientCase(value) match {
              case Some(case_) =>
                val encoder = schemaEncoder(case_.schema).asInstanceOf[BsonEncoder[Z]]
                encoder.toBsonValue(value)
              case None => doc()
            }
        } else {
          discriminator match {
            case None =>
              new BsonEncoder[Z] {
                def encode(writer: BsonWriter, value: Z, ctx: BsonEncoder.EncoderContext): Unit = {
                  writer.writeStartDocument()
                  nonTransientCase(value) match {
                    case Some(case_) =>
                      val encoder = schemaEncoder(case_.schema).asInstanceOf[BsonEncoder[Z]]

                      val name = getCaseName(case_)
                      writer.writeName(name)
                      encoder.encode(writer, value, BsonEncoder.EncoderContext.default)

                    case None =>
                  }
                  writer.writeEndDocument()
                }

                def toBsonValue(value: Z): BsonValue =
                  nonTransientCase(value) match {
                    case Some(case_) =>
                      val encoder = schemaEncoder(case_.schema).asInstanceOf[BsonEncoder[Z]]

                      val name = getCaseName(case_)
                      doc(name -> encoder.toBsonValue(value))

                    case None => doc()
                  }
              }
            case Some(discriminator) =>
              new BsonEncoder[Z] {
                def encode(writer: BsonWriter, value: Z, ctx: BsonEncoder.EncoderContext): Unit = {
                  val nextCtx = ctx.copy(inlineNextObject = true)

                  writer.writeStartDocument()

                  nonTransientCase(value) match {
                    case Some(case_) =>
                      val encoder = schemaEncoder(case_.schema).asInstanceOf[BsonEncoder[Z]]

                      val name = getCaseName(case_)
                      writer.writeName(discriminator)
                      writer.writeString(name)
                      encoder.encode(writer, value, nextCtx)

                    case None =>
                  }

                  writer.writeEndDocument()
                }

                def toBsonValue(value: Z): BsonValue =
                  nonTransientCase(value) match {
                    case Some(case_) =>
                      val encoder  = schemaEncoder(case_.schema).asInstanceOf[BsonEncoder[Z]]
                      val caseBson = encoder.toBsonValue(value)

                      if (!caseBson.isDocument) throw new RuntimeException("Subtype is not encoded as an object")

                      val doc  = caseBson.asDocument()
                      val name = getCaseName(case_)
                      doc.put(discriminator, str(name))
                      doc
                    case None => doc()
                  }
              }
          }
        }
      }

    private def genericRecordEncoder[Z](structure: Seq[Schema.Field[Z, _]]): BsonEncoder[ListMap[String, _]] =
      new BsonEncoder[ListMap[String, _]] {
        override def encode(writer: BsonWriter, value: ListMap[String, _], ctx: BsonEncoder.EncoderContext): Unit = {
          if (!ctx.inlineNextObject) writer.writeStartDocument()

          structure.foreach {
            case Schema.Field(k, a, _, _, _, _) =>
              val enc = schemaEncoder(a.asInstanceOf[Schema[Any]])

              writer.writeName(k)
              enc.encode(writer, value(k), BsonEncoder.EncoderContext.default)
          }

          if (!ctx.inlineNextObject) writer.writeEndDocument()
        }

        override def toBsonValue(value: ListMap[String, _]): BsonValue =
          new BsonDocument(structure.map {
            case Schema.Field(k, a, _, _, _, _) =>
              val enc = schemaEncoder(a.asInstanceOf[Schema[Any]])
              element(k, enc.toBsonValue(value(k)))
          }.asJava)
      }
  }

  object BsonSchemaDecoder {

    import Codecs._
    import ProductDecoder._

    private def chunkDecoder[A: BsonDecoder]: BsonDecoder[Chunk[A]] = BsonDecoder.iterableFactory[A, Chunk]

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[codec] def schemaDecoder[A](schema: Schema[A]): BsonDecoder[A] = schema match {
      case Schema.Primitive(standardType, _)     => primitiveCodec(standardType).decoder
      case Schema.Optional(codec, _)             => BsonDecoder.option(schemaDecoder(codec))
      case Schema.Tuple2(left, right, _)         => tuple2Decoder(schemaDecoder(left), schemaDecoder(right))
      case Schema.Transform(codec, f, _, _, _)   => schemaDecoder(codec).mapOrFail(f)
      case Schema.Sequence(codec, f, _, _, _)    => chunkDecoder(schemaDecoder(codec)).map(f)
      case Schema.Map(ks, vs, _)                 => mapDecoder(ks, vs)
      case Schema.Set(s, _)                      => chunkDecoder(schemaDecoder(s)).map(entries => entries.toSet)
      case Schema.Fail(message, _)               => failDecoder(message)
      case Schema.GenericRecord(_, structure, _) => recordDecoder(structure.toChunk)
      case Schema.Either(left, right, _)         => eitherDecoder(schemaDecoder(left), schemaDecoder(right))
      case l @ Schema.Lazy(_)                    => schemaDecoder(l.schema)
      case s: Schema.Record[A]                   => caseClassDecoder(s)
      case e: Schema.Enum[A]                     => enumDecoder(e)
      case d @ Schema.Dynamic(_)                 => dynamicDecoder(d)
      case null                                  => throw new Exception(s"Missing a handler for decoding of schema $schema.")
    }
    //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

    private[codec] def mapDecoder[K, V](
      ks: Schema[K],
      vs: Schema[V]
    ): BsonDecoder[Map[K, V]] = {
      val valueDecoder = BsonSchemaDecoder.schemaDecoder(vs)
      bsonFieldDecoder(ks) match {
        case Some(bsonFieldDecoder) => BsonDecoder.mapFactory(bsonFieldDecoder, valueDecoder, Map)
        case None                   => chunkDecoder(tuple2Decoder(schemaDecoder(ks), schemaDecoder(vs))).map(_.toList.toMap)
      }
    }

    private[codec] def bsonFieldDecoder[A](schema: Schema[A]): Option[BsonFieldDecoder[A]] =
      schema match {
        case Schema.Primitive(StandardType.StringType, _) => Some(BsonFieldDecoder.string)
        case Schema.Primitive(StandardType.LongType, _)   => Some(BsonFieldDecoder.long)
        case Schema.Primitive(StandardType.IntType, _)    => Some(BsonFieldDecoder.int)
        case _                                            => None
      }

    private def dynamicDecoder(schema: Schema.Dynamic): BsonDecoder[DynamicValue] = {
      val directMapping = schema.annotations.exists {
        case directDynamicMapping() => true
        case _                      => false
      }

      if (directMapping) {
        BsonDecoder.bsonValueDecoder[BsonValue].map(bsonToDynamicValue)
      } else {
        schemaDecoder(DynamicValue.schema)
      }
    }

    private def bsonToDynamicValue(bsonValue: BsonValue): DynamicValue =
      bsonValue.getBsonType match {
        case BsonType.END_OF_DOCUMENT => DynamicValue.NoneValue
        case BsonType.DOUBLE          => DynamicValue.Primitive(bsonValue.asDouble().getValue, StandardType.DoubleType)
        case BsonType.STRING          => DynamicValue.Primitive(bsonValue.asString().getValue, StandardType.StringType)
        case BsonType.DOCUMENT =>
          val values = bsonValue
            .asDocument()
            .asScala
            .toSeq
            .map {
              case (k, v) => k -> bsonToDynamicValue(v)
            }

          DynamicValue.Record(TypeId.Structural, ListMap(values: _*))
        case BsonType.ARRAY =>
          DynamicValue.Sequence(bsonValue.asArray().getValues.asScala.map(bsonToDynamicValue).to(Chunk))
        case BsonType.BINARY =>
          DynamicValue.Primitive(Chunk.fromArray(bsonValue.asBinary().getData), StandardType.BinaryType)
        case BsonType.UNDEFINED => DynamicValue.NoneValue
        case BsonType.OBJECT_ID =>
          DynamicValue.Primitive(bsonValue.asObjectId().getValue.toHexString, StandardType.StringType)
        case BsonType.BOOLEAN => DynamicValue.Primitive(bsonValue.asBoolean().getValue, StandardType.BoolType)
        case BsonType.DATE_TIME =>
          DynamicValue.Primitive(Instant.ofEpochMilli(bsonValue.asDateTime().getValue), StandardType.InstantType)
        case BsonType.NULL                  => DynamicValue.NoneValue
        case BsonType.REGULAR_EXPRESSION    => DynamicValue.NoneValue
        case BsonType.DB_POINTER            => DynamicValue.NoneValue
        case BsonType.JAVASCRIPT            => DynamicValue.NoneValue
        case BsonType.SYMBOL                => DynamicValue.NoneValue
        case BsonType.JAVASCRIPT_WITH_SCOPE => DynamicValue.NoneValue
        case BsonType.INT32                 => DynamicValue.Primitive(bsonValue.asInt32().getValue, StandardType.IntType)
        case BsonType.TIMESTAMP =>
          DynamicValue.Primitive(Instant.ofEpochMilli(bsonValue.asTimestamp().getValue), StandardType.InstantType)
        case BsonType.INT64 => DynamicValue.Primitive(bsonValue.asInt64().getValue, StandardType.LongType)
        case BsonType.DECIMAL128 =>
          DynamicValue.Primitive(bsonValue.asDecimal128().getValue.bigDecimalValue(), StandardType.BigDecimalType)
        case BsonType.MIN_KEY => DynamicValue.NoneValue
        case BsonType.MAX_KEY => DynamicValue.NoneValue
      }

    private def enumDecoder[Z](parentSchema: Schema.Enum[Z]): BsonDecoder[Z] = {
      val cases = parentSchema.cases
      val caseNameAliases = cases.flatMap {
        case Schema.Case(name, _, _, _, _, annotations) =>
          annotations.flatMap {
            case a: caseNameAliases => a.aliases.toList.map(_ -> name)
            case cn: caseName       => List(cn.name -> name)
            case bh: bsonHint       => List(bh.name -> name)
            case _                  => Nil
          }
      }.toMap

      // if all cases are CaseClass0, decode as String
      if (cases.forall(_.schema.isInstanceOf[Schema.CaseClass0[_]])) {
        val caseMap: Map[String, Z] =
          cases.map(case_ => case_.id -> case_.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct()).toMap
        BsonDecoder.string.mapOrFail(
          s =>
            caseMap.get(caseNameAliases.getOrElse(s, s)) match {
              case Some(z) => Right(z)
              case None    => Left("unrecognized string")
            }
        )
      } else {

        val noDiscriminators = parentSchema.annotations.exists {
          case noDiscriminator() => true
          case _                 => false
        }

        if (noDiscriminators) {
          new BsonDecoder[Z] {
            override def decodeUnsafe(
              reader: BsonReader,
              trace: List[BsonTrace],
              ctx: BsonDecoder.BsonDecoderContext
            ): Z =
              unsafeCall(trace) {
                val mark              = reader.getMark
                val it                = cases.iterator
                var result: Option[Z] = None

                while (result.isEmpty && it.hasNext) {
                  val c = it.next()
                  try {
                    val decoded = schemaDecoder(c.schema).decodeUnsafe(reader, trace, ctx).asInstanceOf[Z]
                    result = Some(decoded)
                  } catch {
                    case _: Exception => mark.reset()
                  }
                }

                result match {
                  case Some(value) => value
                  case None        => throw BsonDecoder.Error(trace, "none of the subtypes could decode the data")
                }
              }

            override def fromBsonValueUnsafe(
              value: BsonValue,
              trace: List[BsonTrace],
              ctx: BsonDecoder.BsonDecoderContext
            ): Z = unsafeCall(trace) {
              val it                = cases.iterator
              var result: Option[Z] = None

              while (result.isEmpty && it.hasNext) {
                val c = it.next()
                try {
                  val decoded = schemaDecoder(c.schema).fromBsonValueUnsafe(value, trace, ctx).asInstanceOf[Z]
                  result = Some(decoded)
                } catch {
                  case _: Exception =>
                }
              }

              result match {
                case Some(value) => value
                case None        => throw BsonDecoder.Error(trace, "none of the subtypes could decode the data")
              }
            }
          }
        } else {
          val discriminators = parentSchema.annotations.collect {
            case d: bsonDiscriminator => d.name
            case d: discriminatorName => d.tag
          }.toSet

          val casesIndex = Map(cases.map(c => c.id -> c): _*)

          def getCase(name: String) = casesIndex.get(caseNameAliases.getOrElse(name, name))

          if (discriminators.isEmpty) {
            new BsonDecoder[Z] {
              def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): Z =
                unsafeCall(trace) {
                  reader.readStartDocument()

                  val name      = reader.readName()
                  val nextTrace = BsonTrace.Field(name) :: trace
                  val nextCtx   = BsonDecoder.BsonDecoderContext.default

                  val result =
                    getCase(name) match {
                      case None    => throw BsonDecoder.Error(nextTrace, s"Invalid disambiguator $name.")
                      case Some(c) => schemaDecoder(c.schema).decodeUnsafe(reader, nextTrace, nextCtx)
                    }

                  reader.readEndDocument()

                  result.asInstanceOf[Z]
                }

              def fromBsonValueUnsafe(
                value: BsonValue,
                trace: List[BsonTrace],
                ctx: BsonDecoder.BsonDecoderContext
              ): Z =
                assumeType(trace)(BsonType.DOCUMENT, value) { value =>
                  val fields = value.asDocument().asScala

                  if (fields.size != 1) throw BsonDecoder.Error(trace, "Expected exactly 1 disambiguator.")

                  val (name, element) = fields.head
                  val nextTrace       = BsonTrace.Field(name) :: trace
                  val nextCtx         = BsonDecoder.BsonDecoderContext.default

                  getCase(name) match {
                    case None => throw BsonDecoder.Error(nextTrace, s"Invalid disambiguator $name.")
                    case Some(c) =>
                      schemaDecoder(c.schema).fromBsonValueUnsafe(element, nextTrace, nextCtx).asInstanceOf[Z]
                  }
                }
            }
          } else {
            new BsonDecoder[Z] {
              def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): Z =
                unsafeCall(trace) {
                  val mark = reader.getMark

                  var hint: String          = null
                  var discriminator: String = null

                  reader.readStartDocument()

                  var bsonType = reader.readBsonType()
                  while (hint == null && bsonType != BsonType.END_OF_DOCUMENT) {
                    val name = reader.readName()
                    if (discriminators.contains(name) && bsonType == BsonType.STRING) {
                      hint = unsafeCall(BsonTrace.Field(name) :: trace)(reader.readString())
                      discriminator = name
                    } else reader.skipValue()

                    bsonType = reader.readBsonType()
                  }

                  if (hint == null)
                    throw BsonDecoder.Error(
                      trace,
                      s"Missing disambiguator. Expected any of: ${discriminators.mkString(", ")}."
                    )

                  getCase(hint) match {
                    case None =>
                      throw BsonDecoder.Error(BsonTrace.Field(discriminator) :: trace, s"Invalid disambiguator $hint.")
                    case Some(c) =>
                      mark.reset()
                      val nextCtx = ctx.copy(ignoreExtraField = Some(discriminator))
                      schemaDecoder(c.schema).decodeUnsafe(reader, trace, nextCtx).asInstanceOf[Z]
                  }
                }

              def fromBsonValueUnsafe(
                value: BsonValue,
                trace: List[BsonTrace],
                ctx: BsonDecoder.BsonDecoderContext
              ): Z =
                assumeType(trace)(BsonType.DOCUMENT, value) { value =>
                  val fields = value.asDocument().asScala

                  val discriminatorHint = discriminators.collectFirst {
                    case discriminator if fields.contains(discriminator) => discriminator -> fields(discriminator)
                  }
                  discriminatorHint match {
                    case None =>
                      throw BsonDecoder.Error(
                        trace,
                        s"Missing disambiguator. Expected any of: ${discriminators.mkString(", ")}."
                      )
                    case Some((discriminator, hint)) =>
                      assumeType(BsonTrace.Field(discriminator) :: trace)(BsonType.STRING, hint) { hint =>
                        getCase(hint.asString().getValue) match {
                          case None =>
                            throw BsonDecoder.Error(trace, s"Invalid disambiguator ${hint.asString().getValue}.")
                          case Some(c) =>
                            val nextCtx = ctx.copy(ignoreExtraField = Some(discriminator))
                            schemaDecoder(c.schema).fromBsonValueUnsafe(value, trace, nextCtx).asInstanceOf[Z]
                        }
                      }
                  }
                }
            }

          }
        }
      }
    }

    private def recordDecoder[Z](structure: Seq[Schema.Field[Z, _]]): BsonDecoder[ListMap[String, Any]] =
      new BsonDecoder[ListMap[String, Any]] {
        override def decodeUnsafe(
          reader: BsonReader,
          trace: List[BsonTrace],
          ctx: BsonDecoder.BsonDecoderContext
        ): ListMap[String, Any] = unsafeCall(trace) {
          val builder: ChunkBuilder[(String, Any)] = zio.ChunkBuilder.make[(String, Any)](structure.size)

          reader.readStartDocument()

          while (reader.readBsonType() != BsonType.END_OF_DOCUMENT) {
            val field = reader.readName()

            structure.find(_.name == field) match {
              case Some(Schema.Field(label, schema, _, _, _, _)) =>
                val nextTrace = BsonTrace.Field(field) :: trace
                val value =
                  schemaDecoder(schema).decodeUnsafe(reader, nextTrace, BsonDecoder.BsonDecoderContext.default)
                builder += (label -> value)
              case None => reader.skipValue()
            }
          }

          reader.readEndDocument()

          (ListMap.newBuilder[String, Any] ++= builder.result()).result()
        }

        override def fromBsonValueUnsafe(
          value: BsonValue,
          trace: List[BsonTrace],
          ctx: BsonDecoder.BsonDecoderContext
        ): ListMap[String, Any] = assumeType(trace)(BsonType.DOCUMENT, value) { value =>
          ListMap(
            value
              .asDocument()
              .asScala
              .toVector
              .flatMap {
                case (field, v) =>
                  structure.find(_.name == field) match {
                    case Some(Schema.Field(label, schema, _, _, _, _)) =>
                      val nextTrace = BsonTrace.Field(field) :: trace
                      val value = schemaDecoder(schema)
                        .fromBsonValueUnsafe(v, nextTrace, BsonDecoder.BsonDecoderContext.default)
                      Some((label, value))
                    case None => None
                  }
              }: _*
          )
        }
      }

  }
  private[codec] object ProductEncoder {

    private[codec] def caseClassEncoder[Z](
      parentSchema: Schema.Record[Z]
    ): BsonEncoder[Z] = new BsonEncoder[Z] {
      private val keepNulls = false // TODO: configuration

      private val fields = parentSchema.fields

      private val nonTransientFields = fields.filter {
        case Schema.Field(_, _, annotations, _, _, _) if annotations.collectFirst {
              case _: transientField => ()
              case _: bsonExclude    => ()
            }.isDefined =>
          false
        case _ => true
      }.toArray

      private val names: Array[String] =
        nonTransientFields.map { p =>
          p.annotations.collectFirst { case bsonField(name) => name }.getOrElse(p.name)
        }

      private lazy val tcs: Array[BsonEncoder[Any]] =
        nonTransientFields.map(s => BsonSchemaEncoder.schemaEncoder(s.schema).asInstanceOf[BsonEncoder[Any]])

      private val len = nonTransientFields.length

      def encode(writer: BsonWriter, value: Z, ctx: BsonEncoder.EncoderContext): Unit = {
        val nextCtx = ctx.copy(inlineNextObject = false)

        if (!ctx.inlineNextObject) writer.writeStartDocument()

        var i = 0

        while (i < len) {
          val tc         = tcs(i)
          val fieldValue = nonTransientFields(i).get(value)

          if (keepNulls || !tc.isAbsent(fieldValue)) {
            writer.writeName(names(i))
            tc.encode(writer, fieldValue, nextCtx)
          }

          i += 1
        }

        if (!ctx.inlineNextObject) writer.writeEndDocument()
      }

      def toBsonValue(value: Z): BsonValue = {
        val elements = nonTransientFields.indices.view.flatMap { idx =>
          val fieldValue = nonTransientFields(idx).get(value)
          val tc         = tcs(idx)

          if (keepNulls || !tc.isAbsent(fieldValue)) Some(element(names(idx), tc.toBsonValue(fieldValue)))
          else None
        }.to(Chunk)

        new BsonDocument(elements.asJava)
      }
    }
  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private[codec] object ProductDecoder {

    import BsonSchemaDecoder.schemaDecoder

    private[codec] def caseClassDecoder[Z](caseClassSchema: Schema.Record[Z]): BsonDecoder[Z] = {
      val fields   = caseClassSchema.fields
      val len: Int = fields.length
      Array.ofDim[Any](len)
      val fieldNames = fields.map { f =>
        f.annotations.collectFirst { case bsonField(n) => n }.getOrElse(f.name.asInstanceOf[String])
      }.toArray
      val spans: Array[BsonTrace]   = fieldNames.map(f => BsonTrace.Field(f))
      val schemas: Array[Schema[_]] = fields.map(_.schema).toArray
      val fieldAliases = fields.flatMap {
        case Schema.Field(name, _, annotations, _, _, _) =>
          val aliases = annotations.collectFirst { case a: fieldNameAliases => a.aliases }.getOrElse(Nil)
          aliases.map(_ -> fieldNames.indexOf(name)) :+ (name -> fieldNames.indexOf(name))
      }.toMap
      val indexes = HashMap((fieldAliases ++ fieldNames.zipWithIndex).toSeq: _*)
      val noExtra =
        caseClassSchema.annotations.collectFirst {
          case _: rejectExtraFields => ()
          case _: bsonNoExtraFields => ()
        }.isDefined
      lazy val tcs: Array[BsonDecoder[Any]] = schemas.map(s => schemaDecoder(s).asInstanceOf[BsonDecoder[Any]])
      lazy val defaults: Array[Option[Any]] = fields.map(_.annotations.collectFirst { case a: fieldDefaultValue[_] => a.value }).toArray

      new BsonDecoder[Z] {
        def decodeUnsafe(reader: BsonReader, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): Z = unsafeCall(trace) {
          reader.readStartDocument()

          val nextCtx        = BsonDecoder.BsonDecoderContext.default
          val ps: Array[Any] = Array.ofDim(len)

          while (reader.readBsonType() != BsonType.END_OF_DOCUMENT) {
            val name = reader.readName()
            val idx  = indexes.getOrElse(name, -1)

            if (idx >= 0) {
              val nextTrace = spans(idx) :: trace
              val tc        = tcs(idx)
              if (ps(idx) != null) throw BsonDecoder.Error(nextTrace, "duplicate")
              ps(idx) = defaults(idx) match {
                case Some(default) =>
                  val opt = BsonDecoder.option(tc).decodeUnsafe(reader, nextTrace, nextCtx)
                  opt.getOrElse(default)
                case None =>
                  tc.decodeUnsafe(reader, nextTrace, nextCtx)
              }
            } else if (noExtra && !ctx.ignoreExtraField.contains(name)) {
              throw BsonDecoder.Error(BsonTrace.Field(name) :: trace, "Invalid extra field.")
            } else reader.skipValue()
          }

          var i = 0
          while (i < len) {
            if (ps(i) == null) {
              ps(i) = defaults(i) match {
                case Some(default) => default
                case None          => tcs(i).decodeMissingUnsafe(spans(i) :: trace)
              }
            }
            i += 1
          }

          reader.readEndDocument()

          Unsafe.unsafe { implicit u =>
            caseClassSchema.construct(Chunk.fromArray(ps)) match {
              case Left(err)    => throw BsonDecoder.Error(trace, s"Failed to construct case class: $err")
              case Right(value) => value
            }
          }
        }

        def fromBsonValueUnsafe(value: BsonValue, trace: List[BsonTrace], ctx: BsonDecoder.BsonDecoderContext): Z =
          assumeType(trace)(BsonType.DOCUMENT, value) { value =>
            val nextCtx        = BsonDecoder.BsonDecoderContext.default
            val ps: Array[Any] = Array.ofDim(len)

            value.asDocument().asScala.foreachEntry { (name, value) =>
              val idx = indexes.getOrElse(name, -1)

              if (idx >= 0) {
                val nextTrace = spans(idx) :: trace
                val tc        = tcs(idx)
                if (ps(idx) != null) throw BsonDecoder.Error(nextTrace, "duplicate")
                ps(idx) = defaults(idx) match {
                  case Some(default) =>
                    val opt = BsonDecoder.option(tc).fromBsonValueUnsafe(value, nextTrace, nextCtx)
                    opt.getOrElse(default)
                  case None =>
                    tc.fromBsonValueUnsafe(value, nextTrace, nextCtx)
                }
              } else if (noExtra && !ctx.ignoreExtraField.contains(name))
                throw BsonDecoder.Error(BsonTrace.Field(name) :: trace, "Invalid extra field.")
            }

            var i = 0
            while (i < len) {
              if (ps(i) == null) {
                ps(i) = defaults(i) match {
                  case Some(default) => default
                  case None          => tcs(i).decodeMissingUnsafe(spans(i) :: trace)
                }
              }
              i += 1
            }

            Unsafe.unsafe { implicit u =>
              caseClassSchema.construct(Chunk.fromArray(ps)) match {
                case Left(err)    => throw BsonDecoder.Error(trace, s"Failed to construct case class: $err")
                case Right(value) => value
              }
            }
          }
      }
    }

  }
}
