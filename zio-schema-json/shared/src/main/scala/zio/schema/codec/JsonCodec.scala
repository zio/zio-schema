package zio.schema.codec

import java.nio.CharBuffer
import java.nio.charset.StandardCharsets
import java.util
import java.util.concurrent.ConcurrentHashMap

import scala.annotation.switch
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.util.control.NonFatal

import zio.json.JsonCodec._
import zio.json.JsonDecoder.{ JsonError, UnsafeJson }
import zio.json.ast.Json
import zio.json.internal.{ Lexer, RecordingReader, RetractReader, StringMatrix, WithRecordingReader, Write }
import zio.json.{
  JsonCodec => ZJsonCodec,
  JsonDecoder => ZJsonDecoder,
  JsonEncoder => ZJsonEncoder,
  JsonFieldDecoder,
  JsonFieldEncoder
}
import zio.prelude.NonEmptyMap
import zio.schema.Schema.GenericRecord
import zio.schema._
import zio.schema.annotation.{ rejectExtraFields, _ }
import zio.schema.codec.DecodeError.ReadError
import zio.schema.codec.JsonCodec.JsonDecoder.schemaDecoder
import zio.stream.{ ZChannel, ZPipeline }
import zio.{ Cause, Chunk, ChunkBuilder, ZIO, ZNothing }

object JsonCodec {

  final case class Config(
    ignoreEmptyCollections: Boolean,
    treatStreamsAsArrays: Boolean = false,
    explicitNulls: Boolean = false
  )

  object Config {
    val default: Config = Config(ignoreEmptyCollections = false)
  }

  type DiscriminatorTuple = Option[(String, String)]

  implicit def zioJsonBinaryCodec[A](implicit jsonCodec: ZJsonCodec[A]): BinaryCodec[A] =
    new BinaryCodec[A] {
      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] =
        jsonCodec
          .decodeJson(
            new String(whole.toArray, JsonEncoder.CHARSET)
          )
          .left
          .map(failure => DecodeError.ReadError(Cause.empty, failure))

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
        ZPipeline.fromChannel(
          ZPipeline.utfDecode.channel.mapError(cce => ReadError(Cause.fail(cce), cce.getMessage))
        ) >>> splitOnJsonBoundary >>>
          ZPipeline.mapZIO { (s: String) =>
            ZIO
              .fromEither(jsonCodec.decodeJson(s))
              .mapError(failure => DecodeError.ReadError(Cause.empty, failure))
          }

      override def encode(value: A): Chunk[Byte] =
        JsonEncoder.charSequenceToByteChunk(jsonCodec.encodeJson(value, None))

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
        ZPipeline.mapChunks[A, Chunk[Byte]](_.map(encode)).intersperse(Chunk.single('\n'.toByte)).flattenChunks
    }

  implicit def schemaBasedBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A] =
    schemaBasedBinaryCodec[A](JsonCodec.Config.default)

  private object JsonSplitter {
    val validNumChars: Set[Char] = Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'E', 'e', '-', '+', '.')
    val ContextJson              = 'j'
    val ContextString            = 's'
    val ContextBoolean           = 'b'
    val ContextNull              = 'u'
    val ContextNullAfterFirstL   = 'x'
    val ContextNumber            = 'n'
    val ContextEscape            = 'e'
    val ContextDone              = 'd'

    def jsonSplitter(wrappedInArray: Boolean): ZPipeline[Any, Nothing, String, String] =
      ZPipeline.suspend {
        val stringBuilder = new StringBuilder
        var depth         = if (wrappedInArray) -1 else 0
        var context       = ContextJson

        def fetchChunk(chunk: Chunk[String]): Chunk[String] = {
          val chunkBuilder = ChunkBuilder.make[String]()
          for {
            string <- chunk
            c      <- string
          } {
            var valueEnded = false
            context match {
              case ContextEscape =>
                context = 's'
              case ContextString =>
                c match {
                  case '\\' => context = ContextEscape
                  case '"' =>
                    context = ContextJson
                    valueEnded = true
                  case _ =>
                }
              case ContextBoolean =>
                if (c == 'e') {
                  context = ContextJson
                  valueEnded = true
                }
              case ContextNull =>
                if (c == 'l') {
                  context = ContextNullAfterFirstL
                }
              case ContextNullAfterFirstL =>
                if (c == 'l') {
                  context = ContextJson
                  valueEnded = true
                }
              case ContextNumber =>
                c match {
                  case '}' | ']' =>
                    depth -= 1
                    context = if (depth < 0) ContextDone else ContextJson
                    valueEnded = true
                  case _ if !validNumChars(c) =>
                    context = ContextJson
                    valueEnded = true
                  case _ =>
                }
              case ContextDone => // no more values, ignore everything
              case _ =>
                c match {
                  case '{' | '[' =>
                    depth += 1
                  case '}' | ']' =>
                    depth -= 1
                    valueEnded = true
                    if (depth == -1) context = ContextDone
                  case '"' =>
                    context = ContextString
                  case 't' | 'f' =>
                    context = ContextBoolean
                  case 'n' =>
                    context = ContextNull
                  case x if validNumChars(x) =>
                    context = ContextNumber
                  case _ =>
                }
            }
            if (context != ContextDone && (depth > 0 || context != ContextJson || valueEnded))
              stringBuilder.append(c)

            if (valueEnded && depth == 0) {
              val str = stringBuilder.result()
              if (!str.forall(_.isWhitespace)) {
                chunkBuilder += str
              }
              stringBuilder.clear()
            }
          }
          chunkBuilder.result()
        }

        lazy val loop: ZChannel[Any, ZNothing, Chunk[String], Any, Nothing, Chunk[String], Any] =
          ZChannel.readWithCause(
            in => {
              val out = fetchChunk(in)
              if (out.isEmpty) loop else ZChannel.write(out) *> loop
            },
            err =>
              if (stringBuilder.isEmpty) ZChannel.refailCause(err)
              else ZChannel.write(Chunk.single(stringBuilder.result())) *> ZChannel.refailCause(err),
            done =>
              if (stringBuilder.isEmpty) ZChannel.succeed(done)
              else ZChannel.write(Chunk.single(stringBuilder.result())) *> ZChannel.succeed(done)
          )

        ZPipeline.fromChannel(loop)
      }
  }

  val splitOnJsonBoundary: ZPipeline[Any, Nothing, String, String]    = JsonSplitter.jsonSplitter(wrappedInArray = false)
  val splitJsonArrayElements: ZPipeline[Any, Nothing, String, String] = JsonSplitter.jsonSplitter(wrappedInArray = true)

  def schemaBasedBinaryCodec[A](cfg: Config)(implicit schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {
      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] =
        JsonDecoder.decode(
          schema,
          new String(whole.toArray, JsonEncoder.CHARSET)
        )

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
        ZPipeline.utfDecode.mapError(cce => ReadError(Cause.fail(cce), cce.getMessage)) >>>
          (if (cfg.treatStreamsAsArrays) splitJsonArrayElements else splitOnJsonBoundary) >>>
          ZPipeline.mapZIO { (s: String) =>
            ZIO.fromEither(JsonDecoder.decode(schema, s))
          }

      override def encode(value: A): Chunk[Byte] =
        JsonEncoder.encode(schema, value, cfg)

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
        if (cfg.treatStreamsAsArrays) {
          val interspersed: ZPipeline[Any, Nothing, A, Byte] = ZPipeline
            .mapChunks[A, Chunk[Byte]](_.map(encode))
            .intersperse(Chunk.single(','.toByte))
            .flattenChunks
          val prepended: ZPipeline[Any, Nothing, A, Byte] =
            interspersed >>> ZPipeline.prepend(Chunk.single('['.toByte))
          prepended >>> ZPipeline.append(Chunk.single(']'.toByte))
        } else {
          ZPipeline.mapChunks[A, Chunk[Byte]](_.map(encode)).intersperse(Chunk.single('\n'.toByte)).flattenChunks
        }
    }

  def jsonEncoder[A](schema: Schema[A]): ZJsonEncoder[A] =
    JsonEncoder.schemaEncoder(schema, JsonCodec.Config.default)

  def jsonEncoder[A](cfg: JsonCodec.Config)(schema: Schema[A]): ZJsonEncoder[A] =
    JsonEncoder.schemaEncoder(schema, cfg)

  def jsonDecoder[A](schema: Schema[A]): ZJsonDecoder[A] =
    JsonDecoder.schemaDecoder(schema)

  def jsonCodec[A](schema: Schema[A]): ZJsonCodec[A] =
    ZJsonCodec(jsonEncoder(schema), jsonDecoder(schema))

  def jsonCodec[A](cfg: JsonCodec.Config)(schema: Schema[A]): ZJsonCodec[A] =
    ZJsonCodec(jsonEncoder(cfg)(schema), jsonDecoder(schema))

  object Codecs {
    protected[codec] val unitEncoder: ZJsonEncoder[Unit] =
      (_: Unit, _: Option[Int], out: Write) => out.write("{}")

    private[codec] val unitDecoder: ZJsonDecoder[Unit] =
      (trace: List[ZJsonDecoder.JsonError], in: RetractReader) => {
        Lexer.char(trace, in, '{')
        Lexer.char(trace, in, '}')
        ()
      }

    protected[codec] val unitCodec: ZJsonCodec[Unit] = ZJsonCodec(unitEncoder, unitDecoder)

    protected[codec] def failDecoder[A](message: String): ZJsonDecoder[A] =
      (trace: List[ZJsonDecoder.JsonError], _: RetractReader) => throw UnsafeJson(JsonError.Message(message) :: trace)

    private[codec] def primitiveCodec[A](standardType: StandardType[A]): ZJsonCodec[A] =
      standardType match {
        case StandardType.UnitType           => unitCodec
        case StandardType.StringType         => ZJsonCodec.string
        case StandardType.BoolType           => ZJsonCodec.boolean
        case StandardType.ByteType           => ZJsonCodec.byte
        case StandardType.ShortType          => ZJsonCodec.short
        case StandardType.IntType            => ZJsonCodec.int
        case StandardType.LongType           => ZJsonCodec.long
        case StandardType.FloatType          => ZJsonCodec.float
        case StandardType.DoubleType         => ZJsonCodec.double
        case StandardType.BinaryType         => ZJsonCodec.chunk(ZJsonCodec.byte.encoder, ZJsonCodec.byte.decoder)
        case StandardType.CharType           => ZJsonCodec.char
        case StandardType.BigIntegerType     => ZJsonCodec.bigInteger
        case StandardType.BigDecimalType     => ZJsonCodec.bigDecimal
        case StandardType.UUIDType           => ZJsonCodec.uuid
        case StandardType.DayOfWeekType      => ZJsonCodec.dayOfWeek // ZJsonCodec[java.time.DayOfWeek]
        case StandardType.DurationType       => ZJsonCodec.duration //ZJsonCodec[java.time.Duration]
        case StandardType.InstantType        => ZJsonCodec.instant //ZJsonCodec[java.time.Instant]
        case StandardType.LocalDateType      => ZJsonCodec.localDate //ZJsonCodec[java.time.LocalDate]
        case StandardType.LocalDateTimeType  => ZJsonCodec.localDateTime //ZJsonCodec[java.time.LocalDateTime]
        case StandardType.LocalTimeType      => ZJsonCodec.localTime //ZJsonCodec[java.time.LocalTime]
        case StandardType.MonthType          => ZJsonCodec.month //ZJsonCodec[java.time.Month]
        case StandardType.MonthDayType       => ZJsonCodec.monthDay //ZJsonCodec[java.time.MonthDay]
        case StandardType.OffsetDateTimeType => ZJsonCodec.offsetDateTime //ZJsonCodec[java.time.OffsetDateTime]
        case StandardType.OffsetTimeType     => ZJsonCodec.offsetTime //ZJsonCodec[java.time.OffsetTime]
        case StandardType.PeriodType         => ZJsonCodec.period //ZJsonCodec[java.time.Period]
        case StandardType.YearType           => ZJsonCodec.year //ZJsonCodec[java.time.Year]
        case StandardType.YearMonthType      => ZJsonCodec.yearMonth //ZJsonCodec[java.time.YearMonth]
        case StandardType.ZonedDateTimeType  => ZJsonCodec.zonedDateTime //ZJsonCodec[java.time.ZonedDateTime]
        case StandardType.ZoneIdType         => ZJsonCodec.zoneId //ZJsonCodec[java.time.ZoneId]
        case StandardType.ZoneOffsetType     => ZJsonCodec.zoneOffset //ZJsonCodec[java.time.ZoneOffset]
        case StandardType.CurrencyType       => ZJsonCodec.currency //ZJsonCodec[java.util.Currency]
      }
  }

  object JsonEncoder {

    import Codecs._
    import ProductEncoder._
    import ZJsonEncoder.{ bump, pad }

    private case class EncoderKey[A](schema: Schema[A], cfg: Config, discriminatorTuple: DiscriminatorTuple) {
      override val hashCode: Int = System.identityHashCode(schema) ^ cfg.hashCode ^ discriminatorTuple.hashCode

      override def equals(obj: Any): Boolean = obj match {
        case ek: EncoderKey[_] => (ek.schema eq schema) && ek.cfg == cfg && ek.discriminatorTuple == discriminatorTuple
        case _                 => false
      }
    }

    private[codec] val CHARSET = StandardCharsets.UTF_8
    private[this] val encoders = new ConcurrentHashMap[EncoderKey[_], ZJsonEncoder[_]]()

    final def encode[A](schema: Schema[A], value: A, cfg: Config): Chunk[Byte] =
      charSequenceToByteChunk(schemaEncoder(schema, cfg).encodeJson(value, None))

    private[codec] def charSequenceToByteChunk(chars: CharSequence): Chunk[Byte] = {
      val bytes = CHARSET.newEncoder().encode(CharBuffer.wrap(chars))
      Chunk.fromByteBuffer(bytes)
    }

    private[codec] def schemaEncoder[A](
      schema: Schema[A],
      cfg: Config,
      discriminatorTuple: DiscriminatorTuple = None
    ): ZJsonEncoder[A] = {
      val key                      = EncoderKey(schema, cfg, discriminatorTuple)
      var encoder: ZJsonEncoder[A] = encoders.get(key).asInstanceOf[ZJsonEncoder[A]]
      if (encoder eq null) {
        encoder = schemaEncoderSlow(schema, cfg, discriminatorTuple)
        encoders.put(key, encoder)
      }
      encoder
    }

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[this] def schemaEncoderSlow[A](schema: Schema[A], cfg: Config, discriminatorTuple: DiscriminatorTuple): ZJsonEncoder[A] =
      schema match {
        case Schema.Primitive(standardType, _)                     => primitiveCodec(standardType).encoder
        case Schema.Sequence(schema, _, g, _, _)                   => ZJsonEncoder.chunk(schemaEncoder(schema, cfg, discriminatorTuple)).contramap(g)
        case Schema.NonEmptySequence(schema, _, g, _, _)           => ZJsonEncoder.chunk(schemaEncoder(schema, cfg, discriminatorTuple)).contramap(g)
        case Schema.Map(ks, vs, _)                                 => mapEncoder(ks, vs, discriminatorTuple, cfg)
        case Schema.NonEmptyMap(ks: Schema[kt], vs: Schema[vt], _) => mapEncoder(ks, vs, discriminatorTuple, cfg).contramap[NonEmptyMap[kt, vt]](_.toMap.asInstanceOf[Map[kt, vt]]).asInstanceOf[ZJsonEncoder[A]]
        case Schema.Set(s, _) =>
          ZJsonEncoder.chunk(schemaEncoder(s, cfg, discriminatorTuple)).contramap(m => Chunk.fromIterable(m))
        case Schema.Transform(c, _, g, a, _)    => transformEncoder(a.foldLeft(c)((s, a) => s.annotate(a)), g, cfg, discriminatorTuple)
        case Schema.Tuple2(l, r, _)             => ZJsonEncoder.tuple2(schemaEncoder(l, cfg, discriminatorTuple), schemaEncoder(r, cfg, discriminatorTuple))
        case Schema.Optional(schema, _)         => ZJsonEncoder.option(schemaEncoder(schema, cfg, discriminatorTuple))
        case Schema.Fail(_, _)                  => unitEncoder.contramap(_ => ())
        case s: Schema.GenericRecord            => recordEncoder(s, cfg, discriminatorTuple)
        case Schema.Either(left, right, _)      => ZJsonEncoder.either(schemaEncoder(left, cfg, discriminatorTuple), schemaEncoder(right, cfg, discriminatorTuple))
        case Schema.Fallback(left, right, _, _) => fallbackEncoder(schemaEncoder(left, cfg, discriminatorTuple), schemaEncoder(right, cfg, discriminatorTuple))
        case l @ Schema.Lazy(_)                 => ZJsonEncoder.suspend(schemaEncoder(l.schema, cfg, discriminatorTuple))
        case s: Schema.Record[A]                => caseClassEncoder(s, cfg, discriminatorTuple)
        case s: Schema.Enum[A]                  => enumEncoder(s, cfg)
        case d @ Schema.Dynamic(_)              => dynamicEncoder(d, cfg)
        case null =>
          throw new Exception(s"A captured schema is null, most likely due to wrong field initialization order")
      }
    //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

    private[codec] def transformFieldEncoder[A, B](
      schema: Schema[A],
      g: B => Either[String, A]
    ): Option[JsonFieldEncoder[B]] =
      jsonFieldEncoder(schema).map { fieldEncoder =>
        new JsonFieldEncoder[B] {
          override def unsafeEncodeField(b: B): String =
            g(b) match {
              case Left(_)  => throw new RuntimeException(s"Failed to encode field $b")
              case Right(a) => fieldEncoder.unsafeEncodeField(a)
            }
        }
      }

    private[codec] def jsonFieldEncoder[A](schema: Schema[A]): Option[JsonFieldEncoder[A]] =
      schema match {
        case Schema.Primitive(StandardType.StringType, _) => Option(JsonFieldEncoder.string)
        case Schema.Primitive(StandardType.LongType, _)   => Option(JsonFieldEncoder.long)
        case Schema.Primitive(StandardType.IntType, _)    => Option(JsonFieldEncoder.int)
        case Schema.Transform(c, _, g, a, _)              => transformFieldEncoder(a.foldLeft(c)((s, a) => s.annotate(a)), g)
        case Schema.Lazy(inner)                           => jsonFieldEncoder(inner())
        case _                                            => None
      }

    private[codec] def mapEncoder[K, V](
      ks: Schema[K],
      vs: Schema[V],
      discriminatorTuple: DiscriminatorTuple,
      cfg: Config
    ): ZJsonEncoder[Map[K, V]] = {
      val valueEncoder = JsonEncoder.schemaEncoder(vs, cfg)
      jsonFieldEncoder(ks) match {
        case Some(jsonFieldEncoder) =>
          ZJsonEncoder.keyValueIterable(jsonFieldEncoder, valueEncoder).contramap(a => Chunk.fromIterable(a).toMap)
        case None =>
          ZJsonEncoder
            .chunk(schemaEncoder(ks, cfg, discriminatorTuple).zip(schemaEncoder(vs, cfg, discriminatorTuple)))
            .contramap(m => Chunk.fromIterable(m))
      }
    }

    private[codec] def dynamicEncoder(schema: Schema.Dynamic, cfg: JsonCodec.Config): ZJsonEncoder[DynamicValue] = {
      val directMapping = schema.annotations.exists {
        case directDynamicMapping() => true
        case _                      => false
      }

      if (directMapping) {
        new ZJsonEncoder[DynamicValue] { directEncoder =>
          override def unsafeEncode(value: DynamicValue, indent: Option[Int], out: Write): Unit =
            value match {
              case DynamicValue.Record(_, values) =>
                if (values.isEmpty) {
                  out.write("{}")
                } else {
                  out.write('{')
                  val indent_ = bump(indent)
                  pad(indent_, out)
                  var first = true
                  values.foreach {
                    case (key, value) =>
                      if (first) first = false
                      else {
                        out.write(',')
                        if (indent.isDefined) pad(indent_, out)
                      }
                      string.encoder.unsafeEncode(key, indent_, out)
                      if (indent.isEmpty) out.write(':')
                      else out.write(" : ")
                      directEncoder.unsafeEncode(value, indent_, out)
                  }
                  pad(indent, out)
                  out.write('}')
                }
              case DynamicValue.Enumeration(_, _) =>
                throw new Exception(s"DynamicValue.Enumeration is not supported in directDynamicMapping mode")
              case DynamicValue.Sequence(values) =>
                ZJsonEncoder.chunk(directEncoder).unsafeEncode(values, indent, out)
              case DynamicValue.Dictionary(_) =>
                throw new Exception(s"DynamicValue.Dictionary is not supported in directDynamicMapping mode")
              case DynamicValue.SetValue(values) =>
                ZJsonEncoder.set(directEncoder).unsafeEncode(values, indent, out)
              case DynamicValue.Primitive(value, standardType) =>
                primitiveCodec(standardType).encoder.unsafeEncode(value, indent, out)
              case DynamicValue.Singleton(_) =>
                out.write("{}")
              case DynamicValue.SomeValue(value) =>
                directEncoder.unsafeEncode(value, indent, out)
              case DynamicValue.NoneValue =>
                out.write("null")
              case DynamicValue.Tuple(_, _) =>
                throw new Exception(s"DynamicValue.Tuple is not supported in directDynamicMapping mode")
              case DynamicValue.LeftValue(_) =>
                throw new Exception(s"DynamicValue.LeftValue is not supported in directDynamicMapping mode")
              case DynamicValue.RightValue(_) =>
                throw new Exception(s"DynamicValue.RightValue is not supported in directDynamicMapping mode")
              case DynamicValue.BothValue(left, right) =>
                out.write('[')
                val indent_ = bump(indent)
                pad(indent_, out)
                directEncoder.unsafeEncode(left, indent_, out)
                out.write(',')
                if (indent.isDefined) ZJsonEncoder.pad(indent_, out)
                directEncoder.unsafeEncode(right, indent_, out)
                pad(indent, out)
                out.write(']')
              case DynamicValue.DynamicAst(_) =>
                throw new Exception(s"DynamicValue.DynamicAst is not supported in directDynamicMapping mode")
              case DynamicValue.Error(message) =>
                throw new Exception(message)
            }
        }
      } else {
        schemaEncoder(DynamicValue.schema, cfg)
      }
    }

    private def transformEncoder[A, B](
      schema: Schema[A],
      g: B => Either[String, A],
      cfg: Config,
      discriminatorTuple: DiscriminatorTuple
    ): ZJsonEncoder[B] =
      new ZJsonEncoder[B] {
        private lazy val innerEncoder = schemaEncoder(schema, cfg, discriminatorTuple)

        override def unsafeEncode(b: B, indent: Option[Int], out: Write): Unit =
          g(b) match {
            case Left(_)  => ()
            case Right(a) => innerEncoder.unsafeEncode(a, indent, out)
          }

        override def isNothing(b: B): Boolean =
          g(b) match {
            case Left(_)  => false
            case Right(a) => innerEncoder.isNothing(a)
          }
      }

    private def enumEncoder[Z](schema: Schema.Enum[Z], cfg: Config): ZJsonEncoder[Z] =
      // if all cases are CaseClass0, encode as a String
      if (schema.annotations.exists(_.isInstanceOf[simpleEnum])) {
        val caseMap: Map[Z, String] =
          schema.nonTransientCases
            .map(
              case_ =>
                case_.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct() ->
                  case_.caseName
            )
            .toMap
        ZJsonEncoder.string.contramap(caseMap(_))
      } else { (value: Z, indent: Option[Int], out: Write) =>
        {
          schema.nonTransientCases.find(_.isCase(value)) match {
            case Some(case_) =>
              val caseName         = case_.caseName
              val noDiscriminators = schema.noDiscriminator
              val discriminatorTuple =
                if (noDiscriminators) None
                else schema.annotations.collectFirst { case d: discriminatorName => (d.tag, caseName) }
              val doJsonObjectWrapping = discriminatorTuple.isEmpty && !noDiscriminators
              var indent_              = indent
              if (doJsonObjectWrapping) {
                out.write('{')
                indent_ = bump(indent)
                pad(indent_, out)
                string.encoder.unsafeEncode(caseName, indent_, out)
                if (indent.isEmpty) out.write(':')
                else out.write(" : ")
              }
              schemaEncoder(case_.schema.asInstanceOf[Schema[Any]], cfg, discriminatorTuple)
                .unsafeEncode({
                  try case_.deconstruct(value)
                  catch {
                    case ex if NonFatal(ex) => throw new RuntimeException(s"Failed to encode enum type $schema", ex)
                  }
                }, indent_, out)
              if (doJsonObjectWrapping) {
                pad(indent, out)
                out.write('}')
              }
            case _ =>
              out.write("{}") // for transient cases
          }
        }
      }

    private def fallbackEncoder[A, B](left: ZJsonEncoder[A], right: ZJsonEncoder[B]): ZJsonEncoder[Fallback[A, B]] =
      new ZJsonEncoder[Fallback[A, B]] {

        def unsafeEncode(f: Fallback[A, B], indent: Option[Int], out: Write): Unit =
          f match {
            case Fallback.Left(a)  => left.unsafeEncode(a, indent, out)
            case Fallback.Right(b) => right.unsafeEncode(b, indent, out)
            case Fallback.Both(a, b) =>
              out.write('[')
              if (indent.isDefined) pad(bump(indent), out)
              left.unsafeEncode(a, indent, out)
              out.write(',')
              if (indent.isDefined) pad(bump(indent), out)
              right.unsafeEncode(b, indent, out)
              if (indent.isDefined) pad(indent, out)
              out.write(']')
          }
      }

    private def recordEncoder(
      schema: Schema.GenericRecord,
      cfg: Config,
      discriminatorTuple: DiscriminatorTuple
    ): ZJsonEncoder[ListMap[String, _]] = {
      val nonTransientFields = schema.nonTransientFields.toArray
      val encoders           = nonTransientFields.map(field => schemaEncoder(field.schema.asInstanceOf[Schema[Any]], cfg))
      if (nonTransientFields.isEmpty) { (_: ListMap[String, _], _: Option[Int], out: Write) =>
        out.write("{}")
      } else { (value: ListMap[String, _], indent: Option[Int], out: Write) =>
        {
          out.write('{')
          val doPrettyPrint = indent ne None
          var indent_       = indent
          if (doPrettyPrint) {
            indent_ = bump(indent)
            pad(indent_, out)
          }
          val strEnc = string.encoder
          var first  = true
          if (discriminatorTuple ne None) {
            val tuple = discriminatorTuple.get
            first = false
            strEnc.unsafeEncode(tuple._1, indent_, out)
            if (doPrettyPrint) out.write(" : ")
            else out.write(':')
            strEnc.unsafeEncode(tuple._2, indent_, out)
          }
          var idx = 0
          while (idx < nonTransientFields.length) {
            val field      = nonTransientFields(idx)
            val fieldName  = field.fieldName
            val fieldValue = value(fieldName)
            if (!isEmptyOptionalValue(field, fieldValue, cfg)) {
              if (first) first = false
              else {
                out.write(',')
                if (doPrettyPrint) pad(indent_, out)
              }
              strEnc.unsafeEncode(fieldName, indent_, out)
              if (doPrettyPrint) out.write(" : ")
              else out.write(':')
              encoders(idx).unsafeEncode(fieldValue, indent_, out)
            }
            idx += 1
          }
          if (doPrettyPrint) pad(indent, out)
          out.write('}')
        }
      }
    }
  }

  object JsonDecoder {

    import Codecs._
    import ProductDecoder._

    private case class DecoderKey[A](schema: Schema[A], discriminator: Option[String]) {
      override val hashCode: Int = System.identityHashCode(schema) ^ discriminator.hashCode

      override def equals(obj: Any): Boolean = obj match {
        case dk: DecoderKey[_] => (dk.schema eq schema) && dk.discriminator == discriminator
        case _                 => false
      }
    }

    private[this] val decoders = new ConcurrentHashMap[DecoderKey[_], ZJsonDecoder[_]]

    final def decode[A](schema: Schema[A], json: String): Either[DecodeError, A] =
      schemaDecoder(schema).decodeJson(json) match {
        case Left(value)  => Left(ReadError(Cause.empty, value))
        case Right(value) => Right(value)
      }

    def x[A](dec: ZJsonDecoder[A]): Unit = dec match {
      case _: ZJsonDecoder[_] =>
    }

    private val emptyObjectDecoder: ZJsonDecoder[Boolean] =
      (_: List[JsonError], in: RetractReader) => {
        val c1 = in.nextNonWhitespace()
        val c2 = in.nextNonWhitespace()
        c1 == '{' && c2 == '}'
      }

    private[schema] def option[A](A: ZJsonDecoder[A]): ZJsonDecoder[Option[A]] =
      new ZJsonDecoder[Option[A]] { self =>

        private[this] val ull: Array[Char] = "ull".toCharArray

        def unsafeDecode(trace: List[JsonError], in: RetractReader): Option[A] =
          (in.nextNonWhitespace(): @switch) match {
            case 'n' =>
              Lexer.readChars(trace, in, ull, "null")
              None
            case '{' =>
              // If we encounter a `{` it could either be a legitimate object or an empty object marker
              in.retract()
              val rr = new WithRecordingReader(in, 2)
              if (emptyObjectDecoder.unsafeDecode(trace, rr)) {
                None
              } else {
                rr.rewind()
                Some(A.unsafeDecode(trace, rr))
              }
            case _ =>
              in.retract()
              Some(A.unsafeDecode(trace, in))
          }

        override def unsafeDecodeMissing(trace: List[JsonError]): Option[A] =
          None

        final override def unsafeFromJsonAST(trace: List[JsonError], json: Json): Option[A] =
          json match {
            case Json.Null         => None
            case Json.Obj(Chunk()) => None
            case _                 => Some(A.unsafeFromJsonAST(trace, json))
          }
      }

    private[codec] def schemaDecoder[A](schema: Schema[A], discriminator: Option[String] = None): ZJsonDecoder[A] = {
      val key     = DecoderKey(schema, discriminator)
      var decoder = decoders.get(key).asInstanceOf[ZJsonDecoder[A]]
      if (decoder eq null) {
        decoder = schemaDecoderSlow(schema, discriminator)
        decoders.put(key, decoder)
      }
      decoder
    }

    //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
    private[this] def schemaDecoderSlow[A](schema: Schema[A], discriminator: Option[String]): ZJsonDecoder[A] = schema match {
      case Schema.Primitive(standardType, _)              => primitiveCodec(standardType).decoder
      case Schema.Optional(codec, _)                      => option(schemaDecoder(codec, discriminator))
      case Schema.Tuple2(left, right, _)                  => ZJsonDecoder.tuple2(schemaDecoder(left), schemaDecoder(right))
      case Schema.Transform(c, f, _, a, _)                => schemaDecoder(a.foldLeft(c)((s, a) => s.annotate(a)), discriminator).mapOrFail(f)
      case Schema.Sequence(codec, f, _, _, _)             => ZJsonDecoder.chunk(schemaDecoder(codec)).map(f)
      case s @ Schema.NonEmptySequence(codec, _, _, _, _) => ZJsonDecoder.chunk(schemaDecoder(codec)).map(s.fromChunk)
      case Schema.Map(ks, vs, _)                          => mapDecoder(ks, vs)
      case Schema.NonEmptyMap(ks, vs, _)                  => mapDecoder(ks, vs).mapOrFail(m => NonEmptyMap.fromMapOption(m).toRight("NonEmptyMap expected"))
      case Schema.Set(s, _)                               => ZJsonDecoder.chunk(schemaDecoder(s)).map(entries => entries.toSet)
      case Schema.Fail(message, _)                        => failDecoder(message)
      case s: Schema.GenericRecord                        => recordDecoder(s, discriminator)
      case Schema.Either(left, right, _)                  => ZJsonDecoder.either(schemaDecoder(left), schemaDecoder(right))
      case s @ Schema.Fallback(_, _, _, _)                => fallbackDecoder(s)
      case l @ Schema.Lazy(_)                             => ZJsonDecoder.suspend(schemaDecoder(l.schema, discriminator))
      //case Schema.Meta(_, _)                                                                           => astDecoder
      case s @ Schema.CaseClass0(_, _, _)                                => caseClass0Decoder(discriminator, s)
      case s @ Schema.CaseClass1(_, _, _, _)                             => caseClass1Decoder(discriminator, s)
      case s @ Schema.CaseClass2(_, _, _, _, _)                          => caseClass2Decoder(discriminator, s)
      case s @ Schema.CaseClass3(_, _, _, _, _, _)                       => caseClass3Decoder(discriminator, s)
      case s @ Schema.CaseClass4(_, _, _, _, _, _, _)                    => caseClass4Decoder(discriminator, s)
      case s @ Schema.CaseClass5(_, _, _, _, _, _, _, _)                 => caseClass5Decoder(discriminator, s)
      case s @ Schema.CaseClass6(_, _, _, _, _, _, _, _, _)              => caseClass6Decoder(discriminator, s)
      case s @ Schema.CaseClass7(_, _, _, _, _, _, _, _, _, _)           => caseClass7Decoder(discriminator, s)
      case s @ Schema.CaseClass8(_, _, _, _, _, _, _, _, _, _, _)        => caseClass8Decoder(discriminator, s)
      case s @ Schema.CaseClass9(_, _, _, _, _, _, _, _, _, _, _, _)     => caseClass9Decoder(discriminator, s)
      case s @ Schema.CaseClass10(_, _, _, _, _, _, _, _, _, _, _, _, _) => caseClass10Decoder(discriminator, s)
      case s @ Schema.CaseClass11(_, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass11Decoder(discriminator, s)
      case s @ Schema.CaseClass12(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass12Decoder(discriminator, s)
      case s @ Schema.CaseClass13(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass13Decoder(discriminator, s)
      case s @ Schema
            .CaseClass14(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass14Decoder(discriminator, s)
      case s @ Schema
            .CaseClass15(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass15Decoder(discriminator, s)
      case s @ Schema.CaseClass16(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass16Decoder(discriminator, s)
      case s @ Schema.CaseClass17(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass17Decoder(discriminator, s)
      case s @ Schema.CaseClass18(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass18Decoder(discriminator, s)
      case s @ Schema.CaseClass19(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass19Decoder(discriminator, s)
      case s @ Schema.CaseClass20(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass20Decoder(discriminator, s)
      case s @ Schema.CaseClass21(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass21Decoder(discriminator, s)
      case s @ Schema.CaseClass22(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass22Decoder(discriminator, s)
      case s: Schema.Enum[A]     => enumDecoder(s)
      case d @ Schema.Dynamic(_) => dynamicDecoder(d)
      case _                     => throw new Exception(s"Missing a handler for decoding of schema ${schema.toString()}.")
    }
    //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

    private[codec] def mapDecoder[K, V](
      ks: Schema[K],
      vs: Schema[V]
    ): ZJsonDecoder[Map[K, V]] = {
      val valueDecoder = JsonDecoder.schemaDecoder(vs)
      jsonFieldDecoder(ks) match {
        case Some(jsonFieldDecoder) =>
          ZJsonDecoder.keyValueChunk(jsonFieldDecoder, valueDecoder).map(a => Chunk.fromIterable(a).toMap)
        case None =>
          ZJsonDecoder
            .chunk(schemaDecoder(ks).zip(schemaDecoder(vs)))
            .map(_.toList.toMap)
      }
    }

    private[codec] def jsonFieldDecoder[A](schema: Schema[A]): Option[JsonFieldDecoder[A]] =
      schema match {
        case Schema.Primitive(StandardType.StringType, _) => Option(JsonFieldDecoder.string)
        case Schema.Primitive(StandardType.LongType, _)   => Option(JsonFieldDecoder.long)
        case Schema.Primitive(StandardType.IntType, _)    => Option(JsonFieldDecoder.int)
        case Schema.Transform(c, f, _, a, _) =>
          jsonFieldDecoder(a.foldLeft(c)((s, a) => s.annotate(a))).map(_.mapOrFail(f))
        case Schema.Lazy(inner) => jsonFieldDecoder(inner())
        case _                  => None
      }

    private def dynamicDecoder(schema: Schema.Dynamic): ZJsonDecoder[DynamicValue] = {
      val directMapping = schema.annotations.exists {
        case directDynamicMapping() => true
        case _                      => false
      }

      if (directMapping) {
        Json.decoder.map(jsonToDynamicValue)
      } else {
        schemaDecoder(DynamicValue.schema)
      }
    }

    private def jsonToDynamicValue(json: Json): DynamicValue =
      json match {
        case Json.Obj(fields) =>
          DynamicValue.Record(
            TypeId.Structural,
            ListMap(fields.map { case (k, v) => k -> jsonToDynamicValue(v) }: _*)
          )
        case Json.Arr(elements) => DynamicValue.Sequence(elements.map(jsonToDynamicValue))
        case Json.Bool(value)   => DynamicValue.Primitive(value, StandardType.BoolType)
        case Json.Str(value)    => DynamicValue.Primitive(value, StandardType.StringType)
        case Json.Num(value)    => DynamicValue.Primitive(value, StandardType.BigDecimalType)
        case Json.Null          => DynamicValue.NoneValue
      }

    private def error(msg: String, trace: List[JsonError]): Nothing =
      throw UnsafeJson(JsonError.Message(msg) :: trace)

    private def enumDecoder[Z](parentSchema: Schema.Enum[Z]): ZJsonDecoder[Z] = {
      val caseNameAliases = new mutable.HashMap[String, Schema.Case[Z, Any]]
      parentSchema.cases.foreach { case_ =>
        val schema = case_.asInstanceOf[Schema.Case[Z, Any]]
        caseNameAliases.put(case_.caseName, schema)
        case_.caseNameAliases.foreach(a => caseNameAliases.put(a, schema))
      }

      if (parentSchema.cases.forall(_.schema.isInstanceOf[Schema.CaseClass0[_]])) { // if all cases are CaseClass0, decode as String
        if (caseNameAliases.size <= 64) {
          new ZJsonDecoder[Z] {
            private[this] val stringMatrix = new StringMatrix(caseNameAliases.keys.toArray)
            private[this] val cases = caseNameAliases.values.map { case_ =>
              case_.schema.asInstanceOf[Schema.CaseClass0[Any]].defaultConstruct()
            }.toArray.asInstanceOf[Array[Z]]

            override def unsafeDecode(trace: List[JsonError], in: RetractReader): Z = {
              val idx = Lexer.enumeration(trace, in, stringMatrix)
              if (idx < 0) error("unrecognized string", trace)
              cases(idx)
            }
          }
        } else {
          new ZJsonDecoder[Z] {
            private[this] val cases = new util.HashMap[String, Z](caseNameAliases.size * 2)

            caseNameAliases.foreach {
              case (name, case_) =>
                cases.put(name, case_.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct())
            }

            override def unsafeDecode(trace: List[JsonError], in: RetractReader): Z = {
              val result = cases.get(Lexer.string(trace, in).toString)
              if (result == null) error("unrecognized string", trace)
              result
            }
          }
        }
      } else if (parentSchema.annotations.exists(_.isInstanceOf[noDiscriminator])) {
        new ZJsonDecoder[Z] {
          private[this] val decoders = parentSchema.cases.map(c => schemaDecoder(c.schema))

          override def unsafeDecode(trace: List[JsonError], in: RetractReader): Z = {
            var rr = RecordingReader(in)
            val it = decoders.iterator
            while (it.hasNext) {
              try {
                return it.next().unsafeDecode(trace, rr).asInstanceOf[Z]
              } catch {
                case ex if NonFatal(ex) =>
                  rr.rewind()
                  rr = RecordingReader(rr)
              }
            }
            error("none of the subtypes could decode the data", trace)
          }
        }
      } else {
        val discriminator = parentSchema.annotations.collectFirst { case d: discriminatorName => d.tag }
        discriminator match {
          case None =>
            if (caseNameAliases.size <= 64) {
              val stringMatrix = new StringMatrix(caseNameAliases.keys.toArray)
              val cases = caseNameAliases.values.map { case_ =>
                (JsonError.ObjectAccess(case_.caseName), schemaDecoder(case_.schema))
              }.toArray
              (trace: List[JsonError], in: RetractReader) => {
                val lexer = Lexer
                lexer.char(trace, in, '{')
                if (!lexer.firstField(trace, in)) error("missing subtype", trace)
                val idx = lexer.field(trace, in, stringMatrix)
                if (idx < 0) error("unrecognized subtype", trace)
                val spanWithDecoder = cases(idx)
                val trace_          = spanWithDecoder._1 :: trace
                val decoded         = spanWithDecoder._2.unsafeDecode(trace_, in).asInstanceOf[Z]
                lexer.nextField(trace_, in)
                decoded
              }
            } else {
              val cases =
                new util.HashMap[String, (JsonError.ObjectAccess, ZJsonDecoder[Any])](caseNameAliases.size * 2)
              caseNameAliases.foreach {
                case (name, case_) =>
                  cases.put(name, (JsonError.ObjectAccess(case_.caseName), schemaDecoder(case_.schema)))
              }
              (trace: List[JsonError], in: RetractReader) => {
                val lexer = Lexer
                lexer.char(trace, in, '{')
                if (!lexer.firstField(trace, in)) error("missing subtype", trace)
                val fieldName       = lexer.string(trace, in).toString
                val spanWithDecoder = cases.get(fieldName)
                if (spanWithDecoder eq null) error("unrecognized subtype", trace)
                val trace_ = spanWithDecoder._1 :: trace
                lexer.char(trace_, in, ':')
                val decoded = spanWithDecoder._2.unsafeDecode(trace_, in).asInstanceOf[Z]
                lexer.nextField(trace_, in)
                decoded
              }
            }
          case Some(discriminatorName) =>
            if (caseNameAliases.size <= 64) {
              val discriminatorMatrix = new StringMatrix(Array(discriminatorName))
              val discriminatorSpan   = JsonError.ObjectAccess(discriminatorName)
              val caseMatrix          = new StringMatrix(caseNameAliases.keys.toArray)
              val cases = caseNameAliases.values.map { case_ =>
                (JsonError.ObjectAccess(case_.caseName), schemaDecoder(case_.schema, discriminator))
              }.toArray
              (trace: List[JsonError], in: RetractReader) => {
                val lexer = Lexer
                lexer.char(trace, in, '{')
                if (!lexer.firstField(trace, in)) error("missing subtype", trace)
                val rr = RecordingReader(in)
                while ({
                  (lexer.field(trace, rr, discriminatorMatrix) < 0) && {
                    lexer.skipValue(trace, rr)
                    lexer.nextField(trace, rr) || error("missing subtype", trace)
                  }
                }) ()
                val trace_ = discriminatorSpan :: trace
                val idx    = lexer.enumeration(trace_, rr, caseMatrix)
                rr.rewind()
                if (idx < 0) error("unrecognized subtype", trace_)
                val spanWithDecoder = cases(idx)
                spanWithDecoder._2.unsafeDecode(spanWithDecoder._1 :: trace_, rr).asInstanceOf[Z]
              }
            } else {
              val discriminatorSpan = JsonError.ObjectAccess(discriminatorName)
              val cases =
                new util.HashMap[String, (JsonError.ObjectAccess, ZJsonDecoder[Any])](caseNameAliases.size * 2)
              caseNameAliases.foreach {
                case (name, case_) =>
                  cases.put(name, (JsonError.ObjectAccess(case_.caseName), schemaDecoder(case_.schema, discriminator)))
              }
              (trace: List[JsonError], in: RetractReader) => {
                val lexer = Lexer
                lexer.char(trace, in, '{')
                if (!lexer.firstField(trace, in)) error("missing subtype", trace)
                val rr = RecordingReader(in)
                while ({
                  (lexer.string(trace, rr).toString != discriminatorName) && {
                    lexer.char(trace, rr, ':')
                    lexer.skipValue(trace, rr)
                    lexer.nextField(trace, rr) || error("missing subtype", trace)
                  }
                }) ()
                val trace_ = discriminatorSpan :: trace
                lexer.char(trace_, rr, ':')
                val fieldValue = lexer.string(trace_, rr).toString
                rr.rewind()
                val spanWithDecoder = cases.get(fieldValue)
                if (spanWithDecoder eq null) error("unrecognized subtype", trace_)
                spanWithDecoder._2.unsafeDecode(spanWithDecoder._1 :: trace_, rr).asInstanceOf[Z]
              }
            }
        }
      }
    }

    private def recordDecoder(
      schema: GenericRecord,
      discriminator: Option[String]
    ): ZJsonDecoder[ListMap[String, Any]] = {
      val spansWithDecoders =
        new util.HashMap[String, (JsonError.ObjectAccess, ZJsonDecoder[Any])](schema.fields.size * 2)
      schema.fields.foreach { field =>
        val spanWithDecoder =
          (JsonError.ObjectAccess(field.fieldName), schemaDecoder(field.schema).asInstanceOf[ZJsonDecoder[Any]])
        field.nameAndAliases.foreach(spansWithDecoders.put(_, spanWithDecoder))
      }
      val skipExtraFields = !schema.annotations.exists(_.isInstanceOf[rejectExtraFields])
      (trace: List[JsonError], in: RetractReader) => {
        val lexer    = Lexer
        var continue = true
        if (discriminator eq None) {
          lexer.char(trace, in, '{')
          continue = lexer.firstField(trace, in)
        }
        val map = new util.HashMap[String, Any]
        while (continue) {
          val fieldNameOrAlias = lexer.string(trace, in).toString
          val spanWithDecoder  = spansWithDecoders.get(fieldNameOrAlias)
          if (spanWithDecoder ne null) {
            val span   = spanWithDecoder._1
            val dec    = spanWithDecoder._2
            val trace_ = span :: trace
            lexer.char(trace_, in, ':')
            val fieldName = span.field // reuse strings with calculated hashCode
            val prev      = map.put(fieldName, dec.unsafeDecode(trace_, in))
            if (prev != null) error("duplicate", trace_)
          } else if (skipExtraFields || discriminator.contains(fieldNameOrAlias)) {
            lexer.char(trace, in, ':')
            lexer.skipValue(trace, in)
          } else error("extra field", trace)
          continue = lexer.nextField(trace, in)
        }
        schema.fields.foreach { field =>
          val fieldName = field.fieldName // reuse strings with calculated hashCode
          if (map.get(fieldName) == null) {
            map.put( // mitigation of a linking error for `map.computeIfAbsent` in Scala.js
              fieldName, {
                if ((field.optional || field.transient) && field.defaultValue.isDefined) {
                  field.defaultValue.get
                } else {
                  var schema = field.schema
                  schema match {
                    case l: Schema.Lazy[_] => schema = l.schema
                    case _                 =>
                  }
                  schema match {
                    case _: Schema.Optional[_]               => None
                    case collection: Schema.Collection[_, _] => collection.empty
                    case _                                   => error("missing", spansWithDecoders.get(fieldName)._1 :: trace)
                  }
                }
              }
            )
          }
        }
        (ListMap.newBuilder[String, Any] ++= ({                         // to avoid O(n) insert operations
          import scala.collection.JavaConverters.mapAsScalaMapConverter // use deprecated class for Scala 2.12 compatibility

          map.asScala
        }: @scala.annotation.nowarn)).result()
      }
    }

    private def fallbackDecoder[A, B](schema: Schema.Fallback[A, B]): ZJsonDecoder[Fallback[A, B]] =
      new ZJsonDecoder[Fallback[A, B]] {
        private[this] val leftDecoder  = schemaDecoder(schema.left)
        private[this] val rightDecoder = schemaDecoder(schema.right)

        def unsafeDecode(trace: List[JsonError], in: RetractReader): Fallback[A, B] = {
          var left: Option[A]  = None
          var right: Option[B] = None

          case class BadEnd() extends Throwable

          try {
            // If this doesn't throw exception, it is an array, so it encodes a `Fallback.Both`
            val lexer = Lexer
            lexer.char(trace, in, '[')

            // get left element
            if (lexer.firstArrayElement(in)) {
              val trace_ = JsonError.ArrayAccess(0) :: trace
              try left = Some(leftDecoder.unsafeDecode(trace_, in))
              catch {
                case _: UnsafeJson => ()
              }

              // read until ',' if left wasn't decoded
              var continue = true
              while (continue) {
                try {
                  lexer.nextArrayElement(trace, in)
                  continue = false
                } catch {
                  case _: UnsafeJson => ()
                }
              }
            }

            // get right element
            if (((left eq None) || schema.fullDecode) && lexer.firstArrayElement(in)) {
              val trace_ = JsonError.ArrayAccess(1) :: trace
              try right = Some(rightDecoder.unsafeDecode(trace_, in))
              catch {
                case _: UnsafeJson => ()
              }
              try lexer.nextArrayElement(trace, in)
              catch {
                case _: UnsafeJson => throw BadEnd()
              }
            }

          } catch {
            // It's not an array, so it is of type A or B
            case BadEnd() => ()
            case _: UnsafeJson => {
              in.retract()
              val in2 = new zio.json.internal.WithRecordingReader(in, 64)
              try {
                left = Some(schemaDecoder(schema.left).unsafeDecode(trace, in2))
              } catch {
                case UnsafeJson(_) =>
                  in2.rewind()
                  right = Some(schemaDecoder(schema.right).unsafeDecode(trace, in2))
              }
            }
          }

          (left, right) match {
            case (Some(a), Some(b)) => Fallback.Both(a, b)
            case (Some(a), _)       => Fallback.Left(a)
            case (_, Some(b))       => Fallback.Right(b)
            case _                  => error("Fallback decoder was unable to decode both left and right sides", trace)
          }
        }
      }
  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private[codec] object ProductEncoder {
    import ZJsonEncoder.{ bump, pad }

    private[codec] def isEmptyOptionalValue(schema: Schema.Field[_, _], value: Any, cfg: Config) =
      (cfg.ignoreEmptyCollections || schema.optional) && (value match {
        case None           => true
        case _: Iterable[_] => value.asInstanceOf[Iterable[_]].isEmpty
        case _              => false
      })

    private[codec] def caseClassEncoder[Z](schema: Schema.Record[Z], cfg: Config, discriminatorTuple: DiscriminatorTuple): ZJsonEncoder[Z] = {
      val nonTransientFields = schema.nonTransientFields.map(_.asInstanceOf[Schema.Field[Z, Any]]).toArray
      val fieldEncoders      = nonTransientFields.map(s => JsonEncoder.schemaEncoder(s.schema, cfg, discriminatorTuple))
      (a: Z, indent: Option[Int], out: Write) => {
        out.write('{')
        val doPrettyPrint = indent ne None
        var indent_       = indent
        if (doPrettyPrint) {
          indent_ = bump(indent)
          pad(indent_, out)
        }
        val strEnc = string.encoder
        var first  = true
        if (discriminatorTuple ne None) {
          val tuple = discriminatorTuple.get
          first = false
          strEnc.unsafeEncode(tuple._1, indent_, out)
          if (doPrettyPrint) out.write(" : ")
          else out.write(':')
          strEnc.unsafeEncode(tuple._2, indent_, out)
        }
        var idx = 0
        while (idx < nonTransientFields.length) {
          val schema = nonTransientFields(idx)
          val enc    = fieldEncoders(idx)
          idx += 1
          val value = schema.get(a)
          if (!isEmptyOptionalValue(schema, value, cfg) && (!enc.isNothing(value) || cfg.explicitNulls)) {
            if (first) first = false
            else {
              out.write(',')
              if (doPrettyPrint) pad(indent_, out)
            }
            strEnc.unsafeEncode(schema.fieldName, indent_, out)
            if (doPrettyPrint) out.write(" : ")
            else out.write(':')
            enc.unsafeEncode(value, indent_, out)
          }
        }
        if (doPrettyPrint) pad(indent, out)
        out.write('}')
      }
    }
  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private[codec] object ProductDecoder {

    private[codec] def caseClass0Decoder[Z](discriminator: Option[String], schema: Schema.CaseClass0[Z]): ZJsonDecoder[Z] = {
      val rejectExtraFields = schema.annotations.exists(_.isInstanceOf[rejectExtraFields])
      val noDiscriminator   = discriminator.isEmpty
      (trace: List[JsonError], in: RetractReader) => {
        val lexer = Lexer
        if (noDiscriminator) lexer.char(trace, in, '{')
        var continue = lexer.firstField(trace, in)
        while (continue) {
          if (rejectExtraFields) {
            throw UnsafeJson(JsonError.Message("extra field") :: trace)
          }
          lexer.char(trace, in, '"')
          lexer.skipString(trace, in)
          lexer.char(trace, in, ':')
          lexer.skipValue(trace, in)
          continue = lexer.nextField(trace, in)
        }
        schema.defaultConstruct()
      }
    }

    private[codec] def caseClass1Decoder[A, Z](discriminator: Option[String], schema: Schema.CaseClass1[A, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.defaultConstruct(buffer(0).asInstanceOf[A])
      }
    }

    private[codec] def caseClass2Decoder[A1, A2, Z](discriminator: Option[String], schema: Schema.CaseClass2[A1, A2, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2])
      }
    }

    private[codec] def caseClass3Decoder[A1, A2, A3, Z](discriminator: Option[String], schema: Schema.CaseClass3[A1, A2, A3, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3])
      }
    }

    private[codec] def caseClass4Decoder[A1, A2, A3, A4, Z](discriminator: Option[String], schema: Schema.CaseClass4[A1, A2, A3, A4, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4])
      }
    }

    private[codec] def caseClass5Decoder[A1, A2, A3, A4, A5, Z](discriminator: Option[String], schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5])
      }
    }

    private[codec] def caseClass6Decoder[A1, A2, A3, A4, A5, A6, Z](discriminator: Option[String], schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6])
      }
    }

    private[codec] def caseClass7Decoder[A1, A2, A3, A4, A5, A6, A7, Z](discriminator: Option[String], schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7])
      }
    }

    private[codec] def caseClass8Decoder[A1, A2, A3, A4, A5, A6, A7, A8, Z](discriminator: Option[String], schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8])
      }
    }

    private[codec] def caseClass9Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](discriminator: Option[String], schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9])
      }
    }

    private[codec] def caseClass10Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](discriminator: Option[String], schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10])
      }
    }

    private[codec] def caseClass11Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](discriminator: Option[String], schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11])
      }
    }

    private[codec] def caseClass12Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](discriminator: Option[String], schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12])
      }
    }

    private[codec] def caseClass13Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](discriminator: Option[String], schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12], buffer(12).asInstanceOf[A13])
      }
    }

    private[codec] def caseClass14Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](discriminator: Option[String], schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(
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
      }
    }

    private[codec] def caseClass15Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](discriminator: Option[String], schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(
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
      }
    }

    private[codec] def caseClass16Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](discriminator: Option[String], schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(
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
      }
    }

    private[codec] def caseClass17Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](discriminator: Option[String], schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(
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
      }
    }

    private[codec] def caseClass18Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](discriminator: Option[String], schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(
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
      }
    }

    private[codec] def caseClass19Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](discriminator: Option[String], schema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(
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
      }
    }

    private[codec] def caseClass20Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](discriminator: Option[String], schema: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(
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
      }
    }

    private[codec] def caseClass21Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](discriminator: Option[String], schema: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(
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
      }
    }

    private[codec] def caseClass22Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](discriminator: Option[String], schema: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z]): ZJsonDecoder[Z] = {
      val ccjd = CaseClassJsonDecoder(schema, discriminator)
      (trace: List[JsonError], in: RetractReader) => {
        val buffer: Array[Any] = ccjd.unsafeDecodeFields(trace, in)
        schema.construct(
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
  //scalafmt: { maxColumn = 120, optIn.configStyleArguments = true }

  private class CaseClassJsonDecoder[Z](
    fields: Array[Schema.Field[Z, _]],
    fieldDecoders: Array[ZJsonDecoder[_]],
    spans: Array[JsonError.ObjectAccess],
    stringMatrix: StringMatrix,
    noDiscriminator: Boolean,
    skipExtraFields: Boolean
  ) {

    def unsafeDecodeFields(trace: List[JsonError], in: RetractReader): Array[Any] = {
      val lexer    = Lexer
      var continue = true
      if (noDiscriminator) {
        lexer.char(trace, in, '{')
        continue = lexer.firstField(trace, in)
      }
      val len    = fields.length
      val buffer = new Array[Any](len)
      while (continue) {
        val idx = lexer.field(trace, in, stringMatrix)
        if (idx >= 0) {
          val trace_ = spans(idx) :: trace
          if (idx == len) lexer.skipValue(trace_, in) // skipping discriminator field values
          else if (buffer(idx) == null) buffer(idx) = fieldDecoders(idx).unsafeDecode(trace_, in)
          else error("duplicate", trace_)
        } else if (skipExtraFields) lexer.skipValue(trace, in)
        else error("extra field", trace)
        continue = lexer.nextField(trace, in)
      }
      var idx = 0
      while (idx < len) {
        if (buffer(idx) == null) {
          val field = fields(idx)
          if ((field.optional || field.transient) && field.defaultValue.isDefined) {
            buffer(idx) = field.defaultValue.get
          } else {
            var schema = field.schema
            schema match {
              case l: Schema.Lazy[_] => schema = l.schema
              case _                 =>
            }
            buffer(idx) = schema match {
              case _: Schema.Optional[_]               => None
              case collection: Schema.Collection[_, _] => collection.empty
              case _                                   => error("missing", spans(idx) :: trace)
            }
          }
        }
        idx += 1
      }
      buffer
    }

    private def error(msg: String, trace: List[JsonError]): Nothing =
      throw UnsafeJson(JsonError.Message(msg) :: trace)
  }

  private object CaseClassJsonDecoder {

    def apply[Z](schema: Schema.Record[Z], discriminator: Option[String]): CaseClassJsonDecoder[Z] = {
      val len      = schema.fields.length
      val fields   = new Array[Schema.Field[Z, _]](len)
      val decoders = new Array[ZJsonDecoder[_]](len)
      val spans    = Array.newBuilder[JsonError.ObjectAccess]
      val names    = Array.newBuilder[String]
      val aliases  = Array.newBuilder[(String, Int)]
      var idx      = 0
      schema.fields.foreach { field =>
        fields(idx) = field
        decoders(idx) = schemaDecoder(field.schema)
        val name = field.fieldName
        names += name
        spans += JsonError.ObjectAccess(name)
        (field.nameAndAliases - name).foreach(a => aliases += ((a, idx)))
        idx += 1
      }
      val hasDiscriminator = discriminator.isDefined
      if (hasDiscriminator) {
        val discriminatorName = discriminator.get
        names += discriminatorName
        spans += JsonError.ObjectAccess(discriminatorName)
      }
      new CaseClassJsonDecoder(
        fields,
        decoders,
        spans.result(),
        new StringMatrix(names.result(), aliases.result()),
        !hasDiscriminator,
        !schema.annotations.exists(_.isInstanceOf[rejectExtraFields])
      )
    }
  }
}
