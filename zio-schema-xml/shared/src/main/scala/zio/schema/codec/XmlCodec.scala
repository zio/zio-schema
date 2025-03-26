package zio.schema.codec

import java.lang.Double
import java.math.BigDecimal
import java.nio.CharBuffer
import java.nio.charset.StandardCharsets
import java.util.Currency
import java.util.concurrent.ConcurrentHashMap

import scala.collection.immutable.ListMap
import scala.util.control.NonFatal
import scala.xml.{ Elem, MetaData, NamespaceBinding, Node, Null, TopScope, UnprefixedAttribute, XML }

import zio.prelude.NonEmptyMap
import zio.schema._
import zio.schema.annotation.{ discriminatorName, rejectExtraFields, _ }
import zio.schema.codec.DecodeError.ReadError
import zio.schema.codec.XmlCodec.Codecs._
import zio.stream.ZPipeline
import zio.{ Cause, Chunk, ZIO }

object XmlCodec {

  final case class Config(
    ignoreEmptyCollections: Boolean,
    explicitNulls: Boolean = false,
    namespace: Option[String] = None, // namespace support
    attributes: Set[String] = Set(),  // list of fields to encode as attributes
    prettyPrint: Boolean = false
  )

  object Config {
    val default: Config = Config(ignoreEmptyCollections = false)
  }

  type DiscriminatorTuple = Option[(String, String)]

  // Single implicit definition using Config.default
  implicit def xmlBinaryCodec[A](implicit schema: Schema[A]): BinaryCodec[A] =
    schemaBasedBinaryCodec(Config.default)

  /** A low-level XML encoder abstraction analogous to ZJsonEncoder.
   * Implementations should produce a scala.xml.Elem representing the value.
   */
  trait ZXmlEncoder[A] {
    def encodeXml(value: A): Elem

    // Adding contramap to transform an input type B into A
    def contramap[B](f: B => A): ZXmlEncoder[B] =
      new ZXmlEncoder[B] {
        override def encodeXml(value: B): Elem = ZXmlEncoder.this.encodeXml(f(value))
      }

    def isNothing(value: A): Boolean = false

  }

  // --- Low-level XML Decoder trait ---
  trait ZXmlDecoder[A] { self =>
    def decodeXml(node: Node): Either[DecodeError, A]

    // Default implementation of mapOrFail: transforms the successful value,
    // or returns a failure if the function returns a Left.
    def mapOrFail[B](f: A => Either[String, B]): ZXmlDecoder[B] = new ZXmlDecoder[B] {
      override def decodeXml(node: Node): Either[DecodeError, B] =
        ZXmlDecoder.this.decodeXml(node).flatMap { a =>
          f(a).left.map(err => ReadError(Cause.empty, err))
        }
    }

    // A simple map combinator: applies f to the successful result.
    def map[B](f: A => B): ZXmlDecoder[B] = new ZXmlDecoder[B] {
      override def decodeXml(node: Node): Either[DecodeError, B] =
        ZXmlDecoder.this.decodeXml(node).map(f)
    }

    def flatMap[B](f: A => ZXmlDecoder[B]): ZXmlDecoder[B] =
      new ZXmlDecoder[B] {
        override def decodeXml(node: Node): Either[DecodeError, B] =
          self.decodeXml(node).flatMap(a => f(a).decodeXml(node))
      }
  }

  trait ZXmlDecoderChunk[A] { self =>
    def decodeXmlboolean(node: Node): Either[DecodeError, Chunk[A]]
  }

  trait XmlFieldEncoder[A] {
    def unsafeEncodeField(a: A): String
  }

  trait XmlFieldDecoder[A] {
    def decodeField(node: Node): Either[DecodeError, A]

    def mapOrFail[B](f: A => Either[String, B]): XmlFieldDecoder[B] = new XmlFieldDecoder[B] {
      override def decodeField(node: Node): Either[DecodeError, B] =
        XmlFieldDecoder.this.decodeField(node).flatMap(a => f(a).left.map(err => ReadError(Cause.empty, err)))
    }
  }

  /** A simple container that groups together an XML encoder and decoder for type A. */
  case class ZXmlCodec[A](encoder: ZXmlEncoder[A], decoder: ZXmlDecoder[A])

  object ZXmlCodec {

    def apply[A](encoder: ZXmlEncoder[A], decoder: ZXmlDecoder[A]): ZXmlCodec[A] =
      new ZXmlCodec(encoder, decoder)
  }

  sealed trait XmlError

  object XmlError {
    case class ObjectAccess(field: String) extends XmlError
    case class Message(msg: String)        extends XmlError
    case class Raw(msg: String)            extends XmlError

    def render(errors: List[XmlError]): String =
      errors.map {
        case ObjectAccess(field) => s"ObjectAccess($field)"
        case Message(msg)        => s"Message($msg)"
        case Raw(msg)            => s"$msg"

      }.mkString(", ")
  }

  def schemaBasedBinaryCodec[A](cfg: Config)(implicit schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {
      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] = {
        val xmlStr = new String(whole.toArray, StandardCharsets.UTF_8)
        val result = zio.schema.codec.XmlCodec.XmlDecoder.decode(schema, xmlStr)
        result
      }

      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] = schema match {
        case Schema.Primitive(StandardType.BoolType, _) =>
          (ZPipeline.utfDecode
            .mapError(cce => ReadError(Cause.fail(cce), cce.getMessage))
            .map(_.trim)
            .filter(_.nonEmpty)
            .mapZIO { xmlString =>
              ZIO.fromEither {
                val xml = XML.loadString(xmlString)
                zio.schema.codec.XmlCodec.Codecs.extractAll(statefulBoolDecoder, xml, "No more booleans")
              }
            }
            .mapChunks((chunk: Chunk[List[Boolean]]) => chunk.flatMap(Chunk.fromIterable)))
            .asInstanceOf[ZPipeline[Any, DecodeError, Byte, A]]

        case Schema.Primitive(StandardType.IntType, _) =>
          (ZPipeline.utfDecode
            .mapError(cce => ReadError(Cause.fail(cce), cce.getMessage))
            .map(_.trim)
            .filter(_.nonEmpty)
            .mapZIO { xmlString =>
              ZIO.fromEither {
                val xml = XML.loadString(xmlString)
                zio.schema.codec.XmlCodec.Codecs.extractAll(statefulIntDecoder, xml, "No more ints")
              }
            }
            .mapChunks((chunk: Chunk[List[Int]]) => chunk.flatMap(Chunk.fromIterable)))
            .asInstanceOf[ZPipeline[Any, DecodeError, Byte, A]]

        case _ =>
          // Default pipeline for other types.
          ZPipeline.utfDecode
            .mapError(cce => ReadError(Cause.fail(cce), cce.getMessage))
            .map(_.trim)
            .filter(_.nonEmpty)
            .mapZIO { xmlString =>
              ZIO.fromEither(zio.schema.codec.XmlCodec.XmlDecoder.decode(schema, xmlString))
            }
      }

      override def encode(value: A): Chunk[Byte] = {
        val encoded = XmlEncoder.encode(schema, value, cfg)
        encoded
      }

      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
        ZPipeline
          .mapChunks[A, Chunk[Byte]] { chunk =>
            chunk.map { value =>
              encode(value)
            }
          }
          .intersperse(Chunk.single('\n'.toByte))
          .flattenChunks

    }

  /** Derives an XML encoder for a given schema using the default configuration. */
  def xmlEncoder[A](schema: Schema[A]): ZXmlEncoder[A] =
    xmlEncoder(Config.default)(schema)

  def xmlEncoder[A](cfg: Config)(schema: Schema[A]): ZXmlEncoder[A] =
    XmlEncoder.schemaEncoder(schema, cfg)

  def xmlDecoder[A](schema: Schema[A]): ZXmlDecoder[A] =
    XmlDecoder.schemaDecoder(schema)

  def xmlCodec[A](schema: Schema[A]): ZXmlCodec[A] =
    ZXmlCodec(xmlEncoder(schema), xmlDecoder(schema))

  def xmlCodec[A](cfg: Config)(schema: Schema[A]): ZXmlCodec[A] =
    ZXmlCodec(xmlEncoder(cfg)(schema), xmlDecoder(schema))

  object Codecs {

    import scala.xml.{ Elem, Node }
    import java.time._
    import java.time.format.DateTimeFormatter

    // ─────────────────────────────────────────────────────────────
    // Unit Codec: Representing a Unit value as an XML element.
    // ─────────────────────────────────────────────────────────────

    protected[codec] val unitEncoder: ZXmlEncoder[Unit] =
      new ZXmlEncoder[Unit] {
        override def encodeXml(value: Unit): Elem =
          <unit/>
      }

    private[codec] val unitDecoder: ZXmlDecoder[Unit] =
      new ZXmlDecoder[Unit] {
        override def decodeXml(node: Node): Either[DecodeError, Unit] =
          if (node.label == "unit") Right(())
          else Left(ReadError(Cause.empty, s"Expected <unit/>, found <${node.label}>"))
      }

    protected[codec] val unitCodec: ZXmlCodec[Unit] =
      ZXmlCodec(unitEncoder, unitDecoder)

    // ─────────────────────────────────────────────────────────────
    // Fail Decoder: A helper to produce a failing XML decoder.
    // ─────────────────────────────────────────────────────────────

    protected[codec] def failDecoder[A](message: String): ZXmlDecoder[A] =
      new ZXmlDecoder[A] {
        override def decodeXml(node: Node): Either[DecodeError, A] =
          Left(ReadError(Cause.empty, message))
      }

    protected[codec] def statefulBoolDecoder: ZXmlDecoder[Boolean] =
      new ZXmlDecoder[Boolean] {
        private var cached: Option[Seq[Boolean]] = None
        private var index: Int                   = 0
        private var lastNode: Option[Node]       = None

        override def decodeXml(node: Node): Either[DecodeError, Boolean] = {
          if (lastNode.forall(_ ne node)) {
            cached = None
            index = 0
            lastNode = Some(node)
          }

          if (cached.isEmpty) {
            val direct = node \ "boolean"

            val boolNodes: Seq[Node] = node.label match {
              case "boolean" => Seq(node)
              case _         => if (direct.nonEmpty) direct else node \\ "boolean"
            }

            if (boolNodes.isEmpty) {
              val text = node.text.trim.toLowerCase
              text match {
                case "true"  => return Right(true)
                case "false" => return Right(false)
                case _       => return Left(ReadError(Cause.empty, s"Invalid Boolean value: $text"))
              }
            } else {
              val decoded: Seq[Either[DecodeError, Boolean]] = boolNodes.map { child =>
                val text = child.text.trim.toLowerCase
                text match {
                  case "true"  => Right(true)
                  case "false" => Right(false)
                  case _       => Left(ReadError(Cause.empty, s"Invalid Boolean value: $text"))
                }
              }
              decoded.collectFirst { case Left(err) => err } match {
                case Some(err) => return Left(err)
                case None =>
                  val allBooleans = decoded.collect { case Right(b) => b }
                  cached = Some(allBooleans)
                  index = 0
              }
            }
          }
          cached match {
            case Some(booleans) if index < booleans.size =>
              val result = booleans(index)
              index += 1
              Right(result)
            case _ =>
              Left(ReadError(Cause.empty, "No more booleans"))
          }
        }
      }

    protected[codec] def statefulIntDecoder: ZXmlDecoder[Int] =
      new ZXmlDecoder[Int] {
        private var cached: Option[Seq[Int]] = None
        private var index: Int               = 0
        private var lastNode: Option[Node]   = None

        override def decodeXml(node: Node): Either[DecodeError, Int] = {
          if (lastNode.forall(_ ne node)) {
            cached = None
            index = 0
            lastNode = Some(node)
          }

          if (node.label == "int") {
            if (index == 0) {
              index = 1
              node.text.trim.toIntOption.toRight(
                ReadError(Cause.empty, s"Invalid Int value: ${node.text.trim}")
              )
            } else {
              Left(ReadError(Cause.empty, "No more ints"))
            }
          } else {
            val direct              = node \ "int"
            val intNodes: Seq[Node] = if (direct.nonEmpty) direct.toList else (node \\ "int").toList

            if (intNodes.isEmpty) {
              val tokens = node.text.trim.split("\\s+")
              if (tokens.length == 1) {
                tokens(0).toIntOption match {
                  case Some(i) => Right(i)
                  case None    => Left(ReadError(Cause.empty, s"Invalid Int value: ${tokens(0)}"))
                }
              } else {
                val joined = tokens.mkString("")
                joined.toIntOption match {
                  case Some(i) => Right(i)
                  case None    => Left(ReadError(Cause.empty, s"Invalid concatenated Int value: $joined"))
                }
              }
            } else {
              if (intNodes.size == 1) {
                intNodes.head.text.trim.toIntOption.toRight(
                  ReadError(Cause.empty, s"Invalid Int value: ${intNodes.head.text.trim}")
                )
              } else {
                if (cached.isEmpty) {
                  val decoded: Seq[Either[DecodeError, Int]] = intNodes.map { child =>
                    val text = child.text.trim
                    text.toIntOption match {
                      case Some(i) => Right(i)
                      case None    => Left(ReadError(Cause.empty, s"Invalid Int value: $text"))
                    }
                  }
                  decoded.collectFirst { case Left(err) => err } match {
                    case Some(err) => return Left(err)
                    case None =>
                      val allInts = decoded.collect { case Right(i) => i }
                      cached = Some(allInts)
                      index = 0
                  }
                }
                cached match {
                  case Some(ints) if index < ints.size =>
                    val result = ints(index)
                    index += 1
                    Right(result)
                  case _ =>
                    Left(ReadError(Cause.empty, "No more ints"))
                }
              }
            }
          }
        }
      }

    def extractAll[T](
      decoder: ZXmlDecoder[T],
      node: Node,
      noMoreMsg: String
    ): Either[DecodeError, List[T]] = {
      def loop(acc: List[T]): Either[DecodeError, List[T]] =
        decoder.decodeXml(node) match {
          case Right(value) =>
            loop(acc :+ value)
          case Left(ReadError(_, msg)) if msg == noMoreMsg =>
            Right(acc)
          case Left(err) =>
            Left(err)
        }
      loop(Nil)
    }

    // ─────────────────────────────────────────────────────────────
    // Primitive Codec: Maps a StandardType to an XML codec.
    // ─────────────────────────────────────────────────────────────

    private[codec] def primitiveCodec[A](standardType: StandardType[A]): ZXmlCodec[A] =
      standardType match {
        case StandardType.StringType =>
          ZXmlCodec(
            new ZXmlEncoder[String] {
              override def encodeXml(value: String): Elem = <string>{value}</string>
            },
            new ZXmlDecoder[String] {
              override def decodeXml(node: Node): Either[DecodeError, String] =
                Right(node.text)
            }
          )

        case StandardType.UnitType => unitCodec

        case StandardType.IntType =>
          ZXmlCodec(
            new ZXmlEncoder[Int] {
              override def encodeXml(value: Int): Elem = <int>{value.toString}</int>
            },
            statefulIntDecoder
          )

        case StandardType.BoolType =>
          ZXmlCodec(
            new ZXmlEncoder[Boolean] {
              override def encodeXml(value: Boolean): Elem = <boolean>{value.toString}</boolean>
            },
            statefulBoolDecoder
          )

        case StandardType.FloatType =>
          ZXmlCodec(
            new ZXmlEncoder[Float] {
              override def encodeXml(value: Float): Elem = <float>{value.toString}</float>
            },
            new ZXmlDecoder[Float] {
              override def decodeXml(node: Node): Either[DecodeError, Float] =
                node.text.toFloatOption.toRight(ReadError(Cause.empty, "Invalid Float value"))
            }
          )

        case StandardType.DoubleType =>
          ZXmlCodec(
            new ZXmlEncoder[java.lang.Double] {
              override def encodeXml(value: Double): Elem = {
                val result = <double>{java.lang.Double.toString(value)}</double>
                result
              }
            },
            new ZXmlDecoder[java.lang.Double] {
              override def decodeXml(node: Node): Either[DecodeError, java.lang.Double] =
                try {
                  Right(java.lang.Double.valueOf(node.text).doubleValue())
                } catch {
                  case NonFatal(_) =>
                    Left(ReadError(Cause.empty, s"Invalid Double value: ${node.text}"))
                }
            }
          ).asInstanceOf[ZXmlCodec[A]]

        case StandardType.LocalDateType =>
          ZXmlCodec(
            new ZXmlEncoder[LocalDate] {
              override def encodeXml(value: LocalDate): Elem =
                <localDate>{value.format(DateTimeFormatter.ISO_LOCAL_DATE)}</localDate>
            },
            new ZXmlDecoder[LocalDate] {
              override def decodeXml(node: Node): Either[DecodeError, LocalDate] =
                try {
                  Right(LocalDate.parse(node.text, DateTimeFormatter.ISO_LOCAL_DATE))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, "Invalid LocalDate format"))
                }
            }
          )

        case StandardType.LocalDateTimeType =>
          ZXmlCodec(
            new ZXmlEncoder[LocalDateTime] {
              override def encodeXml(value: LocalDateTime): Elem =
                <localDateTime>{value.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)}</localDateTime>
            },
            new ZXmlDecoder[LocalDateTime] {
              override def decodeXml(node: Node): Either[DecodeError, LocalDateTime] =
                try {
                  Right(LocalDateTime.parse(node.text, DateTimeFormatter.ISO_LOCAL_DATE_TIME))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, "Invalid LocalDateTime format"))
                }
            }
          )

        case StandardType.InstantType =>
          ZXmlCodec(
            new ZXmlEncoder[Instant] {
              override def encodeXml(value: Instant): Elem = <instant>{value.toString}</instant>
            },
            new ZXmlDecoder[Instant] {
              override def decodeXml(node: Node): Either[DecodeError, Instant] =
                try {
                  Right(Instant.parse(node.text))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, "Invalid Instant format"))
                }
            }
          )

        case StandardType.BigDecimalType =>
          ZXmlCodec(
            new ZXmlEncoder[BigDecimal] {
              override def encodeXml(value: BigDecimal): Elem = <bigDecimal>{value.toString}</bigDecimal>
            },
            new ZXmlDecoder[BigDecimal] {
              override def decodeXml(node: Node): Either[DecodeError, BigDecimal] =
                try {
                  Right(new BigDecimal(node.text))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, "Invalid BigDecimal format"))
                }
            }
          )

        case StandardType.ByteType =>
          ZXmlCodec(
            new ZXmlEncoder[Byte] {
              override def encodeXml(value: Byte): Elem = <byte>{value.toString}</byte>
            },
            new ZXmlDecoder[Byte] {
              override def decodeXml(node: Node): Either[DecodeError, Byte] =
                node.text.toByteOption.toRight(ReadError(Cause.empty, "Invalid Byte value"))
            }
          )

        case StandardType.BigIntegerType =>
          ZXmlCodec(
            new ZXmlEncoder[java.math.BigInteger] {
              override def encodeXml(value: java.math.BigInteger): Elem = <bigInt>{BigInt(value).toString}</bigInt>
            },
            new ZXmlDecoder[java.math.BigInteger] {
              override def decodeXml(node: Node): Either[DecodeError, java.math.BigInteger] =
                try {
                  Right(new java.math.BigInteger(node.text))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, "Invalid BigInt format"))
                }
            }
          ).asInstanceOf[ZXmlCodec[A]]

        case StandardType.LongType =>
          ZXmlCodec(
            new ZXmlEncoder[Long] {
              override def encodeXml(value: Long): Elem = <long>{value.toString}</long>
            },
            new ZXmlDecoder[Long] {
              override def decodeXml(node: Node): Either[DecodeError, Long] =
                node.text.toLongOption.toRight(ReadError(Cause.empty, "Invalid Long value"))
            }
          )

        case StandardType.ShortType =>
          ZXmlCodec(
            new ZXmlEncoder[java.lang.Short] {
              override def encodeXml(value: java.lang.Short): Elem = {
                val result = <short>{value.toString}</short>
                result
              }
            },
            new ZXmlDecoder[java.lang.Short] {
              override def decodeXml(node: Node): Either[DecodeError, java.lang.Short] = {
                val receivedText = node.text
                try {
                  val decodedValue = java.lang.Short.valueOf(receivedText)
                  Right(decodedValue)
                } catch {
                  case NonFatal(_) =>
                    Left(ReadError(Cause.empty, s"Invalid Short value: $receivedText"))
                }
              }
            }
          ).asInstanceOf[ZXmlCodec[A]]

        case StandardType.ZonedDateTimeType =>
          ZXmlCodec(
            new ZXmlEncoder[ZonedDateTime] {
              override def encodeXml(value: ZonedDateTime): Elem = <zonedDateTime>{value.toString}</zonedDateTime>
            },
            new ZXmlDecoder[ZonedDateTime] {
              override def decodeXml(node: Node): Either[DecodeError, ZonedDateTime] =
                try {
                  Right(ZonedDateTime.parse(node.text))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, "Invalid ZonedDateTime format"))
                }
            }
          )

        case StandardType.ZoneOffsetType =>
          ZXmlCodec(
            new ZXmlEncoder[ZoneOffset] {
              override def encodeXml(value: ZoneOffset): Elem =
                <ZoneOffset>{value.toString}</ZoneOffset>
            },
            new ZXmlDecoder[ZoneOffset] {
              override def decodeXml(node: Node): Either[DecodeError, ZoneOffset] =
                try {
                  Right(ZoneOffset.of(node.text.trim))
                } catch {
                  case _: Exception =>
                    Left(ReadError(Cause.empty, "Invalid ZoneOffset value"))
                }
            }
          ).asInstanceOf[ZXmlCodec[A]]

        case StandardType.ZoneIdType =>
          ZXmlCodec(
            new ZXmlEncoder[ZoneId] {
              override def encodeXml(value: ZoneId): Elem =
                <ZoneId>{value.toString}</ZoneId>
            },
            new ZXmlDecoder[ZoneId] {
              override def decodeXml(node: Node): Either[DecodeError, ZoneId] =
                try {
                  Right(ZoneId.of(node.text.trim))
                } catch {
                  case _: Exception =>
                    Left(ReadError(Cause.empty, "Invalid ZoneId value"))
                }
            }
          ).asInstanceOf[ZXmlCodec[A]]

        case StandardType.CurrencyType =>
          ZXmlCodec(
            new ZXmlEncoder[Currency] {
              override def encodeXml(value: Currency): Elem =
                <Currency>{value.getCurrencyCode}</Currency>
            },
            new ZXmlDecoder[Currency] {
              override def decodeXml(node: Node): Either[DecodeError, Currency] =
                try {
                  Right(Currency.getInstance(node.text.trim))
                } catch {
                  case _: Exception =>
                    Left(ReadError(Cause.empty, "Invalid Currency value"))
                }
            }
          ).asInstanceOf[ZXmlCodec[A]]

        case StandardType.BinaryType =>
          ZXmlCodec(
            new ZXmlEncoder[Chunk[Byte]] {
              override def encodeXml(value: Chunk[Byte]): Elem = {
                val base64 = java.util.Base64.getEncoder.encodeToString(value.toArray)
                <binary>{base64}</binary>
              }
            },
            new ZXmlDecoder[Chunk[Byte]] {
              override def decodeXml(node: Node): Either[DecodeError, Chunk[Byte]] = {
                val text = node.text.trim
                try {
                  val bytes = java.util.Base64.getDecoder.decode(text)
                  Right(Chunk.fromArray(bytes))
                } catch {
                  case _: Exception =>
                    Left(ReadError(Cause.empty, s"Invalid binary value: $text"))
                }
              }
            }
          ).asInstanceOf[ZXmlCodec[A]]

        case StandardType.CharType =>
          ZXmlCodec(
            new ZXmlEncoder[Char] {
              override def encodeXml(value: Char): Elem =
                <char xml:space="preserve" code={value.toInt.toString}>{value.toString}</char>
            },
            new ZXmlDecoder[Char] {
              override def decodeXml(node: Node): Either[DecodeError, Char] = {
                val charNode = if (node.label == "char") node else (node \ "char").headOption.getOrElse(node)

                val text = charNode.text

                if (text.nonEmpty && text.length == 1) {
                  Right(text.charAt(0))
                } else {
                  // Attempt to recover using the "code" attribute.
                  val codeAttr = (charNode \ "@code").text
                  if (codeAttr.nonEmpty) {
                    try {
                      val recoveredChar = codeAttr.toInt.toChar
                      Right(recoveredChar)
                    } catch {
                      case NonFatal(_) =>
                        val errMsg = s"Invalid Char code: $codeAttr"
                        Left(ReadError(Cause.empty, errMsg))
                    }
                  } else {
                    Right('\u0000')
                  }
                }
              }
            }
          )

        case StandardType.UUIDType =>
          ZXmlCodec(
            new ZXmlEncoder[java.util.UUID] {
              override def encodeXml(value: java.util.UUID): Elem = <uuid>{value.toString}</uuid>
            },
            new ZXmlDecoder[java.util.UUID] {
              override def decodeXml(node: Node): Either[DecodeError, java.util.UUID] =
                try {
                  Right(java.util.UUID.fromString(node.text.trim))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, s"Invalid UUID format: ${node.text.trim}"))
                }
            }
          )

        case StandardType.DayOfWeekType =>
          ZXmlCodec(
            new ZXmlEncoder[java.time.DayOfWeek] {
              override def encodeXml(value: java.time.DayOfWeek): Elem =
                <dayOfWeek>{value.toString}</dayOfWeek>
            },
            new ZXmlDecoder[java.time.DayOfWeek] {
              override def decodeXml(node: Node): Either[DecodeError, java.time.DayOfWeek] =
                try {
                  Right(java.time.DayOfWeek.valueOf(node.text.trim))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, s"Invalid DayOfWeek value: ${node.text.trim}"))
                }
            }
          )

        case StandardType.DurationType =>
          ZXmlCodec(
            new ZXmlEncoder[Duration] {
              override def encodeXml(value: Duration): Elem =
                <duration>{value.toString}</duration>
            },
            new ZXmlDecoder[Duration] {
              override def decodeXml(node: Node): Either[DecodeError, Duration] =
                try {
                  Right(Duration.parse(node.text.trim))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, s"Invalid Duration format: ${node.text.trim}"))
                }
            }
          )

        case StandardType.MonthType =>
          ZXmlCodec(
            new ZXmlEncoder[Month] {
              override def encodeXml(value: Month): Elem = <month>{value.toString}</month>
            },
            new ZXmlDecoder[Month] {
              override def decodeXml(node: Node): Either[DecodeError, Month] =
                try {
                  Right(Month.valueOf(node.text.trim.toUpperCase))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, s"Invalid Month format: ${node.text.trim}"))
                }
            }
          )

        case StandardType.MonthDayType =>
          ZXmlCodec(
            new ZXmlEncoder[MonthDay] {
              override def encodeXml(value: MonthDay): Elem = <monthDay>{value.toString}</monthDay>
            },
            new ZXmlDecoder[MonthDay] {
              override def decodeXml(node: Node): Either[DecodeError, MonthDay] =
                try {
                  Right(MonthDay.parse(node.text.trim))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, s"Invalid MonthDay format: ${node.text.trim}"))
                }
            }
          )

        case StandardType.OffsetTimeType =>
          ZXmlCodec(
            new ZXmlEncoder[OffsetTime] {
              override def encodeXml(value: OffsetTime): Elem = <offsetTime>{value.toString}</offsetTime>
            },
            new ZXmlDecoder[OffsetTime] {
              override def decodeXml(node: Node): Either[DecodeError, OffsetTime] =
                try {
                  Right(OffsetTime.parse(node.text.trim))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, s"Invalid OffsetTime format: ${node.text.trim}"))
                }
            }
          )

        case StandardType.OffsetDateTimeType =>
          ZXmlCodec(
            new ZXmlEncoder[OffsetDateTime] {
              override def encodeXml(value: OffsetDateTime): Elem = <offsetDateTime>{value.toString}</offsetDateTime>
            },
            new ZXmlDecoder[OffsetDateTime] {
              override def decodeXml(node: Node): Either[DecodeError, OffsetDateTime] =
                try {
                  Right(OffsetDateTime.parse(node.text.trim))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, s"Invalid OffsetDateTime format: ${node.text.trim}"))
                }
            }
          )

        case StandardType.LocalTimeType =>
          ZXmlCodec(
            new ZXmlEncoder[LocalTime] {
              override def encodeXml(value: LocalTime): Elem = <localTime>{value.toString}</localTime>
            },
            new ZXmlDecoder[LocalTime] {
              override def decodeXml(node: Node): Either[DecodeError, LocalTime] =
                try {
                  Right(LocalTime.parse(node.text.trim, DateTimeFormatter.ISO_LOCAL_TIME))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, s"Invalid LocalTime format: ${node.text.trim}"))
                }
            }
          )

        case StandardType.PeriodType =>
          ZXmlCodec(
            new ZXmlEncoder[Period] {
              override def encodeXml(value: Period): Elem =
                <period>{value.toString}</period>
            },
            new ZXmlDecoder[Period] {
              override def decodeXml(node: Node): Either[DecodeError, Period] = {
                val text = node.text.trim
                try {
                  Right(Period.parse(text))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, s"Invalid Period format: $text"))
                }
              }
            }
          )

        case StandardType.YearType =>
          ZXmlCodec(
            new ZXmlEncoder[Year] {
              override def encodeXml(value: Year): Elem =
                <year>{value.toString}</year>
            },
            new ZXmlDecoder[Year] {
              override def decodeXml(node: Node): Either[DecodeError, Year] = {
                val text = node.text.trim
                try {
                  Right(Year.parse(text))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, s"Invalid Year format: $text"))
                }
              }
            }
          )

        case StandardType.YearMonthType =>
          ZXmlCodec(
            new ZXmlEncoder[YearMonth] {
              override def encodeXml(value: YearMonth): Elem =
                <yearMonth>{value.toString}</yearMonth>
            },
            new ZXmlDecoder[YearMonth] {
              override def decodeXml(node: Node): Either[DecodeError, YearMonth] = {
                val text = node.text.trim
                try {
                  Right(YearMonth.parse(text))
                } catch {
                  case NonFatal(_) => Left(ReadError(Cause.empty, s"Invalid YearMonth format: $text"))
                }
              }
            }
          )

        case _ =>
          ZXmlCodec(
            new ZXmlEncoder[A] {
              override def encodeXml(value: A): Elem = <primitive>{value.toString}</primitive>
            },
            failDecoder[A]("Unsupported primitive type")
          )
      }
  }

  object XmlFieldEncoder {

    // Basic field encoders for primitive types
    val string: XmlFieldEncoder[String] = new XmlFieldEncoder[String] {
      override def unsafeEncodeField(a: String): String = a
    }

    val long: XmlFieldEncoder[Long] = new XmlFieldEncoder[Long] {
      override def unsafeEncodeField(a: Long): String = a.toString
    }

    val int: XmlFieldEncoder[Int] = new XmlFieldEncoder[Int] {
      override def unsafeEncodeField(a: Int): String = a.toString
    }

    // A helper to build a map field encoder if keys have a field encoder.
    def map[K, V](keyEncoder: XmlFieldEncoder[K], valueEncoder: ZXmlEncoder[V]): ZXmlEncoder[Map[K, V]] =
      new ZXmlEncoder[Map[K, V]] {
        override def encodeXml(value: Map[K, V]): Elem = {
          val entries = value.toSeq.map {
            case (k, v) =>
              <entry>
            <key>{keyEncoder.unsafeEncodeField(k)}</key>
            <value>{valueEncoder.encodeXml(v)}</value>
          </entry>
          }
          <map>{entries}</map>
        }
      }

  }

// -----------------------------------------------------------------------------
// XML Collection Encoder Helpers (for chunking map keys if no field encoder available)
// -----------------------------------------------------------------------------
  object XmlCollectionEncoder {

    // A very basic implementation that decodes a collection of (K, V) pairs.
    def chunk[A](elementEncoder: ZXmlEncoder[A]): ZXmlEncoder[Seq[A]] =
      new ZXmlEncoder[Seq[A]] {
        override def encodeXml(value: Seq[A]): Elem = {
          val elements = value.map(elementEncoder.encodeXml)
          <seq>{elements}</seq>
        }
      }

    def set[A](elementEncoder: ZXmlEncoder[A]): ZXmlEncoder[Set[A]] =
      new ZXmlEncoder[Set[A]] {
        override def encodeXml(value: Set[A]): Elem = {
          val elements = value.toSeq.map(elementEncoder.encodeXml)
          <set>{elements}</set>
        }
      }
  }

  object XmlTupleEncoder {

    def tuple2[A, B](left: ZXmlEncoder[A], right: ZXmlEncoder[B]): ZXmlEncoder[(A, B)] =
      new ZXmlEncoder[(A, B)] {
        override def encodeXml(value: (A, B)): Elem = {
          val (a, b)   = value
          val leftXml  = left.encodeXml(a)
          val rightXml = right.encodeXml(b)
          val tupleXml =
            <tuple>
            <left>{leftXml}</left>
            <right>{rightXml}</right>
          </tuple>
          tupleXml
        }
      }
  }

  object XmlEitherEncoder {

    def either[A, B](left: ZXmlEncoder[A], right: ZXmlEncoder[B]): ZXmlEncoder[Either[A, B]] =
      new ZXmlEncoder[Either[A, B]] {
        override def encodeXml(value: Either[A, B]): Elem = value match {
          case Left(a) =>
            val encodedLeft = left.encodeXml(a)
            <left>{encodedLeft}</left>
          case Right(b) =>
            val encodedRight = right.encodeXml(b)
            <right>{encodedRight}</right>
        }
      }
  }

  // helper objects for decoding logic
  object XmlTupleDecoder {

    def unwrapTo(node: Node, expected: String): Node =
      if (node.label == expected) node
      else {
        node.child.collect { case e: Elem => e }.find(_.label == expected) match {
          case Some(child) => unwrapTo(child, expected)
          case None        => node
        }
      }

    def tuple2[A, B](leftDecoder: ZXmlDecoder[A], rightDecoder: ZXmlDecoder[B]): ZXmlDecoder[(A, B)] =
      new ZXmlDecoder[(A, B)] {
        override def decodeXml(node: Node): Either[DecodeError, (A, B)] = {
          val tupleNode = unwrapTo(node, "tuple")
          if (tupleNode.label != "tuple") {
            val err = ReadError(Cause.empty, s"Expected <tuple> element, found <${tupleNode.label}>")
            Left(err)
          } else {
            // Unwrap left and right parts.
            val leftOpt  = (tupleNode \ "left").headOption.map(unwrapTo(_, "left"))
            val rightOpt = (tupleNode \ "right").headOption.map(unwrapTo(_, "right"))
            (leftOpt, rightOpt) match {
              case (Some(lNode), Some(rNode)) =>
                val decodedLeft  = leftDecoder.decodeXml(lNode)
                val decodedRight = rightDecoder.decodeXml(rNode)
                for {
                  a <- decodedLeft
                  b <- decodedRight
                } yield (a, b)
              case _ =>
                val err = ReadError(Cause.empty, "Missing <left> or <right> element in tuple")
                Left(err)
            }
          }
        }
      }
  }

  object XmlCollectionDecoder {

    private def sequence[A](eithers: Seq[Either[DecodeError, A]]): Either[DecodeError, Seq[A]] =
      eithers.foldRight(Right(Nil): Either[DecodeError, List[A]]) { (elem, acc) =>
        for {
          xs <- acc
          x  <- elem
        } yield x :: xs
      }

    def chunk[A](elementDecoder: ZXmlDecoder[A]): ZXmlDecoder[Seq[A]] =
      new ZXmlDecoder[Seq[A]] {
        override def decodeXml(node: Node): Either[DecodeError, Seq[A]] = {
          val seqNode =
            if (node.label != "seq") {
              node.child.collect { case e: Elem => e }.headOption.filter(_.label == "seq").getOrElse(node)
            } else node
          if (seqNode.label != "seq")
            Left(ReadError(Cause.empty, s"Expected <seq> element, found <${node.label}>"))
          else {
            val children: Seq[Elem] = node.child.collect { case e: Elem => e }.toList
            sequence(children.map { child =>
              val result = elementDecoder.decodeXml(child)
              result
            })
          }
        }
      }

    def set[A](elementDecoder: ZXmlDecoder[A]): ZXmlDecoder[Set[A]] =
      new ZXmlDecoder[Set[A]] {
        override def decodeXml(node: Node): Either[DecodeError, Set[A]] =
          if (node.label != "set")
            Left(ReadError(Cause.empty, s"Expected <set> element, found <${node.label}>"))
          else {
            val children: Seq[Elem] = node.child.collect { case e: Elem => e }.toList
            sequence(children.map { child =>
              val result = elementDecoder.decodeXml(child)
              result
            }).map { seq =>
              val setValue = seq.toSet
              setValue
            }
          }
      }
  }

  object XmlFieldDecoder {

    val string: XmlFieldDecoder[String] = (node: Node) => Right(node.text)

    val int: XmlFieldDecoder[Int] = (node: Node) => node.text.toIntOption.toRight(ReadError(Cause.empty, "Invalid Int"))

    val long: XmlFieldDecoder[Long] = (node: Node) =>
      node.text.toLongOption.toRight(ReadError(Cause.empty, "Invalid Long"))

    def map[K, V](
      keyDecoder: XmlFieldDecoder[K],
      valueDecoder: XmlFieldDecoder[V]
    ): XmlFieldDecoder[Map[K, V]] = (node: Node) => {
      node.label match {
        case "record" =>
          val fields: Seq[Elem] = node.child.collect { case e: Elem if e.label == "field" => e }.toList
          val parsed: Seq[Either[DecodeError, (K, V)]] = fields.map { field =>
            field.attribute("name").flatMap(_.headOption) match {
              case Some(attr) =>
                val keyNode = scala.xml.Text(attr.text)
                for {
                  key <- keyDecoder.decodeField(keyNode)
                  childElem <- field.child.collect { case e: Elem => e }.headOption.toRight(
                                ReadError(Cause.empty, "Missing value element in <field>")
                              )
                  value <- valueDecoder.decodeField(childElem)
                } yield key -> value
              case None =>
                val errorMsg = "Missing 'name' attribute on <field> for key"
                Left(ReadError(Cause.empty, errorMsg))
            }
          }
          sequence(parsed).map(_.toMap)

        case _ =>
          val entries: Seq[Elem] = node.child.collect { case e: Elem => e }.toList
          val parsed: Seq[Either[DecodeError, (K, V)]] = entries.map { entry =>
            for {
              keyNode <- (entry \ "key").headOption
                          .orElse((entry \\ "key").headOption)
                          .toRight(ReadError(Cause.empty, "Missing <key> node"))
              valueNode <- (entry \ "value").headOption
                            .orElse((entry \\ "value").headOption)
                            .toRight(ReadError(Cause.empty, "Missing <value> node"))
              key   <- keyDecoder.decodeField(keyNode)
              value <- valueDecoder.decodeField(valueNode)
            } yield key -> value
          }
          sequence(parsed).map(_.toMap)
      }
    }

    private def sequence[A](eithers: Seq[Either[DecodeError, A]]): Either[DecodeError, Seq[A]] =
      eithers
        .foldRight(Right(Nil): Either[DecodeError, List[A]]) { (elem, acc) =>
          for {
            xs <- acc
            x  <- elem
          } yield x :: xs
        }
        .map(_.reverse)

    def fromZXmlDecoder[V](decoder: ZXmlDecoder[V]): XmlFieldDecoder[V] =
      (node: Node) => decoder.decodeXml(node)

    def asZXmlDecoder[A](fieldDecoder: XmlFieldDecoder[A]): ZXmlDecoder[A] =
      new ZXmlDecoder[A] {
        override def decodeXml(node: Node): Either[DecodeError, A] =
          fieldDecoder.decodeField(node)
      }
  }

  object XmlEitherDecoder {

    def either[A, B](left: ZXmlDecoder[A], right: ZXmlDecoder[B]): ZXmlDecoder[Either[A, B]] =
      new ZXmlDecoder[Either[A, B]] {
        override def decodeXml(node: Node): Either[DecodeError, Either[A, B]] =
          node.label match {
            case "left" =>
              val childOpt = (node \ "_").headOption
              childOpt match {
                case Some(child) =>
                  val decodedLeft = left.decodeXml(child)
                  decodedLeft.map(Left(_))
                case None =>
                  val err = ReadError(Cause.empty, "Missing content in <left> element")
                  Left(err)
              }
            case "right" =>
              val childOpt = (node \ "_").headOption
              childOpt match {
                case Some(child) =>
                  val decodedRight = right.decodeXml(child)
                  decodedRight.map(Right(_))
                case None =>
                  val err = ReadError(Cause.empty, "Missing content in <right> element")
                  Left(err)
              }
            case other =>
              val err = ReadError(Cause.empty, s"Expected <left> or <right> element, but found <$other>")
              Left(err)
          }
      }
  }

  private def unLazy[A](schema: Schema[A]): Schema[A] =
    schema match {
      case l: Schema.Lazy[A] => unLazy(l.schema)
      case _                 => schema
    }

  object XmlEncoder {

    import Codecs._
    import ProductEncoder._

    // --------------------------
    // EncoderKey for XML
    // --------------------------
    private case class EncoderKey[A](schema: Schema[A], cfg: Config, discriminatorTuple: DiscriminatorTuple) {
      override val hashCode: Int = System.identityHashCode(schema) ^ cfg.hashCode ^ discriminatorTuple.hashCode
      override def equals(obj: Any): Boolean = obj match {
        case ek: EncoderKey[_] => (ek.schema eq schema) && ek.cfg == cfg && ek.discriminatorTuple == discriminatorTuple
        case _                 => false
      }
    }

    // --------------------------
    // Charset and Cache
    // --------------------------
    private[codec] val CHARSET = StandardCharsets.UTF_8
    private[this] val encoders = new ConcurrentHashMap[EncoderKey[_], ZXmlEncoder[_]]()

    // --------------------------
    // Top-level encode method
    // --------------------------
    final def encode[A](schema: Schema[A], value: A, cfg: Config): Chunk[Byte] =
      charSequenceToByteChunk(schemaEncoder(schema, cfg).encodeXml(value).toString())

    private[codec] def charSequenceToByteChunk(chars: CharSequence): Chunk[Byte] = {
      val bytes = CHARSET.newEncoder().encode(CharBuffer.wrap(chars))
      Chunk.fromByteBuffer(bytes)
    }

    // --------------------------
    // Schema-based encoder retrieval
    // --------------------------
    private[codec] def schemaEncoder[A](
      schema: Schema[A],
      cfg: Config,
      discriminatorTuple: DiscriminatorTuple = None
    ): ZXmlEncoder[A] = {
      val key     = EncoderKey(schema, cfg, discriminatorTuple)
      var encoder = encoders.get(key).asInstanceOf[ZXmlEncoder[A]]
      if (encoder eq null) {
        encoder = schemaEncoderSlow(schema, cfg, discriminatorTuple)
        encoders.put(key, encoder)
      }
      encoder
    }

    private[codec] def option[A](encoder: ZXmlEncoder[A]): ZXmlEncoder[Option[A]] =
      new ZXmlEncoder[Option[A]] {
        override def encodeXml(value: Option[A]): Elem = value match {
          case Some(v) => encoder.encodeXml(v) // Encode the value if present
          case None    => <null/>              // Represent `None` as `<null/>`
        }
      }

    private[this] def schemaEncoderSlow[A](
      schema: Schema[A],
      cfg: Config,
      discriminatorTuple: DiscriminatorTuple
    ): ZXmlEncoder[A] =
      schema match {
        case Schema.Primitive(standardType, _) =>
          primitiveCodec(standardType).encoder

        case Schema.Optional(inner, _) =>
          XmlEncoder.option(schemaEncoder(inner, cfg))

        case Schema.Tuple2(l, r, _) =>
          XmlTupleEncoder.tuple2(schemaEncoder(l, cfg), schemaEncoder(r, cfg))

        case Schema.Sequence(elementSchema, _, g, _, _) =>
          XmlCollectionEncoder.chunk(schemaEncoder(elementSchema, cfg)).contramap(g)

        case Schema.NonEmptySequence(elementSchema, _, g, _, _) =>
          XmlCollectionEncoder.chunk(schemaEncoder(elementSchema, cfg)).contramap(g)

        case Schema.Map(ks, vs, _) =>
          XmlEncoder.mapEncoder(ks, vs, cfg)

        case Schema.NonEmptyMap(ks, vs, _) =>
          XmlEncoder.mapEncoder(ks, vs, cfg).contramap(_.toMap)

        case Schema.Set(sch, _) =>
          XmlCollectionEncoder.set(schemaEncoder(sch, cfg))

        case Schema.Transform(c, _, g, a, _) =>
          transformEncoder(a.foldLeft(c)((s, a) => s.annotate(a)), g, cfg, discriminatorTuple)

        case Schema.Fail(_, _) =>
          unitEncoder.contramap(_ => ())

        case Schema.Either(left, right, _) =>
          XmlEitherEncoder.either(schemaEncoder(left, cfg), schemaEncoder(right, cfg))

        case Schema.Fallback(left, right, _, _) =>
          fallbackEncoder(schemaEncoder(left, cfg), schemaEncoder(right, cfg), cfg)

        case l @ Schema.Lazy(_) =>
          XmlEncoder.suspend(schemaEncoder(l.schema, cfg))

        case s: Schema.GenericRecord =>
          recordEncoder(s, cfg, discriminatorTuple)

        case s: Schema.Record[A] =>
          caseClassEncoder(s, cfg, discriminatorTuple)

        case s: Schema.Enum[A] =>
          enumEncoder(s, cfg)

        case d @ Schema.Dynamic(_) =>
          dynamicEncoder(d, cfg)

        case null =>
          throw new Exception(s"A captured schema is null, most likely due to wrong field initialization order")
      }

    private[codec] def transformFieldEncoder[A, B](
      schema: Schema[A],
      g: B => Either[String, A]
    ): Option[XmlFieldEncoder[B]] =
      xmlFieldEncoder(schema).map { fieldEncoder =>
        new XmlFieldEncoder[B] {
          override def unsafeEncodeField(b: B): String =
            g(b) match {
              case Right(a)  => fieldEncoder.unsafeEncodeField(a)
              case Left(err) => throw new RuntimeException(s"Failed to encode field $b: $err")
            }
        }
      }

    private[codec] def xmlFieldEncoder[A](schema: Schema[A]): Option[XmlFieldEncoder[A]] =
      schema match {
        case Schema.Primitive(StandardType.StringType, _) => Some(XmlFieldEncoder.string)
        case Schema.Primitive(StandardType.LongType, _)   => Some(XmlFieldEncoder.long)
        case Schema.Primitive(StandardType.IntType, _)    => Some(XmlFieldEncoder.int)
        case Schema.Transform(c, f, g, a, _) =>
          transformFieldEncoder(a.foldLeft(c)((s, a) => s.annotate(a)), g)
        case Schema.Lazy(inner) => xmlFieldEncoder(inner())
        case _                  => None
      }

    // -----------------------------------------------------------------------------
// Map Encoder
// -----------------------------------------------------------------------------
    private[codec] def mapEncoder[K, V](ks: Schema[K], vs: Schema[V], cfg: Config): ZXmlEncoder[Map[K, V]] = {
      val valueEncoder: ZXmlEncoder[V] = schemaEncoder(vs, cfg)
      xmlFieldEncoder(ks) match {
        case Some(fieldEncoder) =>
          XmlFieldEncoder.map(fieldEncoder, valueEncoder)
        case None =>
          val pairEncoder: ZXmlEncoder[(K, V)] =
            new ZXmlEncoder[(K, V)] {
              override def encodeXml(pair: (K, V)): Elem = {
                val (k, v) = pair
                <entry>
              <key>{schemaEncoder(ks, cfg).encodeXml(k)}</key>
              <value>{valueEncoder.encodeXml(v)}</value>
            </entry>
              }
            }
          XmlCollectionEncoder
            .chunk(pairEncoder)
            .contramap[Map[K, V]](m => m.toSeq)
      }
    }

    // -----------------------------------------------------------------------------
// Dynamic Encoder for XML
// -----------------------------------------------------------------------------
    private[codec] def dynamicEncoder(schema: Schema.Dynamic, cfg: Config): ZXmlEncoder[DynamicValue] =
      if (schema.annotations.exists(_.isInstanceOf[directDynamicMapping])) {
        new ZXmlEncoder[DynamicValue] {
          override def encodeXml(value: DynamicValue): Elem = {
            def formatXml(elem: Elem): Elem =
              if (cfg.prettyPrint)
                scala.xml.XML.loadString(new scala.xml.PrettyPrinter(80, 2).format(elem))
              else elem

            value match {
              case DynamicValue.Record(_, values) =>
                formatXml {
                  if (values.isEmpty)
                    <record/>
                  else
                    <record>{
                      values.map {
                        case (key, v) =>
                          Elem(
                            prefix = null,
                            label = key,
                            attributes = Null,
                            scope = TopScope,
                            minimizeEmpty = true,
                            child = schemaEncoder(DynamicValue.schema, cfg).encodeXml(v)
                          )
                      }.toSeq
                    }</record>
                }
              case DynamicValue.Enumeration(_, _) =>
                throw new Exception("DynamicValue.Enumeration is not supported in directDynamicMapping mode")
              case DynamicValue.Sequence(values) =>
                formatXml {
                  <sequence>{
                    values.map(v => schemaEncoder(DynamicValue.schema, cfg).encodeXml(v)).toSeq
                  }</sequence>
                }
              case DynamicValue.Dictionary(_) =>
                throw new Exception("DynamicValue.Dictionary is not supported in directDynamicMapping mode")
              case DynamicValue.SetValue(values) =>
                // Encode as a set.
                formatXml { XmlCollectionEncoder.set(schemaEncoder(DynamicValue.schema, cfg)).encodeXml(values) }
              case DynamicValue.Primitive(value, standardType) =>
                formatXml { primitiveCodec(standardType).encoder.encodeXml(value) }
              case DynamicValue.Singleton(_) =>
                formatXml { <singleton/> }
              case DynamicValue.SomeValue(value) =>
                formatXml { schemaEncoder(DynamicValue.schema, cfg).encodeXml(value) }
              case DynamicValue.NoneValue =>
                formatXml { <null/> }
              case DynamicValue.Tuple(_, _) =>
                throw new Exception("DynamicValue.Tuple is not supported in directDynamicMapping mode")
              case DynamicValue.LeftValue(_) =>
                throw new Exception("DynamicValue.LeftValue is not supported in directDynamicMapping mode")
              case DynamicValue.RightValue(_) =>
                throw new Exception("DynamicValue.RightValue is not supported in directDynamicMapping mode")
              case DynamicValue.BothValue(left, right) =>
                formatXml {
                  <both>
            <left>{schemaEncoder(DynamicValue.schema, cfg).encodeXml(left)}</left>
            <right>{schemaEncoder(DynamicValue.schema, cfg).encodeXml(right)}</right>
          </both>
                }
              case DynamicValue.DynamicAst(_) =>
                throw new Exception("DynamicValue.DynamicAst is not supported in directDynamicMapping mode")
              case DynamicValue.Error(message) =>
                throw new Exception(message)
            }
          }
        }
      } else {
        schemaEncoder(DynamicValue.schema, cfg)
      }

    private def transformEncoder[A, B](
      schema: Schema[A],
      g: B => Either[String, A],
      cfg: Config,
      discriminatorTuple: DiscriminatorTuple
    ): ZXmlEncoder[B] =
      new ZXmlEncoder[B] {
        private lazy val innerEncoder = schemaEncoder(schema, cfg, discriminatorTuple)
        override def encodeXml(b: B): Elem =
          g(b) match {
            case Right(a)  => innerEncoder.encodeXml(a)
            case Left(err) => <error message={err}/>
          }
      }

    private def suspend[A](encoder: => ZXmlEncoder[A]): ZXmlEncoder[A] =
      new ZXmlEncoder[A] {
        lazy val e                             = encoder
        override def encodeXml(value: A): Elem = e.encodeXml(value)
      }

    private def enumEncoder[Z](schema: Schema.Enum[Z], cfg: Config): ZXmlEncoder[Z] = {
      import scala.collection.mutable

      val caseNameAliases = mutable.HashMap[String, Schema.Case[Z, Any]]()
      schema.nonTransientCases.foreach { case_ =>
        val schemaCase = case_.asInstanceOf[Schema.Case[Z, Any]]
        caseNameAliases.put(case_.caseName, schemaCase)
        schemaCase.caseNameAliases.foreach(a => caseNameAliases.put(a, schemaCase))
      }

      // Helper function to apply pretty printing if enabled
      def formatXml(elem: Elem): Elem =
        if (cfg.prettyPrint) scala.xml.XML.loadString(new scala.xml.PrettyPrinter(80, 2).format(elem))
        else elem

      if (schema.annotations.exists(_.isInstanceOf[simpleEnum])) {
        // Encode simple enums as text values
        new ZXmlEncoder[Z] {
          override def encodeXml(a: Z): Elem = formatXml {
            val caseMap: Map[Z, String] = caseNameAliases.map {
              case (name, sc) =>
                sc.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct() -> name
            }.toMap
            <enum>{caseMap(a)}</enum>
          }
        }
      } else if (schema.annotations.exists(_.isInstanceOf[noDiscriminator])) {
        new ZXmlEncoder[Z] {
          private def formatXml(elem: Elem): Elem =
            if (cfg.prettyPrint)
              scala.xml.XML.loadString(new scala.xml.PrettyPrinter(80, 2).format(elem))
            else elem

          override def encodeXml(a: Z): Elem = formatXml {
            val possible = schema.nonTransientCases.iterator.collectFirst {
              case sc if sc.isCase(a) =>
                schemaEncoder(sc.schema.asInstanceOf[Schema[Any]], cfg, None)
                  .encodeXml(sc.deconstruct(a).asInstanceOf[Any])
            }
            possible.getOrElse(scala.xml.Elem(null, "enum", scala.xml.Null, scala.xml.TopScope, minimizeEmpty = true))
          }
        }
      } else {
        // Otherwise, use a discriminator attribute.
        val discriminatorName: Option[String] =
          if (schema.noDiscriminator) None
          else schema.annotations.collectFirst { case d: discriminatorName => d.tag }

        new ZXmlEncoder[Z] {
          override def encodeXml(a: Z): Elem = formatXml {
            var idx   = 0
            val cases = schema.nonTransientCases.toArray
            while (idx < cases.length) {
              val sc = cases(idx)
              if (sc.isCase(a)) {
                // Build a discriminator tuple if a discriminator name is defined.
                val discrTuple =
                  discriminatorName.map(tag => (tag, sc.caseName))
                val innerEncoded =
                  schemaEncoder(sc.schema.asInstanceOf[Schema[Any]], cfg, discrTuple).encodeXml(sc.deconstruct(a))

                return formatXml(
                  Elem(
                    prefix = cfg.namespace.map(_ => "ns").orNull,
                    label = "enum",
                    attributes = new UnprefixedAttribute(discriminatorName.getOrElse("type"), sc.caseName, Null),
                    scope = cfg.namespace.map(ns => NamespaceBinding("ns", ns, TopScope)).getOrElse(TopScope),
                    minimizeEmpty = true,
                    child = innerEncoded
                  )
                )
              }
              idx += 1
            }
            // If no case matches, return an empty <enum/>.
            <enum/>
          }
        }
      }
    }

    private def fallbackEncoder[A, B](
      left: ZXmlEncoder[A],
      right: ZXmlEncoder[B],
      cfg: Config
    ): ZXmlEncoder[Fallback[A, B]] =
      new ZXmlEncoder[Fallback[A, B]] {

        def formatXml(elem: Elem): Elem =
          if (cfg.prettyPrint) scala.xml.XML.loadString(new scala.xml.PrettyPrinter(80, 2).format(elem))
          else
            scala.xml.Utility.trim(elem) match {
              case e: Elem => e
              case other   => throw new Exception(s"Expected an Elem after trimming, got: $other")
            }

        override def encodeXml(f: Fallback[A, B]): Elem = formatXml {
          f match {
            case Fallback.Left(a) =>
              <fallback type="left">
            {left.encodeXml(a)}
          </fallback>
            case Fallback.Right(b) =>
              <fallback type="right">
            {right.encodeXml(b)}
          </fallback>
            case Fallback.Both(a, b) =>
              <fallback type="both">
            <left>{left.encodeXml(a)}</left>
            <right>{right.encodeXml(b)}</right>
          </fallback>
          }
        }
      }

    private def recordEncoder(
      schema: Schema.GenericRecord,
      cfg: Config,
      discriminatorTuple: DiscriminatorTuple
    ): ZXmlEncoder[ListMap[String, Any]] = {
      val nonTransientFields = schema.nonTransientFields.toArray

      if (nonTransientFields.isEmpty) {
        new ZXmlEncoder[ListMap[String, Any]] {
          override def encodeXml(value: ListMap[String, Any]): Elem = <record/>
        }
      } else {
        val encoders = nonTransientFields.map(field => schemaEncoder(field.schema.asInstanceOf[Schema[Any]], cfg))

        new ZXmlEncoder[ListMap[String, Any]] {
          def formatXml(elem: Elem): Elem =
            if (cfg.prettyPrint) scala.xml.XML.loadString(new scala.xml.PrettyPrinter(80, 2).format(elem))
            else elem

          override def encodeXml(fields: ListMap[String, Any]): Elem = formatXml {
            val children: Seq[Elem] = nonTransientFields.zipWithIndex.flatMap {
              case (field, idx) =>
                val name  = field.fieldName
                val value = fields(name)

                if (isEmptyOptionalValue(field, value, cfg) || (encoders(idx).isNothing(value) && !cfg.explicitNulls))
                  None
                else
                  Some(
                    <field name={name}>
                {schemaEncoder(field.schema.asInstanceOf[Schema[Any]], cfg).encodeXml(value)}
              </field>
                  )
            }

            val attributes = discriminatorTuple match {
              case Some((discName, discValue)) =>
                new UnprefixedAttribute(discName, discValue, Null)
              case None => Null
            }

            val attributeMeta: MetaData = cfg.attributes.foldLeft(attributes) { (meta, attr) =>
              fields
                .get(attr)
                .flatMap {
                  case None        => None // Skip None values
                  case Some(value) => Some(new UnprefixedAttribute(attr, value.toString, meta))
                  case value       => Some(new UnprefixedAttribute(attr, value.toString, meta))
                }
                .getOrElse(meta)
            }

            // Create the final record element.
            Elem(
              prefix = cfg.namespace.map(_ => "ns").orNull,
              label = "record",
              attributes = attributeMeta,
              scope = cfg.namespace.map(ns => NamespaceBinding("ns", ns, TopScope)).getOrElse(TopScope),
              minimizeEmpty = true,
              child = children: _*
            )
          }
        }
      }
    }

  }

  object XmlDecoder {

    import Codecs._
    import XmlFieldDecoder._
    import ProductDecoder._

    // --- Caching mechanism ---
    private case class DecoderKey[A](schema: Schema[A], discriminator: Option[String]) {
      override val hashCode: Int = System.identityHashCode(schema) ^ discriminator.hashCode
      override def equals(obj: Any): Boolean = obj match {
        case dk: DecoderKey[_] => (dk.schema eq schema) && dk.discriminator == discriminator
        case _                 => false
      }
    }
    private[this] val decoders = new ConcurrentHashMap[DecoderKey[_], ZXmlDecoder[_]]()

    def filterNodes(node: scala.xml.Node): scala.xml.Node = node match {
      case elem: scala.xml.Elem =>
        val filteredChildren = elem.child.flatMap {
          case t: scala.xml.Text if t.text.trim.nonEmpty => Seq(t)
          case _: scala.xml.Text                         => Seq.empty
          case e: scala.xml.Elem                         => Seq(filterNodes(e))
          case other                                     => Seq(other)
        }
        elem.copy(child = filteredChildren)
      case other => other
    }

    // --- Top-level XML decode method ---
    final def decode[A](schema: Schema[A], xml: String): Either[DecodeError, A] =
      try {
        val node: scala.xml.Node = scala.xml.XML.loadString(xml)
        // Retrieve a decoder based on the schema.
        schemaDecoder(schema).decodeXml(node) match {
          case Left(err)    => Left(ReadError(Cause.empty, err.toString))
          case Right(value) => Right(value)
        }
      } catch {
        case NonFatal(e) => Left(ReadError(Cause.fail(e), e.getMessage))
      }

    // --- Option Decoder for XML ---

    private[codec] def option[A](decoder: ZXmlDecoder[A]): ZXmlDecoder[Option[A]] =
      new ZXmlDecoder[Option[A]] {
        override def decodeXml(node: Node): Either[DecodeError, Option[A]] =
          node.label match {
            case "null" | "empty" => Right(None)
            case _                => decoder.decodeXml(node).map(Some(_))
          }
      }

    // --- Schema-based decoder retrieval with caching ---
    private[codec] def schemaDecoder[A](schema: Schema[A], discriminator: Option[String] = None): ZXmlDecoder[A] = {
      val key     = DecoderKey(schema, discriminator)
      var decoder = decoders.get(key).asInstanceOf[ZXmlDecoder[A]]
      if (decoder eq null) {
        decoder = schemaDecoderSlow(schema, discriminator)
        decoders.put(key, decoder)
      }
      decoder
    }

    // --- Schema-based decoder derivation ---
    private def schemaDecoderSlow[A](schema: Schema[A], discriminator: Option[String]): ZXmlDecoder[A] = schema match {
      case Schema.Primitive(standardType, _) =>
        primitiveCodec(standardType).decoder
      case Schema.Optional(codec, _) =>
        option(schemaDecoder(codec))
      case Schema.Tuple2(left, right, _) =>
        XmlTupleDecoder.tuple2(schemaDecoder(left), schemaDecoder(right))
      case Schema.Transform(c, f, _, a, _) =>
        schemaDecoder(a.foldLeft(c)((s, a) => s.annotate(a)), discriminator).mapOrFail(f)
      case Schema.Sequence(codec, f, _, _, _) =>
        XmlCollectionDecoder.chunk(schemaDecoder(codec)).map(seq => f(zio.Chunk.fromIterable(seq)))
      case s @ Schema.NonEmptySequence(codec, _, _, _, _) =>
        XmlCollectionDecoder.chunk(schemaDecoder(codec)).map(seq => s.fromChunk(zio.Chunk.fromIterable(seq)))
      case Schema.Map(ks, vs, _) =>
        mapDecoder(ks, vs)
      case Schema.NonEmptyMap(ks, vs, _) =>
        mapDecoder(ks, vs).mapOrFail(m => NonEmptyMap.fromMapOption(m).toRight("NonEmptyMap expected"))
      case Schema.Set(s, _) =>
        XmlCollectionDecoder.set(schemaDecoder(s))
      case Schema.Fail(message, _) =>
        failDecoder(message)
      case Schema.Either(left, right, _) =>
        XmlEitherDecoder.either(schemaDecoder(left), schemaDecoder(right))
      case s @ Schema.Fallback(_, _, _, _) => fallbackDecoder(s)

      case l @ Schema.Lazy(_) =>
        suspend(schemaDecoder(l.schema))
      case s: Schema.GenericRecord =>
        recordDecoder(s, discriminator)
      case s: Schema.Enum[A] =>
        enumDecoder(s)
      case s @ Schema.CaseClass0(_, _, _) =>
        caseClass0Decoder(discriminator, s)
      case s @ Schema.CaseClass1(_, _, _, _) =>
        caseClass1Decoder(discriminator, s)
      case s @ Schema.CaseClass2(_, _, _, _, _) =>
        caseClass2Decoder(discriminator, s)
      case s @ Schema.CaseClass3(_, _, _, _, _, _) =>
        caseClass3Decoder(discriminator, s)
      case s @ Schema.CaseClass4(_, _, _, _, _, _, _) =>
        caseClass4Decoder(discriminator, s)
      case s @ Schema.CaseClass5(_, _, _, _, _, _, _, _) =>
        caseClass5Decoder(discriminator, s)
      case s @ Schema.CaseClass6(_, _, _, _, _, _, _, _, _) =>
        caseClass6Decoder(discriminator, s)
      case s @ Schema.CaseClass7(_, _, _, _, _, _, _, _, _, _) =>
        caseClass7Decoder(discriminator, s)
      case s @ Schema.CaseClass8(_, _, _, _, _, _, _, _, _, _, _) =>
        caseClass8Decoder(discriminator, s)
      case s @ Schema.CaseClass9(_, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass9Decoder(discriminator, s)
      case s @ Schema.CaseClass10(_, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass10Decoder(discriminator, s)
      case s @ Schema.CaseClass11(_, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass11Decoder(discriminator, s)
      case s @ Schema.CaseClass12(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass12Decoder(discriminator, s)
      case s @ Schema.CaseClass13(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass13Decoder(discriminator, s)
      case s @ Schema.CaseClass14(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
        caseClass14Decoder(discriminator, s)
      case s @ Schema.CaseClass15(_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) =>
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

      case d @ Schema.Dynamic(_) => dynamicDecoder(d)
      case _ =>
        throw new Exception(s"Missing a handler for decoding of schema ${schema.toString()}.")
    }

// --- XML Field Decoder ---
    private[codec] def xmlFieldDecoder[A](schema: Schema[A]): Option[XmlFieldDecoder[A]] =
      schema match {
        case Schema.Primitive(StandardType.StringType, _) => Some(XmlFieldDecoder.string)
        case Schema.Primitive(StandardType.LongType, _)   => Some(XmlFieldDecoder.long)
        case Schema.Primitive(StandardType.IntType, _)    => Some(XmlFieldDecoder.int)
        case Schema.Transform(c, f, _, a, _) =>
          xmlFieldDecoder(a.foldLeft(c)((s, a) => s.annotate(a))).map(_.mapOrFail(f))
        case Schema.Lazy(inner) => xmlFieldDecoder(inner())
        case _                  => None
      }

// --- XML Map Decoder ---

    private def mapDecoder[K, V](ks: Schema[K], vs: Schema[V]): ZXmlDecoder[Map[K, V]] = {
      val valueDecoder = XmlDecoder.schemaDecoder(vs)
      xmlFieldDecoder(ks) match {
        case Some(fieldDecoder) =>
          asZXmlDecoder(
            XmlFieldDecoder.map(fieldDecoder, XmlFieldDecoder.fromZXmlDecoder(valueDecoder))
          )
        case None =>
          new ZXmlDecoder[Map[K, V]] {
            override def decodeXml(node: Node): Either[DecodeError, Map[K, V]] = {
              val entries: Seq[Elem] = node.label match {
                case "seq" =>
                  node.child.collect { case e: Elem => e }.toList
                case "map" | "record" =>
                  node.child.collect { case e: Elem if e.label == "entry" => e }.toList
                case other =>
                  return Left(ReadError(Cause.empty, s"Unexpected container element <$other> for map decoding"))
              }
              val decodedEntries: Seq[Either[DecodeError, (K, V)]] = entries.map { entry =>
                for {
                  keyContainer <- (entry \ "key").headOption
                                   .orElse((entry \\ "key").headOption)
                                   .toRight(ReadError(Cause.empty, "Missing <key> node"))
                  recordKey = keyContainer.label match {
                    case "record" => keyContainer
                    case _ =>
                      keyContainer.child.collect { case e: Elem if e.label == "record" => e }.headOption
                        .orElse((keyContainer \\ "record").headOption)
                        .getOrElse(keyContainer)
                  }
                  key <- XmlDecoder.schemaDecoder(ks).decodeXml(recordKey)
                  valueContainer <- (entry \ "value").headOption
                                     .orElse((entry \\ "value").headOption)
                                     .toRight(ReadError(Cause.empty, "Missing <value> node"))
                  recordValue = valueContainer.label match {
                    case "record" => valueContainer
                    case _ =>
                      valueContainer.child.collect { case e: Elem if e.label == "record" => e }.headOption
                        .orElse((valueContainer \\ "record").headOption)
                        .getOrElse(valueContainer)
                  }
                  value <- valueDecoder.decodeXml(recordValue)
                } yield key -> value
              }
              sequence(decodedEntries).map(_.toMap)
            }
            private def sequence[A](eithers: Seq[Either[DecodeError, A]]): Either[DecodeError, Seq[A]] =
              eithers
                .foldRight(Right(Nil): Either[DecodeError, List[A]]) { (elem, acc) =>
                  for {
                    xs <- acc
                    x  <- elem
                  } yield x :: xs
                }
                .map(_.reverse)
          }
      }
    }

    // --- XML Dynamic Decoder ---
// If the dynamic schema is annotated for direct mapping, decode the XML directly;
// otherwise, delegate to the decoder for DynamicValue.schema.
    private def dynamicDecoder(schema: Schema.Dynamic): ZXmlDecoder[DynamicValue] =
      if (schema.annotations.exists(_.isInstanceOf[directDynamicMapping])) {
        new ZXmlDecoder[DynamicValue] {
          override def decodeXml(node: Node): Either[DecodeError, DynamicValue] =
            try {
              Right(xmlToDynamicValue(node))
            } catch {
              case e: Exception => Left(ReadError(Cause.fail(e), s"Failed to decode XML: ${e.getMessage}"))
            }
        }
      } else {
        schemaDecoder(DynamicValue.schema)
      }

    // --- XML to DynamicValue Conversion ---
    private def xmlToDynamicValue(node: Node): DynamicValue = node.label match {
      case "record" =>
        DynamicValue.Record(
          TypeId.Structural,
          ListMap(node.child.collect { case e: Elem => e.label -> xmlToDynamicValue(e) }.toList: _*)
        )
      case "sequence" =>
        DynamicValue.Sequence(Chunk.fromIterable(node.child.collect { case e: Elem => xmlToDynamicValue(e) }))
      case "set" =>
        DynamicValue.SetValue(Chunk.fromIterable(node.child.collect { case e: Elem => xmlToDynamicValue(e) }).toSet)
      case "boolean" =>
        DynamicValue.Primitive(node.text.trim.toBoolean, StandardType.BoolType)
      case "double" =>
        DynamicValue.Primitive(node.text.trim.toDouble, StandardType.DoubleType)
      case "int" =>
        DynamicValue.Primitive(node.text.trim.toInt, StandardType.IntType)
      case "bigDecimal" =>
        DynamicValue.Primitive(new java.math.BigDecimal(node.text.trim), StandardType.BigDecimalType)
      case "string" =>
        DynamicValue.Primitive(node.text, StandardType.StringType)
      case "null" =>
        DynamicValue.NoneValue
      case "singleton" =>
        DynamicValue.Singleton(())
      case "both" =>
        val leftNode  = (node \ "left").headOption.map(xmlToDynamicValue).getOrElse(DynamicValue.NoneValue)
        val rightNode = (node \ "right").headOption.map(xmlToDynamicValue).getOrElse(DynamicValue.NoneValue)
        DynamicValue.BothValue(leftNode, rightNode)
      case _ =>
        val childElems = node.child.collect { case e: Elem => e }
        if (childElems.size == 1)
          xmlToDynamicValue(childElems.head)
        else
          DynamicValue.Primitive(node.text, StandardType.StringType)
    }

    // Suspend: a helper to defer evaluation of a decoder (for lazy schemas).
    private def suspend[A](decoder: => ZXmlDecoder[A]): ZXmlDecoder[A] =
      new ZXmlDecoder[A] {
        lazy val d                                                 = decoder
        override def decodeXml(node: Node): Either[DecodeError, A] = d.decodeXml(node)
      }

    // --- XML Enum Decoder ---

    private def enumDecoder[Z](parentSchema: Schema.Enum[Z]): ZXmlDecoder[Z] = {
      import scala.collection.mutable
      val caseNameAliases = mutable.HashMap[String, Schema.Case[Z, Any]]()

      parentSchema.cases.foreach { case_ =>
        val schemaCase = case_.asInstanceOf[Schema.Case[Z, Any]]
        caseNameAliases.put(case_.caseName, schemaCase)
        schemaCase.caseNameAliases.foreach(a => caseNameAliases.put(a, schemaCase))
      }

      // If all enum cases are CaseClass0 (i.e. string-based enums)
      if (parentSchema.cases.forall(_.schema.isInstanceOf[Schema.CaseClass0[_]])) {
        new ZXmlDecoder[Z] {
          override def decodeXml(node: Node): Either[DecodeError, Z] = {
            val caseName =
              if (node.label == "field") {
                val name = (node \ "@name").text.trim
                name
              } else {
                val inner = node.text.trim
                if (inner.nonEmpty) {
                  inner
                } else {
                  node.label
                }
              }
            caseNameAliases.get(caseName) match {
              case Some(schemaCase) =>
                Right(schemaCase.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct())
              case None =>
                val errorMsg = s"Unrecognized enum case: $caseName"
                Left(ReadError(Cause.empty, errorMsg))
            }
          }
        }
      }

      // If @noDiscriminator is present, try each case's decoder.
      else if (parentSchema.annotations.exists(_.isInstanceOf[noDiscriminator])) {
        new ZXmlDecoder[Z] {
          override def decodeXml(node: Node): Either[DecodeError, Z] =
            parentSchema.cases.iterator.map { schemaCase =>
              // Unwrap lazy schemas before decoding.
              schemaDecoder(unLazy(schemaCase.schema), None).decodeXml(node)
            }.collectFirst {
              case Right(value) =>
                Right(value.asInstanceOf[Z])
            }.getOrElse {
              val errorMsg = "None of the subtypes could decode the XML data"
              Left(ReadError(Cause.empty, errorMsg))
            }
        }
      } else {
        val discriminatorTag = parentSchema.annotations.collectFirst {
          case d: discriminatorName => d.tag
        }.getOrElse("type")

        // A helper function to unwrap known wrapper elements
        def unwrapEnumNode(node: Node): Node =
          node.child.filter(_.isInstanceOf[Elem]) match {
            case Seq(inner: Elem) if Set("record", "oneOf").contains(node.label) =>
              unwrapEnumNode(inner)
            case children if children.nonEmpty =>
              children.find(_.attribute("type").exists(_.nonEmpty)).getOrElse(node)
            case _ => node
          }

        new ZXmlDecoder[Z] {
          override def decodeXml(node: Node): Either[DecodeError, Z] = {
            // If the outer node is an <enum>, extract the discriminator from it and use its first child for payload decoding.
            val (discriminatorValueOpt, payloadNode) =
              if (node.label == "enum") {
                val discOpt = node
                  .attribute(discriminatorTag)
                  .flatMap(_.headOption)
                  .map(_.text.trim)
                val inner = node.child.collect { case e: Elem => e }.headOption.getOrElse(node)
                (discOpt, inner)
              } else {
                val effectiveNode = unwrapEnumNode(node)
                val discOpt = effectiveNode
                  .attribute(discriminatorTag)
                  .flatMap(_.headOption)
                  .map(_.text.trim)
                  .orElse((effectiveNode \ discriminatorTag).headOption.map(_.text.trim))
                  .orElse {
                    effectiveNode.child.collect { case e: Elem => e } match {
                      case Seq(singleChild) => Some(singleChild.text.trim)
                      case _                => None
                    }
                  }
                (discOpt, effectiveNode)
              }

            discriminatorValueOpt match {
              case None =>
                val errorMsg = s"Missing discriminator '$discriminatorTag' on enum element"
                Left(ReadError(Cause.empty, errorMsg))
              case Some(discrValue) =>
                caseNameAliases.get(discrValue) match {
                  case None =>
                    val errorMsg = s"Unrecognized enum case: $discrValue"
                    Left(ReadError(Cause.empty, errorMsg))
                  case Some(schemaCase) =>
                    schemaDecoder(unLazy(schemaCase.schema).asInstanceOf[Schema[Any]], None)
                      .decodeXml(payloadNode) match {
                      case Right(value) =>
                        Right(value.asInstanceOf[Z])
                      case Left(err) =>
                        Left(err)
                    }
                }
            }
          }
        }
      }

    }

    private def recordDecoder[A](
      schema: Schema.GenericRecord,
      discriminator: Option[String]
    ): ZXmlDecoder[ListMap[String, Any]] =
      if (schema.fields.foldLeft(0)(_ + _.nameAndAliases.size) <= 64) {
        val ccxd = CaseClassXmlDecoder(schema, discriminator)
        new ZXmlDecoder[ListMap[String, Any]] {
          override def decodeXml(node: scala.xml.Node): Either[DecodeError, ListMap[String, Any]] =
            try {
              scala.xml.Utility.trim(node).toString

              val result = ccxd.unsafeDecodeListMap(Nil, node)

              Right(result)
            } catch {
              case e: Exception =>
                Left(ReadError(Cause.fail(e), e.getMessage))
            }
        }
      } else {
        new ZXmlDecoder[ListMap[String, Any]] {

          def unwrapTo(node: scala.xml.Node, expected: String): scala.xml.Node =
            if (node.label == expected) node
            else {
              node.child.collect { case e: scala.xml.Elem => e }.find(_.label == expected) match {
                case Some(child) => unwrapTo(child, expected)
                case None        => node
              }
            }

          override def decodeXml(node: scala.xml.Node): Either[DecodeError, ListMap[String, Any]] = {
            // First, unwrap the outer node to find a <record> element.
            val effectiveNode = unwrapTo(node, "record")
            if (effectiveNode.label != "record")
              return Left(ReadError(Cause.empty, s"Expected <record> element but found <${effectiveNode.label}>"))

            val fieldsArr = schema.fields.toArray
            val spansWithDecoders =
              new scala.collection.mutable.HashMap[String, (XmlError.ObjectAccess, ZXmlDecoder[Any])]()
            fieldsArr.foreach { field =>
              val span = XmlError.ObjectAccess(field.fieldName)
              val dec  = schemaDecoder(field.schema).asInstanceOf[ZXmlDecoder[Any]]
              field.nameAndAliases.foreach { name =>
                spansWithDecoders.put(name, (span, dec))
              }
            }
            val skipExtraFields = !schema.annotations.exists(_.isInstanceOf[rejectExtraFields])
            val map             = new scala.collection.mutable.HashMap[String, Any]()

            // Iterate over the immediate child elements of the effective <record> node.
            val children = (effectiveNode \ "_").collect { case e: scala.xml.Elem => e }
            children.foreach { child =>
              val fieldNameOrAlias =
                if (child.label == "field") (child \ "@name").text.trim
                else child.label

              spansWithDecoders.get(fieldNameOrAlias) match {
                case Some((span, dec)) =>
                  val effectiveChild =
                    if (child.label == "fields") unwrapTo(child, "seq") else child

                  dec.decodeXml(effectiveChild) match {
                    case Right(value) =>
                      val primaryField = span.field // the primary field name
                      if (map.contains(primaryField))
                        return Left(ReadError(Cause.empty, s"duplicate field: $primaryField"))
                      else
                        map.put(primaryField, value)
                    case Left(err) => return Left(err)
                  }
                case None =>
                  if (!skipExtraFields && !discriminator.contains(fieldNameOrAlias))
                    return Left(ReadError(Cause.empty, s"extra field : $fieldNameOrAlias"))
              }
            }
            // Fill in missing fields.
            fieldsArr.foreach { field =>
              if (!map.contains(field.fieldName)) {
                // Unwrap the schema in case of lazy values.
                val underlying = field.schema match {
                  case l: Schema.Lazy[_] => l.schema
                  case s                 => s
                }
                val value =
                  underlying match {
                    case _: Schema.Optional[_] =>
                      // If the field is optional (even if no default is provided), use None.
                      None
                    case collection: Schema.Collection[_, _] =>
                      collection.empty
                    case _ =>
                      if (field.optional || field.transient)
                        field.defaultValue.getOrElse(None)
                      else
                        return Left(ReadError(Cause.empty, s"missing field: ${field.fieldName}"))
                  }
                map.put(field.fieldName, value)
              }
            }
            Right(ListMap(map.toSeq: _*))
          }
        }
      }

    private def fallbackDecoder[A, B](schema: Schema.Fallback[A, B]): ZXmlDecoder[Fallback[A, B]] =
      new ZXmlDecoder[Fallback[A, B]] {
        override def decodeXml(node: Node): Either[DecodeError, Fallback[A, B]] = {
          val leftDecoder  = schemaDecoder(schema.left)
          val rightDecoder = schemaDecoder(schema.right)

          val leftChildOpt  = (node \ "left").headOption
          val rightChildOpt = (node \ "right").headOption

          (leftChildOpt, rightChildOpt) match {
            case (Some(ln), Some(rn)) =>
              for {
                leftValue  <- leftDecoder.decodeXml(ln)
                rightValue <- rightDecoder.decodeXml(rn)
              } yield Fallback.Both(leftValue, rightValue)
            case (Some(ln), None) =>
              leftDecoder.decodeXml(ln).map(Fallback.Left(_))
            case (None, Some(rn)) =>
              rightDecoder.decodeXml(rn).map(Fallback.Right(_))
            case (None, None) =>
              val nodeToDecode =
                if (node.label == "fallback") {
                  node.child.collect { case e: Elem => e }.headOption match {
                    case Some(inner) =>
                      inner
                    case None =>
                      node
                  }
                } else node

              // First, try decoding the (possibly unwrapped) node with the left decoder.
              leftDecoder.decodeXml(nodeToDecode) match {
                case Right(a) =>
                  Right(Fallback.Left(a))
                case Left(_) =>
                  rightDecoder.decodeXml(nodeToDecode) match {
                    case Right(b) =>
                      Right(Fallback.Right(b))
                    case Left(err) =>
                      Left(err)
                  }
              }
          }
        }
      }

  }

  /** XML Product Encoder
   *
   * If a discriminator tuple is provided, it is added as an attribute to the record.
   */
  private[codec] object ProductEncoder {

    // Helper: determines if a field’s value is considered empty.
    private[codec] def isEmptyOptionalValue(schema: Schema.Field[_, _], value: Any, cfg: XmlCodec.Config): Boolean =
      (cfg.ignoreEmptyCollections || schema.optional) && (value match {
        case None            => true
        case it: Iterable[_] => it.isEmpty
        case _               => false
      })

    /** Encodes a case class to XML.
     *
     * It uses each field’s XML encoder (obtained from XmlEncoder.schemaEncoder) to encode the field.
     * The result is a `<record>` element with child elements for each field.
     * If a discriminator tuple is provided, an attribute is added to the record element.
     */
    private[codec] def caseClassEncoder[Z](
      schema: Schema.Record[Z],
      cfg: XmlCodec.Config,
      discriminatorTuple: Option[(String, String)]
    ): ZXmlEncoder[Z] = {
      val nonTransientFields = schema.nonTransientFields.toArray.asInstanceOf[Array[Schema.Field[Z, Any]]]
      val encoders           = nonTransientFields.map(field => XmlEncoder.schemaEncoder(field.schema, cfg, discriminatorTuple))

      new ZXmlEncoder[Z] {
        def formatXml(elem: Elem): Elem =
          if (cfg.prettyPrint) scala.xml.XML.loadString(new scala.xml.PrettyPrinter(80, 2).format(elem))
          else elem

        override def encodeXml(a: Z): Elem = formatXml {
          val ns = cfg.namespace.getOrElse("")
          val children: Seq[Elem] = nonTransientFields.zip(encoders).flatMap {
            case (field, encoder) =>
              val value = field.get(a)

              // Skip empty optional values and fields that are "nothing" unless explicit nulls are enabled
              if (isEmptyOptionalValue(field, value, cfg) || (encoder.isNothing(value) && !cfg.explicitNulls))
                None
              else
                Some(
                  Elem(
                    prefix = if (ns.nonEmpty) "ns" else null,
                    label = field.fieldName,
                    attributes = Null,
                    scope = if (ns.nonEmpty) NamespaceBinding("ns", ns, TopScope) else TopScope,
                    minimizeEmpty = true,
                    child = encoder.encodeXml(value)
                  )
                )
          }

          // Build attributes from discriminator if provided
          val attributes: MetaData = discriminatorTuple match {
            case Some((discField, discValue)) =>
              new UnprefixedAttribute(discField, discValue, Null)
            case None => Null
          }

          val attributeMeta: MetaData = cfg.attributes.foldLeft(attributes) { (meta, attr) =>
            val fieldOpt = nonTransientFields.find(_.fieldName == attr).map(_.get(a))
            fieldOpt.flatMap {
              case None        => None // Skip None values
              case Some(value) => Some(new UnprefixedAttribute(attr, value.toString, meta))
              case value       => Some(new UnprefixedAttribute(attr, value.toString, meta))
            }.getOrElse(meta)
          }

          Elem(
            prefix = if (ns.nonEmpty) "ns" else null,
            label = "record",
            attributes = attributeMeta,
            scope = if (ns.nonEmpty) NamespaceBinding("ns", ns, TopScope) else TopScope,
            minimizeEmpty = true,
            child = children: _*
          )
        }
      }
    }
  }

  class XmlStringMatrix(names: Array[String], aliases: Array[(String, Int)]) {}

// ----- The Product Decoder for case classes -----

  private[codec] object ProductDecoder {

    private[codec] def caseClass0Decoder[Z](
      discriminator: Option[String],
      schema: Schema.CaseClass0[Z]
    ): ZXmlDecoder[Z] = {
      val rejectExtraFields = schema.annotations.exists(_.isInstanceOf[rejectExtraFields])
      val noDiscriminator   = discriminator.isEmpty

      // Helper function to unwrap a node until one with the expected label is found.
      def unwrapTo(node: scala.xml.Node, expected: String): scala.xml.Node =
        if (node.label == expected) node
        else {
          node.child.collect { case e: scala.xml.Elem => e }.find(_.label == expected) match {
            case Some(child) => unwrapTo(child, expected)
            case None        => node
          }
        }

      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] = {
          val effectiveNode = if (noDiscriminator) unwrapTo(node, "record") else node
          if (noDiscriminator && effectiveNode.label != "record")
            return Left(ReadError(Cause.empty, s"Expected <record> element but i found <${effectiveNode.label}>"))
          val children = (effectiveNode \ "_").collect { case e: scala.xml.Elem => e }
          if (rejectExtraFields && children.nonEmpty)
            return Left(ReadError(Cause.empty, s"Extra field encountered"))
          Right(schema.defaultConstruct())
        }
      }
    }

    private[codec] def caseClass1Decoder[A, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass1[A, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd               = CaseClassXmlDecoder(schema, discriminator)
            val buffer: Array[Any] = ccxd.unsafeDecodeFields(Nil, node)
            Right(schema.defaultConstruct(buffer(0).asInstanceOf[A]))
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass2Decoder[A1, A2, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass2[A1, A2, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd               = CaseClassXmlDecoder(schema, discriminator)
            val buffer: Array[Any] = ccxd.unsafeDecodeFields(Nil, node)
            Right(schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2]))
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass3Decoder[A1, A2, A3, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass3[A1, A2, A3, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
              schema.construct(
                buffer(0).asInstanceOf[A1],
                buffer(1).asInstanceOf[A2],
                buffer(2).asInstanceOf[A3]
              )
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass4Decoder[A1, A2, A3, A4, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass4[A1, A2, A3, A4, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
              schema.construct(
                buffer(0).asInstanceOf[A1],
                buffer(1).asInstanceOf[A2],
                buffer(2).asInstanceOf[A3],
                buffer(3).asInstanceOf[A4]
              )
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass5Decoder[A1, A2, A3, A4, A5, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
              schema.construct(
                buffer(0).asInstanceOf[A1],
                buffer(1).asInstanceOf[A2],
                buffer(2).asInstanceOf[A3],
                buffer(3).asInstanceOf[A4],
                buffer(4).asInstanceOf[A5]
              )
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass6Decoder[A1, A2, A3, A4, A5, A6, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
              schema.construct(
                buffer(0).asInstanceOf[A1],
                buffer(1).asInstanceOf[A2],
                buffer(2).asInstanceOf[A3],
                buffer(3).asInstanceOf[A4],
                buffer(4).asInstanceOf[A5],
                buffer(5).asInstanceOf[A6]
              )
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass7Decoder[A1, A2, A3, A4, A5, A6, A7, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
              schema.construct(
                buffer(0).asInstanceOf[A1],
                buffer(1).asInstanceOf[A2],
                buffer(2).asInstanceOf[A3],
                buffer(3).asInstanceOf[A4],
                buffer(4).asInstanceOf[A5],
                buffer(5).asInstanceOf[A6],
                buffer(6).asInstanceOf[A7]
              )
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass8Decoder[A1, A2, A3, A4, A5, A6, A7, A8, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
              schema.construct(
                buffer(0).asInstanceOf[A1],
                buffer(1).asInstanceOf[A2],
                buffer(2).asInstanceOf[A3],
                buffer(3).asInstanceOf[A4],
                buffer(4).asInstanceOf[A5],
                buffer(5).asInstanceOf[A6],
                buffer(6).asInstanceOf[A7],
                buffer(7).asInstanceOf[A8]
              )
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass9Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
              schema.construct(
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
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass10Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
                buffer(9).asInstanceOf[A10]
              )
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass11Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
                buffer(10).asInstanceOf[A11]
              )
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass12Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
                buffer(11).asInstanceOf[A12]
              )
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass13Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
                buffer(12).asInstanceOf[A13]
              )
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass14Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass15Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass16Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](
      discriminator: Option[String],
      schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass17Decoder[
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
      Z
    ](
      discriminator: Option[String],
      schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass18Decoder[
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
      discriminator: Option[String],
      schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass19Decoder[
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
      discriminator: Option[String],
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
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass20Decoder[
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
      discriminator: Option[String],
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
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass21Decoder[
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
      discriminator: Option[String],
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
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

    private[codec] def caseClass22Decoder[
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
      discriminator: Option[String],
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
    ): ZXmlDecoder[Z] =
      new ZXmlDecoder[Z] {
        override def decodeXml(node: scala.xml.Node): Either[DecodeError, Z] =
          try {
            val ccxd   = CaseClassXmlDecoder(schema, discriminator)
            val buffer = ccxd.unsafeDecodeFields(Nil, node)
            Right(
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
            )
          } catch {
            case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
          }
      }

  }

  // ---------- Existing CaseClassXmlDecoder (unchanged) ----------

  private object NotSet

  private class CaseClassXmlDecoder[Z](
    fields: Array[Schema.Field[Z, _]],
    fieldDecoders: Array[ZXmlDecoder[_]],
    spans: Array[XmlError.ObjectAccess],
    stringMatrix: XmlStringMatrix,
    noDiscriminator: Boolean,
    skipExtraFields: Boolean
  ) {

    private def effectiveName(child: scala.xml.Elem): String =
      if (child.label == "field") {
        val nameAttr = (child \ "@name").text.trim
        if (nameAttr.nonEmpty) {
          nameAttr
        } else {
          child.label
        }
      } else {
        child.label
      }

    private def extractFieldValue(child: scala.xml.Elem): Option[String] =
      if (child.child.exists {
            case e: scala.xml.Elem => e.label == "null"
            case _                 => false
          }) {
        None
      } else {
        val extracted = child.child.collect { case e: scala.xml.Elem => e.text.trim }.headOption
          .getOrElse(child.text.trim)
        val normalized = extracted
        Some(normalized)
      }

    def unsafeDecodeListMap(trace: List[XmlError], node: scala.xml.Node): ListMap[String, Any] = {
      scala.xml.Utility.trim(node).toString

      val buffer = unsafeDecodeFields(trace, node)
      val result = ListMap(fields.zip(buffer).map { case (field, v) => field.fieldName -> v }: _*)

      result
    }

    def unsafeDecodeFields(trace: List[XmlError], node: scala.xml.Node): Array[Any] = {
      scala.xml.Utility.trim(node).toString

      val effectiveNode = {
        val elems = node.child.collect { case e: scala.xml.Elem => e }
        if (elems.size == 1 && elems.head.label == "record") {
          elems.head
        } else node
      }

      val len    = fields.length
      val buffer = Array.fill[Any](len)(NotSet)

      val lookup = new scala.collection.mutable.HashMap[String, Int]()
      for (i <- 0 until len) {
        val field = fields(i)
        lookup(field.fieldName) = i
        field.nameAndAliases.foreach { name =>
          lookup(name) = i
        }
      }

      // Collect all element children of the effective record.
      val allChildren: Seq[scala.xml.Elem] = effectiveNode.child.collect { case e: scala.xml.Elem => e }.toList
      // Decide whether to use the "wrapped" style or "direct" style.
      val children: Seq[scala.xml.Elem] =
        if (allChildren.exists(_.label == "field")) {
          allChildren.filter(_.label == "field")
        } else {
          allChildren
        }

      val effectiveNames = children.map(child => effectiveName(child))
      effectiveNames.groupBy(identity).foreach {
        case (name, group) =>
          if (group.size > 1 && lookup.contains(name)) {
            val idx = lookup(name)
            throw ReadError(
              Cause.empty,
              XmlError.render(XmlError.Message("duplicate") :: spans(idx) :: Nil)
            )
          }
      }

      children.foreach { child =>
        val name = effectiveName(child)
        if (name.isEmpty) {} else {
          lookup.get(name) match {
            case Some(idx) =>
              if (buffer(idx) != NotSet) {
                throw ReadError(Cause.empty, XmlError.render(XmlError.Message("duplicate") :: spans(idx) :: Nil))
              }
              val field = fields(idx)
              field.optional || (field.schema.toString.contains("None") && field.schema.toString.contains("Some"))
              val underlyingSchema = field.schema match {
                case l: Schema.Lazy[_] => l.schema
                case s                 => s
              }

              underlyingSchema match {
                // For string fields, use the current logic.
                case prim: Schema.Primitive[_] if prim.standardType == StandardType.StringType =>
                  fieldDecoders(idx).decodeXml(child) match {
                    case Right(decodedValue) =>
                      buffer(idx) = decodedValue
                    case Left(err) =>
                      throw ReadError(Cause.fail(err), err.toString)
                  }

                // NEW: For collections, avoid flattening text.
                case _: Schema.Collection[_, _] =>
                  fieldDecoders(idx).decodeXml(child) match {
                    case Right(decodedValue) =>
                      buffer(idx) = decodedValue
                    case Left(err) =>
                      throw ReadError(Cause.fail(err), err.toString)
                  }

                case _ =>
                  child.text
                  val trimmedText        = child.text.trim
                  val normalizedValueOpt = extractFieldValue(child)
                  val isOptionalField = underlyingSchema match {
                    case _: Schema.Optional[_] => true
                    case _                     => false
                  }

                  normalizedValueOpt match {
                    case None if isOptionalField =>
                      buffer(idx) = None
                    case Some(value) if value.isEmpty && isOptionalField && child.child.forall {
                          case e: scala.xml.Elem => e.label == "null"
                          case _                 => false
                        } =>
                      buffer(idx) = None
                    case Some(value) if child.child.isEmpty && trimmedText.isEmpty && isOptionalField =>
                      buffer(idx) = None
                    case Some(value) if child.child.isEmpty && trimmedText.isEmpty =>
                      underlyingSchema match {
                        case col: Schema.Collection[_, _] =>
                          buffer(idx) = col.empty
                        case _ =>
                          buffer(idx) = None
                      }
                    case Some(_) =>
                      val newChild =
                        underlyingSchema match {
                          case opt: Schema.Optional[_] =>
                            opt.schema match {
                              case prim: Schema.Primitive[_] if prim.standardType == StandardType.StringType =>
                                child match {
                                  case elem: scala.xml.Elem =>
                                    elem.copy(child = elem.child.map {
                                      case t: scala.xml.Text => scala.xml.Text(t.text.trim)
                                      case other             => other
                                    })
                                  case other => other
                                }
                              case _ => child
                            }
                          case prim: Schema.Primitive[_] if prim.standardType != StandardType.StringType =>
                            child match {
                              case elem: scala.xml.Elem =>
                                elem.copy(child = elem.child.map {
                                  case t: scala.xml.Text => scala.xml.Text(t.text.trim)
                                  case other             => other
                                })
                              case other => other
                            }
                          case _ => child
                        }
                      fieldDecoders(idx).decodeXml(newChild) match {
                        case Right(decodedValue) =>
                          buffer(idx) = decodedValue
                        case Left(err) =>
                          throw ReadError(Cause.fail(err), err.toString)
                      }
                  }
              }
            case None =>
              if (!skipExtraFields) {
                throw ReadError(Cause.empty, s"extra field: $name")
              }
          }
        }
      }

      // For any expected field that was not set, use default or report missing.
      for (i <- 0 until len) {
        if (buffer(i) == NotSet) {
          val field = fields(i)
          if ((field.optional || field.transient) && field.defaultValue.isDefined)
            buffer(i) = field.defaultValue.get
          else {
            val underlyingSchema = field.schema match {
              case l: Schema.Lazy[_] => l.schema
              case s                 => s
            }
            buffer(i) = underlyingSchema match {
              case opt: Schema.Optional[_] if opt.schema.isInstanceOf[Schema.Collection[_, _]] =>
                opt.schema.asInstanceOf[Schema.Collection[_, _]].empty
              case _: Schema.Optional[_]               => None
              case collection: Schema.Collection[_, _] => collection.empty
              case _ =>
                throw ReadError(
                  Cause.empty,
                  XmlError.render(XmlError.Message("missing") :: spans(i) :: trace)
                )
            }
          }
        }
      }
      buffer
    }
  }

  private object CaseClassXmlDecoder {

    def apply[Z](schema: Schema.Record[Z], discriminator: Option[String]): CaseClassXmlDecoder[Z] = {
      val hasDiscriminator = discriminator.isDefined
      val len              = schema.fields.length
      var nameLen          = len
      if (hasDiscriminator) nameLen += 1
      val aliasLen = schema.fields.foldLeft(0)(_ + _.nameAndAliases.size) - len
      val fields   = new Array[Schema.Field[Z, _]](len)
      val decoders = new Array[ZXmlDecoder[_]](len)
      val spans    = new Array[XmlError.ObjectAccess](nameLen)
      val names    = new Array[String](nameLen)
      val aliases  = new Array[(String, Int)](aliasLen)
      var idx      = 0
      var aliasIdx = 0
      schema.fields.foreach { field =>
        fields(idx) = field
        decoders(idx) = XmlDecoder.schemaDecoder(field.schema)
        val name = field.fieldName
        names(idx) = name
        spans(idx) = XmlError.ObjectAccess(name)
        (field.nameAndAliases - name).foreach { a =>
          aliases(aliasIdx) = (a, idx)
          aliasIdx += 1
        }
        idx += 1
      }
      if (hasDiscriminator) {
        val discriminatorName = discriminator.get
        names(idx) = discriminatorName
        spans(idx) = XmlError.ObjectAccess(discriminatorName)
      }
      new CaseClassXmlDecoder(
        fields,
        decoders,
        spans,
        new XmlStringMatrix(names, aliases),
        !hasDiscriminator,
        !schema.annotations.exists(_.isInstanceOf[rejectExtraFields])
      )
    }
  }
}
