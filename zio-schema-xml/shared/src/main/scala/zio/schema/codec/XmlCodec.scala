package zio.schema.codec

import java.math.BigDecimal
import java.nio.CharBuffer
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentHashMap

import scala.collection.immutable.ListMap
import scala.util.control.NonFatal
import scala.xml.{Elem, MetaData, NamespaceBinding, Node, Null, TopScope, UnprefixedAttribute}

import zio.prelude.NonEmptyMap
import zio.schema._
import zio.schema.annotation.{ discriminatorName, rejectExtraFields, _ }
import zio.schema.codec.DecodeError.ReadError
import zio.stream.ZPipeline
import zio.{Cause, Chunk, ZIO}

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

    // Default implementation: assume no value is "nothing"
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

  trait XmlFieldEncoder[A] {
    def unsafeEncodeField(a: A): String
  }

  trait XmlFieldDecoder[A] {
    def decodeField(node: Node): Either[DecodeError, A]

    def mapOrFail[B](f: A => Either[String, B]): XmlFieldDecoder[B] = new XmlFieldDecoder[B] {
      override def decodeField(node: Node): Either[DecodeError, B] =
        XmlFieldDecoder.this.decodeField(node).flatMap(a => f(a).left.map(err => ReadError(Cause.empty, err)))
    }

    // def fromZXmlDecoder[V](decoder: ZXmlDecoder[V]): XmlFieldDecoder[V] =
    //   (node: Node) => decoder.decodeXml(node)
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
    // add more error types here, all extending XmlError
  }

  def schemaBasedBinaryCodec[A](cfg: Config)(implicit schema: Schema[A]): BinaryCodec[A] =
    new BinaryCodec[A] {
      override def decode(whole: Chunk[Byte]): Either[DecodeError, A] = {
        // Convert the byte chunk to a UTF-8 string.
        val xmlStr = new String(whole.toArray, StandardCharsets.UTF_8)
        // Use the XML decoder to parse the XML string.
        zio.schema.codec.XmlCodec.XmlDecoder.decode(schema, xmlStr)
      }
      override def streamDecoder: ZPipeline[Any, DecodeError, Byte, A] =
        ZPipeline.utfDecode
          .mapError(cce => ReadError(Cause.fail(cce), cce.getMessage)) >>>
          ZPipeline.mapZIO { xmlString =>
            ZIO.fromEither(zio.schema.codec.XmlCodec.XmlDecoder.decode(schema, xmlString))
          }
      override def encode(value: A): Chunk[Byte] =
        XmlEncoder.encode(schema, value, cfg)
      override def streamEncoder: ZPipeline[Any, Nothing, A, Byte] =
        ZPipeline
          .mapChunks[A, Chunk[Byte]](_.map(encode))
          .intersperse(Chunk.single('\n'.toByte))
          .flattenChunks
    }

  // ──────────────────────────────────────────────────────────────────────────────
  // Now we define the top-level API functions for XML encoding and decoding,
  // mirroring the JSON ones.
  // ──────────────────────────────────────────────────────────────────────────────

  /** Derives an XML encoder for a given schema using the default configuration. */
  def xmlEncoder[A](schema: Schema[A]): ZXmlEncoder[A] =
    xmlEncoder(Config.default)(schema)

  /** Derives an XML encoder for a given schema and configuration. */
  def xmlEncoder[A](cfg: Config)(schema: Schema[A]): ZXmlEncoder[A] =
    XmlEncoder.schemaEncoder(schema, cfg)

  /** Derives an XML decoder for a given schema. */
  def xmlDecoder[A](schema: Schema[A]): ZXmlDecoder[A] =
    XmlDecoder.schemaDecoder(schema)

  /** Derives a complete XML codec (encoder + decoder) for a given schema using the default configuration. */
  def xmlCodec[A](schema: Schema[A]): ZXmlCodec[A] =
    ZXmlCodec(xmlEncoder(schema), xmlDecoder(schema))

  /** Derives a complete XML codec (encoder + decoder) for a given schema and configuration. */
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

        case StandardType.IntType =>
          ZXmlCodec(
            new ZXmlEncoder[Int] {
              override def encodeXml(value: Int): Elem = <int>{value.toString}</int>
            },
            new ZXmlDecoder[Int] {
              override def decodeXml(node: Node): Either[DecodeError, Int] =
                node.text.toIntOption.toRight(ReadError(Cause.empty, "Invalid Int value"))
            }
          )

        case StandardType.BoolType =>
          ZXmlCodec(
            new ZXmlEncoder[Boolean] {
              override def encodeXml(value: Boolean): Elem = <boolean>{value.toString}</boolean>
            },
            new ZXmlDecoder[Boolean] {
              override def decodeXml(node: Node): Either[DecodeError, Boolean] =
                node.text.toLowerCase match {
                  case "true"  => Right(true)
                  case "false" => Right(false)
                  case _       => Left(ReadError(Cause.empty, "Invalid Boolean value"))
                }
            }
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
            new ZXmlEncoder[Double] {
              override def encodeXml(value: Double): Elem = <double>{value.toString}</double>
            },
            new ZXmlDecoder[Double] {
              override def decodeXml(node: Node): Either[DecodeError, Double] =
                node.text.toDoubleOption.toRight(ReadError(Cause.empty, "Invalid Double value"))
            }
          )

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
          // Assuming BigInteger maps to Scala's BigInt
          ZXmlCodec(
            new ZXmlEncoder[BigInt] {
              override def encodeXml(value: BigInt): Elem = <bigInt>{value.toString}</bigInt>
            },
            new ZXmlDecoder[BigInt] {
              override def decodeXml(node: Node): Either[DecodeError, BigInt] =
                try {
                  Right(BigInt(node.text))
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
            new ZXmlEncoder[Short] {
              override def encodeXml(value: Short): Elem = <short>{value.toString}</short>
            },
            new ZXmlDecoder[Short] {
              override def decodeXml(node: Node): Either[DecodeError, Short] =
                node.text.toShortOption.toRight(ReadError(Cause.empty, "Invalid Short value"))
            }
          )

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
          // Here we build an XML element <map> containing an <entry> for each key/value pair.
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

    // Example: a helper for sets.
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
          val (a, b) = value
          <tuple>
          <left>{left.encodeXml(a)}</left>
          <right>{right.encodeXml(b)}</right>
        </tuple>
        }
      }
  }

  object XmlEitherEncoder {

    def either[A, B](left: ZXmlEncoder[A], right: ZXmlEncoder[B]): ZXmlEncoder[Either[A, B]] =
      new ZXmlEncoder[Either[A, B]] {
        override def encodeXml(value: Either[A, B]): Elem = value match {
          case Left(a)  => <left>{left.encodeXml(a)}</left>
          case Right(b) => <right>{right.encodeXml(b)}</right>
        }
      }
  }

  // helper objects for decoding logic
  object XmlTupleDecoder {

    def tuple2[A, B](leftDecoder: ZXmlDecoder[A], rightDecoder: ZXmlDecoder[B]): ZXmlDecoder[(A, B)] =
      new ZXmlDecoder[(A, B)] {
        override def decodeXml(node: Node): Either[DecodeError, (A, B)] =
          if (node.label != "tuple") {
            Left(ReadError(Cause.empty, s"Expected <tuple> element, found <${node.label}>"))
          } else {
            val leftOpt  = (node \ "left").headOption
            val rightOpt = (node \ "right").headOption
            (leftOpt, rightOpt) match {
              case (Some(lNode), Some(rNode)) =>
                for {
                  a <- leftDecoder.decodeXml(lNode)
                  b <- rightDecoder.decodeXml(rNode)
                } yield (a, b)
              case _ =>
                Left(ReadError(Cause.empty, "Missing <left> or <right> element in tuple"))
            }
          }
      }
  }

  object XmlCollectionDecoder {

    def chunk[A](elementDecoder: ZXmlDecoder[A]): ZXmlDecoder[Seq[A]] =
      new ZXmlDecoder[Seq[A]] {
        override def decodeXml(node: Node): Either[DecodeError, Seq[A]] =
          if (node.label != "seq")
            Left(ReadError(Cause.empty, s"Expected <seq> element, found <${node.label}>"))
          else {
            // Collect all child elements (you might adjust this to filter only elements you want)
            val children: Seq[Elem] = node.child.collect { case e: Elem => e }.toList
            sequence(children.map(elementDecoder.decodeXml))
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

    def set[A](elementDecoder: ZXmlDecoder[A]): ZXmlDecoder[Set[A]] =
      new ZXmlDecoder[Set[A]] {
        override def decodeXml(node: Node): Either[DecodeError, Set[A]] =
          if (node.label != "set")
            Left(ReadError(Cause.empty, s"Expected <set> element, found <${node.label}>"))
          else {
            val children: Seq[Elem] = node.child.collect { case e: Elem => e }.toList
            sequence(children.map(elementDecoder.decodeXml)).map(_.toSet)
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

  object XmlFieldDecoder {

    // Basic field decoders for primitive types
    val string: XmlFieldDecoder[String] = (node: Node) => Right(node.text)

    val int: XmlFieldDecoder[Int] = (node: Node) => node.text.toIntOption.toRight(ReadError(Cause.empty, "Invalid Int"))

    val long: XmlFieldDecoder[Long] = (node: Node) =>
      node.text.toLongOption.toRight(ReadError(Cause.empty, "Invalid Long"))

    /**
     * Builds an XmlFieldDecoder for Map[K, V] from a key field decoder and a value field decoder.
     * It expects the XML to be structured as a <map> element with one or more <entry> children,
     * each containing one <key> element and one <value> element.
     */
    def map[K, V](
      keyDecoder: XmlFieldDecoder[K],
      valueDecoder: XmlFieldDecoder[V]
    ): XmlFieldDecoder[Map[K, V]] = (node: Node) => {
      // Collect all child nodes that are Elem instances (typically the <entry> elements)
      val fields: Seq[Elem] = node.child.collect { case e: Elem => e }.toList

      // For each entry, extract the key and value nodes and decode them.
      val parsed: Seq[Either[DecodeError, (K, V)]] = fields.map { field =>
        for {
          // Extract the first <key> element (or return an error if missing)
          keyNode <- (field \ "key").headOption.toRight(
                      ReadError(Cause.empty, "Missing key node")
                    )
          // Extract the first <value> element (or return an error if missing)
          valueNode <- (field \ "value").headOption.toRight(
                        ReadError(Cause.empty, "Missing value node")
                      )
          key   <- keyDecoder.decodeField(keyNode)
          value <- valueDecoder.decodeField(valueNode)
        } yield key -> value
      }

      // Sequence the parsed entries and convert the sequence into a Map.
      sequence(parsed).map(_.toMap)
    }

    // Helper method to sequence a collection of Either values.
    private def sequence[A](eithers: Seq[Either[DecodeError, A]]): Either[DecodeError, Seq[A]] =
      eithers
        .foldRight(Right(Nil): Either[DecodeError, List[A]]) { (elem, acc) =>
          for {
            xs <- acc
            x  <- elem
          } yield x :: xs
        }
        .map(_.reverse)

    /**
     * Lifts a full-schema ZXmlDecoder into an XmlFieldDecoder.
     * That is, given a ZXmlDecoder for V, this returns an XmlFieldDecoder for V that simply
     * delegates to the full decoder.
     */
    def fromZXmlDecoder[V](decoder: ZXmlDecoder[V]): XmlFieldDecoder[V] =
      (node: Node) => decoder.decodeXml(node)

    /**
     * Converts an XmlFieldDecoder into a full ZXmlDecoder.
     * This allows you to use a field decoder where a full-schema decoder is required.
     */
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
          // Check the label of the current node to determine if it is <left> or <right>
          node.label match {
            case "left" =>
              // Extract the first child element (ignoring text or comments)
              (node \ "_").headOption match {
                case Some(child) => left.decodeXml(child).map(Left(_))
                case None        => Left(ReadError(Cause.empty, "Missing content in <left> element"))
              }
            case "right" =>
              (node \ "_").headOption match {
                case Some(child) => right.decodeXml(child).map(Right(_))
                case None        => Left(ReadError(Cause.empty, "Missing content in <right> element"))
              }
            case other =>
              Left(ReadError(Cause.empty, s"Expected <left> or <right> element, but found <$other>"))
          }
      }
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

//   // --------------------------
//   // Charset and Cache
//   // --------------------------
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
          // Use our primitive codec for XML; extract its encoder.
          primitiveCodec(standardType).encoder

        case Schema.Optional(inner, _) =>
          // For Option, use our XML option helper.
          XmlEncoder.option(schemaEncoder(inner, cfg))

        case Schema.Tuple2(l, r, _) =>
          // Delegate to an XML tuple encoder.
          XmlTupleEncoder.tuple2(schemaEncoder(l, cfg), schemaEncoder(r, cfg))

        case Schema.Sequence(elementSchema, _, g, _, _) =>
          // For sequences, decode each child node and then contramap the transformation.
          XmlCollectionEncoder.chunk(schemaEncoder(elementSchema, cfg)).contramap(g)

        case Schema.NonEmptySequence(elementSchema, _, g, _, _) =>
          XmlCollectionEncoder.chunk(schemaEncoder(elementSchema, cfg)).contramap(g)

        case Schema.Map(ks, vs, _) =>
          // Delegate to our XML map encoder (from ProductEncoder).
          XmlEncoder.mapEncoder(ks, vs, cfg)

        case Schema.NonEmptyMap(ks, vs, _) =>
          XmlEncoder.mapEncoder(ks, vs, cfg).contramap(_.toMap)

        case Schema.Set(sch, _) =>
          // Use an XML collection encoder for sets.
          XmlCollectionEncoder.set(schemaEncoder(sch, cfg))

        case Schema.Transform(c, _, g, a, _) =>
          // Apply transformation logic.
          transformEncoder(a.foldLeft(c)((s, a) => s.annotate(a)), g, cfg, discriminatorTuple)

        case Schema.Fail(_, _) =>
          // For failure schemas, produce an encoder that always emits an empty element.
          unitEncoder.contramap(_ => ())

        case Schema.Either(left, right, _) =>
          XmlEitherEncoder.either(schemaEncoder(left, cfg), schemaEncoder(right, cfg))

        case Schema.Fallback(left, right, _, _) =>
          fallbackEncoder(schemaEncoder(left, cfg), schemaEncoder(right, cfg), cfg)

        case l @ Schema.Lazy(_) =>
          // For lazy schemas, defer evaluation.
          XmlEncoder.suspend(schemaEncoder(l.schema, cfg))

        case s: Schema.GenericRecord =>
          recordEncoder(s, cfg, discriminatorTuple)

        case s: Schema.Record[A] =>
          caseClassEncoder(s, cfg, discriminatorTuple)

        case s: Schema.Enum[A] =>
          // Delegate to our XML enum encoder.
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
        case Schema.Transform(c, f, _, a, _) =>
          transformFieldEncoder(a.foldLeft(c)((s, a) => s.annotate(a)), f)
        case Schema.Lazy(inner) => xmlFieldEncoder(inner())
        case _                  => None
      }

    // -----------------------------------------------------------------------------
// Map Encoder
// -----------------------------------------------------------------------------
    private[codec] def mapEncoder[K, V](ks: Schema[K], vs: Schema[V], cfg: Config): ZXmlEncoder[Map[K, V]] = {
      // Get the value encoder from the XML schema.
      val valueEncoder: ZXmlEncoder[V] = schemaEncoder(vs, cfg)
      // Try to get a field encoder for the key.
      xmlFieldEncoder(ks) match {
        case Some(fieldEncoder) =>
          // If available, use a dedicated helper that builds a map encoder from a key field encoder.
          XmlFieldEncoder.map(fieldEncoder, valueEncoder)
        case None =>
          // Otherwise, fall back to encoding the keys using the full schema and then assembling entries.
          // Assume that XmlCollectionEncoder.chunk produces an encoder for a sequence of (K,V) pairs,
          // and that we can zip the key and value encoders.
          val pairEncoder: ZXmlEncoder[(K, V)] =
            new ZXmlEncoder[(K, V)] {
              override def encodeXml(pair: (K, V)): Elem = {
                val (k, v) = pair
                // Here, we simply encode key and value as separate child elements.
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
                          // For each key/value pair, use the key as the element label.
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

    // XML version of transformEncoder:
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

// XML version of enumEncoder:
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
        // If no discriminator is used, try each case until one succeeds.
        new ZXmlEncoder[Z] {
          override def encodeXml(a: Z): Elem = formatXml {
            val possible = schema.nonTransientCases.iterator.map { sc =>
              schemaEncoder(sc.schema.asInstanceOf[Schema[Any]], cfg, None)
                .encodeXml(sc.deconstruct(a).asInstanceOf[Any])
            }.find(_ => true)           // In practice, you'll need to try decoding/encoding each case.
            possible.getOrElse(<enum/>) // Fallback empty element.
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

        // Helper function to apply pretty printing if enabled
        def formatXml(elem: Elem): Elem =
          if (cfg.prettyPrint) scala.xml.XML.loadString(new scala.xml.PrettyPrinter(80, 2).format(elem))
          else elem

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
        // Build an array of encoders for each field.
        val encoders = nonTransientFields.map(field => schemaEncoder(field.schema.asInstanceOf[Schema[Any]], cfg))

        new ZXmlEncoder[ListMap[String, Any]] {
          // Helper function to apply pretty printing if enabled
          def formatXml(elem: Elem): Elem =
            if (cfg.prettyPrint) scala.xml.XML.loadString(new scala.xml.PrettyPrinter(80, 2).format(elem))
            else elem

          override def encodeXml(fields: ListMap[String, Any]): Elem = formatXml {
            // Build a sequence of <field> elements.
            val children: Seq[Elem] = nonTransientFields.zipWithIndex.flatMap {
              case (field, idx) =>
                val name  = field.fieldName
                val value = fields(name)

                // Skip empty optional values and fields that are "nothing" unless explicit nulls are enabled
                if (isEmptyOptionalValue(field, value, cfg) || (encoders(idx).isNothing(value) && !cfg.explicitNulls))
                  None
                else
                  Some(
                    <field name={name}>
                {schemaEncoder(field.schema.asInstanceOf[Schema[Any]], cfg).encodeXml(value)}
              </field>
                  )
            }

            // Build attributes from config attributes and discriminator
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

    // --- Caching mechanism ---
    private case class DecoderKey[A](schema: Schema[A], discriminator: Option[String]) {
      override val hashCode: Int = System.identityHashCode(schema) ^ discriminator.hashCode
      override def equals(obj: Any): Boolean = obj match {
        case dk: DecoderKey[_] => (dk.schema eq schema) && dk.discriminator == discriminator
        case _                 => false
      }
    }
    private[this] val decoders = new ConcurrentHashMap[DecoderKey[_], ZXmlDecoder[_]]()

    // --- Top-level XML decode method ---
    final def decode[A](schema: Schema[A], xml: String): Either[DecodeError, A] =
      try {
        // Parse the XML string into a Scala XML Node.
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
    // --- Helper decoders for Option, Map, etc. ---
    // Implement an option decoder for XML:
    private[codec] def option[A](decoder: ZXmlDecoder[A]): ZXmlDecoder[Option[A]] =
      new ZXmlDecoder[Option[A]] {
        override def decodeXml(node: Node): Either[DecodeError, Option[A]] =
          // For XML, we might define that an element <null/> or <empty/> indicates a missing value.
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
    // This function mirrors your JSON decoder’s schemaDecoderSlow but produces an XML decoder.
    private def schemaDecoderSlow[A](schema: Schema[A], discriminator: Option[String]): ZXmlDecoder[A] = schema match {
      case Schema.Primitive(standardType, _) =>
        // For primitives, use an XML primitive decoder.
        // You need to implement XmlPrimitiveDecoder.decoder similarly to your JSON primitiveCodec.decoder.
        primitiveCodec(standardType).decoder
      case Schema.Optional(codec, _) =>
        option(schemaDecoder(codec))
      case Schema.Tuple2(left, right, _) =>
        // Implement a tuple2 decoder for XML; you can create XmlTupleDecoder.tuple2
        XmlTupleDecoder.tuple2(schemaDecoder(left), schemaDecoder(right))
      case Schema.Transform(c, f, _, a, _) =>
        // Apply transformation logic analogous to JSON decoder.
        schemaDecoder(a.foldLeft(c)((s, a) => s.annotate(a)), discriminator).mapOrFail(f)
      case Schema.Sequence(codec, f, _, _, _) =>
        // Decode a sequence by decoding each child XML node and mapping the result.
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
        // For lazy schemas, defer evaluation.
        suspend(schemaDecoder(l.schema))
      case s: Schema.GenericRecord =>
        recordDecoder(s, discriminator)
      case s: Schema.Enum[A] =>
        enumDecoder(s)
      // You can add additional cases (e.g., product decoders) here.
      case d @ Schema.Dynamic(_) => dynamicDecoder(d)
      case _ =>
        throw new Exception(s"Missing a handler for decoding of schema ${schema.toString()}.")
    }

    // --- Placeholder implementations for other helpers ---

// --- XML Field Decoder ---
// This helper tries to provide a field decoder (for a single field value) if applicable.
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
// This decoder reads an XML <map> element, iterates over its <entry> children,
// decodes the <key> and <value> subelements, and assembles them into a Map.
    private def mapDecoder[K, V](ks: Schema[K], vs: Schema[V]): ZXmlDecoder[Map[K, V]] = {
      // Obtain the decoder for the values using our XML schema decoder.
      val valueDecoder = XmlDecoder.schemaDecoder(vs)
      xmlFieldDecoder(ks) match {
        case Some(fieldDecoder) =>
          // If a field decoder is available for the key, convert the full schema valueDecoder
          // to a field decoder, then build a map decoder, and finally adapt it to a ZXmlDecoder.
          asZXmlDecoder(
            XmlFieldDecoder.map(fieldDecoder, XmlFieldDecoder.fromZXmlDecoder(valueDecoder))
          )
        case None =>
          // Otherwise, decode the keys via the general schemaDecoder and then combine keys and values.
          // Here we assume that XmlCollectionDecoder.chunk produces a decoder for a collection,
          // and that `zip` combines two decoders into one that decodes a tuple (key, value).

          XmlCollectionDecoder
            .chunk(
              XmlDecoder.schemaDecoder(ks).flatMap { key =>
                valueDecoder.map(value => (key, value))
              }
            )
            .map(_.toMap)
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
// Convert an XML node into a DynamicValue (similar to jsonToDynamicValue).
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
      // Add further cases for other primitive types as needed.
      case _ =>
        // Fallback: treat the node text as a string.
        DynamicValue.Primitive(node.text, StandardType.StringType)
    }

    // Suspend: a helper to defer evaluation of a decoder (for lazy schemas).
    private def suspend[A](decoder: => ZXmlDecoder[A]): ZXmlDecoder[A] =
      new ZXmlDecoder[A] {
        lazy val d                                                 = decoder
        override def decodeXml(node: Node): Either[DecodeError, A] = d.decodeXml(node)
      }

    // You will need to implement similar helpers for tuple decoders, collection decoders,
    // either, fallback, record, enum, and dynamic decoders.
    // For example:

    // --- XML Enum Decoder ---
// This decoder uses a discriminator attribute (e.g. "type") to select the proper case.
// It builds a mapping of case names (and aliases) to decoders.
    private def enumDecoder[Z](parentSchema: Schema.Enum[Z]): ZXmlDecoder[Z] = {
      import scala.collection.mutable
      val caseNameAliases = mutable.HashMap[String, Schema.Case[Z, Any]]()

      parentSchema.cases.foreach { case_ =>
        val schemaCase = case_.asInstanceOf[Schema.Case[Z, Any]]
        caseNameAliases.put(case_.caseName, schemaCase)
        schemaCase.caseNameAliases.foreach(a => caseNameAliases.put(a, schemaCase))
      }

      // Check if all enum cases are CaseClass0 (string-based enums)
      if (parentSchema.cases.forall(_.schema.isInstanceOf[Schema.CaseClass0[_]])) {
        new ZXmlDecoder[Z] {
          override def decodeXml(node: Node): Either[DecodeError, Z] = {
            val caseName = node.text.trim // Extract the text content
            caseNameAliases.get(caseName) match {
              case Some(schemaCase) =>
                Right(schemaCase.schema.asInstanceOf[Schema.CaseClass0[Z]].defaultConstruct())
              case None =>
                Left(ReadError(Cause.empty, s"Unrecognized enum case: $caseName"))
            }
          }
        }
      }
      // Check if @noDiscriminator is present (try each decoder)
      else if (parentSchema.annotations.exists(_.isInstanceOf[noDiscriminator])) {
        new ZXmlDecoder[Z] {
          override def decodeXml(node: Node): Either[DecodeError, Z] =
            parentSchema.cases.iterator.map { schemaCase =>
              schemaDecoder(schemaCase.schema, None).decodeXml(node)
            }.collectFirst { case Right(value) => Right(value.asInstanceOf[Z]) }
              .getOrElse(Left(ReadError(Cause.empty, "None of the subtypes could decode the XML data")))
        }
      }
      // Default case: use a discriminator attribute
      else {
        val discriminator = parentSchema.annotations.collectFirst {
          case d: discriminatorName => d.tag
        }.getOrElse("type") // Default to "type" if no discriminator is provided

        new ZXmlDecoder[Z] {
          override def decodeXml(node: Node): Either[DecodeError, Z] =
            node.attribute(discriminator).flatMap(_.headOption) match {
              case None =>
                Left(ReadError(Cause.empty, s"Missing discriminator attribute '$discriminator' on enum element"))
              case Some(discrValue) =>
                val caseName = discrValue.text
                caseNameAliases.get(caseName) match {
                  case None =>
                    Left(ReadError(Cause.empty, s"Unrecognized enum case: $caseName"))
                  case Some(schemaCase) =>
                    // Assume the inner XML (the case payload) is contained in the node's children
                    schemaDecoder(schemaCase.schema.asInstanceOf[Schema[Any]], None).decodeXml(node) match {
                      case Right(value) => Right(value.asInstanceOf[Z])
                      case Left(err)    => Left(err)
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
        // For smaller records, delegate to the case–class XML decoder.
        val ccxd = CaseClassXmlDecoder(schema, discriminator)
        new ZXmlDecoder[ListMap[String, Any]] {
          override def decodeXml(node: scala.xml.Node): Either[DecodeError, ListMap[String, Any]] =
            try {
              Right(ccxd.unsafeDecodeListMap(Nil, node))
            } catch {
              case e: Exception => Left(ReadError(Cause.fail(e), e.getMessage))
            }
        }
      } else {
        // For records with many fields, build a decoder that iterates over the child elements.
        new ZXmlDecoder[ListMap[String, Any]] {
          override def decodeXml(node: scala.xml.Node): Either[DecodeError, ListMap[String, Any]] = {
            // Verify that the XML node is a <record>
            if (node.label != "record")
              return Left(ReadError(Cause.empty, s"Expected <record> element but found <${node.label}>"))

            // Build a mapping from field names and aliases to (span, decoder)
            val fieldsArr = schema.fields.toArray
            val spansWithDecoders =
              new scala.collection.mutable.HashMap[String, (XmlError.ObjectAccess, ZXmlDecoder[Any])]()
            fieldsArr.foreach { field =>
              val span = XmlError.ObjectAccess(field.fieldName)
              val dec  = schemaDecoder(field.schema).asInstanceOf[ZXmlDecoder[Any]]
              spansWithDecoders.put(field.fieldName, (span, dec))
              // Add all aliases (except the primary name)
              field.nameAndAliases.filterNot(_ == field.fieldName).foreach { alias =>
                spansWithDecoders.put(alias, (span, dec))
              }
            }
            // Determine whether to skip extra fields based on schema annotations.
            val skipExtraFields = !schema.annotations.exists(_.isInstanceOf[rejectExtraFields])
            // Create a mutable map to accumulate decoded fields.
            val map = new scala.collection.mutable.HashMap[String, Any]()

            // Iterate over child elements of the <record>
            (node \ "_").collect { case e: scala.xml.Elem => e }.foreach { child =>
              val fieldNameOrAlias = child.label
              spansWithDecoders.get(fieldNameOrAlias) match {
                case Some((span, dec)) =>
                  dec.decodeXml(child) match {
                    case Right(value) =>
                      // Use the primary field name from the span.
                      val primaryField = span.field
                      if (map.contains(primaryField))
                        return Left(ReadError(Cause.empty, s"Duplicate field: $primaryField"))
                      else
                        map.put(primaryField, value)
                    case Left(err) => return Left(err)
                  }
                case None =>
                  // If the field is unknown, either skip it or report an error.
                  if (!skipExtraFields && !discriminator.contains(fieldNameOrAlias))
                    return Left(ReadError(Cause.empty, s"Extra field encountered: $fieldNameOrAlias"))
                // Otherwise, skip this child.
              }
            }
            // Ensure every field is present (or fill with default/empty value)
            fieldsArr.foreach { field =>
              if (!map.contains(field.fieldName)) {
                map.put(
                  field.fieldName,
                  if (field.optional || field.transient) field.defaultValue.getOrElse(None)
                  else
                    field.schema match {
                      case _: Schema.Optional[_]               => None
                      case collection: Schema.Collection[_, _] => collection.empty
                      case _                                   => return Left(ReadError(Cause.empty, s"Missing field: ${field.fieldName}"))
                    }
                )
              }
            }
            Right(ListMap(map.toSeq: _*))
          }
        }
      }

    private def fallbackDecoder[A, B](schema: Schema.Fallback[A, B]): ZXmlDecoder[Fallback[A, B]] =
      new ZXmlDecoder[Fallback[A, B]] {
        override def decodeXml(node: Node): Either[DecodeError, Fallback[A, B]] = {
          // Retrieve decoders for left and right branches.
          val leftDecoder  = schemaDecoder(schema.left)
          val rightDecoder = schemaDecoder(schema.right)

          // Try to extract child elements named "left" and "right".
          val leftChildOpt  = (node \ "left").headOption
          val rightChildOpt = (node \ "right").headOption

          (leftChildOpt, rightChildOpt) match {
            // If both <left> and <right> children exist, decode both.
            case (Some(ln), Some(rn)) =>
              for {
                leftValue  <- leftDecoder.decodeXml(ln)
                rightValue <- rightDecoder.decodeXml(rn)
              } yield Fallback.Both(leftValue, rightValue)
            // If only <left> exists, decode it as Fallback.Left.
            case (Some(ln), None) =>
              leftDecoder.decodeXml(ln).map(Fallback.Left(_))
            // If only <right> exists, decode it as Fallback.Right.
            case (None, Some(rn)) =>
              rightDecoder.decodeXml(rn).map(Fallback.Right(_))
            // If neither child exists, fall back to attempting a full-node decode.
            case (None, None) =>
              // First, try decoding the whole node with the left decoder.
              leftDecoder.decodeXml(node) match {
                case Right(a) => Right(Fallback.Left(a))
                case Left(_)  =>
                  // If left fails, retract any changes and try decoding as right.
                  rightDecoder.decodeXml(node) match {
                    case Right(b)  => Right(Fallback.Right(b))
                    case Left(err) => Left(err)
                  }
              }
          }
        }
      }

  }

  /** XML Product Encoder
   *
   * This encoder mirrors your JSON product encoder. It encodes a record (case class)
   * into an XML `<record>` element by iterating over its non‐transient fields.
   * Each field is encoded as an XML element with a tag equal to the field’s name.
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
        // Helper function to apply pretty printing if enabled
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

// And a helper class analogous to StringMatrix for XML field names:
  class XmlStringMatrix(names: Array[String], aliases: Array[(String, Int)]) {
    // Implementation details depend on your needs.
    // For our purposes, this is just a placeholder.
  }

// ----- The Product Decoder for case classes -----

  private class CaseClassXmlDecoder[Z](
    fields: Array[Schema.Field[Z, _]],
    fieldDecoders: Array[ZXmlDecoder[_]],
    spans: Array[XmlError.ObjectAccess],
    stringMatrix: XmlStringMatrix,
    noDiscriminator: Boolean,
    skipExtraFields: Boolean
  ) {

    def unsafeDecodeListMap(trace: List[XmlError], node: Node): ListMap[String, Any] = {
      val buffer = unsafeDecodeFields(trace, node)
      ListMap(fields.zip(buffer).map { case (field, v) => field.fieldName -> v }: _*)
    }

    def unsafeDecodeFields(trace: List[XmlError], node: Node): Array[Any] = {
      val len    = fields.length
      val buffer = new Array[Any](len)
      // We assume the XML node is a <record> element.
      // For each field, try to locate a child element with the matching field name.
      for (i <- 0 until len) {
        val field = fields(i)
        // Use the XML API to select children with the given label.
        val children: Seq[Node] = {
          val elems = node \ field.fieldName
          if (elems.nonEmpty) elems
          else node.attribute(field.fieldName).map(_.toSeq).getOrElse(Seq.empty)
        }
        if (children.isEmpty) {
          if ((field.optional || field.transient) && field.defaultValue.isDefined)
            buffer(i) = field.defaultValue.get
          else {
            field match {
              case f if f.optional => buffer(i) = None
              case _               => throw new Exception(s"Missing field: ${field.fieldName}")
            }
          }
        } else {
          // Decode using the first child element.
          buffer(i) = fieldDecoders(i).decodeXml(children.head) match {
            case Right(value) => value
            case Left(err)    => throw new Exception(s"Error decoding field '${field.fieldName}': $err")
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
