package zio.schema.codec

import java.time._
import java.util.UUID
import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}
import org.msgpack.core.{MessagePack, MessageUnpacker}
import zio.prelude.{Validation, ZValidation}
import zio.schema.codec.DecodeError.MalformedFieldWithPath
import zio.schema.codec.MessagePackDecoder._
import zio.schema.{DynamicValue, Schema, StandardType}
import zio.{Chunk, ChunkBuilder}

private[codec] class MessagePackDecoder(bytes: Chunk[Byte]) {
  private val unpacker = MessagePack.newDefaultUnpacker(bytes.toArray)

  def decode[A](schema: Schema[A]): Result[A] =
    decodeValue(Chunk.empty, schema)

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private def decodeValue[A](path: Path, schema: Schema[A]): Result[A] =
    schema match {
      case Schema.GenericRecord(_, structure, _) =>
        val fields = structure.toChunk
        decodeRecord(path, fields)
      case seqSchema @ Schema.Sequence(_, _, _, _, _) => decodeSequence(path, seqSchema)
      case mapSchema @ Schema.Map(_, _, _)            => decodeMap(path, mapSchema)
      case setSchema @ Schema.Set(_, _)               => decodeSet(path, setSchema)
      case Schema.Transform(schema, f, _, _, _)       => decodeTransform(path, schema, f)
      case Schema.Primitive(standardType, _)          => decodePrimitive(path, standardType)
      case Schema.Tuple2(left, right, _)              => decodeTuple(path, left, right)
      case optionalSchema @ Schema.Optional(_, _)     => decodeOptional(path, optionalSchema)
      case Schema.Fail(message, _)                    => fail(path, message)
      case Schema.Either(left, right, _)              => decodeEither(path, left, right)
      case lzy @ Schema.Lazy(_)                       => decodeValue(path, lzy.schema)
      //case Schema.Meta(_, _)                                                                                                        => decode(path, Schema[MetaSchema]).map(_.toSchema)
      case s: Schema.CaseClass0[A]                                                           => caseClass0Decoder(path, s)
      case s: Schema.CaseClass1[_, A]                                                        => caseClass1Decoder(path, s)
      case s: Schema.CaseClass2[_, _, A]                                                     => caseClass2Decoder(path, s)
      case s: Schema.CaseClass3[_, _, _, A]                                                  => caseClass3Decoder(path, s)
      case s: Schema.CaseClass4[_, _, _, _, A]                                               => caseClass4Decoder(path, s)
      case s: Schema.CaseClass5[_, _, _, _, _, A]                                            => caseClass5Decoder(path, s)
      case s: Schema.CaseClass6[_, _, _, _, _, _, A]                                         => caseClass6Decoder(path, s)
      case s: Schema.CaseClass7[_, _, _, _, _, _, _, A]                                      => caseClass7Decoder(path, s)
      case s: Schema.CaseClass8[_, _, _, _, _, _, _, _, A]                                   => caseClass8Decoder(path, s)
      case s: Schema.CaseClass9[_, _, _, _, _, _, _, _, _, A]                                => caseClass9Decoder(path, s)
      case s: Schema.CaseClass10[_, _, _, _, _, _, _, _, _, _, A]                            => caseClass10Decoder(path, s)
      case s: Schema.CaseClass11[_, _, _, _, _, _, _, _, _, _, _, A]                         => caseClass11Decoder(path, s)
      case s: Schema.CaseClass12[_, _, _, _, _, _, _, _, _, _, _, _, A]                      => caseClass12Decoder(path, s)
      case s: Schema.CaseClass13[_, _, _, _, _, _, _, _, _, _, _, _, _, A]                   => caseClass13Decoder(path, s)
      case s: Schema.CaseClass14[_, _, _, _, _, _, _, _, _, _, _, _, _, _, A]                => caseClass14Decoder(path, s)
      case s: Schema.CaseClass15[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]             => caseClass15Decoder(path, s)
      case s: Schema.CaseClass16[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]          => caseClass16Decoder(path, s)
      case s: Schema.CaseClass17[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]       => caseClass17Decoder(path, s)
      case s: Schema.CaseClass18[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A]    => caseClass18Decoder(path, s)
      case s: Schema.CaseClass19[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] => caseClass19Decoder(path, s)
      case s: Schema.CaseClass20[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
        caseClass20Decoder(path, s)
      case s: Schema.CaseClass21[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
        caseClass21Decoder(path, s)
      case s: Schema.CaseClass22[_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, A] =>
        caseClass22Decoder(path, s)
      case Schema.Enum1(_, c, _)                              => decodeEnum(path, c)
      case Schema.Enum2(_, c1, c2, _)                         => decodeEnum(path, c1, c2)
      case Schema.Enum3(_, c1, c2, c3, _)                     => decodeEnum(path, c1, c2, c3)
      case Schema.Enum4(_, c1, c2, c3, c4, _)                 => decodeEnum(path, c1, c2, c3, c4)
      case Schema.Enum5(_, c1, c2, c3, c4, c5, _)             => decodeEnum(path, c1, c2, c3, c4, c5)
      case Schema.Enum6(_, c1, c2, c3, c4, c5, c6, _)         => decodeEnum(path, c1, c2, c3, c4, c5, c6)
      case Schema.Enum7(_, c1, c2, c3, c4, c5, c6, c7, _)     => decodeEnum(path, c1, c2, c3, c4, c5, c6, c7)
      case Schema.Enum8(_, c1, c2, c3, c4, c5, c6, c7, c8, _) => decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8)
      case Schema.Enum9(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9)
      case Schema.Enum10(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
      case Schema.Enum11(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11)
      case Schema.Enum12(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12)
      case Schema.Enum13(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13)
      case Schema.Enum14(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14)
      case Schema.Enum15(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15)
      case Schema.Enum16(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16)
      case Schema.Enum17(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)
      case Schema.Enum18(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18)
      case Schema.Enum19(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19)
      case Schema.Enum20(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20)
      case Schema.Enum21(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21)
      case Schema.Enum22(_, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22, _) =>
        decodeEnum(path, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18, c19, c20, c21, c22)
      case Schema.EnumN(_, cs, _) => decodeEnum(path, cs.toSeq: _*)
      case Schema.Dynamic(_)      => decodeValue(path, DynamicValue.schema)
      case _                      => fail(path, s"Unknown schema ${schema.getClass.getName}")
    }

  private def decodeTransform[A, B](path: Path, schema: Schema[B], f: B => Validation[String, A]): Result[A] =
    decodeValue(path, schema).flatMap(a => f(a).mapError(msg => MalformedFieldWithPath(path, msg)))

  private def decodeRecord[Z](path: Path, fields: Seq[Schema.Field[Z, _]]): Result[ListMap[String, _]] =
    decodeStructure(path, fields.map(f => f.name -> f.schema).toMap)

  private def decodeStructure(path: Path, fields: Map[String, Schema[_]]): Result[ListMap[String, Any]] = {
    @tailrec
    def readFields(m: ListMap[String, Any], index: Int): Result[ListMap[String, Any]] =
      Try(unpacker.unpackString()) match {
        case Failure(err) =>
          fail(path, s"Error reading field name on index $index: [$err]")
        case Success(fieldName) =>
          val actualPath = path :+ s"fieldId:[$fieldName]"
          fields.get(fieldName) match {
            case Some(fieldSchema) =>
              decodeValue(actualPath, fieldSchema) match {
                case f@ Validation.Failure(_, _) => f
                case Validation.Success(_, value) =>
                  if (index == fields.size) {
                                                        succeed(m.updated(fieldName, value))
                                                      } else {
                                                        readFields(m.updated(fieldName, value), index + 1)
                                                      }
              }
//                .flatMap(value => {
//                                    if (index == fields.size) {
//                                      succeed(m.updated(fieldName, value))
//                                    } else {
//                                      readFields(m.updated(fieldName, value), index + 1)
//                                    }
//                })
            case None =>
              fail(path, s"Could not find schema for field: [$fieldName] on index: $index")
          }
      }

    Try(unpacker.unpackMapHeader()) match {
      case Failure(err) => fail(path, s"Error reading object header: [$err]")
      case Success(sizeOfMap) if sizeOfMap != fields.size =>
        fail(path, s"Different than expected number of fields. Expected ${fields.size} but received $sizeOfMap")
      case Success(_) => readFields(ListMap.empty, 1)
    }
  }

  private def decodeSequence[Col, Elem](path: Path, schema: Schema.Sequence[Col, Elem, _]): Result[Col] =
    decodeIterable(path, schema.elementSchema).map(schema.fromChunk)

  private def decodeMap[K, V](path: Path, schema: Schema.Map[K, V]): Result[scala.collection.immutable.Map[K, V]] = {
    @tailrec
    def decodeElements(n: Int, m: scala.collection.mutable.Map[K, V]): Result[scala.collection.immutable.Map[K, V]] =
      if (n > 0) {
        (decodeValue(path, schema.keySchema), decodeValue(path, schema.valueSchema)) match {
          case (zio.prelude.Validation.Success(_, key), zio.prelude.Validation.Success(_, value)) => decodeElements(n - 1, m += ((key, value)))
          case (l, r) =>
            val key   = l.fold(_.map(_.message).toString(), _.toString)
            val value = r.fold(_.map(_.message).toString(), _.toString)
            fail(path, s"Error decoding Map element (key: $key; value: $value)")
        }
      } else {
        succeed(m.toMap)
      }

    Try(unpacker.unpackMapHeader()).fold(err => fail(path, "Can not decode Map header: " + err.getMessage), size => decodeElements(size, scala.collection.mutable.Map.empty[K, V]))
  }

  private def decodeSet[A](path: Path, schema: Schema.Set[A]): Result[scala.collection.immutable.Set[A]] =
    decodeIterable(path, schema.elementSchema).map(_.toSet)

  private def decodeIterable[A](path: Path, elementSchema: Schema[A]): Result[Chunk[A]] = {
    @tailrec
    def decodeElements(n: Int, cb: ChunkBuilder[A]): Result[Chunk[A]] =
      if (n > 0) {
        decodeValue(path, elementSchema) match {
          case zio.prelude.Validation.Success(_, elem) => decodeElements(n - 1, cb += elem)
          case failure@ zio.prelude.Validation.Failure(_, _)   => failure
        }
      } else {
        succeed(cb.result())
      }

    Try(unpacker.unpackArrayHeader())
      .fold(err => fail(path, s"Can not decode field header: [$err]"), size => decodeElements(size, ChunkBuilder.make[A]()))
  }

  private def withUnpacker[A](path: Path, standardType: StandardType[A])(fn: MessageUnpacker => A) =
    Try(fn(unpacker)).fold(err => fail(path, s"Cannot read $standardType: [$err]"), value => succeed(value))

  private def decodeString(path: Path): Result[String] =
    withUnpacker(path, StandardType.StringType)(_.unpackString())

  private def decodeByte(path: Path): Result[Byte] =
    withUnpacker(path, StandardType.ByteType)(_.unpackByte())

  private def decodeInt(path: Path): Result[Int] =
    withUnpacker(path, StandardType.IntType)(_.unpackInt())

  private def decodePrimitive[A](path: Path, standardType: StandardType[A]): Result[A] =
    standardType match {
      case StandardType.UnitType       => withUnpacker(path, StandardType.UnitType)(_.unpackNil())
      case StandardType.StringType     => decodeString(path)
      case StandardType.BoolType       => withUnpacker(path, StandardType.BoolType)(_.unpackBoolean())
      case StandardType.ByteType       => decodeByte(path)
      case StandardType.ShortType      => withUnpacker(path, StandardType.ShortType)(_.unpackShort())
      case StandardType.IntType        => withUnpacker(path, StandardType.IntType)(_.unpackInt())
      case StandardType.LongType       => withUnpacker(path, StandardType.LongType)(_.unpackLong())
      case StandardType.FloatType      => withUnpacker(path, StandardType.FloatType)(_.unpackFloat())
      case StandardType.DoubleType     => withUnpacker(path, StandardType.DoubleType)(_.unpackDouble())
      case StandardType.BigIntegerType => withUnpacker(path, StandardType.BigIntegerType)(_.unpackBigInteger())
      case StandardType.BigDecimalType =>
        decodeRecord(path, MessagePackCodec.bigDecimalStructure).flatMap { data =>
          val opt = for {
            unscaled  <- data.get("unscaled").asInstanceOf[Option[java.math.BigInteger]]
            precision <- data.get("precision").asInstanceOf[Option[Int]]
            scale     <- data.get("scale").asInstanceOf[Option[Int]]
            ctx       = new java.math.MathContext(precision)
          } yield new java.math.BigDecimal(unscaled, scale, ctx)

          opt match {
            case Some(value) => zio.prelude.Validation.succeed(value)
            case None        => fail(path, s"Invalid big decimal record $data")
          }
        }
      case StandardType.BinaryType =>
        withUnpacker(path, StandardType.BinaryType) { unpacker =>
          val size = unpacker.unpackBinaryHeader()
          Chunk.fromArray(unpacker.readPayload(size))
        }
      case StandardType.CharType =>
        decodeString(path).flatMap(
          decoded =>
            if (decoded.length == 1)
              succeed(decoded.charAt(0))
            else {
              fail(path, s"""Expected character, found string "$decoded"""")
            }
        )
      case StandardType.UUIDType =>
        decodeString(path).flatMap { uuid =>
          try succeed(UUID.fromString(uuid))
          catch {
            case NonFatal(err) => fail(path, s"Invalid UUID string: ${err.getMessage}")
          }
        }
      case StandardType.DayOfWeekType =>
        decodeInt(path).map(DayOfWeek.of)
      case StandardType.MonthType =>
        decodeInt(path).map(Month.of)
      case StandardType.MonthDayType =>
        decodeRecord(path, MessagePackCodec.monthDayStructure)
          .map(data => MonthDay.of(data("month").asInstanceOf[Int], data("day").asInstanceOf[Int]))
      case StandardType.PeriodType =>
        decodeRecord(path, MessagePackCodec.periodStructure)
          .map(data => Period.of(data("years").asInstanceOf[Int], data("months").asInstanceOf[Int], data("days").asInstanceOf[Int]))
      case StandardType.YearType =>
        decodeInt(path).map(Year.of)
      case StandardType.YearMonthType =>
        decodeRecord(path, MessagePackCodec.yearMonthStructure)
          .map(data => YearMonth.of(data("year").asInstanceOf[Int], data("month").asInstanceOf[Int]))
      case StandardType.ZoneIdType => decodeString(path).map(ZoneId.of)
      case StandardType.ZoneOffsetType =>
        decodeInt(path)
          .map(ZoneOffset.ofTotalSeconds)
      case StandardType.DurationType =>
        decodeRecord(path, MessagePackCodec.durationStructure)
          .map(data => Duration.ofSeconds(data("seconds").asInstanceOf[Long], data("nanos").asInstanceOf[Int].toLong))
      case StandardType.InstantType =>
        decodeString(path).map(v => Instant.parse(v))
      case StandardType.LocalDateType =>
        decodeString(path).map(LocalDate.parse(_))
      case StandardType.LocalTimeType =>
        decodeString(path).map(LocalTime.parse(_))
      case StandardType.LocalDateTimeType =>
        decodeString(path).map(LocalDateTime.parse(_))
      case StandardType.OffsetTimeType =>
        decodeString(path).map(OffsetTime.parse(_))
      case StandardType.OffsetDateTimeType =>
        decodeString(path).map(OffsetDateTime.parse(_))
      case StandardType.ZonedDateTimeType =>
        decodeString(path).map(ZonedDateTime.parse(_))
      case _ => fail(path, s"Unsupported primitive type $standardType")
    }

  private def decodeOptional[A](path: Path, schema: Schema.Optional[A]): Result[Option[A]] =
    decodeIterable(path, schema.schema).map(_.headOption)

  private def decodeTuple[A, B](path: Path, left: Schema[A], right: Schema[B]): Result[(A, B)] =
    Try(unpacker.unpackArrayHeader()).fold(
      err => {
        fail(path :+ "tuple", s"Failed to decode tuple size: ${err.getMessage}")
      },
      size =>
        if (size != 2) {
          fail(path :+ "tuple", s"Expected 2 elements but received $size.")
        } else {
          decodeValue(path :+ "tuple:left", left).flatMap(l => decodeValue(path :+ "tuple:right", right).map(l -> _))
        }
    )

  private def decodeEither[A, B](path: Path, left: Schema[A], right: Schema[B]): Result[Either[A, B]] =
    Try(unpacker.unpackMapHeader()).fold(
      err => fail(path, s"Error parsing Either structure: ${err.getMessage}"),
      size =>
        if (size != 1) {
          fail(path, s"Expected 1 elements but received $size.")
        } else {
          decodeString(path :+ "either").flatMap {
            case "left"  => decodeValue(path :+ "either:left", left).map(Left(_))
            case "right" => decodeValue(path :+ "either:right", right).map(Right(_))
            case str     => fail(path :+ "either", s"Unexpected field name: $str")
          }
        }
    )

  private def decodeEnum[Z, A](path: Path, cases: Schema.Case[Z, _]*): Result[Z] =
    decodeInt(path).flatMap { caseIndex =>
      if (caseIndex > cases.length) {
        fail(path, s"Error decoding enum with cases ${cases.map(_.id).mkString(", ")}, enum id out of range: ${caseIndex}")
      } else {
        val subtypeCase = cases(caseIndex)
        decodeValue(path :+ s"[case:${subtypeCase.id}]", subtypeCase.schema).asInstanceOf[Result[Z]]
      }
    }

  private def unsafeDecodeFields[Z](path: Path, fields: Schema.Field[Z, _]*): Result[Array[Any]] =
    decodeRecord(path, fields).map(_.values.toArray)

  private def caseClass0Decoder[Z](path: Path, schema: Schema.CaseClass0[Z]): Result[Z] =
    decodePrimitive(path, StandardType.UnitType).map(_ => schema.defaultConstruct())

  private def caseClass1Decoder[A, Z](path: Path, schema: Schema.CaseClass1[A, Z]): Result[Z] =
    unsafeDecodeFields(path, schema.field).flatMap { buffer =>
      succeed(schema.defaultConstruct(buffer(0).asInstanceOf[A]))
    }

  private def caseClass2Decoder[A1, A2, Z](path: Path, schema: Schema.CaseClass2[A1, A2, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2)
    } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2])

  private def caseClass3Decoder[A1, A2, A3, Z](path: Path, schema: Schema.CaseClass3[A1, A2, A3, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3)
    } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3])

  private def caseClass4Decoder[A1, A2, A3, A4, Z](path: Path, schema: Schema.CaseClass4[A1, A2, A3, A4, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4)
    } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4])

  private def caseClass5Decoder[A1, A2, A3, A4, A5, Z](path: Path, schema: Schema.CaseClass5[A1, A2, A3, A4, A5, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5)
    } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5])

  private def caseClass6Decoder[A1, A2, A3, A4, A5, A6, Z](path: Path, schema: Schema.CaseClass6[A1, A2, A3, A4, A5, A6, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6)
    } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6])

  private def caseClass7Decoder[A1, A2, A3, A4, A5, A6, A7, Z](path: Path, schema: Schema.CaseClass7[A1, A2, A3, A4, A5, A6, A7, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7)
    } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7])

  private def caseClass8Decoder[A1, A2, A3, A4, A5, A6, A7, A8, Z](path: Path, schema: Schema.CaseClass8[A1, A2, A3, A4, A5, A6, A7, A8, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field8)
    } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8])

  private def caseClass9Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z](path: Path, schema: Schema.CaseClass9[A1, A2, A3, A4, A5, A6, A7, A8, A9, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9)
    } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9])

  private def caseClass10Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z](path: Path, schema: Schema.CaseClass10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10)
    } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10])

  private def caseClass11Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z](path: Path, schema: Schema.CaseClass11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11)
    } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11])

  private def caseClass12Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z](path: Path, schema: Schema.CaseClass12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12)
    } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12])

  private def caseClass13Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z](path: Path, schema: Schema.CaseClass13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13)
    } yield schema.construct(buffer(0).asInstanceOf[A1], buffer(1).asInstanceOf[A2], buffer(2).asInstanceOf[A3], buffer(3).asInstanceOf[A4], buffer(4).asInstanceOf[A5], buffer(5).asInstanceOf[A6], buffer(6).asInstanceOf[A7], buffer(7).asInstanceOf[A8], buffer(8).asInstanceOf[A9], buffer(9).asInstanceOf[A10], buffer(10).asInstanceOf[A11], buffer(11).asInstanceOf[A12], buffer(12).asInstanceOf[A13])

  private def caseClass14Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z](path: Path, schema: Schema.CaseClass14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14)
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

  private def caseClass15Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z](path: Path, schema: Schema.CaseClass15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15)
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

  private def caseClass16Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z](path: Path, schema: Schema.CaseClass16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16)
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

  private def caseClass17Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z](path: Path, schema: Schema.CaseClass17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17)
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

  private def caseClass18Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z](path: Path, schema: Schema.CaseClass18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18)
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

  private def caseClass19Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z](path: Path, schema: Schema.CaseClass19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19)
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

  private def caseClass20Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z](path: Path, schema: Schema.CaseClass20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20)
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

  private def caseClass21Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z](path: Path, schema: Schema.CaseClass21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20, schema.field21)
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

  private def caseClass22Decoder[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z](path: Path, schema: Schema.CaseClass22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22, Z]): Result[Z] =
    for {
      buffer <- unsafeDecodeFields(path, schema.field1, schema.field2, schema.field3, schema.field4, schema.field5, schema.field6, schema.field7, schema.field9, schema.field9, schema.field10, schema.field11, schema.field12, schema.field13, schema.field14, schema.field15, schema.field16, schema.field17, schema.field18, schema.field19, schema.field20, schema.field21, schema.field22)
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

object MessagePackDecoder {
  type Path = Chunk[String]

  type Result[A] = Validation[DecodeError, A]

  private def succeed[A](a: => A): Result[A] =
    Validation.succeed(a)

  private def fail(path: Path, failure: String): Result[Nothing] =
    Validation.fail(MalformedFieldWithPath(path, failure))
}
