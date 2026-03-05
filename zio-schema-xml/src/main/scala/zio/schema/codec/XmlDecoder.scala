package zio.schema.codec

import java.time._
import java.util.{ Base64, UUID }

import scala.collection.immutable.ListMap
import scala.util.Try
import scala.xml._

import zio.schema.codec.DecodeError.ReadError
import zio.schema.{ DynamicValue, Fallback, Schema, StandardType }
import zio.{ Cause, Chunk, Unsafe }

object XmlDecoder {

  def decode[A](schema: Schema[A], xmlStr: String): Either[DecodeError, A] =
    Try(XML.loadString(xmlStr)).toEither match {
      case Left(ex)   => Left(ReadError(Cause.fail(ex), s"Invalid XML: ${ex.getMessage}"))
      case Right(elem) => decodeValue(schema, elem)
    }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private def decodeValue[A](schema: Schema[A], node: Node): Either[DecodeError, A] =
    schema match {
      case Schema.GenericRecord(_, structure, _) =>
        decodeRecord(structure.toChunk, node).map(_.asInstanceOf[A])
      case Schema.Sequence(element, f, _, _, _) =>
        val items = childElements(node, "item")
        val decoded = items.map(decodeValue(element, _))
        collectAll(decoded).map(chunk => f.asInstanceOf[Chunk[Any] => A](Chunk.fromIterable(chunk)))
      case mapSchema: Schema.Map[_, _] =>
        val ms      = mapSchema.asInstanceOf[Schema.Map[Any, Any]]
        val entries = childElements(node, "entry")
        val decoded = entries.map { entry =>
          for {
            k <- childElement(entry, "key").flatMap(decodeValue(ms.keySchema, _))
            v <- childElement(entry, "value").flatMap(decodeValue(ms.valueSchema, _))
          } yield (k, v)
        }
        collectAll(decoded).map(pairs => ListMap(pairs: _*).asInstanceOf[A])
      case setSchema: Schema.Set[_] =>
        val ss    = setSchema.asInstanceOf[Schema.Set[Any]]
        val items = childElements(node, "item")
        val decoded = items.map(decodeValue(ss.elementSchema, _))
        collectAll(decoded).map(_.toSet.asInstanceOf[A])
      case Schema.Transform(innerSchema, f, _, _, _) =>
        decodeValue(innerSchema, node).flatMap { raw =>
          f.asInstanceOf[Any => Either[String, A]](raw).left.map(msg => ReadError(Cause.empty, msg))
        }
      case Schema.Primitive(standardType, _) =>
        decodePrimitive(standardType, textContent(node)).map(_.asInstanceOf[A])
      case Schema.Tuple2(left, right, _) =>
        for {
          l <- childElement(node, "first").flatMap(decodeValue(left, _))
          r <- childElement(node, "second").flatMap(decodeValue(right, _))
        } yield (l, r).asInstanceOf[A]
      case optSchema: Schema.Optional[_] =>
        val os = optSchema.asInstanceOf[Schema.Optional[Any]]
        val children = node.child.filter(_.isInstanceOf[Elem])
        if (children.isEmpty && textContent(node).isEmpty)
          Right(None.asInstanceOf[A])
        else
          decodeValue(os.schema, node).map(v => Some(v).asInstanceOf[A])
      case eitherSchema: Schema.Either[_, _] =>
        val es = eitherSchema.asInstanceOf[Schema.Either[Any, Any]]
        childElement(node, "left") match {
          case Right(leftNode) => decodeValue(es.left, leftNode).map(l => Left(l).asInstanceOf[A])
          case Left(_) =>
            childElement(node, "right").flatMap(rightNode =>
              decodeValue(es.right, rightNode).map(r => Right(r).asInstanceOf[A])
            )
        }
      case fallbackSchema: Schema.Fallback[_, _] =>
        val fs        = fallbackSchema.asInstanceOf[Schema.Fallback[Any, Any]]
        val hasLeft   = childElement(node, "left")
        val hasRight  = childElement(node, "right")
        (hasLeft, hasRight) match {
          case (Right(l), Right(r)) =>
            for {
              lv <- decodeValue(fs.left, l)
              rv <- decodeValue(fs.right, r)
            } yield Fallback.Both(lv, rv).asInstanceOf[A]
          case (Right(l), _) =>
            decodeValue(fs.left, l).map(v => Fallback.Left(v).asInstanceOf[A])
          case (_, Right(r)) =>
            decodeValue(fs.right, r).map(v => Fallback.Right(v).asInstanceOf[A])
          case _ =>
            Left(ReadError(Cause.empty, "Expected left or right element in fallback"))
        }
      case lzy @ Schema.Lazy(_) =>
        decodeValue(lzy.schema, node)
      case Schema.CaseClass0(_, construct, _) =>
        Right(construct().asInstanceOf[A])
      case cc: Schema.Record[_] =>
        decodeCaseClass(cc.asInstanceOf[Schema.Record[A]], node)
      case enumSchema: Schema.Enum[_] =>
        decodeEnum(enumSchema.asInstanceOf[Schema.Enum[A]], node)
      case Schema.Dynamic(_) =>
        decodeValue(DynamicValue.schema, node).map(_.asInstanceOf[A])
      case _ =>
        Left(ReadError(Cause.empty, s"Unsupported schema type: ${schema.getClass.getSimpleName}"))
    }

  private def decodeCaseClass[A](schema: Schema.Record[A], node: Node): Either[DecodeError, A] = {
    val values = schema.fields.map { field =>
      childElement(node, field.name) match {
        case Right(fieldNode) => decodeValue(field.schema.asInstanceOf[Schema[Any]], fieldNode)
        case Left(_) =>
          field.schema match {
            case _: Schema.Optional[_] => Right(None)
            case _                     => Left(ReadError(Cause.empty, s"Missing field: ${field.name}"))
          }
      }
    }
    collectAll(values).flatMap { decodedValues =>
      Unsafe.unsafe { implicit unsafe =>
        Try(schema.construct(Chunk.fromIterable(decodedValues))).toEither match {
          case Right(result) => result.left.map(msg => ReadError(Cause.empty, msg))
          case Left(ex)      => Left(ReadError(Cause.fail(ex), s"Construction error: ${ex.getMessage}"))
        }
      }
    }
  }

  private def decodeEnum[A](schema: Schema.Enum[A], node: Node): Either[DecodeError, A] = {
    val typeAttr = Option(node \@ "type").filter(_.nonEmpty)
    typeAttr match {
      case Some(caseName) =>
        schema.cases.find(_.id == caseName) match {
          case Some(c) =>
            val cs = c.asInstanceOf[Schema.Case[A, Any]]
            childElement(node, "value")
              .flatMap(decodeValue(cs.schema, _))
              .map(cs.construct)
          case None =>
            Left(ReadError(Cause.empty, s"Unknown enum case: $caseName"))
        }
      case None =>
        Left(ReadError(Cause.empty, "Missing type attribute on enum element"))
    }
  }

  private def decodeRecord(fields: Chunk[Schema.Field[_, _]], node: Node): Either[DecodeError, ListMap[String, Any]] = {
    val results = fields.map { field =>
      childElement(node, field.name) match {
        case Right(fieldNode) =>
          decodeValue(field.schema.asInstanceOf[Schema[Any]], fieldNode).map(v => (field.name, v))
        case Left(err) => Left(err)
      }
    }
    collectAll(results).map(pairs => ListMap(pairs: _*))
  }

  private[codec] def decodePrimitive[A](standardType: StandardType[A], text: String): Either[DecodeError, A] = {
    def parse[T](f: => T): Either[DecodeError, T] =
      Try(f).toEither.left.map(ex => ReadError(Cause.fail(ex), s"Failed to parse ${standardType}: ${ex.getMessage}"))

    val result: Either[DecodeError, Any] = standardType match {
      case StandardType.UnitType           => Right(())
      case StandardType.StringType         => Right(text)
      case StandardType.BoolType           => parse(text.toBoolean)
      case StandardType.ByteType           => parse(text.toByte)
      case StandardType.ShortType          => parse(text.toShort)
      case StandardType.IntType            => parse(text.toInt)
      case StandardType.LongType           => parse(text.toLong)
      case StandardType.FloatType          => parse(text.toFloat)
      case StandardType.DoubleType         => parse(text.toDouble)
      case StandardType.BigIntegerType     => parse(new java.math.BigInteger(text))
      case StandardType.BigDecimalType     => parse(new java.math.BigDecimal(text))
      case StandardType.BinaryType         => parse(Chunk.fromArray(Base64.getDecoder.decode(text)))
      case StandardType.CharType           => if (text.length == 1) Right(text.charAt(0)) else Left(ReadError(Cause.empty, s"Expected single char, got: $text"))
      case StandardType.UUIDType           => parse(UUID.fromString(text))
      case StandardType.DayOfWeekType      => parse(DayOfWeek.valueOf(text))
      case StandardType.MonthType          => parse(Month.valueOf(text))
      case StandardType.MonthDayType       => parse(MonthDay.parse(text))
      case StandardType.PeriodType         => parse(Period.parse(text))
      case StandardType.YearType           => parse(Year.of(text.toInt))
      case StandardType.YearMonthType      => parse(YearMonth.parse(text))
      case StandardType.ZoneIdType         => parse(ZoneId.of(text))
      case StandardType.ZoneOffsetType     => parse(ZoneOffset.of(text))
      case StandardType.DurationType       => parse(Duration.parse(text))
      case StandardType.InstantType        => parse(Instant.parse(text))
      case StandardType.LocalDateType      => parse(LocalDate.parse(text))
      case StandardType.LocalTimeType      => parse(LocalTime.parse(text))
      case StandardType.LocalDateTimeType  => parse(LocalDateTime.parse(text))
      case StandardType.OffsetTimeType     => parse(OffsetTime.parse(text))
      case StandardType.OffsetDateTimeType => parse(OffsetDateTime.parse(text))
      case StandardType.ZonedDateTimeType  => parse(ZonedDateTime.parse(text))
      case StandardType.CurrencyType       => parse(java.util.Currency.getInstance(text))
    }
    result.map(_.asInstanceOf[A])
  }

  private def childElements(parent: Node, name: String): Seq[Node] =
    (parent \ name).filter(_.isInstanceOf[Elem])

  private def childElement(parent: Node, name: String): Either[DecodeError, Node] =
    childElements(parent, name).headOption match {
      case Some(n) => Right(n)
      case None    => Left(ReadError(Cause.empty, s"Missing element: $name"))
    }

  private def textContent(node: Node): String =
    node.text.trim

  private def collectAll[E, T](results: Seq[Either[E, T]]): Either[E, Seq[T]] =
    results.foldRight(Right(Nil): Either[E, List[T]]) { (item, acc) =>
      for {
        list <- acc
        v    <- item
      } yield v :: list
    }
}
