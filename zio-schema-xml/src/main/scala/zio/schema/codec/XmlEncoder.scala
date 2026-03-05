package zio.schema.codec

import java.time._
import java.util.{ Base64, UUID }

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.{ DynamicValue, Fallback, Schema, StandardType }

object XmlEncoder {

  def encode[A](schema: Schema[A], value: A): String = {
    val sb = new StringBuilder
    sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
    encodeValue(sb, "root", schema, value)
    sb.toString()
  }

  //scalafmt: { maxColumn = 400, optIn.configStyleArguments = false }
  private def encodeValue[A](sb: StringBuilder, tag: String, schema: Schema[A], value: A): Unit =
    (schema, value) match {
      case (Schema.GenericRecord(_, structure, _), v: Map[String, _]) =>
        sb.append(s"<$tag>")
        structure.toChunk.foreach { field =>
          v.get(field.name).foreach(fv => encodeValue(sb, field.name, field.schema.asInstanceOf[Schema[Any]], fv))
        }
        sb.append(s"</$tag>")
      case (Schema.Sequence(element, _, g, _, _), v) =>
        sb.append(s"<$tag>")
        g(v).foreach(item => encodeValue(sb, "item", element, item))
        sb.append(s"</$tag>")
      case (mapSchema: Schema.Map[_, _], map: Map[_, _]) =>
        val ms = mapSchema.asInstanceOf[Schema.Map[Any, Any]]
        val m  = map.asInstanceOf[scala.collection.immutable.Map[Any, Any]]
        sb.append(s"<$tag>")
        m.foreach { case (k, v) =>
          sb.append("<entry>")
          encodeValue(sb, "key", ms.keySchema, k)
          encodeValue(sb, "value", ms.valueSchema, v)
          sb.append("</entry>")
        }
        sb.append(s"</$tag>")
      case (setSchema: Schema.Set[_], set: Set[_]) =>
        val ss = setSchema.asInstanceOf[Schema.Set[Any]]
        sb.append(s"<$tag>")
        set.asInstanceOf[scala.collection.immutable.Set[Any]].foreach(item => encodeValue(sb, "item", ss.elementSchema, item))
        sb.append(s"</$tag>")
      case (Schema.Transform(schema, _, g, _, _), _) =>
        g(value).foreach(v => encodeValue(sb, tag, schema, v))
      case (Schema.Primitive(standardType, _), v) =>
        encodePrimitive(sb, tag, standardType, v)
      case (Schema.Tuple2(left, right, _), v @ (_, _)) =>
        sb.append(s"<$tag>")
        encodeValue(sb, "first", left, v._1)
        encodeValue(sb, "second", right, v._2)
        sb.append(s"</$tag>")
      case (optSchema: Schema.Optional[_], v: Option[_]) =>
        v match {
          case Some(inner) => encodeValue(sb, tag, optSchema.asInstanceOf[Schema.Optional[Any]].schema, inner)
          case None        => sb.append(s"<$tag/>")
        }
      case (eitherSchema: Schema.Either[_, _], v: scala.util.Either[_, _]) =>
        val es = eitherSchema.asInstanceOf[Schema.Either[Any, Any]]
        sb.append(s"<$tag>")
        v match {
          case Left(l)  => encodeValue(sb, "left", es.left, l)
          case Right(r) => encodeValue(sb, "right", es.right, r)
        }
        sb.append(s"</$tag>")
      case (fallbackSchema: Schema.Fallback[_, _], v: Fallback[_, _]) =>
        val fs = fallbackSchema.asInstanceOf[Schema.Fallback[Any, Any]]
        val fv = v.asInstanceOf[Fallback[Any, Any]]
        sb.append(s"<$tag>")
        fv match {
          case Fallback.Left(l)       => encodeValue(sb, "left", fs.left, l)
          case Fallback.Right(r)      => encodeValue(sb, "right", fs.right, r)
          case Fallback.Both(l, r) =>
            encodeValue(sb, "left", fs.left, l)
            encodeValue(sb, "right", fs.right, r)
        }
        sb.append(s"</$tag>")
      case (lzy @ Schema.Lazy(_), v) =>
        encodeValue(sb, tag, lzy.schema, v)
      case (Schema.CaseClass0(_, _, _), _) =>
        sb.append(s"<$tag/>")
      case (cc: Schema.Record[_], v) =>
        sb.append(s"<$tag>")
        cc.fields.foreach { field =>
          val fieldValue = field.asInstanceOf[Schema.Field[Any, Any]].get(v)
          encodeValue(sb, field.name, field.schema.asInstanceOf[Schema[Any]], fieldValue)
        }
        sb.append(s"</$tag>")
      case (enumSchema: Schema.Enum[_], v) =>
        val cases = enumSchema.cases
        val matchedCase = cases.find(c => c.asInstanceOf[Schema.Case[Any, Any]].deconstructOption(v).isDefined)
        matchedCase.foreach { c =>
          val cs    = c.asInstanceOf[Schema.Case[Any, Any]]
          val inner = cs.deconstruct(v)
          sb.append(s"""<$tag type="${escapeXml(cs.id)}">""")
          encodeValue(sb, "value", cs.schema.asInstanceOf[Schema[Any]], inner)
          sb.append(s"</$tag>")
        }
      case (Schema.Dynamic(_), v) =>
        encodeValue(sb, tag, DynamicValue.schema, v)
      case (_, _) => ()
    }

  private def encodePrimitive[A](sb: StringBuilder, tag: String, standardType: StandardType[A], value: A): Unit = {
    val text = primitiveToString(standardType, value)
    sb.append(s"<$tag>")
    sb.append(escapeXml(text))
    sb.append(s"</$tag>")
  }

  private[codec] def primitiveToString[A](standardType: StandardType[A], value: A): String =
    (standardType, value) match {
      case (StandardType.UnitType, _)                                   => ""
      case (StandardType.StringType, s: String)                         => s
      case (StandardType.BoolType, b: Boolean)                          => b.toString
      case (StandardType.ByteType, b: Byte)                             => b.toString
      case (StandardType.ShortType, s: Short)                           => s.toString
      case (StandardType.IntType, i: Int)                               => i.toString
      case (StandardType.LongType, l: Long)                             => l.toString
      case (StandardType.FloatType, f: Float)                           => f.toString
      case (StandardType.DoubleType, d: Double)                         => d.toString
      case (StandardType.BigIntegerType, v: java.math.BigInteger)       => v.toString
      case (StandardType.BigDecimalType, v: java.math.BigDecimal)       => v.toString
      case (StandardType.BinaryType, bytes: Chunk[Byte @unchecked])     => Base64.getEncoder.encodeToString(bytes.toArray)
      case (StandardType.CharType, c: Char)                             => c.toString
      case (StandardType.UUIDType, u: UUID)                             => u.toString
      case (StandardType.DayOfWeekType, v: DayOfWeek)                   => v.toString
      case (StandardType.MonthType, v: Month)                           => v.toString
      case (StandardType.MonthDayType, v: MonthDay)                     => v.toString
      case (StandardType.PeriodType, v: Period)                         => v.toString
      case (StandardType.YearType, v: Year)                             => v.getValue.toString
      case (StandardType.YearMonthType, v: YearMonth)                   => v.toString
      case (StandardType.ZoneIdType, v: ZoneId)                         => v.getId
      case (StandardType.ZoneOffsetType, v: ZoneOffset)                 => v.toString
      case (StandardType.DurationType, v: Duration)                     => v.toString
      case (StandardType.InstantType, v: Instant)                       => v.toString
      case (StandardType.LocalDateType, v: LocalDate)                   => v.toString
      case (StandardType.LocalTimeType, v: LocalTime)                   => v.toString
      case (StandardType.LocalDateTimeType, v: LocalDateTime)           => v.toString
      case (StandardType.OffsetTimeType, v: OffsetTime)                 => v.toString
      case (StandardType.OffsetDateTimeType, v: OffsetDateTime)         => v.toString
      case (StandardType.ZonedDateTimeType, v: ZonedDateTime)           => v.toString
      case (StandardType.CurrencyType, v: java.util.Currency)           => v.getCurrencyCode
      case (_, _)                                                       => value.toString
    }

  private[codec] def escapeXml(s: String): String = {
    val sb = new StringBuilder(s.length)
    var i  = 0
    while (i < s.length) {
      s.charAt(i) match {
        case '&'  => sb.append("&amp;")
        case '<'  => sb.append("&lt;")
        case '>'  => sb.append("&gt;")
        case '"'  => sb.append("&quot;")
        case '\'' => sb.append("&apos;")
        case c    => sb.append(c)
      }
      i += 1
    }
    sb.toString()
  }
}
