package zio.schema.codec.json

import zio.json.JsonEncoder.string
import zio.json.internal.Write
import zio.json.{ JsonEncoder, JsonFieldEncoder }
import zio.schema.StandardType

object CodecEncoder {

  final def primitiveEncoder[A](standardType: StandardType[A]): JsonEncoder[A] =
    standardType match {
      case StandardType.UnitType          => unitEncoder
      case StandardType.StringType        => JsonEncoder[String]
      case StandardType.BoolType          => JsonEncoder[Boolean]
      case StandardType.ShortType         => JsonEncoder[Short]
      case StandardType.IntType           => JsonEncoder[Int]
      case StandardType.LongType          => JsonEncoder[Long]
      case StandardType.FloatType         => JsonEncoder[Float]
      case StandardType.DoubleType        => JsonEncoder[Double]
      case StandardType.ByteType          => JsonEncoder[Byte]
      case StandardType.CharType          => JsonEncoder[Char]
      case StandardType.DayOfWeekType     => JsonEncoder[java.time.DayOfWeek]
      case StandardType.Duration(_)       => JsonEncoder[java.time.Duration]
      case StandardType.Instant(_)        => JsonEncoder[java.time.Instant]
      case StandardType.LocalDate(_)      => JsonEncoder[java.time.LocalDate]
      case StandardType.LocalDateTime(_)  => JsonEncoder[java.time.LocalDateTime]
      case StandardType.LocalTime(_)      => JsonEncoder[java.time.LocalTime]
      case StandardType.Month             => JsonEncoder[java.time.Month]
      case StandardType.MonthDay          => JsonEncoder[java.time.MonthDay]
      case StandardType.OffsetDateTime(_) => JsonEncoder[java.time.OffsetDateTime]
      case StandardType.OffsetTime(_)     => JsonEncoder[java.time.OffsetTime]
      case StandardType.Period            => JsonEncoder[java.time.Period]
      case StandardType.Year              => JsonEncoder[java.time.Year]
      case StandardType.YearMonth         => JsonEncoder[java.time.YearMonth]
      case StandardType.ZonedDateTime(_)  => JsonEncoder[java.time.ZonedDateTime]
      case StandardType.ZoneId            => JsonEncoder[java.time.ZoneId]
      case StandardType.ZoneOffset        => JsonEncoder[java.time.ZoneOffset]
    }

  private val unitEncoder: JsonEncoder[Unit] =
    (_: Unit, _: Option[Int], _: Write) => ()

  // A modified version of zio.json.EncoderLowPriority2.keyValueIterable.
  final def recordEncoder[A](keyValueEncoders: Map[String, JsonEncoder[_]]): JsonEncoder[A] = new JsonEncoder[A] {

    def unsafeEncode(record: A, indent: Option[Int], out: Write): Unit = {
      if (keyValueEncoders.isEmpty) return out.write("{}")

      val K = JsonFieldEncoder.string

      // FIXME: Can we do this safely?
      val product = record.asInstanceOf[Product]
      val elements = for (i <- 0 until product.productArity) yield {
        val name  = product.productElementName(i)
        val value = product.productElement(i)
        name -> value
      }

      out.write('{')
      val indent_ = JsonEncoder.bump(indent)
      JsonEncoder.pad(indent_, out)
      var first = true
      elements.foreach {
        case (k, a) =>
          // FIXME: Can we do this safely?
          val A = keyValueEncoders.getOrElse(k, throw new Exception(s"No Schema found for field '$k'."))
          // FIXME: Can we do this safely?
          if (!A.isNothing(a.asInstanceOf)) {
            if (first)
              first = false
            else {
              out.write(',')
              if (!indent.isEmpty)
                JsonEncoder.pad(indent_, out)
            }

            string.unsafeEncode(K.unsafeEncodeField(k), indent_, out)
            if (indent.isEmpty) out.write(':')
            else out.write(" : ")
            // FIXME: Can we do this safely?
            A.unsafeEncode(a.asInstanceOf, indent_, out)
          }
      }
      JsonEncoder.pad(indent, out)
      out.write('}')
    }
  }
}
