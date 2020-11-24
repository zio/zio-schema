package zio.schema.codec.json

import java.time.DayOfWeek
import zio.json.JsonDecoder
import zio.json.internal.RetractReader
import zio.schema.StandardType

object CodecDecoder {

  final def primitiveDecoder[A](standardType: StandardType[A]): JsonDecoder[A] =
    standardType match {
      case StandardType.UnitType          => unitDecoder
      case StandardType.StringType        => JsonDecoder[String]
      case StandardType.BoolType          => JsonDecoder[Boolean]
      case StandardType.ShortType         => JsonDecoder[Short]
      case StandardType.IntType           => JsonDecoder[Int]
      case StandardType.LongType          => JsonDecoder[Long]
      case StandardType.FloatType         => JsonDecoder[Float]
      case StandardType.DoubleType        => JsonDecoder[Double]
      case StandardType.ByteType          => JsonDecoder[Byte]
      case StandardType.CharType          => JsonDecoder[Char]
      case StandardType.DayOfWeekType     => JsonDecoder[DayOfWeek]
      case StandardType.Duration(_)       => JsonDecoder[java.time.Duration]
      case StandardType.Instant(_)        => JsonDecoder[java.time.Instant]
      case StandardType.LocalDate(_)      => JsonDecoder[java.time.LocalDate]
      case StandardType.LocalDateTime(_)  => JsonDecoder[java.time.LocalDateTime]
      case StandardType.LocalTime(_)      => JsonDecoder[java.time.LocalTime]
      case StandardType.Month             => JsonDecoder[java.time.Month]
      case StandardType.MonthDay          => JsonDecoder[java.time.MonthDay]
      case StandardType.OffsetDateTime(_) => JsonDecoder[java.time.OffsetDateTime]
      case StandardType.OffsetTime(_)     => JsonDecoder[java.time.OffsetTime]
      case StandardType.Period            => JsonDecoder[java.time.Period]
      case StandardType.Year              => JsonDecoder[java.time.Year]
      case StandardType.YearMonth         => JsonDecoder[java.time.YearMonth]
      case StandardType.ZonedDateTime(_)  => JsonDecoder[java.time.ZonedDateTime]
      case StandardType.ZoneId            => JsonDecoder[java.time.ZoneId]
      case StandardType.ZoneOffset        => JsonDecoder[java.time.ZoneOffset]
    }

  private val unitDecoder: JsonDecoder[Unit] =
    (_: List[JsonDecoder.JsonError], _: RetractReader) => ()

}
