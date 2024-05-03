package zio.schema.codec

import java.util.Base64

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.json.ast.Json
import zio.schema.annotation.directDynamicMapping
import zio.schema.{ DynamicValue, Schema, StandardType, TypeId }

package object json {
  implicit val schemaJson: Schema[Json] =
    Schema.dynamicValue.transform(toJson, fromJson).annotate(directDynamicMapping())

  private def toJson(dv: DynamicValue): Json =
    dv match {
      case DynamicValue.Record(_, values) =>
        values.foldLeft(Json.Obj()) { case (obj, (name, value)) => (name, toJson(value)) +: obj }
      case DynamicValue.Enumeration(_, _) =>
        throw new Exception("DynamicValue.Enumeration is unsupported")
      case DynamicValue.Sequence(values) =>
        Json.Arr(values.map(toJson))
      case DynamicValue.Dictionary(_) =>
        throw new Exception("DynamicValue.Dictionary is unsupported")
      case DynamicValue.SetValue(values) =>
        Json.Arr(Chunk.fromIterable(values.map(toJson)))
      case DynamicValue.Primitive(value, standardType) =>
        standardType.asInstanceOf[StandardType[_]] match {
          case StandardType.UnitType   => Json.Obj()
          case StandardType.StringType => Json.Str(value.asInstanceOf[String])
          case StandardType.BoolType   => Json.Bool(value.asInstanceOf[Boolean])
          case StandardType.ByteType   => Json.Num(value.asInstanceOf[Byte])
          case StandardType.ShortType  => Json.Num(value.asInstanceOf[Short])
          case StandardType.IntType    => Json.Num(value.asInstanceOf[Int])
          case StandardType.LongType   => Json.Num(value.asInstanceOf[Long])
          case StandardType.FloatType  => Json.Num(value.asInstanceOf[Float])
          case StandardType.DoubleType => Json.Num(value.asInstanceOf[Double])
          case StandardType.BinaryType =>
            Json.Str(Base64.getEncoder.encodeToString(value.asInstanceOf[Chunk[Byte]].toArray))
          case StandardType.CharType           => Json.Str(value.asInstanceOf[Char].toString)
          case StandardType.UUIDType           => Json.Str(value.asInstanceOf[java.util.UUID].toString)
          case StandardType.BigDecimalType     => Json.Num(value.asInstanceOf[java.math.BigDecimal])
          case StandardType.BigIntegerType     => Json.Num(BigDecimal(value.asInstanceOf[java.math.BigInteger]))
          case StandardType.DayOfWeekType      => Json.Str(value.asInstanceOf[java.time.DayOfWeek].toString)
          case StandardType.MonthType          => Json.Str(value.asInstanceOf[java.time.Month].toString)
          case StandardType.MonthDayType       => Json.Str(value.asInstanceOf[java.time.MonthDay].toString)
          case StandardType.PeriodType         => Json.Str(value.asInstanceOf[java.time.Period].toString)
          case StandardType.YearType           => Json.Num(value.asInstanceOf[java.time.Year].getValue)
          case StandardType.YearMonthType      => Json.Str(value.asInstanceOf[java.time.YearMonth].toString)
          case StandardType.ZoneIdType         => Json.Str(value.asInstanceOf[java.time.ZoneId].toString)
          case StandardType.ZoneOffsetType     => Json.Str(value.asInstanceOf[java.time.ZoneOffset].toString)
          case StandardType.DurationType       => Json.Str(value.asInstanceOf[java.time.Duration].toString)
          case StandardType.InstantType        => Json.Str(value.asInstanceOf[java.time.Instant].toString)
          case StandardType.LocalDateType      => Json.Str(value.asInstanceOf[java.time.LocalDate].toString)
          case StandardType.LocalTimeType      => Json.Str(value.asInstanceOf[java.time.LocalTime].toString)
          case StandardType.LocalDateTimeType  => Json.Str(value.asInstanceOf[java.time.LocalDateTime].toString)
          case StandardType.OffsetTimeType     => Json.Str(value.asInstanceOf[java.time.OffsetTime].toString)
          case StandardType.OffsetDateTimeType => Json.Str(value.asInstanceOf[java.time.OffsetDateTime].toString)
          case StandardType.ZonedDateTimeType  => Json.Str(value.asInstanceOf[java.time.ZonedDateTime].toString)
          case StandardType.CurrencyType       => Json.Str(value.asInstanceOf[java.util.Currency].toString)
        }
      case DynamicValue.Singleton(_)       => Json.Obj()
      case DynamicValue.SomeValue(value)   => toJson(value)
      case DynamicValue.NoneValue          => Json.Null
      case DynamicValue.Tuple(left, right) => Json.Arr(Chunk(toJson(left), toJson(right)))
      case DynamicValue.LeftValue(value)   => Json.Obj("Left" -> toJson(value))
      case DynamicValue.RightValue(value)  => Json.Obj("Right" -> toJson(value))
      case DynamicValue.BothValue(_, _)    => throw new Exception("DynamicValue.BothValue is unsupported")
      case DynamicValue.DynamicAst(_)      => throw new Exception("DynamicValue.DynamicAst is unsupported")
      case DynamicValue.Error(_)           => throw new Exception("DynamicValue.Error is unsupported")
    }

  private def fromJson(json: Json): DynamicValue =
    json match {
      case Json.Null        => DynamicValue.NoneValue
      case Json.Bool(value) => DynamicValue.Primitive(value, StandardType.BoolType)
      case Json.Num(value)  => DynamicValue.Primitive(value, StandardType.BigDecimalType)
      case Json.Str(value)  => DynamicValue.Primitive(value, StandardType.StringType)
      case Json.Arr(values) => DynamicValue.Sequence(values.map(fromJson))
      case Json.Obj(values) =>
        DynamicValue.Record(
          TypeId.parse("Json.Obj"),
          ListMap(values.map { case (name, value) => (name, fromJson(value)) }.toList: _*)
        )
    }
}
