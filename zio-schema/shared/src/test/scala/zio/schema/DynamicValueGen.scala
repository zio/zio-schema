package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.random.Random
import zio.test._

object DynamicValueGen {

  def anyPrimitiveDynamicValue[A](standardType: StandardType[A]): Gen[Random with Sized, DynamicValue.Primitive[A]] = {
    def gen[A1](typ: StandardType[A1], gen: Gen[Random with Sized, A1]) = gen.map(DynamicValue.Primitive(_, typ))

    standardType match {
      case typ: StandardType.BinaryType.type     => gen(typ, Gen.chunkOf(Gen.anyByte))
      case typ: StandardType.BoolType.type       => gen(typ, Gen.oneOf(Gen.const(true), Gen.const(false)))
      case typ: StandardType.CharType.type       => gen(typ, Gen.anyChar)
      case typ: StandardType.DoubleType.type     => gen(typ, Gen.anyDouble)
      case typ: StandardType.StringType.type     => gen(typ, Gen.anyString)
      case typ: StandardType.ShortType.type      => gen(typ, Gen.anyShort)
      case typ: StandardType.IntType.type        => gen(typ, Gen.anyInt)
      case typ: StandardType.LongType.type       => gen(typ, Gen.anyLong)
      case typ: StandardType.FloatType.type      => gen(typ, Gen.anyFloat)
      case typ: StandardType.BigDecimalType.type => gen(typ, Gen.anyDouble.map(d => java.math.BigDecimal.valueOf(d)))
      case typ: StandardType.BigIntegerType.type => gen(typ, Gen.anyLong.map(n => java.math.BigInteger.valueOf(n)))
      case typ: StandardType.DayOfWeekType.type  => gen(typ, JavaTimeGen.anyDayOfWeek)
      case typ: StandardType.Duration            => gen(typ, JavaTimeGen.anyDuration)
      case typ: StandardType.Instant             => gen(typ, JavaTimeGen.anyInstant)
      case typ: StandardType.LocalDate           => gen(typ, JavaTimeGen.anyLocalDate)
      case typ: StandardType.LocalDateTime       => gen(typ, JavaTimeGen.anyLocalDateTime)
      case typ: StandardType.LocalTime           => gen(typ, JavaTimeGen.anyLocalTime)
      case typ: StandardType.Month.type          => gen(typ, JavaTimeGen.anyMonth)
      case typ: StandardType.MonthDay.type       => gen(typ, JavaTimeGen.anyMonthDay)
      case typ: StandardType.OffsetDateTime      => gen(typ, JavaTimeGen.anyOffsetDateTime)
      case typ: StandardType.OffsetTime          => gen(typ, JavaTimeGen.anyOffsetTime)
      case typ: StandardType.Period.type         => gen(typ, JavaTimeGen.anyPeriod)
      case typ: StandardType.Year.type           => gen(typ, JavaTimeGen.anyYear)
      case typ: StandardType.YearMonth.type      => gen(typ, JavaTimeGen.anyYearMonth)
      case typ: StandardType.ZonedDateTime       => gen(typ, JavaTimeGen.anyZonedDateTime)
      case typ: StandardType.ZoneId.type         => gen(typ, JavaTimeGen.anyZoneId)
      case typ: StandardType.ZoneOffset.type     => gen(typ, JavaTimeGen.anyZoneOffset)
      case typ: StandardType.UnitType.type       => Gen.const(DynamicValue.Primitive((), typ))
    }
  }

  def anyDynamicValueOfSchema[A](schema: Schema[A]): Gen[Random with Sized, DynamicValue] =
    schema match {
      case Schema.Primitive(standardType)    => anyPrimitiveDynamicValue(standardType)
      case s: Schema.Record[A]               => anyDynamicValueWithStructure(s.structure)
      case Schema.Enumeration(structure)     => anyDynamicValueOfEnumeration(structure)
      case Schema.Enum1(case1)               => anyDynamicValueOfEnum(Chunk(case1))
      case Schema.Enum2(case1, case2)        => anyDynamicValueOfEnum(Chunk(case1, case2))
      case Schema.Enum3(case1, case2, case3) => anyDynamicValueOfEnum(Chunk(case1, case2, case3))
      case Schema.EnumN(cases)               => anyDynamicValueOfEnum(Chunk.fromIterable(cases))
      case Schema.Sequence(schema, _, _)     => Gen.chunkOf(anyDynamicValueOfSchema(schema)).map(DynamicValue.Sequence(_))
      case Schema.Optional(schema)           => Gen.oneOf(anyDynamicSomeValueOfSchema(schema), Gen.const(DynamicValue.NoneValue))
      case Schema.Tuple(left, right)         => anyDynamicTupleValue(left, right)
      case Schema.EitherSchema(left, right) =>
        Gen.oneOf(anyDynamicLeftValueOfSchema(left), anyDynamicRightValueOfSchema(right))
      case Schema.CaseObject(instance)    => Gen.const(DynamicValue.Singleton(instance))
      case Schema.Transform(schema, _, _) => anyDynamicValueOfSchema(schema).map(DynamicValue.Transform(_))
      case Schema.Fail(message)           => Gen.const(DynamicValue.Error(message))
    }

  def anyDynamicLeftValueOfSchema[A](schema: Schema[A]): Gen[Random with Sized, DynamicValue.LeftValue] =
    anyDynamicValueOfSchema(schema).map(DynamicValue.LeftValue(_))

  def anyDynamicRightValueOfSchema[A](schema: Schema[A]): Gen[Random with Sized, DynamicValue.RightValue] =
    anyDynamicValueOfSchema(schema).map(DynamicValue.RightValue(_))

  def anyDynamicSomeValueOfSchema[A](schema: Schema[A]): Gen[Random with Sized, DynamicValue.SomeValue] =
    anyDynamicValueOfSchema(schema).map(DynamicValue.SomeValue(_))

  def anyDynamicTupleValue[A, B](left: Schema[A], right: Schema[B]): Gen[Random with Sized, DynamicValue.Tuple] =
    anyDynamicValueOfSchema(left).zip(anyDynamicValueOfSchema(right)).map {
      case (l, r) => DynamicValue.Tuple(l, r)
    }

  def anyDynamicValueOfEnumeration(
    structure: ListMap[String, Schema[_]]
  ): Gen[Random with Sized, DynamicValue.Enumeration] =
    for {
      index <- Gen.int(0, structure.size - 1)
      value <- anyDynamicValueOfSchema(structure.values.toSeq(index))
    } yield DynamicValue.Enumeration(structure.keys.toSeq(index) -> value)

  def anyDynamicValueOfEnum[A](cases: Chunk[Schema.Case[_, A]]): Gen[Random with Sized, DynamicValue.Enumeration] =
    for {
      index <- Gen.int(0, cases.size - 1)
      value <- anyDynamicValueOfSchema(cases(index).codec)
    } yield DynamicValue.Enumeration(cases(index).id -> value)

  def anyDynamicValueWithStructure(structure: Chunk[Schema.Field[_]]): Gen[Random with Sized, DynamicValue.Record] =
    Gen
      .crossAll(
        structure
          .map(field => Gen.const(field.label).zip(anyDynamicValueOfSchema(field.schema)))
      )
      .map { values =>
        DynamicValue.Record(ListMap.empty ++ values)
      }

}
