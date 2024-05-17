package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.test._

object DynamicValueGen {

  def anyPrimitiveDynamicValue[A](standardType: StandardType[A]): Gen[Sized, DynamicValue.Primitive[A]] = {
    def gen[A1](typ: StandardType[A1], gen: Gen[Sized, A1]) = gen.map(DynamicValue.Primitive(_, typ))

    standardType match {
      case typ: StandardType.BinaryType.type         => gen(typ, Gen.chunkOf(Gen.byte))
      case typ: StandardType.BoolType.type           => gen(typ, Gen.oneOf(Gen.const(true), Gen.const(false)))
      case typ: StandardType.CharType.type           => gen(typ, Gen.asciiChar)
      case typ: StandardType.DoubleType.type         => gen(typ, Gen.double)
      case typ: StandardType.StringType.type         => gen(typ, Gen.string)
      case typ: StandardType.ShortType.type          => gen(typ, Gen.short)
      case typ: StandardType.ByteType.type           => gen(typ, Gen.byte)
      case typ: StandardType.IntType.type            => gen(typ, Gen.int)
      case typ: StandardType.LongType.type           => gen(typ, Gen.long)
      case typ: StandardType.FloatType.type          => gen(typ, Gen.float)
      case typ: StandardType.BigDecimalType.type     => gen(typ, Gen.double.map(d => java.math.BigDecimal.valueOf(d)))
      case typ: StandardType.BigIntegerType.type     => gen(typ, Gen.long.map(n => java.math.BigInteger.valueOf(n)))
      case typ: StandardType.DayOfWeekType.type      => gen(typ, JavaTimeGen.anyDayOfWeek)
      case typ: StandardType.DurationType.type       => gen(typ, JavaTimeGen.anyDuration)
      case typ: StandardType.InstantType.type        => gen(typ, JavaTimeGen.anyInstant)
      case typ: StandardType.LocalDateType.type      => gen(typ, JavaTimeGen.anyLocalDate)
      case typ: StandardType.LocalDateTimeType.type  => gen(typ, JavaTimeGen.anyLocalDateTime)
      case typ: StandardType.LocalTimeType.type      => gen(typ, JavaTimeGen.anyLocalTime)
      case typ: StandardType.MonthType.type          => gen(typ, JavaTimeGen.anyMonth)
      case typ: StandardType.MonthDayType.type       => gen(typ, JavaTimeGen.anyMonthDay)
      case typ: StandardType.OffsetDateTimeType.type => gen(typ, JavaTimeGen.anyOffsetDateTime)
      case typ: StandardType.OffsetTimeType.type     => gen(typ, JavaTimeGen.anyOffsetTime)
      case typ: StandardType.PeriodType.type         => gen(typ, JavaTimeGen.anyPeriod)
      case typ: StandardType.YearType.type           => gen(typ, JavaTimeGen.anyYear)
      case typ: StandardType.YearMonthType.type      => gen(typ, JavaTimeGen.anyYearMonth)
      case typ: StandardType.ZonedDateTimeType.type  => gen(typ, JavaTimeGen.anyZonedDateTime)
      case typ: StandardType.ZoneIdType.type         => gen(typ, JavaTimeGen.anyZoneId)
      case typ: StandardType.ZoneOffsetType.type     => gen(typ, JavaTimeGen.anyZoneOffset)
      case typ: StandardType.UnitType.type           => Gen.const(DynamicValue.Primitive((), typ))
      case typ: StandardType.UUIDType.type           => gen(typ, Gen.uuid)
      case typ: StandardType.CurrencyType.type       => gen(typ, Gen.currency)
    }
  }

  //scalafmt: { maxColumn = 400 }
  def anyDynamicValueOfSchema[A](schema: Schema[A]): Gen[Sized, DynamicValue] =
    schema match {
      case Schema.Primitive(standardType, _)                                                                                                                                                          => anyPrimitiveDynamicValue(standardType)
      case s: Schema.Record[A]                                                                                                                                                                        => anyDynamicValueWithStructure(s.fields)
      case Schema.Enum1(_, case1, _)                                                                                                                                                                  => anyDynamicValueOfEnum(Chunk(case1))
      case Schema.Enum2(_, case1, case2, _)                                                                                                                                                           => anyDynamicValueOfEnum(Chunk(case1, case2))
      case Schema.Enum3(_, case1, case2, case3, _)                                                                                                                                                    => anyDynamicValueOfEnum(Chunk(case1, case2, case3))
      case Schema.Enum4(_, case1, case2, case3, case4, _)                                                                                                                                             => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4))
      case Schema.Enum5(_, case1, case2, case3, case4, case5, _)                                                                                                                                      => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5))
      case Schema.Enum6(_, case1, case2, case3, case4, case5, case6, _)                                                                                                                               => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6))
      case Schema.Enum7(_, case1, case2, case3, case4, case5, case6, case7, _)                                                                                                                        => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7))
      case Schema.Enum8(_, case1, case2, case3, case4, case5, case6, case7, case8, _)                                                                                                                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8))
      case Schema.Enum9(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, _)                                                                                                          => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9))
      case Schema.Enum10(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, _)                                                                                                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10))
      case Schema.Enum11(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, _)                                                                                         => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11))
      case Schema.Enum12(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, _)                                                                                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12))
      case Schema.Enum13(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, _)                                                                         => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13))
      case Schema.Enum14(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, _)                                                                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14))
      case Schema.Enum15(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, _)                                                         => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15))
      case Schema.Enum16(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, _)                                                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16))
      case Schema.Enum17(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, _)                                         => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17))
      case Schema.Enum18(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, _)                                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18))
      case Schema.Enum19(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, _)                         => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19))
      case Schema.Enum20(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, _)                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20))
      case Schema.Enum21(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, case21, _)         => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, case21))
      case Schema.Enum22(_, case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, case21, case22, _) => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, case21, case22))
      case Schema.EnumN(_, cases, _)                                                                                                                                                                  => anyDynamicValueOfEnum(Chunk.fromIterable(cases.toSeq))
      case Schema.Sequence(schema, _, _, _, _)                                                                                                                                                        => Gen.chunkOfBounded(0, 2)(anyDynamicValueOfSchema(schema)).map(DynamicValue.Sequence(_))
      case Schema.Map(ks, vs, _)                                                                                                                                                                      => Gen.chunkOfBounded(0, 2)(anyDynamicValueOfSchema(ks).zip(anyDynamicValueOfSchema(vs))).map(DynamicValue.Dictionary(_))
      case Schema.Set(schema, _)                                                                                                                                                                      => Gen.setOfBounded(0, 2)(anyDynamicValueOfSchema(schema)).map(DynamicValue.SetValue(_))
      case Schema.Optional(schema, _)                                                                                                                                                                 => Gen.oneOf(anyDynamicSomeValueOfSchema(schema), Gen.const(DynamicValue.NoneValue))
      case Schema.Tuple2(left, right, _)                                                                                                                                                              => anyDynamicTupleValue(left, right)
      case Schema.Either(left, right, _) =>
        Gen.oneOf(anyDynamicLeftValueOfSchema(left), anyDynamicRightValueOfSchema(right))
      case Schema.Fallback(left, right, _, _) =>
        Gen.oneOf(anyDynamicLeftValueOfSchema(left), anyDynamicRightValueOfSchema(right), anyDynamicBothValueOfSchema(left, right))
      case Schema.Transform(schema, _, _, _, _) => anyDynamicValueOfSchema(schema)
      case Schema.Fail(message, _)              => Gen.const(DynamicValue.Error(message))
      case l @ Schema.Lazy(_)                   => anyDynamicValueOfSchema(l.schema)
      case Schema.Dynamic(_)                    => SchemaGen.anySchema.flatMap(anyDynamicValueOfSchema(_))
    }
  //scalafmt: { maxColumn = 120 }

  def anyDynamicLeftValueOfSchema[A](schema: Schema[A]): Gen[Sized, DynamicValue.LeftValue] =
    anyDynamicValueOfSchema(schema).map(DynamicValue.LeftValue(_))

  def anyDynamicRightValueOfSchema[A](schema: Schema[A]): Gen[Sized, DynamicValue.RightValue] =
    anyDynamicValueOfSchema(schema).map(DynamicValue.RightValue(_))

  def anyDynamicBothValueOfSchema[A, B](left: Schema[A], right: Schema[A]): Gen[Sized, DynamicValue.BothValue] =
    anyDynamicValueOfSchema(left).zip(anyDynamicValueOfSchema(right)).map {
      case (l, r) => DynamicValue.BothValue(l, r)
    }

  def anyDynamicSomeValueOfSchema[A](schema: Schema[A]): Gen[Sized, DynamicValue.SomeValue] =
    anyDynamicValueOfSchema(schema).map(DynamicValue.SomeValue(_))

  def anyDynamicTupleValue[A, B](left: Schema[A], right: Schema[B]): Gen[Sized, DynamicValue.Tuple] =
    anyDynamicValueOfSchema(left).zip(anyDynamicValueOfSchema(right)).map {
      case (l, r) => DynamicValue.Tuple(l, r)
    }

  def anyDynamicValueOfEnum[A](cases: Chunk[Schema.Case[A, _]]): Gen[Sized, DynamicValue.Enumeration] =
    for {
      index <- Gen.int(0, cases.size - 1)
      value <- anyDynamicValueOfSchema(cases(index).schema)
    } yield DynamicValue.Enumeration(TypeId.Structural, cases(index).id -> value)

  def anyDynamicValueWithStructure[A](structure: Chunk[Schema.Field[A, _]]): Gen[Sized, DynamicValue.Record] =
    Gen
      .collectAll(
        structure
          .map(field => Gen.const(field.name).zip(anyDynamicValueOfSchema(field.schema)))
      )
      .map { values =>
        DynamicValue.Record(TypeId.Structural, ListMap.empty ++ values)
      }

  def anyDynamicSequence[A](itemSchema: Schema[A]): Gen[Sized, DynamicValue.Sequence] =
    Gen.chunkOf(anyDynamicValueOfSchema(itemSchema)).map { items =>
      DynamicValue.Sequence(items)
    }

  def anyDynamicSet[A](itemSchema: Schema[A]): Gen[Sized, DynamicValue.SetValue] =
    Gen.setOf(anyDynamicValueOfSchema(itemSchema)).map { items =>
      DynamicValue.SetValue(items)
    }
}
