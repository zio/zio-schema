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
      case typ: StandardType.CharType.type       => gen(typ, Gen.anyASCIIChar)
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
      case typ: StandardType.InstantType         => gen(typ, JavaTimeGen.anyInstant)
      case typ: StandardType.LocalDateType       => gen(typ, JavaTimeGen.anyLocalDate)
      case typ: StandardType.LocalDateTimeType   => gen(typ, JavaTimeGen.anyLocalDateTime)
      case typ: StandardType.LocalTimeType       => gen(typ, JavaTimeGen.anyLocalTime)
      case typ: StandardType.MonthType.type      => gen(typ, JavaTimeGen.anyMonth)
      case typ: StandardType.MonthDayType.type   => gen(typ, JavaTimeGen.anyMonthDay)
      case typ: StandardType.OffsetDateTimeType  => gen(typ, JavaTimeGen.anyOffsetDateTime)
      case typ: StandardType.OffsetTimeType      => gen(typ, JavaTimeGen.anyOffsetTime)
      case typ: StandardType.PeriodType.type     => gen(typ, JavaTimeGen.anyPeriod)
      case typ: StandardType.YearType.type       => gen(typ, JavaTimeGen.anyYear)
      case typ: StandardType.YearMonthType.type  => gen(typ, JavaTimeGen.anyYearMonth)
      case typ: StandardType.ZonedDateTimeType   => gen(typ, JavaTimeGen.anyZonedDateTime)
      case typ: StandardType.ZoneIdType.type     => gen(typ, JavaTimeGen.anyZoneId)
      case typ: StandardType.ZoneOffsetType.type => gen(typ, JavaTimeGen.anyZoneOffset)
      case typ: StandardType.UnitType.type       => Gen.const(DynamicValue.Primitive((), typ))
      case typ: StandardType.UUIDType.type       => gen(typ, Gen.anyUUID)
    }
  }

  //scalafmt: { maxColumn = 400 }
  def anyDynamicValueOfSchema[A](schema: Schema[A]): Gen[Random with Sized, DynamicValue] =
    schema match {
      case Schema.Primitive(standardType, _)                                                                                                                                                       => anyPrimitiveDynamicValue(standardType)
      case s: Schema.Record[A]                                                                                                                                                                     => anyDynamicValueWithStructure(s.structure)
      case Schema.Enum1(case1, _)                                                                                                                                                                  => anyDynamicValueOfEnum(Chunk(case1))
      case Schema.Enum2(case1, case2, _)                                                                                                                                                           => anyDynamicValueOfEnum(Chunk(case1, case2))
      case Schema.Enum3(case1, case2, case3, _)                                                                                                                                                    => anyDynamicValueOfEnum(Chunk(case1, case2, case3))
      case Schema.Enum4(case1, case2, case3, case4, _)                                                                                                                                             => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4))
      case Schema.Enum5(case1, case2, case3, case4, case5, _)                                                                                                                                      => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5))
      case Schema.Enum6(case1, case2, case3, case4, case5, case6, _)                                                                                                                               => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6))
      case Schema.Enum7(case1, case2, case3, case4, case5, case6, case7, _)                                                                                                                        => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7))
      case Schema.Enum8(case1, case2, case3, case4, case5, case6, case7, case8, _)                                                                                                                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8))
      case Schema.Enum9(case1, case2, case3, case4, case5, case6, case7, case8, case9, _)                                                                                                          => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9))
      case Schema.Enum10(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, _)                                                                                                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10))
      case Schema.Enum11(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, _)                                                                                         => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11))
      case Schema.Enum12(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, _)                                                                                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12))
      case Schema.Enum13(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, _)                                                                         => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13))
      case Schema.Enum14(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, _)                                                                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14))
      case Schema.Enum15(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, _)                                                         => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15))
      case Schema.Enum16(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, _)                                                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16))
      case Schema.Enum17(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, _)                                         => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17))
      case Schema.Enum18(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, _)                                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18))
      case Schema.Enum19(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, _)                         => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19))
      case Schema.Enum20(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, _)                 => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20))
      case Schema.Enum21(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, case21, _)         => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, case21))
      case Schema.Enum22(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, case21, case22, _) => anyDynamicValueOfEnum(Chunk(case1, case2, case3, case4, case5, case6, case7, case8, case9, case10, case11, case12, case13, case14, case15, case16, case17, case18, case19, case20, case21, case22))
      case Schema.EnumN(cases, _)                                                                                                                                                                  => anyDynamicValueOfEnum(Chunk.fromIterable(cases.toSeq))
      case Schema.Sequence(schema, _, _, _, _)                                                                                                                                                     => Gen.chunkOfBounded(0, 2)(anyDynamicValueOfSchema(schema)).map(DynamicValue.Sequence(_))
      case Schema.MapSchema(ks, vs, _)                                                                                                                                                             => Gen.chunkOfBounded(0, 2)(anyDynamicValueOfSchema(ks).zip(anyDynamicValueOfSchema(vs))).map(DynamicValue.Dictionary(_))
      case Schema.SetSchema(schema, _)                                                                                                                                                             => Gen.setOfBounded(0, 2)(anyDynamicValueOfSchema(schema)).map(DynamicValue.SetValue(_))
      case Schema.Optional(schema, _)                                                                                                                                                              => Gen.oneOf(anyDynamicSomeValueOfSchema(schema), Gen.const(DynamicValue.NoneValue))
      case Schema.Tuple(left, right, _)                                                                                                                                                            => anyDynamicTupleValue(left, right)
      case Schema.EitherSchema(left, right, _) =>
        Gen.oneOf(anyDynamicLeftValueOfSchema(left), anyDynamicRightValueOfSchema(right))
      case Schema.Transform(schema, _, _, _, _) => anyDynamicValueOfSchema(schema).map(DynamicValue.Transform(_))
      case Schema.Fail(message, _)              => Gen.const(DynamicValue.Error(message))
      case l @ Schema.Lazy(_)                   => anyDynamicValueOfSchema(l.schema)
      case Schema.Meta(meta, _)                 => anyDynamicValueOfSchema(meta.toSchema)
    }
  //scalafmt: { maxColumn = 120 }

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
