package zio.schema

import scala.collection.immutable.ListMap

import zio.schema.Schema.{ Field, Primitive, record }

package object codec {
  lazy val monthDayStructure: Schema[ListMap[String, _]] =
    record(Field("month", Primitive(StandardType.IntType)), Field("day", Primitive(StandardType.IntType)))
      .annotate(AvroAnnotations.name("MonthDay"))
      .annotate(AvroAnnotations.namespace("zio.schema.codec.avro"))

  lazy val periodStructure: Schema[ListMap[String, _]] =
    record(
      Field("years", Primitive(StandardType.IntType)),
      Field("months", Primitive(StandardType.IntType)),
      Field("days", Primitive(StandardType.IntType))
    ).annotate(AvroAnnotations.name("Period")).annotate(AvroAnnotations.namespace("zio.schema.codec.avro"))

  lazy val yearMonthStructure: Schema[ListMap[String, _]] =
    record(Field("year", Primitive(StandardType.IntType)), Field("month", Primitive(StandardType.IntType)))
      .annotate(AvroAnnotations.name("YearMonth"))
      .annotate(AvroAnnotations.namespace("zio.schema.codec.avro"))

  lazy val durationStructure: Schema[ListMap[String, _]] =
    record(Field("seconds", Primitive(StandardType.LongType)), Field("nanos", Primitive(StandardType.IntType)))
      .annotate(AvroAnnotations.name("Duration"))
      .annotate(AvroAnnotations.namespace("zio.schema.codec.avro"))

}
