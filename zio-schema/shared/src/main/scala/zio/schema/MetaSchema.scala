package zio.schema

import java.time.temporal.{ ChronoUnit, TemporalUnit }

import scala.collection.immutable.ListMap

import zio.Chunk

sealed trait MetaSchema { self =>

  def toSchema: Schema[_] = self match {
    case MetaSchema.Duration(units)         => Schema.Primitive(StandardType.Duration(units))
    case MetaSchema.Value(tpe)              => Schema.Primitive(tpe)
    case MetaSchema.Fail(message)           => Schema.Fail(message)
    case MetaSchema.Optional(meta)          => meta.toSchema.optional
    case MetaSchema.EitherMeta(left, right) => left.toSchema <+> right.toSchema
    case MetaSchema.TupleMeta(left, right)  => left.toSchema <*> right.toSchema
    case MetaSchema.Record(structure) =>
      Schema.GenericRecord(structure.map(metaField => Schema.Field(metaField.label, metaField.fieldType.toSchema)))
    case MetaSchema.Enum(cases) =>
      Schema.Enumeration(ListMap.empty ++ cases.map(metaCase => metaCase.id -> metaCase.subType.toSchema))
    case MetaSchema.Sequence(tpe) => Schema.chunk(tpe.toSchema)
  }
}

object MetaSchema {

  def fromSchema[A](schema: Schema[A]): MetaSchema = schema match {
    case Schema.Primitive(StandardType.Duration(units)) => Duration(units)
    case Schema.Primitive(tpe)                          => Value(tpe)
    case Schema.Optional(schema)                        => Optional(fromSchema(schema))
    case Schema.EitherSchema(left, right)               => EitherMeta(fromSchema(left), fromSchema(right))
    case Schema.Tuple(left, right)                      => TupleMeta(fromSchema(left), fromSchema(right))
    case Schema.Sequence(schema, _, _)                  => Sequence(fromSchema(schema))
    case Schema.Fail(message)                           => Fail(message)
    case Schema.Transform(schema, _, _)                 => fromSchema(schema)
    case lzy @ Schema.Lazy(_)                           => fromSchema(lzy.schema)
    case s: Schema.Record[A] =>
      Record(s.structure.map(field => MetaField(field.label, fromSchema(field.schema))))
    case Schema.Enumeration(structure) =>
      Enum(
        Chunk.fromIterable(structure.map {
          case (id, schema) =>
            MetaCase(id, fromSchema(schema))
        })
      )
    case Schema.Enum1(c) =>
      Enum(Chunk(MetaCase(c.id, fromSchema(c.codec))))
    case Schema.Enum2(c1, c2) =>
      Enum(
        Chunk(
          MetaCase(c1.id, fromSchema(c1.codec)),
          MetaCase(c2.id, fromSchema(c2.codec))
        )
      )
    case Schema.Enum3(c1, c2, c3) =>
      Enum(
        Chunk(
          MetaCase(c1.id, fromSchema(c1.codec)),
          MetaCase(c2.id, fromSchema(c2.codec)),
          MetaCase(c3.id, fromSchema(c3.codec))
        )
      )
    case Schema.EnumN(cases) =>
      Enum(
        Chunk.fromIterable(
          cases.map(c => MetaCase(c.id, fromSchema(c.codec)))
        )
      )
    case Schema.Meta(spec) => spec
  }

  implicit val schema: Schema[MetaSchema] = DeriveSchema.gen[MetaSchema]

  final case class MetaField(label: String, fieldType: MetaSchema)

  object MetaField {
    implicit val schema: Schema[MetaField] =
      Schema.CaseClass2(
        field1 = Schema.Field("label", Schema[String]),
        field2 = Schema.Field("fieldType", Schema.defer(MetaSchema.schema)),
        construct = (label: String, fieldType: MetaSchema) => MetaField(label, fieldType),
        extractField1 = _.label,
        extractField2 = _.fieldType
      )
    implicit val chunkSchema: Schema[Chunk[MetaField]] = Schema.chunk(schema)
  }
  final case class MetaCase(id: String, subType: MetaSchema)

  object MetaCase {
    implicit val schema: Schema[MetaCase] =
      Schema.CaseClass2(
        field1 = Schema.Field("id", Schema[String]),
        field2 = Schema.Field("subType", Schema.defer(MetaSchema.schema)),
        construct = (label: String, subType: MetaSchema) => MetaCase(label, subType),
        extractField1 = _.id,
        extractField2 = _.subType
      )
    implicit val chunkSchema: Schema[Chunk[MetaCase]] = Schema.chunk(schema)
  }

  final case class Value(valueType: StandardType[_]) extends MetaSchema

  object Value {
    implicit val schema: Schema[Value] = Schema[String].transformOrFail(
      termType => StandardType.fromString(termType).map(Value(_)).toRight(s"$termType is not valid type"),
      value => Right(value.valueType.tag)
    )
  }
  final case class Duration(units: TemporalUnit) extends MetaSchema

  object Duration {
    implicit val schema: Schema[Duration] = Schema[String].transformOrFail(
      units =>
        try {
          Right(Duration(ChronoUnit.valueOf(units.toUpperCase())))
        } catch { case _: Throwable => Left(s"$units is not valid temporal unit") },
      duration => Right(duration.units.toString())
    )
  }
  final case class Fail(message: String)                           extends MetaSchema
  final case class Sequence(elementType: MetaSchema)               extends MetaSchema
  final case class Optional(valueType: MetaSchema)                 extends MetaSchema
  final case class EitherMeta(left: MetaSchema, right: MetaSchema) extends MetaSchema
  final case class TupleMeta(left: MetaSchema, right: MetaSchema)  extends MetaSchema
  final case class Enum(cases: Chunk[MetaCase])                    extends MetaSchema
  final case class Record(structure: Chunk[MetaField])             extends MetaSchema
}
