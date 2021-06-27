package zio.schema

import scala.collection.immutable.ListMap

import zio.Chunk
import zio.schema.MetaSchema.Singleton

sealed trait MetaSchema { self =>

  def toSchema: Either[String, Schema[_]] = self match {
    case MetaSchema.Duration(units) =>
      StandardType.fromTemporalUnits(units).map(Schema.Primitive(_)).toRight(s"invalid temporal units $units")
    case MetaSchema.Value(tpe) =>
      StandardType.fromString(tpe).map(Schema.Primitive(_)).toRight(s"unknown type $tpe")
    case MetaSchema.Fail(message)  => Right(Schema.Fail(message))
    case MetaSchema.Optional(meta) => meta.toSchema.map(_.optional)
    case MetaSchema.EitherMeta(left, right) =>
      for {
        leftSchema  <- left.toSchema
        rightSchema <- right.toSchema
      } yield leftSchema <+> rightSchema
    case MetaSchema.TupleMeta(left, right) =>
      for {
        leftSchema  <- left.toSchema
        rightSchema <- right.toSchema
      } yield leftSchema <*> rightSchema
    case MetaSchema.Record(structure) =>
      structure
        .foldLeft[Either[String, Chunk[Schema.Field[_]]]](Right(Chunk.empty)) {
          case (error @ Left(_), _) => error
          case (Right(structure), MetaSchema.MetaField(label, meta)) =>
            meta.toSchema.map(schema => structure :+ Schema.Field(label, schema))
        }
        .map(s => Schema.GenericRecord(s))
    case MetaSchema.Enum(cases) =>
      cases
        .foldLeft[Either[String, Chunk[(String, Schema[_])]]](Right(Chunk.empty)) {
          case (error @ Left(_), _) => error
          case (Right(structure), MetaSchema.MetaCase(label, meta)) =>
            meta.toSchema.map(schema => structure :+ label -> schema)
        }
        .map(s => Schema.Enumeration(ListMap.empty ++ s))
    case MetaSchema.Sequence(tpe) => tpe.toSchema.map(Schema.chunk(_))
    case MetaSchema.Singleton     => Right(Schema.CaseObject(Singleton))
  }
}

object MetaSchema {

  def fromSchema[A](schema: Schema[A]): MetaSchema = schema match {
    case Schema.Primitive(StandardType.Duration(units)) => Duration(units.toString)
    case Schema.Primitive(tpe)                          => Value(tpe.tag)
    case Schema.Optional(schema)                        => Optional(fromSchema(schema))
    case Schema.EitherSchema(left, right)               => EitherMeta(fromSchema(left), fromSchema(right))
    case Schema.Tuple(left, right)                      => TupleMeta(fromSchema(left), fromSchema(right))
    case Schema.Sequence(schema, _, _)                  => Sequence(fromSchema(schema))
    case Schema.Fail(message)                           => Fail(message)
    case Schema.Transform(schema, _, _)                 => fromSchema(schema)
    case lzy @ Schema.Lazy(_)                           => fromSchema(lzy.schema)
    case Schema.CaseObject(_)                           => Singleton
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

  final case class MetaField(label: String, `type`: MetaSchema)

  object MetaField {
    implicit val schema: Schema[MetaField]             = DeriveSchema.gen[MetaField]
    implicit val chunkSchema: Schema[Chunk[MetaField]] = Schema.chunk[MetaField]
  }
  final case class MetaCase(id: String, `type`: MetaSchema)

  object MetaCase {
    implicit val schema: Schema[MetaCase]             = DeriveSchema.gen[MetaCase]
    implicit val chunkSchema: Schema[Chunk[MetaCase]] = Schema.chunk[MetaCase]
  }

  final case object Singleton                                      extends MetaSchema
  final case class Value(`type`: String)                           extends MetaSchema
  final case class Duration(units: String)                         extends MetaSchema
  final case class Fail(message: String)                           extends MetaSchema
  final case class Sequence(`type`: MetaSchema)                    extends MetaSchema
  final case class Optional(`type`: MetaSchema)                    extends MetaSchema
  final case class EitherMeta(left: MetaSchema, right: MetaSchema) extends MetaSchema
  final case class TupleMeta(left: MetaSchema, right: MetaSchema)  extends MetaSchema
  final case class Enum(cases: Chunk[MetaCase])                    extends MetaSchema
  final case class Record(structure: Chunk[MetaField])             extends MetaSchema
}
