package zio.schema.annotation

import scala.collection.immutable.ListMap

import zio.schema.TypeId

final case class genericTypeInfo(appliedTypes: ListMap[String, TypeId.Nominal])
    extends scala.annotation.StaticAnnotation
