package zio.schema.annotation

import scala.collection.immutable.ListMap

import zio.schema.TypeId

/**
 * Records the type arguments of an applied generic type. `DeriveSchema.gen` adds it to the schema of a generic case class or sealed trait applied to type arguments, such as `Box[Int]`, mapping each type parameter name to the [[zio.schema.TypeId]] of its argument (`ListMap("A" -> TypeId.parse("scala.Int"))`).
 *
 * @param appliedTypes
 *   the type parameter names, in declaration order, with the type ids of their arguments
 */
final case class genericTypeInfo(appliedTypes: ListMap[String, TypeId.Nominal])
    extends scala.annotation.StaticAnnotation
