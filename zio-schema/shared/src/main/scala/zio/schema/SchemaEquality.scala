package zio.schema

import scala.collection.mutable

import zio.prelude._

trait SchemaEquality {

  private class SchemaEqual(ignoreTransformations: Boolean) extends Equal[Schema[_]] {
    override protected def checkEqual(l: Schema[_], r: Schema[_]): Boolean = {
      val visited = mutable.HashSet.empty[(Schema[_], Schema[_])]
      recursiveEqual(l, r, visited)
    }

    private def recursiveEqual(
      l: Schema[_],
      r: Schema[_],
      visitedPairs: mutable.Set[(Schema[_], Schema[_])]
    ): Boolean = {
      implicit lazy val selfEqual: Equal[Schema[_]] = Equal.make(recursiveEqual(_, _, visitedPairs))
      implicit lazy val fieldEqual: Equal[Schema.Field[_]] =
        (l: Schema.Field[_], r: Schema.Field[_]) => {
          l.label === r.label &&
            l.schema === r.schema &&
            l.annotations == r.annotations
        }

      val pair: (Schema[_], Schema[_]) = (l, r)
      if (visitedPairs.contains(pair)) {
        true // recursion point found in both schemas
      } else {
        visitedPairs += pair

        val result = (l, r) match {
          case (lEnum: Schema.Enum[_], rEnum: Schema.Enum[_]) =>
            l.annotations == r.annotations && lEnum.structure === rEnum.structure
          case (lRecord: Schema.Record[_], rRecord: Schema.Record[_]) =>
            l.annotations == r.annotations && lRecord.structure === rRecord.structure
          case (lMap: Schema.MapSchema[_, _], rMap: Schema.MapSchema[_, _]) =>
            lMap.annotations == rMap.annotations && lMap.ks === rMap.ks && lMap.vs === rMap.vs
          case (lSet: Schema.SetSchema[_], rSet: Schema.SetSchema[_]) =>
            lSet.annotations == rSet.annotations && lSet.as === rSet.as
          case (lSeq: Schema.Sequence[_, _, _], rSeq: Schema.Sequence[_, _, _]) =>
            (ignoreTransformations || (lSeq.identity == rSeq.identity)) &&
              lSeq.annotations == rSeq.annotations &&
              lSeq.schemaA === rSeq.schemaA
          case (lTransform: Schema.Transform[_, _, _], rTransform: Schema.Transform[_, _, _]) =>
            (ignoreTransformations || (lTransform.identity == rTransform.identity)) &&
              lTransform.annotations == rTransform.annotations &&
              lTransform.codec === rTransform.codec
          case (lPrimitive: Schema.Primitive[_], rPrimitive: Schema.Primitive[_]) =>
            lPrimitive.annotations == rPrimitive.annotations &&
              lPrimitive.standardType == rPrimitive.standardType
          case (lOptional: Schema.Optional[_], rOptional: Schema.Optional[_]) =>
            lOptional.annotations == rOptional.annotations &&
              lOptional.codec === rOptional.codec
          case (lFail: Schema.Fail[_], rFail: Schema.Fail[_]) =>
            lFail.annotations == rFail.annotations && lFail.message == rFail.message
          case (lTuple: Schema.Tuple[_, _], rTuple: Schema.Tuple[_, _]) =>
            lTuple.annotations == rTuple.annotations &&
              lTuple.left === rTuple.left &&
              rTuple.right === rTuple.right
          case (lEither: Schema.EitherSchema[_, _], rEither: Schema.EitherSchema[_, _]) =>
            lEither.annotations == rEither.annotations &&
              lEither.left === rEither.left &&
              lEither.right === rEither.right
          case (lMeta: Schema.Meta, rMeta: Schema.Meta) =>
            lMeta.annotations == rMeta.annotations &&
              lMeta.ast === rMeta.ast
          case (lLazy: Schema.Lazy[_], rLazy: Schema.Lazy[_]) =>
            if (lLazy.schema eq rLazy.schema)
              true
            else
              recursiveEqual(lLazy.schema, rLazy.schema, visitedPairs)
          case (lLazy: Schema.Lazy[_], r: Schema[_]) =>
            recursiveEqual(lLazy.schema, r, visitedPairs)
          case (l: Schema[_], rLazy: Schema.Lazy[_]) =>
            recursiveEqual(l, rLazy.schema, visitedPairs)
          case (lTransform: Schema.Transform[_, _, _], r: Schema[_]) if ignoreTransformations =>
            recursiveEqual(lTransform.codec, r, visitedPairs)
          case (l: Schema[_], rTransform: Schema.Transform[_, _, _]) if ignoreTransformations =>
            recursiveEqual(l, rTransform.codec, visitedPairs)
          case (_, _) => false
        }
        result
      }
    }
  }

  val strictEquality: Equal[Schema[_]]    = new SchemaEqual(ignoreTransformations = false)
  val structureEquality: Equal[Schema[_]] = new SchemaEqual(ignoreTransformations = true)
}
