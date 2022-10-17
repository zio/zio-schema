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
      implicit lazy val fieldEqual: Equal[Schema.Field[_, _]] =
        (l: Schema.Field[_, _], r: Schema.Field[_, _]) => {
          l.name === r.name &&
            l.schema === r.schema &&
            l.annotations == r.annotations
        }
      implicit lazy val caseEqual: Equal[Schema.Case[_, _]] =
        (l: Schema.Case[_, _], r: Schema.Case[_, _]) => {
          l.id == r.id &&
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
            l.annotations == r.annotations && lEnum.cases === rEnum.cases
          case (lRecord: Schema.Record[_], rRecord: Schema.Record[_]) =>
            l.annotations == r.annotations && lRecord.fields === rRecord.fields
          case (lMap: Schema.Map[_, _], rMap: Schema.Map[_, _]) =>
            lMap.annotations == rMap.annotations && lMap.keySchema === rMap.keySchema && lMap.valueSchema === rMap.valueSchema
          case (lSet: Schema.Set[_], rSet: Schema.Set[_]) =>
            lSet.annotations == rSet.annotations && lSet.elementSchema === rSet.elementSchema
          case (lSeq: Schema.Sequence[_, _, _], rSeq: Schema.Sequence[_, _, _]) =>
            (ignoreTransformations || (lSeq.identity == rSeq.identity)) &&
              lSeq.annotations == rSeq.annotations &&
              lSeq.elementSchema === rSeq.elementSchema
          case (lTransform: Schema.Transform[_, _, _], rTransform: Schema.Transform[_, _, _]) =>
            (ignoreTransformations || (lTransform.identity == rTransform.identity)) &&
              lTransform.annotations == rTransform.annotations &&
              lTransform.schema === rTransform.schema
          case (lPrimitive: Schema.Primitive[_], rPrimitive: Schema.Primitive[_]) =>
            lPrimitive.annotations == rPrimitive.annotations &&
              lPrimitive.standardType == rPrimitive.standardType
          case (lOptional: Schema.Optional[_], rOptional: Schema.Optional[_]) =>
            lOptional.annotations == rOptional.annotations &&
              lOptional.schema === rOptional.schema
          case (lFail: Schema.Fail[_], rFail: Schema.Fail[_]) =>
            lFail.annotations == rFail.annotations && lFail.message == rFail.message
          case (lTuple: Schema.Tuple2[_, _], rTuple: Schema.Tuple2[_, _]) =>
            lTuple.annotations == rTuple.annotations &&
              lTuple.left === rTuple.left &&
              rTuple.right === rTuple.right
          case (lEither: Schema.Either[_, _], rEither: Schema.Either[_, _]) =>
            lEither.annotations == rEither.annotations &&
              lEither.left === rEither.left &&
              lEither.right === rEither.right
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
            recursiveEqual(lTransform.schema, r, visitedPairs)
          case (l: Schema[_], rTransform: Schema.Transform[_, _, _]) if ignoreTransformations =>
            recursiveEqual(l, rTransform.schema, visitedPairs)
          case (_, _) => false
        }
        result
      }
    }
  }

  val strictEquality: Equal[Schema[_]]    = new SchemaEqual(ignoreTransformations = false)
  val structureEquality: Equal[Schema[_]] = new SchemaEqual(ignoreTransformations = true)
}
