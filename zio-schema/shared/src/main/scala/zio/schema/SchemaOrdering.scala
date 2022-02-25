package zio.schema

import scala.annotation.tailrec

import zio.Chunk
import zio.schema.DynamicValue._
import zio.schema.StandardType.UnitType

object SchemaOrdering {

  private[schema] def ordering[A](schema: Schema[A]): Ordering[A] = (l: A, r: A) => {
    compareBySchema(schema)(schema.toDynamic(l), schema.toDynamic(r))
  }

  private def compareBySchema[A](schema: Schema[A])(l: DynamicValue, r: DynamicValue): Int = (schema, l, r) match {
    case (schema: Schema.Lazy[_], l, r) => compareBySchema(schema.schema)(l, r)
    case (schema: Schema.Primitive[t], Primitive(lVal, lType), Primitive(rVal, rType))
        if lType == rType && schema.standardType == lType =>
      val lTypeCoerced = lType.asInstanceOf[StandardType[t]]
      lTypeCoerced.compare(lVal.asInstanceOf[t], rVal.asInstanceOf[t])
    case (Schema.Primitive(UnitType, _), _, _) => 0
    case (Schema.EitherSchema(leftSchema, _, _), LeftValue(lVal), LeftValue(rVal)) =>
      compareBySchema(leftSchema)(lVal, rVal)
    case (Schema.EitherSchema(_, rightSchema, _), RightValue(lVal), RightValue(rVal)) =>
      compareBySchema(rightSchema)(lVal, rVal)
    case (Schema.EitherSchema(_, _, _), LeftValue(_), RightValue(_)) => -1
    case (Schema.EitherSchema(_, _, _), RightValue(_), LeftValue(_)) => 1
    case (Schema.Optional(innerSchema, _), SomeValue(lVal), SomeValue(rVal)) =>
      compareBySchema(innerSchema)(lVal, rVal)
    case (Schema.Optional(_, _), NoneValue, SomeValue(_)) => -1
    case (Schema.Optional(_, _), SomeValue(_), NoneValue) => 1
    case (Schema.Tuple(lSchema, rSchema, _), l: Tuple, r: Tuple) => {
      val leftComparison = compareBySchema(lSchema)(l.left, r.left)
      if (leftComparison != 0)
        leftComparison
      else
        compareBySchema(rSchema)(l.right, r.right)
    }
    case (Schema.Sequence(schema, _, _, _, _), Sequence(lVal), Sequence(rVal)) =>
      compareSequences(lVal, rVal, compareBySchema(schema))
    case (Schema.Fail(_, _), Error(lVal), Error(rVal))                                     => lVal.compareTo(rVal)
    case (Schema.Transform(_, _, _, _, _), Transform(Error(lval)), Transform(Error(rVal))) => lval.compareTo(rVal)
    case (Schema.Transform(_, _, _, _, _), Transform(Error(_)), Transform(_))              => -1
    case (Schema.Transform(_, _, _, _, _), Transform(_), Transform(Error(_)))              => 1
    case (Schema.Transform(schemaA, _, _, _, _), Transform(lVal), Transform(rVal)) =>
      compareBySchema(schemaA)(lVal, rVal)
    case (e: Schema.Enum[_], Enumeration((lField, lVal)), Enumeration((rField, rVal))) if lField == rField =>
      compareBySchema(e.structure(lField))(lVal, rVal)
    case (e: Schema.Enum[_], Enumeration((lField, _)), Enumeration((rField, _))) => {
      val fields = e.structure.keys.toList
      fields.indexOf(lField).compareTo(fields.indexOf(rField))
    }
    case (r: Schema.Record[_], Record(lVals), Record(rVals)) =>
      compareRecords(r, lVals, rVals)
    case _ => 0
  }

  private def compareRecords(
    r: Schema.Record[_],
    lVals: Map[String, DynamicValue],
    rVals: Map[String, DynamicValue]
  ): Int = {
    val j = r.structure.length
    @tailrec
    def loop(i: Int): Int =
      if (i == j) 0
      else {
        val field           = r.structure(i)
        val fieldComparison = compareBySchema(field.schema)(lVals(field.label), rVals(field.label))
        if (fieldComparison == 0) loop(i + 1) else fieldComparison
      }
    loop(0)
  }

  private def compareSequences(
    l: Chunk[DynamicValue],
    r: Chunk[DynamicValue],
    f: (DynamicValue, DynamicValue) => Int
  ): Int = {
    val j = l.length
    val k = r.length

    @tailrec
    def loop(i: Int): Int =
      if (i == j && i == k) 0
      else if (i == j) -1
      else if (i == k) 1
      else {
        val compare = f(l(i), r(i))
        if (compare == 0) loop(i + 1) else compare
      }

    loop(0)
  }

}
