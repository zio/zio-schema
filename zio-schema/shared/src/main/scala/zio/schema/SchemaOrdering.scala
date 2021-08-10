package zio.schema

import scala.annotation.tailrec
import zio.Chunk
import zio.schema.DynamicValue._
import zio.schema.Schema
import zio.schema.StandardType.UnitType

object SchemaOrdering {

  def ordering[A](schema:Schema[A]):Ordering[A] = (l: A, r: A) =>
    compareBySchema(schema)(schema.toDynamic(l),schema.toDynamic(r))

  private def compareBySchema[A](schema:Schema[A])(l:DynamicValue,r:DynamicValue):Int = (schema,l,r) match {
    case (schema:Schema.Lazy[_], l, r) => compareBySchema(schema.schema)(l,r)
    case (Schema.Primitive(UnitType), _, _) => 0
    case (Schema.Primitive(standardType), Primitive(lVal, lType), Primitive(rVal, rType)) if lType==rType && standardType==lType =>
      lType.compare(lVal,rVal)
    case (Schema.EitherSchema(leftSchema, _), LeftValue(lVal), LeftValue(rVal)) =>
      compareBySchema(leftSchema)(lVal,rVal)
    case (Schema.EitherSchema(_, rightSchema), RightValue(lVal), RightValue(rVal)) =>
      compareBySchema(rightSchema)(lVal,rVal)
    case (Schema.EitherSchema(_,_), LeftValue(_),RightValue(_)) => -1
    case (Schema.EitherSchema(_,_),_,_) => 1
    case (Schema.Optional(innerSchema), SomeValue(lVal), SomeValue(rVal)) =>
      compareBySchema(innerSchema)(lVal,rVal)
    case (Schema.Optional(_), NoneValue, SomeValue(_)) => -1
    case (Schema.Optional(_), SomeValue(_), NoneValue) => 1
    case (Schema.Tuple(lSchema,rSchema), l:Tuple, r:Tuple ) => {
      val leftComparison = compareBySchema(lSchema)(l.left, r.left)
      if(leftComparison!=0)
        leftComparison
      else
        compareBySchema(rSchema)(l.right, r.right)
    }
    case (Schema.Sequence(schema,_,_), Sequence(lVal), Sequence(rVal)) =>
      compareSequences(lVal,rVal, compareBySchema(schema))
    case (Schema.Fail(_), Error(lVal), Error(rVal) ) => lVal.compareTo(rVal)
    case (Schema.Transform(schemaA,_,_), Transform(lVal), Transform(rVal) ) =>
      compareBySchema(schemaA)(lVal,rVal)
    case (Schema.Enum1(c),_,_) => compareBySchema(Schema.EnumN(Seq(c)))(l,r)
    case (Schema.Enum2(c1,c2),_, _) => compareBySchema(Schema.EnumN(Seq(c1,c2)))(l,r)
    case (Schema.Enum3(c1,c2,c3),_, _) => compareBySchema(Schema.EnumN(Seq(c1,c2,c3)))(l,r)
    case (Schema.EnumN(cases), Enumeration((lField,lVal)), Enumeration((rField,rVal))) if lField==rField =>
        compareBySchema(cases.find(_.id==lField).get.codec)(lVal,rVal)
    case (Schema.EnumN(cases), Enumeration((lField,_)), Enumeration((rField,_))) =>
        cases.indexWhere(_.id ==lField).compareTo(cases.indexWhere(_.id ==rField))
    case (r:Schema.Record[_],Record(lVals),Record(rVals)) =>
      compareRecords(r, lVals, rVals)
    case _ => 0
  }

  def compareRecords(r:Schema.Record[_], lVals:Map[String,DynamicValue], rVals:Map[String,DynamicValue]):Int = {
    val j = r.structure.length
    @tailrec
    def loop(i: Int): Int = {
      if (i == j) 0
      else {
        val field = r.structure(i)
        val fieldComparison = compareBySchema(field.schema)(lVals(field.label),rVals(field.label))
        if (fieldComparison == 0) loop(i + 1) else fieldComparison
      }
    }
    loop(0)
  }

  def compareSequences(l:Chunk[DynamicValue], r:Chunk[DynamicValue], f:(DynamicValue,DynamicValue)=>Int):Int = {
    val j = l.length
    val k = r.length

    @tailrec
    def loop(i: Int): Int = {
      if (i == j && i == k) 0
      else if (i == j) -1
      else if (i == k) 1
      else {
        val compare = f(l(i),r(i))
        if (compare == 0) loop(i + 1) else compare
      }
    }

    loop(0)
  }

}
