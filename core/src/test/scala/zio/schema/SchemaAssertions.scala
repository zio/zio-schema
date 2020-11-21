package zio.schema

import zio.test.Assertion
import zio.test.AssertionM.Render.param

object SchemaAssertions {

  def hasSameSchema[A](expected: Schema[A]): Assertion[Schema[A]] =
    Assertion.assertion("hasSameSchema")(param(expected))(actual => equalsSchema(expected, actual))

  private def equalsSchema[A](left: Schema[A], right: Schema[A]): Boolean =
    (left: Schema[_], right: Schema[_]) match {
      case (Schema.Transform(codec1, _, _), Schema.Transform(codec2, _, _)) =>
        equalsSchema(codec1, codec2)
      case (Schema.Record(structure1), Schema.Record(structure2)) =>
        hasSameKeys(structure1, structure2) &&
          structure1.forall { keyAndSchema =>
            val left: Schema[Any]  = keyAndSchema._2.asInstanceOf[Schema[Any]]
            val right: Schema[Any] = structure2(keyAndSchema._1).asInstanceOf[Schema[Any]]
            equalsSchema(left, right)
          }
      case (Schema.Sequence(element1), Schema.Sequence(element2)) => equalsSchema(element1, element2)
      case (Schema.Enumeration(structure1), Schema.Enumeration(structure2)) =>
        hasSameKeys(structure1, structure2) &&
          structure1.forall { keyAndSchema =>
            val left: Schema[Any]  = keyAndSchema._2.asInstanceOf[Schema[Any]]
            val right: Schema[Any] = structure2(keyAndSchema._1).asInstanceOf[Schema[Any]]
            equalsSchema(left, right)
          }
      case (Schema.Primitive(standardType1), Schema.Primitive(standardType2)) =>
        standardType1 == standardType2
      case (Schema.Tuple(left1, right1), Schema.Tuple(left2, right2)) =>
        equalsSchema(left1, left2) && equalsSchema(right1, right2)
      case (Schema.Optional(codec1), Schema.Optional(codec2)) => equalsSchema(codec1, codec2)
      case _                                                  => false
    }

  private def hasSameKeys[K, V](map1: Map[K, V], map2: Map[K, V]): Boolean =
    map1.keySet.diff(map2.keySet).isEmpty
}