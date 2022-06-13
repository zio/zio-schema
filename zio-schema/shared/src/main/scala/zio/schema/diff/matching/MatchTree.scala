package zio.schema.diff.matching

import zio.schema.DynamicValue
import zio.schema.DynamicValue.SetValue
import zio.schema.DynamicValue.Tuple
import zio.schema.DynamicValue.Primitive
import zio.schema.DynamicValue.SomeValue
import zio.schema.DynamicValue.Dictionary
import zio.schema.DynamicValue.Error
import zio.schema.DynamicValue.Sequence
import zio.schema.DynamicValue.Singleton
import zio.schema.DynamicValue.DynamicAst
import zio.schema.DynamicValue.LeftValue
import zio.schema.DynamicValue.RightValue
import zio.schema.DynamicValue.NoneValue
import zio.schema.Schema
import zio.schema.Schema._

sealed trait MatchTree {
  val id: List[Int]
  val value: DynamicValue
}

object MatchTree {
  //Should this be done for Schema instead?
  // TODO add annotations (ignored for now)
  def fromDynamicValue(value: DynamicValue): MatchTree = {
    def loop(id: List[Int], value: DynamicValue): MatchTree =
      value match {
        case SetValue(values) =>
          val children = values.zipWithIndex.map { case (v, i) => loop(i :: id, v) }
          SetNode(id, value, children)
        case Tuple(left, right)              => Node(id, value, Vector(loop(1::id, left), loop(2::id, right)))
        case DynamicValue.Enumeration(values) => Node(id, value, values.zipWithIndex.map { case (v, i) => loop(i :: id, v) })
        case DynamicValue.Record(values)     => ???
        case Dictionary(entries)             => ???
        case DynamicValue.Enumeration(value) => ???
        case Sequence(values)                => ???
        case DynamicAst(ast)                 => ???
        case LeftValue(value)                => ???
        case RightValue(value)               => ???
        //all cases included in case another case is added to DynamicValue
        case _: Singleton[_]      => Leaf(id, value)
        case _: NoneValue.type => Leaf(id, value)
        case _: SomeValue      => Leaf(id, value)
        case _: Primitive[_]   => Leaf(id, value)
        case _: Error          => Leaf(id, value)
      }
    loop(List(1), value)
  }
  case class SetNode(id: List[Int], value: DynamicValue, children: Set[MatchTree]) extends MatchTree
  case class Node(id: List[Int], value: DynamicValue, children: Vector[MatchTree]) extends MatchTree
  case class Leaf(id: List[Int], value: DynamicValue)                              extends MatchTree
}

sealed trait MatchOps

object MatchOps {
  case object CantMatch extends MatchOps
  case object NoMatch   extends MatchOps

}
