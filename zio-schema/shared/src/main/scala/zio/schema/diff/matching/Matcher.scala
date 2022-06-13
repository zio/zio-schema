package zio.schema.diff.matcher

import zio.schema.Schema
import zio.schema.Schema._
import zio.schema.DynamicValue
import zio.schema.DynamicValue.{
  Dictionary,
  DynamicAst,
  Enumeration,
  Error,
  LeftValue,
  NoneValue,
  Primitive,
  Record,
  RightValue,
  Sequence,
  SetValue,
  Singleton,
  SomeValue,
  Tuple
}
import scala.annotation.tailrec

/**
 * Finds the best match between two `A`s.
 *
 * Based on:
 *  https://www.merlin.uzh.ch/publication/show/2531
 *  https://bitbucket.org/sealuzh/tools-changedistiller/src/master/
 *
 */
trait TreeMatcher[A] {

  protected def indexNodes(value: A): (Map[Int, DynamicValue], Map[Int, DynamicValue])
  //val similarityCalculators: Map[Schema[A], (SimilarityCalculator[A], Double)]
  def treeMatch(l: A, r: A): TreeMatch[A]
}

object DynamicValueMatcher extends TreeMatcher[DynamicValue] {

  override protected  def indexNodes(
    value: DynamicValue
  ): (Map[Int, DynamicValue], Map[Int, DynamicValue]) = {
    val innerNodes = scala.collection.mutable.Map.empty[Int, DynamicValue]
    val leafNodes  = scala.collection.mutable.Map.empty[Int, DynamicValue]
    ???
  }

  def treeMatch(l: DynamicValue, r: DynamicValue): TreeMatch[DynamicValue] =
    ???
}
final case class TreeMatch[A](nodes: Set[Pair[_]], schema: Schema[A])

final case class Pair[A](l: A, r: A, schema: Schema[A])

trait SimilarityCalculator[A] {}


