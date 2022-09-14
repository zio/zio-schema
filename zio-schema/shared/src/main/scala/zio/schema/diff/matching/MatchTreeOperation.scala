package zio.schema.diff.matching

trait Operation

object Operation {
  case class Align[A,B](nodeA: MatchTree[A], nodeB: MatchTree[B])  extends Operation
  case class Insert[A,B](node: MatchTree[A], parent:MatchTree[B])  extends Operation
  case class Move[A,B](node: MatchTree[A], parent:MatchTree[B], index: Int)  extends Operation
  case class Delete[A](node: MatchTree[A])  extends Operation
}
