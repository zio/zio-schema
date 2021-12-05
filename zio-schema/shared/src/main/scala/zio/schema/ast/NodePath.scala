package zio.schema.ast

import zio.Chunk
import zio.prelude._

object NodePath extends Subtype[Chunk[String]] {
  val root: NodePath  = NodePath(Chunk.fromArray(Array.empty[String]))
  val empty: NodePath = NodePath(Chunk.fromArray(Array.empty[String]))

  implicit class NodePathSyntax(private val self: NodePath) extends AnyVal {

    def /(label: String): NodePath =
      NodePath(self :+ label)

    def /(subpath: NodePath): NodePath = NodePath(self ++ subpath)

    def relativeTo(path: NodePath): NodePath =
      if (self.isSubpathOf(path))
        NodePath(self.drop(path.length))
      else
        self

    def isSubpathOf(path: NodePath): Boolean =
      if (path.length > self.length)
        false
      else
        path.zip(self).forall(p => p._1 == p._2)

    def partitionLeaf: (NodePath, Option[String]) =
      if (self.isEmpty) (self, None)
      else (NodePath(self.dropRight(1)), Some(self.last))

    def render: String =
      if (self.isEmpty)
        "ROOT"
      else
        self.mkString("/")
  }
}
