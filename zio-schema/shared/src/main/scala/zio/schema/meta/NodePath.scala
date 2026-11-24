package zio.schema.meta

import zio.Chunk
import zio.prelude._

/**
 * Builds [[NodePath]] values. A `NodePath` is a ZIO Prelude `Subtype` of
 * `Chunk[String]`, so `NodePath(chunk)` wraps a chunk of labels and every
 * `NodePath` is also a `Chunk[String]`.
 *
 * {{{
 * import zio.schema.meta._
 * import zio.schema.meta.NodePath._
 *
 * val city: NodePath = NodePath.root / "address" / "city"
 *
 * val rendered: String = city.render                                // "address/city"
 * val rest: NodePath   = city.relativeTo(NodePath.root / "address") // Chunk(city)
 * }}}
 */
object NodePath extends Subtype[Chunk[String]] {
  /** The empty path, which points to the root node. */
  val root: NodePath  = NodePath(Chunk.fromArray(Array.empty[String]))
  /** The empty path; the same value as `root`. */
  val empty: NodePath = NodePath(Chunk.fromArray(Array.empty[String]))

  /**
   * Operations on [[NodePath]]. On Scala 2.12 they need
   * `import zio.schema.meta.NodePath._`; on Scala 2.13 and 3 they are found
   * without an import.
   */
  implicit class NodePathSyntax(private val self: NodePath) extends AnyVal {

    /** Returns this path with `label` appended. */
    def /(label: String): NodePath =
      NodePath(self :+ label)

    /** Returns this path with the labels of `subpath` appended. */
    def /(subpath: NodePath): NodePath = NodePath(self ++ subpath)

    /**
     * Returns the rest of this path after the prefix `path`. Returns this path
     * unchanged when `path` is not a prefix of it.
     */
    def relativeTo(path: NodePath): NodePath =
      if (self.isSubpathOf(path))
        NodePath(self.drop(path.length))
      else
        self

    /**
     * Returns `true` when `path` is a prefix of this path, so this path points
     * to `path` or to a node below it. Every path is a subpath of the root.
     */
    def isSubpathOf(path: NodePath): Boolean =
      if (path.length > self.length)
        false
      else
        path.zip(self).forall(p => p._1 == p._2)

    /**
     * Splits this path into its parent path and its last label. Returns
     * `(root, None)` for the root.
     */
    def partitionLeaf: (NodePath, Option[String]) =
      if (self.isEmpty) (self, None)
      else (NodePath(self.dropRight(1)), Some(self.last))

    /** Renders the labels joined by `/`, or `ROOT` for the root path. */
    def render: String =
      if (self.isEmpty)
        "ROOT"
      else
        self.mkString("/")
  }
}
