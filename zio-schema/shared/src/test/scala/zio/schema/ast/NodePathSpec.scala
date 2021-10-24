package zio.schema.ast

import zio.random.Random
import zio.test.{ DefaultRunnableSpec, _ }

object NodePathSpec extends DefaultRunnableSpec {

  override def spec: ZSpec[Environment, Failure] = suite("NodePath")(
    suite("relativeTo")(
      test("compute relative subpath") {
        val path    = NodePath.root / "foo" / "bar"
        val subpath = NodePath.root / "foo" / "bar" / "baz"

        val relative = subpath.relativeTo(path)

        assertTrue(relative == NodePath.root / "baz")
      },
      test("compute path relative to self") {
        val path     = NodePath.root / "foo" / "bar"
        val relative = path.relativeTo(path)

        assertTrue(relative == NodePath.root)
      },
      test("return full path when not subpath") {
        val path1 = NodePath.root / "foo" / "bar"
        val path2 = NodePath.root / "foo" / "baz"

        val relative = path2.relativeTo(path1)

        assertTrue(relative == path2)
      }
    ),
    suite("isSubpathOf")(
      testM("any path is subpath of root") {
        check(anyPath) { path =>
          assertTrue(path.isSubpathOf(NodePath.root))
        }
      },
      testM("any relative path is subpath") {
        val pathAndSubpath =
          for {
            path         <- anyPath
            relativePath <- anyPath
          } yield path -> (path / relativePath)

        check(pathAndSubpath) {
          case (path, subpath) =>
            assertTrue(subpath.isSubpathOf(path))
        }
      }
    ),
    suite("partitionLeaf")(
      testM("partition path into internal path and leaf label") {
        check(anyPathOfN(2)) { path =>
          val (internalPath, leaf) = path.partitionLeaf

          assertTrue(internalPath == NodePath(path.dropRight(1)))
          assertTrue(leaf == Some(path.last))
        }
      },
      test("partition root path into empty path and None") {
        val (internalPath, leaf) = NodePath.root.partitionLeaf

        assertTrue(internalPath == NodePath.root)
        assertTrue(leaf == None)
      }
    )
  )

  def anyPathOfN(n: Int): Gen[Random with Sized, NodePath] = Gen.chunkOfN(n)(Gen.anyString).map(NodePath(_))

  val anyPath: Gen[Random with Sized, NodePath] = Gen.chunkOf(Gen.anyString).map(NodePath(_))

}
