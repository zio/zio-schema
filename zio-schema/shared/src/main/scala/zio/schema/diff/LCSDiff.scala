package zio.schema.diff

import zio.schema.{ Diff, Differ }
import zio.{ Chunk, ChunkBuilder }

object LCSDiff {

  def apply[A]: Differ[Chunk[A]] = new Differ[Chunk[A]] {
    override def apply(original: Chunk[A], modified: Chunk[A]): Diff[Chunk[A]] = {
      var varOriginal                      = original
      var varModified                      = modified
      var longestCommonSubstring: Chunk[A] = getLongestCommonSubsequence(original, modified)

      val buffer: ChunkBuilder[Edit[A]] = ChunkBuilder.make()

      while (longestCommonSubstring.size > 0) {
        val headOfLongestCommonSubstring = longestCommonSubstring(0)
        longestCommonSubstring = longestCommonSubstring.drop(1)

        var headOfModified = varModified(0)
        var loop           = true

        while (loop) {
          headOfModified = varModified(0)
          varModified = varModified.drop(1)
          if (headOfModified != headOfLongestCommonSubstring)
            buffer += Edit.Insert(headOfModified)

          loop = varModified.size > 0 && headOfModified != headOfLongestCommonSubstring
        }

        var headOfOriginal = varOriginal(0)
        loop = true

        while (loop) {
          headOfOriginal = varOriginal(0)
          varOriginal = varOriginal.drop(1)
          if (headOfOriginal != headOfLongestCommonSubstring)
            buffer += Edit.Delete(headOfOriginal)

          loop = varOriginal.size > 0 && headOfOriginal != headOfLongestCommonSubstring
        }

        buffer += Edit.Keep(headOfLongestCommonSubstring)
      }

      while (varModified.size > 0) {
        val headOfModified = varModified(0)
        varModified = varModified.drop(1)
        buffer += Edit.Insert(headOfModified)
      }

      while (varOriginal.size > 0) {
        val headOfOriginal = varOriginal(0)
        varOriginal = varOriginal.drop(1)
        buffer += Edit.Delete(headOfOriginal)
      }

      val edits = buffer.result()

      if (isIdentical(edits)) Diff.identical else Diff.LCS(edits)

    }

    private def isIdentical(edits: Chunk[Edit[A]]): Boolean =
      edits.isEmpty || edits.forall {
        case Edit.Keep(_) => true
        case _            => false
      }

    def getLongestCommonSubsequence(original: Chunk[A], modified: Chunk[A]): Chunk[A] =
      if (original.length == 0 || modified.length == 0) Chunk.empty
      else if (original == modified) original
      else {
        val myersMatrix: Array[Array[Int]]            = initializeMyersMatrix(original, modified)
        val longestCommonSubsequence: ChunkBuilder[A] = ChunkBuilder.make()

        var originalPosition = original.length
        var modifiedPosition = modified.length

        var loop = true

        while (loop) {
          if (myersMatrix(originalPosition)(modifiedPosition) == myersMatrix(originalPosition - 1)(modifiedPosition)) {
            originalPosition -= 1
          } else if (myersMatrix(originalPosition)(modifiedPosition) == myersMatrix(originalPosition)(
                       modifiedPosition - 1
                     )) {
            modifiedPosition -= 1
          } else {
            longestCommonSubsequence += original(originalPosition - 1)
            originalPosition -= 1
            modifiedPosition -= 1
          }

          loop = originalPosition > 0 && modifiedPosition > 0
        }

        longestCommonSubsequence.result().reverse
      }

    private def initializeMyersMatrix(original: Chunk[A], modified: Chunk[A]): Array[Array[Int]] = {
      val originalLength = original.length
      val modifiedLength = modified.length

      val myersMatrix = Array.fill[Int](originalLength + 1, modifiedLength + 1)(0)

      for (i <- 0 until originalLength) {
        for (j <- 0 until modifiedLength) {
          if (original(i) == modified(j)) {
            myersMatrix(i + 1)(j + 1) = myersMatrix(i)(j) + 1
          } else {
            if (myersMatrix(i)(j + 1) >= myersMatrix(i + 1)(j)) {
              myersMatrix(i + 1)(j + 1) = myersMatrix(i)(j + 1)
            } else {
              myersMatrix(i + 1)(j + 1) = myersMatrix(i + 1)(j)
            }
          }
        }
      }

      myersMatrix
    }
  }

  val string: Differ[String] = apply[Int].transform(
    (s: String) => Chunk.fromArray(s.chars().toArray),
    (as: Chunk[Int]) => new String(as.map(_.toChar).toArray)
  )

  def list[A]: Differ[List[A]] = apply[A].transform(
    (as: List[A]) => Chunk.fromIterable(as),
    (as: Chunk[A]) => as.toList
  )

  def map[K, V]: Differ[Map[K, V]] = apply[(K, V)].transform(
    (m: Map[K, V]) => Chunk.fromIterable(m.toSeq),
    (kvs: Chunk[(K, V)]) => kvs.toMap
  )

  def set[A]: Differ[Set[A]] = apply[A].transform(
    (as: Set[A]) => Chunk.fromIterable(as),
    (as: Chunk[A]) => as.toSet
  )

}

sealed trait Edit[A]

object Edit {
  case class Insert[A](value: A) extends Edit[A]
  case class Delete[A](value: A) extends Edit[A]
  case class Keep[A](value: A)   extends Edit[A]
}
