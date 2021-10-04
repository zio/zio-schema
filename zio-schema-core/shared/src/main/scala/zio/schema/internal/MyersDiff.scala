package zio.schema.internal

import zio.Chunk
import zio.schema.{ Diff, Differ }

object MyersDiff extends Differ[String] {

  def apply(original: String, modified: String): Diff = {

    var varOriginal                    = original
    var varModified                    = modified
    var longestCommonSubstring: String = getLongestCommonSubsequence(original, modified)

    var edits: Chunk[Diff.Edit] = Chunk.empty

    while (longestCommonSubstring.size > 0) {
      val headOfLongestCommonSubstring = longestCommonSubstring(0)
      longestCommonSubstring = longestCommonSubstring.drop(1)

      var headOfModified = varModified(0)
      var loop           = true

      while (loop) {
        headOfModified = varModified(0)
        varModified = varModified.drop(1)
        if (headOfModified != headOfLongestCommonSubstring)
          edits = edits :+ Diff.Edit.Insert(headOfModified.toString)

        loop = varModified.size > 0 && headOfModified != headOfLongestCommonSubstring
      }

      var headOfOriginal = varOriginal(0)
      loop = true

      while (loop) {
        headOfOriginal = varOriginal(0)
        varOriginal = varOriginal.drop(1)
        if (headOfOriginal != headOfLongestCommonSubstring)
          edits = edits :+ Diff.Edit.Delete(headOfOriginal.toString)

        loop = varOriginal.size > 0 && headOfOriginal != headOfLongestCommonSubstring
      }

      edits = edits :+ Diff.Edit.Keep(headOfLongestCommonSubstring.toString)
    }

    while (varModified.size > 0) {
      val headOfModified = varModified(0)
      varModified = varModified.drop(1)
      edits = edits :+ Diff.Edit.Insert(headOfModified.toString)
    }

    while (varOriginal.size > 0) {
      val headOfOriginal = varOriginal(0)
      varOriginal = varOriginal.drop(1)
      edits = edits :+ Diff.Edit.Delete(headOfOriginal.toString)
    }

    if (isIdentical(edits)) Diff.Identical else Diff.Myers(edits)
  }

  private def isIdentical(edits: Chunk[Diff.Edit]): Boolean =
    edits.isEmpty || edits.forall {
      case Diff.Edit.Keep(_) => true
      case _                 => false
    }

  def getLongestCommonSubsequence(original: String, modified: String): String =
    if (original == null || original.length() == 0 || modified == null || modified.length() == 0) ""
    else if (original == modified) original
    else {

      val myersMatrix: Array[Array[Int]] = initializeMyersMatrix(original, modified)
      val longestCommonSubsequence       = new StringBuilder()

      var originalPosition = original.length()
      var modifiedPosition = modified.length()

      var loop = true

      while (loop) {
        if (myersMatrix(originalPosition)(modifiedPosition) == myersMatrix(originalPosition - 1)(modifiedPosition)) {
          originalPosition -= 1
        } else if (myersMatrix(originalPosition)(modifiedPosition) == myersMatrix(originalPosition)(
                     modifiedPosition - 1
                   )) {
          modifiedPosition -= 1
        } else {
          longestCommonSubsequence += original.charAt(originalPosition - 1)
          originalPosition -= 1
          modifiedPosition -= 1
        }

        loop = originalPosition > 0 && modifiedPosition > 0
      }

      longestCommonSubsequence.toString.reverse
    }

  private def initializeMyersMatrix(original: String, modified: String): Array[Array[Int]] = {
    val originalLength = original.length()
    val modifiedLength = modified.length()

    val myersMatrix = Array.fill[Int](originalLength + 1, modifiedLength + 1)(0)

    for (i <- 0 until originalLength) {
      for (j <- 0 until modifiedLength) {
        if (original.charAt(i) == modified.charAt(j)) {
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
