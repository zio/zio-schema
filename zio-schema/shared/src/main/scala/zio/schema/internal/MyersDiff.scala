package zio.schema.internal

import java.time.ZoneId
import java.util.UUID

import scala.annotation.tailrec

import zio.Chunk
import zio.schema.Diff.Edit.{ Delete, Insert, Keep }
import zio.schema.{ Diff, Differ, Schema, StandardType }

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

  override def patch[A](schema: Schema[A], diff: Diff): Either[String, A => Either[String, A]] =
    (schema, diff) match {
      case (Schema.Primitive(StandardType.StringType), Diff.Myers(edits)) =>
        Right((a: A) => calculateStringFromEdits(a, edits))
      case (Schema.Primitive(StandardType.UUIDType), Diff.Myers(edits)) =>
        Right((a: A) => calculateStringFromEdits(a.toString, edits).map(UUID.fromString))
      case (Schema.Primitive(StandardType.ZoneId), Diff.Myers(edits)) =>
        Right((a: A) => calculateStringFromEdits(a.getId(), edits).map(ZoneId.of))
      case (schema, diff) => Left(s"Incorrect Diff=$diff for Schema=$schema.")
    }

  def calculateStringFromEdits(input: String, edits: Chunk[Diff.Edit]): Either[String, String] = {
    @tailrec
    def calc(in: List[Char], edits: List[Diff.Edit], result: List[Char]): Either[String, String] = (in, edits) match {
      case (_ :: _, Nil)                               => Left(s"Incorrect Diff - no istructions for these letters: ${in.mkString}.")
      case (h :: _, Delete(s) :: _) if s != h.toString => Left(s"Cannot Delete $s - current letter is $h.")
      case (Nil, Delete(s) :: _)                       => Left(s"Cannot Delete $s - no letters left to delete.")
      case (_ :: t, Delete(_) :: tail)                 => calc(t, tail, result)
      case (h :: _, Keep(s) :: _) if s != h.toString   => Left(s"Cannot Keep $s - current letter is $h.")
      case (Nil, Keep(s) :: _)                         => Left(s"Cannot Keep $s - no letters left to keep.")
      case (h :: t, Keep(_) :: tail)                   => calc(t, tail, result :+ h)
      case (in, Insert(s) :: tail)                     => calc(in, tail, result ++ s.toList)
      case (Nil, Nil)                                  => Right(result.mkString)
    }

    calc(input.toList, edits.toList, Nil)
  }
}
