package zio.schema.doc.generator

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import scala.io.Source

object Main {

  private val standardTypeFile =
    Paths.get(".", "zio-schema", "jvm", "src", "main", "scala", "zio", "schema", "StandardType.scala").toFile

  final private case class StandardTypeForDoc(
    name: String,
    isJVMSupported: Boolean = true,
    isJSSupported: Boolean = true,
    isNativeSupported: Boolean = true
  )

  private def convertBooleanToText(bool: Boolean): String =
    if (bool) {
      "✅"
    } else {
      "❌"
    }

  private def generateStandardTypeFileText(standardTypeFile: File): String = {
    val standardTypeRegex                                 = "StandardType\\[.*".r
    val suffixRegex                                       = "((])(?!.*])).*".r
    var unsortedStandardTypes: Vector[StandardTypeForDoc] = Vector.empty
    var markdownFile                                      =
      """---
        |id: standard-type-reference
        |title: "Standard Type Reference"
        |---
        |# Standard Type Reference
        |
        |ZIO Schema provides a number of built-in primitive types, that we can use to represent our data. These can be seen in the following table:
        |
        ||Standard Type|JVM Support|ScalaJS Support|Scala Native Support|
        ||--------------|:--------------:|:--------------:|:--------------:|""".stripMargin

    val source = Source.fromFile(standardTypeFile)

    try {
      for (line <- source.getLines()) {
        if (line.contains("implicit object")) {
          val unparsedLine = standardTypeRegex
            .findFirstIn(line)
            .getOrElse("Expected StandardType to be present in line while parsing standard type doc")
          val trimmedLine        = unparsedLine.trim()
          val lineWithoutPrefix  = trimmedLine.replace("StandardType[", "")
          val standardTypeStr    = suffixRegex.replaceFirstIn(lineWithoutPrefix, "")
          val standardTypeForDoc = standardTypeStr match {
            case typ @ "java.util.UUID"     => StandardTypeForDoc(typ, isJSSupported = false)
            case typ @ "java.util.Currency" => StandardTypeForDoc(typ, isJSSupported = false, isNativeSupported = false)
            case typ                        => StandardTypeForDoc(typ)
          }
          unsortedStandardTypes = unsortedStandardTypes :+ standardTypeForDoc
        }
      }

      val sortedStandardTypes = unsortedStandardTypes.sortBy(_.name)
      sortedStandardTypes.foreach { standardType =>
        val jsSupport     = convertBooleanToText(standardType.isJSSupported)
        val jvmSupport    = convertBooleanToText(standardType.isJVMSupported)
        val nativeSupport = convertBooleanToText(standardType.isNativeSupported)
        markdownFile += s"\n|`${standardType.name}`|$jvmSupport|$jsSupport|$nativeSupport|"
      }
      markdownFile
    } finally {
      source.close()
    }
  }

  def main(args: Array[String]): Unit = {
    val table = generateStandardTypeFileText(standardTypeFile)
    Files.write(
      Paths.get(".", "docs", "standard-type-reference.md"),
      table.getBytes(StandardCharsets.UTF_8)
    )
    ()
  }
}
