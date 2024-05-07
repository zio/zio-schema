import java.io.File
import scala.io.Source

object DocGenerator {

  final private case class StandardTypeForDoc(
    name: String,
    isJVMSupported: Boolean = true,
    isJSSupported: Boolean = true,
    isNativeSupported: Boolean = true
  )

  private def convertBooleanToText(bool: Boolean): String =
    if (bool) {
      "Yes"
    } else {
      "No"
    }

  def generateStandardTypeTable(standardTypeFile: File): String = {
    val source                                            = Source.fromFile(standardTypeFile)
    val standardTypeRegex                                 = "StandardType\\[.*".r
    val suffixRegex                                       = "].*".r
    var unsortedStandardTypes: Vector[StandardTypeForDoc] = Vector.empty
    var markdownFile =
      "|Standard Type|JVM Support|ScalaJS Support|Scala Native Support|\n|--------------|:--------------:|:--------------:|:--------------:|"

    for (line <- source.getLines()) {
      if (line.contains("implicit object")) {
        val unparsedLine = standardTypeRegex
          .findFirstIn(line)
          .getOrElse("Expected StandardType to be present in line while parsing standard type doc")
        val trimmedLine       = unparsedLine.trim()
        val lineWithoutPrefix = trimmedLine.replace("StandardType[", "")
        val standardTypeStr   = suffixRegex.replaceFirstIn(lineWithoutPrefix, "")
        val standardTypeForDoc = standardTypeStr match {
          case typ @ "java.util.UUID"     => StandardTypeForDoc(typ, isJSSupported = false)
          case typ @ "java.util.Currency" => StandardTypeForDoc(typ, isJSSupported = false, isNativeSupported = false)
          case typ @ "Chunk[Byte"         => StandardTypeForDoc("Chunk[Byte]")
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
  }
}
