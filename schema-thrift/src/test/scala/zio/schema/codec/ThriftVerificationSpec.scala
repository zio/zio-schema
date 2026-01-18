package zio.schema.codec

import zio._
import zio.test._
import java.nio.file.{ Files, Path, Paths }
import scala.io.Source

object ThriftVerificationSpec extends ZIOSpecDefault {

  sealed trait RuleType
  case object Forbidden extends RuleType
  case object Required  extends RuleType

  case class Rule(
    name: String,
    pattern: String,
    ruleType: RuleType,
    errorMessage: String
  )

  case class FileVerificationResult(filename: String, errors: List[String]) {
    def isValid: Boolean = errors.isEmpty
  }

  val globalRules = List(
    Rule("No ZTransducer", "ZTransducer", Forbidden, "Found legacy 'ZTransducer' (ZIO 1). Use 'ZPipeline' instead."),
    Rule("No unsafeRun", "unsafeRun(", Forbidden, "Found legacy 'unsafeRun' (ZIO 1). Use 'Unsafe.unsafe' block."),
    Rule("Using ZIO Chunk", "Chunk", Required, "Must use 'Chunk' (ZIO native data type).")
  )

  val codecSpecificRules = List(
    Rule("Using ZPipeline", "ZPipeline", Required, "Codec must use 'ZPipeline' for streaming (ZIO 2)."),
    Rule("Using Unsafe Block", "Unsafe.unsafe", Required, "Codec must use 'Unsafe.unsafe' for low-level operations.")
  )

  val transportSpecificRules = List(
    Rule("Extends TTransport", "extends TTransport", Required, "Must implement Apache Thrift TTransport interface.")
  )

  def findFile(filename: String): Task[Path] = ZIO.attempt {
    val userDir = java.lang.System.getProperty("user.dir")
    val possiblePaths = List(
      Paths.get(userDir, "schema-thrift", "src", "main", "scala", "zio", "schema", "codec", "thrift", filename),
      Paths.get(userDir, "schema-thrift", "src", "main", "scala", "zio", "schema", "codec", filename),
      Paths.get(userDir, "src", "main", "scala", "zio", "schema", "codec", filename),
      Paths.get(userDir, "scala", "zio", "schema", "codec", filename)
    )

    possiblePaths.find(p => Files.exists(p)) match {
      case Some(path) => path
      case None =>
        throw new RuntimeException(
          s"CRITICAL: Could not find '$filename'. Searched in:\n${possiblePaths.mkString("\n")}"
        )
    }
  }

  def verifyFile(filename: String, rules: List[Rule]): Task[FileVerificationResult] =
    for {
      path     <- findFile(filename)
      lines    <- ZIO.attempt(Source.fromFile(path.toFile).getLines().toList)
      fullText = lines.mkString("\n")
    } yield {
      val errors = scala.collection.mutable.ListBuffer[String]()

      rules.filter(_.ruleType == Forbidden).foreach { rule =>
        lines.zipWithIndex.foreach {
          case (line, index) =>
            val trimmed   = line.trim
            val isComment = trimmed.startsWith("//") || trimmed.startsWith("*") || trimmed.startsWith("/*")

            if (line.contains(rule.pattern) && !isComment) {
              errors += s"[Line ${index + 1}] ${rule.errorMessage} -> Found: '${trimmed}'"
            }
        }
      }

      rules.filter(_.ruleType == Required).foreach { rule =>
        if (!fullText.contains(rule.pattern)) {
          errors += s"[Missing Feature] ${rule.errorMessage} -> Pattern '${rule.pattern}' not found in file."
        }
      }

      FileVerificationResult(filename, errors.toList)
    }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Thrift Compliance & Verification")(
    test("Analyze 'ThriftCodec.scala' for ZIO 2 Compliance") {
      val rules = globalRules ++ codecSpecificRules
      for {
        result <- verifyFile("ThriftCodec.scala", rules)
        _      <- ZIO.foreach(result.errors)(e => ZIO.debug(s"Error in ${result.filename}: $e"))
      } yield assertTrue(result.isValid)
    },
    test("Analyze 'ChunkTransport.scala' for ZIO 2 Compliance") {
      val rules = globalRules ++ transportSpecificRules
      for {
        result <- verifyFile("ChunkTransport.scala", rules)
        _      <- ZIO.foreach(result.errors)(e => ZIO.debug(s"Error in ${result.filename}: $e"))
      } yield assertTrue(result.isValid)
    }
  )
}
