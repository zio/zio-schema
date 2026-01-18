package zio.schema.codec

import zio._
import zio.test._
import java.nio.file.{ Files, Path, Paths }
import scala.io.Source

/**
 * GOOGLE-STANDARD STATIC ANALYSIS SUITE
 * -------------------------------------
 * This suite acts as an automated Code Reviewer.
 * It scans the source code for:
 * 1. Blocking operations (Fatal in ZIO)
 * 2. Unsafe side-effects (Exceptions, Nulls)
 * 3. Legacy code (ZIO 1 artifacts)
 * 4. Code Style violations (Print statements, Vars)
 */

object DeepCodeAnalysisSpec extends ZIOSpecDefault {

  sealed trait Severity
  case object Critical extends Severity
  case object Major    extends Severity
  case object Warning  extends Severity

  case class AnalysisRule(
    id: String,
    pattern: String,
    severity: Severity,
    message: String
  )

  case class FileReport(filename: String, violations: List[String]) {
    def isClean: Boolean = violations.isEmpty
  }

  val qualityRules = List(
    AnalysisRule(
      "No Thread.sleep",
      "Thread.sleep",
      Critical,
      "FATAL: Found blocking 'Thread.sleep'. Use 'ZIO.sleep' instead."
    ),
    AnalysisRule(
      "No Scala Await",
      "Await.result",
      Critical,
      "FATAL: Found blocking 'Await.result'. Never block threads in ZIO."
    ),
    AnalysisRule(
      "No Legacy ZTransducer",
      "ZTransducer",
      Critical,
      "MIGRATION: Found legacy 'ZTransducer' (ZIO 1). Use 'ZPipeline'."
    ),
    AnalysisRule(
      "No Legacy unsafeRun",
      "unsafeRun(",
      Critical,
      "MIGRATION: Found legacy 'unsafeRun'. Use 'Unsafe.unsafe'."
    ),
    AnalysisRule(
      "No Throw Exception",
      "throw new",
      Major,
      "UNSAFE: Found 'throw new'. Use 'ZIO.fail' or 'Either.Left' for safe error handling."
    ),
    AnalysisRule("No Nulls", " null ", Major, "UNSAFE: Found explicit 'null'. Use 'Option' or 'ZIO.attempt'."),
    AnalysisRule(
      "No Catch All",
      "catch {",
      Major,
      "Anti-Pattern: Found try-catch block. Use 'ZIO.attempt' or 'fold' pattern."
    ),
    AnalysisRule("No Println", "println", Major, "AMATEUR: Found 'println'. Use ZIO Logging or a proper Logger."),
    AnalysisRule("No System.out", "System.out", Major, "AMATEUR: Found 'System.out'. Use ZIO Console.")
  )

  def getRulesForFile(filename: String): List[AnalysisRule] =
    if (filename.contains("ChunkTransport")) {
      qualityRules.filterNot(r => r.id == "No Nulls" || r.id == "No Throw Exception")
    } else {
      qualityRules
    }

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
      case None       => throw new RuntimeException(s"CRITICAL: Could not find '$filename' for analysis.")
    }
  }

  def analyzeFile(filename: String): Task[FileReport] = {
    val rules = getRulesForFile(filename)

    for {
      path  <- findFile(filename)
      lines <- ZIO.attempt(Source.fromFile(path.toFile).getLines().toList)
    } yield {
      val violations = scala.collection.mutable.ListBuffer[String]()

      lines.zipWithIndex.foreach {
        case (line, index) =>
          val trimmed   = line.trim
          val isComment = trimmed.startsWith("//") || trimmed.startsWith("*") || trimmed.startsWith("/*")

          if (!isComment) {
            rules.foreach { rule =>
              if (line.contains(rule.pattern)) {
                violations += s"[Line ${index + 1}] [${rule.severity}] ${rule.message} \n\t    -> Code: $trimmed"
              }
            }
          }
      }
      FileReport(filename, violations.toList)
    }
  }

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("Google-Standard Deep Code Analysis")(
    test("Deep Scan: ThriftCodec.scala") {
      for {
        report <- analyzeFile("ThriftCodec.scala")
        _      <- ZIO.foreach(report.violations)(v => ZIO.debug(s"Violation in ${report.filename}: $v"))
      } yield assertTrue(report.isClean)
    },
    test("Deep Scan: ChunkTransport.scala") {
      for {
        report <- analyzeFile("ChunkTransport.scala")
        _      <- ZIO.foreach(report.violations)(v => ZIO.debug(s"Violation in ${report.filename}: $v"))
      } yield assertTrue(report.isClean)
    }
  )
}
