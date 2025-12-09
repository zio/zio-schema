import sbt.*
import Keys.*
import sbtcrossproject.CrossPlugin.autoImport.*
import sbtbuildinfo.*
import BuildInfoKeys.*
import com.typesafe.tools.mima.core.*
import com.typesafe.tools.mima.core.ProblemFilters.exclude
import com.typesafe.tools.mima.plugin.MimaKeys.{ mimaBinaryIssueFilters, mimaFailOnProblem, mimaPreviousArtifacts }
import com.typesafe.tools.mima.plugin.MimaPlugin.autoImport.mimaCheckDirection
import sbtdynver.DynVerPlugin.autoImport.previousStableVersion
import scalafix.sbt.ScalafixPlugin.autoImport.*
import scalanativecrossproject.NativePlatform

import scala.scalanative.build.{ GC, Mode }
import scala.scalanative.sbtplugin.ScalaNativePlugin.autoImport.nativeConfig

object BuildHelper {

  private val versions: Map[String, String] = {
    import org.snakeyaml.engine.v2.api.{ Load, LoadSettings }

    import java.util.{ List => JList, Map => JMap }
    import scala.jdk.CollectionConverters._

    val doc = new Load(LoadSettings.builder().build())
      .loadFromReader(scala.io.Source.fromFile(".github/workflows/ci.yml").bufferedReader())
    val yaml = doc.asInstanceOf[JMap[String, JMap[String, JMap[String, JMap[String, JMap[String, JList[String]]]]]]]
    val list = yaml.get("jobs").get("build").get("strategy").get("matrix").get("scala").asScala
    list.map(v => (v.split('.').take(2).mkString("."), v)).toMap
  }

  val Scala212: String = versions("2.12")
  val Scala213: String = versions("2.13")
  val Scala3: String   = versions("3.3")

  val zioVersion                   = "2.1.23"
  val zioJsonVersion               = "0.7.45"
  val zioPreludeVersion            = "1.0.0-RC44"
  val zioOpticsVersion             = "0.2.2"
  val zioBsonVersion               = "1.0.6"
  val avroVersion                  = "1.12.1"
  val bsonVersion                  = "5.6.2"
  val zioConstraintlessVersion     = "0.3.3"
  val scalaCollectionCompatVersion = "2.14.0"
  val msgpackVersion               = "0.9.10"
  val jacksonScalaVersion          = "2.20.1"
  val thriftVersion                = "0.22.0"
  val javaxAnnotationApiVersion    = "3.0.0"
  val scalaJavaTimeVersion         = "2.6.0"

  def macroDefinitionSettings = Seq(
    scalacOptions += "-language:experimental.macros",
    libraryDependencies ++= {
      if (scalaVersion.value == Scala3) Seq()
      else
        Seq(
          "org.scala-lang" % "scala-reflect"  % scalaVersion.value % Provided,
          "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided
        )
    }
  )

  private def compileOnlyDeps(scalaVersion: String): Seq[ModuleID] =
    (
      CrossVersion.partialVersion(scalaVersion) match {
        case Some((2, x)) =>
          Seq(
            compilerPlugin(("org.typelevel" %% "kind-projector" % "0.13.4").cross(CrossVersion.full))
          )
        case _ => Seq.empty
      }
    ) ++ (
      CrossVersion.partialVersion(scalaVersion) match {
        case Some((2, x)) if x <= 12 =>
          Seq(
            compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full))
          )
        case _ => Seq.empty
      }
    )

  private def compilerOptions(scalaVersion: String, optimize: Boolean) = {
    val stdOptions = Seq(
      "-deprecation",
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked",
      "-language:existentials"
    ) ++ {
      if (sys.env.contains("CI")) {
        Seq("-Xfatal-warnings")
      } else {
        Seq()
      }
    }

    val std2xOptions = Seq(
      "-language:higherKinds",
      "-explaintypes",
      "-Yrangepos",
      "-Xlint:_,-missing-interpolator,-type-parameter-shadow,-infer-any",
      "-Ypatmat-exhaust-depth",
      "40",
      "-Ywarn-numeric-widen",
      "-Ywarn-value-discard",
      "-Xsource:3.0"
    )

    val optimizerOptions =
      if (optimize)
        Seq(
          "-opt:l:inline"
        )
      else Seq.empty

    val extraOptions = CrossVersion.partialVersion(scalaVersion) match {
      case Some((3, _)) =>
        Seq(
          "-language:implicitConversions",
          "-Xignore-scala2-macros",
          "-Ykind-projector"
        )
      case Some((2, 13)) =>
        Seq(
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Ywarn-unused",
          "-Ymacro-annotations",
          "-Ywarn-macros:after"
        ) ++ std2xOptions ++ optimizerOptions
      case Some((2, 12)) =>
        Seq(
          "-Ypartial-unification",
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit",
          "-Wconf:cat=unused-nowarn:s"
        ) ++ std2xOptions ++ optimizerOptions
      case _ => Seq.empty
    }

    stdOptions ++ extraOptions
  }

  val dottySettings = Seq(
    scalacOptions --= {
      if (scalaVersion.value == Scala3)
        Seq("-Xfatal-warnings")
      else
        Seq()
    }
  )

  def platformSpecificSources(platform: String, conf: String, baseDirectory: File)(versions: String*): Seq[File] =
    for {
      platform <- List("shared", platform)
      version  <- "scala" :: versions.toList.map("scala-" + _)
      result   = baseDirectory.getParentFile / platform.toLowerCase / "src" / conf / version
      if result.exists
    } yield result

  def crossPlatformSources(scalaVer: String, platform: String, conf: String, baseDir: File): Seq[sbt.File] = {
    val versions = CrossVersion.partialVersion(scalaVer) match {
      case Some((2, 11)) =>
        List("2.11", "2.11+", "2.11-2.12", "2.x")
      case Some((2, 12)) =>
        List("2.12", "2.11+", "2.12+", "2.11-2.12", "2.12-2.13", "2.x")
      case Some((2, 13)) =>
        List("2.13", "2.11+", "2.12+", "2.13+", "2.12-2.13", "2.x")
      case _ =>
        List()
    }
    platformSpecificSources(platform, conf, baseDir)(versions: _*)
  }

  lazy val crossProjectSettings = Seq(
    Compile / unmanagedSourceDirectories ++= {
      crossPlatformSources(
        scalaVersion.value,
        crossProjectPlatform.value.identifier,
        "main",
        baseDirectory.value
      )
    },
    Test / unmanagedSourceDirectories ++= {
      crossPlatformSources(
        scalaVersion.value,
        crossProjectPlatform.value.identifier,
        "test",
        baseDirectory.value
      )
    },
    nativeConfig ~= { _.withMode(Mode.releaseFast) },
    scalacOptions += {
      if (crossProjectPlatform.value == NativePlatform)
        "-P:scalanative:genStaticForwardersForNonTopLevelObjects"
      else ""
    },
    Test / fork := crossProjectPlatform.value == JVMPlatform // set fork to `true` on JVM to improve log readability, JS and Native need `false`
  )

  def buildInfoSettings(packageName: String) = Seq(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, isSnapshot),
    buildInfoPackage := packageName
  )

  def stdSettings(prjName: String) =
    Seq(
      name := s"$prjName",
      crossScalaVersions := Seq(Scala213, Scala212, Scala3),
      ThisBuild / scalaVersion := Scala213, //crossScalaVersions.value.head, //Scala3,
      scalacOptions ++= compilerOptions(scalaVersion.value, optimize = !isSnapshot.value),
      libraryDependencies ++= compileOnlyDeps(scalaVersion.value),
      versionScheme := Some("early-semver"),
      ThisBuild / semanticdbEnabled := scalaVersion.value != Scala3, // enable SemanticDB,
      ThisBuild / semanticdbOptions += "-P:semanticdb:synthetics:on",
      ThisBuild / semanticdbVersion := scalafixSemanticdb.revision,
      ThisBuild / scalafixDependencies ++= List(
        "com.github.vovapolu" %% "scaluzzi" % "0.1.23"
      ),
      Test / parallelExecution := !sys.env.contains("CI"),
      incOptions ~= (_.withLogRecompileOnMacro(true)),
      autoAPIMappings := true,
      testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework")),
      mimaPreviousArtifacts := previousStableVersion.value
        .filter(_ != "1.5.0")
        .map(organization.value %% name.value % _)
        .toSet ++ Set(
        organization.value %% name.value % "1.4.1"
      ),
      mimaCheckDirection := "backward", // TODO use "both" for patch versions
      mimaBinaryIssueFilters ++= Seq(
        exclude[Problem]("zio.schema.Schema#Collection.fromChunk"),
        exclude[Problem]("zio.schema.Schema#Collection.toChunk"),
        exclude[Problem]("zio.schema.Schema#Collection.empty"),
        exclude[Problem]("zio.schema.Schema#NonEmptyMap.fromChunk"),
        exclude[Problem]("zio.schema.Schema#NonEmptyMap.toChunk"),
        exclude[Problem]("zio.schema.codec.JsonCodec#JsonDecoder.x"),
        exclude[Problem]("zio.schema.SchemaPlatformSpecific.*"),
        exclude[Problem]("zio.schema.Schema.url"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("zio.schema.codec.JsonCodec#CaseClassJsonDecoder.this"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("zio.schema.codec.JsonCodec#CaseClassJsonDecoder.apply"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("zio.schema.codec.JsonCodec#CaseClassJsonDecoder.this"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("zio.schema.codec.JsonCodec#JsonDecoder.decode"),
        ProblemFilters.exclude[Problem]("zio.schema.codec.JsonCodec#JsonDecoder#DecoderKey.*"),
        ProblemFilters.exclude[DirectMissingMethodProblem]("zio.schema.codec.JsonCodec#CaseClassJsonDecoder.apply"),
        ProblemFilters.exclude[Problem]("zio.schema.codec.JsonCodec#JsonEncoder#EncoderKey.*"),
        ProblemFilters.exclude[ReversedMissingMethodProblem](
          "zio.schema.Schema#Enum.zio$schema$Schema$Enum$_setter_$discriminatorName_="
        ),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("zio.schema.Schema#Enum.discriminatorName"),
        ProblemFilters
          .exclude[ReversedMissingMethodProblem]("zio.schema.Schema#Field.zio$schema$Schema$Field$_setter_$aliases_="),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("zio.schema.Schema#Field.aliases"),
        ProblemFilters.exclude[ReversedMissingMethodProblem](
          "zio.schema.Schema#Record.zio$schema$Schema$Record$_setter_$rejectExtraFields_="
        ),
        ProblemFilters.exclude[ReversedMissingMethodProblem]("zio.schema.Schema#Record.rejectExtraFields")
      ),
      mimaFailOnProblem := true
    )
}
