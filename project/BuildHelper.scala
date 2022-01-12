import sbt._
import Keys._

import explicitdeps.ExplicitDepsPlugin.autoImport._
import sbtcrossproject.CrossPlugin.autoImport.{ CrossType, crossProjectPlatform }
import sbtbuildinfo._
import BuildInfoKeys._
import scalafix.sbt.ScalafixPlugin.autoImport._

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

  val Scala212: String   = versions("2.12")
  val Scala213: String   = versions("2.13")
  val ScalaDotty: String = "3.1.0" //versions.getOrElse("3.0", versions("3.1"))

  val zioVersion        = "1.0.12"
  val zioJsonVersion    = "0.2.0-M2"
  val zioPreludeVersion = "1.0.0-RC7"
  val zioOpticsVersion  = "0.1.0"
  val silencerVersion   = "1.7.8"

  private val testDeps = Seq(
    "dev.zio" %% "zio-test"     % zioVersion % "test",
    "dev.zio" %% "zio-test-sbt" % zioVersion % "test"
  )

  def macroDefinitionSettings = Seq(
    scalacOptions += "-language:experimental.macros",
    libraryDependencies ++= {
      if (scalaVersion.value == ScalaDotty) Seq()
      else
        Seq(
          "org.scala-lang" % "scala-reflect"  % scalaVersion.value % "provided",
          "org.scala-lang" % "scala-compiler" % scalaVersion.value % "provided"
        )
    }
  )

  private def compileOnlyDeps(scalaVersion: String) = {
    val stdCompileOnlyDeps = {
      if (scalaVersion == ScalaDotty)
        Seq(
          "com.github.ghik" % s"silencer-lib_$Scala213" % silencerVersion % Provided
        )
      else
        Seq(
          ("com.github.ghik" % "silencer-lib" % silencerVersion % Provided).cross(CrossVersion.full),
          compilerPlugin(("com.github.ghik" % "silencer-plugin" % silencerVersion).cross(CrossVersion.full))
        )
    }
    CrossVersion.partialVersion(scalaVersion) match {
      case Some((2, x)) if x <= 12 =>
        stdCompileOnlyDeps ++ Seq(
          compilerPlugin(("org.scalamacros" % "paradise" % "2.1.1").cross(CrossVersion.full))
        )
      case _ => stdCompileOnlyDeps
    }
  }

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
        Seq("-Xfatal-warnings", "-Ypatmat-exhaust-depth", "80")
      } else {
        Seq("-Ypatmat-exhaust-depth", "80")
//        Nil // to enable Scalafix locally
      }
    }

    val std2xOptions = Seq(
      "-language:higherKinds",
      "-explaintypes",
      "-Yrangepos",
      "-Xlint:_,-missing-interpolator,-type-parameter-shadow",
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
      case Some((3, 0)) =>
        Seq(
          "-language:implicitConversions",
          "-Xignore-scala2-macros"
        )
      case Some((2, 13)) =>
        Seq(
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Ywarn-unused",
          "-Ymacro-annotations"
        ) ++ std2xOptions ++ optimizerOptions
      case Some((2, 12)) =>
        Seq(
          "-Ypartial-unification",
          "-opt-warnings",
          "-Ywarn-extra-implicit",
          "-Ywarn-unused",
          "-Yno-adapted-args",
          "-Ywarn-inaccessible",
          "-Ywarn-infer-any",
          "-Ywarn-nullary-override",
          "-Ywarn-nullary-unit"
        ) ++ std2xOptions ++ optimizerOptions
      case _ => Seq.empty
    }

    stdOptions ++ extraOptions
  }

  val dottySettings = Seq(
    crossScalaVersions += ScalaDotty,
    scalacOptions ++= {
      if (scalaVersion.value == ScalaDotty)
        Seq("-noindent")
      else
        Seq()
    },
    scalacOptions --= {
      if (scalaVersion.value == ScalaDotty)
        Seq("-Xfatal-warnings")
      else
        Seq()
    },
    Compile / doc / sources := {
      val old = (Compile / doc / sources).value
      if (scalaVersion.value == ScalaDotty) {
        Nil
      } else {
        old
      }
    },
    Test / parallelExecution := {
      val old = (Test / parallelExecution).value
      if (scalaVersion.value == ScalaDotty) {
        false
      } else {
        old
      }
    }
  )

  def platformSpecificSources(platform: String, conf: String, baseDirectory: File)(versions: String*) =
    for {
      platform <- List("shared", platform)
      version  <- "scala" :: versions.toList.map("scala-" + _)
      result   = baseDirectory.getParentFile / platform.toLowerCase / "src" / conf / version
      if result.exists
    } yield result

  def crossPlatformSources(scalaVer: String, platform: String, conf: String, baseDir: File) = {
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
    }
  )

  def buildInfoSettings(packageName: String) = Seq(
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, isSnapshot),
    buildInfoPackage := packageName
  )

  def stdSettings(prjName: String) =
    Seq(
      name := s"$prjName",
      crossScalaVersions := Seq(Scala213, Scala212),
      ThisBuild / scalaVersion := crossScalaVersions.value.head, //ScalaDotty,
      scalacOptions := compilerOptions(scalaVersion.value, optimize = !isSnapshot.value),
      libraryDependencies ++= compileOnlyDeps(scalaVersion.value) ++ testDeps,
      ThisBuild / semanticdbEnabled := scalaVersion.value != ScalaDotty, // enable SemanticDB,
      ThisBuild / semanticdbOptions += "-P:semanticdb:synthetics:on",
      ThisBuild / semanticdbVersion := scalafixSemanticdb.revision,
      ThisBuild / scalafixScalaBinaryVersion := CrossVersion.binaryScalaVersion(scalaVersion.value),
      ThisBuild / scalafixDependencies ++= List(
        "com.github.liancheng" %% "organize-imports" % "0.6.0",
        "com.github.vovapolu"  %% "scaluzzi"         % "0.1.20"
      ),
      Test / parallelExecution := true,
      incOptions ~= (_.withLogRecompileOnMacro(true)),
      autoAPIMappings := true,
      testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
    )
}
